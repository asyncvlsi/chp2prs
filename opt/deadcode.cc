/*************************************************************************
 *
 *  This file is part of the ACT library
 *
 *  Copyright (c) 2018-2020 Lincoln Berkley
 *  Copyright (c) 2021-2022 Henry Heffan
 *
 *  This program is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU General Public License
 *  as published by the Free Software Foundation; either version 2
 *  of the License, or (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 51 Franklin Street, Fifth Floor,
 *  Boston, MA  02110-1301, USA.
 *
 **************************************************************************
 */
#include "chp-opt.h"
#include "chp-print.h"
#include "algos.h"
#include "hassert.h"

/**
 * This file implements dead code elimination. This consists mainly of 2 kinds
 * of optimization. First, if we have code like this
 *
 * z := x
 * END
 *
 * or like this
 *
 * z := x
 * z := y
 *
 * Then we can remove the statement `z := x`. In general, if there are chp
 * assignments whose values are never used, the assignments can be removed.
 *
 * Second, if we have a select statement like
 *
 * [ false -> z := x [] n > 7 -> z:= y ]
 *
 * we can replace it with
 *
 * [ n > 7 -> z:= y ]
 */

namespace ChpOptimize {



 namespace {

enum class IsAlive { no, yes };

inline IsAlive operator|(const IsAlive &a, const IsAlive &b) {
    return ((a == IsAlive::yes) || (b == IsAlive::yes)) ? IsAlive::yes
                                                        : IsAlive::no;
}

struct VarLivenessTable {
    std::unordered_set<VarId> live_vars;

  public:
    [[nodiscard]] IsAlive at(VarId id) const {
        return live_vars.contains(id) ? IsAlive::yes : IsAlive::no;
    }

    // return whether insert was actually needed
    bool set_alive(VarId id) {
        auto it = live_vars.find(id);
        if (it == live_vars.end()) {
            live_vars.insert(id);
            return true;
        }
        return false;
    }
};

static_assert(IsAlive{} == IsAlive::no);

void flow(VarLivenessTable &table, const Sequence &seq, const IdPool &id_pool,
          std::set<Block *> &run_once) {
    auto update_cell = [&](VarId id, const IsAlive &val) -> bool {
        auto nval = table.at(id) | val;
        if (nval == IsAlive::yes)
            return table.set_alive(id);
        return false;
    };

    auto update_cell_from = [&](VarId dst, VarId src) {
        return update_cell(dst, table.at(src));
    };

    for (Block *curr = seq.endseq->parent(); curr != seq.startseq;
         curr = curr->parent()) {
        switch (curr->type()) {
        case BlockType::Basic:
            switch (curr->u_basic().stmt.type()) {
            case StatementType::Assign: {
                const auto &assign = curr->u_basic().stmt.u_assign();
                hassert(assign.ids.size() == assign.e.roots.size());

                std::unordered_set<VarId> used_ids;
                for (ssize_t i = 0; i < (ssize_t)assign.ids.size(); ++i) {
                    if (table.at(assign.ids[i]) == IsAlive::no)
                        continue;

                    for (VarId used_id :
                         getIdsUsedByExpr(assign.e, assign.e.roots[i]))
                        used_ids.insert(used_id);
                }

                for (auto id : used_ids)
                    update_cell(id, IsAlive::yes);
                break;
            }
            case StatementType::Receive: {
                break;
            }
            case StatementType::Send:
                for (auto id :
                     getIdsUsedByExpr(curr->u_basic().stmt.u_send().e))
                    update_cell(id, IsAlive::yes);
                break;
            }
            break;
        case BlockType::Par: {
            bool changed = false;
            // first apply the merge in reverse
            for (const auto &merge : curr->u_par().merges) {
                hassert(Algo::count_if(merge.branch_ids,
                                       [](const OptionalVarId &id) {
                                           return static_cast<bool>(id);
                                       }));
                for (const auto &branch_id : merge.branch_ids) {
                    if (branch_id)
                        changed |= update_cell_from(*branch_id, merge.post_id);
                }
            }

            if (changed || !run_once.contains(curr)) {
                // then do all the flows
                run_once.insert(curr);

                for (const auto &branch : curr->u_par().branches)
                    flow(table, branch, id_pool, run_once);

                // If any branch has the read as alive, then the output is also
                // alive
                for (const auto &split : curr->u_par().splits) {
                    IsAlive is_alive =
                        Algo::any_of(split.branch_ids,
                                     [&](const OptionalVarId branch_id) {
                                         return branch_id &&
                                                table.at(*branch_id) ==
                                                    IsAlive::yes;
                                     })
                            ? IsAlive::yes
                            : IsAlive::no;
                    update_cell(split.pre_id, is_alive);
                }
            }
            break;
        }
        case BlockType::Select: {
            bool changed = false;
            // first apply the merge in reverse
            for (const auto &merge : curr->u_select().merges) {
                for (const auto &branch_id : merge.branch_ids)
                    changed |= update_cell_from(branch_id, merge.post_id);
            }

            if (changed || !run_once.contains(curr)) {
                // then do all the flows
                run_once.insert(curr);

                // then do all the flows
                for (const auto &branch : curr->u_select().branches)
                    flow(table, branch.seq, id_pool, run_once);

                // If any branch has the read as alive, then the output is also
                // alive
                for (const auto &split : curr->u_select().splits) {
                    IsAlive is_alive =
                        Algo::any_of(split.branch_ids,
                                     [&](const OptionalVarId branch_id) {
                                         return branch_id &&
                                                table.at(*branch_id) ==
                                                    IsAlive::yes;
                                     })
                            ? IsAlive::yes
                            : IsAlive::no;
                    update_cell(split.pre_id, is_alive);
                }

                // finally, the guards might make some variables alive
                for (const auto &branch : curr->u_select().branches) {
                    if (branch.g.type() == IRGuardType::Expression) {
                        for (auto id : getIdsUsedByExpr(branch.g.u_e().e))
                            update_cell(id, IsAlive::yes);
                    }
                }
            }
            break;
        }
        case BlockType::DoLoop: {
            bool changed = false;

            // first apply the merge in reverse
            for (const auto &phi : curr->u_doloop().out_phis)
                changed |= update_cell_from(phi.bodyout_id, phi.post_id);
            for (const auto &phi : curr->u_doloop().loop_phis) {
                changed |= update_cell_from(phi.bodyout_id, phi.bodyin_id);
                if (phi.post_id)
                    changed |= update_cell_from(phi.bodyout_id, *phi.post_id);
            }

            if (changed || !run_once.contains(curr)) {
                // then do all the flows
                run_once.insert(curr);

                do {
                    // first, the guards might make some variables alive
                    for (auto id : getIdsUsedByExpr(curr->u_doloop().guard))
                        update_cell(id, IsAlive::yes);

                    flow(table, curr->u_doloop().branch, id_pool, run_once);

                    // and keep doing it until it stabilizes
                    changed = false;
                    for (const auto &phi : curr->u_doloop().in_phis)
                        changed |= update_cell_from(phi.pre_id, phi.bodyin_id);
                    for (const auto &phi : curr->u_doloop().loop_phis) {
                        changed |= update_cell_from(phi.pre_id, phi.bodyin_id);
                        changed |=
                            update_cell_from(phi.bodyout_id, phi.bodyin_id);
                    }
                } while (changed);
            }
            break;
        }
        case BlockType::StartSequence:
        case BlockType::EndSequence:
            hassert(false);
            break;
        }
    }
}

VarLivenessTable flow(const ChpGraph &graph) {
    VarLivenessTable table;
    std::set<Block *> run_once;
    flow(table, graph.m_seq, graph.id_pool(), run_once);
    return table;
}

UsesAndDefs buildVarUsageTable(
    const Sequence &seq,
    std::unordered_map<const Block *, UsesAndDefs> &block_to_usedefs) {
    Block *curr = seq.startseq->child();
    UsesAndDefs all_ud;
    while (curr != seq.endseq) {
        UsesAndDefs curr_ud;
        switch (curr->type()) {
        case BlockType::StartSequence:
        case BlockType::EndSequence:
            hassert(false);
            break;
        case BlockType::Basic: {
            switch (curr->u_basic().stmt.type()) {
            case StatementType::Assign:
                addIdsUsedByExpr(curr_ud.var_reads,
                                 curr->u_basic().stmt.u_assign().e);
                for (const auto &id : curr->u_basic().stmt.u_assign().ids)
                    curr_ud.var_writes.insert(id);
                break;
            case StatementType::Send:
                curr_ud.uses_chan = true;
                addIdsUsedByExpr(curr_ud.var_reads,
                                 curr->u_basic().stmt.u_send().e);
                break;
            case StatementType::Receive:
                curr_ud.uses_chan = true;
                if (curr->u_basic().stmt.u_receive().var)
                    curr_ud.var_writes.insert(
                        *curr->u_basic().stmt.u_receive().var);
                break;
            }
            break;
        }
        case BlockType::Par:
            for (const Sequence &path : curr->u_par().branches)
                curr_ud |= buildVarUsageTable(path, block_to_usedefs);
            break;
        case BlockType::Select:
            hassert(curr->u_select().splits.empty());
            hassert(curr->u_select().merges.empty());
            for (const SelectBranch &branch : curr->u_select().branches) {
                if (branch.g.type() == IRGuardType::Expression)
                    addIdsUsedByExpr(curr_ud.var_reads, branch.g.u_e().e);
                curr_ud |= buildVarUsageTable(branch.seq, block_to_usedefs);
            }
            // TODO add code to prove this is false during a different pass?
            // TODO add option to compiler that allows it to assume this is
            // false
            curr_ud.can_hang = Algo::none_of(
                curr->u_select().branches, [](const SelectBranch &sb) {
                    return sb.g.type() == IRGuardType::Else;
                });
            break;
        case BlockType::DoLoop:
            hassert(curr->u_doloop().in_phis.empty());
            hassert(curr->u_doloop().out_phis.empty());
            hassert(curr->u_doloop().loop_phis.empty());
            // TODO add code to prove this is false during a different pass?
            // TODO add option to compiler that allows it to assume this is
            // false
            curr_ud |=
                buildVarUsageTable(curr->u_doloop().branch, block_to_usedefs);
            curr_ud.can_hang = true;
            addIdsUsedByExpr(curr_ud.var_reads, curr->u_doloop().guard);
            break;
        }

        hassert(!block_to_usedefs.count(curr));
        block_to_usedefs[curr] = curr_ud;

        all_ud |= curr_ud;

        curr = curr->child();
    }
    return all_ud;
}
} // namespace

std::unordered_map<const Block *, UsesAndDefs>
getDefUsesTable(const ChpGraph &graph) {
    std::unordered_map<const Block *, UsesAndDefs> result;
    buildVarUsageTable(graph.m_seq, result);
    return result;
}

namespace {

// For each block, if the block is a basic block and if the basic block is an
// assignment, then check whether in this blocks child (there is only 1 because
// it is a basic block) the variable is dead. If it is, then this assignment can
// be eliminated!
bool eliminateCompletelyDeadPass(const std::vector<Block *> &blocks,
                                 const VarLivenessTable &table) {
    // first remove assignments that are not needed
    bool changed = false;
    for (Block *b : blocks) {
        if (b->dead)
            continue;

        switch (b->type()) {

        case BlockType::Basic:
            switch (b->u_basic().stmt.type()) {

            case StatementType::Assign: {
                hassert(b->u_basic().stmt.u_assign().ids.size() ==
                        b->u_basic().stmt.u_assign().e.roots.size());
                auto before_root_ct =
                    b->u_basic().stmt.u_assign().e.roots.size();
                Algo::remove_filter_2_if(b->u_basic().stmt.u_assign().ids,
                                         b->u_basic().stmt.u_assign().e.roots,
                                         [&](const VarId &id, const auto &) {
                                             bool should_remove =
                                                 table.at(id) == IsAlive::no;
                                             changed |= should_remove;
                                             return should_remove;
                                         });
                changed |= before_root_ct !=
                           b->u_basic().stmt.u_assign().e.roots.size();
                hassert(b->u_basic().stmt.u_assign().ids.size() ==
                        b->u_basic().stmt.u_assign().e.roots.size());
                if (b->u_basic().stmt.u_assign().ids.empty()) {
                    ChpGraph::spliceOutBasicBlock(b, MarkDead::yes);
                    continue;
                }
                break;
            }
            case StatementType::Send:
                break;
            case StatementType::Receive: {
                auto var_id = b->u_basic().stmt.u_receive().var;
                if (var_id) {
                    bool should_remove = table.at(*var_id) == IsAlive::no;
                    if (should_remove) {
                        b->u_basic().stmt.u_receive().var =
                            OptionalVarId::null_id();
                        changed = true;
                        continue;
                    }
                }
                break;
            }
            }
            break;
        case BlockType::Par: {
            for (auto &split : b->u_par().splits) {
                for (auto &branch_id : split.branch_ids) {
                    if (branch_id && table.at(*branch_id) == IsAlive::no)
                        branch_id = OptionalVarId::null_id();
                }
            }
            Algo::remove_filter_if(b->u_par().splits, [&](const auto &split) {
                bool all_branch_ids_null =
                    Algo::all_of(split.branch_ids, [](const auto &opt_id) {
                        return opt_id == OptionalVarId::null_id();
                    });
                return table.at(split.pre_id) == IsAlive::no ||
                       all_branch_ids_null;
            });
            Algo::remove_filter_if(b->u_par().merges, [&](const auto &merge) {
                return table.at(merge.post_id) == IsAlive::no;
            });
            break;
        }
        case BlockType::Select: {
            for (auto &split : b->u_select().splits) {
                for (auto &branch_id : split.branch_ids) {
                    if (branch_id && table.at(*branch_id) == IsAlive::no)
                        branch_id = OptionalVarId::null_id();
                }
            }
            Algo::remove_filter_if(
                b->u_select().splits, [&](const auto &split) {
                    bool all_branch_ids_null =
                        Algo::all_of(split.branch_ids, [](const auto &opt_id) {
                            return opt_id == OptionalVarId::null_id();
                        });
                    return table.at(split.pre_id) == IsAlive::no ||
                           all_branch_ids_null;
                });
            Algo::remove_filter_if(
                b->u_select().merges, [&](const auto &merge) {
                    return table.at(merge.post_id) == IsAlive::no;
                });
            break;
        }
        case BlockType::DoLoop: {
            // loop_phis can either be downgraded or removed
            for (auto it = b->u_doloop().loop_phis.begin();
                 it != b->u_doloop().loop_phis.end();) {
                auto &phi = *it;
                if (phi.post_id && table.at(*phi.post_id) == IsAlive::no)
                    phi.post_id = OptionalVarId::null_id();

                if (phi.post_id && phi.bodyout_id == phi.bodyin_id) {
                    // replace with a "in_phi" function and a copy
		    //hassert(false); // TODO
		  warning ("Missing optimization opportunity in dead-code elim");
		  fprintf (stderr, ">> loop_phi: post=%d, bodyout=bodyin=%d\n",
			   (int)(*phi.post_id).m_id,
			   (int)phi.bodyout_id.m_id);
		  ++it;
                } else if (!phi.post_id && phi.bodyout_id == phi.bodyin_id) {
                    // replace with a "in_phi" function
                    b->u_doloop().in_phis.push_back(
                        {phi.pre_id, phi.bodyin_id});
                    it = b->u_doloop().loop_phis.erase(it);
                } else if (phi.post_id &&
                           table.at(phi.bodyin_id) == IsAlive::no) {
                    // replace with an out_phi
                    b->u_doloop().out_phis.push_back(
                        {phi.bodyout_id, *phi.post_id});
                    it = b->u_doloop().loop_phis.erase(it);
                } else if (!phi.post_id &&
                           table.at(phi.bodyin_id) == IsAlive::no) {
                    // then delete this phi function
                    it = b->u_doloop().loop_phis.erase(it);
                } else {
                    // this phi function is still needed
                    ++it;
                }
            }

            Algo::remove_filter_if(b->u_doloop().in_phis, [&](const auto &phi) {
                return table.at(phi.pre_id) == IsAlive::no;
            });
            Algo::remove_filter_if(
                b->u_doloop().out_phis, [&](const auto &phi) {
                    return table.at(phi.post_id) == IsAlive::no;
                });
            break;
        }
        case BlockType::StartSequence:
        case BlockType::EndSequence:
            hassert(false);
            break;
        }
    }
    return changed;
}

void remapVarsInSeq(
    Sequence &seq, const std::unordered_map<VarId, VarId> &new_id_from_old_id) {
    auto remap = [&](VarId &id) {
        if (new_id_from_old_id.contains(id))
            id = new_id_from_old_id.at(id);
    };
    auto remap_opt = [&](OptionalVarId &oid) {
        if (oid) {
            VarId id = *oid;
            remap(id);
            oid = id;
        }
    };
    auto remap_expr = [&](ChpExprDag &dag) {
        ChpExprDag::mapNodes(dag, [&](ChpExprDag::Node &n) {
            if (n.type() == IRExprTypeKind::Var)
                remap(n.u_var().id);
        });
    };
    // first remove assignments that are not needed
    for (Block *curr = seq.startseq->child(); curr != seq.endseq;
         curr = curr->child()) {
        switch (curr->type()) {
        case BlockType::Basic:
            switch (curr->u_basic().stmt.type()) {
            case StatementType::Assign:
                for (auto &id : curr->u_basic().stmt.u_assign().ids)
                    remap(id);
                remap_expr(curr->u_basic().stmt.u_assign().e);
                break;
            case StatementType::Send:
                remap_expr(curr->u_basic().stmt.u_send().e.m_dag);
                break;
            case StatementType::Receive:
                remap_opt(curr->u_basic().stmt.u_receive().var);
                break;
            }
            break;
        case BlockType::Par:
            for (auto &split : curr->u_par().splits)
                remap(split.pre_id);
            for (auto &merge : curr->u_par().merges)
                remap(merge.post_id);
            break;
        case BlockType::Select:
            for (auto &branch : curr->u_select().branches) {
                if (branch.g.type() == IRGuardType::Expression)
                    remap_expr(branch.g.u_e().e.m_dag);
            }
            for (auto &split : curr->u_select().splits)
                remap(split.pre_id);
            for (auto &merge : curr->u_select().merges)
                remap(merge.post_id);
            break;
        case BlockType::DoLoop:
            for (auto &phi : curr->u_doloop().in_phis)
                remap(phi.pre_id);
            for (auto &phi : curr->u_doloop().out_phis)
                remap(phi.post_id);
            for (auto &phi : curr->u_doloop().loop_phis) {
                remap(phi.pre_id);
                if (phi.post_id)
                    remap_opt(phi.post_id);
            }
            break;
        case BlockType::StartSequence:
        case BlockType::EndSequence:
            hassert(false);
            break;
        }
    }
}

[[maybe_unused]] bool pruneDeadBranchesPass(const std::vector<Block *> &blocks,
                                            ChpGraph &graph) {
    // TODO: Add simplifications where guards are constant true/false
    // Add skips where necessary to avoid empty guarded branches, remove empty
    // parallel branches

    hassert(!graph.m_seq.empty());

    auto isTrueExpr = [&](const ChpExprDag::Node *e) {
        hassert(e);
        if (e->type() != IRExprTypeKind::Const)
            return false;
        hassert(e->u_cons().v_width == 1);
        return e->u_cons().v == BigInt{1};
    };
    auto isFalseExpr = [&](const ChpExprDag::Node *e) {
        hassert(e);
        if (e->type() != IRExprTypeKind::Const)
            return false;
        hassert(e->u_cons().v_width == 1);
        return e->u_cons().v == BigInt{0};
    };

    bool changed = false;
    for (Block *b : blocks) {
        if (b->dead)
            continue;

        auto isFalseGuard = [&](const IRGuard &guard) {
            return guard.type() == IRGuardType::Expression &&
                   isFalseExpr(guard.u_e().e.root());
        };
        auto isTrueGuard = [&](const IRGuard &guard) {
            return guard.type() == IRGuardType::Expression &&
                   isTrueExpr(guard.u_e().e.root());
        };
        auto isElseGuard = [&](const IRGuard &guard) {
            return guard.type() == IRGuardType::Else;
        };

        // TODO if there two branches have the same code, merge them. In
        // particular, if 2 branches are just SKIP, merge them.
        // TODO if a branch is skip, turn it into an "else" branch (because we
        // are in a "no-guard" world)?

        // Prune dead branches from a SELECT BLOCK.
        // It is NOT ok to flatten a block with only 1 guard, because the CHP is
        // _required_ to go into an infinite loop if the guard is false
        switch (b->type()) {
        case BlockType::StartSequence:
        case BlockType::EndSequence:
            hassert(false);
            break;
        case BlockType::Basic:
            break;
        case BlockType::Select: {
            int true_ct = (int)Algo::count_if(b->u_select().branches,
                                              [&](const SelectBranch &branch) {
                                                  return isTrueGuard(branch.g);
                                              });
            int false_ct = (int)Algo::count_if(
                b->u_select().branches, [&](const SelectBranch &branch) {
                    return isFalseGuard(branch.g);
                });
            int else_ct = (int)Algo::count_if(b->u_select().branches,
                                              [&](const SelectBranch &branch) {
                                                  return isElseGuard(branch.g);
                                              });
            int num_branches = (int)b->u_select().branches.size();

            hassert(true_ct + false_ct + else_ct <=
                    num_branches); // sanity check
            hassert(else_ct <= 1);

            if (true_ct > 1) {
                fprintf(stderr, "A select statement may not have more than 1 "
                                "true branch\n");
                exit(1);
            }

            if (false_ct == num_branches) {
                fprintf(
                    stderr,
                    "A Select statement may not have only false branches\n");
                exit(1);
            }

            // Otherwise, breaks apart the select into a more useful form
            struct SelectBranchData {
                Sequence seq;
                IRGuard g;
                std::vector<OptionalVarId> phi_in_ids;
                std::vector<VarId> phi_out_ids;
            };
            std::vector<SelectBranchData> branches;
            std::vector<VarId> phi_pre_ids, phi_post_ids;

            // first transform into the more converiant form
            for (auto &branch : b->u_select().branches)
                branches.push_back({branch.seq, std::move(branch.g), {}, {}});
            for (const auto &split : b->u_select().splits) {
                phi_pre_ids.push_back(split.pre_id);
                hassert(split.branch_ids.size() == branches.size());
                for (int i = 0; i < (int)split.branch_ids.size(); ++i)
                    branches[i].phi_in_ids.push_back(split.branch_ids[i]);
            }
            for (const auto &merge : b->u_select().merges) {
                phi_post_ids.push_back(merge.post_id);
                hassert(merge.branch_ids.size() == branches.size());
                for (int i = 0; i < (int)merge.branch_ids.size(); ++i)
                    branches[i].phi_out_ids.push_back(merge.branch_ids[i]);
            }
            b->u_select().branches.clear();
            b->u_select().splits.clear();
            b->u_select().merges.clear();

            // Then do the analysis on the flat structures.
            // First, delete every branch which is a "false" branch
            const size_t branches_original_size = branches.size();
            Algo::remove_filter_if(branches,
                                   [&](const SelectBranchData &branch) {
                                       return isFalseGuard(branch.g);
                                   });
            changed |= (branches.size() != branches_original_size);
            // if there is only 1 guard left, and it is the ELSE_GUARD, change
            // it to the "true" guard
            if (branches.size() == 1 && isElseGuard(branches[0].g)) {
                branches[0].g = IRGuard::makeExpression(
                    ChpExprSingleRootDag::makeConstant(BigInt(1), 1));
                changed = true;
            }

            // if there are multiple "skip" branches, merge them together
            // Actually, this is hard, because what gets passed into and out of
            // the phi functions matters
            // TODO this can be generalized for _any_ kind of repeated branch
            if (Algo::count_if(branches, [&](const SelectBranchData &branch) {
                    return branch.seq.empty();
                }) >= 2) {
                std::map<std::map<VarId, VarId>, std::vector<Block *>>
                    empty_seqstarts_from_phimap;
                for (auto &branch : branches) {
                    if (!branch.seq.empty())
                        continue;

                    // we want to find two branches that connec tthe same input
                    // phis to the same output phis
                    std::map<VarId, VarId> pre_id_from_branch_id;
                    for (size_t j = 0; j < branch.phi_in_ids.size(); ++j) {
                        if (branch.phi_in_ids[j])
                            pre_id_from_branch_id[*branch.phi_in_ids[j]] =
                                phi_pre_ids[j];
                    }
                    std::map<VarId, VarId> pre_id_from_post_id;
                    for (size_t j = 0; j < branch.phi_out_ids.size(); ++j) {
                        hassert(pre_id_from_branch_id.contains(
                            branch.phi_out_ids[j]));
                        pre_id_from_post_id[phi_post_ids[j]] =
                            pre_id_from_branch_id.at(branch.phi_out_ids[j]);
                    }

                    empty_seqstarts_from_phimap[pre_id_from_post_id].push_back(
                        branch.seq.startseq);
                }

                std::vector<std::pair<VarId, ChpExprSingleRootDag>>
                    assign_block;
                for (const auto &[_, seqstarts] : empty_seqstarts_from_phimap) {
                    if (seqstarts.size() < 2)
                        continue;
                    changed = true;
                    auto seqstarts_set =
                        std::set<Block *>{seqstarts.begin(), seqstarts.end()};

                    // do this to get around the fact that each loops mutation
                    // of the list branches will invalidate pointers into the
                    // `branches` vector
                    std::vector<SelectBranchData *> target_branches;
                    for (auto &branch : branches) {
                        if (seqstarts_set.contains(branch.seq.startseq))
                            target_branches.push_back(&branch);
                    }

                    bool is_else_guard =
                        (Algo::count_if(target_branches,
                                        [&](const SelectBranchData *branch) {
                                            return isElseGuard(branch->g);
                                        }) != 0);

                    IRGuard new_guard;
                    if (is_else_guard) {
                        new_guard = IRGuard::makeElse();
                    } else {
                        std::vector<ChpExprSingleRootDag> guards;
                        for (const auto &branch : target_branches) {
                            hassert(branch->g.type() ==
                                    IRGuardType::Expression);
                            if (branch->seq.empty())
                                guards.push_back(
                                    ChpExprSingleRootDag::deep_copy(
                                        branch->g.u_e().e));
                        }

                        // then we have some branches to fuse together combine
                        // all the guards with "OR"s

                        VarId guard_id = graph.id_pool().makeUniqueVar(1);
                        assign_block.emplace_back(
                            guard_id, ChpExprSingleRootDag::combine_reduce(
                                          IRBinaryOpType::Or, guards));
                        new_guard = IRGuard::makeExpression(
                            ChpExprSingleRootDag::makeVariableAccess(guard_id,
                                                                     1));
                    }

                    // now make the first of the "target_branches" into the
                    // combined branch, and remove all the other branches
                    target_branches[0]->g = std::move(new_guard);

                    Algo::remove_filter_if(branches, [&](const auto &branch) {
                        return seqstarts_set.contains(branch.seq.startseq) &&
                               branch.seq.startseq !=
                                   target_branches[0]->seq.startseq;
                    });
                }

                if (!assign_block.empty()) {
                    changed = true;
                    std::vector<VarId> new_ids;
                    ChpExprDag dag;

                    for (const auto &[id, expr] : assign_block) {
                        auto *root = dag.addSubdag(expr);
                        dag.roots.push_back(root);
                        new_ids.push_back(id);
                    }

                    Block *assign = graph.newBasicBlock(
                        Statement::makeAssignment(new_ids, std::move(dag)));

                    ChpGraph::spliceInSequenceBefore(b, {assign});
                }
            }

            // If there is a select statement with a single branch, and it has
            // either true or "else" as its guard, then we can remove the select
            // completely. Otherwise,  synthesize the branches.
            if (branches.size() == 1 &&
                (isTrueGuard(branches[0].g) || isElseGuard(branches[0].g))) {
                auto &branch = branches[0];

                // build the remaps
                std::unordered_map<VarId, VarId> new_id_from_old_id;
                hassert(phi_pre_ids.size() == branch.phi_in_ids.size());
                for (int i = 0; i < (int)phi_pre_ids.size(); ++i) {
                    if (branch.phi_in_ids[i])
                        new_id_from_old_id[*branch.phi_in_ids[i]] =
                            phi_pre_ids[i];
                }
                hassert(phi_post_ids.size() == branch.phi_out_ids.size());
                for (int i = 0; i < (int)phi_post_ids.size(); ++i)
                    new_id_from_old_id[branch.phi_out_ids[i]] = phi_post_ids[i];
                b->u_select().splits.clear();
                b->u_select().merges.clear();

                // splice out the select block, then apply remaps and insert the
                // sequence
                Block *before = b->parent();
                ChpGraph::spliceOutEmptyControlBlock(b, MarkDead::yes);

                remapVarsInSeq(branch.seq, new_id_from_old_id);
                ChpGraph::spliceInSequenceAfter(before, branch.seq);

                changed = true;
            } else {
                // recreate the branches
                for (auto &branch : branches)
                    b->u_select().branches.emplace_back(branch.seq,
                                                        std::move(branch.g));

                for (int i = 0; i < (int)phi_pre_ids.size(); ++i) {
                    std::vector<OptionalVarId> ids;
                    for (auto &branch : branches)
                        ids.push_back(branch.phi_in_ids[i]);
                    b->u_select().splits.push_back(
                        {phi_pre_ids[i], std::move(ids)});
                }
                for (int i = 0; i < (int)phi_post_ids.size(); ++i) {
                    std::vector<VarId> ids;
                    for (auto &branch : branches)
                        ids.push_back(branch.phi_out_ids[i]);
                    b->u_select().merges.push_back(
                        {std::move(ids), phi_post_ids[i]});
                }
            }
            break;
        }
        case BlockType::DoLoop: {
            /* TODO reimplement this!
            if (isFalseExpr(b->u_doloop().guard.root())) {
                // then remove the loop as we only go through it once
                ChpGraph::spliceOutFlatNonemptyControlBlock(b, MarkDead::yes);
                changed = true;
            } else if (isTrueExpr(b->u_doloop().guard.root())) {
                // then remove the "after" path

                ChpGraph::spliceOutUntilEndSequence(b->child(), MarkDead::yes);
            }
             */
            break;
        }
        case BlockType::Par: {
            // Remove any empty branch. This is required to enforce graph
            // invariants!
            std::vector<const Block *>
                to_remove; // holds the starts of the sequences of the blocks to
                           // remove
            for (const Sequence &path : b->u_par().branches) {
                if (path.startseq->child() == path.endseq) {
                    hassert(path.startseq == path.endseq->parent());
                    to_remove.push_back(path.startseq);
                }
            }
            ChpGraph::spliceOutControlBlockBranches(b, to_remove,
                                                    MarkDead::yes);

            // Remove parallel blocks with no branches, and flatten blocks with
            // 1 branch
            if (b->u_par().branches.empty()) {
                hassert(b->u_par().splits.empty());
                hassert(b->u_par().merges.empty());
                ChpGraph::spliceOutEmptyControlBlock(b, MarkDead::yes);
            } else if (b->u_par().branches.size() == 1) {
                std::unordered_map<VarId, VarId> new_id_from_old_id;
                for (const auto &split : b->u_par().splits) {
                    hassert(split.branch_ids.size() == 1);
                    if (split.branch_ids[0])
                        new_id_from_old_id[*split.branch_ids[0]] = split.pre_id;
                }
                for (const auto &merge : b->u_par().merges) {
                    hassert(merge.branch_ids.size() == 1);
                    hassert(merge.branch_ids[0]);
                    new_id_from_old_id[*merge.branch_ids[0]] = merge.post_id;
                }
                b->u_par().splits.clear();
                b->u_par().merges.clear();

                Sequence seq = b->u_par().branches.front();
                ChpGraph::spliceOutControlBlockBranches(
                    b, {b->u_par().branches.front().startseq}, MarkDead::no);

                Block *before = b->parent();
                ChpGraph::spliceOutEmptyControlBlock(b, MarkDead::yes);

                remapVarsInSeq(seq, new_id_from_old_id);
                ChpGraph::spliceInSequenceAfter(before, seq);
            }
            break;
        }
        }
    }
    return changed;
}

bool pruneUnobservableOnEndingSeq(
    Sequence &seq,
    const std::unordered_map<const Block *, UsesAndDefs> &def_uses) {
    bool changed = false;
    // iterate from back to front, removing any block which does not access a
    // channel
    while (seq.endseq->parent() != seq.startseq) {
        Block *curr = seq.endseq->parent();
        if (def_uses.at(curr).uses_chan) {
            // we have found observable behavior. If this is a basic block,
            // there is nothing left to prune, so we stop. Likewise for a
            // DoLoop. If this is a select statement, we can prune each branch
            // independently. Likewise, for a Par block (as each branch of a Par
            // block is not allowed to read any variable written on any other
            // branch)
            switch (curr->type()) {
            case BlockType::StartSequence:
            case BlockType::EndSequence:
                hassert(false);
                break;
            case BlockType::Basic:
                break;
            case BlockType::Par:
                for (auto &path : curr->u_par().branches)
                    changed |= pruneUnobservableOnEndingSeq(path, def_uses);
                hassert(!Algo::all_of(
                    curr->u_par().branches,
                    [](const Sequence &path) -> bool { return path.empty(); }));
                break;
            case BlockType::Select:
                for (auto &branch : curr->u_select().branches)
                    changed |=
                        pruneUnobservableOnEndingSeq(branch.seq, def_uses);
                hassert(!Algo::all_of(curr->u_select().branches,
                                      [](const SelectBranch &branch) -> bool {
                                          return branch.seq.empty();
                                      }));
                break;
            case BlockType::DoLoop:
                break;
            }

            return changed;
        }
        ChpGraph::spliceOutBasicBlock(curr, MarkDead::yes);
        changed = true;
    }
    return changed;
}

[[maybe_unused]] bool pruneUnobservablePass(ChpGraph &graph) {
    auto def_uses = getDefUsesTable(graph);
    return pruneUnobservableOnEndingSeq(graph.m_seq, def_uses);
}

[[maybe_unused]] bool
pruneHaltingSkipSelects(const std::vector<Block *> &all_blocks) {
    bool changed = false;
    for (Block *b : all_blocks) {
        if (b->dead)
            continue;
        if (b->type() == BlockType::Select &&
            b->u_select().branches.size() == 1 &&
            b->u_select().branches.front().seq.empty()) {
            changed = true;
            ChpGraph::spliceOutFlatNonemptyControlBlock(b, MarkDead::yes);
        }
    }
    return changed;
}
} // namespace

// TODO
// (b) We should keep track of reads on the level of bits, and propogate bits
// through expressions based on which bits matter to a given read (c) using
// that, we should rewrite things with resizes, and also shorten break variables
// into pieces?
bool eliminateDeadCode(ChpGraph &graph) {
    bool changed = true, ever_changed = false;
    while (changed) {

        VarLivenessTable table = flow(graph);

        auto all_blocks = graph.getLiveBlocks();
        for (Block *b : all_blocks) {
            b->dead = false;
        }

        changed = eliminateCompletelyDeadPass(all_blocks, table);

	if (graph.m_seq.empty()) {
	  // everything is dead!
	  break;
	}
	
        changed |= pruneDeadBranchesPass(all_blocks, graph);

        //        print_chp(std::cerr, graph);

        // if we have a select statement that is of the form [ g -> skip ]
        // remove it
        // TODO add a marker for that assumption? Can optimize based on it more?
        //        changed |= pruneHaltingSkipSelects(all_blocks);

        // If there is any code which runs after the final observable behavior,
        // we can eliminate it
        // TODO       changed |= pruneUnobservablePass(graph);

        // If there is a doloop which does not interact with channels and with a
        // guard that is not written inside the loop, we can replace the loop
        // with a select plus an infinite loop changed |=
        // flattenUnchangedGuardLoopsPass(all_blocks, graph);

        if (changed)
            ever_changed = true;

        graph.validateGraphInvariants();
    }

    graph.validateGraphInvariants();

    return ever_changed;
}

} // namespace ChpOptimize
