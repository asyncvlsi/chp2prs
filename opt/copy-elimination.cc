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

namespace ChpOptimize {

namespace {
std::unordered_map<VarId, VarId>
remove_copies(Sequence &seq,
              std::unordered_map<VarId, VarId> &&new_id_from_old_id,
              bool &changed) {
    auto remap_with = [&](const std::unordered_map<VarId, VarId> &map,
                          VarId &id) -> void {
        if (map.contains(id)) {
            auto new_id = map.at(id);
            changed |= (id != new_id);
            id = new_id;
        }
    };
    auto remap_expr_with = [&](const std::unordered_map<VarId, VarId> &map,
                               ChpExprDag &dag) {
        ChpExprDag::mapNodes(dag, [&](ChpExprDag::Node &n) {
            if (n.type() == IRExprTypeKind::Var)
                remap_with(map, n.u_var().id);
        });
    };
    auto remap_opt_with = [&](const std::unordered_map<VarId, VarId> &map,
                              OptionalVarId &oid) {
        if (oid) {
            VarId id = *oid;
            remap_with(map, id);
            oid = id;
        }
    };

    auto remap = [&](VarId &id) -> void { remap_with(new_id_from_old_id, id); };
    //    auto remap_opt = [&](OptionalVarId &oid) {
    //        if (oid) {
    //            VarId id = *oid;
    //            remap(id);
    //            oid = id;
    //        }
    //    };
    auto remap_expr = [&](ChpExprDag &dag) {
        remap_expr_with(new_id_from_old_id, dag);
    };

    for (Block *curr = seq.startseq->child(); curr != seq.endseq;
         curr = curr->child()) {
        switch (curr->type()) {
        case BlockType::Basic:
            switch (curr->u_basic().stmt.type()) {
            case StatementType::Assign: {
                auto &assign = curr->u_basic().stmt.u_assign();
                ChpExprDag::deduplicate(assign.e);
                // remap first so that "copy-chains" fuse properly
                remap_expr(assign.e);
                Algo::remove_filter_2_if(
                    assign.ids, assign.e.roots,
                    [&](const VarId &id, const ChpExprDag::Node *root) {
                        hassert(!new_id_from_old_id.contains(id));
                        if (root->type() == IRExprTypeKind::Var) {
                            new_id_from_old_id[id] = root->u_var().id;
                            changed = true;
                            return true;
                        }
                        return false;
                    });
                hassert(assign.ids.size() == assign.e.roots.size());
                for (auto id : assign.ids)
                    hassert(!getIdsUsedByExpr(assign.e).contains(id));

                // then deduplicate output variables that both copy from the
                // same node
                std::unordered_map<const ChpExprDag::Node *, VarId>
                    id_from_node;
                Algo::remove_filter_2_if(
                    assign.ids, assign.e.roots,
                    [&](const VarId &id, const ChpExprDag::Node *root) {
                        if (id_from_node.contains(root)) {
                            new_id_from_old_id[id] = id_from_node.at(root);
                            changed = true;
                            return true;
                        }
                        id_from_node[root] = id;
                        return false;
                    });

                if (assign.ids.empty()) {
                    curr = curr->parent();
                    ChpGraph::spliceOutBasicBlock(curr->child(), MarkDead::yes);
                }
                break;
            }
            case StatementType::Send:
                remap_expr(curr->u_basic().stmt.u_send().e.m_dag);
                break;
            case StatementType::Receive:
                if (curr->u_basic().stmt.u_receive().var)
                    hassert(!new_id_from_old_id.contains(
                        *curr->u_basic().stmt.u_receive().var));
                break;
            }
            break;
        case BlockType::Par: {
            for (auto &split : curr->u_par().splits)
                remap(split.pre_id);

            // First sort the splits. It is stable so it is deterministic across
            // platforms
            std::stable_sort(curr->u_par().splits.begin(),
                             curr->u_par().splits.end(),
                             [](const auto &a, const auto &b) {
                                 return a.pre_id < b.pre_id;
                             });

            // TODO rewrite splits input variable, and fuse splits that have the
            // same input variable
            auto it = curr->u_par().branches.begin();
            for (size_t i = 0; i < curr->u_par().branches.size(); ++i) {
                hassert(it != curr->u_par().branches.end());
                auto &branch = *it;

                // As the splits are sorted, if {A, B} both have the same
                // pre_id, we can combine A into B. This will merge all the
                // branches
                std::unordered_map<VarId, VarId>
                    new_branch_id_from_old_branch_id;
                for (size_t j = 1; j < curr->u_par().splits.size(); ++j) {
                    auto &prev_split = curr->u_par().splits[j - 1];
                    auto &curr_split = curr->u_par().splits[j];
                    if (prev_split.pre_id == curr_split.pre_id) {
                        if (prev_split.branch_ids[i] &&
                            curr_split.branch_ids[i]) {
                            new_branch_id_from_old_branch_id
                                [*prev_split.branch_ids[i]] =
                                    *curr_split.branch_ids[i];
                            prev_split.branch_ids[i] = OptionalVarId::null_id();
                        } else if (prev_split.branch_ids[i] &&
                                   !curr_split.branch_ids[i]) {
                            // move the id into the second split
                            curr_split.branch_ids[i] = prev_split.branch_ids[i];
                            prev_split.branch_ids[i] = OptionalVarId::null_id();
                        }
                    }
                }

                auto branch_remaps = remove_copies(
                    branch, std::move(new_branch_id_from_old_branch_id),
                    changed);

                for (auto &merge : curr->u_par().merges) {
                    if (merge.branch_ids[i])
                        remap_opt_with(branch_remaps, merge.branch_ids[i]);
                }

                ++it;
            }
            hassert(it == curr->u_par().branches.end());

            // now combine merge statements that have the same set of inputs
            {
                std::unordered_map<std::vector<OptionalVarId>, VarId>
                    out_id_from_branch_ids;
                for (auto &merge : curr->u_par().merges) {
                    if (!out_id_from_branch_ids.contains(merge.branch_ids))
                        out_id_from_branch_ids[merge.branch_ids] =
                            merge.post_id;
                    else
                        new_id_from_old_id[merge.post_id] =
                            out_id_from_branch_ids.at(merge.branch_ids);
                }
            }

            // fuse split-merge pairs where on the writing path the split gets
            // passed along to the merge
            {
                std::unordered_map<VarId, VarId> pre_id_from_split_id;
                for (auto &split : curr->u_par().splits) {
                    for (const auto &branch_id : split.branch_ids) {
                        if (branch_id)
                            pre_id_from_split_id[*branch_id] = split.pre_id;
                    }
                }
                for (auto &merge : curr->u_par().merges) {
                    hassert(Algo::count_if(merge.branch_ids,
                                           [](const OptionalVarId &id) {
                                               return (bool)id;
                                           }) == 1);
                    VarId branch_id = **Algo::find_assert_if(
                        merge.branch_ids,
                        [](const OptionalVarId &id) { return (bool)id; });
                    if (pre_id_from_split_id.contains(branch_id))
                        new_id_from_old_id[merge.post_id] =
                            pre_id_from_split_id.at(branch_id);
                }
            }

            // TODO a merge is a copy if there is a (split, merge, branch_idx)
            // pair where the split gets passed
            //  along to the merge
            break;
        }
        case BlockType::Select: {
            for (auto &splits : curr->u_select().splits)
                remap(splits.pre_id);
            for (auto &branch : curr->u_select().branches) {
                if (branch.g.type() == IRGuardType::Expression)
                    remap_expr(branch.g.u_e().e.m_dag);
            }

            // First sort the splits. It is stable so it is deterministic across
            // platforms
            std::stable_sort(curr->u_select().splits.begin(),
                             curr->u_select().splits.end(),
                             [](const auto &a, const auto &b) {
                                 return a.pre_id < b.pre_id;
                             });

            auto it = curr->u_select().branches.begin();
            for (size_t i = 0; i < curr->u_select().branches.size(); ++i) {
                hassert(it != curr->u_select().branches.end());
                auto &branch = *it;

                // As the splits are sorted, if {A, B} both have the same
                // pre_id, we can combine A into B. This will merge all the
                // branches
                std::unordered_map<VarId, VarId>
                    new_branch_id_from_old_branch_id;
                for (size_t j = 1; j < curr->u_select().splits.size(); ++j) {
                    auto &prev_split = curr->u_select().splits[j - 1];
                    auto &curr_split = curr->u_select().splits[j];
                    if (prev_split.pre_id == curr_split.pre_id) {
                        if (prev_split.branch_ids[i] &&
                            curr_split.branch_ids[i]) {
                            new_branch_id_from_old_branch_id
                                [*prev_split.branch_ids[i]] =
                                    *curr_split.branch_ids[i];
                            prev_split.branch_ids[i] = OptionalVarId::null_id();
                        } else if (prev_split.branch_ids[i] &&
                                   !curr_split.branch_ids[i]) {
                            // move the id into the second split
                            curr_split.branch_ids[i] = prev_split.branch_ids[i];
                            prev_split.branch_ids[i] = OptionalVarId::null_id();
                        }
                    }
                }

                auto branch_remaps = remove_copies(
                    branch.seq, std::move(new_branch_id_from_old_branch_id),
                    changed);
                for (auto &merge : curr->u_select().merges)
                    remap_with(branch_remaps, merge.branch_ids[i]);

                ++it;
            }
            hassert(it == curr->u_select().branches.end());

            // now combine merge statements that have the same set of inputs
            std::unordered_map<std::vector<VarId>, VarId>
                out_id_from_branch_ids;
            for (auto &merge : curr->u_select().merges) {
                if (!out_id_from_branch_ids.contains(merge.branch_ids))
                    out_id_from_branch_ids[merge.branch_ids] = merge.post_id;
                else
                    new_id_from_old_id[merge.post_id] =
                        out_id_from_branch_ids.at(merge.branch_ids);
            }

            // fuse split-merge pairs where every merge path connects to a split
            // from the same pre_id
            {
                std::unordered_map<VarId, VarId> pre_id_from_split_id;
                for (auto &split : curr->u_select().splits) {
                    for (const auto &branch_id : split.branch_ids) {
                        if (branch_id)
                            pre_id_from_split_id[*branch_id] = split.pre_id;
                    }
                }
                for (auto &merge : curr->u_select().merges) {
                    if (pre_id_from_split_id.contains(merge.branch_ids[0]) &&
                        Algo::all_of(
                            merge.branch_ids, [&](const auto &branch_id) {
                                return pre_id_from_split_id.contains(
                                           branch_id) &&
                                       pre_id_from_split_id.at(branch_id) ==
                                           pre_id_from_split_id.at(
                                               merge.branch_ids[0]);
                            })) {
                        new_id_from_old_id[merge.post_id] =
                            pre_id_from_split_id.at(merge.branch_ids[0]);
                    }
                }
            }

            break;
        }
        case BlockType::DoLoop: {
            // TODO rewrite in_phi and loop_phi input variables
            // TODO fuse in_phi that have the same input variable
            // TODO fuse loop_phi if they do the same computation twice (HOW TO
            // DETECT THIS)

            for (auto &phi : curr->u_doloop().in_phis)
                remap(phi.pre_id);
            for (auto &phi : curr->u_doloop().loop_phis)
                remap(phi.pre_id);

            std::unordered_map<VarId, VarId> branch_id_from_pre_id;
            std::unordered_map<VarId, VarId> new_branch_id_from_old_branch_id;
            for (auto &phi : curr->u_doloop().in_phis) {
                if (!branch_id_from_pre_id.contains(phi.pre_id))
                    branch_id_from_pre_id[phi.pre_id] = phi.bodyin_id;
                else
                    new_branch_id_from_old_branch_id[phi.bodyin_id] =
                        branch_id_from_pre_id.at(phi.pre_id);
            }
            // TODO handle loop_phis?

            auto branch_remaps = remove_copies(
                curr->u_doloop().branch,
                std::move(new_branch_id_from_old_branch_id), changed);

            for (auto &phi : curr->u_doloop().out_phis)
                remap_with(branch_remaps, phi.bodyout_id);
            for (auto &phi : curr->u_doloop().loop_phis)
                remap_with(branch_remaps, phi.bodyout_id);
            remap_expr_with(branch_remaps, curr->u_doloop().guard.m_dag);

            // now combine out_phis statements that have the same set of inputs
            std::unordered_map<VarId, VarId> out_id_from_branch_ids;
            for (auto &phi : curr->u_doloop().out_phis) {
                if (!out_id_from_branch_ids.contains(phi.bodyout_id))
                    out_id_from_branch_ids[phi.bodyout_id] = phi.post_id;
                else
                    new_id_from_old_id[phi.post_id] =
                        out_id_from_branch_ids.at(phi.bodyout_id);
            }
            // TODO handle loop_phis?

            break;
        }
        case BlockType::StartSequence:
        case BlockType::EndSequence:
            break;
        }
    }
    return std::move(new_id_from_old_id);
}
} // namespace

bool eliminateCopies(ChpGraph &graph) {
    bool changed = false;
    remove_copies(graph.m_seq, {}, changed);
    graph.validateGraphInvariants();

    //        print_chp(std::cerr, graph);
    //        std::cerr << std::endl<<std::endl;
    return changed;
}

} // namespace ChpOptimize
