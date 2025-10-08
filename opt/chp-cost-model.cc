/*************************************************************************
 *
 *  This file is part of the ACT library
 *
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

#include "chp-cost-model.h"
#include "chp-graph.h"
#include "algos.h"

namespace ChpOptimize {
namespace {

// charge for the width of each var, except if a var node is not a root and has
// a bitfield as its parent, in which case charge for the subset of the bitfield
// overlying the var
ChpCosts cost_of_expr(const ChpExprDag &dag, const CostModel &model,
                      const IdPool &id_pool) {
    using Node = ChpExprDag::Node;
    std::unordered_set<const Node *> vars;
    std::unordered_set<const Node *> bitfield_above_var;
    ChpExprDag::iterNodes(dag, [&](const Node &node) {
        if (node.type() == IRExprTypeKind::Var) {
            vars.insert(&node);
        } else if (node.type() == IRExprTypeKind::Bitfield &&
                   node.u_bitfield().e->type() == IRExprTypeKind::Var) {
            bitfield_above_var.insert(&node);
        }
    });

    for (const Node *bf : bitfield_above_var) {
        const Node *var = bf->u_bitfield().e;
        if (!Algo::contains(dag.roots, var)) {
            vars.erase(var);
        }
    }

    // TODO do this using a bit vector
    std::unordered_map<VarId, std::vector<BitSlice>> var_reads;
    for (const Node *var : vars) {
        var_reads[var->u_var().id].emplace_back(
            id_pool.getBitwidth(var->u_var().id) - 1, 0);
    }
    for (const Node *bf : bitfield_above_var) {
        var_reads[bf->u_bitfield().e->u_var().id].push_back(
            bf->u_bitfield().slice);
    }

    int read_bits = 0;
    for (const auto &[id, slices] : var_reads) {
        for (int i = 0; i <= id_pool.getBitwidth(id) - 1; ++i) {
            if (Algo::any_of(slices, [&](const auto &slice) {
                    return slice.lo() <= i && i <= slice.hi();
                })) {
                read_bits++;
            }
        }
    }

    return model.cost_read(read_bits);
}
ChpCosts cost_of_expr(const ChpExprSingleRootDag &dag, const CostModel &model,
                      const IdPool &id_pool) {
    return cost_of_expr(dag.m_dag, model, id_pool);
}

ChpCosts cost_of_seq(const Sequence &seq, const CostModel &model,
                     const IdPool &id_pool) {
    ChpCosts result;
    Block *curr = seq.startseq->child();
    while (curr->type() != BlockType::EndSequence) {
        switch (curr->type()) {
        case BlockType::StartSequence:
        case BlockType::EndSequence:
            hassert(false);
            break;
        case BlockType::Basic: {
            switch (curr->u_basic().stmt.type()) {
            case StatementType::Assign: {
                // Assignment: x := E
                // cost_write(width(x)) + cost_expr (E) +
                // cost_transfer(width(x), width(E)) + (x appears in E ?
                // cost_var(width(x)) : 0)
                result += cost_of_expr(curr->u_basic().stmt.u_assign().e, model,
                                       id_pool);
                auto expr_ids =
                    getIdsUsedByExpr(curr->u_basic().stmt.u_assign().e);
                hassert(curr->u_basic().stmt.u_assign().ids.size() ==
                        curr->u_basic().stmt.u_assign().e.roots.size());
                for (int i = 0;
                     i < (int)curr->u_basic().stmt.u_assign().ids.size(); ++i) {
                    const auto &id = curr->u_basic().stmt.u_assign().ids[i];
                    const auto &root =
                        curr->u_basic().stmt.u_assign().e.roots[i];
                    int id_width = id_pool.getBitwidth(id);

                    result += model.cost_transfer(id_width, root->width);
                    result += model.cost_write(id_width);

                    if (expr_ids.count(id)) {
                        // then we need to move the variable to a temporary
                        result += model.cost_var(id_width);
                        result += model.cost_read(id_width);
                        result += model.cost_write(id_width);
                    }
                }
                break;
            }
            case StatementType::Send: {
                // Send: C!E
                // cost_expr(E) + cost_transfer(width(C), width(E)) +
                // cost_out_port(width(C))
                int chan_width =
                    id_pool.getBitwidth(curr->u_basic().stmt.u_send().chan);
                const auto &e = curr->u_basic().stmt.u_send().e;
                result += cost_of_expr(e, model, id_pool);
                result += model.cost_transfer(chan_width, e.width());

                result += model.cost_out_port(chan_width);
                break;
            }
            case StatementType::Receive:
                // Receive: C?x
                // cost_receive(width(x)) + cost_in_port(width(C))
                int chan_width =
                    id_pool.getBitwidth(curr->u_basic().stmt.u_receive().chan);
                result += model.cost_in_port(chan_width);
                if (curr->u_basic().stmt.u_receive().var) {
                    int var_width = id_pool.getBitwidth(
                        *curr->u_basic().stmt.u_receive().var);
                    result += model.cost_receive(var_width);
                    // result += model.cost_write(var_width);
                }
                break;
            }
            break;
        }
        case BlockType::Par:
            // Parallel: S1,S2,...,Sn
            // For area:   sum(cost(S).area for S in {S1,...,Sn}) +
            // cost_parallel(n).area For delay:  max(cost(S).area for S in
            // {S1,...,Sn}) + cost_parallel(n).delay
            for (const auto &path : curr->u_par().branches) {
                auto cost_branch = cost_of_seq(path, model, id_pool);
                result += cost_branch;
            }
            result += model.cost_celem((int)curr->u_par().branches.size());
            break;
        case BlockType::Select:
            // Selection: n-way sequential
            // For area:   sum(cost(G).area for G in {G1,...,Gn}) +
            // sum(cost(G).area for S in {G1,...,Gn}) +
            //             n * cost_var(width=1) +  cost_or(n).area
            // For delay:  TODO
            //             in the 2-way case we have
            //             cost(G1) + p * cost(S1) + (1-p)*cost (G2) + (1-p) *
            //             cost (S2) + cost(“or”, 2)
            hassert(curr->u_select().splits.empty());
            hassert(curr->u_select().merges.empty());
            for (const auto &branch : curr->u_select().branches) {
                if (branch.g.type() == IRGuardType::Expression) {
                    if (branch.g.u_e().e.root()->type() ==
                        IRExprTypeKind::Const) {
                        // there is no cost
                    } else {
                        result += model.cost_var(1);
                        result += model.cost_write(1);
                        result += model.cost_read(1);
                        auto cost_guard =
                            cost_of_expr(branch.g.u_e().e, model, id_pool);
                        result += cost_guard;
                    }
                    // add a special case for 1-bit variables that dont change
                    // in any branch
                } else {
                    // Else's are free because they are just a wire
                    hassert(branch.g.type() == IRGuardType::Else);
                }
                auto cost_branch = cost_of_seq(branch.seq, model, id_pool);
                result += cost_branch;
            }
            result += model.cost_or((int)curr->u_select().branches.size());
            break;
        case BlockType::DoLoop:
            hassert(curr->u_doloop().in_phis.empty());
            hassert(curr->u_doloop().out_phis.empty());
            hassert(curr->u_doloop().loop_phis.empty());
            if (curr->u_doloop().guard.root()->type() ==
                IRExprTypeKind::Const) {
                // there is no cost
            } else {
                result += model.cost_var(1);
                result += model.cost_write(1);
                result += model.cost_read(1);
                auto cost_guard =
                    cost_of_expr(curr->u_doloop().guard, model, id_pool);
                result += cost_guard;
            }
            auto cost_branch =
                cost_of_seq(curr->u_doloop().branch, model, id_pool);
            result += cost_branch;
            result += model.cost_not_or();
            break;
        }
        curr = curr->child();
        if (curr->type() != BlockType::EndSequence) {
            // Semicolon: S1; S2
            // cost(S1) + cost(S2) + cost(“sequencer”)
            result += model.cost_sequencer();
        }
    }
    return result;
}
} // namespace
ChpCosts cost_of_chp(const ChpGraph &g, const CostModel &model) {
    hassert(!g.is_static_token_form);
    ChpCosts vars_cost =
        Algo::sum<ChpCosts>(g.allUsedVarIds(), [&](const VarId &id) {
            return model.cost_var(g.id_pool().getBitwidth(id));
        });
    ChpCosts code_cost = cost_of_seq(g.m_seq, model, g.id_pool());
    return vars_cost + code_cost;
}

std::ostream &operator<<(std::ostream &o, const ChpCosts &costs) {
    o << "Area:            " << costs.area << std::endl;
    o << "Var Bits:        " << costs.ct_one_bit_var << std::endl;
    o << "Var Reads:       " << costs.ct_one_bit_read << std::endl;
    o << "Var Writes:      " << costs.ct_one_bit_write << std::endl;
    o << "In Port Bits:    " << costs.ct_one_bit_in_port << std::endl;
    o << "Out Port Bits:   " << costs.ct_one_bit_out_port << std::endl;
    o << "Receive Bits:    " << costs.ct_one_bit_receive << std::endl;
    o << "Two Input Or:    " << costs.ct_two_input_or << std::endl;
    o << "Two Input Celem: " << costs.ct_two_input_celem << std::endl;
    o << "Not Or:          " << costs.ct_not_or << std::endl;
    o << "Sequencers:      " << costs.ct_sequencer << std::endl;
    return o;
}

} // namespace ChpOptimize
