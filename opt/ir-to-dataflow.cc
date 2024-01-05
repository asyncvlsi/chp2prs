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

#include "ir-to-dataflow.h"
#include "chp-print.h"
#include "algos.h"
#include "utils.h"
#include <functional>

namespace ChpOptimize {
namespace {

  
/*
 * This holds the mapping from variables to channels, and from
 * channels to channels for multiple channel access.
 */
struct DataflowChannelManager {
  /*--- For multi-channel access management ---*/

  /*
    Original CHP channel to current dataflow element that generates the
    latest verion of the channel. Note that we need the ChanId in the
    map because a split has multiple outputs; otherwise the dataflow
    element itself would suffice.
  */
  std::unordered_map<ChanId, std::pair<Dataflow *, ChanId> > curr_out;

  /*
    If the channel output is variable, we need the control channel
    mapping. The argument to this map is the *current name* of the
    channel, and the map returns the variable control token sequence
    for the channel (the 0/1/2 sequence).
  */
  std::unordered_map<ChanId, ChanId> mapped_ctrl;


  /*--- for variable to channel map ---*/

  /*
    This holds the map from the optimized variables to channels
  */
  std::unordered_map<VarId,ChanId> varmap;
  IdPool *id_pool;

  /*
    Outermost block for a channel
  */
  std::unordered_map<ChanId, Block *> chanmap;
};


ChanId dflow_map(VarId v, DataflowChannelManager &maps)
{
  if (!maps.varmap.contains(v)) {
    maps.varmap[v] =
      maps.id_pool->makeUniqueChan (maps.id_pool->getBitwidth (v));
  }
  return maps.varmap[v];
}
						  
/* 
   Take a ChpExprDag that uses variables and convert it to a 
   dataflow dag, remaing variables to channels.
*/
DExprDag of_chp_dag(const ChpExprDag &dag, DataflowChannelManager &maps)
{
    DExprDag ddag;
    std::unordered_map<const ChpExprDag::Node *, DExprDag::Node *> mp;
    ChpExprDag::iterNodes(dag, [&](const ChpExprDag::Node &n) {
        switch (n.type()) {
        case IRExprTypeKind::BinaryOp:
            mp[&n] = ddag.newNode(DExprDag::Node::makeBinaryOp(
                n.u_e2().op_type, mp.at(n.u_e2().l), mp.at(n.u_e2().r)));
            break;
        case IRExprTypeKind::UnaryOp:
            mp[&n] = ddag.newNode(DExprDag::Node::makeUnaryOp(
                n.u_e1().op_type, mp.at(n.u_e1().l)));
            break;
        case IRExprTypeKind::Query:
            mp[&n] = ddag.newNode(DExprDag::Node::makeQuery(
                mp.at(n.u_query().selector), mp.at(n.u_query().l),
                mp.at(n.u_query().r)));
            break;
        case IRExprTypeKind::Const:
            mp[&n] = ddag.newNode(
                DExprDag::Node::makeConstant(n.u_cons().v, n.u_cons().v_width));
            break;
        case IRExprTypeKind::Var:
	    mp[&n] =
	      ddag.newNode(DExprDag::Node::makeVariableAccess(
		      dflow_map (n.u_var().id, maps), n.width));;
            break;
        case IRExprTypeKind::Bitfield:
            mp[&n] = ddag.newNode(DExprDag::Node::makeBitfield(
                mp.at(n.u_bitfield().e), n.u_bitfield().hi(),
                n.u_bitfield().lo()));
            break;
        }
    });
    for (const auto &root : dag.roots)
        ddag.roots.push_back(mp.at(root));
    return ddag;
}

/*
  How many bits do we need for the guard?
*/
int select_guard_width(const Block::Variant_Select &select) {
    return log_2_round_up(select.branches.size());
}

#if 0 
MultiChanAccessHandler make_multi_chan_access_handler(const ChpGraph &g,
                                                      IdPool &id_pool) {
    // Collect all the ChanIds that are used.  We use a std::set because we want
    // the order to be deterministic. If it turns out to matter for performance,
    // we could make this a flat_hash_map, and then sort it afterwords
    std::set<ChanId> send_chan_ids;
    std::set<ChanId> recv_chan_ids;
    ChpGraph::iter_blocks(g, [&](const Block *curr) {
        switch (curr->type()) {
        case BlockType::Basic:
            switch (curr->u_basic().stmt.type()) {
            case StatementType::Assign:
                break;
            case StatementType::Send:
                send_chan_ids.insert(curr->u_basic().stmt.u_send().chan);
                break;
            case StatementType::Receive:
                recv_chan_ids.insert(curr->u_basic().stmt.u_receive().chan);
                break;
            }
            break;
        case BlockType::Par:
        case BlockType::Select:
        case BlockType::DoLoop:
        case BlockType::StartSequence:
        case BlockType::EndSequence:
            break;
        }
    });

    assert(Algo::none_of(send_chan_ids, [&](const auto &id) {
        return recv_chan_ids.contains(id);
    }));

    MultiChanAccessHandler handler;
    for (const auto &chan_id : send_chan_ids) {
        auto result = build_chan_ctrl_structure(
            handler, {chan_id, g.id_pool().getBitwidth(chan_id)}, id_pool,
            g.m_seq);
        assert(result);
        handler.writable_channels[chan_id] = result->A;
    }
    for (const auto &chan_id : recv_chan_ids) {
        auto result = build_chan_ctrl_structure(
            handler, {chan_id, g.id_pool().getBitwidth(chan_id)}, id_pool,
            g.m_seq);
        assert(result);
        handler.readable_channels[chan_id] = result->A;
    }
    return handler;
}

struct FuncNode {
    DVarId output;
    DExpr e;
};
struct MergeNode {
    DVarId ctrl;
    std::vector<DVarId> inputs;
    DVarId output;
};
struct SplitNode {
    DVarId ctrl;
    DVarId input;
    std::vector<DVarId> outputs;
};
struct DupNode {
    DVarId orig, output;
};
// Initializes the "output" with a "zero" token, and copies tokens from orig to
// output
struct DupInitNode {
    DVarId orig, output;
};

struct DFlatNodes {
    std::vector<FuncNode> funcs;
    std::vector<MergeNode> merges;
    std::vector<SplitNode> splits;
    std::vector<DupNode> dups;
    std::vector<DupInitNode> dup_inits;

    std::vector<std::pair<ChanId, DVarId>> readable_channels;
    std::vector<std::pair<ChanId, DVarId>> writable_channels;
};

void nodes_add_assign(DFlatNodes &nodes, const ChpExprDag &chp_dag,
                      const std::vector<DVarId> &outputs, DVarIdPool &id_pool) {
    hassert(chp_dag.roots.size() == outputs.size());
    hassert(!chp_dag.roots.empty());

    // First, create a token id for each dag node
    const auto dag = of_chp_dag(chp_dag, id_pool);
    std::unordered_map<const DExprDag ::Node *, DVarId> node_ids;
    DExprDag ::iterNodes(dag, [&](const auto &node) {
        node_ids[&node] = id_pool.new_id(node.width);
    });

    DExprDag ::iterNodes(dag, [&](const DExprDag::Node &node) {
        auto e = [&]() {
            auto var_read = [&](const DExprDag::Node *n) {
                return std::make_unique<DExpr>(
                    DExpr::makeVariableAccess(node_ids.at(n), n->width));
            };
            switch (node.type()) {
            case IRExprTypeKind::BinaryOp:
                return DExpr ::makeBinaryOp(node.u_e2().op_type,
                                            var_read(node.u_e2().l),
                                            var_read(node.u_e2().r));
            case IRExprTypeKind::UnaryOp:
                return DExpr ::makeUnaryOp(node.u_e1().op_type,
                                           var_read(node.u_e1().l));
            case IRExprTypeKind::Query:
                return DExpr ::makeQuery(var_read(node.u_query().selector),
                                         var_read(node.u_query().l),
                                         var_read(node.u_query().r));
            case IRExprTypeKind::Const:
                return DExpr ::makeConstant(node.u_cons().v,
                                            node.u_cons().v_width);
            case IRExprTypeKind::Var:
                return DExpr ::makeVariableAccess(node.u_var().id, node.width);
            case IRExprTypeKind::Bitfield:
                return DExpr ::makeBitfield(var_read(node.u_bitfield().e),
                                            node.u_bitfield().hi(),
                                            node.u_bitfield().lo());
            }
        }();

        nodes.funcs.push_back(FuncNode{node_ids.at(&node), std::move(e)});
    });

    // in a separate loop because the same node might appear as a root more than
    // once
    for (int i = 0; i < (ssize_t)dag.roots.size(); ++i)
        nodes.dups.push_back({node_ids.at(dag.roots[i]), outputs[i]});
}

void nodes_add_assign(DFlatNodes &nodes, const ChpExprDag &dag,
                      const std::vector<VarId> &outputs, DVarIdPool &id_pool) {
    return nodes_add_assign(
        nodes, dag,
        Algo::map1<DVarId>(outputs,
                           [&](const auto &id) { return id_pool.at(id); }),
        id_pool);
}

DVarId nodes_add_select_guard(DFlatNodes &nodes,
                              const Block::Variant_Select &select,
                              DVarIdPool &id_pool) {
    hassert(Algo::count_if(select.branches, [&](const SelectBranch &branch) {
                return branch.g.type() == IRGuardType::Else;
            }) <= 1);

    // first find the else branch (if there is one). Otherwise, since there are
    // no probes, promote the first branch to be the "else" case
    auto else_branch =
        Algo::find_if(select.branches, [&](const SelectBranch &branch) {
            return branch.g.type() == IRGuardType::Else;
        });
    if (else_branch == select.branches.end())
        else_branch = select.branches.begin();

    int else_branch_idx =
        (int)std::distance(select.branches.begin(), else_branch);
    int width = select_guard_width(select);

    DExprDag guard_dag;
    DExprDag::Node *guard_dag_root = guard_dag.newNode(
        DExprDag::Node::makeConstant(BigInt(else_branch_idx), width));

    int idx = -1;
    for (const auto &branch : select.branches) {
        idx++;
        if (&branch == &*else_branch)
            continue;
        hassert(branch.g.type() != IRGuardType::Else);
        hassert(branch.g.type() == IRGuardType::Expression);
        auto tmp_id = id_pool.new_id(1);
        nodes_add_assign(nodes, branch.g.u_e().e.m_dag, {tmp_id}, id_pool);
        guard_dag_root = guard_dag.newNode(DExprDag::Node::makeQuery(
            guard_dag.newNode(DExprDag::Node::makeVariableAccess(tmp_id, 1)),
            guard_dag.newNode(DExprDag::Node::makeConstant(BigInt(idx), width)),
            guard_dag_root));
    }

    guard_dag.roots.push_back(guard_dag_root);

    auto guard_id = id_pool.new_id(width);
    nodes.funcs.push_back(
        {guard_id, DExprSingleRootDag ::to_expr(
                       DExprSingleRootDag(std::move(guard_dag)))});
    return guard_id;
}

struct DoLoopGuardVar {
    DVarId pre_assign_id;
    DVarId post_assign_id;
};
 
DoLoopGuardVar nodes_add_doloop_guard(DFlatNodes &nodes,
                                      const Block::Variant_DoLoop &do_loop,
                                      DVarIdPool &id_pool) {
    DoLoopGuardVar guard;
    guard.pre_assign_id = id_pool.new_id(1);
    guard.post_assign_id = id_pool.new_id(1);
    nodes.dup_inits.push_back(
        DupInitNode{guard.post_assign_id, guard.pre_assign_id});

    nodes_add_assign(nodes, do_loop.guard.m_dag, {guard.post_assign_id},
                     id_pool);
    return guard;
}

// TODO we probably dont need the seq_ctrl_tok_id everywhere (but maybe can
// leave it to the optimizer to strip out)?
void nodes_add_seq(DFlatNodes &nodes, DVarId seq_ctrl_tok_id,
                   const Sequence &seq,
                   const MultiChanAccessHandler &multichan_handler,
                   DVarIdPool &id_pool) {

    auto nodes_add_seq_ = [&](DVarId seq_ctrl_tok_id, const Sequence &seq) {
        nodes_add_seq(nodes, seq_ctrl_tok_id, seq, multichan_handler, id_pool);
    };

    // first we iterate through and gather all the variable uses. For parallel
    // blocks, we  add a copy node for both the split and the merge. If this
    // matters, the dataflow optimizer can clean it up
    for (const Block *curr = seq.startseq->child(); curr != seq.endseq;
         curr = curr->child()) {
        switch (curr->type()) {
        case BlockType::Basic:
            switch (curr->u_basic().stmt.type()) {
            case StatementType::Assign: {
                // TODO is it a problem if an assign does not have any data
                // dependencies?
                const auto &assign = curr->u_basic().stmt.u_assign();
                nodes_add_assign(nodes, assign.e, assign.ids, id_pool);
                break;
            }
            case StatementType::Send: {
                // TODO is it a problem if an assign does not have any data
                // dependencies?
                const auto &send = curr->u_basic().stmt.u_send();
                auto tmp_id = id_pool.new_id(send.e.width());
                nodes_add_assign(nodes, send.e.m_dag, {tmp_id}, id_pool);
                // We replace the operator `A!x` with `A_0!x` where A_0 is the
                // channel alias used by this block
                nodes.dups.push_back({
                    tmp_id,
                    multichan_handler.renames.at(curr).chan_alias,
                });
                break;
            }
            case StatementType::Receive:
                const auto &receive = curr->u_basic().stmt.u_receive();
                // We replace the operator `A?x` with `A_0?x` where A_0 is the
                // channel alias used by this block. If there is no variable,
                // then we dont copy the token anywhere. In the second pass, we
                // will add a sink onto this channel
                if (receive.var)
                    nodes.dups.push_back(
                        {multichan_handler.renames.at(curr).chan_alias,
                         id_pool.at(*receive.var)});
                break;
            }
            break;
        case BlockType::Par:
            // There is nothing special to handel in a parallel block with
            // respect to the multichan_handler
            for (const auto &phi : curr->u_par().splits) {
                for (const auto &id : phi.branch_ids) {
                    if (id)
                        nodes.dups.push_back(
                            DupNode{id_pool.at(phi.pre_id), id_pool.at(*id)});
                }
            }
            for (const auto &phi : curr->u_par().merges) {
                hassert(Algo::count_if(phi.branch_ids, [&](const auto &id) {
                            return static_cast<bool>(id);
                        }) == 1);
                for (const auto &id : phi.branch_ids) {
                    if (id)
                        nodes.dups.push_back(
                            DupNode{id_pool.at(*id), id_pool.at(phi.post_id)});
                }
            }
            for (const auto &branch : curr->u_par().branches) {
                nodes_add_seq_(seq_ctrl_tok_id, branch);
            }
            break;
        case BlockType::Select: {
            // TODO is it a problem if the guard does not have any "immediate"
            // data dependencies.

            auto guard_var_id =
                nodes_add_select_guard(nodes, curr->u_select(), id_pool);

            // For a select block, we need send the guard to each of the
            // select_listeners.
            for (const auto &listener :
                 multichan_handler.select_listeners.at(curr))
                nodes.dups.push_back({guard_var_id, listener});

            auto branch_ctrl_tok_ids = Algo::map1<DVarId>(
                curr->u_select().branches,
                [&](const auto &) { return id_pool.new_ctrl_tok(); });
            nodes.splits.push_back(
                {guard_var_id, seq_ctrl_tok_id, branch_ctrl_tok_ids});

            // then add nodes for the phi functions
            for (const auto &phi : curr->u_select().splits) {
                nodes.splits.push_back(
                    {guard_var_id, id_pool.at(phi.pre_id),
                     Algo::map1<DVarId>(phi.branch_ids, [&](const auto id) {
                         if (id)
                             return id_pool.at(*id);
                         return id_pool.new_id_with_width_of(phi.pre_id);
                     })});
            }
            for (const auto &phi : curr->u_select().merges) {
                nodes.merges.push_back(
                    {guard_var_id,
                     Algo::map1<DVarId>(
                         phi.branch_ids,
                         [&](const auto id) { return id_pool.at(id); }),
                     id_pool.at(phi.post_id)});
            }

            // then add nodes for the branches
            int idx = 0;
            for (const auto &branch : curr->u_select().branches) {
                nodes_add_seq_(branch_ctrl_tok_ids[idx], branch.seq);
                idx++;
            }
            break;
        }
        case BlockType::DoLoop: {
            auto guard_var_id =
                nodes_add_doloop_guard(nodes, curr->u_doloop(), id_pool);
            auto branch_ctrl_tok_id = id_pool.new_ctrl_tok();
            // First add the control token flow logic
            {
                auto tmp_loop_id = id_pool.new_ctrl_tok();
                auto tmp_dead_id = id_pool.new_ctrl_tok();
                nodes.merges.push_back({guard_var_id.pre_assign_id,
                                        {seq_ctrl_tok_id, tmp_loop_id},
                                        branch_ctrl_tok_id});
                nodes.splits.push_back({guard_var_id.post_assign_id,
                                        branch_ctrl_tok_id,
                                        {tmp_dead_id, tmp_loop_id}});
            }

            // For a do loop block, we need send the guard to each of the
            // doloop_listeners, and also send a "1" token right before the loop
            // runs.
            for (const auto &listener :
                 multichan_handler.doloop_listeners.at(curr)) {
                nodes.dups.push_back({guard_var_id.post_assign_id, listener});
            }

            // A do loop has "merges" at the top and "splits" at the bottom, and
            // an initializer on its guard variable
            // - For the merges (at the top), the guard var is 0 if it is
            // comming from before the loop, and 1 if it is coming as a
            // "repeated" iteration of the loop
            //
            // - For the splits (at the bottom), the guard is 0 if we are
            // exiting the loop, and 1 if we are going to loop again
            for (const auto &phi : curr->u_doloop().in_phis) {
                // This phi function will take on the form
                // *[ bodyin_id = merge(guard, pre_id, tmp_loop_id);
                //    ...;
                //    guard = COMPUTE_GUARD;
                //    (tmp_dead_id, tmp_loop_id) = split(guard, bodyin_id);
                //    <- guard
                //  ]
                auto tmp_loop_id = id_pool.new_id_with_width_of(phi.pre_id);
                auto tmp_dead_id = id_pool.new_id_with_width_of(phi.pre_id);
                nodes.merges.push_back({guard_var_id.pre_assign_id,
                                        {id_pool.at(phi.pre_id), tmp_loop_id},
                                        id_pool.at(phi.bodyin_id)});
                nodes.splits.push_back({guard_var_id.post_assign_id,
                                        id_pool.at(phi.bodyin_id),
                                        {tmp_dead_id, tmp_loop_id}});
            }
            for (const auto &phi : curr->u_doloop().out_phis) {
                // This phi function will take on the form
                // *[ ...;
                //    guard = COMPUTE_GUARD;
                //    (post_id, tmp_dead_id) = split(guard, bodyout_id);
                //    <- guard
                //  ]
                auto tmp_dead_id = id_pool.new_id_with_width_of(phi.post_id);
                nodes.splits.push_back(
                    {guard_var_id.post_assign_id,
                     id_pool.at(phi.bodyout_id),
                     {id_pool.at(phi.post_id), tmp_dead_id}});
            }
            for (const auto &phi : curr->u_doloop().loop_phis) {
                // This phi function will take on the form
                // *[ bodyin_id = merge(guard, pre_id, tmp_loop_id);
                //    ...;
                //    guard = COMPUTE_GUARD;
                //    (post_id, tmp_loop_id) = split(guard, bodyin_id);
                //    <- guard
                //  ]
                auto tmp_loop_id = id_pool.new_id_with_width_of(phi.pre_id);
                auto post_id =
                    phi.post_id ? id_pool.at(*phi.post_id)
                                : id_pool.new_id_with_width_of(phi.bodyout_id);
                nodes.merges.push_back({guard_var_id.pre_assign_id,
                                        {id_pool.at(phi.pre_id), tmp_loop_id},
                                        id_pool.at(phi.bodyin_id)});
                nodes.splits.push_back({guard_var_id.post_assign_id,
                                        id_pool.at(phi.bodyin_id),
                                        {post_id, tmp_loop_id}});
            }
            nodes_add_seq_(branch_ctrl_tok_id, curr->u_doloop().branch);
            break;
        }
        case BlockType::StartSequence:
        case BlockType::EndSequence:
            hassert(false);
            break;
        }
    }
}

void add_multichan_handler_control_processes(
    DFlatNodes &nodes, const MultiChanAccessHandler &handler,
    DVarIdPool &id_pool) {
    auto inv_expr = [&](DVarId id) {
        return DExpr::makeUnaryOp(
            IRUnaryOpType::Not,
            std::make_unique<DExpr>(DExpr::makeVariableAccess(id, 1)));
    };
    auto drop = [&](int width) { return id_pool.new_id(width); };

    // We want to add the process *[B!0; B!1]
    // To do this, we add the process `s-; *[B!s; s:= ~s]`
    for (const auto &[A_is_send, A_width, A, B] : handler.base_cases) {
        auto s_in = id_pool.new_id(1);
        auto s_out = id_pool.new_id(1);
        nodes.dups.push_back({s_in, B});
        nodes.funcs.push_back({s_out, inv_expr(s_in)});
        nodes.dup_inits.push_back({s_out, s_in});
    }

    // handle the `S0; S1` case
    for (const auto &[A_is_send, A_width, A_1, B_1, A_2, B_2, A, B] :
         handler.seq_cases) {
        if (A_is_send) {
            auto s_in = id_pool.new_id(1);
            auto s_out = id_pool.new_id(1);
            auto b = id_pool.new_id(1);
            auto x_in = id_pool.new_id(A_width);
            auto x_out = id_pool.new_id(A_width);

            // [ ~s -> B_1?b  []  s -> B_2?b ]
            nodes.merges.push_back(MergeNode{s_in, {B_1, B_2}, b});

            // [ ~b -> s := ~s   []  b->   [ ~s -> A_1?x  []  s -> A_2?x ]   ]
            // which requires top level splits and merges on `s` and `x`
            {
                auto s_in_1 = id_pool.new_id(1);
                auto s_in_2 = id_pool.new_id(1);
                auto s_out_1 = id_pool.new_id(1);
                auto s_out_2 = s_in_2;
                nodes.funcs.push_back({s_out_1, inv_expr(s_in_1)});
                nodes.splits.push_back(SplitNode{b, s_in, {s_in_1, s_in_2}});
                nodes.merges.push_back(MergeNode{b, {s_out_1, s_out_2}, s_out});

                auto x_in_1 = id_pool.new_id(A_width);
                auto x_in_2 = id_pool.new_id(A_width);
                auto x_out_1 = x_in_1;
                auto x_out_2 = id_pool.new_id(A_width);
                nodes.splits.push_back(SplitNode{b, x_in, {x_in_1, x_in_2}});
                nodes.merges.push_back(MergeNode{b, {x_out_1, x_out_2}, x_out});
                nodes.merges.push_back(MergeNode{s_in_2, {A_1, A_2}, x_out_2});
            }

            // [ s&~b -> skip  []  else -> B!b ]
            {
                auto s_and_not_b_expr = DExpr::makeBinaryOp(
                    IRBinaryOpType::And,
                    std::make_unique<DExpr>(DExpr::makeVariableAccess(s_in, 1)),
                    std::make_unique<DExpr>(inv_expr(b)));
                auto g = id_pool.new_id(1);
                nodes.funcs.push_back({g, std::move(s_and_not_b_expr)});
                nodes.splits.push_back(SplitNode{g, b, {id_pool.new_id(1), B}});
            }

            // [ ~b -> skip  [] b -> A!x ]
            nodes.splits.push_back(
                SplitNode{b, x_out, {id_pool.new_id(A_width), A}});

            // finally, wire thing back around in a loop (with initializers)
            nodes.dup_inits.push_back({s_out, s_in});
            nodes.dup_inits.push_back({x_out, x_in});
        } else {
            auto s_in = id_pool.new_id(1);
            auto s_out = id_pool.new_id(1);
            auto b = id_pool.new_id(1);
            auto x_in = id_pool.new_id(A_width);
            auto x_out = id_pool.new_id(A_width);

            // [ ~s -> B_1?b  []  s -> B_2?b ]
            // Which encodes as
            // b = merge(s_in, B_1, B_2)
            nodes.merges.push_back(MergeNode{s_in, {B_1, B_2}, b});

            // [ ~s&~b -> skip  []  else -> B!b ]
            // Which encodes as
            // (_, b) = split(~s_in&~b)
            {
                auto not_s_and_not_b_expr =
                    DExpr::makeBinaryOp(IRBinaryOpType::And,
                                        std::make_unique<DExpr>(inv_expr(s_in)),
                                        std::make_unique<DExpr>(inv_expr(b)));
                auto g = id_pool.new_id(1);
                nodes.funcs.push_back({g, std::move(not_s_and_not_b_expr)});
                nodes.splits.push_back(SplitNode{g, b, {drop(1), B}});
            }
            // [ ~b -> skip  [] b -> A?x ]
            // Which encodes as
            // x_out = merge(b, x_in, A)
            {
                auto x_1 = id_pool.new_id(A_width);
                nodes.splits.push_back(
                    SplitNode{b, x_in, {x_1, drop(A_width)}});
                nodes.merges.push_back(MergeNode{b, {x_1, A}, x_out});
            }

            // [ ~b -> s := ~s   []  b->   [ ~s -> A_1!x  []  s -> A_2!x ]   ]
            // s_out = merge(b, ~s_in, s_in)
            // (_, x2) = split(b, x_out)
            // (A_1, A_2) = split(s_in, x2)
            {
                auto s_in_1 = id_pool.new_id(1);
                auto s_in_2 = id_pool.new_id(1);
                auto s_out_1 = id_pool.new_id(1);
                auto s_out_2 = s_in_2;
                nodes.funcs.push_back({s_out_1, inv_expr(s_in_1)});
                nodes.splits.push_back(SplitNode{b, s_in, {s_in_1, s_in_2}});
                nodes.merges.push_back(MergeNode{b, {s_out_1, s_out_2}, s_out});

                auto x_in_2 = id_pool.new_id(A_width);
                nodes.splits.push_back(
                    SplitNode{b, x_out, {drop(A_width), x_in_2}});
                nodes.splits.push_back(SplitNode{s_in_2, x_in_2, {A_1, A_2}});
            }

            // finally, wire thing back around in a loop (with initializers)
            nodes.dup_inits.push_back({s_out, s_in});
            nodes.dup_inits.push_back({x_out, x_in});
        }
    }

    // handle the select block case
    for (const auto &[A_is_send, A_width, ctrls, A, B, C, C_width] :
         handler.select_cases) {
        if (A_is_send) {
            auto b_in = id_pool.new_id(1);
            auto b_out = id_pool.new_id(1);
            auto g_in = id_pool.new_id(C_width);
            auto g_out = id_pool.new_id(C_width);
            auto x_in = id_pool.new_id(A_width);
            auto x_out = id_pool.new_id(A_width);

            // [ ~b -> C?g [] b -> skip ]
            auto g_tmp = id_pool.new_id(C_width);
            nodes.splits.push_back({b_in, g_in, {drop(C_width), g_tmp}});
            nodes.merges.push_back({b_in, {C, g_tmp}, g_out});

            // [ g=i -> B_i?b; [ ~b -> skip  []  b -> A_i?x ];    []   g=j ->
            // b:= false ]
            std::vector<DVarId> x_ins, x_outs, b_outs;
            for (const auto &ctrl : ctrls) {
                auto x_in_i = id_pool.new_id(A_width);
                auto x_out_i = id_pool.new_id(A_width);
                auto b = id_pool.new_id(1);
                if (!ctrl) {
                    nodes.dups.push_back({x_in_i, x_out_i});
                    nodes.funcs.push_back(
                        {b, DExpr::makeConstant(BigInt{0}, 1)});
                } else {
                    const auto &[A_i, B_i] = *ctrl;
                    nodes.dups.push_back({B_i, b});

                    // [ ~b-> skip  []  b->A_i?x  ] decodes as a split and a
                    // merge
                    auto x_tmp = id_pool.new_id(A_width);
                    nodes.splits.push_back({b, x_in_i, {x_tmp, drop(A_width)}});
                    nodes.merges.push_back({b, {x_tmp, A_i}, x_out_i});
                }
                x_ins.push_back(x_in_i);
                x_outs.push_back(x_out_i);
                b_outs.push_back(b);
            }
            nodes.splits.push_back({g_out, x_in, x_ins});
            nodes.merges.push_back({g_out, x_outs, x_out});
            nodes.merges.push_back({g_out, b_outs, b_out});

            // B!b; [ ~b -> skip  []  b -> A!x ]
            nodes.dups.push_back({b_out, B});
            nodes.splits.push_back({b_out, x_out, {drop(A_width), A}});

            // now wrap everything back up together
            nodes.dup_inits.push_back({b_out, b_in});
            nodes.dup_inits.push_back({x_out, x_in});
            nodes.dup_inits.push_back({g_out, g_in});
        } else {
            auto b_in = id_pool.new_id(1);
            auto b_out = id_pool.new_id(1);
            auto g_in = id_pool.new_id(C_width);
            auto g_out = id_pool.new_id(C_width);
            auto x_in = id_pool.new_id(A_width);
            auto x_out = id_pool.new_id(A_width);

            // [ ~b -> C?g [] b -> skip ]
            auto g_tmp = id_pool.new_id(C_width);
            nodes.splits.push_back({b_in, g_in, {drop(C_width), g_tmp}});
            nodes.merges.push_back({b_in, {C, g_tmp}, g_out});

            // [ g=i -> B_i?b;   []   g=j -> b:= false ]
            std::vector<DVarId> b_outs;
            for (const auto &ctrl : ctrls) {
                auto b = id_pool.new_id(1);
                if (!ctrl) {
                    nodes.funcs.push_back(
                        {b, DExpr::makeConstant(BigInt{0}, 1)});
                } else {
                    nodes.dups.push_back({ctrl->B, b});
                }
                b_outs.push_back(b);
            }
            nodes.merges.push_back({g_out, b_outs, b_out});

            // B!b; [ ~b -> skip  []  b -> A?x ]
            auto x_tmp = id_pool.new_id(A_width);
            nodes.dups.push_back({b_out, B});
            nodes.splits.push_back({b_out, x_in, {x_tmp, drop(A_width)}});
            nodes.merges.push_back({b_out, {x_tmp, A}, x_out});

            // [ g=i -> [ ~b -> skip  []  b-> A_i!x ]     []   g=j -> skip ]
            std::vector<DVarId> x_ins, b_ins;
            for (const auto &ctrl : ctrls) {
                auto x_in_i = id_pool.new_id(A_width);
                auto b_in_i = id_pool.new_id(1);
                if (!ctrl) {
                    // skip
                } else {
                    nodes.splits.push_back(
                        {b_in_i, x_in_i, {drop(A_width), ctrl->A}});
                }
                x_ins.push_back(x_in_i);
                b_outs.push_back(b_in_i);
            }

            // now wrap everything back up together
            nodes.dup_inits.push_back({b_out, b_in});
            nodes.dup_inits.push_back({x_out, x_in});
            nodes.dup_inits.push_back({g_out, g_in});
        }
    }

    for (const auto &[A_is_send, A_width, A_1, B_1, A, B, C] :
         handler.doloop_cases) {
        // C_0!g; *[ g -> S_1; C_1!g ] paired with `s:=false; *[ [~s -> C_0?s []
        // s -> C_1?s]; C!s ]` is equivalent to C_0!1; *[ S_1; C_1!g <- g ]
        // paired with `s:=false; *[ [~s -> C_0?s  []  s -> C_1?s]; C!s ]` is
        // equivalent to
        // *[ S_1; C!g <- g ] paired with `s:=false; *[ [~s -> s:=true  []  s ->
        // C?s]; C_MAIN!s ]` Where C is the channel that "listens" to the
        // do-loop, and C_MAIN is used in the main control process

        auto C_main = id_pool.new_id(1);
        {
            auto s_in = id_pool.new_id(1);
            auto s_out = id_pool.new_id(1);

            auto one_source = id_pool.new_id(1);
            nodes.funcs.push_back(
                {one_source, DExpr ::makeConstant(BigInt{1}, 1)});
            nodes.merges.push_back({s_in, {one_source, C}, s_out});

            nodes.dups.push_back({s_out, C_main});
            nodes.dup_inits.push_back({s_out, s_in});
        }

        // The main control process then uses C_main
        {
            auto b_in = id_pool.new_id(1);
            auto b_out = id_pool.new_id(1);
            auto g_in = id_pool.new_id(1);
            auto g_out = id_pool.new_id(1);
            // [ ~b -> C_main?g  []  b -> skip ]
            {
                auto g_tmp = id_pool.new_id(1);
                nodes.splits.push_back({b_in, g_in, {drop(1), g_tmp}});
                nodes.merges.push_back({b_in, {C_main, g_tmp}, g_out});
            }

            // [ else -> skip  []  b|g -> B_1?b   ]
            {
                auto b_or_g_expr = DExpr::makeBinaryOp(
                    IRBinaryOpType::Or,
                    std::make_unique<DExpr>(DExpr::makeVariableAccess(b_in, 1)),
                    std::make_unique<DExpr>(
                        DExpr::makeVariableAccess(g_out, 1)));

                auto b_or_g_id = id_pool.new_id(1);
                nodes.funcs.push_back({b_or_g_id, std::move(b_or_g_expr)});

                auto b_tmp = id_pool.new_id(1);
                nodes.splits.push_back({b_or_g_id, b_in, {b_tmp, drop(1)}});
                nodes.merges.push_back({b_or_g_id, {b_tmp, B_1}, b_out});
            }

            // [ else -> B!b   []  (g^b) -> skip  ]
            {
                auto b_xor_g_expr = DExpr::makeBinaryOp(
                    IRBinaryOpType::Xor,
                    std::make_unique<DExpr>(
                        DExpr::makeVariableAccess(b_out, 1)),
                    std::make_unique<DExpr>(
                        DExpr::makeVariableAccess(g_out, 1)));

                auto b_xor_g_id = id_pool.new_id(1);
                nodes.funcs.push_back({b_xor_g_id, std::move(b_xor_g_expr)});

                nodes.splits.push_back({b_xor_g_id, b_in, {B, drop(1)}});
            }

            // now wrap everything back up together
            nodes.dup_inits.push_back({b_out, b_in});
            nodes.dup_inits.push_back({g_out, g_in});
        }
        // finally, connect up the A with the A_1
        if (A_is_send) {
            nodes.dups.push_back({A_1, A});
        } else {
            nodes.dups.push_back({A, A_1});
        }
    }

    for (const auto &chan_id : Algo::get_sorted_keys(handler.readable_channels))
        nodes.readable_channels.emplace_back(
            chan_id, handler.readable_channels.at(chan_id));
    for (const auto &chan_id : Algo::get_sorted_keys(handler.writable_channels))
        nodes.writable_channels.emplace_back(
            chan_id, handler.writable_channels.at(chan_id));
}

std::pair<DFlatNodes, DVarIdPool> build_flat_nodes(const ChpGraph &g) {
    DVarIdPool id_pool = make_id_pool(g);
    const MultiChanAccessHandler multichan_handler =
        make_multi_chan_access_handler(g, id_pool);

    DFlatNodes nodes;

    // create the initial control token
    DVarId seq_ctrl_tok_id = id_pool.new_ctrl_tok();
    nodes.dup_inits.push_back({id_pool.new_ctrl_tok(), seq_ctrl_tok_id});

    // Then add the main part of the graph
    nodes_add_seq(nodes, seq_ctrl_tok_id, g.m_seq, multichan_handler, id_pool);

    // Then generate control structures based on the multichan_handler
    add_multichan_handler_control_processes(nodes, multichan_handler, id_pool);
    return {std::move(nodes), std::move(id_pool)};
}

std::string unary_op_marker(IRUnaryOpType op) {
    switch (op) {
    case IRUnaryOpType::Not:
        return " ~";
    case IRUnaryOpType::UnaryMinus:
        return " -";
    }
    hassert(false);
    return "";
}
std::string binary_op_marker(IRBinaryOpType op) {
    switch (op) {
    case IRBinaryOpType::And:
        return " & ";
    case IRBinaryOpType::Or:
        return " | ";
    case IRBinaryOpType::Xor:
        return " ^ ";
    case IRBinaryOpType::Plus:
        return " + ";
    case IRBinaryOpType::Minus:
        return " - ";
    case IRBinaryOpType::Mult:
        return " * ";
    case IRBinaryOpType::Div:
        return " / ";
    case IRBinaryOpType::Mod:
        return " % ";
    case IRBinaryOpType::LeftShift:
        return " << ";
    case IRBinaryOpType::RightShift:
        return " >> ";
    case IRBinaryOpType::ArithmeticRightShift:
        return " >>> ";
    case IRBinaryOpType::LT:
        return " < ";
    case IRBinaryOpType::GT:
        return " > ";
    case IRBinaryOpType::LE:
        return " <= ";
    case IRBinaryOpType::GE:
        return " >= ";
    case IRBinaryOpType::EQ:
        return " = ";
    case IRBinaryOpType::NE:
        return " != ";
    case IRBinaryOpType::Concat:
        hassert(false);
    }
    hassert(false);
    return "";
}

void print_dexpr(std::ostream &o, const DExpr &e) {
    switch (e.type()) {
    case IRExprTypeKind::Const:
        o << string_format("int(%s, %d)", e.u_cons().v.to_hex_string().c_str(),
                           e.u_cons().v_width);
        break;
    case IRExprTypeKind::Var:
        o << string_format("ND%d", e.u_var().id.m_id);
        break;
    case IRExprTypeKind::BinaryOp:
        if (e.u_e2().op_type == IRBinaryOpType::Concat) {
            o << "{";
            print_dexpr(o, *e.u_e2().l);
            o << ", ";
            print_dexpr(o, *e.u_e2().r);
            o << "}";
        } else {
            o << "(";
            print_dexpr(o, *e.u_e2().l);
            o << binary_op_marker(e.u_e2().op_type);
            print_dexpr(o, *e.u_e2().r);
            o << ")";
        }
        break;
    case IRExprTypeKind::UnaryOp:
        o << "(";
        o << unary_op_marker(e.u_e1().op_type);
        print_dexpr(o, *e.u_e1().l);
        o << ")";
        break;
    case IRExprTypeKind::Query:
        o << "(";
        print_dexpr(o, *e.u_query().selector);
        o << " ? ";
        print_dexpr(o, *e.u_query().l);
        o << " : ";
        print_dexpr(o, *e.u_query().r);
        o << ")";
        break;
    case IRExprTypeKind::Bitfield:
        // bitfields not supported in dataflow
        o << "int((";
        print_dexpr(o, *e.u_bitfield().e);
        o << string_format(") >> %d, %d)", e.u_bitfield().lo(),
                           e.u_bitfield().ct());
        break;
    }
}

std::string string_of_dexpr(const DExpr &e) {
    std::stringstream ss;
    print_dexpr(ss, e);
    return ss.str();
}

#endif

void computeOutermostBlock (Sequence seq, DataflowChannelManager &dm,
			     Block *outer = NULL)
{
  Block *curr = seq.startseq->child();
  while (curr->type() != BlockType::EndSequence) {
    switch (curr->type()) {
    case BlockType::Basic: {
      switch (curr->u_basic().stmt.type()) {
      case StatementType::Assign: {
	break;
      }
      case StatementType::Send: {
	if (dm.chanmap.contains (curr->u_basic().stmt.u_send().chan)) {
	  // we already have a map for this channel, so we need to
	  // move it to the current outer scope
	  dm.chanmap[curr->u_basic().stmt.u_send().chan] =
	    (outer ? outer : seq.startseq);
	}
	else {
	  dm.chanmap[curr->u_basic().stmt.u_send().chan] =
	    (outer ? outer : curr);
	}
	break;
      }
      case StatementType::Receive: {
	if (dm.chanmap.contains (curr->u_basic().stmt.u_receive().chan)) {
	  // we already have a map for this channel, so we need to
	  // move it to the current outer scope
	  dm.chanmap[curr->u_basic().stmt.u_receive().chan] =
	    (outer ? outer : seq.startseq);
	}
	else {
	  dm.chanmap[curr->u_basic().stmt.u_receive().chan] =
	    (outer ? outer : curr);
	}
	break;
      }
      }
      break;
    }
    case BlockType::Par: {
      // A well-formed program cannot have channel conflicts in
      // parallel branches
      for (auto &branch : curr->u_par().branches) {
	computeOutermostBlock (branch, dm, outer);
      }
      break;
    }
    case BlockType::Select: {
      for (auto &branch : curr->u_select().branches) {
	computeOutermostBlock (branch.seq, dm, outer ? outer : seq.startseq);
      }
      break;
    }
    case BlockType::DoLoop: {
      computeOutermostBlock (curr->u_doloop().branch, dm, 
			     outer ? outer : seq.startseq);
      break;
    }
    case BlockType::StartSequence:
    case BlockType::EndSequence:
      hassert(false);
      break;
    }
    curr = curr->child();
  }
}

void printOutermostBlock (DataflowChannelManager &dm)
{
  for (auto &[chan, block] : dm.chanmap) {
    printf ("ch %d -> ", chan.m_id);
    if (!block) {
      printf ("null\n");
    }
    else {
      switch (block->type()) {
      case BlockType::Basic: {
	switch (block->u_basic().stmt.type()) {
	case StatementType::Assign:
	  printf ("assign!\n");
	break;
	case StatementType::Send:
	  printf ("send\n");
	break;
	case StatementType::Receive:
	  printf ("recv!\n");
	break;
	}
	break;
      }
      case BlockType::StartSequence:
	printf ("seq\n");
	break;
	
      default:
	hassert(false);
	break;
      }
    }
  }
}


void createDataflow (Sequence seq, DataflowChannelManager &dm,
		     std::vector<Dataflow> &d)
{
  Block *curr = seq.startseq->child();
  while (curr->type() != BlockType::EndSequence) {
    switch (curr->type()) {
    case BlockType::Basic: {
      switch (curr->u_basic().stmt.type()) {
      case StatementType::Assign: {
	
	
	break;
      }
      case StatementType::Send: {
	break;
      }
      case StatementType::Receive: {
	break;
      }
      }
      break;
    }
    case BlockType::Par: {
      // A well-formed program cannot have channel conflicts in
      // parallel branches
      for (auto &branch : curr->u_par().branches) {
	createDataflow (branch, dm, d);
      }
      break;
    }
    case BlockType::Select: {
      // deal with guards, phiinv, and phi
      for (auto &branch : curr->u_select().branches) {
	createDataflow (branch.seq, dm, d);
      }
      break;
    }
    case BlockType::DoLoop: {
      // deal with loopphi, phiinv, loopphinv
      createDataflow (curr->u_doloop().branch, dm, d);
      break;
    }
    case BlockType::StartSequence:
    case BlockType::EndSequence:
      hassert(false);
      break;
    }
    curr = curr->child();
  }
}
 
} // namespace

std::vector<Dataflow> chp_to_dataflow(ChpGraph &chp)
{
  std::vector<Dataflow> d;
  DataflowChannelManager m;

  hassert (chp.is_static_token_form);
  
  m.id_pool = &chp.id_pool();

  // recursively translate, while doing multichannel stuff
  // simultaneously
  computeOutermostBlock (chp.m_seq, m);

  // now create dataflow blocks!
  createDataflow (chp.m_seq, m, d);

  return d;
}

} // namespace ChpOptimize
