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
#include "sequencers.h"
#include "algos.h"
#include "hassert.h"

#include <queue>

namespace ChpOptimize {
namespace {

void applyRemaps(
    ChpExprDag &dag,
    const std::map<VarId, std::unique_ptr<ChpExprSingleRootDag>> &remaps) {
    // TODO only add each subdag once!
    ChpExprDag ::mapNodes(dag, [&](ChpExprDag::Node &n) {
        if (n.type() == IRExprTypeKind::Var) {
            auto it = remaps.find(n.u_var().id);
            if (it != remaps.end()) {
                ChpExprDag ::Node *s_root = dag.addSubdag(*it->second);
                n = ChpExprDag ::Node ::shallow_copy(*s_root);
                return true;
            }
        }
        return false;
    });
}
void applyRemaps(
    ChpExprSingleRootDag &dag,
    const std::map<VarId, std::unique_ptr<ChpExprSingleRootDag>> &remaps) {
    applyRemaps(dag.m_dag, remaps);
}

} // namespace

bool transposeAssignmentAndChanOps(ChpGraph &graph) {
    bool changed = false;
    // If there are two sequential assignment operators, fuse them together into
    // one bigger assignment operator
    graph.iterInnerPaths([&](Block *start, Block *after) -> bool {
        hassert(start != after);
        for (Block *curr = start; curr->child() != after;) {
            hassert(curr->type() == BlockType::Basic);
            Block *next = curr->child();
            hassert(next->type() == BlockType::Basic);

            if (curr->u_basic().stmt.type() == StatementType::Send &&
                next->u_basic().stmt.type() == StatementType::Assign) {
                // replace `L!e; b := c` with `b:=c; L!e`

                ChpGraph::spliceOutBasicBlock(next, MarkDead::no);
                ChpGraph::spliceInSequenceBefore(curr, {next});

                changed = true;
                // dont change `curr`
                continue;
            } else if (curr->u_basic().stmt.type() == StatementType::Assign &&
                       next->u_basic().stmt.type() == StatementType::Receive) {
                if (next->u_basic().stmt.u_receive().var) {
                    // replace `b := c; L?a` with `L?a; b:=c`
                    ChpGraph::spliceOutBasicBlock(curr, MarkDead::no);
                    ChpGraph::spliceInSequenceAfter(next, {curr});
                } else {
                    // replace `L?; b := c` with `b:=c; L?`
                    ChpGraph::spliceOutBasicBlock(next, MarkDead::no);
                    ChpGraph::spliceInSequenceBefore(curr, {next});
                }
                changed = true;
                // dont change `curr`
                continue;
            } else {
                curr = curr->child();
            }
        }
        return false;
    });
    return changed;
}

bool fuseAssignments(ChpGraph &graph) {
    auto fuse_assign2_into_assign1 = [](Block *bb1, Block *bb2) {
        hassert(bb1->u_basic().stmt.u_assign().e.roots.size() ==
                bb1->u_basic().stmt.u_assign().ids.size());
        hassert(bb2->u_basic().stmt.u_assign().e.roots.size() ==
                bb2->u_basic().stmt.u_assign().ids.size());

        // First copy bb2 into bb1
        auto bb2roots_in_bb1 = bb1->u_basic().stmt.u_assign().e.addSubdag(
            bb2->u_basic().stmt.u_assign().e);

        // For each variable assigned in bb1, map a read of that variable in bb2
        // to the node that is assigned into that variable
        std::unordered_map<VarId, ChpExprDag::Node *> bb2_var_to_new_node;
        for (ssize_t i = 0;
             i < (ssize_t)bb1->u_basic().stmt.u_assign().e.roots.size(); ++i) {
            const auto root = bb1->u_basic().stmt.u_assign().e.roots[i];
            const auto var_id = bb1->u_basic().stmt.u_assign().ids[i];
            bb2_var_to_new_node[var_id] = root;
        }
        ChpExprDag::mapNodesBelow(bb2roots_in_bb1, [&](ChpExprDag::Node &n) {
            if (n.type() == IRExprTypeKind::Var) {
                if (auto it = bb2_var_to_new_node.find(n.u_var().id);
                    it != bb2_var_to_new_node.end()) {
                    n = ChpExprDag::Node::shallow_copy(*it->second);
                }
            }
        });

        // Then erase root/id pairs from bb1 that are also assigned in bb2
        const auto bb2_ids_vec = bb2->u_basic().stmt.u_assign().ids;
        std::unordered_set<VarId> bb2_ids{bb2_ids_vec.begin(),
                                          bb2_ids_vec.end()};

        Algo::remove_filter_2_if(
            bb1->u_basic().stmt.u_assign().e.roots,
            bb1->u_basic().stmt.u_assign().ids,
            [&](ChpExprDag::Node * /*root*/, const VarId &id) {
                return bb2_ids.contains(id);
            });

        // Finally, add in the root/id pairs from bb2
        for (ssize_t i = 0; i < (ssize_t)bb2roots_in_bb1.size(); ++i) {
            bb1->u_basic().stmt.u_assign().ids.push_back(
                bb2->u_basic().stmt.u_assign().ids[i]);
            bb1->u_basic().stmt.u_assign().e.roots.push_back(
                bb2roots_in_bb1[i]);
        }

        hassert(bb2->u_basic().stmt.u_assign().e.roots.size() ==
                bb2->u_basic().stmt.u_assign().ids.size());
    };

    bool changed = false;
    // If there are two sequential assignment operators, fuse them together into
    // one bigger assignment operator
    graph.iterInnerPaths([&](Block *start, Block *after) -> bool {
        std::vector<std::pair<Block *, Block *>> regions;
        Block *first_assign = nullptr;
        for (Block *curr = start; curr != after; curr = curr->child()) {
            if (curr->type() == BlockType::Basic &&
                curr->u_basic().stmt.type() == StatementType::Assign) {
                if (first_assign == nullptr)
                    first_assign = curr;
            } else {
                if (first_assign != nullptr) {
                    regions.emplace_back(first_assign, curr);
                    first_assign = nullptr;
                }
            }
        }
        if (first_assign != nullptr) {
            regions.emplace_back(first_assign, after);
        }

        for (const auto &[region_start, region_after] : regions) {
            while (region_start->child() != region_after) {
                fuse_assign2_into_assign1(region_start, region_start->child());
                ChpGraph::spliceOutBasicBlock(region_start->child(),
                                              MarkDead::yes);
                changed = true;
            }
            // Then, once all the blocks are fused, deduplicate the regions
            ChpExprDag::deduplicate(region_start->u_basic().stmt.u_assign().e);
        }
        return false;
    });
    return changed;
}

void packAssignmentOneVar(ChpGraph &graph) {
    // If there are two sequential assignment operators, fuse them together into
    // one bigger assignment operator
    graph.iterInnerPaths([&](Block *start, Block *after) -> bool {
        Block *b = start;
        while (b != after) {
            Block *bb = b;
            b = b->child();
            hassert(bb->type() == BlockType::Basic);
            if (bb->u_basic().stmt.type() == StatementType::Assign) {
                hassert(bb->u_basic().stmt.u_assign().ids.size() ==
                        bb->u_basic().stmt.u_assign().e.roots.size());
                if (bb->u_basic().stmt.u_assign().ids.size() > 1) {
                    ChpExprDag::deduplicate(bb->u_basic().stmt.u_assign().e);

                    // then replace [id1, id2, id3 := expr_dag] with
                    // tmp := concat(expr_dag); id1 := tmp{..}; id2 := tmp{..};
                    // id3 := tmp{..}

                    // this needs to be ordered so code deterministic
                    std::map<VarId, BitSlice> id_to_slice;

                    auto &dag = bb->u_basic().stmt.u_assign().e;
                    auto &roots = dag.roots;
                    auto &ids = bb->u_basic().stmt.u_assign().ids;

                    ChpExprDag ::Node *new_root = roots.back();
                    id_to_slice[ids.back()] =
                        BitSlice(roots.back()->width - 1, 0);
                    roots.pop_back();
                    ids.pop_back();
                    while (!ids.empty()) {
                        int cur_width = new_root->width;
                        new_root = dag.newNode(ChpExprDag::Node::makeBinaryOp(
                            IRBinaryOpType::Concat, roots.back(), new_root));
                        id_to_slice[ids.back()] = BitSlice(
                            roots.back()->width - 1 + cur_width, cur_width);
                        roots.pop_back();
                        ids.pop_back();
                    }
                    hassert(roots.empty());

                    roots.push_back(new_root);
                    int new_id_width = new_root->width;
                    VarId new_id = graph.id_pool().makeUniqueVar(new_id_width);
                    ids.push_back(new_id);

                    std::vector<Block *> blocks;
                    for (const auto &[id, slice] : id_to_slice) {
                        blocks.push_back(
                            graph.newBasicBlock(Statement::makeAssignment(
                                id, ChpExprSingleRootDag::makeBitfield(
                                        std::make_unique<ChpExprSingleRootDag>(
                                            ChpExprSingleRootDag::
                                                makeVariableAccess(
                                                    new_id, new_id_width)),
                                        slice.hi(), slice.lo()))));
                    }

                    ChpGraph::spliceInSequenceAfter(bb, blocks);
                }
            }
        }
        return false;
    });
}

void inlineOneWriteExprs(ChpGraph &graph) {
    // we both inline noop expressions, and expressions that are written only by
    // a single assignment statement and read in a single statement it is ok if
    // they are read multiple times in the statemnt, the exression optimizer
    // will then optimize this out however, this could lead to an exponentially
    // sized tree so
    // TODO we should repalce xpressions with an "expression DAG"
    std::unordered_map<VarId, int> read_ct;
    std::unordered_map<VarId, int> assign_write_ct;
    std::unordered_map<VarId, int> receive_write_ct;
    {
        auto blocks = graph.getLiveBlocks();
        for (Block *b : blocks) {
            // TODO: Stop creating and leaking so many lattice objects
            switch (b->type()) {
            case BlockType::StartSequence:
            case BlockType::EndSequence:
                hassert(false);
                break;
            case BlockType::Basic: {
                // Usage in a line supercedes definition in that line, so that
                // code is run afterwords
                switch (b->u_basic().stmt.type()) {
                case StatementType::Assign: {
                    for (const auto id : b->u_basic().stmt.u_assign().ids)
                        assign_write_ct[id]++;
                    for (const auto id :
                         getIdsUsedByExpr(b->u_basic().stmt.u_assign().e))
                        read_ct[id]++;
                    break;
                }
                case StatementType::Send:
                    for (const auto id :
                         getIdsUsedByExpr(b->u_basic().stmt.u_send().e))
                        read_ct[id]++;
                    break;
                case StatementType::Receive:
                    if (b->u_basic().stmt.u_receive().var) {
                        receive_write_ct[*b->u_basic().stmt.u_receive().var]++;
                    }
                    break;
                }
                break;
            }
            case BlockType::Select: {
                for (const SelectBranch &gb : b->u_select().branches) {
                    if (gb.g.type() != IRGuardType::Else) {
                        hassert(gb.g.type() == IRGuardType::Expression);
                        for (const auto id : getIdsUsedByExpr(gb.g.u_e().e))
                            read_ct[id]++;
                    }
                }
                break;
            }
            case BlockType::DoLoop: {
                for (const auto id : getIdsUsedByExpr(b->u_doloop().guard))
                    read_ct[id]++;
                break;
            }
            case BlockType::Par: {
                break;
            }
            }
        }
    }

    graph.iterInnerPaths([&](Block *start, Block *after) {
        const auto block_vec = [&]() {
            std::vector<Block *> blocks;
            Block *b = start;
            while (b != after) {
                Block *bb = b;
                hassert(b->type() == BlockType::Basic);
                blocks.push_back(bb);
                b = b->child();
            }
            hassert(!blocks.empty());
            hassert(blocks.back()->child() == after);
            return blocks;
        }();

        std::map<VarId, std::unique_ptr<ChpExprSingleRootDag>> remaps;

        for (Block *bb : block_vec) {
            switch (bb->u_basic().stmt.type()) {
            case StatementType::Assign: {
                applyRemaps(bb->u_basic().stmt.u_assign().e, remaps);

                hassert(bb->u_basic().stmt.u_assign().ids.size() ==
                        bb->u_basic().stmt.u_assign().e.roots.size());
                hassert(!bb->u_basic().stmt.u_assign().ids.empty());
                if (bb->u_basic().stmt.u_assign().ids.size() == 1) {
                    auto id0 = bb->u_basic().stmt.u_assign().ids.at(0);
                    auto root0 = bb->u_basic().stmt.u_assign().e.roots.at(0);
                    if (read_ct.at(id0) == 1 && assign_write_ct.at(id0) == 1 &&
                        receive_write_ct.count(id0) == 0) {
                        hassert(root0->width ==
                                graph.id_pool().getBitwidth(id0));
                        remaps[id0] = std::make_unique<ChpExprSingleRootDag>(
                            ChpExprSingleRootDag(ChpExprDag ::deep_copy(
                                bb->u_basic().stmt.u_assign().e)));
                    }
                }

                break;
            }
            case StatementType::Receive:
                break;
            case StatementType::Send:

                applyRemaps(bb->u_basic().stmt.u_send().e, remaps);
                break;
            }
        }

        return false;
    });
}

namespace {
void uninlineBitfieldExprsHelper(ChpExprDag &dag) {
    ChpExprDag::mapNodes(dag, [&](ChpExprDag::Node &n) {
        switch (n.type()) {
        case IRExprTypeKind::Var:
        case IRExprTypeKind::ChanVar:
        case IRExprTypeKind::ChanProbe:
        case IRExprTypeKind::Const:
        case IRExprTypeKind::Query:
        case IRExprTypeKind::BinaryOp:
        case IRExprTypeKind::UnaryOp:
            break;
        case IRExprTypeKind::Bitfield: {
            if (n.u_bitfield().e->type() != IRExprTypeKind::Var &&
                n.u_bitfield().lo() != 0) {
                n = ChpExprDag ::Node::makeBitfield(
                    dag.newNode(ChpExprDag::Node::makeBinaryOp(
                        IRBinaryOpType::RightShift, n.u_bitfield().e,
                        dag.newNode(ChpExprDag::Node::makeConstant(
                            BigInt(n.u_bitfield().lo()), 32)))),
                    n.u_bitfield().ct() - 1, 0);
            }
            break;
        }
        }
    });
}
void uninlineBitfieldExprsHelper(ChpExprSingleRootDag &dag) {
    uninlineBitfieldExprsHelper(dag.m_dag);
}
} // namespace

// should be run _after_ adding parallelization
void uninlineBitfieldExprsHack(ChpGraph &graph) {

    auto all_blocks_vec = graph.getLiveBlocks();

    for (Block *b : all_blocks_vec) {
        if (b->type() == BlockType::Basic) {
            switch (b->u_basic().stmt.type()) {
            case StatementType::Assign: {
                uninlineBitfieldExprsHelper(b->u_basic().stmt.u_assign().e);
                break;
            }
            case StatementType::Receive:
                break;
            case StatementType::Send:
                uninlineBitfieldExprsHelper(b->u_basic().stmt.u_send().e);

                break;
            }
        }

        if (b->type() == BlockType::Select) {
            for (SelectBranch &gb : b->u_select().branches) {
                if (gb.g.type() == IRGuardType::Expression) {
                    uninlineBitfieldExprsHelper(gb.g.u_e().e);
                }
            }
        } else if (b->type() == BlockType::DoLoop) {
            uninlineBitfieldExprsHelper(b->u_doloop().guard);
        }
    }
}

namespace {
std::unique_ptr<ChpExpr> unpackBigintHelper(const BigInt &val, int width) {
    hassert(width > 0);
    if (val <= BigInt(~(1ULL))) {
        return std::make_unique<ChpExpr>(ChpExpr::makeConstant(val, width));
    }
    return std::make_unique<ChpExpr>(ChpExpr::makeBinaryOp(
        IRBinaryOpType::Concat, unpackBigintHelper(val >> 64, width - 64),
        std::make_unique<ChpExpr>(
            ChpExpr::makeConstant(BigInt{val.getVal(0)}, 64))));
}

void unpackLargeConstantHelper(ChpExprDag &dag) {
    ChpExprDag::mapNodes(dag, [&](ChpExprDag::Node &n) {
        switch (n.type()) {
        case IRExprTypeKind::Var:
        case IRExprTypeKind::ChanVar:
        case IRExprTypeKind::ChanProbe:
        case IRExprTypeKind::Query:
        case IRExprTypeKind::BinaryOp:
        case IRExprTypeKind::UnaryOp:
        case IRExprTypeKind::Bitfield:
            break;
        case IRExprTypeKind::Const: {
            auto unpacked =
                unpackBigintHelper(n.u_cons().v, n.u_cons().v_width);
            ChpExprDag::Node *int_dag = dag.addSubdag(*unpacked);
            n = ChpExprDag ::Node::shallow_copy(*int_dag);
            break;
        }
        }
    });
}
void unpackLargeConstantHelper(ChpExprSingleRootDag &dag) {
    unpackLargeConstantHelper(dag.m_dag);
}
} // namespace

void unpackLargeConstantHack(ChpGraph &graph) {
    auto all_blocks_vec = graph.getLiveBlocks();

    for (Block *b : all_blocks_vec) {
        if (b->type() == BlockType::Basic) {
            switch (b->u_basic().stmt.type()) {
            case StatementType::Assign: {
                unpackLargeConstantHelper(b->u_basic().stmt.u_assign().e);
                break;
            }
            case StatementType::Receive:
                break;
            case StatementType::Send:
                unpackLargeConstantHelper(b->u_basic().stmt.u_send().e);

                break;
            }
        }

        if (b->type() == BlockType::Select) {
            for (SelectBranch &gs : b->u_select().branches) {
                if (gs.g.type() == IRGuardType::Expression) {
                    unpackLargeConstantHelper(gs.g.u_e().e);
                }
            }
        } else if (b->type() == BlockType::DoLoop) {
            unpackLargeConstantHelper(b->u_doloop().guard);
        }
    }
}

// TODO add variable renaming
// Maybe start with only doing in non-branching segments
// i.e. for each controll block branch, see if there are no control blocks in
// it. If that is the case, then do renaming. Otherwise, recurse in
namespace {

void parallelizeSequence_nonrec(
    Sequence &seq,
    const std::unordered_map<const Block *, UsesAndDefs> &use_defs,
    ChpGraph &graph) {
    // If a block reads `id`, it must be run before every block after it in the
    // sequence that writes `id`. If a block write to `id`, it must be run
    // before every block after in which reads or writes to `id`. The order of
    // two blocks which use channels may not be transposed We will iterate from
    // back to front. The following tables will accumulate lists of all the
    // blocks which read or write each variable for all the blocks we have
    // passed while iterating.
    std::unordered_map<VarId, std::vector<const Block *>> var_to_readers;
    std::unordered_map<VarId, std::vector<const Block *>> var_to_writers;
    std::vector<const Block *> chan_accessers;
    std::vector<const Block *> pos_hangers;
    std::unordered_map<const Block *, std::vector<const Block *>>
        block_to_dependencies;

    for (Block *curr = seq.endseq->parent(); curr != seq.startseq;
         curr = curr->parent()) {
        const auto &curr_ud = use_defs.at(curr);
        // first add all the dependencies. Note that a block which uses a
        // channel can not be transposed with a block which can hang
        // TODO deduplicate things?
        for (const auto id : curr_ud.var_reads) {
            for (const Block *writer : var_to_writers[id])
                block_to_dependencies[writer].push_back(curr);
        }
        for (const auto id : curr_ud.var_writes) {
            for (const Block *reader : var_to_readers[id])
                block_to_dependencies[reader].push_back(curr);
            for (const Block *writer : var_to_writers[id])
                block_to_dependencies[writer].push_back(curr);
        }
        if (curr_ud.uses_chan) {
            for (const Block *chan_user : chan_accessers)
                block_to_dependencies[chan_user].push_back(curr);

            // TODO technically, it is ok if the "can_hang" block goes in
            // parallel with `curr`, it just cant happen in a
            //  layer before this one. However, we dont currently have a way to
            //  encode that, so for now we use the stronger condition
            for (const Block *can_hang : pos_hangers)
                block_to_dependencies[can_hang].push_back(curr);
        }
        if (curr_ud.can_hang) {
            for (const Block *chan_user : chan_accessers)
                block_to_dependencies[chan_user].push_back(curr);
        }

        // then add curr onto the proper lists
        for (const auto id : curr_ud.var_reads) {
            var_to_readers[id].push_back(curr);
        }
        for (const auto id : curr_ud.var_writes) {
            var_to_writers[id].push_back(curr);
        }
        if (curr_ud.uses_chan)
            chan_accessers.push_back(curr);
        if (curr_ud.can_hang)
            pos_hangers.push_back(curr);
        // then step to the next block and loop around
    }

    std::vector<std::vector<Block *>> layers = [&]() {
        std::unordered_set<const Block *> done_blocks;
        std::vector<std::vector<Block *>> layers;

        // This is definitely not the right way to do this because it is O(n^2)
        while (true) {
            layers.emplace_back();
            // use the block vector to order accesses because the order of an
            // absl_flat_map is not deterministic
            for (Block *curr = seq.startseq->child(); curr != seq.endseq;
                 curr = curr->child()) {
                if (done_blocks.count(curr))
                    continue;
                const auto &dependencies = block_to_dependencies[curr];
                bool able_to_run =
                    Algo::all_of(dependencies, [&](const Block *const dep) {
                        return done_blocks.find(dep) != done_blocks.end();
                    });
                if (able_to_run) {
                    layers.back().push_back(curr);
                }
            }
            for (const Block *const block : layers.back()) {
                done_blocks.insert(block);
            }
            if (layers.back().empty()) {
                layers.pop_back();
                break;
            }
        }

        // hassert some invariants
        auto block_ct = [&]() {
            size_t ct = 0;
            for (Block *curr = seq.startseq->child(); curr != seq.endseq;
                 curr = curr->child()) {
                ct++;
                hassert(done_blocks.count(curr));
            }
            return ct;
        }();
        auto layers_ct = Algo::sum<size_t>(
            layers, [](const auto &layer) { return layer.size(); }, 0);
        hassert(layers_ct == block_ct);
        hassert(done_blocks.size() == block_ct);
        hassert(!done_blocks.count(seq.startseq));
        hassert(!done_blocks.count(seq.endseq));

        return layers;
    }();

    // finally, rewire everything so the layers are correctly glued together

    for (Block *curr = seq.startseq->child(); curr != seq.endseq;
         curr = curr->child()) {
        Block::disconnect(curr->parent(), curr);
    }
    Block::disconnect(seq.endseq->parent(), seq.endseq);

    Block *last_layer = seq.startseq;
    for (const auto &layer : layers) {
        hassert(!layer.empty());
        Block *new_layer;
        if (layer.size() == 1) {
            new_layer = layer[0];
        } else {
            new_layer = graph.newParBlock();
            for (const auto &entry : layer) {
                ChpGraph::spliceInControlBlockBranch(
                    new_layer, graph.newSequence({entry}));
            }
        }
        Block::connect(last_layer, new_layer);
        last_layer = new_layer;
    }
    Block::connect(last_layer, seq.endseq);
}

void parallelizeSequence(
    Sequence &seq,
    const std::unordered_map<const Block *, UsesAndDefs> &use_defs,
    ChpGraph &graph) {
    for (Block *curr = seq.startseq->child(); curr != seq.endseq;
         curr = curr->child()) {
        switch (curr->type()) {
        case BlockType::StartSequence:
        case BlockType::EndSequence:
            hassert(false);
            break;
        case BlockType::Basic:
            break;
        case BlockType::Par:
            for (Sequence &path : curr->u_par().branches)
                parallelizeSequence(path, use_defs, graph);
            break;
        case BlockType::Select:
            hassert(curr->u_select().splits.empty());
            hassert(curr->u_select().merges.empty());
            for (SelectBranch &branch : curr->u_select().branches)
                parallelizeSequence(branch.seq, use_defs, graph);
            break;
        case BlockType::DoLoop:
            hassert(curr->u_doloop().in_phis.empty());
            hassert(curr->u_doloop().out_phis.empty());
            hassert(curr->u_doloop().loop_phis.empty());
            parallelizeSequence(curr->u_doloop().branch, use_defs, graph);
            break;
        }
    }
    parallelizeSequence_nonrec(seq, use_defs, graph);
    graph.validateGraphInvariants();
}

} // namespace

void parallelizeGraph(ChpGraph &graph) {
    auto use_defs = getDefUsesTable(graph);
    parallelizeSequence(graph.m_seq, use_defs, graph);
}

namespace {

[[nodiscard]] VarId addNewAndRename(const VarId &id,
                                    std::unordered_map<VarId, VarId> &renames,
                                    IdPool &id_pool) {
    auto newId = id_pool.makeUniqueVar(id_pool.getBitwidth(id));
    renames[id] = newId;
    return newId;
}
void setDontRename(const VarId &id, std::unordered_map<VarId, VarId> &renames) {
    auto it = renames.find(id);
    if (it != renames.end())
        renames.erase(it);
}

} // namespace

// TODO add variable renaming
// Maybe start with only doing in non-branching segments
// i.e. for each controll block branch, see if there are no control blocks in
// it. If that is the case, then do renaming. Otherwise, recurse in
bool uniqueifyInnerPaths(ChpGraph &graph) {
    return graph.iterInnerPaths([&](Block *start, Block *after) {
        bool changed = false;

        // rename the variable in each assignment. When we are done, assign back
        // into the original variables
        std::map<VarId, int> assignment_cts; // maps old name to new name
        {
            Block *b = start;
            while (b != after) {
                hassert(b->type() == BlockType::Basic);
                switch (b->u_basic().stmt.type()) {
                case StatementType::Assign: {
                    for (const auto &id : b->u_basic().stmt.u_assign().ids)
                        assignment_cts[id]++;
                    break;
                }
                case StatementType::Receive: {
                    if (b->u_basic().stmt.u_receive().var) {
                        assignment_cts[*b->u_basic().stmt.u_receive().var]++;
                    }
                    break;
                }
                case StatementType::Send:
                    break;
                }
                b = b->child();
            }
        }

        std::unordered_map<VarId, VarId> renames; // maps old name to new name
        Block *b = start;
        while (true) {
            hassert(b->type() == BlockType::Basic);
            switch (b->u_basic().stmt.type()) {
            case StatementType::Assign: {
                changed |= ChpExprDag::remapVars(b->u_basic().stmt.u_assign().e,
                                                 renames);
                for (auto &id : b->u_basic().stmt.u_assign().ids) {
                    if (assignment_cts[id] > 1) {
                        assignment_cts[id]--;
                        id = addNewAndRename(id, renames, graph.id_pool());
                        changed = true;
                    } else {
                        setDontRename(id, renames);
                    }
                }
                break;
            }
            case StatementType::Receive: {
                auto &opt_var_id = b->u_basic().stmt.u_receive().var;
                if (opt_var_id) {
                    if (assignment_cts[*opt_var_id] > 1) {
                        assignment_cts[*opt_var_id]--;
                        opt_var_id = addNewAndRename(*opt_var_id, renames,
                                                     graph.id_pool());
                        changed = true;
                    } else {
                        setDontRename(*opt_var_id, renames);
                    }
                }
                break;
            }
            case StatementType::Send:
                changed |= ChpExprSingleRootDag::remapVars(
                    b->u_basic().stmt.u_send().e, renames);

                break;
            }
            if (b->child() == after)
                break;
            b = b->child();
        }
        hassert(b->child() == after);
        return changed;
    });
}

bool liftGuards(ChpGraph &graph) {
    bool changed = false;
    auto all_blocks_vec = graph.getLiveBlocks();

    for (Block *block : all_blocks_vec) {
        block->dead = false;
    }

    for (Block *block : all_blocks_vec) {
        if (block->dead)
            continue;

        if (block->type() == BlockType::Select) {
            for (SelectBranch &gs : block->u_select().branches) {
                if (gs.g.type() == IRGuardType::Expression &&
                    gs.g.u_e().e.root()->type() != IRExprTypeKind::Var &&
                    gs.g.u_e().e.root()->type() != IRExprTypeKind::Const) {
                    hassert(gs.g.u_e().e.root()->width == 1);
                    VarId id = graph.id_pool().makeUniqueVar(1);
                    Block *b = graph.newBasicBlock(
                        Statement::makeAssignment(id, std::move(gs.g.u_e().e)));
                    gs.g.u_e().e =
                        ChpExprSingleRootDag::makeVariableAccess(id, 1);
                    ChpGraph::spliceInSequenceBefore(block, {b});
                    changed = true;
                }
            }
        }
    }
    return changed;
}

namespace {
void hassertNoSelfAssignments_helper(const Sequence &seq) {
    for (Block *c = seq.startseq->child(); c != seq.endseq; c = c->child()) {
        switch (c->type()) {
        case BlockType::Basic:
            switch (c->u_basic().stmt.type()) {
            case StatementType::Assign: {
                const auto &assign = c->u_basic().stmt.u_assign();
                auto ids_used_by_expr = getIdsUsedByExpr(assign.e);

                std::vector<std::pair<VarId, VarId>> old_new_pairs;
                for (const auto &id : assign.ids)
                    hassert(!ids_used_by_expr.contains(id));
                break;
            }
            case StatementType::Send:
            case StatementType::Receive:
                break;
            }
            break;
        case BlockType::Par:
            for (const Sequence &branch : c->u_par().branches)
                hassertNoSelfAssignments_helper(branch);
            break;
        case BlockType::Select:
            for (const SelectBranch &branch : c->u_select().branches)
                hassertNoSelfAssignments_helper(branch.seq);
            break;
        case BlockType::DoLoop:
            hassertNoSelfAssignments_helper(c->u_doloop().branch);
            break;
        case BlockType::StartSequence:
        case BlockType::EndSequence:
            hassert(false);
            break;
        }
    }
}
} // namespace

void hassertNoSelfAssignments(const ChpGraph &graph) {
    hassertNoSelfAssignments_helper(graph.m_seq);
}

namespace {
void removeSelfAssignments_helper(Sequence &seq, ChpGraph &graph) {
    for (Block *c = seq.startseq->child(); c != seq.endseq; c = c->child()) {
        switch (c->type()) {
        case BlockType::Basic:
            switch (c->u_basic().stmt.type()) {
            case StatementType::Assign: {
                auto &assign = c->u_basic().stmt.u_assign();
                auto ids_used_by_expr = getIdsUsedByExpr(assign.e);

                std::vector<std::pair<VarId, VarId>> old_new_pairs;
                for (auto &id : assign.ids) {
                    // This function should only be called on graphs with 1 id
                    // per assignment (e.g. newly parsed ones)
                    if (ids_used_by_expr.contains(id)) {
                        VarId new_id = graph.id_pool().makeUniqueVar(
                            graph.id_pool().getBitwidth(id));
                        old_new_pairs.emplace_back(id, new_id);
                        id = new_id;
                    }
                }

                if (old_new_pairs.size() == 1) {
                    const auto &[old_id, new_id] = old_new_pairs[0];
                    Block *bb = graph.newBasicBlock(Statement::makeAssignment(
                        old_id, ChpExprSingleRootDag::makeVariableAccess(
                                    new_id, graph.id_pool())));
                    ChpGraph::spliceInSequenceAfter(c, {bb});
                } else if (old_new_pairs.size() >= 2) {
                    Block *par = graph.newParBlock();
                    for (const auto &[old_id, new_id] : old_new_pairs) {
                        Block *bb =
                            graph.newBasicBlock(Statement::makeAssignment(
                                old_id,
                                ChpExprSingleRootDag::makeVariableAccess(
                                    new_id, graph.id_pool())));
                        ChpGraph::spliceInControlBlockBranch(
                            par, graph.newSequence({bb}));
                    }
                    ChpGraph::spliceInSequenceAfter(c, {par});
                }

                break;
            }
            case StatementType::Send:
            case StatementType::Receive:
                break;
            }
            break;
        case BlockType::Par:
            for (Sequence &branch : c->u_par().branches)
                removeSelfAssignments_helper(branch, graph);
            break;
        case BlockType::Select:
            for (SelectBranch &branch : c->u_select().branches)
                removeSelfAssignments_helper(branch.seq, graph);
            break;
        case BlockType::DoLoop:
            removeSelfAssignments_helper(c->u_doloop().branch, graph);
            break;
        case BlockType::StartSequence:
        case BlockType::EndSequence:
            hassert(false);
            break;
        }
    }
}
} // namespace

void removeSelfAssignments(ChpGraph &graph) {
    removeSelfAssignments_helper(graph.m_seq, graph);
}

namespace {
void removeElseOnlySelect_helper(Sequence &seq) {
    for (Block *c = seq.startseq->child(); c != seq.endseq; c = c->child()) {
        switch (c->type()) {
        case BlockType::Basic:
            break;
        case BlockType::Par:
            for (Sequence &branch : c->u_par().branches)
                removeElseOnlySelect_helper(branch);
            break;
        case BlockType::Select:
            for (SelectBranch &branch : c->u_select().branches)
                removeElseOnlySelect_helper(branch.seq);
            if (c->u_select().branches.size() == 1 &&
                c->u_select().branches.front().g.type() == IRGuardType::Else) {
	      if (c->u_select().splits.size() == 0 &&
		  c->u_select().merges.size() == 0) {
		Block *d = c->child();
		ChpGraph::spliceInSequenceBetween (c->parent(), c,
						   c->u_select().branches.front().seq);
		// new parent 
		ChpGraph::spliceOutBasicBlock (c, MarkDead::yes);
		c = d->parent();
	      }
	      else {
                c->u_select().branches.front().g = IRGuard::makeExpression(
                    ChpExprSingleRootDag::makeConstant(BigInt{1}, 1));
	      }
	    }
            break;
        case BlockType::DoLoop:
            removeElseOnlySelect_helper(c->u_doloop().branch);
            break;
        case BlockType::StartSequence:
        case BlockType::EndSequence:
            hassert(false);
            break;
        }
    }
}
} // namespace

void removeElseOnlySelect(ChpGraph &graph) {
    removeElseOnlySelect_helper(graph.m_seq);
}
} // namespace ChpOptimize
