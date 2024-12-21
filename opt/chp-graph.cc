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
#include "chp-graph.h"
#include "algos.h"
#include "hassert.h"

namespace ChpOptimize {

namespace {
void validateExprDag(const ChpExprDag &dag, const IdPool &id_pool) {
    // we want to validate that the dag is actually dag (i.e has no cycles), and
    // that each node has the correct bitwidth
    using Node = ChpExprDag::Node;

    // first gather a candidate post-ordering
    std::unordered_map<const Node *, int> postorder;
    int idx = 0;
    ChpExprDag::iterNodes(dag, [&](const Node &n) {
        hassert(!postorder.contains(&n));
        postorder[&n] = idx++;
    });

    // then check that each nodes children have a number lower that itself
    ChpExprDag::iterNodes(dag, [&](const Node &n) {
        switch (n.type()) {
        case IRExprTypeKind::Const:
        case IRExprTypeKind::Var:
	case IRExprTypeKind::ChanVar:
	case IRExprTypeKind::ChanProbe:
            break;
        case IRExprTypeKind::BinaryOp:
            hassert(postorder[&n] > postorder.at(n.u_e2().l));
            hassert(postorder[&n] > postorder.at(n.u_e2().r));
            break;
        case IRExprTypeKind::UnaryOp:
            hassert(postorder[&n] > postorder.at(n.u_e1().l));
            break;
        case IRExprTypeKind::Query:
            hassert(postorder[&n] > postorder.at(n.u_query().selector));
            hassert(postorder[&n] > postorder.at(n.u_query().l));
            hassert(postorder[&n] > postorder.at(n.u_query().r));
            break;
        case IRExprTypeKind::Bitfield:
            hassert(postorder[&n] > postorder.at(n.u_bitfield().e));
            break;
        }
    });

    // then check that all the widths are correct. This is intentionally a
    // reimplementation of the bitwidth math to double check the other
    // implementations as this has been the source of a lot of bugs.
    ChpExprDag::iterNodes(dag, [&](const Node &n) {
        switch (n.type()) {
        case IRExprTypeKind::Const:
            hassert(n.u_cons().v_width == n.width);
            break;
        case IRExprTypeKind::Var:
            hassert(id_pool.getBitwidth(n.u_var().id) == n.width);
            break;
	case IRExprTypeKind::ChanProbe:
	    hassert(1 == n.width);
	    break;
	case IRExprTypeKind::ChanVar:
	    hassert(id_pool.getBitwidth(n.u_chvar().id) == n.width);
	    break;
        case IRExprTypeKind::BinaryOp:
            switch (n.u_e2().op_type) {
            case IRBinaryOpType::And:
            case IRBinaryOpType::Or:
            case IRBinaryOpType::Xor:
                hassert(n.width ==
                        std::max(n.u_e2().l->width, n.u_e2().r->width));
                break;
            case IRBinaryOpType::Plus:
            case IRBinaryOpType::Minus:
                hassert(n.width ==
                        std::max(n.u_e2().l->width, n.u_e2().r->width) + 1);
                break;
            case IRBinaryOpType::Mult:
                hassert(n.width == n.u_e2().l->width + n.u_e2().r->width);
                break;
            case IRBinaryOpType::Div:
            case IRBinaryOpType::RightShift:
            case IRBinaryOpType::ArithmeticRightShift:
                hassert(n.width == n.u_e2().l->width);
                break;
            case IRBinaryOpType::Mod:
                hassert(n.width == n.u_e2().r->width);
                break;
            case IRBinaryOpType::LeftShift:
                hassert(n.u_e2().r->width <= 30);
                hassert(n.width ==
                        n.u_e2().l->width + (1 << n.u_e2().r->width) - 1);
                break;
            case IRBinaryOpType::LT:
            case IRBinaryOpType::GT:
            case IRBinaryOpType::LE:
            case IRBinaryOpType::GE:
            case IRBinaryOpType::EQ:
            case IRBinaryOpType::NE:
                hassert(n.width == 1);
                break;
            case IRBinaryOpType::Concat:
                hassert(n.width == n.u_e2().l->width + n.u_e2().r->width);
                break;
            }
            break;
        case IRExprTypeKind::UnaryOp:
            switch (n.u_e1().op_type) {
            case IRUnaryOpType::Not:
                hassert(n.width == n.u_e1().l->width);
                break;
            case IRUnaryOpType::UnaryMinus:
                hassert(n.width == n.u_e1().l->width);
                break;
            }
            break;
        case IRExprTypeKind::Query:
            hassert(n.u_query().selector->width == 1);
            hassert(n.width ==
                    std::max(n.u_query().l->width, n.u_query().r->width));
            break;
        case IRExprTypeKind::Bitfield:
            break;
        }
    });
}
void validateExprSingleRootDag(const ChpExprSingleRootDag &dag,
                               const IdPool &id_pool) {
    hassert(dag.m_dag.roots.size() == 1);
    validateExprDag(dag.m_dag, id_pool);
}

void validateSeqInvariantsRecursive(const Sequence &seq,
                                    const IdPool &id_pool) {
    std::set<const Block *> m_caps;
    for (Block *b = seq.startseq->child(); b != seq.endseq; b = b->child()) {
        hassert(b);

        // validate form of parent and child connections
        hassert(b->parent() != nullptr);
        hassert(b->child() != nullptr);

        // validate symmetry of parent and child connections
        hassert(b->parent()->child() == b);
        hassert(b->child()->parent() == b);

        // Validate that each child has a valid number of branches
        switch (b->type()) {
        case BlockType::StartSequence:
        case BlockType::EndSequence:
            hassert(false);
            break;
        case BlockType::Basic:
        case BlockType::Par:
        case BlockType::DoLoop:
            break;
        case BlockType::Select:
            hassert(!b->u_select().branches.empty());
            break;
        }

        // Validate each expression is well-formed
        switch (b->type()) {
        case BlockType::StartSequence:
        case BlockType::EndSequence:
            hassert(false);
            break;
        case BlockType::Basic: {
            switch (b->u_basic().stmt.type()) {
            case StatementType::Assign:
                // check there are the correct number of roots, that all the
                // assigned ids are unique, that each id is assigned from an
                // expression of the same width, and that the expression dag is
                // well-formed
                hassert(b->u_basic().stmt.u_assign().e.roots.size() ==
                        b->u_basic().stmt.u_assign().ids.size());
                hassert(Algo::vector_has_no_repeates(
                    b->u_basic().stmt.u_assign().ids));
                validateExprDag(b->u_basic().stmt.u_assign().e, id_pool);
                hassert(!b->u_basic().stmt.u_assign().ids.empty());
                for (size_t i = 0; i < b->u_basic().stmt.u_assign().ids.size();
                     ++i) {
                    const auto &id = b->u_basic().stmt.u_assign().ids[i];
                    const auto &root = b->u_basic().stmt.u_assign().e.roots[i];
                    hassert(root->width == id_pool.getBitwidth(id));
                }
                break;
            case StatementType::Receive:
                // check that the channel has the same width as the variable
                if (b->u_basic().stmt.u_receive().var)
                    hassert(id_pool.getBitwidth(
                                b->u_basic().stmt.u_receive().chan) ==
                            id_pool.getBitwidth(
                                *b->u_basic().stmt.u_receive().var));
                break;
            case StatementType::Send:
                // check that the channel has the same width as the expression,
                // and that the expression dag is well-formed
                validateExprSingleRootDag(b->u_basic().stmt.u_send().e,
                                          id_pool);
                hassert(id_pool.getBitwidth(b->u_basic().stmt.u_send().chan) ==
                        b->u_basic().stmt.u_send().e.width());
                break;
            }
            break;
        }
        case BlockType::Par:
            break;
        case BlockType::DoLoop:
            // check that the guard is well-formed and has a width of 1
            validateExprSingleRootDag(b->u_doloop().guard, id_pool);
            hassert(b->u_doloop().guard.width() == 1);
            break;
        case BlockType::Select:
            // check that there is at most one else branch, and that each guard
            // is well-formed and has a width of 1
            hassert(
                Algo::count_if(b->u_select().branches, [](const auto &branch) {
                    return branch.g.type() == IRGuardType::Else;
                }) <= 1);
            for (const auto &branch : b->u_select().branches) {
                if (branch.g.type() == IRGuardType::Expression) {
                    validateExprSingleRootDag(branch.g.u_e().e, id_pool);
                    hassert(branch.g.u_e().e.width() == 1);
                }
            }
            break;
        }

        // Check the uniqueness of sequence start and stop blocks
        if (b->type() == BlockType::Par) {
            for (const Sequence &path : b->u_par().branches) {
                hassert(path.startseq != path.endseq);
                hassert(!m_caps.count(path.startseq));
                hassert(!m_caps.count(path.endseq));
                m_caps.insert(path.startseq);
                m_caps.insert(path.endseq);
            }
        } else if (b->type() == BlockType::Select) {
            for (const SelectBranch &gs : b->u_select().branches) {
                hassert(gs.seq.startseq != gs.seq.endseq);
                hassert(!m_caps.count(gs.seq.startseq));
                hassert(!m_caps.count(gs.seq.endseq));
                m_caps.insert(gs.seq.startseq);
                m_caps.insert(gs.seq.endseq);
            }
        } else if (b->type() == BlockType::DoLoop) {
            hassert(b->u_doloop().branch.startseq !=
                    b->u_doloop().branch.endseq);
            hassert(!m_caps.count(b->u_doloop().branch.startseq));
            hassert(!m_caps.count(b->u_doloop().branch.endseq));
            m_caps.insert(b->u_doloop().branch.startseq);
            m_caps.insert(b->u_doloop().branch.endseq);
        }

        // Then recurse down into each child sequence
        switch (b->type()) {
        case BlockType::StartSequence:
        case BlockType::EndSequence:
            hassert(false);
            break;
        case BlockType::Basic:
            break;
        case BlockType::Par:
            for (const auto &path : b->u_par().branches)
                validateSeqInvariantsRecursive(path, id_pool);
            break;
        case BlockType::DoLoop:
            validateSeqInvariantsRecursive(b->u_doloop().branch, id_pool);
            break;
        case BlockType::Select:
            for (const auto &branch : b->u_select().branches)
                validateSeqInvariantsRecursive(branch.seq, id_pool);
            hassert(!b->u_select().branches.empty());
            break;
        }
    }
}
} // namespace

void validateGraphInvariants(const Sequence &seq, const IdPool &id_pool) {
    validateSeqInvariantsRecursive(seq, id_pool);
}

namespace {

template <FlowDirection dir>
void makeReversePostorder_helper(Sequence &seq,
                                 std::vector<Block *> &blocks_output) {
    Block *curr = dir == FlowDirection::forward ? seq.startseq->child()
                                                : seq.endseq->parent();
    Block *end = dir == FlowDirection::forward ? seq.endseq : seq.startseq;
    while (curr != end) {
        switch (curr->type()) {
        case BlockType::StartSequence:
        case BlockType::EndSequence:
            hassert(false); // FALLTHROUGH
        case BlockType::Par:
            for (Sequence &path : curr->u_par().branches)
                makeReversePostorder_helper<dir>(path, blocks_output);
            break;
        case BlockType::Select:
            for (SelectBranch &branch : curr->u_select().branches)
                makeReversePostorder_helper<dir>(branch.seq, blocks_output);
            break;
        case BlockType::DoLoop:
            makeReversePostorder_helper<dir>(curr->u_doloop().branch,
                                             blocks_output);
            break;
        case BlockType::Basic:
            // just do the stuff below
            break;
        }
        blocks_output.push_back(curr);
        curr = dir == FlowDirection::forward ? curr->child() : curr->parent();
        hassert(curr);
    }
}
} // namespace

std::vector<Block *> ChpGraph::makeReversePostorder(FlowDirection dir) {
    std::vector<Block *> blocks_output;
    switch (dir) {
    case FlowDirection::forward:
        makeReversePostorder_helper<FlowDirection::forward>(m_seq,
                                                            blocks_output);
        break;
    case FlowDirection::backward:
        makeReversePostorder_helper<FlowDirection::backward>(m_seq,
                                                             blocks_output);
        break;
    }
    return blocks_output;
}

std::vector<VarId> ChpGraph::allUsedVarIds() const {

    std::unordered_set<VarId> ids;
    auto blocks = const_cast<ChpGraph *>(this)->getLiveBlocks();
    for (Block *b : blocks) {
        switch (b->type()) {
        case BlockType::StartSequence:
        case BlockType::EndSequence:
            break;
        case BlockType::Basic:
            // Usage in a line supersedes definition in that line, so that code
            // is run afterwords
            switch (b->u_basic().stmt.type()) {
            case StatementType::Assign: {
                for (const auto id : b->u_basic().stmt.u_assign().ids)
                    ids.insert(id);
                addIdsUsedByExpr(ids, b->u_basic().stmt.u_assign().e);
                break;
            }
            case StatementType::Send:
                addIdsUsedByExpr(ids, b->u_basic().stmt.u_send().e.root());
                break;
            case StatementType::Receive:
                if (b->u_basic().stmt.u_receive().var)
                    ids.insert(*b->u_basic().stmt.u_receive().var);
                break;
            }
            break;
        case BlockType::Select: {
            for (const SelectBranch &gb : b->u_select().branches) {
                if (gb.g.type() != IRGuardType::Else) {
                    hassert(gb.g.type() == IRGuardType::Expression);
                    addIdsUsedByExpr(ids, gb.g.u_e().e.root());
                }
            }
            break;
        }
        case BlockType::DoLoop: {
            addIdsUsedByExpr(ids, b->u_doloop().guard.root());
            break;
        }
        case BlockType::Par:
            break;
        }
    }

    return Algo::as_sorted_vector<VarId>(ids);
}

// given the structure A0 -> CB -> B0 -> ... -> BN -> CB_MATCH -> A1
//
// flatten it to the structure A0 -> B0 -> ... -> BN -> A1
/*static*/ void ChpGraph::spliceOutFlatNonemptyControlBlock(Block *cb,
                                                            MarkDead markDead) {
    hassert(cb);
    hassert(cb->type() == BlockType::Select || cb->type() == BlockType::Par ||
            cb->type() == BlockType::DoLoop);
    auto before = cb->parent();
    auto after = cb->child();
    Block::disconnect(before, cb);
    Block::disconnect(cb, after);

    Sequence &seq = [&]() -> Sequence & {
        switch (cb->type()) {
        case BlockType::StartSequence:
        case BlockType::EndSequence:
        case BlockType::Basic:
            hassert(false); // FALLTHROUGH
        case BlockType::Par:
            hassert(cb->u_par().branches.size() == 1);
            return cb->u_par().branches.front();
        case BlockType::Select:
            hassert(cb->u_select().branches.size() == 1);
            return cb->u_select().branches.front().seq;
        case BlockType::DoLoop:
            return cb->u_doloop().branch;
        }
        hassert(false);
        return cb->u_doloop().branch;
    }();

    if (seq.startseq->child() == seq.endseq) {
        // the sequence is empty
        hassert(seq.endseq->parent() == seq.startseq);
        Block::connect(before, after);
    } else {
        auto inside_start = seq.startseq->child();
        auto inside_end = seq.endseq->parent();
        Block::disconnect(seq.startseq, inside_start);
        Block::disconnect(inside_end, seq.endseq);
        Block::connect(before, inside_start);
        Block::connect(inside_end, after);
    }

    if (markDead == MarkDead::yes) {
        cb->dead = true;
    }
}

// given the structure A0 -> CB -> CB_MATCH -> A1
//
// flatten it to the structure A0 -> A1
/*static*/ void ChpGraph::spliceOutEmptyControlBlock(Block *cb,
                                                     MarkDead markDead) {
    hassert(cb);
    switch (cb->type()) {
    case BlockType::StartSequence:
    case BlockType::EndSequence:
    case BlockType::Basic:
        hassert(false); // FALLTHROUGH
    case BlockType::DoLoop:
        // DoLoops always have a branch, so they cannot be empty
        hassert(false); // FALLTHROUGH
    case BlockType::Par:
        hassert(cb->u_par().branches.empty());
        break;
    case BlockType::Select:
        hassert(cb->u_select().branches.empty());
        break;
    }

    auto before = cb->parent();
    auto after = cb->child();
    Block::disconnect(before, cb);
    Block::disconnect(cb, after);

    Block::connect(before, after);

    if (markDead == MarkDead::yes) {
        cb->dead = true;
    }
}

namespace {
void markSequenceDead(Sequence &seq) {
    Block *b = seq.startseq;
    while (b->type() != BlockType::EndSequence) {
        b->dead = true;
        switch (b->type()) {
        case BlockType::EndSequence:
            hassert(false); // FALLTHROUGH
        case BlockType::StartSequence:
        case BlockType::Basic:
            break;
        case BlockType::Par:
            for (Sequence &path : b->u_par().branches)
                markSequenceDead(path);
            break;
        case BlockType::Select:
            for (SelectBranch &gs : b->u_select().branches)
                markSequenceDead(gs.seq);
            break;
        case BlockType::DoLoop:
            markSequenceDead(b->u_doloop().branch);
            break;
        }
        b = b->child();
    }
    hassert(b == seq.endseq);
    b->dead = true;
}
} // namespace

/*static*/ void ChpGraph::spliceOutControlBlockBranch(Block *cb,
                                                      const Block *branch_start,
                                                      MarkDead markDead) {
    switch (cb->type()) {

    case BlockType::Basic:
    case BlockType::StartSequence:
    case BlockType::EndSequence:
    case BlockType::DoLoop:
        hassert(false); // FALLTHROUGH
    case BlockType::Par: {
        auto it = Algo::find_assert_if(cb->u_par().branches,
                                       [&](const Sequence &path) {
                                           return path.startseq == branch_start;
                                       });
        if (markDead == MarkDead::yes)
            markSequenceDead(*it);
        cb->u_par().branches.erase(it);
        break;
    }
    case BlockType::Select: {
        auto it = Algo::find_assert_if(
            cb->u_select().branches, [&](const SelectBranch &gs) {
                return gs.seq.startseq == branch_start;
            });
        if (markDead == MarkDead::yes)
            markSequenceDead(it->seq);
        cb->u_select().branches.erase(it);
        break;
    }
    }
}

/*static*/ void ChpGraph::spliceOutControlBlockBranches(
    Block *cb, const std::vector<const Block *> &branch_starts,
    MarkDead markDead) {
    for (const Block *branch_start : branch_starts) {
        spliceOutControlBlockBranch(cb, branch_start, markDead);
    }
}

// remove all the basic blocks between `b` (inclusive) and the next "match"
// control block (exclusive) Block must NOT BE a match control block
/*static*/ void ChpGraph::spliceOutUntilEndSequence(Block *b,
                                                    MarkDead markDead) {
    hassert(b->type() != BlockType::StartSequence);
    if (b->type() == BlockType::EndSequence)
        return;
    Block *before = b->parent();
    Block::disconnect(before, b);
    if (markDead == MarkDead::yes) {
        while (b->type() != BlockType::EndSequence) {
            b->dead = true;
            switch (b->type()) {
            case BlockType::StartSequence:
            case BlockType::EndSequence:
                hassert(false); // FALLTHROUGH
            case BlockType::Basic:
                break;
            case BlockType::Par:
                for (Sequence &path : b->u_par().branches)
                    markSequenceDead(path);
                break;
            case BlockType::Select:
                for (SelectBranch &gs : b->u_select().branches)
                    markSequenceDead(gs.seq);
                break;
            case BlockType::DoLoop:
                markSequenceDead(b->u_doloop().branch);
                break;
            }
            b = b->child();
        }
    }
    Block::disconnect(b->parent(), b);
    Block::connect(before, b);
}

// given the structure A0 -> BB -> A1
//
// flatten it to the structure A0 -> A1
/*static*/ void ChpGraph::spliceOutBasicBlock(Block *bb, MarkDead markDead) {
    // otherwise, we are safe to remove the skip
    Block *before = bb->parent();
    Block *after = bb->child();

    Block::disconnect(before, bb);
    Block::disconnect(bb, after);
    Block::connect(before, after);

    if (markDead == MarkDead::yes) {
        bb->dead = true;
    }
}

// flatten the structure A -> BB  into the structure  A -> B0 -> ... -> BN -> BB
/*static*/ void ChpGraph::spliceInSequenceBetween(Block *before, Block *after,
                                                  std::vector<Block *> blocks) {
    if (blocks.empty())
        return;

    for (const Block *block : blocks) {
        hassert(block->u_basic().child == nullptr);
        hassert(block->u_basic().parent == nullptr);
    }

    Block::disconnect(before, after);
    Block::connect(before, blocks.front());
    for (ssize_t i = 0; i != (ssize_t)blocks.size() - 1; ++i) {
        Block::connect(blocks[i], blocks[i + 1]);
    }
    Block::connect(blocks.back(), after);
}
/*static*/ void ChpGraph::spliceInSequenceBetween(Block *before, Block *after,
                                                  Sequence seq) {
    if (seq.empty())
        return;

    auto seq_begin = seq.startseq->child();
    auto seq_last = seq.endseq->parent();
    Block::disconnect(seq.startseq, seq_begin);
    Block::disconnect(seq_last, seq.endseq);

    Block::disconnect(before, after);
    Block::connect(before, seq_begin);
    Block::connect(seq_last, after);
}

/*static*/ void
ChpGraph::spliceInControlBlockBranch(Block *cb, Sequence seq,
                                     std::optional<IRGuard> guard) {
    switch (cb->type()) {
    case BlockType::Basic:
    case BlockType::DoLoop:
    case BlockType::StartSequence:
    case BlockType::EndSequence:
        hassert(false);
        break;
    case BlockType::Par: {
        hassert(!static_cast<bool>(guard));
        cb->u_par().branches.push_back(seq);
        break;
    }
    case BlockType::Select: {
        hassert(static_cast<bool>(guard));
        cb->u_select().branches.emplace_back(seq, std::move(*guard));
        break;
    }
    }
}

bool ChpGraph::iterInnerPaths(Sequence &seq,
                              const std::function<bool(Block *, Block *)> &fn) {
    bool any = false;
    Block *b = seq.startseq->child();

    // This is intentionally nullptr if b is not a BasicBlock
    Block *last_first_bb = b->type() == BlockType::Basic ? b : nullptr;

    while (b != seq.endseq) {
        hassert(b);
        switch (b->type()) {
        case BlockType::StartSequence:
        case BlockType::EndSequence:
            hassert(false); // FALLTHROUGH
        case BlockType::Basic: {
            if (!last_first_bb)
                last_first_bb = b;
            break;
        }
        case BlockType::Par: {
            if (last_first_bb) {
                // then there is an inner path from last_first_bb to here
                fn(last_first_bb, b);
                last_first_bb = nullptr;
            }

            // copy so if fn alters stuff it doesnt invalidate things
            std::vector<Sequence> sub_seqs = Algo::map1<Sequence>(
                b->u_par().branches, [&](const Sequence &seq) { return seq; });
            for (Sequence &sub_seq : sub_seqs) {
                any |= iterInnerPaths(sub_seq, fn);
            }
            break;
        }
        case BlockType::Select: {
            if (last_first_bb) {
                // the there is an inner path from last_first_bb to here
                fn(last_first_bb, b);
                last_first_bb = nullptr;
            }

            // copy so if fn alters stuff it doesnt invalidate things
            std::vector<Sequence> sub_seqs = Algo::map1<Sequence>(
                b->u_select().branches,
                [&](const SelectBranch &gs) { return gs.seq; });
            for (Sequence &sub_seq : sub_seqs) {
                any |= iterInnerPaths(sub_seq, fn);
            }
            break;
        }
        case BlockType::DoLoop: {
            if (last_first_bb) {
                // the there is an inner path from last_first_bb to here
                fn(last_first_bb, b);
                last_first_bb = nullptr;
            }

            // copy so if fn alters stuff it doesnt invalidate things
            any |= iterInnerPaths(b->u_doloop().branch, fn);
            break;
        }
        }
        b = b->child();
    }

    if (last_first_bb) {
        // the there is an inner path from last_first_bb to the end
        fn(last_first_bb, seq.endseq);
    }
    return any;
}

void ChpGraph::validateGraphInvariants() {
    ChpOptimize::validateGraphInvariants(m_seq, m_id_pool);
}

} // namespace ChpOptimize
