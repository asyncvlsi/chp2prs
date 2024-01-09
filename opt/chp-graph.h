#pragma once
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

#include "ir-expr-dag.h"
#include "ir-expr.h"
#include "ir-id.h"
#include <deque>
#include <functional>
#include <list>
#include <map>
#include <memory>
#include <utility>
#include <vector>

struct act_chp_lang;

namespace ChpOptimize {

struct ChpTag {};

} // namespace ChpOptimize

template <> struct ::std::hash<::ChpOptimize::ChpTag> {
    size_t operator()(const ::ChpOptimize::ChpTag & /*obj*/) const { return 0; }
};


namespace ChpOptimize {

using ChpExpr = IRExpr<ChpTag, VarId, ManageMemory::yes>;
using ChpExprDag = IRExprDag<ChpTag, VarId>;
using ChpExprSingleRootDag = IRExprSingleRootDag<ChpTag, VarId>;

enum class IRGuardType { Else, Expression };

struct IRGuard {
  public:
    struct Variant_Expression {
        ChpExprSingleRootDag e;
        explicit Variant_Expression(ChpExprSingleRootDag e_)
            : e{std::move(e_)} {};

        Variant_Expression() = default;
        ~Variant_Expression() = default;
        Variant_Expression(Variant_Expression &&o) = default;
        Variant_Expression &operator=(Variant_Expression &&o) = default;

        Variant_Expression(const Variant_Expression &o)
            : e{ChpExprSingleRootDag::deep_copy(o.e)} {}
        Variant_Expression &operator=(const Variant_Expression &o) {
            e = ChpExprSingleRootDag::deep_copy(o.e);
            return *this;
        }
    };
    struct Variant_Else {};

  private:
    using Variant_t =
        TypedVariant2<IRGuardType, Variant_Else,
                      IRGuardType::Else, // the default option
                      Variant_Expression, IRGuardType::Expression>;
    Variant_t u;

    explicit IRGuard(Variant_t u_)
        : u{std::move(u_)} {}

  public:
    [[nodiscard]] Variant_Else &u_else() { return u.u_v0(); }
    [[nodiscard]] const Variant_Else &u_else() const { return u.u_v0(); }
    [[nodiscard]] Variant_Expression &u_e() { return u.u_v1(); }
    [[nodiscard]] const Variant_Expression &u_e() const { return u.u_v1(); }
    [[nodiscard]] IRGuardType type() const { return u.type(); }

    IRGuard() = default;
    ~IRGuard() = default;
    IRGuard(IRGuard &&o) = default;
    IRGuard &operator=(IRGuard &&o) noexcept = default;

    IRGuard(const IRGuard &) = delete;
    IRGuard &operator=(const IRGuard &) = delete;
    static IRGuard deep_copy(const IRGuard &o) {
        IRGuard result;
        result.u = o.u;
        return result;
    }

    static IRGuard makeExpression(ChpExprSingleRootDag e) {
        return IRGuard{Variant_t{Variant_Expression{std::move(e)}}};
    }
    static IRGuard makeElse() { return IRGuard{Variant_t{Variant_Else{}}}; }
};

enum class StatementType { Assign, Send, Receive };

struct Statement {
  public:
    struct Variant_Assign {
        std::vector<VarId> ids;
        ChpExprDag e;

        Variant_Assign(std::vector<VarId> ids_, ChpExprDag e_)
            : ids{std::move(ids_)}
            , e{std::move(e_)} {}

        Variant_Assign() = default;
        ~Variant_Assign() = default;
        Variant_Assign(Variant_Assign &&o) = default;
        Variant_Assign &operator=(Variant_Assign &&o) = default;

        Variant_Assign(const Variant_Assign &o)
            : ids{o.ids}
            , e{ChpExprDag::deep_copy(o.e)} {}
        Variant_Assign &operator=(const Variant_Assign &o) {
            ids = o.ids;
            e = ChpExprDag::deep_copy(o.e);
            return *this;
        }
    };
    struct Variant_Send {
        ChanId chan;
        // ActId *var; /* variable for assignment */
        ChpExprSingleRootDag e; /* expression to be sent */
        // int flavor; /* up, down, blank */

        Variant_Send(ChanId chan_, ChpExprSingleRootDag e_)
            : chan{chan_}
            , e{std::move(e_)} {}

        Variant_Send() = default;
        ~Variant_Send() = default;
        Variant_Send(Variant_Send &&o) = default;
        Variant_Send &operator=(Variant_Send &&o) = default;

        Variant_Send(const Variant_Send &o)
            : chan{o.chan}
            , e{ChpExprSingleRootDag::deep_copy(o.e)} {}
        Variant_Send &operator=(const Variant_Send &o) {
            chan = o.chan;
            e = ChpExprSingleRootDag::deep_copy(o.e);
            return *this;
        }
    };
    struct Variant_Receive {
        ChanId chan;
        OptionalVarId var;
    };

  private:
    using Variant_t =
        TypedVariant3<StatementType, Variant_Assign, StatementType::Assign,
                      Variant_Receive, StatementType::Receive, Variant_Send,
                      StatementType::Send>;
    Variant_t u;

    explicit Statement(Variant_t u_)
        : u{std::move(u_)} {}

  public:
    [[nodiscard]] Variant_Assign &u_assign() { return u.u_v0(); }
    [[nodiscard]] const Variant_Assign &u_assign() const { return u.u_v0(); }
    [[nodiscard]] Variant_Receive &u_receive() { return u.u_v1(); }
    [[nodiscard]] const Variant_Receive &u_receive() const { return u.u_v1(); }
    [[nodiscard]] Variant_Send &u_send() { return u.u_v2(); }
    [[nodiscard]] const Variant_Send &u_send() const { return u.u_v2(); }

    [[nodiscard]] StatementType type() const { return u.type(); }

    // put us into some valid state

    Statement() = default;
    ~Statement() = default;
    Statement(Statement &&o) noexcept = default;
    Statement &operator=(Statement &&o) noexcept = default;

    Statement(const Statement &) = delete;
    Statement &operator=(const Statement &) = delete;

    static Statement deep_copy(const Statement &o) {
        return Statement{Variant_t{o.u}};
    }

    static Statement makeAssignment(std::vector<VarId> vars, ChpExprDag val) {
        return Statement{
            Variant_t{Variant_Assign{std::move(vars), std::move(val)}}};
    }
    static Statement makeAssignment(VarId var, ChpExprSingleRootDag val) {
        return makeAssignment({var}, std::move(val.m_dag));
    }

    static Statement makeSend(ChanId chan, ChpExprSingleRootDag e) {
        return Statement{Variant_t{Variant_Send{chan, std::move(e)}}};
    }

    static Statement makeReceive(ChanId chan, OptionalVarId var) {
        return Statement{Variant_t{Variant_Receive{chan, var}}};
    }
};

class Block;

class Sequence {
  public:
    // The particular `scope_start` and `scope_end` blocks are owned by this
    // GuardedScope.
    // TODO make these unique ptrs?
    Block *startseq; // a Block with type ScopeStart
    Block *endseq;   // a Block with type ScopeEnd

    [[nodiscard]] bool empty() const;

    Sequence()
        : startseq{nullptr}
        , endseq{nullptr} {}
    Sequence(Block *startseq, Block *endseq)
        : startseq{startseq}
        , endseq{endseq} {
        hassert(startseq);
        hassert(endseq);
        // TODO hassert they are actually connected
    }
};

class SelectBranch {
  public:
    Sequence seq;
    IRGuard g;

    SelectBranch() = default;
    SelectBranch(Sequence seq, IRGuard t_g)
        : seq{seq}
        , g(std::move(t_g)) {}
};

enum class BlockType { Basic, Par, Select, DoLoop, StartSequence, EndSequence };
class Block {
  public:
    // a lot of code makes certain blocks dead halfway through code generation.
    // This is a way of marking dead blocks because invariants might not longer
    // hold on them
    bool dead = false;

    struct Variant_StartSequence {
        Block *child;
        Block *_; // parent
        Variant_StartSequence()
            : child{nullptr}
            , _{nullptr} {}
    };
    struct Variant_EndSequence {
      private:
// gives parent the same offset as in the code bellow. I think this _should_
// allow a smart compiler to merge together certain cases in switch statements
// if hasserts are off
#ifdef __clang__
#pragma clang diagnostic ignored "-Wunused-private-field"
#endif
        Block *_;
#ifdef __clang__
#pragma clang diagnostic warning "-Wunused-private-field"
#endif
      public:
        Block *parent;
        Variant_EndSequence()
            : _{nullptr}
            , parent{nullptr} {}
    };
    struct Variant_BasicBlock {
      public:
        Block *child;
        Block *parent;
        Statement stmt;

        Variant_BasicBlock(Statement stmt, Block *child, Block *parent)
            : child{child}
            , parent{parent}
            , stmt{std::move(stmt)} {}
        explicit Variant_BasicBlock(Statement t_stmt)
            : Variant_BasicBlock(std::move(t_stmt), nullptr, nullptr) {}

        Variant_BasicBlock()
            : Variant_BasicBlock{Statement{}} {}
        ~Variant_BasicBlock() = default;
        Variant_BasicBlock(Variant_BasicBlock &&) = default;
        Variant_BasicBlock &operator=(Variant_BasicBlock &&) = default;
        Variant_BasicBlock(const Variant_BasicBlock &o) = delete;
        Variant_BasicBlock &operator=(const Variant_BasicBlock &o) = delete;
    };
    struct Variant_Par {
      public:
        struct PhiSplit {
            VarId pre_id;
            std::vector<OptionalVarId> branch_ids;
        };
        struct PhiMerge {
            std::vector<OptionalVarId>
                branch_ids; // exactly one of these can be non-null
            VarId post_id;
        };

        Block *child;
        Block *parent;
        std::list<Sequence> branches;

       /* this is a list of variable splits, one for each variable used in
	  a parallel branch.
	  This is used by the H-STF form.
       */
        std::vector<PhiSplit> splits;

      /* this is a list of defines, one for each variable defined in a
	 parallel branch.
	 This is used by the H-STF form.
      */
        std::vector<PhiMerge> merges; 

        Variant_Par(std::list<Sequence> branches, Block *child, Block *parent)
            : child{child}
            , parent{parent}
            , branches{std::move(branches)} {}

        Variant_Par()
            : Variant_Par({}, nullptr, nullptr) {}
        ~Variant_Par() = default;
        Variant_Par(Variant_Par &&) = default;
        Variant_Par &operator=(Variant_Par &&) = default;
        Variant_Par(const Variant_Par &o) = delete;
        Variant_Par &operator=(const Variant_Par &o) = delete;
    };
    struct Variant_Select {
      public:
        struct PhiSplit {
            VarId pre_id;
            std::vector<OptionalVarId> branch_ids;
        };
        struct PhiMerge {
            std::vector<VarId> branch_ids;
            VarId post_id;
        };

        Block *child;
        Block *parent;
        std::list<SelectBranch> branches;
        std::vector<PhiSplit> splits;
        std::vector<PhiMerge> merges;

        Variant_Select(std::list<SelectBranch> branches, Block *child,
                       Block *parent)
            : child{child}
            , parent{parent}
            , branches{std::move(branches)} {}

        Variant_Select()
            : Variant_Select({}, nullptr, nullptr) {}
        ~Variant_Select() = default;
        Variant_Select(Variant_Select &&) = default;
        Variant_Select &operator=(Variant_Select &&) = default;
        Variant_Select(const Variant_Select &o) = delete;
        Variant_Select &operator=(const Variant_Select &o) = delete;
    };
    struct Variant_DoLoop {
        // we conceive of a DoLoops guard as being run during the endseq block
        // in its subsequence
      public:
      
        /* This maps an inbound variable to a new value */
        struct InPhi {
            VarId pre_id;
            VarId bodyin_id;
        };

        /* This maps an outbound variable to its final value */
        struct OutPhi {
            VarId bodyout_id;
            VarId post_id;
        };

        /* 
	   This contains the loop-inbound and loop-carried id, and has
	   body_in mapping and body_out mapping
	*/
        struct LoopPhi {
            VarId pre_id;
            VarId bodyin_id;
            VarId bodyout_id;
            OptionalVarId post_id;
        };

        Block *child;
        Block *parent;
        Sequence branch;
        ChpExprSingleRootDag guard;
        std::vector<InPhi> in_phis;
        std::vector<OutPhi> out_phis;
        std::vector<LoopPhi> loop_phis;

        Variant_DoLoop(Sequence branch, ChpExprSingleRootDag guard,
                       Block *child, Block *parent)
            : child{child}
            , parent{parent}
            , branch{branch}
            , guard{std::move(guard)} {}

        Variant_DoLoop()
            : Variant_DoLoop({}, {}, nullptr, nullptr) {}
        ~Variant_DoLoop() = default;
        Variant_DoLoop(Variant_DoLoop &&) = default;
        Variant_DoLoop &operator=(Variant_DoLoop &&) = default;
        Variant_DoLoop(const Variant_DoLoop &o) = delete;
        Variant_DoLoop &operator=(const Variant_DoLoop &o) = delete;
    };

  private:
    using Variant_t =
        TypedVariant6<BlockType, Variant_BasicBlock, BlockType::Basic,
                      Variant_Par, BlockType::Par, Variant_Select,
                      BlockType::Select, Variant_DoLoop, BlockType::DoLoop,
                      Variant_StartSequence, BlockType::StartSequence,
                      Variant_EndSequence, BlockType::EndSequence>;

    Variant_t u;

    explicit Block(Variant_t u)
        : u{std::move(u)} {}

    static Block makeStartSequence() {
        return Block{Variant_t{Variant_StartSequence{}}};
    }
    static Block makeEndSequence() {
        return Block{Variant_t{Variant_EndSequence{}}};
    }

  public:
    [[nodiscard]] Variant_BasicBlock &u_basic() { return u.u_v0(); }
    [[nodiscard]] const Variant_BasicBlock &u_basic() const { return u.u_v0(); }
    [[nodiscard]] Variant_Par &u_par() { return u.u_v1(); }
    [[nodiscard]] const Variant_Par &u_par() const { return u.u_v1(); }
    [[nodiscard]] Variant_Select &u_select() { return u.u_v2(); }
    [[nodiscard]] const Variant_Select &u_select() const { return u.u_v2(); }
    [[nodiscard]] Variant_DoLoop &u_doloop() { return u.u_v3(); }
    [[nodiscard]] const Variant_DoLoop &u_doloop() const { return u.u_v3(); }
    [[nodiscard]] Variant_StartSequence &u_startseq() { return u.u_v4(); }
    [[nodiscard]] const Variant_StartSequence &u_startseq() const {
        return u.u_v4();
    }
    [[nodiscard]] Variant_EndSequence &u_endseq() { return u.u_v5(); }
    [[nodiscard]] const Variant_EndSequence &u_endseq() const {
        return u.u_v5();
    }

    [[nodiscard]] BlockType type() const { return u.type(); }

    static Block makeBasicBlock(Statement stmt) {
        return Block{Variant_t{Variant_BasicBlock{std::move(stmt), {}, {}}}};
    }
    static Block makeParBlock() {
        return Block{Variant_t{Variant_Par{{}, nullptr, nullptr}}};
    }
    static Block makeSelectBlock() {
        return Block{Variant_t{Variant_Select{{}, nullptr, nullptr}}};
    }
    static Block makeDoLoopBlock() {
        return Block{Variant_t{Variant_DoLoop{{}, {}, nullptr, nullptr}}};
    }

    [[nodiscard]] Block *&parent() {
        switch (type()) {
        case BlockType::StartSequence:
            hassert(false); // FALLTHROUGH
        case BlockType::EndSequence:
            return u_endseq().parent;
        case BlockType::Basic:
            return u_basic().parent;
        case BlockType::Par:
            return u_par().parent;
        case BlockType::Select:
            return u_select().parent;
        case BlockType::DoLoop:
            return u_doloop().parent;
        }
        hassert(false);
        return u_doloop().parent;
    }
    [[nodiscard]] const Block *parent() const {
        switch (type()) {
        case BlockType::StartSequence:
            hassert(false); // FALLTHROUGH
        case BlockType::EndSequence:
            return u_endseq().parent;
        case BlockType::Basic:
            return u_basic().parent;
        case BlockType::Par:
            return u_par().parent;
        case BlockType::Select:
            return u_select().parent;
        case BlockType::DoLoop:
            return u_doloop().parent;
        }
        hassert(false);
        return u_doloop().parent;
    }
    [[nodiscard]] Block *&child() {
        switch (type()) {
        case BlockType::StartSequence:
            return u_startseq().child;
        case BlockType::EndSequence:
            hassert(false); // FALLTHROUGH
        case BlockType::Basic:
            return u_basic().child;
        case BlockType::Par:
            return u_par().child;
        case BlockType::Select:
            return u_select().child;
        case BlockType::DoLoop:
            return u_doloop().child;
        }
        hassert(false);
        return u_doloop().child;
    }
    [[nodiscard]] const Block *child() const {
        switch (type()) {
        case BlockType::StartSequence:
            return u_startseq().child;
        case BlockType::EndSequence:
            hassert(false); // FALLTHROUGH
        case BlockType::Basic:
            return u_basic().child;
        case BlockType::Par:
            return u_par().child;
        case BlockType::Select:
            return u_select().child;
        case BlockType::DoLoop:
            return u_doloop().child;
        }
        hassert(false);
        return u_doloop().child;
    }

    static void connect(Block *parent, Block *child) {
        hassert(parent);
        hassert(child);
        hassert(parent->child() == nullptr);
        parent->child() = child;
        hassert(child->parent() == nullptr);
        child->parent() = parent;
    }
    static void disconnect(Block *parent, Block *child) {
        hassert(parent);
        hassert(child);
        hassert(parent->child() == child);
        parent->child() = nullptr;
        hassert(child->parent() == parent);
        child->parent() = nullptr;
    }

    Block() = default;
    ~Block() = default;
    Block(Block &&) = default;
    Block &operator=(Block &&) = default;
    Block(const Block &) = delete;
    Block &operator=(const Block &) = delete;

    friend class BlockAllocator;
};

inline bool Sequence::empty() const {
    hassert(startseq);
    hassert(endseq);
    bool b = startseq->child() == endseq;
    hassert(b == (endseq->parent() == startseq));
    return b;
}

class BlockAllocator {
    std::deque<Block> m_blocks;

  public:
    Block *newBlock(Block b) {
        m_blocks.emplace_back(std::move(b));
        return &m_blocks.back();
    }
    Sequence newSequence() {
        Block *start = newBlock(Block::makeStartSequence());
        Block *end = newBlock(Block::makeEndSequence());
        start->u_startseq().child = nullptr;
        end->u_endseq().parent = nullptr;
        return Sequence{start, end};
    }
    Sequence newSequence(const std::vector<Block *> &blocks) {
        Sequence seq = newSequence();
        if (blocks.empty()) {
            Block::connect(seq.startseq, seq.endseq);
            return seq;
        }
        Block::connect(seq.startseq, blocks.front());
        for (ssize_t i = 0; i < (ssize_t)blocks.size() - 1; ++i) {
            Block::connect(blocks[i], blocks[i + 1]);
        }
        Block::connect(blocks.back(), seq.endseq);
        return seq;
    }
    Sequence newSequence(Block *entry, Block *exit_) {
        // hassert that `entry` is connected to `exit_`
        Sequence seq = newSequence();
        Block::connect(seq.startseq, entry);
        Block::connect(exit_, seq.endseq);
        return seq;
    }
};

// Given the outermost sequence, this will validate that the chp program is
// well-formed. It will check the following: 1) Every sequence is a
// doubly-linked list from seq.startseq to seq.endseq 2) Every expression has
// correct widths 3) Across all the sequences and its child sequences, each one
// has a unique start and stop block
void validateGraphInvariants(const Sequence &seq, const IdPool &id_pool);

enum class FlowDirection { backward, forward };
enum class MarkDead { no, yes };

class ChpGraph {
    BlockAllocator m_blockAllocator;

    IdPool m_id_pool;

  public:
    bool is_static_token_form = false;
    Sequence m_seq;

    [[nodiscard]] IdPool &id_pool() { return m_id_pool; }
    [[nodiscard]] const IdPool &id_pool() const { return m_id_pool; }
    [[nodiscard]] BlockAllocator &blockAllocator() { return m_blockAllocator; }
    [[nodiscard]] const BlockAllocator &blockAllocator() const {
        return m_blockAllocator;
    }

    ChpGraph() = default;

    [[nodiscard]] std::vector<Block *> getLiveBlocks() {
        return makeReversePostorder(FlowDirection::forward);
    }

    [[nodiscard]] Block *newBasicBlock(Statement t_stmt) {
        return m_blockAllocator.newBlock(
            Block::makeBasicBlock(std::move(t_stmt)));
    }
    [[nodiscard]] Block *newParBlock() {
        return m_blockAllocator.newBlock(Block::makeParBlock());
    }
    [[nodiscard]] Block *newSelectBlock() {
        return m_blockAllocator.newBlock(Block::makeSelectBlock());
    }
    [[nodiscard]] Block *newDoLoopBlock() {
        return m_blockAllocator.newBlock(Block::makeDoLoopBlock());
    }
    [[nodiscard]] Sequence newSequence(const std::vector<Block *> &blocks) {
        return m_blockAllocator.newSequence(blocks);
    }

    [[nodiscard]] std::vector<VarId> allUsedVarIds() const;

    // flatten the structure  A0 -> CB -> B0 -> ... -> BN -> CB_MATCH -> A1 into
    // the structure  A0 -> B0 -> ... -> BN
    // -> A1
    static void spliceOutFlatNonemptyControlBlock(/*Control*/ Block *cb,
                                                  MarkDead markDead);

    // flatten the structure A0 -> CB -> CB_MATCH -> A1  into the structure  A0
    // -> A1
    static void spliceOutEmptyControlBlock(/*Control*/ Block *cb,
                                           MarkDead markDead);

    // remove the branch of a control block, given an iterator into
    // cb->m_children. It returns the next iterator in m_children after [child]
    // has been removed (like std::list<>::erase).
    static void spliceOutControlBlockBranch(/*Control*/ Block *cb,
                                            const Block *startseq,
                                            MarkDead markDead);
    static void
    spliceOutControlBlockBranches(/*Control*/ Block *cb,
                                  const std::vector<const Block *> &startseqs,
                                  MarkDead markDead);

    static void
    spliceInControlBlockBranch(/*Control*/ Block *cb, Sequence seq,
                               std::optional<IRGuard> guard = std::nullopt);

    // remove all the basic blocks between `b` (inclusive) and the end of the
    // sequence. for example `B -> PAR -> X0 -> PAR_MATCH -> X1 -> SELECT_MATCH`
    // would delete all the way to SELECT_MATCH Block must NOT BE a match
    // control block
    static void spliceOutUntilEndSequence(Block *b, MarkDead markDead);

    // flatten the structure A0 -> BB -> A1  into the structure  A0 -> A1
    static void spliceOutBasicBlock(/*Basic*/ Block *bb, MarkDead markDead);
    // This works the same as BasicBlock because a LOOP has its entry and exit
    // point to the LOOP start (not its match)
    //    void spliceOutEmptyLoop(ControlBlock *cb, MarkDead markDead);

    // flatten the structure before -> after  into the structure  before -> B0
    // -> ... -> BN -> after
    static void spliceInSequenceBetween(Block *before, Block *after,
                                        std::vector</*Basic*/ Block *> blocks);
    static void spliceInSequenceBetween(Block *before, Block *after,
                                        Sequence seq);

    // flatten the structure A -> BB  into the structure  A -> B0 -> ... -> BN
    // -> BB
    static void spliceInSequenceBefore(Block *bb,
                                       std::vector</*Basic*/ Block *> blocks) {
        ChpGraph::spliceInSequenceBetween(bb->parent(), bb, std::move(blocks));
    }
    static void spliceInSequenceBefore(Block *bb, Sequence seq) {
        ChpGraph::spliceInSequenceBetween(bb->parent(), bb, seq);
    }
    static void spliceInSequenceAfter(Block *bb,
                                      std::vector</*Basic*/ Block *> blocks) {
        ChpGraph::spliceInSequenceBetween(bb, bb->child(), std::move(blocks));
    }
    static void spliceInSequenceAfter(Block *bb, Sequence seq) {
        ChpGraph::spliceInSequenceBetween(bb, bb->child(), seq);
    }

  private:
    template <typename F>
    static void iter_blocks_in_seq(const Sequence &seq, const F &fn) {
        fn(seq.startseq);
        for (const Block *curr = seq.startseq->child(); curr != seq.endseq;
             curr = curr->child()) {
            switch (curr->type()) {
            case BlockType::Basic:
                break;
            case BlockType::Par:
                for (const auto &branch : curr->u_par().branches)
                    iter_blocks_in_seq(branch, fn);
                break;
            case BlockType::Select:
                for (const auto &branch : curr->u_select().branches) {
                    iter_blocks_in_seq(branch.seq, fn);
                }
                break;
            case BlockType::DoLoop:
                iter_blocks_in_seq(curr->u_doloop().branch, fn);
                break;
            case BlockType::StartSequence:
            case BlockType::EndSequence:
                hassert(false);
                break;
            }
            fn(curr);
        }
        fn(seq.endseq);
    }

  public:
    // This does a post-ordering. It calls the function on a node after it calls
    // it on its children, and calls a function on a node after it calls it on
    // its predecessor in the sequence. It WILL call the function on the
    // StartSeq and EndSeq nodes
    template <typename F>
    static void iter_blocks(const ChpGraph &graph, const F &fn) {
        iter_blocks_in_seq(graph.m_seq, fn);
    }

  private:
    bool iterInnerPaths(Sequence &seq,
                        const std::function<bool(Block *, Block *)> &fn);

  public:
    bool iterInnerPaths(const std::function<bool(Block *, Block *)> &fn) {
        return iterInnerPaths(m_seq, fn);
    }

    void validateGraphInvariants();

  public:
    // void print(FILE *fp, FlowTable &flow_data);
    // void print(FILE *fp, FlowTable &flow_table, BlockPrinter aux_printer);
    std::vector<Block *> makeReversePostorder(FlowDirection dir);
};


struct GraphWithChanNames {
    ChpGraph graph;
    std::unordered_map<ChanId, std::string> name_from_chan;
    std::unordered_map<VarId, std::string> name_from_var;
};
GraphWithChanNames chp_graph_from_act(act_chp_lang *lang, Scope *s);
act_chp_lang *chp_graph_to_act(GraphWithChanNames &gr,
			       std::vector<ActId *> &newnames,
			       Scope *s);


} // namespace ChpOptimize
