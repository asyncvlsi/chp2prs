/*************************************************************************
 *
 *  This file is part of the ACT library
 *
 *  Copyright (c) 2025 Karthi Srinivasan
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

/*
    Whenever possible, i.e. there are no write-write or
    read-write conflicts, convert a semi-colon separated
    set of statements into comma-separated ones.
*/

namespace ChpOptimize {

namespace {

Block *make_parallel(ChpGraph &g, Block *b1, Block *b2,
                     std::unordered_map<const Block *, UsesAndDefs> &udmap)
{
    auto pll = g.blockAllocator().newBlock(Block::makeParBlock());
    g.spliceInSequenceBefore(b1, g.blockAllocator().newSequence({pll}));

    g.spliceOutBasicBlock(b1, MarkDead::no);
    g.spliceOutBasicBlock(b2, MarkDead::no);

    pll->u_par().branches.push_back(g.blockAllocator().newSequence({b1}));
    pll->u_par().branches.push_back(g.blockAllocator().newSequence({b2}));

    auto tmp = udmap.at(b1);
    tmp |= udmap.at(b2);
    udmap.insert({pll,tmp});

    return pll;
}

void make_parallel(ChpGraph &g, Sequence &seq, 
                   std::unordered_map<const Block *, UsesAndDefs> &udmap, 
                   bool &changed)
{
    auto intersect = [](const std::unordered_set<VarId> &a, 
                        const std::unordered_set<VarId> &b) {
        std::unordered_set<VarId> ret = {};
        for ( const auto &x : a ) {
            if (b.count(x)) ret.insert(x);
        }
        return ret;
    };
    Block *curr1 = seq.startseq->child();
    while (curr1 != seq.endseq) 
    {
        Block *curr2 = curr1->child();
        while (curr2 != seq.endseq) 
        {
            Assert (udmap.count(curr1), "what1");
            auto ud1 = udmap.at(curr1);
            Assert (udmap.count(curr2), "what2");
            auto ud2 = udmap.at(curr2);
            auto ww = intersect(ud1.var_writes, ud2.var_writes);
            auto wr = intersect(ud1.var_writes, ud2.var_reads);
            auto rw = intersect(ud1.var_reads , ud2.var_writes);

            if (ww.empty() && wr.empty() && rw.empty()) {
                curr1 = make_parallel(g, curr1, curr2, udmap);
                curr2 = curr1;
            }
            else {
                break;
            }
            curr2 = curr2->child();
        }
        curr1 = curr1->child();
    }
}

bool is_empty(Sequence seq) 
{
    bool ret = true;
    Block *curr = seq.startseq->child();
    while (curr->type() != BlockType::EndSequence) {
    if (!ret) return false;
    switch (curr->type()) {
    case BlockType::Basic: {
        return false;
    }
    break;
    case BlockType::Par: {
        for (auto &branch : curr->u_par().branches) {
            ret &= is_empty (branch);
        }
    }
    break;
    case BlockType::Select: {
        for (auto &branch : curr->u_select().branches) {
            ret &= is_empty (branch.seq);
        }
    }
    break;
    case BlockType::DoLoop: {
        ret &= is_empty(curr->u_doloop().branch);
    }
    break;
    case BlockType::StartSequence:
    case BlockType::EndSequence:
        hassert(false);
    break;
    }
    curr = curr->child();
    }
    return ret;
}

void eliminate_empty_code(ChpGraph &g, Sequence &seq) 
{
    Block *curr = seq.startseq->child();
    while (curr != seq.endseq) {
        switch (curr->type()) {
        case BlockType::Basic:
            break;
        case BlockType::Par: {
            std::list<Sequence> branches = {};
            for ( auto branch : curr->u_par().branches ) {
                eliminate_empty_code(g, branch);
                if (!is_empty(branch)) branches.push_back(branch);
            }
            if (branches.empty()) 
                curr->u_par().branches = {g.blockAllocator().newSequence({})};
            else
                curr->u_par().branches = branches;
            break;
        }
        case BlockType::Select: {
            bool all_empty = true;
            for ( auto &branch : curr->u_select().branches ) {
                eliminate_empty_code(g, branch.seq);
                all_empty &= is_empty(branch.seq);
            }
            if (all_empty) {
                curr->u_select().branches.clear();
                auto tmp = curr->parent();
                g.spliceOutEmptyControlBlock(curr, MarkDead::no);
                curr = tmp;
            }
            break;
        }
        case BlockType::DoLoop: {
            eliminate_empty_code(g, curr->u_doloop().branch);
            break;
        }
        case BlockType::StartSequence:
        case BlockType::EndSequence:
            break;
        }
        curr = curr->child();
    }
}

void parallelize_statememts(ChpGraph &g, Sequence &seq, 
                            std::unordered_map<const Block *, UsesAndDefs> &ud, 
                            bool &changed) 
{
    Block *curr = seq.startseq->child();
    while (curr != seq.endseq) {
        switch (curr->type()) {
        case BlockType::Basic:
            break;
        case BlockType::Par: {
            for ( auto branch : curr->u_par().branches ) {
                parallelize_statememts(g, branch, ud, changed);
                // make_parallel(g, branch, ud, changed);
            }
            break;
        }
        case BlockType::Select: {
            for ( auto &branch : curr->u_select().branches ) {
                parallelize_statememts(g, branch.seq, ud, changed);
                make_parallel(g, branch.seq, ud, changed);
            }
            break;
        }
        case BlockType::DoLoop: {
            parallelize_statememts(g, curr->u_doloop().branch, ud, changed);
            make_parallel(g, curr->u_doloop().branch, ud, changed);
            break;
        }
        case BlockType::StartSequence:
        case BlockType::EndSequence:
            break;
        }
        curr = curr->child();
    }
}

void fill_in_else_explicit (ChpGraph &g, Sequence seq)
{
    Block *curr = seq.startseq->child();
    while (curr != seq.endseq) {
        switch (curr->type()) {
        case BlockType::Basic:
            break;
        case BlockType::Par: {
            for ( auto branch : curr->u_par().branches ) {
                fill_in_else_explicit(g, branch);
            }
            break;
        }
        case BlockType::Select: {
            for ( auto &branch : curr->u_select().branches ) {
                fill_in_else_explicit(g, branch.seq);
            }
            ChpExprSingleRootDag disj_gs = ChpExprSingleRootDag::makeConstant(BigInt(0),1);

            for ( auto &branch : curr->u_select().branches ) {
                if (branch.g.type()==IRGuardType::Expression) {
                    disj_gs = ChpExprSingleRootDag::makeBinaryOp(IRBinaryOpType::Or,
                        std::move(std::make_unique<ChpExprSingleRootDag>(ChpExprSingleRootDag::deep_copy(disj_gs))), 
                        std::move(std::make_unique<ChpExprSingleRootDag>(ChpExprSingleRootDag::deep_copy(branch.g.u_e().e))));
                }
                else {
                    disj_gs = ChpExprSingleRootDag::makeUnaryOp(IRUnaryOpType::Not,
                        std::move(std::make_unique<ChpExprSingleRootDag>(ChpExprSingleRootDag::deep_copy(disj_gs))));
                    branch.g = IRGuard::makeExpression(std::move(disj_gs));
                }
            }
            break;
        }
        case BlockType::DoLoop: {
            fill_in_else_explicit(g, curr->u_doloop().branch);
            break;
        }
        case BlockType::StartSequence:
        case BlockType::EndSequence:
            break;
        }
        curr = curr->child();
    }
}

} // namespace

bool parallelizeStatements(ChpGraph &graph) {
    bool changed = false;
    auto ud = getDefUsesTable(graph);
    parallelize_statememts(graph, graph.m_seq, ud, changed);
    graph.validateGraphInvariants();

    return changed;
}

bool fillInElseExplicit(ChpGraph &graph) {
    fill_in_else_explicit(graph, graph.m_seq);
    return true;
}

bool eliminateEmptyCode(ChpGraph &graph) {
    eliminate_empty_code(graph, graph.m_seq);
    return true;
}

} // namespace ChpOptimize
