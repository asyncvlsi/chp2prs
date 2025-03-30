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
    Block *curr1 = seq.startseq->child();
    while (curr1 != seq.endseq) 
    {
        Assert (udmap.contains(curr1), "what1");
        auto ud1 = udmap.at(curr1);
        Block *curr2 = curr1->child();
        while (curr2 != seq.endseq) 
        {
            Assert (udmap.contains(curr2), "what2");
            auto ud2 = udmap.at(curr2);
            std::unordered_set<VarId> ww = {};
            std::unordered_set<VarId> wr = {};
            std::unordered_set<VarId> rw = {};
            std::set_intersection(ud1.var_writes.begin(), ud1.var_writes.end(),
                                  ud2.var_writes.begin(), ud2.var_writes.end(),
                                  std::inserter(ww, ww.begin()));

            std::set_intersection(ud1.var_writes.begin(), ud1.var_writes.end(),
                                  ud2.var_reads.begin() , ud2.var_reads.end(),
                                  std::inserter(wr, wr.begin()));

            std::set_intersection(ud2.var_writes.begin(), ud2.var_writes.end(),
                                  ud1.var_reads.begin() , ud1.var_reads.end(),
                                  std::inserter(rw, rw.begin()));

            if (ww.empty() && wr.empty() && rw.empty()) {
                curr1 = make_parallel(g, curr1, curr2, udmap);
                curr2 = curr1;
                ud1 = udmap.at(curr1);
            }
            else {
                break;
            }
            curr2 = curr2->child();
        }
        curr1 = curr1->child();
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
                make_parallel(g, branch, ud, changed);
            }
            break;
        }
        case BlockType::Select: {
            for ( auto &branch : curr->u_select().branches ) {
                make_parallel(g, branch.seq, ud, changed);
            }
            break;
        }
        case BlockType::DoLoop: {
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

} // namespace

bool parallelizeStatements(ChpGraph &graph) {
    bool changed = false;
    auto ud = getDefUsesTable(graph);
    parallelize_statememts(graph, graph.m_seq, ud, changed);
    graph.validateGraphInvariants();

    return changed;
}

} // namespace ChpOptimize
