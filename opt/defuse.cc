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

void updateDefUsesTable (Sequence seq,
			 std::unordered_map<const Block *, UsesAndDefs> &table)
{
  Block *curr = seq.startseq;
  if (table.contains (curr)) return;
  curr = curr->child();
  while (curr->type() != BlockType::EndSequence) {
    table[seq.startseq] |= table[curr];
    switch (curr->type()) {
    case BlockType::Basic:
      break;

    case BlockType::Par:
      for (auto &br : curr->u_par().branches) {
	updateDefUsesTable (br, table);
      }
      break;
      
    case BlockType::Select:
      for (auto &br : curr->u_select().branches) {
	updateDefUsesTable (br.seq, table);
      }
      break;
      
    case BlockType::DoLoop:
      updateDefUsesTable (curr->u_doloop().branch, table);
      break;

    case BlockType::StartSequence:
    case BlockType::EndSequence:
      hassert(false);
      break;
    }
    curr = curr->child();
  }
}
    
}   // anonymous namespace

std::unordered_map<const Block *, UsesAndDefs>
getDefUsesTable(const ChpGraph &graph) {
    std::unordered_map<const Block *, UsesAndDefs> result;
    buildVarUsageTable(graph.m_seq, result);
    updateDefUsesTable (graph.m_seq, result);
    return result;
}

} // namespace ChpOptimize
