/*************************************************************************
 *
 *  Copyright (c) 2024 Karthi Srinivasan
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

#include "breakpoint.h"

void BreakPoints::mark_breakpoints()
{
    _mark_breakpoints_v0 (g->graph.m_seq, 0);
    // _mark_breakpoints_v1 (g->graph.m_seq, 0);
}

void BreakPoints::_mark_breakpoints_v0(Sequence seq, int mark_next)
{
    Block *curr = seq.startseq->child();
    decomp_info *di;

    while (curr->type() != BlockType::EndSequence) {
    switch (curr->type()) {
    case BlockType::Basic: {
        switch (curr->u_basic().stmt.type()) {
        case StatementType::Send:
        case StatementType::Assign:
            break;
        case StatementType::Receive:
            // break before every new assignment
            di = (live_in_vars_map.find(curr))->second;
            di->break_before = true;
            di->break_after = true;
            break;
      }
        di = (live_in_vars_map.find(curr))->second;
        // _print_decomp_info (di);
    }
    break;
      
    case BlockType::Par: {
        // fatal_error ("working on par...");
        for (auto &branch : curr->u_par().branches) {
            _mark_breakpoints_v0 (branch, 0);
        }
    }
    break;
      
    case BlockType::Select:
        // fprintf (stdout, "reached select start\n");
        // break before every selection
        di = (live_in_vars_map.find(curr))->second;
        di->break_before = true;
        di->break_after = true;
        
        for (auto &branch : curr->u_select().branches) {
            _mark_breakpoints_v0 (branch.seq, 0);
        }
        // fprintf (stdout, "reached select end\n");
    break;
      
    case BlockType::DoLoop:
        // fprintf (stdout, "\n\nreached do-loop\n");
        _mark_breakpoints_v0 (curr->u_doloop().branch, 0);
        break;
    
    case BlockType::StartSequence:
    case BlockType::EndSequence:
        hassert(false);
        break;
    }
    curr = curr->child();
    }

}

void BreakPoints::_mark_breakpoints_v1(Sequence seq, int mark_next)
{
    Block *curr = seq.startseq->child();
    decomp_info *di;

    while (curr->type() != BlockType::EndSequence) {
    switch (curr->type()) {
    case BlockType::Basic: {
        switch (curr->u_basic().stmt.type()) {
        case StatementType::Send:
        case StatementType::Assign:
        case StatementType::Receive:
            break;
      }
    }
    break;
      
    case BlockType::Par: {
        for (auto &branch : curr->u_par().branches) {
            _mark_breakpoints_v1 (branch, 0);
        }
    }
    break;
      
    case BlockType::Select:
        // break out every selection
        di = (live_in_vars_map.find(curr))->second;
        di->break_before = true;
        di->break_after = true;

        for (auto &branch : curr->u_select().branches) {
            _mark_breakpoints_v1 (branch.seq, 0);
        }
    break;
      
    case BlockType::DoLoop:
        _mark_breakpoints_v1 (curr->u_doloop().branch, 0);
        break;
    
    case BlockType::StartSequence:
    case BlockType::EndSequence:
        hassert(false);
        break;
    }
    curr = curr->child();
    }
}
