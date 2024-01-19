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
}

void BreakPoints::_mark_breakpoints_v0(Sequence seq, int mark_next)
{
    Block *curr = seq.startseq->child();
    decomp_info *di;

    while (curr->type() != BlockType::EndSequence) {
    // break after every selection
    if (curr->parent() && 
        curr->parent()->type() == BlockType::Select)
    {
        di = (live_in_vars_map.find(curr))->second;
        di->is_breakpoint = true;
    }
    switch (curr->type()) {
    case BlockType::Basic: {
        switch (curr->u_basic().stmt.type()) {
        case StatementType::Send:
            break;
        case StatementType::Assign:
        case StatementType::Receive:
            // break before every new assignment
            di = (live_in_vars_map.find(curr))->second;
            di->is_breakpoint = true;
            break;
      }
        di = (live_in_vars_map.find(curr))->second;
        // _print_decomp_info (di);
    }
    break;
      
    case BlockType::Par: {
        fatal_error ("working on par...");
        for (auto &branch : curr->u_par().branches) {
        }
    }
    break;
      
    case BlockType::Select:
        // fprintf (stdout, "reached select start\n");
        // break before every selection
        di = (live_in_vars_map.find(curr))->second;
        di->is_breakpoint = true;
        // _print_decomp_info (di);

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
