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
    switch (parallelism) {
    case 0:
        break;
    case 1:
        _mark_breakpoints_v1 (g->graph.m_seq, 0);
        break;
    case 2:
        _mark_breakpoints_v2 (g->graph.m_seq, 0);
        break;
    case 3:
        _mark_breakpoints_v3 (g->graph.m_seq, 0);
        break;
    case 4:
        _compute_min_and_max();
        _mark_breakpoints_v4 (g->graph.m_seq, 0);
        break;
    default:
        break;
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
            break;
        case StatementType::Receive:
            // break before every new assignment
            di = (decomp_info_map.find(curr))->second;
            di->break_before = true;
            di->break_after = true;
            break;
      }
        di = (decomp_info_map.find(curr))->second;
        // _print_decomp_info (di);
    }
    break;
      
    case BlockType::Par: {
        // fatal_error ("working on par...");
        for (auto &branch : curr->u_par().branches) {
            _mark_breakpoints_v1 (branch, 0);
        }
    }
    break;
      
    case BlockType::Select:
        // fprintf (stdout, "reached select start\n");
        // break before every selection
        di = (decomp_info_map.find(curr))->second;
        di->break_before = true;
        di->break_after = true;

        for (auto &branch : curr->u_select().branches) {
            _mark_breakpoints_v1 (branch.seq, 0);
        }
        // fprintf (stdout, "reached select end\n");
    break;
      
    case BlockType::DoLoop:
        // fprintf (stdout, "\n\nreached do-loop\n");
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

void BreakPoints::_mark_breakpoints_v2(Sequence seq, int mark_next)
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
            _mark_breakpoints_v2 (branch, 0);
        }
    }
    break;
      
    case BlockType::Select:
        // break out every selection
        di = (decomp_info_map.find(curr))->second;
        di->break_before = true;
        di->break_after = true;

        for (auto &branch : curr->u_select().branches) {
            _mark_breakpoints_v2 (branch.seq, 0);
        }
    break;
      
    case BlockType::DoLoop:
        _mark_breakpoints_v2 (curr->u_doloop().branch, 0);
        break;
    
    case BlockType::StartSequence:
    case BlockType::EndSequence:
        hassert(false);
        break;
    }
    curr = curr->child();
    }
}

void BreakPoints::_mark_breakpoints_v3(Sequence seq, int mark_next)
{
    Block *curr = seq.startseq->child();
    decomp_info *di;

    while (curr->type() != BlockType::EndSequence) {
    switch (curr->type()) {
    case BlockType::Basic: {
        switch (curr->u_basic().stmt.type()) {
        case StatementType::Send:
            break;
        case StatementType::Assign:
        case StatementType::Receive:
            di = (decomp_info_map.find(curr))->second;
                di->break_before = true;
                di->break_after = true;
            break;
        }
    }
    break;
      
    case BlockType::Par: {
        di = (decomp_info_map.find(curr))->second;
        if (curr->parent() != seq.startseq)
        {
            di->break_before = true;
            di->break_after = true;
        }
        for (auto &branch : curr->u_par().branches) {
            _mark_breakpoints_v3 (branch, 0);
        }
    }
    break;
      
    case BlockType::Select: {
        // di = (decomp_info_map.find(curr))->second;
        //     di->break_before = true;
        //     di->break_after = true;
        for (auto &branch : curr->u_select().branches) {
            _mark_breakpoints_v3 (branch.seq, 0);
        }
    }
    break;
      
    case BlockType::DoLoop:
        _mark_breakpoints_v3 (curr->u_doloop().branch, 0);
        break;
    
    case BlockType::StartSequence:
    case BlockType::EndSequence:
        hassert(false);
        break;
    }
    curr = curr->child();
    }
    // di = (decomp_info_map.find(curr))->second;
}

void BreakPoints::_compute_min_and_max()
{
    min_live_var_bw = 0;
    max_live_var_bw = INT32_MAX;

    for (auto dim_itr : decomp_info_map)
    {
        auto di = dim_itr.second;
        if (min_live_var_bw > di->total_bitwidth_in) min_live_var_bw = di->total_bitwidth_in;
        if (max_live_var_bw < di->total_bitwidth_in) max_live_var_bw = di->total_bitwidth_in;
    }
}

void BreakPoints::_mark_breakpoints_v4(Sequence seq, int mark_next)
{
    // heuristic
    int testval = 0.5*(min_live_var_bw+max_live_var_bw);

    Block *curr = seq.startseq->child();
    decomp_info *di;

    while (curr->type() != BlockType::EndSequence) {
    switch (curr->type()) {
    case BlockType::Basic: {
        switch (curr->u_basic().stmt.type()) {
        case StatementType::Send:
        case StatementType::Assign:
        case StatementType::Receive:
            // break before every new assignment
            di = (decomp_info_map.find(curr))->second;
            if (di->total_bitwidth_in > testval)
            {
                di->break_before = true;
            }
            if (di->total_bitwidth_out > testval)
            {
                di->break_after = true;
            }
            break;
      }
        // di = (decomp_info_map.find(curr))->second;
        // _print_decomp_info (di);
    }
    break;
      
    case BlockType::Par: {
        // fatal_error ("working on par...");
        for (auto &branch : curr->u_par().branches) {
            _mark_breakpoints_v4 (branch, 0);
        }
    }
    break;
      
    case BlockType::Select:
        // fprintf (stdout, "reached select start\n");
        // break before every selection
        di = (decomp_info_map.find(curr))->second;
        if (di->total_bitwidth_in > testval)
        {
            di->break_before = true;
            di->break_after = true;
        }
        for (auto &branch : curr->u_select().branches) {
            _mark_breakpoints_v4 (branch.seq, 0);
        }
        // fprintf (stdout, "reached select end\n");
    break;
      
    case BlockType::DoLoop:
        // fprintf (stdout, "\n\nreached do-loop\n");
        _mark_breakpoints_v4 (curr->u_doloop().branch, 0);
        break;
    
    case BlockType::StartSequence:
    case BlockType::EndSequence:
        hassert(false);
        break;
    }
    curr = curr->child();
    }

}

