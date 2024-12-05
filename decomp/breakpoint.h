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

#include "analysis.h"
/*
 * Marks breakpoints on each block. There are a few rules that must
 * be followed in order for a marking to be a valid marking.
 * 1. The first block in the top-level sequence cannot be marked 
 *    with a break_before.
 * 2. The last block in the top-level sequence cannot be marked 
 *    with a break_after.
 * 3. If a sequence within a selection has any markings, then
 *    the parent selection must be marked break_before AND break_after.
 * 4. I'll probably put more in here as I deal with this thing lol
*/
class BreakPoints : public DecompAnalysis {
    public:

        BreakPoints ( FILE *fp_out, GraphWithChanNames &g_in, 
                Scope *s_in, int parallelism_level)
            : DecompAnalysis (fp_out, g_in, s_in)
            {
                parallelism = parallelism_level;
                analyze();
            }
        
        void mark_breakpoints();

    private:

        /* 
         * -P0
         * No breakpoints
         */ 

        /* 
         * -P1
         * Receives only 
         */ 
        void _mark_breakpoints_v1(Sequence seq, int root);
        /*
         * -P2
         * Selections only - for testing
        */
        void _mark_breakpoints_v2(Sequence seq, int root);

        /*
         * -P3
         * Assignments, receives and parallel
        */
        void _mark_breakpoints_v3(Sequence seq, int root);

        /*
         * -P4
         * Breakpoints at minimum live var. points
        */
        void _mark_breakpoints_v4(Sequence seq, int root);

        void _compute_min_and_max();

        int min_live_var_bw;
        int max_live_var_bw;
        int parallelism;
};
