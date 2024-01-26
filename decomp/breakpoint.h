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

class BreakPoints : public DecompAnalysis {
    public:

        BreakPoints ( FILE *fp_out, GraphWithChanNames &g_in, 
                Scope *s_in)
            : DecompAnalysis (fp_out, g_in, s_in)
            {
                analyze();
            }
        
        void mark_breakpoints();

    private:

        /* 
         * Naive breakpoint computation algorithm. Breaks at
         * every receive/assignment. Also breaks just before
         * and just after every selection.
         */ 
        void _mark_breakpoints_v0(Sequence seq, int root);
        /*
         * Breaks out selections only - for testing
        */
        void _mark_breakpoints_v1(Sequence seq, int root);

        /*
         * Receives only, ignores selections - for testing
        */
        void _mark_breakpoints_v2(Sequence seq, int root);
};
