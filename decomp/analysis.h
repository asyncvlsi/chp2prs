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

#include <act/act.h>
#include "../opt/chp-opt.h"

// ChpOptimize::ChpGraph *g;

typedef struct decomp_info {
    std::unordered_set<ChpOptimize::VarId *> tx_vars;
    int total_bitwidth;
    bool is_breakpoint;
} decomp_info_t;

class DecompAnalysis {
    public:

        DecompAnalysis ( FILE *fp_out, ChpOptimize::GraphWithChanNames g_in )
            {   
                fp = fp_out;
                g = g_in;
            }

        void analyze ();

        void print_decomp_info ();
    
    private: 

        FILE *fp;
        ChpOptimize::GraphWithChanNames g;

        // map from a block to variables that are live-in to that block
        std::unordered_map<ChpOptimize::Block *, decomp_info_t *> live_in_vars_map;

        // running state of live variables
        std::unordered_set<ChpOptimize::VarId *> H_live;

        // copy of running state
        std::unordered_set<ChpOptimize::VarId *> H_saved;
        
        // stack of parent states - used when descending down into selections
        list_t *H_parents;

        unsigned int total_bits;

        // traverse the graph and generate the live-in var map
        void _generate_decomp_info ( int root);

        // compute total bitwidth of set of vars
        int _compute_total_bits (std::unordered_set<ChpOptimize::VarId> vars);

        // return a decomp_info_t based on the current state of H_live
        decomp_info_t *_generate_decomp_info ();

        // print decomp_info_t's for the whole graph
        void _print_decomp_info ( int root);

        // print a decomp_info_t object
        void _print_decomp_info (decomp_info *di);

        // save the state of H_live into H_saved
        void _save_state_live_vars ();

        // restore the state of H_live from H_saved
        void _restore_state_live_vars ();

        // push current H_live into H_parents stack
        void _init_union ();
        
        // pop H_parents stack
        void _free_union ();
        
        // compute union of top element of stack and current H_live
        void _h_live_union_h_parent ();

        // add the vars from top element of stack to H_live
        void _update_live_vars_from_parent (); //_restore_live_vars_from_parent
      
};
