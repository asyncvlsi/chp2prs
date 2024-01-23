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

#ifndef __DECOMP_ANALYSIS_H__
#define __DECOMP_ANALYSIS_H__

#include <act/act.h>
#include "../opt/chp-opt.h"
#include "../opt/act-names.h"

// ChpOptimize::ChpGraph *g;
using namespace ChpOptimize;

typedef struct decomp_info {
    std::unordered_set<VarId> tx_vars;
    int total_bitwidth;
    bool is_breakpoint;
} decomp_info_t;

class DecompAnalysis {
    public:

        DecompAnalysis ( FILE *fp_out, GraphWithChanNames &g_in, Scope *s_in)
            {   
                fp = fp_out;
                g = &g_in;
                s = s_in;
            } 

        void analyze ();

        void print_decomp_info ();

        std::unordered_map<Block *, decomp_info_t *> get_live_vars_map ();
    
    protected: 

        FILE *fp;
        GraphWithChanNames *g;
        Scope *s;

        // map from a block to variables that are live-in to that block
        std::unordered_map<Block *, decomp_info_t *> live_in_vars_map;

        // running state of live variables
        std::unordered_set<VarId> H_live;

        // copy of running state
        std::unordered_set<VarId> H_saved;

        // stack of parent states - used when descending down into selections
        std::vector<std::unordered_set<VarId>> H_parents;

        unsigned int total_bits;

        // traverse the graph and generate the live-in var map
        void _generate_decomp_info (Sequence seq, int root);

        void _map_block_to_live_vars (Block *, decomp_info_t *);

        void _add_to_live_vars (VarId vid);
        void _add_to_live_vars (std::unordered_set<VarId> vids);
        void _remove_from_live_vars (VarId vid);

        // compute total bitwidth of set of vars
        int _compute_total_bits (std::unordered_set<VarId> vars);

        // return a decomp_info_t based on the current state of H_live
        decomp_info_t *_generate_decomp_info ();
        decomp_info_t *_generate_decomp_info (std::unordered_set<VarId> H);

        // print decomp_info_t's for the whole graph
        void _print_decomp_info (Sequence seq,  int root);

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
        std::unordered_set<VarId> _set_union (std::unordered_set<VarId>, std::unordered_set<VarId>);

        std::unordered_set<VarId> _prune_T (std::unordered_set<VarId>, std::vector<std::unordered_set<VarId>>);

        // add the vars from top element of stack to H_live
        void _restore_live_vars_from_parent (); //_restore_live_vars_from_parent
      
};

#endif