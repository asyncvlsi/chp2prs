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


#ifndef __MULTICHAN_H__
#define __MULTICHAN_H__

#include "analysis.h"

using namespace ChpOptimize;

// To hold a block* and the id of the channel (will become alias ids)
// the unsigned int is the 'alias number' of the alias chan
typedef std::unordered_map<Block *, std::pair<ChanId, unsigned int>> chan_blk_pair;

// program-wide data structure to hold all chan ids
// and their alias info, block * etc.
typedef std::unordered_map<ChanId, chan_blk_pair> multichan_alias_struct;

// next alias map
// if vec longer than 1, it's the 
typedef std::unordered_map<OptionalChanId, std::vector<OptionalChanId>> next_alias_set; 

/*
    Handler for Multiple Channel Access.
    Replaces channel accesses with accessess
    of fresh alias channels. Generates auxiliary
    state-machine process to correctly merge aliases etc. 
*/
class MultiChan : public DecompAnalysis {
    public:

        MultiChan ( FILE *fp_out, GraphWithChanNames &g_in, 
                Scope *s_in)
            : DecompAnalysis (fp_out, g_in, s_in)
            {
                analyze();
            }

        void process_multichans();

        std::vector<Sequence> get_auxiliary_procs();

    private:

        void _build_multichan_info(Sequence);

        void _delete_singles();

        void _print_multichan_info();

        void _update_with_aliases(Sequence, ChanId);

        void _add_chan_blk_pair (Block *, ChanId);

        void _insert_guard_comm (Block *);

        void _replace_with_alias (Block *, ChanId);

        bool _contains_chan_access (Sequence, ChanId);

        Sequence _build_aux_process (Sequence, ChanId);

        Block *_compute_first_alias_block (Sequence, ChanId, int);

        std::pair<Block *, std::pair<ChanId, unsigned int>>
             _compute_next_alias (Block *);

        Block *_wrap_in_do_loop (Sequence);

        multichan_alias_struct mc_info;

        std::vector<Sequence> v_aux;

        unsigned int alias_number;

};


#endif