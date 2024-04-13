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
// typedef std::pair<ChanId, Block *> chan_blk_pair;
typedef std::unordered_map<Block *, ChanId> chan_blk_pair;

// program-wide data structure to hold all chan ids
// and their alias info, block * etc.
typedef std::unordered_map<ChanId, chan_blk_pair> multichan_alias_struct;

// // possibilities for how to compute next alias
// enum class NextAliasType { Direct, Branch, Unknown };

// next alias map
typedef std::unordered_map<ChanId, OptionalChanId> next_alias_set; 

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

        void get_auxiliary_procs();

    private:

        void _build_multichan_info(Sequence);

        void _delete_singles();

        void _print_multichan_info();

        void _update_with_aliases(Sequence, ChanId);

        void _add_chan_blk_pair (Block *, ChanId);

        void _insert_guard_comm (Block *);

        void _replace_with_alias (Block *);

        void _compute_next_aliases (Sequence, ChanId);

        bool _contains_chan_access (Sequence, ChanId);

        multichan_alias_struct mc_info;

        next_alias_set nas;

};


#endif