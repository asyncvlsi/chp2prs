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

enum class Cond { Dead, True, Guard };

class StateRow {
    public:
        int curr;
        Cond c;
        std::vector<int> nexts;
        Block *sel; // select or doloop

        StateRow (int curr_st, int next_st)
        {
            curr = curr_st;
            c = Cond::True;
            nexts.clear();
            nexts.push_back(next_st);
            sel = NULL;
        }
        StateRow (int curr_st, std::vector<int> next_sts, Block *sel_blk)
        {
            Assert ((sel_blk->type() == BlockType::Select || sel_blk->type() == BlockType::DoLoop), "hmm");
            curr = curr_st;
            c = Cond::Guard;
            nexts = next_sts;
            sel = sel_blk;
        }
};

typedef std::vector<StateRow> StateTable;

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

        void _insert_guard_comm (Block *, ChanId, int);
        void _insert_guard_comm_loop (Block *, ChanId, int);

        void _replace_with_alias (Block *, ChanId);

        bool _contains_chan_access (Sequence, ChanId);
        bool _contains_chan_access_shallow (Sequence, ChanId);

        Sequence _build_aux_process_new (StateTable, ChanId);

        IRGuard _build_send_guard (VarId, int, int);
        Block *_build_next_assign (VarId, int, std::vector<int>, VarId, int);

        Block *_find_alias_block (ChanId, unsigned int);

        Block *_compute_next_alias_block (Sequence, ChanId, int);

        Block *_wrap_in_do_loop (Sequence);

        multichan_alias_struct mc_info;

        StateTable _st;

        int _build_state_table (Sequence, ChanId, int);

        void _optimize_state_table ();
        void _optimize_state_table_1 ();
        void _optimize_state_table_2 ();
        void _replace_next_states (int, int);
        void _re_encode_states ();
        void _re_encode_state (int, int);

        bool _is_relatively_unconditional ();

        void _print_state_table (StateTable);

        std::vector<Sequence> v_aux;

        unsigned int alias_number;
        
        int _gen_alias_number ();

        bool _seq_contains_block (Block *, Sequence);

        Block *_splice_in_block_between (Block *, Block *, Block *);

};


#endif