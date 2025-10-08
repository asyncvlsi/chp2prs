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

/*
    To hold a block* and the id of the channel (will become alias ids)
    the unsigned int is the 'alias number' of the alias chan
*/
typedef std::unordered_map<Block *, std::pair<ChanId, unsigned int>> chan_blk_pair;

/*
    Program-wide data structure to hold all chan ids
    and their alias info, block * etc.
*/
typedef std::unordered_map<ChanId, chan_blk_pair> multichan_alias_struct;

/*
    Type of transition in the state table
    dead  - to be pruned during optimization
    true  - unconditional state update
    guard - conditional state update 
*/
enum class Cond { Dead, True, Guard };

/*
    Class encoding a single row in the state table
*/
class StateRow {
    public:
        int curr; // current state
        Cond c; // transition type
        std::vector<int> nexts; // list of next states
        Block *sel; // select or doloop - only if c is Cond::Guard

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
        
        /*
            Top-level processing function
        */
        void process_multichans();

        /*
            Get a vector of generated
            multi-channel handler processes 
        */
        std::vector<Sequence> get_auxiliary_procs();

    private:

        /*
            Build the channel-block-pair map
        */
        void _build_multichan_info(Sequence);

        /*
            If a channel is accessed only once,
            no need to process it - just delete
            it from the list of ones to handle
        */
        void _delete_singles();

        void _print_multichan_info();

        /*
            Update the channel-block-pair map
            with IDs of freshly generated
            alias channels
        */
        void _update_with_aliases(Sequence, ChanId);

        void _add_chan_blk_pair (Block *, ChanId);

        /*
            Before a selection, insert channel communication
            to communicate info about which branch was taken
            to the handler process 
        */
        void _insert_guard_comm (Block *, ChanId, int);
        /*
            Before the end of a do-loop, insert channel communication
            to communicate info about whether loop exits
            or continues to the handler process 
        */
        void _insert_guard_comm_loop (Block *, ChanId, int);

        /*
            Update channel accesses in the CHP with
            accesses on the new alias channels 
        */
        void _replace_with_alias (Block *, ChanId);

        /*
            Check if the sequence contains an
            access of the given channel
        */
        bool _contains_chan_access (Sequence, ChanId);
        /*
            Check if the sequence contains an
            access of the given channel, but do
            not recursively search within
        */
        bool _contains_chan_access_shallow (Sequence, ChanId);

        /*
            Construct the auxiliary multi-channel handler process
        */
        Sequence _build_aux_process_new (StateTable, ChanId);

        /*
            Build a guard expression that encodes whether
            a sending (receiving) state was reached in this
            iteartion of the handler process
        */
        IRGuard _build_send_guard (VarId, int, int);

        /*
            Build a ternary expression that encodes a 
            conditional update in the state table
        */
        Block *_build_next_assign (VarId, int, std::vector<int>, VarId, int);

        /*
            Find the send (receive) block containing the
            access of a given channel and alias number
            This is maybe inefficient 
        */
        Block *_find_alias_block (ChanId, unsigned int);

        /*
            Wrap CHP in an infinite loop
        */
        Block *_wrap_in_do_loop (Sequence);

        multichan_alias_struct mc_info;

        StateTable _st;

        /*
            Build the state-transition table that
            tracks the execution of the relevant 
            portions of the program
        */
        int _build_state_table (Sequence, ChanId, int);

        /*
            For every staterow where the current state is a
            non-receiving state, replace all its (curr) occurrences
            in the next_state vectors of all other rows with
            next_state of this row. Effectively trims 
            unconditional state updates from a non-receiving state
            to other states.
        */
        void _optimize_state_table ();

        /*
            If guard case but the guard just evaluates to true,
            we take that into account to simplify
        */ 
        void _optimize_state_table_1 ();

        /*
            If alias accesses are all relatively unconditional, 
            we only need those state rows, rest are irrelevant
        */ 
        void _optimize_state_table_2 ();

        /*
            Replace all occurrences of old_state in the 
            next state column by new_state
        */
        void _replace_next_states (int, int);

        /*
            Re-encode states so that they are the 
            first N integers
        */
        void _re_encode_states ();

        /*
            Re-encode a single state
        */
        void _re_encode_state (int, int);

        /*
            Checks this:
            All alias accesses are within the same branch, 
            i.e. are relatively unconditional
        */
        bool _is_relatively_unconditional ();

        void _print_state_table (StateTable);

        std::vector<Sequence> v_aux;

        unsigned int alias_number;
        
        int _gen_alias_number ();

        bool _seq_contains_block (Block *, Sequence);

        Block *_splice_in_block_between (Block *, Block *, Block *);

};


#endif