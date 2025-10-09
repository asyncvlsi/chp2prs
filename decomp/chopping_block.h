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

#ifndef __CHOPPING_BLOCK_H__
#define __CHOPPING_BLOCK_H__

#include "analysis.h"

/*
    Class for implementing the breaking of 
    a single program into multiple parallel programs.
    1. Helper functions
    2. Loop excision
    3. Live-variable based decomposition (deprecated)
*/
class ChoppingBlock {
    public:

        ChoppingBlock ( FILE *fp_in, GraphWithChanNames &g_in, 
                        std::unordered_map<const Block *, decomp_info_t *> vmap_in,
                        Scope *s_in)
            {
                fp = fp_in;
                g = &g_in;
                vmap = vmap_in;
                idpool = g->graph.id_pool();
                s = s_in;
            }

        /*
            Break the graph based on breakpoints
        */
        void chop_graph();

        /*
            Convert nested loops into multiple parallel loops
        */
        void excise_internal_loops();

        /*
            Print decomposed programs
        */
        void print_chopped_seqs();

        /*
            Returns vector of decomposed programs 
        */
        std::vector<Sequence> get_chopped_seqs();

    protected:

        FILE *fp;
        GraphWithChanNames *g;
        std::unordered_map<const Block *, decomp_info_t *> vmap;
        std::vector<Sequence> v_seqs;
        Scope *s;
        IdPool idpool;

        void _chop_graph (Sequence seq, int root);

        /*
            Replace loop-carried dependencies with sends and 
            receives at the start and end of a loop 

        */
        void _handle_ic_lcd (Sequence seq);
        
        /*
            Helper function to handle loop-carries
        */
        void _handle_ic_lcd_helper (Block *doloop);
        
        /*
            Recursively extract loops
        */
        void _excise_internal_loops (Sequence seq, int root);
        
        /*
            Splice a block out from its sequence.
            Returns the succeeding block.
        */
        Block *_splice_out_block (Block *bb);

        /*
            Splice in a block in between two blocks.
            The first two must be connected in order.
        */
        Block *_splice_in_block_between (Block *before, Block *after, Block *bb);

        /*
            Generate communication (send) of concatenation of 
            all variables that are live-in to this block.
        */
        Block *_generate_send_to_be_recvd_by (Block *bb);

        /*
            Generate communication (send) of concatenation of 
            all variables that are live-out of this block.
        */
        Block *_generate_send_to_be_sent_from (Block *bb);

        /*
            Find next break_after point in current sequence.
        */
        Block *_find_next_break_after (Block *b);

        /*
            Find next break_before point in current sequence.
        */
        Block *_find_next_break_before (Block *b);

        /*
            Splice out blocks from b_start to b_end.
            b_start included, b_end excluded.
            Both must be part of the same sequence.
        */
        std::vector<Block *> _split_sequence_from_to(Block *b_start, Block *b_end);

        /*
            Splice out blocks from b_start to b_end.
            b_start and b_end included.
            Both must be part of the same sequence.
        */
        std::vector<Block *> _split_sequence_from_to_new(Block *b_start, Block *b);

        /*
            Splice out a set of blocks, emplace a send
            at the end and construct a process from it.
            Also return the tail-emplaced send
        */
        Block *_build_sequence(Block *b_start, Block *b_end, int type);

        /*
            Return vector of assignment blocks that initialized 
            variables before the start of an excised loop.
        */
        std::vector<Block *> _initialize_ics(Block *curr);

        /*
            Given a send block of a concatenation of variables,
            generate one of the following, categorized by the 
            integer in the return type:
            0. An empty sequence if the send is empty
            1. A simple receive if the send is a single variable
            2. Receive + parallel bitfield assignments if the 
               send is a concatenation of several variables
        */
        std::pair<int, Sequence> _generate_recv_and_maybe_assigns (Block *send, int type);

        /*
            Generate the corresponding receive and place it in
            the right place, for a given send
        */
        int _splice_in_recv_before (Block *bb, Block *send, int type);

        /*
            Handle extraction of a selection block
        */
        Block *_process_selection (Block *sel, int n);

        /*
            Handle extraction of a parallel block
        */
        Block *_process_parallel (Block *sel, int n);

        /*
            Generate blocks for the control transmission, live-var
            transmissions and merges after exiting a selection
            *[C?live_in; { live_1 := live_in{0..i}, live_2 := live_in{i+1..j} ... };
                [ G1 -> Ctrl!1 , Co1!live_out_1
                []G2 -> Ctrl!2 , Co2!live_out_2
                ..
                []Gn -> Ctrl!n , Con!live_out_n
                ]
            ]
        */
        std::tuple<Block *, Block *, Block *> _generate_split_merge_and_seed_branches (Block *sel);

        /*
            Generate blocks for the live-var transmissions
            for the start and end of a parallel block 
        */
        std::pair<Block *, Block *> _generate_pll_send_recv_and_seed_branches (Block *pll);

        /*
            Core loop excision logic. Essentially converts this:
            *[..; *[G1->S1[]G2->S2..[]Gn->Sn]; ..]
            into this:
            *[..;Ls!\{x1,x2,..,xn\},Lf?\{y1,y2,..,ym\};..] ||
            c:=0;
            *[ [c=0->Ls?\{x1',x2',..,xn'\},c:=1
            []c=1->skip];
            [G1'->S1'[]G2'->S2'..[]Gn'->Sn'
            []else->Lf!\{y1',y2',..,ym'\},c:=0] ]
        */
        Block *_excise_loop (Block *curr);

        /*
            construct this:
            x:=0;
            init_vars:=0;
            *[  [ c=0 -> Ls?{vars};{assign all from concat};c:=1 (line 1)
                []c=1 -> skip (line 2)
                ];
                [ L
                []else -> Lf!{vars};c:=0 (line 3)
                ]  ]
            
            where L is the original loop (turned into selection) and an else branch is added
        */
        Sequence _construct_sm_loop (Block *, std::vector<Block *>, Block *);

        /*
            Wrap a sequence in an infinite loop
        */
        Sequence _wrap_in_do_loop (Sequence seq);

        void _print_seq (Sequence seq);
};

#endif