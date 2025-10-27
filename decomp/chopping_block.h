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

#include <act/chp/analysis.h>
#include <act/chp/utils.h>

/*
    Class with:
    1. Several helper functions for manipulating the ChpGraph IR.
    2. Code to perform loop excision (nested loops -> multiple parallel loops)
*/
class ChoppingBlock {
    public:

        ChoppingBlock (GraphWithChanNames &g_in, Scope *s_in)
        {
            g = &g_in;
            s = s_in;
            idpool = g->graph.id_pool();
            DecompAnalysis dca = DecompAnalysis (*g, s);
            dca.analyze();
            vmap = dca.get_decomp_info_map();
        }

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

        GraphWithChanNames *g;
        std::unordered_map<const Block *, decomp_info_t *> vmap;
        std::vector<Sequence> v_seqs;
        Scope *s;
        IdPool idpool;
        
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