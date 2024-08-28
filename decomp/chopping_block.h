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

        void chop_graph();
        void excise_internal_loops();
        void print_chopped_seqs();
        std::vector<Sequence> get_chopped_seqs();

    private:

        FILE *fp;
        GraphWithChanNames *g;
        std::unordered_map<const Block *, decomp_info_t *> vmap;
        std::vector<Sequence> v_seqs;
        Scope *s;
        IdPool idpool;

        void _chop_graph (Sequence seq, int root);

        void _handle_ic_lcd (Sequence seq);
        
        void _handle_ic_lcd_helper (Block *doloop);
        
        void _excise_internal_loops (Sequence seq, int root);
        
        void _split_sequence_before (Block *b, Sequence seq_in, int root);
        
        Block *_splice_out_block (Block *bb);

        Block *_splice_in_block_between (Block *before, Block *after, Block *bb);

        Block *_generate_send_to_be_recvd_by (Block *bb);

        Block *_generate_send_to_be_sent_from (Block *bb);

        Block *_find_next_break_after (Block *b);

        Block *_find_next_break_before (Block *b);

        std::vector<Block *> _split_sequence_from_to(Block *b_start, Block *b_end);

        std::vector<Block *> _split_sequence_from_to_new(Block *b_start, Block *b);

        Block *_build_sequence(Block *b_start, Block *b_end, int type);

        std::vector<Block *> _initialize_ics(Block *curr);

        std::pair<int, Sequence> _generate_recv_and_maybe_assigns (Block *send, int type);

        int _splice_in_recv_before (Block *bb, Block *send, int type);

        Block *_process_selection (Block *sel, int n);

        Block *_process_parallel (Block *sel, int n);

        std::tuple<Block *, Block *, Block *> _generate_split_merge_and_seed_branches (Block *sel);

        std::pair<Block *, Block *> _generate_pll_send_recv_and_seed_branches (Block *pll);

        Block *_excise_loop (Block *curr);

        Sequence _construct_sm_loop (Block *, std::vector<Block *>, Block *);

        Sequence _wrap_in_do_loop (Sequence seq);

        void _print_seq (Sequence seq);
};
