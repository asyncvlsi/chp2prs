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
                        std::unordered_map<Block *, decomp_info_t *> vmap_in)
            {
                fp = fp_in;
                g = &g_in;
                vmap = vmap_in;
                idpool = g->graph.id_pool();
            }

        void chop_graph();
        void print_chopped_seqs();

    private:

        FILE *fp;
        GraphWithChanNames *g;
        std::unordered_map<Block *, decomp_info_t *> vmap;
        std::vector<Sequence> v_seqs;
        IdPool idpool;

        void _chop_graph(Sequence seq, int root);
        
        Sequence _split_sequence_before(Block *b, Sequence seq_in);
        
        Block *_splice_out_block(Block *bb);

        Sequence _wrap_in_do_loop (Sequence seq);

        void _print_seq (Sequence seq);
         

};