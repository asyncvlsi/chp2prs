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


#ifndef __PROJECTION_H__
#define __PROJECTION_H__

#include "chopping_block.h"

#include "../opt/static-tokens.h"
#include "../opt/union-find.h"

enum class NodeType { Basic, LoopInPhi, LoopOutPhi, LoopLoopPhi, SelPhi, SelPhiInv };

class DFG_Node {
    public:
        NodeType t;
        Block *b;
        Block::Variant_DoLoop::InPhi    lip;
        Block::Variant_DoLoop::OutPhi   lop;
        Block::Variant_DoLoop::LoopPhi  llp;
        Block::Variant_Select::PhiSplit pi;
        Block::Variant_Select::PhiMerge p;
        int id;
        int set_n;

        DFG_Node (Block *_b, int idx) 
        {
            t = NodeType::Basic;
            b = _b;
            id = idx;
            set_n = -1;
        }
        DFG_Node (Block *_b, Block::Variant_DoLoop::InPhi x, int idx) 
        {
            t = NodeType::LoopInPhi;
            b = _b;
            lip = x;
            id = idx;
            set_n = -1;
        }
        DFG_Node (Block *_b, Block::Variant_DoLoop::OutPhi x, int idx) 
        {
            t = NodeType::LoopOutPhi;
            b = _b;
            lop = x;
            id = idx;
            set_n = -1;
        }
        DFG_Node (Block *_b, Block::Variant_DoLoop::LoopPhi x, int idx) 
        {
            t = NodeType::LoopLoopPhi;
            b = _b;
            llp = x;
            id = idx;
            set_n = -1;
        }
};

class DFG {
    public:
        std::vector<DFG_Node *> nodes;
        std::vector<std::vector<DFG_Node *>> adj;
        int id;

        DFG () {
            nodes.clear();
            adj.clear();
            id = 0;
        }

        int gen_id () {
            id++;
            return id;
        }

        void add_node (DFG_Node *n) {
            nodes.push_back(n);
            adj.push_back({});
        }

        void add_edge (DFG_Node *from, DFG_Node *to) {
            int f_idx = -1, t_idx = -1;
            for (int i=0; i<nodes.size(); i++) {
                if (nodes[i] == from) {
                    f_idx = i;
                }
                if (nodes[i] == to) {
                    t_idx = i;
                }
            }
            if (f_idx==t_idx) {
                fatal_error ("Self-edge in graph?");
            }
            if (f_idx==-1 || t_idx==-1) {
                fatal_error ("Node doesn't exist in graph");
            }
            adj[f_idx].push_back(nodes[t_idx]);
        }

        bool node_exists (DFG_Node *node) {
            bool ret = false;
            for ( auto n1 : nodes ) {
                if (node==n1) ret = true;
            }
            return ret;
        }

        void print_adj (FILE *fp) {
            fprintf (fp, "\n/* ------ adj list ------\n");
            for (int i=0;i<adj.size();i++) {
                fprintf(fp, "\n %d (type: %d): ", nodes[i]->id, int(nodes[i]->t));
                for (int j=0;j<adj[i].size();j++) {
                    fprintf (fp, "%d, ", adj[i][j]->id);
                }
            }
            fprintf (fp, "\n\n   ------ adj list ------ */\n");
        }
};

class Projection : protected ChoppingBlock {
    public:

        Projection (FILE *fp_in, GraphWithChanNames &g_in, 
                    std::unordered_map<const Block *, decomp_info_t *> vmap_in,
                        Scope *s_in) 
            : ChoppingBlock (fp_in, g_in, vmap_in, s_in) 
            {
            }
        
        void project ();
        std::vector<Sequence> get_procs ();
        void print_subgraphs ();
        void split_assignments ();

    private:

        std::vector<Sequence> seqs;
        // std::vector<Block *> nodes;
        // std::vector<Block *> ics;
        std::unordered_map<UnionFind<DFG_Node *>::id, std::vector<DFG_Node *>> subgraphs;
        DFG dfg;

        void _build_graph (Sequence);

        void _compute_connected_components ();

        void _build_sub_procs ();        

        bool _check_linear (Sequence, int);

        bool _check_data_dependence (DFG_Node *, DFG_Node *);

        void _split_assignments (Sequence);

};

#endif