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

enum class NodeType { Basic, Guard, LoopInPhi, LoopOutPhi, LoopLoopPhi, SelPhi, SelPhiInv, PllPhi, PllPhiInv };

class DFG_Node {
    public:
        NodeType t;
        Block *b;
        // union
            Block::Variant_Select::PhiSplit phi_inv;
            Block::Variant_Select::PhiMerge phi;
            Block::Variant_Par::PhiSplit    pll_phi_inv;
            Block::Variant_Par::PhiMerge    pll_phi;
            std::pair<int, IRGuard>         g;
            Block::Variant_DoLoop::InPhi    lip;
            Block::Variant_DoLoop::OutPhi   lop;
            Block::Variant_DoLoop::LoopPhi  llp;
        // union
        int id;
        int set_n;

        DFG_Node (Block *_b, int idx) 
        {
            hassert (_b->type() == BlockType::Basic);
            t = NodeType::Basic;
            b = _b;
            id = idx;
            set_n = -1;
        }
        DFG_Node (Block *_b, int br, IRGuard _g, int idx) 
        {
            hassert (_b->type() == BlockType::Select);
            t = NodeType::Guard;
            b = _b;
            g = {br, IRGuard::deep_copy(_g)};
            id = idx;
            set_n = -1;
        }
        DFG_Node (Block *_b, Block::Variant_Select::PhiSplit x, int idx) 
        {
            hassert (_b->type() == BlockType::Select);
            t = NodeType::SelPhiInv;
            b = _b;
            phi_inv = x;
            id = idx;
            set_n = -1;
        }
        DFG_Node (Block *_b, Block::Variant_Select::PhiMerge x, int idx) 
        {
            hassert (_b->type() == BlockType::Select);
            t = NodeType::SelPhi;
            b = _b;
            phi = x;
            id = idx;
            set_n = -1;
        }
        DFG_Node (Block *_b, Block::Variant_Par::PhiSplit x, int idx) 
        {
            hassert (_b->type() == BlockType::Par);
            t = NodeType::PllPhiInv;
            b = _b;
            pll_phi_inv = x;
            id = idx;
            set_n = -1;
        }
        DFG_Node (Block *_b, Block::Variant_Par::PhiMerge x, int idx) 
        {
            hassert (_b->type() == BlockType::Par);
            t = NodeType::PllPhi;
            b = _b;
            pll_phi = x;
            id = idx;
            set_n = -1;
        }
        DFG_Node (Block *_b, Block::Variant_DoLoop::InPhi x, int idx) 
        {
            hassert (_b->type() == BlockType::DoLoop);
            t = NodeType::LoopInPhi;
            b = _b;
            lip = x;
            id = idx;
            set_n = -1;
        }
        DFG_Node (Block *_b, Block::Variant_DoLoop::OutPhi x, int idx) 
        {
            hassert (_b->type() == BlockType::DoLoop);
            t = NodeType::LoopOutPhi;
            b = _b;
            lop = x;
            id = idx;
            set_n = -1;
        }
        DFG_Node (Block *_b, Block::Variant_DoLoop::LoopPhi x, int idx) 
        {
            hassert (_b->type() == BlockType::DoLoop);
            t = NodeType::LoopLoopPhi;
            b = _b;
            llp = x;
            id = idx;
            set_n = -1;
        }
        void print ()
        {
            if (t==NodeType::Basic) ChpOptimize::print_chp_block(std::cout, b);
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

        void clear () {
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
                printf("\nfrom: %d, to: %d", f_idx, t_idx);
                fatal_error ("Node doesn't exist in graph");
            }
            adj[f_idx].push_back(nodes[t_idx]);
        }

        void delete_edge (DFG_Node *from, DFG_Node *to) {
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
                printf("\nfrom: %d, to: %d", f_idx, t_idx);
                fatal_error ("Node doesn't exist in graph");
            }
            std::vector<DFG_Node *> new_adj = {};
            for (auto i = 0; i<adj[f_idx].size(); i++) {
                if ( !(adj[f_idx][i] == to) )
                    new_adj.push_back(adj[f_idx][i]);
            }
            adj[f_idx] = new_adj;
        }

        bool contains_edge (DFG_Node *from, DFG_Node *to) {
            int f_idx = -1;
            for (int i=0; i<nodes.size(); i++) {
                if (nodes[i] == from) {
                    f_idx = i;
                }
            }
            if (f_idx==-1) {
                printf("\nfrom: %d", f_idx);
                fatal_error ("Node doesn't exist in graph");
            }
            for ( auto x : adj[f_idx] ) {
                if (x==to) return true;
            }
            return false;
        }

        bool contains (DFG_Node *node) {
            for ( auto n1 : nodes ) {
                if (node==n1) return true;
            }
            return false;
        }

        bool contains (Block *b) {
            for ( auto n1 : nodes ) {
                if (b==(n1->b)) return true;
            }
            return false;
        }

        DFG_Node *find (Block *b) {
            // hassert (b->type()==BlockType::Basic);
            for ( auto n1 : nodes ) {
                if (b == n1->b) {
                    return n1;
                }
            }
            return NULL;
        }
        DFG_Node *find (Block *b, Block::Variant_Par::PhiSplit ps) {
            hassert (b->type()==BlockType::Par);
            for ( auto n1 : nodes ) {
                if ( b==(n1->b) && (n1->t == NodeType::PllPhiInv) 
                    && (n1->pll_phi_inv.pre_id == ps.pre_id) && 
                    (n1->pll_phi_inv.branch_ids == ps.branch_ids) ) 
                        return n1;
            }
            return NULL;
        }
        DFG_Node *find (Block *b, Block::Variant_Par::PhiMerge pm) {
            hassert (b->type()==BlockType::Par);
            for ( auto n1 : nodes ) {
                if ( b==(n1->b) && (n1->t == NodeType::PllPhi) 
                    && (n1->pll_phi.post_id == pm.post_id) && 
                    (n1->pll_phi.branch_ids == pm.branch_ids) ) 
                        return n1;
            }
            return NULL;
        }
        DFG_Node *find (Block *b, Block::Variant_Select::PhiSplit ps) {
            hassert (b->type()==BlockType::Select);
            for ( auto n1 : nodes ) {
                if ( b==(n1->b) && (n1->t == NodeType::SelPhiInv) 
                    && (n1->phi_inv.pre_id == ps.pre_id) && 
                    (n1->phi_inv.branch_ids == ps.branch_ids) ) 
                        return n1;
            }
            return NULL;
        }
        DFG_Node *find (Block *b, Block::Variant_Select::PhiMerge pm) {
            hassert (b->type()==BlockType::Select);
            for ( auto n1 : nodes ) {
                if ( b==(n1->b) && (n1->t == NodeType::SelPhi) 
                    && (n1->phi.post_id == pm.post_id) && 
                    (n1->phi.branch_ids == pm.branch_ids) ) 
                        return n1;
            }
            return NULL;
        }
        DFG_Node *find (Block *b, std::pair<int, IRGuard> g) {
            hassert (b->type()==BlockType::Select);
            for ( auto n1 : nodes ) {
                if ( b==(n1->b) && (n1->t == NodeType::Guard) 
                    && n1->g.first == g.first ) 
                        return n1;
            }
            return NULL;
        }

        void print_adj (FILE *fp) {
            fprintf (fp, "\n/* ------ adj list ------\n");
            for (int i=0;i<adj.size();i++) {
                fprintf(fp, "\n %d (type: %d): (", nodes[i]->id, int(nodes[i]->t));
                nodes[i]->print();
                fprintf(fp, "): ");
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
                sel_set_id = 0;
            }
        
        void project ();
        std::vector<Sequence> get_seqs ();
        std::vector<act_chp_lang_t *> get_procs ();
        void print_subgraphs (FILE *);
        void split_assignments (ChpGraph &);
        void split_selections ();

    private:

        std::vector<Sequence> seqs;
        std::vector<act_chp_lang_t *> procs;
        // std::vector<Block *> nodes;
        // std::vector<Block *> ics;
        std::unordered_map<UnionFind<DFG_Node *>::id, std::vector<DFG_Node *>> subgraphs;
        DFG dfg;

        std::unordered_map<int, std::vector<Block *>> sel_sets;
        int sel_set_id;

        int _gen_sel_set_id();

        void _build_graph (Sequence);

        void _compute_connected_components ();
        void _insert_guard_comms ();
        void _remove_guard_comms (GraphWithChanNames &, Sequence);
        void _insert_copy (GraphWithChanNames &, DFG_Node *, VarId);
        void _insert_copies (GraphWithChanNames &, Sequence);
        void _replace_uses (GraphWithChanNames &, Sequence, VarId, VarId, Block *);

        void _build_sub_proc_new (GraphWithChanNames &, Sequence, std::unordered_set<DFG_Node *>&);
        void _build_basic_new (GraphWithChanNames &, std::vector<DFG_Node *>);
        bool _all_basic (std::vector<DFG_Node *>);

        bool _set_contains (Block *, std::unordered_set<DFG_Node *>&);

        bool _check_linear (Sequence, int);
        void _splice_out_node (DFG_Node *);
        void _splice_out_blocks (std::vector<Block *>);
        void _splice_out_block_new (Block *);

        std::vector<VarId> get_defs (DFG_Node *);
        std::unordered_set<VarId> get_uses (DFG_Node *);

        bool _check_data_dependence (DFG_Node *, DFG_Node *);
        bool _check_guard_phi_dependence (DFG_Node *, DFG_Node *);
        bool _check_guard_phi_inv_dependence (DFG_Node *, DFG_Node *);

        void _split_selections (Sequence);
        void _split_assignments (Sequence);

};

#endif