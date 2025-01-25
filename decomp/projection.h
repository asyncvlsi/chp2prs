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

/*
    Class that implements a single node in the 
    data-dependence graph.
    @param NodeType t Type of the node
    @param b ChpGraph Block corresponding to the node  
    @param phi_inv Selection-Phi-Inverse 
    @param phi Selection-Phi
    @param pll_phi_inv Parallel-Phi-Inverse
    @param pll_phi Parallel-Phi
    @param g Pair of guard expr and branch id in a selection
    @param lip Loop-In-Phi
    @param lop Loop-Out-Phi
    @param llp Loop-Loop-Phi 
    @param id Unique node ID
    @param set_n Set number for the node

*/
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

        /*
            Print the DFG Node
        */
        void print (std::ostream &ss)
        {
            auto strofid = [&](OptionalVarId id) {
                return id ? "v" + std::to_string(((*id).m_id)) : "vNULL";
            };
            switch (t) {
            case NodeType::Basic: {
                ChpOptimize::print_chp_block(ss, b);
            }
            break;
            case NodeType::Guard: {
                ss << "guard " << g.first << std::endl;
            }
            break;
            case NodeType::SelPhi: {
                ss << strofid(phi.post_id) << " = phi(";
                bool first = true;
                for (const auto &id : phi.branch_ids) {
                    if (!first) ss << ", ";
                    first = false;
                    ss << strofid(id);
                }
                ss << ");" << std::endl;
            }
            break;
            case NodeType::SelPhiInv: {
                bool first = true;
                ss << "(";
                for (const auto &id : phi_inv.branch_ids) {
                    if (!first) ss << ", ";
                    ss << strofid(id);
                    first = false;
                }
                ss << ") = phi_inv(" << strofid(phi_inv.pre_id) <<");" << std::endl;
            }
            break;
            default:
            break;
            }
        }
};

/*
    Class implementing the data-dependence graph.
    @param nodes Vector of DFG nodes
    @param adj Adjancency list encoding edges between DFG nodes
    @param id Internal ID counter to enumerate nodes
*/
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

        /*
            Clear the DFG.
        */
        void clear () {
            nodes.clear();
            adj.clear();
            id = 0;
        }

        int gen_id () {
            // id++;
            return id++;
        }

        /*
            Add a given node to the DFG.
        */
        void add_node (DFG_Node *n) {
            nodes.push_back(n);
            adj.push_back({});
        }

        /*
            Add a directed edge between two nodes.
            Both nodes must be in the DFG.
        */
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

        /*
            Delete a directed edge between two nodes.
            Edge and both nodes must be in the DFG.
        */
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

        /*
            Check if there exists a directed edge
            between the given nodes. 
        */
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

        /*
            Check if given node exists in the DFG.
        */
        bool contains (DFG_Node *node) {
            for ( auto n1 : nodes ) {
                if (node==n1) return true;
            }
            return false;
        }

        /*
            Check if a node with the given Block* exists in the DFG.
        */
        bool contains (Block *b) {
            for ( auto n1 : nodes ) {
                if (b==(n1->b)) return true;
            }
            return false;
        }

        /*
            Check if given basic block exists in the DFG.
        */
        DFG_Node *find (Block *b) {
            hassert (b->type()==BlockType::Basic);
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

        /*
            Print the DFG
        */
        void print_adj (FILE *fp) {
            fprintf (fp, "\n/* ------ adj list ------\n");
            for (int i=0;i<adj.size();i++) {
                fprintf(fp, "\n %d (type: %d): (", nodes[i]->id, int(nodes[i]->t));
                nodes[i]->print(std::cout);
                fprintf(fp, "): ");
                for (int j=0;j<adj[i].size();j++) {
                    fprintf (fp, "%d, ", adj[i][j]->id);
                }
            }
            fprintf (fp, "\n\n   ------ adj list ------ */\n");
        }
};

/*
    Class implementing projection based on the DFG.
    @param seqs Vector of Sequences of projected processes
    @param procs Vector of act_chp_lang's of projected processes
    @param subgraphs Union-Find structure to track connected components in DFG
    @param dfg The DFG itself
*/
class Projection : protected ChoppingBlock {
    public:

        Projection (FILE *fp_in, GraphWithChanNames &g_in, 
                    std::unordered_map<const Block *, decomp_info_t *> vmap_in,
                        Scope *s_in) 
            : ChoppingBlock (fp_in, g_in, vmap_in, s_in) 
            {
                seqs.clear();
                procs.clear();
                subgraphs.clear();
                dfg.clear();
            }
        
        /*
            Compute the projected processes
        */
        void project ();

        /*
            Returns vector of Sequences of projected proceses 
        */
        std::vector<Sequence> get_seqs ();

        /*
            Returns vector of act_chp_lang's of projected proceses 
        */
        std::vector<act_chp_lang_t *> get_procs ();

        /*
            Print DFG of subgraphs into a file
        */
        void print_subgraphs (FILE *);

        /*
            Split multi-assignments into single
        */
        void split_assignments (ChpGraph &);

        /*
            Print DOT graph of the projected processes
        */
        void export_dot(std::string);

    private:

        std::vector<Sequence> seqs;
        std::vector<act_chp_lang_t *> procs;
        std::unordered_map<UnionFind<DFG_Node *>::id, std::vector<DFG_Node *>> subgraphs;
        DFG dfg;

        /*
            Construct DFG from ChpGraph
        */
        void _build_graph (Sequence);

        /*
            Use Union-Find to compute connected 
            components in the DFG
        */
        void _compute_connected_components ();

        /*
            Insert distributed assignment of 
            guard variables before selections
        */
        void _insert_guard_comms ();

        /*
            Remove distributed assignment of 
            guard variables before selections,
            if they are in same process
        */
        void _remove_guard_comms (GraphWithChanNames &, Sequence);

        /*
            Insert a distributed assignment for a given variable, 
            and flush the renaming downstream in the program. 
            Due to STF, it is sufficient to rename within the sequence.
        */
        void _insert_copy (GraphWithChanNames &, Sequence, DFG_Node *, VarId);

        /*
            Same as other overload, with option to specify where to place 
            the distributed assignment and where to start the renaming from. 
        */
        void _insert_copy (GraphWithChanNames &, Sequence, Block *, Block *, VarId);

        /*
            Rename `old_var` to `new_var`, but exclude `excl` and start after `start_after`
        */
        void _replace_uses (GraphWithChanNames &, Sequence, VarId, VarId, Block *, Block *);
        
        /*
            Copy insertion strategy: at all receives.
        */
        void _insert_copies_v0 (GraphWithChanNames &, Sequence);

        /*
            Copy insertion strategy: at splits and merges.
        */
        void _insert_copies_v1 (GraphWithChanNames &, Sequence);

        /*
            Copy insertion strategy: heuristic-based.
            Sends/receives included
        */
        void _insert_copies_v2 (GraphWithChanNames &, Sequence, int, bool &);

        /*
            Copy insertion strategy: heuristic-based.
            Sends/receives excluded
        */
        void _insert_copies_v3 (GraphWithChanNames &, Sequence, int, bool &);

        DFG_Node *_heuristic1 (DFG_Node *, int);
        DFG_Node *_heuristic2 (DFG_Node *, int);

        /*
            Construct a sub-process from a set of DFG nodes.
        */
        bool _build_sub_proc_new (GraphWithChanNames &, Sequence, std::unordered_set<DFG_Node *>&);
        
        /*
            Construct a sub-process from a set of DFG nodes, 
            where all the nodes are basic nodes.
        */
        void _build_basic_new (GraphWithChanNames &, std::vector<DFG_Node *>);
        
        /*
            Check if all nodes are basic nodes.
        */
        bool _all_basic (std::vector<DFG_Node *>);

        /*
            Check if a sequence is linear, i.e. has no
            selections or loops within it.
        */
        bool _check_linear (Sequence, int);

        /*
            Splice out set of blocks
        */
        void _splice_out_blocks (std::vector<Block *>);
        
        /*
            Splice out a block from its sequence
        */
        void _splice_out_block_new (Block *);

        /*
            Get set of vars defined by a DFG node
        */
        std::vector<VarId> get_defs (DFG_Node *);
        
        /*
            Get set of vars used by a DFG node
        */
        std::unordered_set<VarId> get_uses (DFG_Node *);

        /*
            Check if there is a data dependence between
            `prev` and `curr`. Order matters.
        */
        bool _check_data_dependence (DFG_Node *, DFG_Node *);

        /*
            Check if there is guard-phi dependence between
            `guard_node` and `phi_node`. Returns true when
            the guard node and phi node are of the same selection.
        */
        bool _check_guard_phi_dependence (DFG_Node *, DFG_Node *);
        
        /*
            Check if there is guard-phi-inv dependence between
            `guard_node` and `phi_inv_node`. Returns true when
            `guard_node` and `phi_inv_node` are of the same selection,
            and the output of the `phi_inv` corresponding to the 
            `guard_node`'s branch is not NULL.
        */
        bool _check_guard_phi_inv_dependence (DFG_Node *, DFG_Node *);

        void _split_assignments (Sequence);
};

#endif