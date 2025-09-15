/*************************************************************************
 *
 *  Copyright (c) 2024-2025 Karthi Srinivasan
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

#include <act/chp/ddg.h>

DFG dfg1;
DFG dfg2;

/*
    Projection Copy-Insertion Strategy
*/
enum class Strategy { None, Heuristic, BruteForce, Timing };

/*
    Class implementing projection based on the DFG.
    @param seqs Vector of Sequences of projected processes
    @param procs Vector of act_chp_lang's of projected processes
    @param subgraphs Union-Find structure to track connected components in DFG
    @param dfg The DFG itself
*/
class Projection : protected ChoppingBlock {
    public:

        Projection (GraphWithChanNames &g_in, Scope *s_in) 
            : ChoppingBlock (g_in, s_in) 
            {
                procs.clear();
                dfg1.clear();
                dfg2.clear();
            }
        
        std::tuple<
            std::unordered_set<ActId *>, 
            act_chp_lang_t *,
            std::vector<std::unordered_map<ChpOptimize::ChanId, ActId *>>
            > get_result ();

        /*
            Compute the projected processes
        */
        void project (Strategy);

        /*
            Returns vector of act_chp_lang's of projected proceses 
        */
        std::vector<act_chp_lang_t *> get_procs ();

        /*
            Print DFG of subgraphs into a file
        */
        void print_subgraphs (FILE *, const std::unordered_map<UnionFind<int>::id, std::vector<int>> &);

        /*
            Split multi-assignments into single
        */
        void split_assignments (ChpGraph &);

        /*
            Print DOT graph of the projected processes
        */
        void export_dot(std::string, const DFG &);

    private:

        std::vector<act_chp_lang_t *> procs;

        void step1(GraphWithChanNames &, DFG &);
        void step2(GraphWithChanNames &, DFG &);
        /*
            Construct CHP process from DFG
        */
        void _build_procs (const GraphWithChanNames &, DFG &d_in);
        /*
            Construct DFG from ChpGraph
        */
        void _build_graph (const Sequence &, DFG &);
        void _build_graph_nodes (const Sequence &, DFG &);
        void _build_graph_edges (DFG &);

        void build_vardefmap (DFG &);
        /*
            Insert distributed assignment of 
            guard variables before selections
        */
        void _insert_guard_comms (GraphWithChanNames &, DFG &);

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
        VarId _insert_hyperedge_copy (GraphWithChanNames &, const DFG &, HyperEdge, VarId, CopyLocMap &);
        void _uninsert_hyperedge_copy (GraphWithChanNames &, const DFG &, HyperEdge, VarId, VarId, CopyLocMap &);

        /*
            Replace use only in this block
        */
        void _replace_use (GraphWithChanNames &, VarId, VarId, const DFG_Node &);

        /*
            Copy insertion strategy: heuristic-based.
            Sends/receives excluded
        */
        void _insert_copies_v3 (GraphWithChanNames &, DFG &, Sequence, int, int, bool &);

        /*
            Copy insertion strategy: latency cost-based.
        */
        void _insert_copies_v6 (GraphWithChanNames &, DFG &);

        NodeId _heuristic2 (DFG &, const DFG_Node &, int);
        NodeId _heuristic3 (DFG &, const DFG_Node &, int);
        
        /*
            Construct a sub-process from a set of DFG nodes.
        */
        bool _build_sub_proc_new (GraphWithChanNames &, const DFG &d_in, Sequence, std::unordered_set<NodeId>&);
        
        /*
            Construct a sub-process from a set of DFG nodes, 
            where all the nodes are basic nodes.
        */
        void _build_basic_new (GraphWithChanNames &, const DFG &, std::vector<NodeId>);
        
        /*
            Check if all nodes are basic nodes.
        */
        bool _all_basic (const DFG &, std::vector<NodeId>);

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
        std::vector<VarId> get_defs (const DFG_Node&);
        
        /*
            Get set of vars used by a DFG node
        */
        std::unordered_set<VarId> get_uses (const DFG_Node&);

        /*
            Check if there is a data dependence between
            `prev` and `curr`. Order matters.
        */
        bool _check_data_dependence (const DFG_Node&, const DFG_Node&);

        /*
            Check if there is guard-phi dependence between
            `guard_node` and `phi_node`. Returns true when
            the guard node and phi node are of the same selection.
        */
        bool _check_guard_phi_dependence (const DFG_Node &, const DFG_Node &);
        
        /*
            Check if there is guard-phi-inv dependence between
            `guard_node` and `phi_inv_node`. Returns true when
            `guard_node` and `phi_inv_node` are of the same selection,
            and the output of the `phi_inv` corresponding to the 
            `guard_node`'s branch is not NULL.
        */
        bool _check_guard_phi_inv_dependence (const DFG_Node &, const DFG_Node &);

        void _split_assignments (Sequence);
};

#endif