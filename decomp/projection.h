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
#include "chp_cost.h"

#include "../opt/static-tokens.h"
#include "../opt/union-find.h"

enum class NodeType { Basic, Guard, LoopGuard, LoopInPhi, LoopOutPhi, LoopLoopPhi, SelPhi, SelPhiInv, PllPhi, PllPhiInv };

typedef std::pair<int,int> IntPair;

template<> struct std::hash<IntPair> {
    size_t operator()(const IntPair &x) const {
        size_t seed = 0;
        hash_combine(seed, x.first);
        hash_combine(seed, x.second);
        return seed;
    }
};

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

        // should be union
            Block::Variant_Select::PhiSplit phi_inv;
            Block::Variant_Select::PhiMerge phi;
            Block::Variant_Par::PhiSplit    pll_phi_inv;
            Block::Variant_Par::PhiMerge    pll_phi;
            std::pair<int, IRGuard>         g;
            Block::Variant_DoLoop::InPhi    lip;
            Block::Variant_DoLoop::OutPhi   lop;
            Block::Variant_DoLoop::LoopPhi  llp;
        // should be union

        int id;
        int set_n;

        explicit operator bool() const {
            return (id!=-1);
        }

        DFG_Node (const DFG_Node &other) {
            t = other.t;
            b = other.b;
            phi_inv = other.phi_inv;
            phi = other.phi;
            pll_phi_inv = other.pll_phi_inv;
            pll_phi = other.pll_phi;
            g = {other.g.first,IRGuard::deep_copy(other.g.second)};
            lip = other.lip;
            lop = other.lop;
            llp = other.llp;
            id = other.id;
            set_n = other.set_n;
        }

        // Do not use !!
        DFG_Node (int idx) 
        {
            t = NodeType::Basic;
            b = NULL;
            id = idx;
            set_n = -1;
        }
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
        // Note: Don't think this nodetype is necessary, but 
        // leaving it in for now..
        DFG_Node (Block *_b, const ChpExprSingleRootDag &_g, int idx) 
        {
            hassert (_b->type() == BlockType::DoLoop);
            t = NodeType::LoopGuard;
            b = _b;
            g = {0, IRGuard::makeExpression(ChpExprSingleRootDag::deep_copy(_g))};
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
            case NodeType::LoopLoopPhi: {
                ss << "(" << strofid(llp.post_id) << ", " << strofid(llp.bodyin_id) << ") = phiL(" << strofid(llp.pre_id) << ", " << strofid(llp.bodyout_id) << ");" << std::endl;
            }
            break;
            default:
            break;
            }
        }
};

/*
    bottom/null element for this class.
    idk what the idiomatic C/C++ way to do this is.
*/
static DFG_Node bot(-1);

/*
    Class implementing the data-dependence graph.
    @param nodes Vector of DFG nodes
    @param adj Adjancency list encoding edges between DFG nodes
    @param id Internal ID counter to enumerate nodes
*/
class DFG {
    public:
        std::vector<std::unique_ptr<DFG_Node>> nodes;
        std::vector<std::vector<int>> adj;
        std::unordered_map<int, int> sccs;
        int id;

        DFG () :
        nodes {}, adj {}
        {
            nodes.clear();
            adj.clear();
            sccs.clear();
            id = 0;
        }

        /*
            Clear the DFG.
        */
        void clear () {
            nodes.clear();
            adj.clear();
            sccs.clear();
            id = 0;
        }

        int gen_id () {
            return id++;
        }

        /*
            Add a given node to the DFG.
        */
        void add_node (DFG_Node n) {
            nodes.push_back(std::make_unique<DFG_Node> (n));
            adj.push_back({});
        }

        /*
            Add a directed edge between two nodes.
            Both nodes must be in the DFG.
        */
        void add_edge (int from, int to) {
            Assert (from>=0 && from<adj.size(), "invalid from node");
            Assert (to>=0 && to<adj.size(), "invalid to node");
            adj[from].push_back(to);
        }

        /*
            Delete a directed edge between two nodes.
            Edge and both nodes must be in the DFG.
        */
        void delete_edge (int from, int to) {
            Assert (from>=0 && from<adj.size(), "invalid from node");
            Assert (to>=0 && to<adj.size(), "invalid to node");
            adj[from].erase(std::remove(adj[from].begin(), adj[from].end(), to), adj[from].end());
        }

        /*
            Check if there exists a directed edge
            between the given nodes. 
        */
        bool contains_edge (int from, int to) {
            Assert (from>=0 && from<adj.size(), "invalid from node");
            Assert (to>=0 && to<adj.size(), "invalid to node");
            const auto &neighbors = adj[from];
            return std::find(neighbors.begin(), neighbors.end(), to) != neighbors.end();
        }

        /*
            Check if given node_id exists in the DFG.
        */
        bool contains (int node_id) {
            for ( const auto &n1 : nodes ) {
                if (node_id==n1->id) return true;
            }
            return false;
        }

        /*
            Check if a node with the given Block* exists in the DFG.
        */
        bool contains (Block *b) {
            for ( const auto &n1 : nodes ) {
                if (b==(n1->b)) return true;
            }
            return false;
        }

        /*
            Find a given basic block in the DFG.
        */
        const DFG_Node &find (int node_id) {
            hassert (node_id>=0 && node_id<nodes.size());
            for ( const auto &n1 : nodes ) {
                if (n1->id == node_id) 
                    return *n1;
            }
            hassert(false);
            return bot;
       }

        const DFG_Node &find (Block *b) {
            hassert (b->type()==BlockType::Basic);
            for ( const auto &n1 : nodes ) {
                if (b == n1->b) {
                    return *n1;
                }
            }
            return bot;
        }

        const DFG_Node &find (Block *b, Block::Variant_Par::PhiSplit ps) {
            hassert (b->type()==BlockType::Par);
            for ( const auto &n1 : nodes ) {
                if ( b==(n1->b) && (n1->t == NodeType::PllPhiInv) 
                    && (n1->pll_phi_inv.pre_id == ps.pre_id) && 
                    (n1->pll_phi_inv.branch_ids == ps.branch_ids) ) 
                        return *n1;
            }
            return bot;
        }
        const DFG_Node &find (Block *b, Block::Variant_Par::PhiMerge pm) {
            hassert (b->type()==BlockType::Par);
            for ( const auto &n1 : nodes ) {
                if ( b==(n1->b) && (n1->t == NodeType::PllPhi) 
                    && (n1->pll_phi.post_id == pm.post_id) && 
                    (n1->pll_phi.branch_ids == pm.branch_ids) ) 
                        return *n1;
            }
            return bot;
        }
        const DFG_Node &find (Block *b, Block::Variant_Select::PhiSplit ps) {
            hassert (b->type()==BlockType::Select);
            for ( const auto &n1 : nodes ) {
                if ( b==(n1->b) && (n1->t == NodeType::SelPhiInv) 
                    && (n1->phi_inv.pre_id == ps.pre_id) && 
                    (n1->phi_inv.branch_ids == ps.branch_ids) ) 
                        return *n1;
            }
            return bot;
        }
        const DFG_Node &find (Block *b, Block::Variant_Select::PhiMerge pm) {
            hassert (b->type()==BlockType::Select);
            for ( const auto &n1 : nodes ) {
                if ( b==(n1->b) && (n1->t == NodeType::SelPhi) 
                    && (n1->phi.post_id == pm.post_id) && 
                    (n1->phi.branch_ids == pm.branch_ids) ) 
                        return *n1;
            }
            return bot;
        }
        const DFG_Node &find (Block *b, std::pair<int, IRGuard> g) {
            hassert (b->type()==BlockType::Select);
            for ( const auto &n1 : nodes ) {
                if ( b==(n1->b) && (n1->t == NodeType::Guard) 
                    && n1->g.first == g.first ) 
                        return *n1;
            }
            return bot;
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
                    fprintf (fp, "%d, ", adj[i][j]);
                }
            }
            fprintf (fp, "\n\n   ------ adj list ------ */\n");
        }

        // runs depth first search starting at vertex v.
        // each visited vertex is appended to the output vector when dfs leaves it.
        void dfs(int v, std::vector<std::vector<int>> const& _adj, 
                std::vector<int> &output, std::vector<bool> &visited) {
            hassert(v>=0);
            visited[v] = true;
            for (auto u : _adj[v])
                if (!visited[u])
                    dfs(u, _adj, output, visited);
            output.push_back(v);
        }

        // input: adj -- adjacency list of G
        // output: components -- the strongy connected components in G
        // output: adj_cond -- adjacency list of G^SCC (by root vertices)
        void scc(std::vector<std::vector<int>> &comps,
                    std::vector<std::vector<int>> &adj_cond,
                    std::vector<bool> &visited) {
            int nv = adj.size();
            comps.clear(), adj_cond.clear(), visited.clear();

            std::vector<int> order; // will be a sorted list of G's vertices by exit time
            for (int i=0; i<nv; i++) {
                visited.push_back(false);
            }

            // first series of depth first searches
            for ( const auto &n : nodes )
                if (!visited[n->id])
                    dfs(n->id, adj, order, visited);

            // create adjacency list of G^T
            std::vector<std::vector<int>> adj_rev(nv);
            for ( const auto &v : nodes )
                for (auto u : adj[v->id])
                    adj_rev[u].push_back(v->id);

            visited.assign(nv, false);
            reverse(order.begin(), order.end());

            std::vector<int> roots(nv, -1); // gives the root vertex of a vertex's SCC

            // second series of depth first searches
            for (auto v : order)
                if (!visited[v]) {
                    std::vector<int> component;
                    dfs(v, adj_rev, component, visited);
                    comps.push_back(component);
                    auto root = component[0];
                    for (auto u : component)
                        roots[u] = root;
                }

            // add edges to condensation graph
            adj_cond.assign(nv, {});
            for (const auto &v : nodes)
                for (auto u : adj[v->id])
                    if (roots[v->id] != roots[u])
                        adj_cond[roots[v->id]].push_back(roots[u]);
        }

        /*
            Build strongly-connected components map
        */
        void build_sccs (const std::vector<std::vector<int>> &comps)
        {
            int i=0;
            for ( const auto &comp : comps ) {
                for ( const auto n : comp ) {
                    sccs[n]=i;
                }
                i++;
            }
        }
};

DFG dfg;

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
        std::unordered_map<UnionFind<int>::id, std::vector<int>> subgraphs;

        /*
            Construct CHP process from DFG
        */
        void _build_procs (GraphWithChanNames &);
        /*
            Construct DFG from ChpGraph
        */
        void _build_graph (Sequence);
        void _build_graph_nodes (Sequence);
        void _build_graph_edges ();

        /*
            Use Union-Find to compute weakly
            connected components in the DFG
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
        VarId _insert_copy (GraphWithChanNames &, int, VarId);
        // void _insert_copy (GraphWithChanNames &, Sequence, int, VarId);
        void _uninsert_copy (GraphWithChanNames &, int, VarId, VarId);


        /*
            Same as other overload, with option to specify where to place 
            the distributed assignment and where to start the renaming from. 
            Deprecated.
        */
        // void _insert_copy (GraphWithChanNames &, Sequence, Block *, Block *, VarId);

        /*
            Rename `old_var` to `new_var`, but exclude `excl` and start after `start_after`
        */
        void _replace_uses (GraphWithChanNames &, VarId, VarId, Block *, Block *);
        
        /*
            Internal use only.
        */
        void _replace_uses (GraphWithChanNames &, Sequence, VarId, VarId, Block *, Block *);
        void _replace_uses (GraphWithChanNames &, Block *, VarId, VarId, Block *, Block *);
        
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
        void _insert_copies_v3 (GraphWithChanNames &, Sequence, int, int, bool &);

        /*
            Copy insertion strategy: latency cost-based.
        */
        void _insert_copies_v4 (GraphWithChanNames &);

        int _heuristic1 (const DFG_Node &, int);
        int _heuristic2 (const DFG_Node &, int);
        int _heuristic3 (const DFG_Node &, int);

        std::unordered_map<IntPair, std::vector<IntPair>> _candidate_edges ();

        /*
            Check if two nodes are in the same
            strongly-connected component in the DDG
        */
        bool _in_same_scc (int, int);
        
        /*
            Find SCC of a given node
        */
        int _find_scc (int n1);
        /*
            Find SCC of a given node
        */
        std::vector<int>  find_component (DFG_Node *);

        /*
            Find all edges in same group as this edge
            group_ij: set of all edges that go from SCC_i to SCC_j
        */
        std::pair<std::vector<int>, std::vector<int>>  find_components (int, int);


        /*
            Construct a sub-process from a set of DFG nodes.
        */
        bool _build_sub_proc_new (GraphWithChanNames &, Sequence, std::unordered_set<int>&);
        
        /*
            Construct a sub-process from a set of DFG nodes, 
            where all the nodes are basic nodes.
        */
        void _build_basic_new (GraphWithChanNames &, std::vector<int>);
        
        /*
            Check if all nodes are basic nodes.
        */
        bool _all_basic (std::vector<int>);

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