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

#include <act/chp/chopping_block.h>
#include <act/chp/chp_cost.h>

#include <act/chp/static-tokens.h>
#include <act/chp/union-find.h>


enum class NodeType { Basic, Copy, Guard, LoopGuard, LoopInPhi, LoopOutPhi, LoopLoopPhi, SelPhi, SelPhiInv, PllPhi, PllPhiInv };

class NodeId {
public:
    NodeId() : id_(-1) {}
    explicit NodeId(int id) : id_(id) {}

    static NodeId generate() {
        return NodeId(counter_++); 
    }

    static void reset() {
        counter_ = 0;
    }

    int get_raw() const { return id_; }

    bool operator==(const NodeId& other) const { return id_ == other.id_; }
    bool operator!=(const NodeId& other) const { return id_ != other.id_; }
    bool operator<(const NodeId& other)  const { return id_ < other.id_; }

private:
    int id_;
    static int counter_;
};

int NodeId::counter_{0};

static NodeId bot_id = NodeId(-1); 

template <> struct std::hash<NodeId> {
    size_t operator()(const NodeId &obj) const {
        return hash<int>()(obj.get_raw());
    }
};

typedef std::pair<NodeId,NodeId> Edge;

// Basically a copy tree
typedef std::pair<NodeId,std::unordered_set<NodeId>> HyperEdge;

inline bool operator==(const HyperEdge& a, const HyperEdge& b) {
    return a.first == b.first && a.second == b.second;
}

template<> struct std::hash<HyperEdge> {
    size_t operator()(const HyperEdge& e) const noexcept {
        size_t h = std::hash<NodeId>{}(e.first);
        for (const auto& tail : e.second) {
            h ^= std::hash<NodeId>{}(tail) + 0x9e3779b9 + (h << 6) + (h >> 2);
        }
        return h;
    }
};

typedef std::unordered_map<NodeId,std::unordered_set<NodeId>> HyperEdges;

typedef std::unordered_map<VarId, Block *> CopyLocMap;

template<> struct std::hash<Edge> {
    size_t operator()(const Edge &x) const {
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

        NodeId id;
        int set_n;

        explicit operator bool() const {
            return (id.get_raw()!=-1);
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
        DFG_Node (NodeId idx) 
        {
            t = NodeType::Copy;
            b = NULL;
            id = NodeId(idx.get_raw());
            set_n = -1;
        }
        DFG_Node (Block *_b) 
        {
            hassert (_b->type() == BlockType::Basic);
            t = NodeType::Basic;
            b = _b;
            id = NodeId::generate();
            set_n = -1;
        }
        DFG_Node (Block *_b, int br, IRGuard _g) 
        {
            hassert (_b->type() == BlockType::Select);
            t = NodeType::Guard;
            b = _b;
            g = {br, IRGuard::deep_copy(_g)};
            id = NodeId::generate();
            set_n = -1;
        }
        // Note: Don't think this nodetype is necessary, but 
        // leaving it in for now..
        DFG_Node (Block *_b, const ChpExprSingleRootDag &_g) 
        {
            hassert (_b->type() == BlockType::DoLoop);
            t = NodeType::LoopGuard;
            b = _b;
            g = {0, IRGuard::makeExpression(ChpExprSingleRootDag::deep_copy(_g))};
            id = NodeId::generate();
            set_n = -1;
        }
        DFG_Node (Block *_b, const Block::Variant_Select::PhiSplit &x) 
        {
            hassert (_b->type() == BlockType::Select);
            t = NodeType::SelPhiInv;
            b = _b;
            phi_inv = x;
            id = NodeId::generate();
            set_n = -1;
        }
        DFG_Node (Block *_b, const Block::Variant_Select::PhiMerge &x) 
        {
            hassert (_b->type() == BlockType::Select);
            t = NodeType::SelPhi;
            b = _b;
            phi = x;
            id = NodeId::generate();
            set_n = -1;
        }
        DFG_Node (Block *_b, const Block::Variant_Par::PhiSplit &x) 
        {
            hassert (_b->type() == BlockType::Par);
            t = NodeType::PllPhiInv;
            b = _b;
            pll_phi_inv = x;
            id = NodeId::generate();
            set_n = -1;
        }
        DFG_Node (Block *_b, const Block::Variant_Par::PhiMerge &x) 
        {
            hassert (_b->type() == BlockType::Par);
            t = NodeType::PllPhi;
            b = _b;
            pll_phi = x;
            id = NodeId::generate();
            set_n = -1;
        }
        DFG_Node (Block *_b, const Block::Variant_DoLoop::InPhi &x) 
        {
            hassert (_b->type() == BlockType::DoLoop);
            t = NodeType::LoopInPhi;
            b = _b;
            lip = x;
            id = NodeId::generate();
            set_n = -1;
        }
        DFG_Node (Block *_b, const Block::Variant_DoLoop::OutPhi &x) 
        {
            hassert (_b->type() == BlockType::DoLoop);
            t = NodeType::LoopOutPhi;
            b = _b;
            lop = x;
            id = NodeId::generate();
            set_n = -1;
        }
        DFG_Node (Block *_b, const Block::Variant_DoLoop::LoopPhi &x) 
        {
            hassert (_b->type() == BlockType::DoLoop);
            t = NodeType::LoopLoopPhi;
            b = _b;
            llp = x;
            id = NodeId::generate();
            set_n = -1;
        }

        /*
            Print the DFG Node
        */
        void print (std::ostream &ss) const
        {
            auto strofid = [&](const OptionalVarId &id) {
                return id ? "v" + std::to_string(((*id).m_id)) : "vNULL";
            };
            switch (t) {
            case NodeType::Basic: {
                ChpOptimize::print_chp_block(ss, b);
            }
            break;
            case NodeType::LoopGuard: {
                ss << "l";
            } 
            case NodeType::Guard: {
                ss << "guard: " << g.first;
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
                ss << ")";
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
                ss << ") = phi_inv(" << strofid(phi_inv.pre_id) <<")";
            }
            break;
            case NodeType::LoopLoopPhi: {
                ss << "(" << strofid(llp.post_id) << ", " << strofid(llp.bodyin_id) << ") = phiL(" << strofid(llp.pre_id) << ", " << strofid(llp.bodyout_id) << ")";
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
static DFG_Node bot(NodeId(-1));

// TODO: Wrap node_id int in its own class for type-safety

/*
    Class implementing the data-dependence graph.
    @param nodes Vector of DFG nodes
    @param adj Adjancency list encoding edges between DFG nodes
    @param id Internal ID counter to enumerate nodes
*/
class DFG {
    public:
        std::vector<std::unique_ptr<DFG_Node>> nodes;
        std::unordered_map<NodeId,std::unordered_set<NodeId>> adj;
        std::unordered_map<NodeId, int> sccs;
        std::unordered_map<VarId, NodeId> vardefmap;
        bool sccs_built;

        DFG ()
        {
            nodes.clear();
            adj.clear();
            sccs.clear();
            vardefmap.clear();
            NodeId::reset();
            sccs_built = false;
        }

        /*
            Clear the DFG.
        */
        void clear () {
            nodes.clear();
            adj.clear();
            sccs.clear();
            vardefmap.clear();
            NodeId::reset();
            sccs_built = false;
        }

        /*
            Add a given node to the DFG.
        */
        void add_node (DFG_Node n) {
            nodes.push_back(std::make_unique<DFG_Node> (n));
            adj[nodes.back()->id] = {};
            sccs_built = false;
        }

        /*
            Add a directed edge between two nodes.
            Both nodes must be in the DFG.
        */
        void add_edge (NodeId from, NodeId to) {
            Assert (contains(from), "invalid from node");
            Assert (contains(to), "invalid to node");
            adj[from].insert(to);
            sccs_built = false;
        }

        /*
            Delete a directed edge between two nodes.
            Edge and both nodes must be in the DFG.
        */
        void delete_edge (NodeId from, NodeId to) {
            Assert (contains(from), "invalid from node");
            Assert (contains(to), "invalid to node");
            adj[from].erase(to);
            sccs_built = false;
        }

        /*
            Delete all outgoing edges from a node.
        */
        void delete_all_out_edges (NodeId from) {
            Assert (contains(from), "invalid from node");
            adj[from].clear();
            sccs_built = false;
        }

        /*
            Get all outgoing edges from a node
        */
        std::unordered_set<NodeId> get_out_edges (NodeId from) const {
            Assert (contains(from), "invalid from node");
            return adj.at(from);
        }

        /*
            Check if there exists a directed edge
            between the given nodes. 
        */
        bool contains_edge (NodeId from, NodeId to) const {
            Assert (contains(from), "invalid from node");
            Assert (contains(to), "invalid to node");
            return adj.at(from).count(to);
        }

        /*
            Check if given node_id exists in the DFG.
        */
        bool contains (NodeId node_id) const {
            for ( const auto &n1 : nodes ) {
                if (node_id==n1->id) return true;
            }
            return false;
        }

        /*
            Check if a node with the given Block* exists in the DFG.
        */
        bool contains (const Block *b) const {
            for ( const auto &n1 : nodes ) {
                if (b==(n1->b)) return true;
            }
            return false;
        }

        /*
            Find a given basic block in the DFG.
        */
        const DFG_Node &find (NodeId node_id) const {
            if (!contains(node_id)) {
                fprintf(stdout, "Node ID : %d", node_id.get_raw());
                print_adj(stdout);
            }
            Assert (contains(node_id), "Invalid node");
            for ( const auto &n1 : nodes ) {
                if (n1->id == node_id) 
                    return *n1;
            }
            hassert(false);
            return bot;
       }

        const DFG_Node &find (const Block *b) const {
            hassert (b->type()==BlockType::Basic);
            for ( const auto &n1 : nodes ) {
                if (b == n1->b) {
                    return *n1;
                }
            }
            return bot;
        }

        const DFG_Node &find (const Block *b, const Block::Variant_Par::PhiSplit &ps) const {
            hassert (b->type()==BlockType::Par);
            for ( const auto &n1 : nodes ) {
                if ( b==(n1->b) && (n1->t == NodeType::PllPhiInv) 
                    && (n1->pll_phi_inv.pre_id == ps.pre_id) && 
                    (n1->pll_phi_inv.branch_ids == ps.branch_ids) ) 
                        return *n1;
            }
            return bot;
        }
        const DFG_Node &find (const Block *b, const Block::Variant_Par::PhiMerge &pm) const {
            hassert (b->type()==BlockType::Par);
            for ( const auto &n1 : nodes ) {
                if ( b==(n1->b) && (n1->t == NodeType::PllPhi) 
                    && (n1->pll_phi.post_id == pm.post_id) && 
                    (n1->pll_phi.branch_ids == pm.branch_ids) ) 
                        return *n1;
            }
            return bot;
        }
        const DFG_Node &find (const Block *b, const Block::Variant_Select::PhiSplit &ps) const {
            hassert (b->type()==BlockType::Select);
            for ( const auto &n1 : nodes ) {
                if ( b==(n1->b) && (n1->t == NodeType::SelPhiInv) 
                    && (n1->phi_inv.pre_id == ps.pre_id) && 
                    (n1->phi_inv.branch_ids == ps.branch_ids) ) 
                        return *n1;
            }
            return bot;
        }
        const DFG_Node &find (const Block *b, const Block::Variant_Select::PhiMerge &pm) const {
            hassert (b->type()==BlockType::Select);
            for ( const auto &n1 : nodes ) {
                if ( b==(n1->b) && (n1->t == NodeType::SelPhi) 
                    && (n1->phi.post_id == pm.post_id) && 
                    (n1->phi.branch_ids == pm.branch_ids) ) 
                        return *n1;
            }
            return bot;
        }
        const DFG_Node &find (const Block *b, const std::pair<int, IRGuard> &g) const {
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
        void print_adj (FILE *fp) const {
            fprintf (fp, "\n/* ------ adj list ------\n");
            for (auto x : adj) {
                auto node = find(x.first);
                fprintf(fp, "\n %d (type: %d): (", x.first.get_raw(), int(node.t));
                node.print(std::cout);
                fprintf(fp, "): ");
                for (auto y : x.second) {
                    fprintf (fp, "%d, ", y.get_raw());
                }
            }
            fprintf (fp, "\n\n   ------ adj list ------ */\n");
        }

        /*
            Use Union-Find to compute weakly
            connected components in the DFG
        */
        std::unordered_map<UnionFind<NodeId>::id, std::vector<NodeId>> get_wccs () const {
            std::unordered_map<UnionFind<NodeId>::id, std::vector<NodeId>> wccs = {};

            ChpOptimize::UnionFind<NodeId> uf;
            for ( auto x : adj ) {
                for ( auto y : x.second ) {
                    uf.union_(x.first,y);
                }
            }
            for ( const auto &n : nodes ) {
                auto ufn = uf.find(n->id);
                if (!wccs.count(ufn)) {
                    wccs.insert({ufn,{}});
                }
                wccs[ufn].push_back(n->id);
            } 
            return wccs;
        }

        /*
            runs depth first search starting at vertex v.
            each visited vertex is appended to the output vector when dfs leaves it.
        */
        void dfs (NodeId v, const std::unordered_map<NodeId,std::unordered_set<NodeId>> &_adj, 
                std::vector<NodeId> &output, 
                std::unordered_map<NodeId,bool> &visited) const {
            hassert(v.get_raw()>=0);
            visited[v] = true;
            Assert (_adj.count(v), "what tf");
            for (auto u : _adj.at(v))
                if (!visited[u])
                    dfs(u, _adj, output, visited);
            output.push_back(v);
        }

        /*
            input: adj -- adjacency list of G
            output: components -- the strongy connected components in G
            output: adj_cond -- adjacency list of G^SCC (by root vertices)
        */
        void scc_helper(std::vector<std::vector<NodeId>> &comps,
                    std::unordered_map<NodeId,std::unordered_set<NodeId>> &adj_cond,
                    std::unordered_map<NodeId,bool> &visited) const {
            int nv = adj.size();
            comps.clear(), adj_cond.clear(), visited.clear();

            // will be a sorted list of G's vertices by exit time
            std::vector<NodeId> order; 
            for (auto x : adj) {
                visited[x.first] = false;
            }
            // first series of depth first searches
            for ( const auto &n : nodes )
                if (!visited[n->id])
                    dfs(n->id, adj, order, visited);

            // create adjacency list of G^T
            std::unordered_map<NodeId,std::unordered_set<NodeId>> adj_rev;
            for ( auto x : adj ) adj_rev[x.first] = {};
            for ( auto x : adj )
                for (auto u : x.second )
                    adj_rev[u].insert(x.first);

            for (auto x : adj) {
                visited[x.first] = false;
            }
            reverse(order.begin(), order.end());

            // gives the root vertex of a vertex's SCC
            std::unordered_map<NodeId, NodeId> roots = {};

            // second series of depth first searches
            for (auto v : order)
                if (!visited[v]) {
                    std::vector<NodeId> component;
                    dfs(v, adj_rev, component, visited);
                    comps.push_back(component);
                    auto root = component[0];
                    for (auto u : component)
                        roots[u] = root;
                }

            // add edges to condensation graph
            adj_cond.clear();
            for (auto x : adj) {
                adj_cond[x.first] = {};
            }
            for (const auto &v : nodes)
                for (auto u : adj.at(v->id))
                    if (roots[v->id] != roots[u])
                        adj_cond[roots[v->id]].insert(roots[u]);
        }

        /*
            Build strongly-connected components map
        */
        void build_sccs ()
        {   
            std::vector<std::vector<NodeId>> comps;
            std::unordered_map<NodeId,std::unordered_set<NodeId>> adj_cond;
            std::unordered_map<NodeId,bool> visited;
            adj_cond.clear(); comps.clear(); visited.clear();
            scc_helper (comps, adj_cond, visited);
            int i=0;
            for ( const auto &comp : comps ) {
                for ( const auto n : comp ) {
                    sccs[n]=i;
                }
                i++;
            }
            sccs_built = true;
        }

        /*
            Find the SCC ID of a given node_id
        */
       int find_scc_id (NodeId n1) const
        {
            Assert (n1.get_raw()!=-1, "Invalid node");
            Assert (contains(n1), "Node does not exist");
            Assert (sccs_built, "SCCs not built");
            Assert (sccs.count(n1), "Node not in SCCs? sth went wrong");
            return sccs.at(n1);
        }

        /*
            Check if two nodes are in the same SCC
        */
        bool in_same_scc (NodeId n1, NodeId n2) const
        {
            return (find_scc_id(n1)==find_scc_id(n2));
        }

        /*
            Get the complete SCC for a given node.
            Returns vector of node_id's
        */
        std::vector<NodeId> find_scc (NodeId n1) const
        {
            std::vector<NodeId> ret = {};
            auto scc_id = find_scc_id (n1);
            for ( const auto &x : sccs ) {
                if (x.second==scc_id) ret.push_back(x.first);
            }
            return ret;
        }

        void print_sccs (FILE *fp) const {
            Assert (sccs_built, "SCCs not built");
            fprintf (fp, "\n/* ------ SCCs ------\n");
            for ( auto x : sccs ) {
                fprintf(stdout, "Node : %d, SCC ID : %d\n", x.first.get_raw(), x.second);
            }
            fprintf (fp, "\n\n   ------ SCCs ------ */\n");
        }

        int n_edges () const {
            int len = 0;
            for ( const auto &x : adj ) 
                len += x.second.size();
            return len;
        }
};

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

        Projection (FILE *fp_in, GraphWithChanNames &g_in, 
                    std::unordered_map<const Block *, decomp_info_t *> vmap_in,
                        Scope *s_in) 
            : ChoppingBlock (fp_in, g_in, vmap_in, s_in) 
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