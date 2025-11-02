/*************************************************************************
 *
 *  Copyright (c) 2025 Karthi Srinivasan
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


#ifndef __DGG_H__
#define __DGG_H__

#include <act/chp/chopping_block.h>
#include <act/chp/chp_cost.h>

#include <act/chp/static-tokens.h>
#include <act/chp/union-find.h>
#include <variant>

template <class G>
class NodeIdT {
public:
    NodeIdT() : id_(-1) {}
    explicit NodeIdT(int id) : id_(id) {}

    int get_raw() const { return id_; }

    bool operator==(const NodeIdT& other) const { return id_ == other.id_; }
    bool operator!=(const NodeIdT& other) const { return id_ != other.id_; }
    bool operator<(const NodeIdT& other)  const { return id_ < other.id_; }

private:
    int id_;
};

template<class G>
class NodeIdGeneratorT {
public:
    NodeIdGeneratorT() : counter_(0) {}
    NodeIdT<G> generate() { return NodeIdT<G>(counter_++); }
    void reset() { counter_ = 0; }

private:
    int counter_;
};


struct G_DFG {};
using NodeId = NodeIdT<G_DFG>;
using NodeIdGenerator = NodeIdGeneratorT<G_DFG>;

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

// Vector of Hyperedges - this is typically a bunch to cut at once
typedef std::vector<HyperEdge> HyperEdgeVec;

// Vector of bunches - used as set to iterate over and try cutting
typedef std::vector<HyperEdgeVec> HyperEdgesVec;

typedef std::unordered_map<NodeId,std::unordered_set<NodeId>> HyperEdgeSet;

typedef std::vector<HyperEdgeSet> HyperEdgeSetVec;

typedef std::unordered_map<VarId, Block *> CopyLocMap;

template<> struct std::hash<Edge> {
    size_t operator()(const Edge &x) const {
        size_t seed = 0;
        hash_combine(seed, x.first);
        hash_combine(seed, x.second);
        return seed;
    }
};

enum class NodeType { Null, Basic, 
                      Guard, LoopGuard, 
                      LoopInPhi, LoopOutPhi, LoopLoopPhi, 
                      SelPhi, SelPhiInv, 
                      PllPhi, PllPhiInv };

typedef std::variant<
    std::monostate,
    Block::Variant_Select::PhiSplit, 
    Block::Variant_Select::PhiMerge, 
    Block::Variant_Par::PhiSplit,
    Block::Variant_Par::PhiMerge,
    std::pair<int, IRGuard>,
    Block::Variant_DoLoop::InPhi,
    Block::Variant_DoLoop::OutPhi,
    Block::Variant_DoLoop::LoopPhi
    > NodeData;

template<class... Ts> struct Overload : Ts... { using Ts::operator()...; };
template<class... Ts> Overload(Ts...) -> Overload<Ts...>;

static NodeType get_node_type (const NodeData &d) {
    return std::visit(Overload{
        [](const std::monostate                  &x) { return NodeType::Basic;      },
        [](const Block::Variant_Select::PhiSplit &x) { return NodeType::SelPhiInv;  },
        [](const Block::Variant_Select::PhiMerge &x) { return NodeType::SelPhi;     },
        [](const Block::Variant_Par::PhiSplit    &x) { return NodeType::PllPhiInv;  },
        [](const Block::Variant_Par::PhiMerge    &x) { return NodeType::PllPhi;     },
        [](const std::pair<int, IRGuard>         &x) { return NodeType::Guard;      },
        [](const Block::Variant_DoLoop::InPhi    &x) { return NodeType::LoopInPhi;  },
        [](const Block::Variant_DoLoop::OutPhi   &x) { return NodeType::LoopOutPhi; },
        [](const Block::Variant_DoLoop::LoopPhi  &x) { return NodeType::LoopLoopPhi;}
    }, d);
}

static NodeData clone_data (const NodeData &d) {
    return std::visit(Overload{
    [](std::monostate                  x) { return NodeData(std::monostate{}); },
    [](Block::Variant_Select::PhiSplit x) { return NodeData(x); },
    [](Block::Variant_Select::PhiMerge x) { return NodeData(x); },
    [](Block::Variant_Par::PhiSplit    x) { return NodeData(x); },
    [](Block::Variant_Par::PhiMerge    x) { return NodeData(x); },
    [](const std::pair<int, IRGuard>  &x) { return NodeData(std::move(std::make_pair(int(x.first),IRGuard::deep_copy(x.second)))); },
    [](Block::Variant_DoLoop::InPhi    x) { return NodeData(x); },
    [](Block::Variant_DoLoop::OutPhi   x) { return NodeData(x); },
    [](Block::Variant_DoLoop::LoopPhi  x) { return NodeData(x); }
}, d);
}

/*
    Class that implements a single node in the 
    data-dependence graph.
    @param t Type of the node
    @param b ChpGraph Block corresponding to the node  
    @param data Contains data for non-basic nodes
    @param id Unique node ID
*/
class DFG_Node {
    public:
        NodeType t;
        Block *b;
        NodeData data;

        NodeId id;

        explicit operator bool() const {
            return (id.get_raw()!=-1);
        }

        // Do not use !!
        DFG_Node (NodeId idx) 
        {
            t = NodeType::Null;
            b = nullptr;
            id = NodeId(idx.get_raw());
        }
        DFG_Node (Block *_b, NodeId _id) 
        {
            hassert (_b->type() == BlockType::Basic);
            t = NodeType::Basic;
            b = _b;
            data = std::monostate{};
            id = _id;
        }
        DFG_Node (Block *_b, NodeData d, NodeId _id) {
            b = _b;
            data = std::move(d);
            t = get_node_type(data);
            id = _id;
        } 
        DFG_Node clone () const {
            DFG_Node ret = DFG_Node (id);
            ret.data = clone_data(data);
            ret.b = b;
            ret.t = t;
            return std::move(ret);
        }

        DFG_Node (const DFG_Node&) = delete;
        DFG_Node& operator=(const DFG_Node&) = delete;
        DFG_Node(DFG_Node&&) noexcept = default;
        DFG_Node& operator=(DFG_Node&&) noexcept = default;

        const Block::Variant_Select::PhiSplit &phi_inv() const {
            hassert (std::holds_alternative<Block::Variant_Select::PhiSplit>(data));
            return std::get<Block::Variant_Select::PhiSplit>(data);
        }
        const Block::Variant_Select::PhiMerge &phi() const {
            hassert (std::holds_alternative<Block::Variant_Select::PhiMerge>(data));
            return std::get<Block::Variant_Select::PhiMerge>(data);
        }
        const Block::Variant_Par::PhiSplit &pll_phi_inv() const {
            hassert (std::holds_alternative<Block::Variant_Par::PhiSplit>(data));
            return std::get<Block::Variant_Par::PhiSplit>(data);
        }
        const Block::Variant_Par::PhiMerge &pll_phi() const {
            hassert (std::holds_alternative<Block::Variant_Par::PhiMerge>(data));
            return std::get<Block::Variant_Par::PhiMerge>(data);
        }
        const std::pair<int, IRGuard> &g() const {
            hassert ((std::holds_alternative<std::pair<int, IRGuard>>(data)));
            return std::get<std::pair<int, IRGuard>>(data);
        }
        const Block::Variant_DoLoop::InPhi &lip() const {
            hassert (std::holds_alternative<Block::Variant_DoLoop::InPhi>(data));
            return std::get<Block::Variant_DoLoop::InPhi>(data);
        }
        const Block::Variant_DoLoop::OutPhi &lop() const {
            hassert (std::holds_alternative<Block::Variant_DoLoop::OutPhi>(data));
            return std::get<Block::Variant_DoLoop::OutPhi>(data);
        }
        const Block::Variant_DoLoop::LoopPhi &llp() const {
            hassert (std::holds_alternative<Block::Variant_DoLoop::LoopPhi>(data));
            return std::get<Block::Variant_DoLoop::LoopPhi>(data);
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
                auto u_g = std::get_if<std::pair<int, IRGuard>>(&data);
                hassert (u_g);
                ss << "guard: " << (*u_g).first;
            }
            break;
            case NodeType::SelPhi: {
                auto u_phi = std::get_if<Block::Variant_Select::PhiMerge>(&data);
                hassert(u_phi);
                ss << strofid((*u_phi).post_id) << " = phi(";
                bool first = true;
                for (const auto &id : (*u_phi).branch_ids) {
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
                auto u_phi_inv = std::get_if<Block::Variant_Select::PhiSplit>(&data);
                hassert(u_phi_inv);
                for (const auto &id : (*u_phi_inv).branch_ids) {
                    if (!first) ss << ", ";
                    ss << strofid(id);
                    first = false;
                }
                ss << ") = phi_inv(" << strofid((*u_phi_inv).pre_id) <<")";
            }
            break;
            case NodeType::LoopLoopPhi: {
                auto u_llp = std::get_if<Block::Variant_DoLoop::LoopPhi>(&data);
                hassert(u_llp);
                ss << "(" << strofid((*u_llp).post_id) << ", " << strofid((*u_llp).bodyin_id) 
                    << ") = phiL(" << strofid((*u_llp).pre_id) << ", " << strofid((*u_llp).bodyout_id) << ")";
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
static const DFG_Node bot(NodeId(-1));

// TODO: Wrap CompId in its own class for type-safety
using CompId = int;
/*
    Class implementing the data-dependence graph.
    @param nodes Vector of DFG nodes
    @param adj Adjancency list encoding edges between DFG nodes
    @param sccs Map from `NodeId` to its strongly-connected component ID
    @param sccs_toposorted SCC IDs in topological order
    @param vardefmap Map from `VarId` to the `NodeId` where that var is defined 
*/
class DFG {
    public:
        std::vector<std::unique_ptr<DFG_Node>> nodes;
        std::unordered_map<NodeId,std::unordered_set<NodeId>> adj;
        std::unordered_map<NodeId, CompId> sccs;
        std::vector<CompId> sccs_toposorted;
        std::unordered_map<VarId, NodeId> vardefmap;
        bool sccs_built;

        NodeIdGenerator idgen;

        DFG ()
        : nodes(), adj(), sccs(), sccs_toposorted(), 
          vardefmap(), sccs_built(false), idgen()
        {}

        /*
            Clear the DFG.
        */
        void clear () {
            nodes.clear();
            adj.clear();
            sccs.clear();
            sccs_toposorted.clear();
            vardefmap.clear();
            idgen.reset();
            sccs_built = false;
        }

        NodeId gen_id () { return idgen.generate(); }

        DFG clone () {
            auto ret = DFG();
            for ( const auto &nn : nodes ) {
                ret.nodes.push_back(std::make_unique<DFG_Node> ( std::move((*nn).clone())) );
            }
            ret.adj = adj;
            ret.sccs = sccs;
            ret.sccs_toposorted = sccs_toposorted;
            ret.vardefmap = vardefmap;
            ret.sccs_built = sccs_built;
            return std::move(ret);
        }

        DFG(const DFG&) = delete;
        DFG& operator=(const DFG&) = delete;
        DFG(DFG&&) noexcept = default;
        DFG& operator=(DFG&&) noexcept = default;

        /*
            Add a given node to the DFG.
        */
        void add_node (DFG_Node n) {
            nodes.push_back(std::make_unique<DFG_Node> (std::move(n)));
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
        std::unordered_set<Edge> get_out_edges1 (NodeId from) const {
            Assert (contains(from), "invalid from node");
            auto dests = adj.at(from);
            std::unordered_set<Edge> ret = {};
            for ( const auto &d : dests ) ret.insert({from, d});
            return ret;
        }

        /*
            Get all incoming edges to a node
        */
        std::unordered_set<NodeId> get_in_edges (NodeId to) const {
            std::unordered_set<NodeId> ret = {};
            Assert (contains(to), "invalid to node");
            for ( const auto &[x,y] : adj ) {
                if (y.count(to)) { ret.insert(x); }
            }
            return ret;
        }
        std::unordered_set<Edge> get_in_edges1 (NodeId to) const {
            Assert (contains(to), "invalid from node");
            std::unordered_set<Edge> ret = {};
            for ( const auto &[x,y] : adj ) {
                if (y.count(to)) { ret.insert({x,to}); }
            }
            return ret;
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
                    && (n1->pll_phi_inv().pre_id == ps.pre_id) && 
                    (n1->pll_phi_inv().branch_ids == ps.branch_ids) ) 
                        return *n1;
            }
            return bot;
        }
        const DFG_Node &find (const Block *b, const Block::Variant_Par::PhiMerge &pm) const {
            hassert (b->type()==BlockType::Par);
            for ( const auto &n1 : nodes ) {
                if ( b==(n1->b) && (n1->t == NodeType::PllPhi) 
                    && (n1->pll_phi().post_id == pm.post_id) && 
                    (n1->pll_phi().branch_ids == pm.branch_ids) ) 
                        return *n1;
            }
            return bot;
        }
        const DFG_Node &find (const Block *b, const Block::Variant_Select::PhiSplit &ps) const {
            hassert (b->type()==BlockType::Select);
            for ( const auto &n1 : nodes ) {
                if ( b==(n1->b) && (n1->t == NodeType::SelPhiInv) 
                    && (n1->phi_inv().pre_id == ps.pre_id) && 
                    (n1->phi_inv().branch_ids == ps.branch_ids) ) 
                        return *n1;
            }
            return bot;
        }
        const DFG_Node &find (const Block *b, const Block::Variant_Select::PhiMerge &pm) const {
            hassert (b->type()==BlockType::Select);
            for ( const auto &n1 : nodes ) {
                if ( b==(n1->b) && (n1->t == NodeType::SelPhi) 
                    && (n1->phi().post_id == pm.post_id) && 
                    (n1->phi().branch_ids == pm.branch_ids) ) 
                        return *n1;
            }
            return bot;
        }
        const DFG_Node &find (const Block *b, const std::pair<int, IRGuard> &g) const {
            hassert (b->type()==BlockType::Select);
            for ( const auto &n1 : nodes ) {
                if ( b==(n1->b) && (n1->t == NodeType::Guard) 
                    && n1->g().first == g.first ) 
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
                const auto &node = find(x.first);
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
            CompId i=0;
            for ( const auto &comp : comps ) {
                for ( const auto n : comp ) {
                    sccs[n]=i;
                }
                i++;
            }
            toposort_scc_ids();
            sccs_built = true;
        }

        void toposort_scc_ids() {
            // 1) Build condensation DAG: comp u -> comp v when there exists edge u->v with comp(u)!=comp(v).
            std::unordered_map<CompId, std::unordered_set<CompId>> dag;
            dag.reserve(sccs.size());
            // Also collect the set of all component ids present.
            std::unordered_set<CompId> comps;
            comps.reserve(sccs.size());
            for (const auto& [node, cid] : sccs) comps.insert(cid);

            for (const auto& [u, nbrs] : adj) {
                auto it_u = sccs.find(u);
                Assert (it_u != sccs.end(), "hmm1");
                CompId cu = it_u->second;

                for (const auto& v : nbrs) {
                    auto it_v = sccs.find(v);
                    Assert (it_v != sccs.end(), "hmm2");
                    CompId cv = it_v->second;
                    if (cu == cv) continue; // ignore intra-SCC edges
                    dag[cu].insert(cv);     // use set to avoid parallel edges
                    if (!dag.count(cv)) dag.emplace(cv, std::unordered_set<CompId>{});
                }
                // ensure cu appears as key even if it has no outgoing edges
                if (!dag.count(cu)) dag.emplace(cu, std::unordered_set<CompId>{});
            }
            // Some SCCs might be completely isolated (no edges in adj). Add them.
            for (CompId c : comps) {
                if (!dag.count(c)) dag.emplace(c, std::unordered_set<CompId>{});
            }
            // 2) Kahn’s algorithm on DAG of component ids
            std::unordered_map<CompId, int> indeg;
            indeg.reserve(dag.size() * 2);
            for (const auto& [c, _] : dag) indeg[c] = 0;
            for (const auto& [c, outs] : dag)
                for (CompId d : outs) ++indeg[d];

            std::vector<CompId> zero;
            zero.reserve(indeg.size());
            for (const auto& [c, d] : indeg) if (d == 0) zero.push_back(c);

            std::vector<CompId> order; order.reserve(indeg.size());
            while (!zero.empty()) {
                CompId c = zero.back(); zero.pop_back();
                order.push_back(c);
                for (CompId d : dag[c]) {
                    int& deg = indeg[d];
                    if (--deg == 0) zero.push_back(d);
                }
            }
            Assert((order.size() == indeg.size()), "Cycle in SCC-Graph?!"); 
            sccs_toposorted = order;
        }

        std::vector<CompId> get_sccs_topo () const { return sccs_toposorted; }

        /*
            Find the SCC ID of a given node_id
        */
        CompId find_scc_id (NodeId n1) const
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
        std::unordered_set<NodeId> find_scc (NodeId n1) const
        {
            std::unordered_set<NodeId> ret = {};
            auto scc_id = find_scc_id (n1);
            return find_scc(scc_id);
        }
        std::unordered_set<NodeId> find_scc (CompId ci) const
        {
            std::unordered_set<NodeId> ret = {};
            for ( const auto &x : sccs ) {
                if (x.second==ci) ret.insert(x.first);
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

#endif