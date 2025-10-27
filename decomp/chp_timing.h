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

#ifndef __CHP_TIMING_H__
#define __CHP_TIMING_H__

#include <act/chp/chp_cost.h>
#include <act/expr_cache.h>
#include <act/chp/ddg.h>
#include <act/chp/ir-expr-act-conversion.h>


struct G_TG {};
using TimingNodeId = NodeIdT<G_TG>;
using TimingNodeIdGenerator = NodeIdGeneratorT<G_TG>;

template <> struct std::hash<TimingNodeId> {
    size_t operator()(const TimingNodeId &obj) const {
        return hash<int>()(obj.get_raw());
    }
};

class TimingNode {
    public:
        TimingNode (int p, TimingNodeId _id) {
            id = _id;
            label = "<None>";
            pid = p;
        }
        
        TimingNode (int p, std::string s, TimingNodeId _id) {
            id = _id;
            label = s;
            pid = p;
        }
        
        TimingNodeId id;
        std::string label;
        int pid;
};

class TimingEdge {
    public:
        TimingNodeId from;
        TimingNodeId to;
        double weight;
        bool ticked;
};

class TimingGraph {
    public:
        TimingGraph() {
            nodes.clear();
            edges.clear();
            idgen.reset();
        }

        TimingNodeId gen_id () { return idgen.generate(); }

        TimingNodeId add_node(int p) {
            nodes.emplace_back(TimingNode(p, gen_id()));
            return nodes.back().id;
        }

        TimingNodeId add_node(int p, std::string s) {
            nodes.push_back(TimingNode(p,s, gen_id()));
            return nodes.back().id;
        }

        void add_edge(TimingNodeId u, TimingNodeId v, double w, bool tick) {
            edges.push_back({u,v,w,tick});
        }

        const std::vector<TimingNode>& get_nodes() const { return nodes; }
        const std::vector<TimingEdge>& get_edges() const { return edges; }

        const TimingNode &find (const TimingNodeId id) const {
            for ( const auto &n : nodes ) {
                if (n.id==id)
                    return n;
            }
            Assert (false, "Invalid Node");
            return nodes[0];
        }

        TimingEdge find_edge(const TimingNodeId& from, const TimingNodeId& to) {
            auto it = std::find_if(edges.begin(), edges.end(),
                    [&](const TimingEdge& e) 
                    { return e.from == from && e.to == to; });
            Assert (it!=edges.end(), "Edge not found!");
            return *it;
        }

    private:
        std::vector<TimingNode> nodes;
        std::vector<TimingEdge> edges;
        TimingNodeIdGenerator idgen;
};

// ---------- Internal Use Only ----------
struct RawEdge {
    int u, v;   // endpoints on original graph [0..n-1]
    double w;   // weight
    int t;      // tick (0 or 1)
};

struct RawExpandedEdge {
    int u, v;   // endpoints on expanded graph [0..2n-1]
    double w;   // transformed weight (w - lambda * t)
};

struct RawResult {
    double ratio;  // maximum mean value
    std::vector<int> cycle; // one witness cycle, possibly empty if none
};
// ---------- Internal Use Only ----------

struct TimingResult {
    double ratio;  
    std::vector<TimingNodeId> cycle; 
    std::vector<TimingEdge> edges;
    int n_ticks;
};

typedef std::unordered_map<ChanId, std::pair<TimingNodeId, TimingNodeId>> chan_to_nodes;

class ChpTiming : public ChpCost {
    public:

        ChpTiming (const GraphWithChanNames &g_in, const DFG &dfg_in, Scope *s_in)
        : ChpCost (s_in) {
            g = &g_in;
            dfg = &dfg_in;
            tg = TimingGraph();
            id_to_idx = {};
            idx_to_id = {};
            pctr = 0;
            construct_tg();
            run_maxcycle();
        }

        const GraphWithChanNames *g;
        const DFG *dfg;
        std::unordered_map<TimingNodeId, std::vector<const DFG_Node *>> nmap;

        void construct_tg();
        void _construct_tg(Sequence, var_to_actvar&, chan_to_nodes&, chan_to_nodes&);
        TimingNodeId _construct_subtg(Sequence, TimingNodeId, var_to_actvar&, 
                            chan_to_nodes&, chan_to_nodes&, int);

        void export_dot(std::string);
        void print_result(FILE *);

        void run_maxcycle ();
        TimingResult get_maxcycle () const ;
        RawResult run_max_ratio();
        
        TimingGraph tg;
        TimingResult maxcycle;

        int pctr; // process counter - for prettier dot graph

        std::unordered_map<TimingNodeId,int> id_to_idx;
        std::unordered_map<int,TimingNodeId> idx_to_id;

};

#endif