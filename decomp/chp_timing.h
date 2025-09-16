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
using TNodeId = NodeIdT<G_TG>;

template <> struct std::hash<TNodeId> {
    size_t operator()(const TNodeId &obj) const {
        return hash<int>()(obj.get_raw());
    }
};

class TimingNode {
    public:
        TimingNode () {
            id = TNodeId::generate();
        }
        
        TNodeId id;
};

class TimingEdge {
    public:
        TNodeId from;
        TNodeId to;
        double weight;
        bool ticked;
};

class TimingGraph {
    public:
        TimingGraph() {
            nodes.clear();
            edges.clear();
        }

        TNodeId add_node() {
            nodes.emplace_back();
            return nodes.back().id;
        }

        void add_edge(TNodeId u, TNodeId v, double w, bool tick) {
            edges.push_back({u,v,w,tick});
        }

        const std::vector<TimingNode>& get_nodes() const { return nodes; }
        const std::vector<TimingEdge>& get_edges() const { return edges; }

    private:
        std::vector<TimingNode> nodes;
        std::vector<TimingEdge> edges;
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

typedef std::unordered_map<ChanId, std::pair<TNodeId, TNodeId>> chan_to_nodes;

class ChpTiming : public ChpCost {
    public:

        ChpTiming (GraphWithChanNames &g_in, Scope *s_in)
        : ChpCost (s_in) {
            g = &g_in;
            tg = TimingGraph();
            id_to_idx = {};
            idx_to_id = {};
        }

        ~ChpTiming () {
            if (eeo) {
                eeo->~ExprCache();
                eeo = NULL;
            }
        }

        GraphWithChanNames *g;

        void construct_tg();
        TNodeId _construct_tg(Sequence, TNodeId, var_to_actvar&, 
                            chan_to_nodes&, chan_to_nodes&, 
                            int);

        std::vector<TNodeId> get_maxcycle ();
        RawResult run_max_ratio(const TimingGraph&);

        TimingGraph tg;

        std::unordered_map<TNodeId,int> id_to_idx;
        std::unordered_map<int,TNodeId> idx_to_id;

};

#endif