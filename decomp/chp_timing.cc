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

#include "chp_timing.h"
#include "math.h"

static constexpr double EPS = 1e-12;

void ChpTiming::_construct_tg(Sequence seq, var_to_actvar &table, 
                    chan_to_nodes &c2n_recv, chan_to_nodes &c2n_send)
{
    Block *curr = seq.startseq->child();
    while (curr->type() != BlockType::EndSequence) {
    switch (curr->type()) {
    case BlockType::Basic:
    break;
      
    case BlockType::Par: {
        for (auto &branch : curr->u_par().branches) {
            _construct_tg(branch, table, c2n_recv, c2n_send);
        }
    }
    break;
      
    case BlockType::Select:
    break;
      
    case BlockType::DoLoop: {
        TNodeId tmp;
        _construct_subtg (seq, tmp, table, c2n_recv, c2n_send, 1);
    }
    break;
    
    case BlockType::StartSequence:
    case BlockType::EndSequence:
        hassert(false);
        break;
    }
    curr = curr->child();
    }
}

void ChpTiming::construct_tg()
{
    chan_to_nodes c2n_recv = {};
    chan_to_nodes c2n_send = {};
    var_to_actvar table(_s, g->graph.id_pool());
    _construct_tg(g->graph.m_seq, table, c2n_recv, c2n_send);

    // connect internal channels
    // terminate port channels with sources/sinks
    std::unordered_set<ChanId> c_i = {}; // internal
    std::unordered_set<ChanId> c_s = {}; // sends
    std::unordered_set<ChanId> c_r = {}; // recvs
    for ( auto x : c2n_recv ) {
        if (c2n_send.count(x.first)) {
            c_i.insert(x.first);
        }
        else {
            c_r.insert(x.first);
        }
    }
    for ( auto x : c2n_send ) {
        if (!c2n_recv.count(x.first)) {
            c_s.insert(x.first);
        }
    }

    for ( auto cc : c_i ) {
        auto sendnodes = c2n_send[cc];
        auto recvnodes = c2n_recv[cc];
        tg.add_edge(sendnodes.first, recvnodes.first, 0, false);
        tg.add_edge(recvnodes.second, sendnodes.second, 0, false);
    }
    for ( auto cc : c_r ) {
        auto recvnodes = c2n_recv[cc];
        // FIXME : Correct delay
        tg.add_edge(recvnodes.second, recvnodes.first, 1, true);
    } 
    for ( auto cc : c_s ) {
        auto sendnodes = c2n_send[cc];
        // FIXME : Correct delay
        auto snk_term_req = tg.add_node("snk_term.r");
        auto snk_term_ack = tg.add_node("snk_term.a");
        tg.add_edge(snk_term_req, snk_term_ack, capture_delay, false);
        tg.add_edge(snk_term_ack, snk_term_req, 1, true);
        tg.add_edge(sendnodes.first, snk_term_req, 0, false);
        tg.add_edge(snk_term_ack, sendnodes.second, 0, false);
    } 
    
}

TNodeId ChpTiming::_construct_subtg(Sequence seq, TNodeId previd, var_to_actvar &table, 
                        chan_to_nodes &c2n_recv, chan_to_nodes &c2n_send, int root)
{
  auto varToId = [&] (const VarId &v) { return table.varMap (v); };
  auto chanToId = [&] (const ChanId &v) { return table.chanMap (v); };

    TNodeId currid = previd;
    Block *curr = seq.startseq->child();

    while (curr->type() != BlockType::EndSequence) {
    switch (curr->type()) {
    case BlockType::Basic: if (root==0) {
        switch (curr->u_basic().stmt.type()) {
        case StatementType::Receive: {
            auto chan = curr->u_basic().stmt.u_receive().chan;
            auto reqid = tg.add_node(std::to_string(chan.m_id)+"?.r");
            auto ackid = tg.add_node(std::to_string(chan.m_id)+"?.a");
            nmap[reqid] = &dfg->find(curr);
            nmap[ackid] = &dfg->find(curr);
            Assert (!c2n_recv.count(curr->u_basic().stmt.u_receive().chan), "Multi Chan Access?");
            c2n_recv[curr->u_basic().stmt.u_receive().chan] = {reqid,ackid};
            tg.add_edge(currid, reqid, recv_delay, 0);
            tg.add_edge(reqid, ackid, capture_delay, 0);
            currid = ackid;
        }
        break;
        case StatementType::Send: {
            auto chan = curr->u_basic().stmt.u_send().chan;
            auto reqid = tg.add_node(std::to_string(chan.m_id)+"!.r");
            auto ackid = tg.add_node(std::to_string(chan.m_id)+"!.a");
            nmap[reqid] = &dfg->find(curr);
            nmap[ackid] = &dfg->find(curr);
            Assert (!c2n_send.count(chan), "Multi Chan Access?");
            c2n_send[chan] = {reqid,ackid};
            Expr *e = ChpOptimize::template_func_new_expr_from_irexpr (
                        *curr->u_basic().stmt.u_send().e.m_dag.roots[0],
						ActExprIntType::Int, varToId, chanToId);
            auto edel = expr_delay(e, g->graph.id_pool().getBitwidth(chan));
            tg.add_edge(currid, reqid, send_delay + edel, 0);
            currid = ackid;
        }
        break;
        case StatementType::Assign: {
            auto ids = curr->u_basic().stmt.u_assign().ids;
            Assert (ids.size()==1, "assignments unsplit");
            Expr *e = ChpOptimize::template_func_new_expr_from_irexpr ( 
                        *curr->u_basic().stmt.u_assign().e.roots[0], 
                        ActExprIntType::Int, varToId, chanToId);
            auto edel = expr_delay(e, g->graph.id_pool().getBitwidth(ids[0]));
            auto assnid = tg.add_node("assn");
            nmap[assnid] = &dfg->find(curr);
            tg.add_edge(currid, assnid, assn_delay + capture_delay + edel, 0);
            currid = assnid;
        }
        break;
        }
    }
    break;
      
    case BlockType::Par: {
        auto split_id = tg.add_node("par_split");
        auto merge_id = tg.add_node("par_merge");
        tg.add_edge(currid, split_id, 0, 0);
        int n = curr->u_par().branches.size();
        for (auto &branch : curr->u_par().branches) {
            auto tmpid = tg.add_node();
            tg.add_edge(split_id, tmpid, n, 0);
            auto finalid = _construct_subtg (branch, tmpid, table, c2n_recv, c2n_send, 0);
            tg.add_edge(finalid, merge_id, n, 0);
        }
        currid = merge_id;
    }
    break;
      
    case BlockType::Select: {
        auto split_id = tg.add_node("sel_split");
        auto merge_id = tg.add_node("sel_merge");
        tg.add_edge(currid, split_id, 0, 0);
        double max_delay_g = 0;
        int n = curr->u_select().branches.size();
        for ( auto &branch : curr->u_select().branches ) {
            auto &g = branch.g;
            Expr *e = ChpOptimize::template_func_new_expr_from_irexpr (
                        *branch.g.u_e().e.m_dag.roots[0],
                        ActExprIntType::Bool, varToId, chanToId);
            auto delay = expr_delay(e, 1);
            max_delay_g = std::max(delay, max_delay_g);
        }
        int i=0;
        for ( auto &branch : curr->u_select().branches ) {
            auto tmpid = tg.add_node();
            nmap[tmpid] = &dfg->find(curr, {i,IRGuard::deep_copy(branch.g)});
            // FIXME : Correct delay
            tg.add_edge(split_id, tmpid, max_delay_g, 0);
            auto finalid = _construct_subtg (branch.seq, tmpid, table, c2n_recv, c2n_send, 0);
            // FIXME : Correct delay
            tg.add_edge(finalid, merge_id, n, 0);
            i++;
        }
        auto itr = merge_id;
        for ( auto &phi : curr->u_select().merges ) {
            auto tmpid = tg.add_node("phi");
            tg.add_edge(itr, tmpid, 0, 0);
            Assert (dfg->find(curr, phi).id.get_raw() != bot_id.get_raw(), "what");
            nmap[tmpid] = &dfg->find(curr, phi);
            itr = tmpid;
        }
        currid = itr;
        // currid = merge_id;
    }
    break;
      
    case BlockType::DoLoop: {
        auto start = tg.add_node("Start");
        auto end = tg.add_node("End");
        tg.add_edge(end, start, 1, true);
        auto final = _construct_subtg(curr->u_doloop().branch, start, table, c2n_recv, c2n_send, 0);
        tg.add_edge(final, end, 0, false);
    }
    break;
    
    case BlockType::StartSequence:
    case BlockType::EndSequence:
        hassert(false);
        break;
    }
    curr = curr->child();
    }
    return currid;
}

// Build expanded 2-layer edges for a given lambda.
static std::vector<RawExpandedEdge> build_expanded_edges(int n, const std::vector<RawEdge>& E, double lambda) 
{
    std::vector<RawExpandedEdge> X;
    X.reserve(E.size() * 2);
    for (const auto& e : E) {
        double wprime = e.w - lambda * double(e.t);
        if (e.t == 0) {
            // stay in same layer
            X.push_back({e.u,       e.v,       wprime}); // 0->0
            X.push_back({e.u + n,   e.v + n,   wprime}); // 1->1
        } else {
            // ticked: move to/within layer 1
            X.push_back({e.u,       e.v + n,   wprime}); // 0->1
            X.push_back({e.u + n,   e.v + n,   wprime}); // 1->1
        }
    }
    return X;
}

// Bellman–Ford style test: does any positive cycle exist in layer 1?
static bool has_positive_cycle(int n, const std::vector<RawExpandedEdge>& X) 
{
    int N = 2 * n;
    std::vector<double> dist(N, 0.0);
    bool updated = false;

    for (int i = 0; i < N; ++i) {
        updated = false;
        for (const auto& e : X) {
            if (dist[e.u] + e.w > dist[e.v] + 1e-15) {
                dist[e.v] = dist[e.u] + e.w;
                updated = true;
            }
        }
        if (!updated) break;
    }
    if (!updated) return false; // no positive cycles anywhere

    // If still relaxable, there is a positive cycle; ensure it touches layer 1.
    for (const auto& e : X) {
        if (dist[e.u] + e.w > dist[e.v] + 1e-15) {
            if (e.v >= n) return true; // cycle affects a node in layer 1
        }
    }
    return false;
}

// Try to reconstruct one cycle at (or just below) the optimal lambda.
static std::vector<int> reconstruct_cycle(int n, const std::vector<RawExpandedEdge>& X) 
{
    int N = 2 * n;
    std::vector<double> dist(N, 0.0);
    std::vector<int> prev(N, -1);
    bool updated = false;

    for (int i = 0; i < N; ++i) {
        updated = false;
        for (const auto& e : X) {
            if (dist[e.u] + e.w > dist[e.v] + 1e-12) {
                dist[e.v] = dist[e.u] + e.w;
                prev[e.v] = e.u;
                updated = true;
            }
        }
        if (!updated) break;
    }
    if (!updated) return {}; // nothing to reconstruct

    int cyc = -1;
    for (const auto& e : X) {
        if (dist[e.u] + e.w > dist[e.v] + 1e-12) {
            if (e.v >= n) { cyc = e.v; break; } // prefer layer-1
        }
    }
    if (cyc == -1) return {};

    // Walk back N steps to enter the cycle.
    for (int i = 0; i < N; ++i) {
        cyc = prev[cyc];
        if (cyc == -1) return {};
    }

    // Record cycle by following predecessors until we revisit a node.
    std::vector<int> order;
    std::vector<int> seen(N, -1);
    int cur = cyc;
    while (seen[cur] == -1) {
        seen[cur] = (int)order.size();
        order.push_back(cur);
        cur = prev[cur];
        if (cur == -1) return {};
    }
    int s = seen[cur];
    std::vector<int> cyc_nodes(order.begin() + s, order.end());

    // Keep only layer-1 vertices and map back to original ids.
    std::vector<int> cycle_orig;
    cycle_orig.reserve(cyc_nodes.size());
    for (int v : cyc_nodes) {
        if (v >= n) cycle_orig.push_back(v - n);
    }
    // Put in forward order
    reverse(cycle_orig.begin(), cycle_orig.end());
    if (!cycle_orig.empty() && cycle_orig.front() == cycle_orig.back())
        cycle_orig.pop_back();
    return cycle_orig;
}

RawResult max_tick_ratio_cycle(int n, const std::vector<RawEdge>& E) 
{
    // Binary search for lambda*: largest lambda s.t. there exists a positive cycle in layer 1
    double L = 0.0;
    double maxW = 0.0;
    for (auto& e : E) maxW = std::max(maxW, e.w);
    double U = maxW * std::max(1, n); // generous upper bound

    for (int it = 0; it < 80; ++it) {
        double mid = (L + U) / 2.0;
        auto X = build_expanded_edges(n, E, mid);
        if (has_positive_cycle(n, X)) {
            L = mid;   // mid < optimum
        } else {
            U = mid;
        }
        if (U - L < 1e-9) break;
    }

    // Reconstruct a witness cycle at slightly below L to ensure positivity.
    auto X = build_expanded_edges(n, E, L - 1e-9);
    std::vector<int> cyc = reconstruct_cycle(n, X);

    // Verify the ratio on the found cycle.
    auto cycle_ratio = [&](const std::vector<int>& C) -> double {
        if (C.empty()) return -std::numeric_limits<double>::infinity();
        double num = 0.0, den = 0.0;
        int m = (int)C.size();
        for (int i = 0; i < m; ++i) {
            int a = C[i], b = C[(i+1)%m];
            bool found = false;
            for (const auto& e : E) {
                if (e.u == a && e.v == b) {
                    num += e.w;
                    den += e.t;
                    found = true;
                    break;
                }
            }
            if (!found) return -std::numeric_limits<double>::infinity();
        }
        if (den <= 0.0) return -std::numeric_limits<double>::infinity();
        return num / den;
    };

    double ratio = cycle_ratio(cyc);
    // If reconstruction failed (rare), still report L as the ratio.
    if (!std::isfinite(ratio)) ratio = L;
    return {ratio, cyc};
}

RawResult ChpTiming::run_max_ratio() 
{
    bool printt = false;
    // Map TNodeId -> compact index
    int idx = 0;
    for (auto& n : tg.get_nodes()) {
        idx_to_id[idx] = n.id;
        id_to_idx[n.id] = idx;
        if (printt) fprintf(stdout, "\n Node ID : %d", n.id.get_raw());
        idx++;
    }
    int n_nodes = idx;
    if (printt) fprintf (stdout, "\n Graph Nodes : %d", n_nodes);
    if (printt) fprintf (stdout, "\n Graph Edges : %zu\n\n", tg.get_edges().size());

    std::vector<RawEdge> E;
    for (auto& e : tg.get_edges()) {
        if (printt) fprintf(stdout, "Edge : %d -> %d\n", e.from.get_raw(), e.to.get_raw());
    }
    TNodeId bot;
    for (auto& e : tg.get_edges()) {
        Assert (e.from!=bot && e.to!=bot, "Malformed Timing Graph!");
        int u = id_to_idx.at(e.from);
        int v = id_to_idx.at(e.to);
        E.push_back({u,v,e.weight, e.ticked ? 1 : 0});
    }

    return max_tick_ratio_cycle(n_nodes, E);
}

void ChpTiming::run_maxcycle() 
{
    auto r = run_max_ratio();
    Assert(!r.cycle.empty(), "Critical Cycle Failure! Unticked cycle may exist.");

    std::vector<TNodeId> ret;
    for ( auto id : r.cycle ) {
        ret.push_back(idx_to_id[id]);
    }
    
    std::vector<TimingEdge> cycle_edges;
    int nticks = 0;
    for (size_t i = 0; i < ret.size(); i++) {
        size_t j = (i + 1) % ret.size(); 
        auto e = tg.find_edge(ret[i],ret[j]);
        cycle_edges.push_back(e);
        nticks += int(e.ticked);
    }

    auto it = std::find_if(cycle_edges.begin(), cycle_edges.end(),
                [](const TimingEdge& e){ return e.ticked; });
    std::rotate(cycle_edges.begin(), it, cycle_edges.end());
    std::rotate(cycle_edges.begin(), cycle_edges.begin()+1, cycle_edges.end());
    Assert (cycle_edges.back().ticked, "Huh");

    maxcycle = {r.ratio, ret, cycle_edges, nticks};
}

TimingResult ChpTiming::get_maxcycle() const
{
    return maxcycle;
}

void ChpTiming::export_dot(std::string filename)
{
    FILE *ff = fopen(filename.c_str(), "w");

    std::string edge_repr = "->";
    fprintf(ff,"\ndigraph{ ");
    for ( const auto &node : tg.get_nodes() ) {
        fprintf(ff, "\n_%d [label=\"%d : %s\"];", 
            node.id.get_raw(), node.id.get_raw(), node.label.c_str());
    }
    for ( auto e : tg.get_edges() ) {
        std::string color = (e.ticked) ? "red" : "black";
        fprintf(ff, "\n_%d %s _%d [color=%s];", 
            e.from.get_raw(), edge_repr.c_str(), e.to.get_raw(), color.c_str());
    }
    fprintf(ff,"\n}");
    fclose(ff);
}