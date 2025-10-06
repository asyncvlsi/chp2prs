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

#include "projection.h"

/*
    Projection TODO
    - IC-LCD handling (done)
    - selection handling (done)
    - test interaction with other passes (done)
    - implement cost-based cut (done)
    - expose complex part cleanly for optimizer plug-in (done)
*/

void eliminate_unobservable(ChpGraph &g, Sequence seq);

std::tuple<
    std::unordered_set<ActId *>, 
    act_chp_lang_t *,
    std::vector<std::unordered_map<ChpOptimize::ChanId, ActId *>>
    > Projection::get_result ()
{     
    std::unordered_set<ActId *> names = {};
    std::vector<std::unordered_map<ChpOptimize::ChanId, ActId *>> nfc = {};

    act_chp_lang_t *top_chp = new act_chp_lang_t;
    top_chp->label = NULL;
    top_chp->space = NULL;
    top_chp->type = ACT_CHP_COMMA;
    top_chp->u.semi_comma.cmd = list_new(); 

    for ( auto v : procs )
    {
        auto _g = ChpOptimize::chp_graph_from_act (v, s, 1);
        ChpOptimize::optimize_chp_basic (_g.graph, "brr", false);
        eliminate_unobservable(_g.graph, _g.graph.m_seq);
        ChpOptimize::parallelizeStatements (_g.graph);
        std::vector<ActId *> tmp_names2;
        v = chp_graph_to_act (_g, tmp_names2, s);
        for ( auto x : tmp_names2 ) { names.insert(x); }
        nfc.push_back(_g.name_from_chan);
        // No chans -> can delete process
        // Perhaps a better way to check this
        if (_g.name_from_chan.size()>0) {
            list_append(top_chp->u.semi_comma.cmd, v);
        }
    }

    return {names, top_chp, nfc};
}

std::vector<act_chp_lang_t *> Projection::get_procs ()
{
    return procs;
}

void Projection::step2(GraphWithChanNames &g_in, DFG &d_in)
{
    d_in.clear();
    if (!g_in.graph.is_static_token_form) {
        ChpOptimize::parallelizeStatements(g_in.graph);
        ChpOptimize::putIntoNewStaticTokenForm(g_in.graph);
    }
    _build_graph(g_in.graph.m_seq, d_in);

    // Compute SCCs
    d_in.build_sccs();
}

void Projection::project(Strategy ss)
{
    bool printt = false;

    // split multi-assignments into single
    split_assignments(g->graph);
    
    // compute strongly-connected components info
    step2(*g, dfg1);

    bool skip = false;
    // Copy-insertion strategy
    switch (ss)
    {
    case Strategy::None: {
    }
    break;
    case  Strategy::Heuristic: {
        bool _ins = false;
        _insert_copies_v3 (*g, dfg1, g->graph.m_seq, dfg1.get_wccs().size(), 1, _ins);
    }
    break;
    case Strategy::BruteForce: {
        _insert_copies_v6 (*g, dfg1);
    }
    break;
    case Strategy::Timing: {
        // export_dot("out.dot",dfg1);
        _insert_copies_v7 (*g, dfg1);
        skip = true;
    }
    break;
    }
    // Construct sub-processes
    if (!skip) {
        dfg1.clear();
        _build_graph(g->graph.m_seq, dfg1);
        _build_procs (*g, dfg1);
        
        ChpOptimize::takeOutOfNewStaticTokenForm(g->graph);
    }
}

void Projection::_insert_copies_v7 (GraphWithChanNames &g, DFG &d_in)
{
    bool verbose = false;

    // make copy of graph
    std::unordered_map<ChanId, ChanId> cc;
    std::unordered_map<VarId, VarId> vv;
    auto g_copy = deep_copy_graph(g,cc,vv);
    DFG d_loc;
    step2(g_copy, d_loc);

    std::vector<double> max_cycles_trace = {0.0}; // dummy val
    // ChpTiming xct(g_copy, d_loc, s); xct.export_dot("tg_orig.dot"); xct.print_result(stdout);

    do {
        ChpTiming ct(g_copy, d_loc, s);
        auto r1 = ct.get_maxcycle();
        max_cycles_trace.push_back(r1.ratio);
        if (verbose) { fprintf(stdout, "\n// Latest   Cycle : %.2f", max_cycles_trace.back()); } 
        
        // auto hhvec = _get_candidates_dynamic(ct, 20);
        auto hhvec = _get_candidates_segment(ct);
        HyperEdgeVec best_hs = {}; 
        double itr_best_cycle = r1.ratio;
        if (verbose) { 
            fprintf(stdout, ", Hyperedge set : %zu", hhvec.size()); 
            for (const auto &hset : hhvec) {
                fprintf(stdout, "\n");
                for (const auto &h : hset) {
                    fprintf(stdout, "%d : ", h.first.get_raw());
                    for (const auto &out : h.second) { 
                        fprintf(stdout, "%d, ", out.get_raw());
                    }
                    fprintf(stdout, "\n");
                }
            } 
        } 
        int n_wcc_orig = d_loc.get_wccs().size();

        for ( const auto &hset : hhvec ) {

            std::unordered_map<VarId, VarId> old_to_new = {};
            CopyLocMap clm = {};
            for ( const auto &h : hset ) {
                const auto &node = d_loc.find(h.first);
                auto vars = get_defs(node);
                if (_breakable(node)) {
                    for ( auto x : h.second ) {
                        d_loc.delete_edge (h.first, x);
                    }
                    if (!old_to_new.count(vars[0])) {
                        auto newvar = _insert_hyperedge_copy (g_copy, d_loc, h, vars[0], clm);
                        old_to_new.insert({vars[0],newvar});
                    }
                }
            }
            
            int n_wcc = d_loc.get_wccs().size();
            if (n_wcc > n_wcc_orig) {
                _build_procs(g_copy, d_loc);
                auto [names, top_chp, nfc] = get_result();
                _fill_in_else_explicit (top_chp, s);
                auto g_tmp = chp_graph_from_act (top_chp, s, 1);
                DFG d_tmp;
                ChpOptimize::parallelizeStatements (g_tmp.graph);
                step2(g_tmp, d_tmp);
                ChpTiming ct_tmp(g_tmp, d_tmp, s);
                auto r_tmp = ct_tmp.get_maxcycle();
                if (itr_best_cycle > r_tmp.ratio) {
                    best_hs = hset;
                    itr_best_cycle = r_tmp.ratio;
                }
            }

            for ( const auto &h : hset ) {
                const auto &node = d_loc.find(h.first);
                auto vars = get_defs(node);
                if (_breakable(node)) {
                    for ( auto x : h.second ) {
                        d_loc.add_edge (h.first, x);
                    }
                    if (old_to_new.count(vars[0])) {
                        _uninsert_hyperedge_copy (g_copy, d_loc, h, old_to_new[vars[0]], vars[0], clm);
                        old_to_new.erase(vars[0]);
                    }
                }
            }
        }

        CopyLocMap clm = {};
        for ( const auto &best_h : best_hs ) {
            if (best_h.first!=bot_id) {
                const auto &node = d_loc.find(best_h.first);
                auto vars = get_defs(node);
                if (_breakable(node)) {
                    for ( auto x : best_h.second ) {
                        d_loc.delete_edge (best_h.first, x);
                    }
                    auto newvar = _insert_hyperedge_copy (g_copy, d_loc, best_h, vars[0], clm);
                }
            }
        }

        _build_procs(g_copy, d_loc);
        auto [names, top_chp, nfc] = get_result();
        _fill_in_else_explicit (top_chp, s);
        g_copy = chp_graph_from_act (top_chp, s, 1);
        ChpOptimize::parallelizeStatements (g_copy.graph);
        step2(g_copy, d_loc);
    
    } while ( abs(*(max_cycles_trace.end()-1) - *(max_cycles_trace.end()-2)) >= 0.01 );

    // ChpTiming yct(g_copy, d_loc, s); yct.export_dot("tg_final.dot"); yct.print_result(stdout);

    if (verbose) { fprintf(stdout, "\n\n// Cycle Trace : "); for (auto x:max_cycles_trace) { fprintf(stdout, "%.2f, ", x); } }
    _build_procs(g_copy, d_loc);
}

template <typename T>
static std::vector<std::vector<T>> power_set(const std::unordered_set<T>& s) {
    std::vector<T> elems(s.begin(), s.end());
    std::vector<std::vector<T>> result;
    int n = elems.size();
    Assert (n<=15, "Extremely high out-degree?");
    int total = 1 << n; // 2^n subsets

    result.reserve(total);

    for (int mask = 0; mask < total; ++mask) {
        std::vector<T> subset;
        for (int i = 0; i < n; ++i) {
            if (mask & (1 << i)) {
                subset.push_back(elems[i]);
            }
        }
        result.push_back(std::move(subset));
    }
    return result;
}

template <typename T>
auto union_ = 
[](const std::unordered_set<T>& a, const std::unordered_set<T>& b) {
    std::unordered_set<T> result = a;
    result.insert(b.begin(), b.end());
    return result;
};

// Based on TG, look at the SCC-Graph, in a topological order
// Try prefixes of the topo-sorted list
HyperEdgesVec Projection::_get_candidates_segment(const ChpTiming &ct)
{
    auto r = ct.get_maxcycle();
    auto all_sccs = ct.dfg->get_sccs_topo();
    std::unordered_set<CompId> relevant_sccs = {};

    for ( const auto &x : r.cycle ) {
        for ( auto n : ct.nmap.at(x) ) {
            relevant_sccs.insert(ct.dfg->find_scc_id(n->id));
        }
    }

    all_sccs.erase( std::remove_if(all_sccs.begin(), all_sccs.end(),
                [&](const CompId& val) { return relevant_sccs.count(val) == 0; }),
                all_sccs.end());
    auto relevant_sccs_topo = all_sccs;

    HyperEdgesVec hs = {};
    std::unordered_set<CompId> itr = {};
    // try scc prefixes
    for ( const auto &scc_id : relevant_sccs_topo ) {
        itr.insert(scc_id);

        std::unordered_set<Edge> all_outs = {};
        for ( const auto &si : itr ) {
            auto sccnodes = ct.dfg->find_scc(si);
            for ( const auto &sn : sccnodes ) {
                all_outs = union_<Edge> (all_outs, ct.dfg->get_out_edges1(sn));
            }
        }

        std::unordered_set<Edge> all_outs_filter = {};
        // only edges that cross over from our scc prefix to downstream
        for ( const auto &ee : all_outs ) {
            if (itr.count(ct.dfg->find_scc_id(ee.first)) && !itr.count(ct.dfg->find_scc_id(ee.second))) 
                all_outs_filter.insert(ee);
        }

        HyperEdgeSet tmp = {};
        for (const auto &[u,v] : all_outs_filter) {
            tmp[u].insert(v);
        }
        HyperEdgeVec hh = {};
        for (const auto &[u,v] : tmp ) {
            hh.push_back({u,v});
        }

        // TODO: this can be done better --------
        // auto ps = power_set(all_outs_filter);
        // for ( auto x1 : ps ) {
        //     HyperEdgeSet tmp = {};
        //     for (const auto &[u,v] : x1) {
        //         tmp[u].insert(v);
        //     }
        //     HyperEdgeVec hh = {};
        //     for (const auto &[u,v] : tmp ) {
        //         hh.push_back({u,v});
        //     }
        //     if (!hh.empty())
        //     hs.push_back(hh);
        // }
        // --------------------------------------

        if (!hh.empty())
            hs.push_back(hh);
    }
    return hs;
}

// Try every singleton on the max-cycle
HyperEdgesVec Projection::_get_candidates_all(const ChpTiming &ct)
{
    std::unordered_set<NodeId> cand_nodes;
    auto r = ct.get_maxcycle();
    for ( const auto &x : r.cycle ) {
        for ( auto n : ct.nmap.at(x) ) {
            if (n->t!=NodeType::LoopLoopPhi) {
                cand_nodes.insert(n->id);
            }
            if (n->t==NodeType::Guard) {
                auto children = ct.dfg->get_out_edges(n->id);
                for ( const auto &y : children ) {
                    cand_nodes.insert(ct.dfg->find(y).id);
                }
            }
        }
    }
    HyperEdgesVec hs = {};
    for ( const auto &id : cand_nodes ) {
        auto all_outs = ct.dfg->get_out_edges(id);
        std::unordered_set<NodeId> excl_same_scc = {};
        for ( auto x : all_outs ) {
            if (!ct.dfg->in_same_scc(id, x)) {
                excl_same_scc.insert(x);
            }
        }
        auto pow_set = power_set(excl_same_scc);
        for ( auto outs : pow_set ) {
            hs.push_back({{id,std::unordered_set<NodeId>(outs.begin(),outs.end())}});
        }
    }

    return hs;
}

// Max-Cycle Bisection Heuristic - Singleton
HyperEdgesVec Projection::_get_candidates_bisect(const ChpTiming &ct, double lb, double ub)
{
    auto r = ct.get_maxcycle();

    // find maximum segment and rotate to align
    auto es = r.edges;
    double max_seg_wt = 0.0;
    double seg_wt = 0.0;
    Assert (es.back().ticked, "Last not ticked!");
    std::vector<TimingEdge>::iterator itr = es.begin();
    for (auto it = es.begin(); it != es.end(); it++) {
        seg_wt += (*it).weight;
        if ((*it).ticked) {
            if(max_seg_wt < seg_wt) {
                itr = it;
                max_seg_wt = seg_wt;    
            }
            seg_wt = 0;
        }
    }
    std::rotate(es.begin(), itr, es.end());

    // consider only nodes in neighborhood of midpoint of max-segment
    Assert ((lb>=0 && lb <=1.0), "Huh");
    Assert ((ub>=0 && ub <=1.0), "Huh");
    Assert ((lb<=ub), "Huh");
    seg_wt = 0.0;
    std::vector<TNodeId> cand_tnodes;
    for (auto it = es.begin(); it != es.end(); it++) {
        if ((seg_wt > lb*max_seg_wt) && (seg_wt < ub*max_seg_wt)) {
            cand_tnodes.push_back((*it).from);
        }
        seg_wt += (*it).weight;
        if (seg_wt > ub*max_seg_wt) break; 
    }

    // pick dfg nodes based on timing nodes
    std::unordered_set<NodeId> cand_nodes;
    for ( const auto &x : cand_tnodes ) {
        for ( auto n : ct.nmap.at(x) ) {
            if (n->t!=NodeType::LoopLoopPhi) {
                cand_nodes.insert(n->id);
            }
            if (n->t==NodeType::Guard) {
                auto children = ct.dfg->get_out_edges(n->id);
                for ( const auto &y : children ) {
                    cand_nodes.insert(ct.dfg->find(y).id);
                }
            }
        }
    }

    HyperEdgesVec hs = {};
    for ( const auto &id : cand_nodes ) {
        auto all_outs = ct.dfg->get_out_edges(id);
        std::unordered_set<NodeId> excl_same_scc = {};
        for ( auto x : all_outs ) {
            if (!ct.dfg->in_same_scc(id, x)) {
                excl_same_scc.insert(x);
            }
        }
        auto pow_set = power_set(excl_same_scc);
        for ( auto outs : pow_set ) {
            hs.push_back({{id,std::unordered_set<NodeId>(outs.begin(),outs.end())}});
        }
    }
    return hs;
}

// Dynamic Singleton Selection
HyperEdgesVec Projection::_get_candidates_dynamic(const ChpTiming &ct, int max_sz)
{
    double lb = 0.4;
    double ub = 0.6;
    double del = 0.05;
    auto hs = _get_candidates_bisect(ct, lb, ub);
    while (hs.size()<max_sz && lb>0.0 && ub<1.0) {
        hs = _get_candidates_bisect(ct, lb, ub);
        lb-=del; ub+=del;
    }
    if (hs.size()==0) hs = _get_candidates_all(ct);
    return hs;
}

bool Projection::_breakable (const DFG_Node &n)
{
    std::unordered_set<NodeType> allowed = 
        {NodeType::Basic, NodeType::Guard, 
            NodeType::Copy, NodeType::SelPhi, NodeType::PllPhi};
    auto vars = get_defs(n);
    return (vars.size()==1 && bool(allowed.count(n.t)));
}

/*
    This is the brute-force bit.
    Exponential in no. of SCC edges, at least (lol).
    Checks every possible subset of SCC-edges that can be cut.
    Picks the one with minimum maximum-latency cost
*/
void Projection::_insert_copies_v6 (GraphWithChanNames &g, DFG &d_in)
{
    bool verbose = false;
    build_vardefmap(d_in);  

    // make copy of graph
    std::unordered_map<ChanId, ChanId> cc;
    std::unordered_map<VarId, VarId> vv;
    auto g_copy = deep_copy_graph(g,cc,vv);
    std::unordered_map<VarId, VarId> vv_inv = {};
    for ( const auto &v : vv ) {
        vv_inv.insert({v.second,v.first});
    }
    DFG d_loc;
    step2(g_copy, d_loc);

    // initialize cost-related stuff
    ChpCost c(s);
    double min_max_cost = std::numeric_limits<double>::max();
    HyperEdgeSet best_subset = {};
    std::vector<double> best_costs = {};
    
    std::unordered_map<int, Edge> emap = {};
    int i = 0;
    for ( auto x : d_loc.adj ) {
        for ( auto y : x.second ) {
            if (d_loc.find(x.first).t!=NodeType::LoopLoopPhi
            && d_loc.find(y).t!=NodeType::LoopLoopPhi)
            {
                emap.insert({i,Edge(x.first,y)});
                i++;
            }
        }
    }
    auto n_cand_edges = i;
    unsigned long long max_itr = 1024*1024*1024;
    // unsigned long long max_itr_edge = 32;
    unsigned long long n_itr = std::min(max_itr, 1ULL<<n_cand_edges);

    if (verbose) {
        fprintf(stdout, "\n\n// hyperedge-copy subsets to check: %llu\n", (1ULL<<n_cand_edges));
        fprintf(stdout, "// iteration limit: %llu\n\n", n_itr);
    }

    for (unsigned long long mask=0; mask<n_itr; mask++)
    {
        HyperEdgeSet h_subset = {};
        for (int i = 0; i < n_cand_edges; i++) {
            if (mask & (1 << i)) {
                auto e = emap[i];
                if (!h_subset.count(e.first)) {
                    h_subset.insert({e.first, {}});
                }
                h_subset.at(e.first).insert(e.second);
            }
        }

        // delete hyperedges in this subset - only do single-var defining nodes
        std::unordered_map<VarId, VarId> old_to_new = {};
        CopyLocMap clm = {};
        for ( auto h : h_subset ) {
            const auto &node = d_loc.find(h.first);
            auto vars = get_defs(node);
            if (_breakable(node)) {
                for ( auto x : h.second ) {
                    d_loc.delete_edge (h.first, x);
                }
                if (!old_to_new.count(vars[0])) {
                    auto newvar = _insert_hyperedge_copy (g_copy, d_loc, h, vars[0], clm);
                    old_to_new.insert({vars[0],newvar});
                }
            }
        }

        // build the subprocesses and check what the cost is
        _build_procs(g_copy, d_loc);
        c.clear();
        c.add_procs(procs);
        auto cost = c.get_max_latency_cost();
        if (cost < min_max_cost) {
            min_max_cost = cost;
            best_subset = h_subset;
            best_costs = c.get_latency_costs();
        }

        // add hyperedges back - only do single-var defining nodes
        for ( auto h : h_subset ) {
            const auto &node = d_loc.find(h.first);
            auto vars = get_defs(node);
            if (_breakable(node)) {
                for ( auto x : h.second ) {
                    d_loc.add_edge (h.first, x);
                }
                if (old_to_new.count(vars[0])) {
                    _uninsert_hyperedge_copy (g_copy, d_loc, h, old_to_new[vars[0]], vars[0], clm);
                    old_to_new.erase(vars[0]);
                }
            }
        }
    }

    fprintf(stdout, "\n\n// minimax cost: %lf\n", min_max_cost);
    fprintf(stdout, "\n// vars to copy: ");

        CopyLocMap clm = {};
        for ( const auto &h : best_subset ) {
            // use vardefmap to insert correct copies in original graph and finish
            const auto &node = d_loc.find(h.first);
            auto vars = get_defs(node);
            if (_breakable(node)) {
                Assert (vv_inv.count(vars[0]), "Var not in map");
                auto var_in_old_g = vv_inv[vars[0]];
                fprintf(stdout, "v%llu, ", var_in_old_g.m_id);
                Assert(d_in.vardefmap.count(var_in_old_g), "Var not in vardefmap");
                auto node_id = d_in.vardefmap[var_in_old_g];
                _insert_hyperedge_copy (g, d_in, h, var_in_old_g, clm);
            }
        }
    fprintf(stdout, "\n");
}

VarId Projection::_insert_hyperedge_copy (GraphWithChanNames &gg, const DFG &d_in, 
    HyperEdge h, VarId v, CopyLocMap &clm)
{
    Assert (d_in.contains(h.first), "Node not found");
    for ( auto x : h.second ) {
        Assert (d_in.contains(x), "Node not found");
    }
    if (h.second.empty()) { clm.insert({v, nullptr}); return v; }

    auto b_from = d_in.find(h.first).b;

    ChanId ci = gg.graph.id_pool().makeUniqueChan(gg.graph.id_pool().getBitwidth(v), false);
    var_to_actvar vtoa(s, gg.graph.id_pool());
    ActId *id = vtoa.chanMap(ci);
    gg.name_from_chan.insert({ci, id});
    
    auto send = gg.graph.blockAllocator().newBlock(
        Block::makeBasicBlock(Statement::makeSend(ci, 
        ChpExprSingleRootDag::makeVariableAccess(v, gg.graph.id_pool().getBitwidth(v)))));
    
    VarId copy_var = gg.graph.id_pool().makeUniqueVar(gg.graph.id_pool().getBitwidth(v), false);

    auto recv = gg.graph.blockAllocator().newBlock(
        Block::makeBasicBlock(Statement::makeReceive(ci, copy_var)));

    auto dist_assn = gg.graph.blockAllocator().newBlock(Block::makeParBlock());
    dist_assn->u_par().branches.push_back(gg.graph.blockAllocator().newSequence({send}));
    dist_assn->u_par().branches.push_back(gg.graph.blockAllocator().newSequence({recv}));

    _splice_in_block_between (b_from, b_from->child(), dist_assn);
    
    for ( auto x : h.second ) {
        _replace_use (gg, v, copy_var, d_in.find(x));
    }

    clm.insert({copy_var, dist_assn});

    return copy_var;
}

void Projection::_uninsert_hyperedge_copy (GraphWithChanNames &gg, const DFG &d_in, 
    HyperEdge h, VarId copyvar, VarId origvar, CopyLocMap &clm)
{
    if (copyvar==origvar) return;
    auto b_from = d_in.find(h.first).b;

    Assert (clm.count(copyvar), "what");
    auto dist_assn = clm.at(copyvar);
    Assert (dist_assn->type()==BlockType::Par, "par block");
    Assert (dist_assn->u_par().branches.size()==2, "two par branches");
    auto snd = dist_assn->u_par().branches.front().startseq->child();
    auto rcv = dist_assn->u_par().branches.back().startseq->child();
    Assert (snd->type()==BlockType::Basic && snd->u_basic().stmt.type()==StatementType::Send, "send");
    Assert (rcv->type()==BlockType::Basic && rcv->u_basic().stmt.type()==StatementType::Receive, "recv");
    Assert (rcv->u_basic().stmt.u_receive().var, "No receiving var");
    if (!(*(rcv->u_basic().stmt.u_receive().var)==copyvar)) {
        fprintf(stdout, "\n\ncopyvar: %llu", copyvar.m_id);
        fprintf(stdout, "\n\norigvar: %llu", origvar.m_id);
        print_chp_block (std::cout, dist_assn);
        print_chp(std::cout, gg.graph);
    }
    Assert (*(rcv->u_basic().stmt.u_receive().var)==copyvar, "Not the correct dist_asn block?");
    auto snd_ids = getIdsUsedByExpr(snd->u_basic().stmt.u_send().e);
    Assert (snd_ids.count(origvar), "Not the correct dist_asn block?");

    _splice_out_block (dist_assn);

    for ( auto x : h.second ) {
        Assert (d_in.contains(x), "Node not found");
    }
    for ( auto x : h.second ) {
        _replace_use (gg, copyvar, origvar, d_in.find(x));
    }
}

void Projection::_build_procs (const GraphWithChanNames &gx, DFG &d_in)
{
    procs.clear();

    int num_subgraphs = d_in.get_wccs().size();
    DFG d_loc;
    {
        d_loc.clear();
        _build_graph(gx.graph.m_seq, d_loc);
        num_subgraphs = d_loc.get_wccs().size();
        d_loc.clear();
    }
    std::unordered_set<NodeId> marker_node_ids = {};

    for (int i=0; i<num_subgraphs; i++)
    {
        std::unordered_map<ChanId, ChanId> cc;
        std::unordered_map<VarId, VarId> vv;
        auto g1 = deep_copy_graph(gx,cc,vv);
        Assert (g1.graph.is_static_token_form, "Input graph not in STF");

        d_loc.clear();
        _build_graph(g1.graph.m_seq, d_loc);
        auto tmp_sgs = d_loc.get_wccs();
        auto itr = tmp_sgs.begin();
        while ((marker_node_ids.count((*itr).second[0]))) 
        { itr++; }
        hassert (itr != tmp_sgs.end());
        marker_node_ids.insert((*itr).second[0]);

        if (_all_basic(d_loc, (*itr).second)) {
            _build_basic_new (g1, d_loc, (*itr).second);
        }
        else {
            std::unordered_set<NodeId> tmp ((*itr).second.begin(), (*itr).second.end());
            _build_sub_proc_new (g1, d_loc, g1.graph.m_seq, tmp);
        }

        ChpOptimize::takeOutOfNewStaticTokenForm(g1.graph);
        std::vector<ActId *> tmp_names2;
        act_chp_lang_t *tmpact = chp_graph_to_act (g1, tmp_names2, s);
        procs.push_back(tmpact);

        // fprintf(stdout, "\n\n// num_subgraphs: %d, tmp_sgs: %d\n\n", num_subgraphs, int(tmp_sgs.size()));
        hassert (num_subgraphs == tmp_sgs.size());
    }
    hassert (marker_node_ids.size() == num_subgraphs);
}

bool Projection::_build_sub_proc_new (GraphWithChanNames &gg, const DFG &d_in, Sequence seq, std::unordered_set<NodeId> &s)
{
    bool empty = true;
    Block *curr = seq.startseq->child();
    while (curr->type() != BlockType::EndSequence) {
    switch (curr->type()) {
    case BlockType::Basic: {
        const auto &dfgnode = d_in.find(curr);
        if (dfgnode && s.count(dfgnode.id)) {
            s.erase(dfgnode.id);
            empty = false;
        }
        else {
            curr->dead = true;
            curr = _splice_out_block (curr);
            curr = curr->parent();
        }
    }
    break;
      
    case BlockType::Par: {

        std::vector<Block::Variant_Par::PhiSplit> new_splits = {};
        for (auto phi_inv : curr->u_par().splits) {
            const auto &dfgnode = d_in.find(curr, phi_inv);
            if (dfgnode && s.count(dfgnode.id)) {
                s.erase(dfgnode.id);
                new_splits.push_back(phi_inv);
                empty = false;
            }
        }
        curr->u_par().splits = new_splits;
        
        for (auto &branch : curr->u_par().branches) {
            empty &= _build_sub_proc_new (gg, d_in, branch, s);
        }

        std::vector<Block::Variant_Par::PhiMerge> new_merges = {};
        for (auto phi : curr->u_par().merges) {
            const auto &dfgnode = d_in.find(curr, phi);
            if (dfgnode && s.count(dfgnode.id)) {
                s.erase(dfgnode.id);
                new_merges.push_back(phi);
                empty = false;
            }
        }
        curr->u_par().merges = new_merges;

    }
    break;
      
    case BlockType::Select: {

        /*
            Entire projected proces could be within a selection.
            Need to lift up the branch in this case.
            This happens when there is no guard node of this
            selection in the set of DFG nodes.
        */
        bool lift = true;
        for ( const auto &n : s ) {
            if (d_in.find(n).t==NodeType::Guard && d_in.find(n).b == curr) {
                lift = false; break;
            }
        }

        std::vector<Block::Variant_Select::PhiSplit> new_splits = {};
        for (auto phi_inv : curr->u_select().splits) {
            const auto &dfgnode = d_in.find(curr, phi_inv);
            if (dfgnode && s.count(dfgnode.id)) {
                s.erase(dfgnode.id);
                new_splits.push_back(phi_inv);
                empty = false;
            }
        }
        curr->u_select().splits = new_splits;

        std::list<SelectBranch> new_branches = {};

        int i=0;
        int orig_size = curr->u_select().branches.size();
        bool all_empty = true;
        for ( auto &branch : curr->u_select().branches ) {
            bool emp = _build_sub_proc_new (gg, d_in, branch.seq, s);

            const auto &dfgnode = d_in.find(curr, {i, IRGuard::deep_copy(branch.g)});
            if (!emp || (dfgnode && s.count(dfgnode.id))) {
                s.erase(dfgnode.id);
                new_branches.push_back({branch.seq, IRGuard::deep_copy(branch.g)});
                all_empty = false;
            }
            i++;
        }

        if (lift) {
            hassert (new_branches.size()<=1);
        }

        if (new_branches.size()<orig_size) {
            if (new_branches.empty()) {
                new_branches.push_back({gg.graph.blockAllocator().newSequence({}), 
                    IRGuard::makeExpression(ChpExprSingleRootDag::makeConstant(ChpOptimize::BigInt(1), 1))});
            }
            new_branches.push_back({gg.graph.blockAllocator().newSequence({}), IRGuard::makeElse()});
            curr->u_select().branches.clear();
            for (auto &x : new_branches) {
                curr->u_select().branches.push_back({x.seq, IRGuard::deep_copy(x.g)});
            }
        }

        std::vector<Block::Variant_Select::PhiMerge> new_merges = {};
        for (auto phi : curr->u_select().merges) {
            const auto &dfgnode = d_in.find(curr, phi);
            if (dfgnode && s.count(dfgnode.id)) {
                s.erase(dfgnode.id);
                new_merges.push_back(phi);
                empty = false;
            }
        }
        curr->u_select().merges = new_merges;

        if (lift) {
            Block *newcurr = gg.graph.blockAllocator().newBlock(Block::makeParBlock());
            newcurr->u_par().branches.push_back(new_branches.front().seq);
            _splice_in_block_between(curr->parent(),curr,newcurr);
            _splice_out_block_new(curr);
            curr = newcurr;
        }
    }
    break;
    case BlockType::DoLoop: {
        empty = _build_sub_proc_new(gg, d_in, curr->u_doloop().branch, s);

        curr->u_doloop().guard = (ChpExprSingleRootDag::of_expr(
                                        ChpExpr::makeConstant(ChpOptimize::BigInt{1}, 1)));
    }
    break;
    
    case BlockType::StartSequence:
    case BlockType::EndSequence:
        hassert(false);
        break;
    }
    curr = curr->child();
    }
    return empty;
}

void Projection::_build_basic_new (GraphWithChanNames &gg, const DFG &d_in, std::vector<NodeId> nodes)
{
    std::vector<Block *> blks;
    for ( auto n : nodes ) {
        hassert (d_in.find(n).t == NodeType::Basic);
        blks.push_back(d_in.find(n).b);
    }
    _splice_out_blocks (blks);
    auto seq = gg.graph.blockAllocator().newSequence(blks);

    Block *doloop = gg.graph.blockAllocator().newBlock(Block::makeDoLoopBlock());
    doloop->u_doloop().branch = seq;

    doloop->u_doloop().guard = (ChpExprSingleRootDag::of_expr(
                                    ChpExpr::makeConstant(ChpOptimize::BigInt{1}, 1)));
    
    gg.graph.m_seq = gg.graph.blockAllocator().newSequence({doloop});
}

bool Projection::_all_basic (const DFG &d_in, std::vector<NodeId> xs)
{
    for ( auto x : xs ) {
        if (d_in.find(x).t != NodeType::Basic)
            return false;
    }
    return true;
}

void Projection::build_vardefmap (DFG &d_in)
{
    for ( const auto &n : d_in.nodes ) {
        auto vars = get_defs(*n);
        for ( const auto &var : vars ) {
            Assert (!(d_in.vardefmap.count(var)), "Multiple nodes defining the same var? STF violation");
            d_in.vardefmap.insert({var,n->id});
        }
    }
}

std::vector<VarId> Projection::get_defs (const DFG_Node &node)
{
    Block *blk = node.b;
    std::vector<VarId> ret = {};
    switch (node.t)
    {                
    case NodeType::Basic: {
        switch (blk->u_basic().stmt.type()) {
        case StatementType::Send:
        break;
        case StatementType::Assign: {
            ret = blk->u_basic().stmt.u_assign().ids;
        }
        break;
        case StatementType::Receive: {
            if (blk->u_basic().stmt.u_receive().var)
                ret.push_back(*blk->u_basic().stmt.u_receive().var);
        }
        break;
        }
    }
    break;
    case NodeType::Guard: {
        // does not define any variables
    }
    break;
    case NodeType::LoopGuard: {
        // does not define any variables
    }
    break;
    case NodeType::SelPhi: {
        auto phi = node.phi;
        ret.push_back(phi.post_id);
    }
    break;
    case NodeType::SelPhiInv: {
        auto phi_inv = node.phi_inv;
        for ( auto y : phi_inv.branch_ids ) {
            if (y) ret.push_back(*y);
        }
    }
    break;
    case NodeType::PllPhi: {
        auto phi = node.pll_phi;
        ret.push_back(phi.post_id);
    }
    break;
    case NodeType::PllPhiInv: {
        auto phi_inv = node.pll_phi_inv;
        for ( auto y : phi_inv.branch_ids ) {
            if (y) ret.push_back(*y);
        }
    }
    break;
    case NodeType::LoopInPhi: {
        auto lip = node.lip;
        ret.push_back(lip.bodyin_id);
    }
    break;
    case NodeType::LoopOutPhi: {
        auto lop = node.lop;
        ret.push_back(lop.post_id);
    }
    break;
    case NodeType::LoopLoopPhi: {
        auto llp = node.llp;
        ret.push_back(llp.bodyin_id);
        if (llp.post_id) {
            ret.push_back(*llp.post_id);
        }
    }
    break;
    default:
        hassert (false);
    break;
    }
    return ret;
}

std::unordered_set<VarId> Projection::get_uses (const DFG_Node &node) 
{
    Block *blk = node.b;
    std::unordered_set<VarId> ret = {};
    switch (node.t)
    {                
    case NodeType::Basic: {
        switch (blk->u_basic().stmt.type()) {
        case StatementType::Send: {
            ret = getIdsUsedByExpr(blk->u_basic().stmt.u_send().e);
        }
        break;
        case StatementType::Assign: {
            ret = getIdsUsedByExpr(blk->u_basic().stmt.u_assign().e);
        }
        break;
        case StatementType::Receive: {
        }
        break;
        }
    }
    break;
    case NodeType::Guard: {
        ret = getIdsUsedByExpr(node.g.second.u_e().e);
    }
    case NodeType::LoopGuard: {
        ret = getIdsUsedByExpr(node.g.second.u_e().e);
    }
    case NodeType::SelPhi: {
        auto phi = node.phi;
        for ( auto x : phi.branch_ids )
            ret.insert(x);
    }
    break;
    case NodeType::SelPhiInv: {
        auto phi_inv = node.phi_inv;
        ret.insert(phi_inv.pre_id);
    }
    break;
    case NodeType::PllPhi: {
        auto phi = node.pll_phi;
        for ( auto x : phi.branch_ids )
            if (x) ret.insert(*x);
    }
    break;
    case NodeType::PllPhiInv: {
        auto phi_inv = node.pll_phi_inv;
        ret.insert(phi_inv.pre_id);
    }
    break;
    case NodeType::LoopInPhi: {
        auto lip = node.lip;
        ret.insert(lip.pre_id);
    }
    break;
    case NodeType::LoopOutPhi: {
        auto lop = node.lop;
        ret.insert(lop.bodyout_id);
    }
    break;
    case NodeType::LoopLoopPhi: {
        auto llp = node.llp;
        ret.insert(llp.bodyout_id);
        ret.insert(llp.pre_id);
    }
    break;
    default:
        hassert (false);
    break;
    }
    return ret;
}

bool Projection::_check_data_dependence (const DFG_Node &prev, const DFG_Node &curr) 
{
    auto defs = get_defs(prev);
    auto uses = get_uses(curr);

    for ( auto v : defs ) {
        if (uses.count(v)) return true;
    }
    return false;
}

bool Projection::_check_guard_phi_inv_dependence (const DFG_Node &guard_node, const DFG_Node &phi_inv_node)
{
    hassert (guard_node.t == NodeType::Guard);
    hassert (phi_inv_node.t == NodeType::SelPhiInv);

    if (guard_node.b != phi_inv_node.b) return false;

    auto br_ids = phi_inv_node.phi_inv.branch_ids;
    int br = guard_node.g.first;
    hassert (br < br_ids.size());
    auto br_id = (br_ids[br]);
    return (br_id) ? true : false; // this is just for my own sanity
}

bool Projection::_check_guard_phi_dependence (const DFG_Node &guard_node, const DFG_Node &phi_node)
{
    hassert (guard_node.t == NodeType::Guard);
    hassert (phi_node.t == NodeType::SelPhi);
    return (guard_node.b == phi_node.b);
}

void Projection::_build_graph (const Sequence &seq, DFG &d_in)
{
    _build_graph_nodes (seq, d_in);
    _build_graph_edges (d_in);
}

void Projection::_build_graph_edges (DFG &d_in)
{
    for ( const auto &n1 : d_in.nodes ) {
        for ( const auto &n2 : d_in.nodes ) {

            bool add_edge = false;
            if (n1->id != n2->id) {

                add_edge |= _check_data_dependence(*n1,*n2);

                if (n1->t == NodeType::Guard && n2->t == NodeType::SelPhiInv)
                    add_edge |= _check_guard_phi_inv_dependence(*n1,*n2);

                if (n1->t == NodeType::Guard && n2->t == NodeType::SelPhi)
                    add_edge |= _check_guard_phi_dependence(*n1,*n2);
            
            }
            
            if (add_edge) d_in.add_edge(n1->id,n2->id);
        
        }
    }
}

void Projection::_build_graph_nodes (const Sequence &seq, DFG &d_in)
{
    Block *curr = seq.startseq->child();

    while (curr->type() != BlockType::EndSequence) {
    switch (curr->type()) {
    case BlockType::Basic: {
        d_in.add_node(DFG_Node (curr));
    }
    break;
      
    case BlockType::Par: {
        for ( auto phi_inv : curr->u_par().splits ) {
            d_in.add_node(DFG_Node (curr, phi_inv));
        }
        for (auto &branch : curr->u_par().branches) {
            _build_graph_nodes (branch, d_in);
        }
        for ( auto phi : curr->u_par().merges ) {
            d_in.add_node(DFG_Node (curr, phi));
        }
    }
    break;
      
    case BlockType::Select: {
        // phi-inverses
        for ( auto phi_inv : curr->u_select().splits ) {
            d_in.add_node(DFG_Node (curr, phi_inv));
        }
        // guard nodes
        int i=0;
        for ( auto &branch : curr->u_select().branches ) {
            d_in.add_node(DFG_Node (curr, i, IRGuard::deep_copy(branch.g)));
            i++;
        }
        // branches
        for ( auto &branch : curr->u_select().branches ) {
            _build_graph_nodes (branch.seq, d_in);
        }
        // phi's
        for ( auto phi : curr->u_select().merges ) {
            d_in.add_node(DFG_Node (curr, phi));
        }
    }
    break;
      
    case BlockType::DoLoop: {
        for ( auto iphi : curr->u_doloop().in_phis ) {
            d_in.add_node(DFG_Node (curr, iphi));
        }
        for ( auto lphi : curr->u_doloop().loop_phis ) {
            d_in.add_node(DFG_Node (curr, lphi));
        }

        _build_graph_nodes(curr->u_doloop().branch, d_in);

        /*
            // Assuming top-level loop guard is always `true` 
        */
        d_in.add_node(DFG_Node (curr, curr->u_doloop().guard));
        for ( auto ophi : curr->u_doloop().out_phis ) {
            d_in.add_node(DFG_Node (curr, ophi));
        }
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

NodeId Projection::_heuristic2(DFG &d_in, const DFG_Node &n, int nwcc)
{
    std::vector<NodeId> src_nodes = {};

    for ( auto ns : d_in.adj ) {
        for ( auto n1 : ns.second ) {
            if (n1==n.id) src_nodes.push_back(d_in.find(ns.first).id);
        }
    }

    for ( auto nn : src_nodes ) {
        if (!d_in.in_same_scc(n.id,nn)) {
            // dfg.print_adj(stdout);

            d_in.delete_edge(nn,n.id);
            auto tmp = d_in.get_wccs();

            d_in.print_adj(stdout);
            d_in.add_edge(nn,n.id);

            if (tmp.size()>nwcc) {
                return nn;
            }
        }
    }

    return bot_id;
}

NodeId Projection::_heuristic3(DFG &d_in, const DFG_Node &n, int nwcc)
{
    auto dest_nodes = d_in.adj[n.id];
    std::vector<Edge> reconnect = {};
    // for (int i=0;i<dest_nodes.size();i++) {
    for ( auto dn : dest_nodes ) {
        if (!d_in.in_same_scc(n.id,dn)) {

            // auto [c1, c2] = find_components(d_in, n.id,dest_nodes[i]);
            auto c1 = d_in.find_scc(n.id);
            auto c2 = d_in.find_scc(dn);
            // fprintf(stdout, "\n// CHECKING\n");

            // delete ALL edges from SCC_i to SCC_j
            for ( auto m1 : c1 ) {
                for ( auto m2 : c2 ) {
                    if (d_in.contains_edge(m1, m2)) {
                        d_in.delete_edge(m1, m2);
                        reconnect.push_back({m1,m2});
                    }
                }
            }

            // now check if |WCC| has increased
            auto tmp = d_in.get_wccs();

            // add back the original edges from SCC_i to SCC_j
            for ( auto x : reconnect ) {
                d_in.add_edge(x.first,x.second);
            }

            if (tmp.size()>nwcc) {
                // fprintf(stdout, "\n// FOUND\n");
                return dn;
            }
        }
    }
    return bot_id;
}

void Projection::_insert_copies_v3 (GraphWithChanNames &gg, DFG &d_in, Sequence seq, int nwcc, int root, bool &inserted)
{
    Block *curr = seq.startseq->child();
    CopyLocMap clm;

    while (curr->type() != BlockType::EndSequence) {
    if (inserted) return;
    switch (curr->type()) {
    case BlockType::Basic: {
        switch (curr->u_basic().stmt.type()) {
        case StatementType::Receive: {
        }
        break;
        case StatementType::Send: {
        }
        break;
        case StatementType::Assign: {
            const auto &n = d_in.find(curr);
            if (n && root==0) {
                auto vars = get_defs(n);
                hassert (vars.size()<=1);
                if (vars.size()==1 && (_heuristic3(d_in, n, nwcc)!=bot_id)) {
                    _insert_hyperedge_copy(gg, d_in, {n.id,d_in.adj.at(n.id)}, vars[0], clm);
                    inserted = true;
                    return;
                }
                auto n1 = _heuristic2(d_in, n, nwcc);
                if (d_in.contains(n1)) {
                    auto vars = get_defs(n1);
                    hassert (vars.size()==1);
                    _insert_hyperedge_copy(gg, d_in, {n1,d_in.adj.at(n1)}, vars[0], clm);
                    inserted = true;
                    return;
                }
            }
        }
        break;
        }
    }
    break;
      
    case BlockType::Par: {
        for (auto &branch : curr->u_par().branches) {
            _insert_copies_v3 (gg, d_in, branch, nwcc, root, inserted);
        }
    }
    break;
      
    case BlockType::Select: {
        
        for (auto &split : curr->u_select().splits) {
            const auto &n = d_in.find(curr, split);
            auto vars = get_defs(n);
            if (root==0 && _heuristic3(d_in, n, nwcc)!=bot_id) {
                _insert_hyperedge_copy (gg, d_in, {n.id,d_in.adj.at(n.id)}, split.pre_id, clm);
                inserted = true;
                return;
            }
        }
        for (auto &branch : curr->u_select().branches) {
            _insert_copies_v3 (gg, d_in, branch.seq, nwcc, root, inserted);
        }
        for (auto &merge : curr->u_select().merges) {
            auto n = d_in.find(curr, merge);
            auto vars = get_defs(n);
            if (root==0 && _heuristic3(d_in, n, nwcc)!=bot_id) {
                _insert_hyperedge_copy (gg, d_in, {n.id,d_in.adj.at(n.id)}, merge.post_id, clm);
                inserted = true;
                return;
            }
        }
    }
    break;
    case BlockType::DoLoop: {
        _insert_copies_v3 (gg, d_in, curr->u_doloop().branch, nwcc, 0, inserted);
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

void Projection::_replace_use (GraphWithChanNames &gg, VarId oldvar, VarId newvar, const DFG_Node &nn)
{
    auto remap = [&](VarId &id) {
        if (id==oldvar)
            id = newvar;
    };
    auto remap_vec = [&](std::vector<VarId> &idvec) {
        for ( auto &id : idvec ) {
            remap(id);
        }
    };
    auto remap_opt = [&](OptionalVarId &oid) {
        if (oid) {
            VarId id = *oid;
            remap(id);
            oid = id;
        }
    };
    auto remap_opt_vec = [&](std::vector<OptionalVarId> &idvec) {
        for ( auto &id : idvec ) {
            remap_opt(id);
        }
    };
    auto remap_expr = [&](ChpExprDag &dag) {
        ChpExprDag::mapNodes(dag, [&](ChpExprDag::Node &n) {
            if (n.type() == IRExprTypeKind::Var)
                remap(n.u_var().id);
        });
    };

    switch (nn.t)
    {                
    case NodeType::Basic: {
        switch (nn.b->u_basic().stmt.type()) {
        case StatementType::Send: {
            remap_expr(nn.b->u_basic().stmt.u_send().e.m_dag);
        }
        break;
        case StatementType::Assign: {
            remap_expr(nn.b->u_basic().stmt.u_assign().e);
        }
        break;
        case StatementType::Receive: {
        }
        break;
        }
    }
    break;
    case NodeType::Guard: {
        auto br = nn.b->u_select().branches.begin();
        for (int i=0; i<nn.b->u_select().branches.size();i++) {
            if (i==nn.g.first) break;
            br++;
        }
        if (br->g.type()==IRGuardType::Expression) {
            remap_expr(br->g.u_e().e.m_dag);
        }
    }
    break;
    case NodeType::LoopGuard: {
        remap_expr(nn.b->u_doloop().guard.m_dag);
    }
    break;
    case NodeType::SelPhi: {
        for ( auto &phi : nn.b->u_select().merges ) {
            remap_vec (phi.branch_ids);
        }
    }
    break;
    case NodeType::SelPhiInv: {
        for ( auto &phi_inv : nn.b->u_select().splits ) {
            remap (phi_inv.pre_id);
        }
    }
    break;
    case NodeType::PllPhi: {
        for (auto &split : nn.b->u_par().splits) {
            remap(split.pre_id);
        }
    }
    break;
    case NodeType::PllPhiInv: {
        for (auto &merge : nn.b->u_par().merges) {
            remap_opt_vec(merge.branch_ids);
        }
    }
    break;
    case NodeType::LoopInPhi: {
        for (auto &in_phi : nn.b->u_doloop().in_phis) {
            remap(in_phi.pre_id);
        }
    }
    break;
    case NodeType::LoopOutPhi: {
        for (auto &out_phi : nn.b->u_doloop().out_phis) {
            remap(out_phi.bodyout_id);
        }
    }
    break;
    case NodeType::LoopLoopPhi: {
        for (auto &loop_phi : nn.b->u_doloop().loop_phis) {
            remap(loop_phi.bodyout_id);
            remap(loop_phi.pre_id);
        }
    }
    break;
    default:
        hassert (false);
    break;
    }

}

void Projection::print_subgraphs (FILE *ff, const std::unordered_map<UnionFind<int>::id, std::vector<int>> &sgs)
{
    fprintf (ff, "\n/* --- Connected components ---\n");
    for ( const auto &x : sgs ) {
        fprintf(ff, "\ncomponent : ");
        for ( const auto &n : x.second ) {
            fprintf(ff, "%d, ", n);
        }
    }
    fprintf (ff, "\n\n   --- Connected components --- */ \n");
}

void Projection::_splice_out_blocks (std::vector<Block *> blks)
{
    for ( auto b : blks ) {
        _splice_out_block_new (b);
    }
}

void Projection::_splice_out_block_new(Block *bb) 
{
    Block *before = bb->parent();
    Block *after = bb->child();

    if (before) Block::disconnect(before, bb);
    if (after)  Block::disconnect(bb, after);
    if (before && after) Block::connect(before, after);
}

void Projection::split_assignments(ChpGraph &gg)
{
    _split_assignments(gg.m_seq);
}

void Projection::_split_assignments(Sequence seq)
{
    Block *curr = seq.startseq->child();

    while (curr->type() != BlockType::EndSequence) {
    switch (curr->type()) {
    case BlockType::Basic: {
        switch (curr->u_basic().stmt.type()) {
        case StatementType::Send:
        case StatementType::Receive: {
            }
            break;
        case StatementType::Assign: {
            auto ids  = curr->u_basic().stmt.u_assign().ids;
            auto prev = curr->parent();
            auto tmp = curr->child();
            _splice_out_block(curr);
            std::vector<Block *> blks = {};
            int idx = 0;
            for ( auto id : ids ) {
                auto expr = ChpExprDag::deep_copy(curr->u_basic().stmt.u_assign().e);
                auto e2 = ChpExpr::deep_copy(ChpExprDag::to_expr(*expr.roots[idx]));
                ChpExprSingleRootDag d = ChpExprSingleRootDag::of_expr(e2);

                blks.push_back(g->graph.blockAllocator().newBlock(Block::makeBasicBlock(
                    Statement::makeAssignment(id, std::move(d))
                )));
                idx++;
            }
            Sequence ss = g->graph.blockAllocator().newSequence(blks);
            g->graph.spliceInSequenceAfter(prev,ss);
            curr = tmp->parent();
            }
            break;
        }
    }
    break;
      
    case BlockType::Par: {
        for (auto &branch : curr->u_par().branches) {
            _split_assignments (branch);
        }
    }
    break;
      
    case BlockType::Select: {
        for (auto &branch : curr->u_select().branches) {
            _split_assignments (branch.seq);
        }
    }
    break;
      
    case BlockType::DoLoop: {
        _split_assignments(curr->u_doloop().branch);
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

void Projection::export_dot(std::string filename, const DFG &d_in)
{
    FILE *ff = fopen(filename.c_str(), "w");

    std::string edge_repr = "->";
    fprintf(ff,"\ndigraph{ ");
    for (const auto &node : d_in.nodes)
    {
        std::stringstream ss;
        node->print(ss);
        std::ostringstream ss1;
        ss1 << ss.rdbuf();
        auto sl = ss1.str();
        std::string attr = "[label=\""+std::to_string(node->id.get_raw())+": "+sl+"\"";
        if (node->t==NodeType::Basic) {
            attr += " shape=box color=black]";
        } else if (node->t==NodeType::Guard) {
            attr += " shape=parallelogram color=red]";
        } else {
            attr += " shape=ellipse color=blue]";
        }
        fprintf(ff, "\n_%d %s;", node->id.get_raw(), attr.c_str());
    }
    for ( auto sn : d_in.adj )
    {
        for ( auto dn : sn.second ) 
        {
            fprintf(ff, "\n_%d %s _%d;", sn.first.get_raw(), edge_repr.c_str(), dn.get_raw());
        }
    }
    fprintf(ff,"\n}");
    fclose(ff);
}

bool is_unobservable(ChpGraph &g, Sequence seq)
{
    bool ret = true;
    Block *curr = seq.startseq->child();

    while (curr->type() != BlockType::EndSequence) {
    switch (curr->type()) {
    case BlockType::Basic: {
        switch (curr->u_basic().stmt.type()) {
        case StatementType::Send:
        case StatementType::Receive: {
            return false;
            }
            break;
        case StatementType::Assign: {
            }
            break;
        }
    }
    break;
      
    case BlockType::Par: {
        for (auto &branch : curr->u_par().branches) {
            ret &= is_unobservable (g, branch);
        }
    }
    break;
      
    case BlockType::Select: {
        for (auto &branch : curr->u_select().branches) {
            ret &= is_unobservable (g, branch.seq);
        }
    }
    break;
      
    case BlockType::DoLoop: {
        ret &= is_unobservable(g, curr->u_doloop().branch);
    }
    break;
    
    case BlockType::StartSequence:
    case BlockType::EndSequence:
        hassert(false);
        break;
    }
    curr = curr->child();
    }
    return ret;
}

void eliminate_unobservable(ChpGraph &g, Sequence seq)
{
    Block *curr = seq.startseq->child();
    switch (curr->type()) {
    case BlockType::Par: {
        std::list<Sequence> new_branches = {};
        for (auto &branch : curr->u_par().branches) {
            if(!is_unobservable (g, branch)) {
                new_branches.push_back(branch);
            }
        }
        curr->u_par().branches = new_branches;
    }
    break;
    case BlockType::DoLoop: 
    case BlockType::Basic:
    case BlockType::Select:
    break;
    case BlockType::StartSequence:
    case BlockType::EndSequence:
        hassert(false);
        break;
    }
} 
