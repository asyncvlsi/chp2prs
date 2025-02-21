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

#include "projection.h"

/*
    Projection TODO
    - IC-LCD handling (done)
    - selection handling (done)
    - test interaction with other passes (done)
    - implement cost-based cut (done)
    - expose complex part cleanly for optimizer plug-in (done)
*/

std::vector<Sequence> Projection::get_seqs ()
{
    return seqs;
}

std::vector<act_chp_lang_t *> Projection::get_procs ()
{
    return procs;
}

void Projection::step1(GraphWithChanNames &g_in, DFG &d_in)
{
    d_in.clear();
    ChpOptimize::putIntoNewStaticTokenForm(g_in.graph);
    _build_graph(g_in.graph.m_seq, d_in);
    ChpOptimize::takeOutOfNewStaticTokenForm(g_in.graph);
    _insert_guard_comms(g_in, d_in);
    d_in.clear();
}

void Projection::step2(GraphWithChanNames &g_in, DFG &d_in)
{
    d_in.clear();
    if (!g_in.graph.is_static_token_form) 
        ChpOptimize::putIntoNewStaticTokenForm(g_in.graph);
    _build_graph(g_in.graph.m_seq, d_in);

    // SCC aux. structures
    std::vector<std::vector<int>> adj_cond, comps;
    std::vector<bool> visited;
    adj_cond.clear(); comps.clear(); visited.clear();
    hassert (d_in.id==d_in.nodes.size());

    // Compute SCCs
    d_in.scc(comps, adj_cond, visited);
    d_in.build_sccs(comps);
}

void Projection::project()
{
    bool printt = false;

    // split multi-assignments into single
    split_assignments(g->graph);

    // insert guard bool communications
    step1(*g, dfg1);

    // compute strongly-connected components info
    step2(*g, dfg1);
    
    // Copy-insertion strategy
    bool _ins = false;
    if (1) {
        _insert_copies_v3 (*g, dfg1, g->graph.m_seq, _compute_connected_components(dfg1).size(), 1, _ins);
    }
    else {
        // ChpOptimize::takeOutOfNewStaticTokenForm(g->graph);
        _insert_copies_v4 (*g, dfg1);
    }
    
    dfg1.clear();
    _build_graph(g->graph.m_seq, dfg1);

    // Construct sub-processes
    ChpOptimize::takeOutOfNewStaticTokenForm(g->graph);
    _build_procs (*g, dfg1);
    // ChpOptimize::putIntoNewStaticTokenForm(g->graph);
}

/*
    This is the brute-force bit.
    Exponential in no. of SCC edges, at least (lol).
    Checks every possible subset of SCC-edges that can be cut.
    Picks the one with minimum maximum-latency cost
*/
void Projection::_insert_copies_v4 (const GraphWithChanNames &g, DFG &d_in)
{
    // make copy of graph
    std::unordered_map<ChanId, ChanId> cc;
    std::unordered_map<VarId, VarId> vv;
    auto g_copy = deep_copy_graph(g,cc,vv);
    DFG d_loc;
    step2(g_copy, d_loc);
    ChpOptimize::takeOutOfNewStaticTokenForm(g_copy.graph);

    // get candidate edges
    auto cand_edges = _candidate_edges(d_loc);
    std::vector<std::pair<IntPair, std::vector<IntPair>>> edges (cand_edges.begin(), cand_edges.end());
    auto sz = cand_edges.size();

    ChpCost c(s);
    double min_max_cost = std::numeric_limits<double>::max();
    std::vector<IntPair> best_edges_subset = {};

    // iterate over all possible subsets
    fprintf(stdout, "\n\n// subsets to check: %d\n\n", (1<<sz));
    for (int mask=0; mask<2; mask++)
    // for (int mask=0; mask<(1<<sz); mask++)
    {
        std::vector<IntPair> edges_subset = {};
        // pick the subset 
        for (int i = 0; i < sz; i++) {
            if (mask & (1 << i)) {
                edges_subset.insert(edges_subset.end(), edges[i].second.begin(), edges[i].second.end());
            }
        }

        fprintf (stdout, "\nchp 1 -----\n");
        print_chp (std::cout, g_copy.graph);
        // d_loc.print_adj(stdout);
        fprintf (stdout, "\nchp 1 -----\n");

        // delete edges in this subset
        // only do single-var defining nodes
        std::unordered_map<VarId, VarId> old_to_new = {};
        for ( auto e : edges_subset ) {
            const auto &node = d_loc.find(e.first);
            auto vars = get_defs(node);
            if (vars.size()==1) {
                d_loc.delete_edge (e.first, e.second);
                // Actually insert copies corresponding to this edge deletion
                auto newvar = _insert_copy (g_copy, d_loc, e.first, vars[0]);
                old_to_new.insert({vars[0],newvar});
                fprintf (stdout, "\n\ninserting copy: %llu, %llu \n", vars[0].m_id, newvar.m_id);
            }
        }

        fprintf (stdout, "\nchp after copy insertion -----\n");
        print_chp (std::cout, g_copy.graph);
        // d_loc.print_adj(stdout);
        fprintf (stdout, "\nchp after copy insertion -----\n");

        // build the subprocesses and see what the cost is
        _build_procs(g_copy, d_loc);
        c.add_procs(procs);
        auto cost = c.get_max_latency_cost();
        if (cost < min_max_cost) {
            min_max_cost = cost;
            best_edges_subset = edges_subset;
        }

        // add edges back
        // only do single-var defining nodes
        for ( auto e : edges_subset ) {
            const auto &node = d_loc.find(e.first);
            auto vars = get_defs(node);
            if (vars.size()==1) {
                d_loc.add_edge (e.first, e.second);
                // Un-insert the copy corresponding to this edge
                _uninsert_copy (g_copy, d_loc, e.first, old_to_new[vars[0]], vars[0]);
            }
        }
    }
    fprintf(stdout, "\n\n// min. max. cost: %lf", min_max_cost);
    fprintf(stdout, "\n\n// edges to break: %d", int(best_edges_subset.size()));
    fprintf(stdout, "\n\n// vars to copy: ");
    for ( const auto &e : best_edges_subset ) {
        const auto &node = d_loc.find(e.first);
        auto vars = get_defs(node);
        Assert(vars.size()<=1, "hm");
        if (vars.size()==1) {
            fprintf(stdout, "v%llu, ", vars[0].m_id);
        }
    }

}

void Projection::_uninsert_copy (GraphWithChanNames &gg, const DFG &d_in, int from, VarId copyvar, VarId origvar)
{
    auto b_from = d_in.find(from).b;
    auto dist_assn = b_from->child();
    // print_chp_block(std::cout, dist_assn);
    Assert (dist_assn->type()==BlockType::Par, "par block");
    Assert (dist_assn->u_par().branches.size()==2, "two par branches");
    auto snd = dist_assn->u_par().branches.front().startseq->child();
    auto rcv = dist_assn->u_par().branches.back().startseq->child();
    Assert (snd->type()==BlockType::Basic && snd->u_basic().stmt.type()==StatementType::Send, "send");
    Assert (rcv->type()==BlockType::Basic && rcv->u_basic().stmt.type()==StatementType::Receive, "recv");

    _splice_out_block (dist_assn);

    _replace_uses (gg, copyvar, origvar, snd, b_from);
}

/*
    Returns map from pair `{scc_i,scc_j}` to vector of edges that go from `scc_i` to `scc_j`
*/
std::unordered_map<IntPair, std::vector<IntPair>> Projection::_candidate_edges (const DFG &d_in)
{
    std::unordered_map<IntPair, std::vector<IntPair>> ret = {};
    int i=0;
    for ( const auto &src : d_in.adj ) {
        for ( const auto &dest : src ) {
            auto scc1 = _find_scc(d_in, i);
            auto scc2 = _find_scc(d_in, dest);
            if (scc1!=scc2) {
                if (!ret.contains(IntPair(scc1,scc2)))
                    ret.insert({IntPair(scc1,scc2),{}});
                ret[IntPair(scc1,scc2)].push_back(IntPair(i,dest));
            }
        }
        i++;
    }
    return ret;
}

void Projection::_build_procs (const GraphWithChanNames &gx, DFG &d_in)
{
    procs.clear();
    // ChpOptimize::takeOutOfNewStaticTokenForm(gx.graph);

    int num_subgraphs = _compute_connected_components(d_in).size();
    std::unordered_set<int> marker_node_ids = {};

    DFG d_loc;
    for (int i=0; i<num_subgraphs; i++)
    {
        std::vector<ActId *> tmp_names;

        auto a1 = chp_graph_to_act (gx, tmp_names, s);
        auto g1 = chp_graph_from_act (a1, s);

        ChpOptimize::putIntoNewStaticTokenForm(g1.graph);

        d_loc.clear();
        _build_graph(g1.graph.m_seq, d_loc);
        auto tmp_sgs = _compute_connected_components(d_loc);

        auto itr = tmp_sgs.begin();
        while ((marker_node_ids.contains((*itr).second[0]))) 
        { itr++; }
        hassert (itr != subgraphs.end());
        marker_node_ids.insert((*itr).second[0]);

        if (_all_basic(d_loc, (*itr).second)) {
            _build_basic_new (g1, d_loc, (*itr).second);
        }
        else {
            std::unordered_set<int> tmp ((*itr).second.begin(), (*itr).second.end());
            _build_sub_proc_new (g1, d_loc, g1.graph.m_seq, tmp);
            _remove_guard_comms (g1, g1.graph.m_seq);
        }

        ChpOptimize::takeOutOfNewStaticTokenForm(g1.graph);
        seqs.push_back(g1.graph.m_seq);
        std::vector<ActId *> tmp_names2;
        act_chp_lang_t *tmpact = chp_graph_to_act (g1, tmp_names2, s);
        procs.push_back(tmpact);

        // fprintf(stdout, "\n\nnum_subgraphs : %d, tmp_sgs: %d\n", num_subgraphs, int(tmp_sgs.size()));
        hassert (num_subgraphs == tmp_sgs.size());
    }
    hassert (marker_node_ids.size() == num_subgraphs);
}

bool Projection::_build_sub_proc_new (GraphWithChanNames &gg, const DFG &d_in, Sequence seq, std::unordered_set<int> &s)
{
    bool empty = true;
    Block *curr = seq.startseq->child();
    while (curr->type() != BlockType::EndSequence) {
    switch (curr->type()) {
    case BlockType::Basic: {
        const auto &dfgnode = d_in.find(curr);
        if (dfgnode && s.contains(dfgnode.id)) {
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
            if (dfgnode && s.contains(dfgnode.id)) {
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
            if (dfgnode && s.contains(dfgnode.id)) {
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
            if (dfgnode && s.contains(dfgnode.id)) {
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
            if (!emp || (dfgnode && s.contains(dfgnode.id))) {
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
                    IRGuard::makeExpression(ChpExprSingleRootDag::makeConstant(BigInt(1), 1))});
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
            if (dfgnode && s.contains(dfgnode.id)) {
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
                                        ChpExpr::makeConstant(BigInt{1}, 1)));
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

void Projection::_build_basic_new (GraphWithChanNames &gg, const DFG &d_in, std::vector<int> nodes)
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
                                    ChpExpr::makeConstant(BigInt{1}, 1)));
    
    gg.graph.m_seq = gg.graph.blockAllocator().newSequence({doloop});
}

bool Projection::_all_basic (const DFG &d_in, std::vector<int> xs)
{
    for ( auto x : xs ) {
        if (d_in.find(x).t != NodeType::Basic)
            return false;
    }
    return true;
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
        if (uses.contains(v)) return true;
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
        d_in.add_node(DFG_Node (curr, d_in.gen_id()));
    }
    break;
      
    case BlockType::Par: {
        for ( auto phi_inv : curr->u_par().splits ) {
            d_in.add_node(DFG_Node (curr, phi_inv, d_in.gen_id()));
        }
        for (auto &branch : curr->u_par().branches) {
            _build_graph_nodes (branch, d_in);
        }
        for ( auto phi : curr->u_par().merges ) {
            d_in.add_node(DFG_Node (curr, phi, d_in.gen_id()));
        }
    }
    break;
      
    case BlockType::Select: {
        // phi-inverses
        for ( auto phi_inv : curr->u_select().splits ) {
            d_in.add_node(DFG_Node (curr, phi_inv, d_in.gen_id()));
        }
        // guard nodes
        int i=0;
        for ( auto &branch : curr->u_select().branches ) {
            d_in.add_node(DFG_Node (curr, i, IRGuard::deep_copy(branch.g), d_in.gen_id()));
            i++;
        }
        // branches
        for ( auto &branch : curr->u_select().branches ) {
            _build_graph_nodes (branch.seq, d_in);
        }
        // phi's
        for ( auto phi : curr->u_select().merges ) {
            d_in.add_node(DFG_Node (curr, phi, d_in.gen_id()));
        }
    }
    break;
      
    case BlockType::DoLoop: {
        for ( auto iphi : curr->u_doloop().in_phis ) {
            d_in.add_node(DFG_Node (curr, iphi, d_in.gen_id()));
        }
        for ( auto lphi : curr->u_doloop().loop_phis ) {
            d_in.add_node(DFG_Node (curr, lphi, d_in.gen_id()));
        }

        _build_graph_nodes(curr->u_doloop().branch, d_in);

        /*
            // Assuming top-level loop guard is always `true` 
            dfg.add_node(DFG_Node (curr, curr->u_doloop().guard, dfg.gen_id()));
        */
        for ( auto ophi : curr->u_doloop().out_phis ) {
            d_in.add_node(DFG_Node (curr, ophi, d_in.gen_id()));
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

std::unordered_map<UnionFind<int>::id, std::vector<int>>
Projection::_compute_connected_components (const DFG &d_in)
{
    std::unordered_map<UnionFind<int>::id, std::vector<int>> subgs;
    ChpOptimize::UnionFind<int> uf;
    for (int i=0; i<d_in.adj.size(); i++) {
        for (int j=0; j<d_in.adj[i].size(); j++) {
            uf.union_(d_in.nodes[i]->id,d_in.adj[i][j]);
        }
    }

    subgs.clear();
    for ( const auto &n : d_in.nodes ) {
        auto ufn = uf.find(n->id);
        if (!subgs.contains(ufn)) {
            subgs.insert({ufn,{}});
        }
        subgs[ufn].push_back(n->id);
    } 
    return subgs;
}

void Projection::_insert_guard_comms (GraphWithChanNames &g_in, DFG &d_in)
{
    std::vector<std::pair<int, int>> to_add;
    std::vector<std::pair<int, int>> to_delete;
    for ( int i = 0; i<d_in.nodes.size(); i++ )
    {
        if (d_in.nodes[i]->t == NodeType::Basic) {
            auto assn_blk = d_in.nodes[i]->b;
            for ( auto n = d_in.adj[i].begin(); n != d_in.adj[i].end(); n++ ) {
                const auto &nn = d_in.find(*n);
                if (nn.t == NodeType::Guard) {

                    hassert ( assn_blk->type() == BlockType::Basic) ;
                    hassert ( assn_blk->u_basic().stmt.type() == StatementType::Assign );
                    hassert ( assn_blk->u_basic().stmt.u_assign().ids.size() == 1 );
                    hassert ( g_in.graph.id_pool().getBitwidth(assn_blk->u_basic().stmt.u_assign().ids[0]) == 1 );

                    ChanId ci = g_in.graph.id_pool().makeUniqueChan(1, false);
                    var_to_actvar vtoa(s, g_in.graph.id_pool());
                    ActId *id = vtoa.chanMap(ci);
                    g_in.name_from_chan.insert({ci, id});
                    
                    auto send = g_in.graph.blockAllocator().newBlock(
                        Block::makeBasicBlock(Statement::makeSend(ci, 
                        ChpExprSingleRootDag::makeVariableAccess(assn_blk->u_basic().stmt.u_assign().ids[0], 1))));
                    
                    VarId g_var = g_in.graph.id_pool().makeUniqueVar(1, false);

                    auto recv = g_in.graph.blockAllocator().newBlock(
                        Block::makeBasicBlock(Statement::makeReceive(ci, g_var)));

                    // auto send_node = new DFG_Node (send, dfg.gen_id()); 
                    d_in.add_node (DFG_Node (send, d_in.gen_id()));
                    // printf("\nsend node id: %d", send_node->id);
                    // hassert (dfg.contains(send_node));

                    // auto recv_node = new DFG_Node (recv, dfg.gen_id()); 
                    // dfg.add_node (recv_node);
                    d_in.add_node (DFG_Node (recv, d_in.gen_id()));
                    // printf("\nrecv node id: %d", recv_node->id);
                    // hassert (dfg.contains(recv_node));

                    to_add.push_back({d_in.nodes[i]->id, (d_in.find(send)).id});
                    to_add.push_back({(d_in.find(recv)).id, nn.id});
                    to_delete.push_back({d_in.nodes[i]->id,nn.id});

                    auto dist_assn = g_in.graph.blockAllocator().newBlock(Block::makeParBlock());
                    dist_assn->u_par().branches.push_back(g_in.graph.blockAllocator().newSequence({send}));
                    dist_assn->u_par().branches.push_back(g_in.graph.blockAllocator().newSequence({recv}));

                    auto orig_sel = nn.b;

                    _splice_in_block_between (orig_sel->parent(), orig_sel, dist_assn);

                    int i=0;
                    int br_num = nn.g.first;
                    for (auto ll = orig_sel->u_select().branches.begin(); ll != orig_sel->u_select().branches.end(); ll++)
                    {
                        if (i==br_num) {
                            ll->g = IRGuard::makeExpression(ChpExprSingleRootDag::makeVariableAccess(g_var,1));
                            break;
                        }
                        i++;
                    }
                    // create new block with send
                    // insert and add correct edges in DFG
                    // rip out this edge
                }
            }
        }
    }
    for (auto x : to_add) {
        d_in.add_edge (x.first, x.second);
    }
    for (auto x : to_delete) {
        d_in.delete_edge (x.first, x.second);
    }
}

#if 0
void Projection::_insert_copies_v0 (GraphWithChanNames &gg, Sequence seq)
{
    Block *curr = seq.startseq->child();

    while (curr->type() != BlockType::EndSequence) {
    switch (curr->type()) {
    case BlockType::Basic: {
        switch (curr->u_basic().stmt.type()) {
        case StatementType::Receive: {
            const auto &n = dfg.find(curr);
            if (n) { // only do if pre-existing receive
                auto oldvar = curr->u_basic().stmt.u_receive().var;
                if (oldvar) {
                    _insert_copy(gg, seq, n, *oldvar);
                }
            }
        }
            break;
        case StatementType::Send: {
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
            _insert_copies_v0 (gg, branch);
        }
    }
    break;
      
    case BlockType::Select: {
        for (auto &branch : curr->u_select().branches) {
            _insert_copies_v0 (gg, branch.seq);
        }
    }
    break;
    case BlockType::DoLoop: {
        _insert_copies_v0 (gg, curr->u_doloop().branch);
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

void Projection::_insert_copies_v1 (GraphWithChanNames &gg, Sequence seq)
{
    Block *curr = seq.startseq->child();

    while (curr->type() != BlockType::EndSequence) {
    switch (curr->type()) {
    case BlockType::Basic: {
    }
    break;
      
    case BlockType::Par: {
        for (auto &branch : curr->u_par().branches) {
            _insert_copies_v1 (gg, branch);
        }
    }
    break;
      
    case BlockType::Select: {
        
        for (auto &split : curr->u_select().splits) {
            _insert_copy(gg, seq, curr, curr->parent(), split.pre_id);
        }
        for (auto &branch : curr->u_select().branches) {
            _insert_copies_v1 (gg, branch.seq);
        }
        for (auto &merge : curr->u_select().merges) {
            _insert_copy(gg, seq, curr->child(), curr, merge.post_id);
        }
    }
    break;
    case BlockType::DoLoop: {
        // for (auto &lphi : curr->u_doloop().loop_phis) {
        //     _insert_copy(gg, seq, curr->u_doloop().branch.startseq->child(), lphi.bodyin_id);
        // }
        _insert_copies_v1 (gg, curr->u_doloop().branch);
        // for (auto &lphi : curr->u_doloop().loop_phis) {
        //     _insert_copy(gg, seq, curr->u_doloop().branch.endseq, lphi.bodyout_id);
        // }
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
#endif

bool Projection::_in_same_scc (const DFG &d_in, int n1, int n2)
{
    hassert (n1!=-1);
    hassert (n2!=-1);

    hassert (d_in.sccs.contains(n1));
    hassert (d_in.sccs.contains(n2));

    return (d_in.sccs.at(n1)==d_in.sccs.at(n2));
}

int Projection::_find_scc (const DFG &d_in, int n1)
{
    hassert (n1!=-1);
    hassert (d_in.sccs.contains(n1));
    return d_in.sccs.at(n1);
}

std::pair<std::vector<int>, std::vector<int>> Projection::find_components (const DFG &d_in, int n1, int n2)
{
    std::vector<int> c1 = {};
    std::vector<int> c2 = {};
    
    hassert (d_in.sccs.contains(n1));
    hassert (d_in.sccs.contains(n2));

    auto r1 = d_in.sccs.at(n1);
    auto r2 = d_in.sccs.at(n2);
    hassert (r1!=r2);

    for ( const auto &x : d_in.sccs ) {
        if (x.second==r1) c1.push_back(x.first);
        if (x.second==r2) c2.push_back(x.first);
    }

    return {c1,c2};
}

int Projection::_heuristic1(DFG &d_in, const DFG_Node &n, int nwcc)
{
    auto dest_nodes = d_in.adj[n.id];
    for (int i=0;i<dest_nodes.size();i++) {
        if (!_in_same_scc(d_in, n.id,dest_nodes[i])) {

            d_in.delete_edge(n.id,dest_nodes[i]);

            auto tmp = _compute_connected_components(d_in);

            d_in.add_edge(n.id,dest_nodes[i]);

            if (tmp.size()>nwcc) {
                return dest_nodes[i];
            }
        }
    }
    return -1;
}

int Projection::_heuristic2(DFG &d_in, const DFG_Node &n, int nwcc)
{
    std::vector<int> src_nodes = {};
    int i=0;

    for ( auto ns : d_in.adj ) {
        for ( auto n1 : ns ) {
            if (n1==n.id) src_nodes.push_back(d_in.nodes[i]->id);
        }
        i++;
    }

    for (int i=0;i<src_nodes.size();i++) {
        if (!_in_same_scc(d_in, n.id,src_nodes[i])) {
            // dfg.print_adj(stdout);

            d_in.delete_edge(src_nodes[i],n.id);
            auto tmp = _compute_connected_components(d_in);

            d_in.print_adj(stdout);
            d_in.add_edge(src_nodes[i],n.id);

            if (tmp.size()>nwcc) {
                return src_nodes[i];
            }
        }
    }

    return -1;
}

int Projection::_heuristic3(DFG &d_in, const DFG_Node &n, int nwcc)
{
    auto dest_nodes = d_in.adj[n.id];
    std::vector<std::pair<int, int>> reconnect = {};
    for (int i=0;i<dest_nodes.size();i++) {
        if (!_in_same_scc(d_in, n.id,dest_nodes[i])) {

            auto [c1, c2] = find_components(d_in, n.id,dest_nodes[i]);
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
            auto tmp = _compute_connected_components(d_in);

            // add back the original edges from SCC_i to SCC_j
            for ( auto x : reconnect ) {
                d_in.add_edge(x.first,x.second);
            }

            if (tmp.size()>nwcc) {
                // fprintf(stdout, "\n// FOUND\n");
                return dest_nodes[i];
            }
        }
    }
    return -1;
}

#if 0
void Projection::_insert_copies_v2 (GraphWithChanNames &gg, Sequence seq, int nwcc, bool &inserted)
{
    Block *curr = seq.startseq->child();

    while (curr->type() != BlockType::EndSequence) {
    if (inserted) return;
    switch (curr->type()) {
    case BlockType::Basic: {
        switch (curr->u_basic().stmt.type()) {
        case StatementType::Receive: {
            auto n = dfg.find(curr);
            if (n) { // only do if pre-existing receive
                auto vars = get_defs(n);
                hassert (vars.size()<=1);
                if (vars.size()==1 && (_heuristic1(n, nwcc)!=-1)) {
                    _insert_copy(gg, n.id, vars[0]);
                    inserted = true;
                }
            }
        }
            break;
        case StatementType::Send: {
            auto n = dfg.find(curr);
            if (n) {
                auto n1 = _heuristic2(n, nwcc);
                if (dfg.contains(n1)) {
                    auto vars = get_defs(n1);
                    hassert (vars.size()==1);
                    _insert_copy(gg, n1, vars[0]);
                    inserted = true;
                } 
            }
        }
        break;
        case StatementType::Assign: {
            auto n = dfg.find(curr);
            if (n) {
                auto vars = get_defs(n);
                hassert (vars.size()<=1);
                if (vars.size()==1 && (_heuristic1(n, nwcc)!=-1)) {
                    _insert_copy(gg, n.id, vars[0]);
                    inserted = true;
                    return;
                }
                auto n1 = _heuristic2(n, nwcc);
                if (n1!=-1) {
                    auto vars = get_defs(n1);
                    hassert (vars.size()==1);
                    _insert_copy(gg, n1, vars[0]);
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
            _insert_copies_v2 (gg, branch, nwcc, inserted);
        }
    }
    break;
      
    case BlockType::Select: {
        
        for (auto &split : curr->u_select().splits) {
            auto n = dfg.find(curr, split);
            auto vars = get_defs(n);
            if (_heuristic1(n, nwcc)!=-1) {
                _insert_copy (gg, n.id, split.pre_id);
                inserted = true;
                return;
            }
        }
        for (auto &branch : curr->u_select().branches) {
            _insert_copies_v2 (gg, branch.seq, nwcc, inserted);
        }
        for (auto &merge : curr->u_select().merges) {
            auto n = dfg.find(curr, merge);
            auto vars = get_defs(n);
            if (_heuristic1(n, nwcc)!=-1) {
                _insert_copy (gg, n.id, merge.post_id);
                inserted = true;
                return;
            }
        }
    }
    break;
    case BlockType::DoLoop: {
        _insert_copies_v2 (gg, curr->u_doloop().branch, nwcc, inserted);
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
#endif

void Projection::_insert_copies_v3 (GraphWithChanNames &gg, DFG &d_in, Sequence seq, int nwcc, int root, bool &inserted)
{
    Block *curr = seq.startseq->child();

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
                if (vars.size()==1 && (_heuristic3(d_in, n, nwcc)!=-1)) {
                    _insert_copy(gg, d_in, n.id, vars[0]);
                    inserted = true;
                    return;
                }
                auto n1 = _heuristic2(d_in, n, nwcc);
                if (d_in.contains(n1)) {
                    auto vars = get_defs(n1);
                    hassert (vars.size()==1);
                    _insert_copy(gg, d_in, n1, vars[0]);
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
            if (root==0 && _heuristic3(d_in, n, nwcc)!=-1) {
                _insert_copy (gg, d_in, n.id, split.pre_id);
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
            if (root==0 && _heuristic3(d_in, n, nwcc)!=-1) {
                _insert_copy (gg, d_in, n.id, merge.post_id);
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

VarId Projection::_insert_copy (GraphWithChanNames &gg, const DFG &d_in, int from, VarId v)
{
    Assert (d_in.contains(from), "Node not found");
    auto b_from = d_in.find(from).b;

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

    Block *strt = b_from;
    while (strt->type()!=BlockType::StartSequence) { strt = strt->parent(); }

    _replace_uses (gg, strt, v, copy_var, send, dist_assn);
    return copy_var;
}

#if 0
void Projection::_insert_copy (GraphWithChanNames &gg, Sequence seq, Block *splice_before, Block *start_after, VarId v)
{
    ChanId ci = gg.graph.id_pool().makeUniqueChan(gg.graph.id_pool().getBitwidth(v), false);
    var_to_actvar vtoa(s, &gg.graph.id_pool());
    ActId *id = vtoa.chanMap(ci);
    gg.name_from_chan.insert({ci, id});
    
    auto send = gg.graph.blockAllocator().newBlock(
        Block::makeBasicBlock(Statement::makeSend(ci, 
        ChpExprSingleRootDag::makeVariableAccess(v, 1))));
    
    VarId copy_var = gg.graph.id_pool().makeUniqueVar(gg.graph.id_pool().getBitwidth(v), false);

    auto recv = gg.graph.blockAllocator().newBlock(
        Block::makeBasicBlock(Statement::makeReceive(ci, copy_var)));

    auto dist_assn = gg.graph.blockAllocator().newBlock(Block::makeParBlock());
    dist_assn->u_par().branches.push_back(gg.graph.blockAllocator().newSequence({send}));
    dist_assn->u_par().branches.push_back(gg.graph.blockAllocator().newSequence({recv}));

    _splice_in_block_between (splice_before->parent(), splice_before, dist_assn);

    // fprintf(stdout,"\n\ngot here \n\n");
    // fprintf(stdout,"\n\ngot here \n\n");

    _replace_uses (gg, seq, v, copy_var, send, start_after);
}
#endif

void Projection::_replace_uses (GraphWithChanNames &gg, VarId oldvar, VarId newvar, Block *excl, Block *start_after)
{
    hassert(excl->type()==BlockType::Basic && excl->u_basic().stmt.type()==StatementType::Send);

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

    Block *curr = start_after;
    curr = curr->child();

    while (curr->type() != BlockType::EndSequence) {
    switch (curr->type()) {
    case BlockType::Basic: {
        switch (curr->u_basic().stmt.type()) {
        case StatementType::Receive: 
            break;
        case StatementType::Send: {
            if (curr!=excl)
                remap_expr(curr->u_basic().stmt.u_send().e.m_dag);
        }
            break;
        case StatementType::Assign: {
            remap_expr(curr->u_basic().stmt.u_assign().e);
            }
            break;
        }
    }
    break;
      
    case BlockType::Par: {
        for (auto &split : curr->u_par().splits) {
            remap(split.pre_id);
        }
        for (auto &merge : curr->u_par().merges) {
            remap_opt_vec(merge.branch_ids);
        }
        for (auto &branch : curr->u_par().branches) {
            _replace_uses (gg, branch.startseq, oldvar, newvar, excl, branch.startseq);
        }
    }
    break;
      
    case BlockType::Select: {
        for (auto &split : curr->u_select().splits) {
            remap(split.pre_id);
        }
        for (auto &merge : curr->u_select().merges) {
            remap_vec(merge.branch_ids);
        }
        for (auto &branch : curr->u_select().branches) {
            _replace_uses (gg, branch.seq.startseq , oldvar, newvar, excl, branch.seq.startseq);
        }
    }
    break;
    case BlockType::DoLoop: {
        for (auto &in_phi : curr->u_doloop().in_phis) {
            remap(in_phi.pre_id);
        }
        for (auto &out_phi : curr->u_doloop().out_phis) {
            remap(out_phi.bodyout_id);
        }
        for (auto &loop_phi : curr->u_doloop().loop_phis) {
            remap(loop_phi.bodyout_id);
            remap(loop_phi.pre_id);
        }
        _replace_uses (gg, curr->u_doloop().branch.startseq, oldvar, newvar, excl, curr->u_doloop().branch.startseq);
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

void Projection::_replace_uses (GraphWithChanNames &gg, Block *strt, VarId oldvar, VarId newvar, Block *excl, Block *start_after)
{
    hassert(strt->type()==BlockType::StartSequence);
    hassert(excl->type()==BlockType::Basic && excl->u_basic().stmt.type()==StatementType::Send);

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

    Block *curr = strt;
    while (curr != start_after) {
        curr = curr->child();
        if (curr->type()==BlockType::EndSequence) return;
    }
    curr = curr->child();

    while (curr->type() != BlockType::EndSequence) {
    switch (curr->type()) {
    case BlockType::Basic: {
        switch (curr->u_basic().stmt.type()) {
        case StatementType::Receive: 
            break;
        case StatementType::Send: {
            if (curr!=excl)
                remap_expr(curr->u_basic().stmt.u_send().e.m_dag);
        }
            break;
        case StatementType::Assign: {
            remap_expr(curr->u_basic().stmt.u_assign().e);
            }
            break;
        }
    }
    break;
      
    case BlockType::Par: {
        for (auto &split : curr->u_par().splits) {
            remap(split.pre_id);
        }
        for (auto &merge : curr->u_par().merges) {
            remap_opt_vec(merge.branch_ids);
        }
        for (auto &branch : curr->u_par().branches) {
            _replace_uses (gg, branch.startseq, oldvar, newvar, excl, branch.startseq);
        }
    }
    break;
      
    case BlockType::Select: {
        for (auto &split : curr->u_select().splits) {
            remap(split.pre_id);
        }
        for (auto &merge : curr->u_select().merges) {
            remap_vec(merge.branch_ids);
        }
        for (auto &branch : curr->u_select().branches) {
            _replace_uses (gg, branch.seq.startseq , oldvar, newvar, excl, branch.seq.startseq);
        }
    }
    break;
    case BlockType::DoLoop: {
        for (auto &in_phi : curr->u_doloop().in_phis) {
            remap(in_phi.pre_id);
        }
        for (auto &out_phi : curr->u_doloop().out_phis) {
            remap(out_phi.bodyout_id);
        }
        for (auto &loop_phi : curr->u_doloop().loop_phis) {
            remap(loop_phi.bodyout_id);
            remap(loop_phi.pre_id);
        }
        _replace_uses (gg, curr->u_doloop().branch.startseq, oldvar, newvar, excl, curr->u_doloop().branch.startseq);
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

#if 0
void Projection::_replace_uses (GraphWithChanNames &gg, Sequence seq, VarId oldvar, VarId newvar, Block *excl, Block *start_after)
{
    hassert(excl->type()==BlockType::Basic && excl->u_basic().stmt.type()==StatementType::Send);

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

    Block *curr = seq.startseq;
    while (curr != start_after) {
        curr = curr->child();
        if (curr->type()==BlockType::EndSequence) return;
    }
    curr = curr->child();

    while (curr->type() != BlockType::EndSequence) {
    switch (curr->type()) {
    case BlockType::Basic: {
        switch (curr->u_basic().stmt.type()) {
        case StatementType::Receive: 
            break;
        case StatementType::Send: {
            if (curr!=excl)
                remap_expr(curr->u_basic().stmt.u_send().e.m_dag);
        }
            break;
        case StatementType::Assign: {
            remap_expr(curr->u_basic().stmt.u_assign().e);
            }
            break;
        }
    }
    break;
      
    case BlockType::Par: {
        for (auto &split : curr->u_par().splits) {
            remap(split.pre_id);
        }
        for (auto &merge : curr->u_par().merges) {
            remap_opt_vec(merge.branch_ids);
        }
        for (auto &branch : curr->u_par().branches) {
            _replace_uses (gg, branch, oldvar, newvar, excl, branch.startseq);
        }
    }
    break;
      
    case BlockType::Select: {
        for (auto &split : curr->u_select().splits) {
            remap(split.pre_id);
        }
        for (auto &merge : curr->u_select().merges) {
            remap_vec(merge.branch_ids);
        }
        for (auto &branch : curr->u_select().branches) {
            _replace_uses (gg, branch.seq , oldvar, newvar, excl, branch.seq.startseq);
        }
    }
    break;
    case BlockType::DoLoop: {
        for (auto &in_phi : curr->u_doloop().in_phis) {
            remap(in_phi.pre_id);
        }
        for (auto &out_phi : curr->u_doloop().out_phis) {
            remap(out_phi.bodyout_id);
        }
        for (auto &loop_phi : curr->u_doloop().loop_phis) {
            remap(loop_phi.bodyout_id);
            remap(loop_phi.pre_id);
        }
        _replace_uses (gg, curr->u_doloop().branch, oldvar, newvar, excl, curr->u_doloop().branch.startseq);
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
#endif

/*
    TODO: This could conflict with an actual
    copy of a 1-bit var.
*/
void Projection::_remove_guard_comms (GraphWithChanNames &gg, Sequence seq)
{
    Block *curr = seq.startseq->child();

    while (curr->type() != BlockType::EndSequence) {
    switch (curr->type()) {
    case BlockType::Basic:
    break;
      
    case BlockType::Par: {
        for (auto &branch : curr->u_par().branches) {
            _remove_guard_comms (gg, branch);
        }
        if (curr->u_par().branches.size()==2) {
            auto first = curr->u_par().branches.front();
            auto second = curr->u_par().branches.back();
            if (first.startseq->child()->type() == BlockType::Basic
            && second.startseq->child()->type() == BlockType::Basic) {
                // always (send,recv), never (recv,send)
                auto &send = first.startseq->child()->u_basic().stmt;
                auto &recv = second.startseq->child()->u_basic().stmt;
                if (send.type()==StatementType::Send 
                 && recv.type()==StatementType::Receive
                 && send.u_send().chan == recv.u_receive().chan
                 && (recv.u_receive().var)
                 && gg.graph.id_pool().getBitwidth(send.u_send().chan) == 1) {
                    auto assn = gg.graph.blockAllocator().newBlock(Block::makeBasicBlock(
                        Statement::makeAssignment(*(recv.u_receive().var),
                        ChpExprSingleRootDag::deep_copy(send.u_send().e))
                    ));
                    _splice_in_block_between(curr,curr->child(),assn);
                    curr = _splice_out_block (curr);
                    hassert (curr==assn);
                }
            }
        }
    }
    break;
      
    case BlockType::Select: {
        for (auto &branch : curr->u_select().branches) {
            _remove_guard_comms (gg, branch.seq);
        }
    }
    break;
    case BlockType::DoLoop: {
        _remove_guard_comms(gg, curr->u_doloop().branch);
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

#if 0
void Projection::split_selections()
{
    _split_selections(g->graph.m_seq);
}
#endif

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
        fprintf(ff, "\n_%d [label=\"%d: %s\"];", node->id, node->id, sl.c_str());
    }
    for (int i=0;i<d_in.adj.size();i++)
    {
        for (int j=0;j<d_in.adj[i].size();j++) 
        {
            fprintf(ff, "\n_%d %s _%d;", d_in.nodes[i]->id, edge_repr.c_str(), d_in.adj[i][j]);
        }
    }
    fprintf(ff,"\n}");
    fclose(ff);
}

#if 0
void Projection::_split_selections(Sequence seq)
{
    Block *curr = seq.startseq->child();

    while (curr->type() != BlockType::EndSequence) {
    switch (curr->type()) {
    case BlockType::Basic: {
    }
    break;
      
    case BlockType::Par: {
        for (auto &branch : curr->u_par().branches) {
            _split_selections (branch);
        }
    }
    break;
      
    case BlockType::Select: {
        for (auto &branch : curr->u_select().branches) {
            _split_selections (branch.seq);
        }
        auto tmp = curr->parent();
        _splice_out_block(curr);
        Block *par = g->graph.blockAllocator().newBlock(Block::makeParBlock());
        auto selsetid = _gen_sel_set_id();
        hassert(!(sel_sets.contains(selsetid)));
        sel_sets.insert({selsetid,{}});
        for (auto &branch : curr->u_select().branches) {
            Block *sel2 = g->graph.blockAllocator().newBlock(Block::makeSelectBlock());
            sel2->u_select().branches.push_back({branch.seq,IRGuard::deep_copy(branch.g)});
            sel2->u_select().branches.push_back({g->graph.newSequence({}),IRGuard::makeElse()});
            par->u_par().branches.push_back(g->graph.newSequence({sel2}));
            sel_sets[selsetid].push_back(sel2);
        }
        tmp = _splice_in_block_between(tmp,tmp->child(),par);
        curr = tmp->parent();
    }
    break;
      
    case BlockType::DoLoop: {
        _split_selections(curr->u_doloop().branch);
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
#endif