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
    Topological sort not needed (?)

    Projection TODO
    - IC-LCD handling
    - cycle detection + topological sorting of blocks
    - selection handling
    - test interaction with other passes
    - implement min-cut

*/

std::vector<Sequence> Projection::get_seqs ()
{
    return seqs;
}

std::vector<act_chp_lang_t *> Projection::get_procs ()
{
    return procs;
}

bool Projection::_check_linear (Sequence seq, int root)
{
    Block *curr = seq.startseq->child();
    bool ret = true;

    while (curr->type() != BlockType::EndSequence) {
    switch (curr->type()) {
    case BlockType::Basic: {
    }
    break;
      
    case BlockType::Par: {
    }
    break;
      
    case BlockType::Select: {
        ret = false;
    }
    break;
      
    case BlockType::DoLoop:
        if (root == 0) {
            ret = false;
        }
        else {
            ret &= _check_linear(curr->u_doloop().branch, 0);
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

void Projection::project()
{
    // if (!_check_linear(g->graph.m_seq,1))
    //     return;

    // fprintf(stdout, "\n/* One \n");
    // print_chp(std::cout, g->graph);
    // fprintf(stdout, "\n*/\n");

    split_assignments(g->graph);

    // fprintf(stdout, "\n/* Two \n");
    // print_chp(std::cout, g->graph);
    // fprintf(stdout, "\n*/\n");
    
    // ChpOptimize::putIntoStaticTokenForm(g->graph);
    ChpOptimize::putIntoNewStaticTokenForm(g->graph);

    fprintf(stdout, "\n/* STF \n");
    print_chp(std::cout, g->graph);
    fprintf(stdout, "\n*/\n");

    // insert guard bool communications
    dfg.clear();
    _build_graph(g->graph.m_seq);
    ChpOptimize::takeOutOfNewStaticTokenForm(g->graph);
    fprintf(stdout, "\n/* Non-STF \n");
    print_chp(std::cout, g->graph);
    fprintf(stdout, "\n*/\n");
    _insert_guard_comms();

    std::vector<ActId *> tmp_names;
    auto a1 = chp_graph_to_act (*g, tmp_names, s);
    auto g1 = chp_graph_from_act (a1, s);

    dfg.clear();
    ChpOptimize::putIntoNewStaticTokenForm(g1.graph);
    _build_graph(g1.graph.m_seq);
    _compute_connected_components();

    fprintf(stdout, "\n// Adj. List g1 \n");
    dfg.print_adj(stdout);
    fprintf(stdout, "\n\n");
    fprintf(stdout, "\n// Subgraphs g1 \n");
    print_subgraphs(stdout);
    
    fprintf(stdout, "\n/* STF g1 \n");
    print_chp(std::cout, g1.graph);
    fprintf(stdout, "\n*/\n");

    // _insert_copies_v0 (g1, g1.graph.m_seq);
    _insert_copies_v1 (g1, g1.graph.m_seq);
    // _insert_copies_v2 (g1, g1.graph.m_seq);

    ChpOptimize::takeOutOfNewStaticTokenForm(g1.graph);

    std::vector<ActId *> tmp_names2;
    auto a2 = chp_graph_to_act (g1, tmp_names2, s);
    auto g2 = chp_graph_from_act (a2, s);

    dfg.clear();
    ChpOptimize::putIntoNewStaticTokenForm(g2.graph);
    _build_graph(g2.graph.m_seq);
    _compute_connected_components();

    fprintf(stdout, "\n// Adj. List g2 \n");
    dfg.print_adj(stdout);
    fprintf(stdout, "\n\n");
    fprintf(stdout, "\n// Subgraphs g2 \n");
    print_subgraphs(stdout);
    
    fprintf(stdout, "\n/* STF g2 \n");
    print_chp(std::cout, g2.graph);
    fprintf(stdout, "\n*/\n");

    export_dot("zz_graph.dot");

    ChpOptimize::takeOutOfNewStaticTokenForm(g2.graph);

    int num_subgraphs = subgraphs.size();
    // if (num_subgraphs==1) return;

    std::unordered_set<int> marker_node_ids = {};
    // for (auto subgraph : subgraphs)
    for (int i=0; i<num_subgraphs; i++)
    {
        std::vector<ActId *> tmp_names;
        // auto a1 = chp_graph_to_act (*g, tmp_names, s);
        auto a1 = chp_graph_to_act (g2, tmp_names, s);
        auto g1 = chp_graph_from_act (a1, s);

        ChpOptimize::putIntoNewStaticTokenForm(g1.graph);

        dfg.clear();
        _build_graph(g1.graph.m_seq);
        _compute_connected_components();

        // dfg.print_adj(stdout);
        // fprintf(stdout, "\n\n");
        // print_subgraphs(stdout);
        // fprintf(stdout, "\n/* STF \n");
        // print_chp(std::cout, g1.graph);
        // fprintf(stdout, "\n*/\n");

        auto itr = subgraphs.begin();
        while ((marker_node_ids.contains((*itr).second[0]->id))) 
        {
            itr++;
        }
        hassert (itr != subgraphs.end());
        marker_node_ids.insert((*itr).second[0]->id);
        // print_chp(std::cout, g1.graph);
        if (_all_basic((*itr).second)) {
            // fprintf(stdout, "\n// building basic: %d \n", int((*itr).second.size()));
            // for ( auto x : (*itr).second ) {
            //     fprintf(stdout, "%d, ", x->id);
            // }
            // fprintf(stdout, "\n");
            _build_basic_new (g1, (*itr).second);
        }
        else {
            // fprintf(stdout, "\n// building full: %d \n", int((*itr).second.size()));
            // for ( auto x : (*itr).second ) {
            //     fprintf(stdout, "%d, ", x->id);
            // }
            // fprintf(stdout, "\n");
            std::unordered_set<DFG_Node *> tmp ((*itr).second.begin(), (*itr).second.end());
            _build_sub_proc_new (g1, g1.graph.m_seq, tmp);
            _remove_guard_comms (g1, g1.graph.m_seq);
        }


        // fprintf(stdout, "\n/* Post-build STF \n");
        // print_chp(std::cout, g1.graph);
        // fprintf(stdout, "\n*/\n");
        ChpOptimize::takeOutOfNewStaticTokenForm(g1.graph);
        seqs.push_back(g1.graph.m_seq);
        fprintf(stdout, "\n/* subproc: \n\n");
        std::vector<ActId *> tmp_names2;
        act_chp_lang_t *tmpact = chp_graph_to_act (g1, tmp_names2, s);
        chp_print(stdout, tmpact);
        procs.push_back(tmpact);
        fprintf(stdout, "\n\n*/\n");
        // fprintf(stdout, "\nnum_subg: %d", num_subgraphs);
        // fprintf(stdout, "\nsubg_size: %d", int(subgraphs.size()));
        hassert (num_subgraphs == subgraphs.size());
    }
    hassert (marker_node_ids.size() == subgraphs.size());
}

bool Projection::_build_sub_proc_new (GraphWithChanNames &gg, Sequence seq, std::unordered_set<DFG_Node *> &s)
{
    bool empty = true;
    Block *curr = seq.startseq->child();
    while (curr->type() != BlockType::EndSequence) {
    switch (curr->type()) {
    case BlockType::Basic: {
        auto dfgnode = dfg.find(curr);
        if (dfgnode && s.contains(dfgnode)) {
            s.erase(dfgnode);
            empty = false;
            // printf ("\n// skipping\n");
        }
        else {
            curr->dead = true;
            curr = _splice_out_block (curr);
            curr = curr->parent();
            // printf ("\n// splicing\n");
        }
    }
    break;
      
    case BlockType::Par: {

        std::vector<Block::Variant_Par::PhiSplit> new_splits = {};
        for (auto phi_inv : curr->u_par().splits) {
            auto dfgnode = dfg.find(curr, phi_inv);
            if (dfgnode && s.contains(dfgnode)) {
                s.erase(dfgnode);
                new_splits.push_back(phi_inv);
                empty = false;
            }
        }
        curr->u_par().splits = new_splits;
        
        for (auto &branch : curr->u_par().branches) {
            empty &= _build_sub_proc_new (gg, branch, s);
        }

        std::vector<Block::Variant_Par::PhiMerge> new_merges = {};
        for (auto phi : curr->u_par().merges) {
            auto dfgnode = dfg.find(curr, phi);
            if (dfgnode && s.contains(dfgnode)) {
                s.erase(dfgnode);
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
            if (n->t==NodeType::Guard && n->b == curr) {
                lift = false; break;
            }
        }

        std::vector<Block::Variant_Select::PhiSplit> new_splits = {};
        for (auto phi_inv : curr->u_select().splits) {
            auto dfgnode = dfg.find(curr, phi_inv);
            if (dfgnode && s.contains(dfgnode)) {
                s.erase(dfgnode);
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
            bool emp = _build_sub_proc_new (gg, branch.seq, s);

            auto dfgnode = dfg.find(curr, {i, IRGuard::deep_copy(branch.g)});
            if (!emp || (dfgnode && s.contains(dfgnode))) {
                s.erase(dfgnode);
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
            auto dfgnode = dfg.find(curr, phi);
            if (dfgnode && s.contains(dfgnode)) {
                s.erase(dfgnode);
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
        empty = _build_sub_proc_new(gg, curr->u_doloop().branch, s);
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

void Projection::_build_basic_new (GraphWithChanNames &gg, std::vector<DFG_Node *> nodes)
{
    std::vector<Block *> blks;
    for ( auto n : nodes ) {
        hassert (n->t == NodeType::Basic);
        blks.push_back(n->b);
    }
    _splice_out_blocks (blks);
    auto seq = gg.graph.blockAllocator().newSequence(blks);

    Block *doloop = gg.graph.blockAllocator().newBlock(Block::makeDoLoopBlock());
    doloop->u_doloop().branch = seq;

    doloop->u_doloop().guard = (ChpExprSingleRootDag::of_expr(
                                    ChpExpr::makeConstant(BigInt{1}, 1)));
    
    gg.graph.m_seq = gg.graph.blockAllocator().newSequence({doloop});
}

bool Projection::_all_basic (std::vector<DFG_Node *> xs)
{
    for ( auto x : xs ) {
        if (x->t != NodeType::Basic)
            return false;
    }
    return true;
}

#if 0
bool Projection::_set_contains (Block *b, std::unordered_set<DFG_Node *> &s)
{
    for ( auto node : s ) 
    {
        switch (node->t) {
        case NodeType::Basic: {
            switch (blk->u_basic().stmt.type()) {
            case StatementType::Send:
            break;
            case StatementType::Assign: {
            }
            break;
            case StatementType::Receive: {
            }
            break;
            }
        }
        break;
        case NodeType::Guard: {
            // does not define any variables
        }
        break;
        case NodeType::SelPhi: {
        }
        break;
        case NodeType::SelPhiInv: {
        }
        break;
        case NodeType::PllPhi: {
        }
        break;
        case NodeType::PllPhiInv: {
        }
        break;
        case NodeType::LoopInPhi: {
            hassert (false);
        }
        break;
        case NodeType::LoopOutPhi: {
            hassert (false);
        }
        break;
        case NodeType::LoopLoopPhi: {
            hassert (false);
        }
        break;
        default:
            hassert (false);
        break;
        }
    }
    return false;
}
#endif

#if 0
int Projection::_gen_sel_set_id()
{
    sel_set_id++;
    return sel_set_id;
}
#endif

std::vector<VarId> Projection::get_defs (DFG_Node *node)
{
    hassert(dfg.contains(node));
    Block *blk = node->b;
    std::vector<VarId> ret = {};
    switch (node->t)
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
    case NodeType::SelPhi: {
        auto phi = node->phi;
        ret.push_back(phi.post_id);
    }
    break;
    case NodeType::SelPhiInv: {
        auto phi_inv = node->phi_inv;
        for ( auto y : phi_inv.branch_ids ) {
            if (y) ret.push_back(*y);
        }
    }
    break;
    case NodeType::PllPhi: {
        auto phi = node->pll_phi;
        ret.push_back(phi.post_id);
    }
    break;
    case NodeType::PllPhiInv: {
        auto phi_inv = node->pll_phi_inv;
        for ( auto y : phi_inv.branch_ids ) {
            if (y) ret.push_back(*y);
        }
    }
    break;
    case NodeType::LoopInPhi: {
        auto lip = node->lip;
        ret.push_back(lip.bodyin_id);
    }
    break;
    case NodeType::LoopOutPhi: {
        auto lop = node->lop;
        ret.push_back(lop.post_id);
    }
    break;
    case NodeType::LoopLoopPhi: {
        auto llp = node->llp;
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

std::unordered_set<VarId> Projection::get_uses (DFG_Node *node) 
{
    hassert(dfg.contains(node));
    Block *blk = node->b;
    std::unordered_set<VarId> ret = {};
    switch (node->t)
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
        ret = getIdsUsedByExpr(node->g.second.u_e().e);
    }
    case NodeType::SelPhi: {
        auto phi = node->phi;
        for ( auto x : phi.branch_ids )
            ret.insert(x);
    }
    break;
    case NodeType::SelPhiInv: {
        auto phi_inv = node->phi_inv;
        ret.insert(phi_inv.pre_id);
    }
    break;
    case NodeType::PllPhi: {
        auto phi = node->pll_phi;
        for ( auto x : phi.branch_ids )
            if (x) ret.insert(*x);
    }
    break;
    case NodeType::PllPhiInv: {
        auto phi_inv = node->pll_phi_inv;
        ret.insert(phi_inv.pre_id);
    }
    break;
    case NodeType::LoopInPhi: {
        auto lip = node->lip;
        ret.insert(lip.pre_id);
    }
    break;
    case NodeType::LoopOutPhi: {
        auto lop = node->lop;
        ret.insert(lop.bodyout_id);
    }
    break;
    case NodeType::LoopLoopPhi: {
        auto llp = node->llp;
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

/*
    True if dependent
*/
bool Projection::_check_data_dependence (DFG_Node *prev, DFG_Node *curr) 
{
    auto defs = get_defs(prev);
    auto uses = get_uses(curr);

    for ( auto v : defs ) {
        if (uses.contains(v)) return true;
    }
    return false;
}

bool Projection::_check_guard_phi_inv_dependence (DFG_Node *guard_node, DFG_Node *phi_inv_node)
{
    hassert (guard_node->t == NodeType::Guard);
    hassert (phi_inv_node->t == NodeType::SelPhiInv);

    if (guard_node->b != phi_inv_node->b) return false;

    auto br_ids = phi_inv_node->phi_inv.branch_ids;
    int br = guard_node->g.first;
    hassert (br < br_ids.size());
    auto br_id = (br_ids[br]);
    return (br_id) ? true : false; // this is just for my own sanity
}

bool Projection::_check_guard_phi_dependence (DFG_Node *guard_node, DFG_Node *phi_node)
{
    hassert (guard_node->t == NodeType::Guard);
    hassert (phi_node->t == NodeType::SelPhi);

    return (guard_node->b == phi_node->b);
}

void Projection::_build_graph (Sequence seq)
{
    Block *curr = seq.startseq->child();

    while (curr->type() != BlockType::EndSequence) {
    switch (curr->type()) {
    case BlockType::Basic: {
        auto node = new DFG_Node (curr, dfg.gen_id()); 
        dfg.add_node(node);
        for ( auto n : dfg.nodes ) {
            if (_check_data_dependence(n, node)) {
                dfg.add_edge(n,node);
            }
            if (_check_data_dependence(node, n)) {
                dfg.add_edge(node,n);
            }
        }
    }
    break;
      
    case BlockType::Par: {
        for ( auto phi_inv : curr->u_par().splits ) {
            // fprintf(fp, "\n// pll_phi_inv: %llu - ",phi_inv.pre_id.m_id);
            // for ( auto y : phi_inv.branch_ids ) {
            //     if (y) fprintf(fp, "%llu, ", y._getId());
            //     else   fprintf(fp, "null, ");
            // }
            auto node = new DFG_Node (curr, phi_inv, dfg.gen_id()); 
            dfg.add_node(node);
            for ( auto n : dfg.nodes ) {
                if (_check_data_dependence(n, node)) {
                    dfg.add_edge(n,node);
                }
                if (_check_data_dependence(node, n)) {
                    dfg.add_edge(node,n);
                }
            }
        }
        for (auto &branch : curr->u_par().branches) {
            _build_graph (branch);
        }
        for ( auto phi : curr->u_par().merges ) {
            // fprintf(fp, "\n// pll_phi: ");
            // for ( auto y : phi.branch_ids ) {
            //     if (y) fprintf(fp, "%llu, ", y._getId());
            //     else   fprintf(fp, "null, ");
            // }
            // fprintf(fp, " - %llu",phi.post_id.m_id);
            auto node = new DFG_Node (curr, phi, dfg.gen_id()); 
            dfg.add_node(node);
            for ( auto n : dfg.nodes ) {
                if (_check_data_dependence(n, node)) {
                    dfg.add_edge(n,node);
                }
                if (_check_data_dependence(node, n)) {
                    dfg.add_edge(node,n);
                }
            }
        }
    }
    break;
      
    case BlockType::Select: {
        // phi-inverses
        for ( auto phi_inv : curr->u_select().splits ) {
            // fprintf(fp, "\n// phi_inv: %llu - ",phi_inv.pre_id.m_id);
            // for ( auto y : phi_inv.branch_ids ) {
            //     if (y) fprintf(fp, "%llu, ", y._getId());
            //     else   fprintf(fp, "null, ");
            // }
            auto node = new DFG_Node (curr, phi_inv, dfg.gen_id()); 
            dfg.add_node(node);
            for ( auto n : dfg.nodes ) {
                if (_check_data_dependence(n, node)) {
                    dfg.add_edge(n,node);
                }
                if (_check_data_dependence(node, n)) {
                    dfg.add_edge(node,n);
                }
            }
        }
        // guard nodes
        int i=0;
        for ( auto &branch : curr->u_select().branches ) {
            auto node = new DFG_Node (curr, i, IRGuard::deep_copy(branch.g), dfg.gen_id()); 
            dfg.add_node(node);
            for ( auto n : dfg.nodes ) {
                if (_check_data_dependence(n, node)) {
                    dfg.add_edge(n,node);
                }
                if (_check_data_dependence(node, n)) {
                    dfg.add_edge(node,n);
                }
                if (n->t == NodeType::SelPhiInv) {
                    if (_check_guard_phi_inv_dependence(node, n)) {
                        dfg.add_edge(node,n);
                    }
                }
            }
            i++;
        }
        // branches
        for ( auto &branch : curr->u_select().branches ) {
            _build_graph (branch.seq);
        }
        // phi's
        for ( auto phi : curr->u_select().merges ) {
            // fprintf(fp, "\n// phi: ");
            // for ( auto y : phi.branch_ids ) {
            //     fprintf(fp, "%llu, ", y.m_id);
            // }
            // fprintf(fp, " - %llu",phi.post_id.m_id);
            auto node = new DFG_Node (curr, phi, dfg.gen_id()); 
            dfg.add_node(node);
            for ( auto n : dfg.nodes ) {
                if (_check_data_dependence(n, node)) {
                    dfg.add_edge(n,node);
                }
                if (_check_data_dependence(node, n)) {
                    dfg.add_edge(node,n);
                }
                if (n->t == NodeType::Guard) {
                    if (_check_guard_phi_dependence(n, node)) {
                        dfg.add_edge(n,node);
                    }
                }
            }
        }
    }
    break;
      
    case BlockType::DoLoop: {
        for ( auto iphi : curr->u_doloop().in_phis ) {
            auto node = new DFG_Node (curr, iphi, dfg.gen_id()); 
            dfg.add_node(node);
            for ( auto n : dfg.nodes ) {
                if (_check_data_dependence(n, node)) {
                    dfg.add_edge(n,node);
                }
                if (_check_data_dependence(node, n)) {
                    dfg.add_edge(node,n);
                }
            }
            // fprintf(fp, "\n// inphi: %llu, %llu",phi.bodyin_id.m_id,phi.pre_id.m_id);
            // hassert(false);
        }
        for ( auto lphi : curr->u_doloop().loop_phis ) {
            auto node = new DFG_Node (curr, lphi, dfg.gen_id()); 
            dfg.add_node(node);
            for ( auto n : dfg.nodes ) {
                if (_check_data_dependence(n, node)) {
                    dfg.add_edge(n,node);
                }
                if (_check_data_dependence(node, n)) {
                    dfg.add_edge(node,n);
                }
            }
            // fprintf(fp, "\n// loopphi: %llu, %llu, %llu, %llu",phi.pre_id.m_id,phi.bodyin_id.m_id,phi.bodyout_id.m_id, phi.post_id._getId());
        }
        // fprintf(fp, "\n\n");
        _build_graph(curr->u_doloop().branch);

        for ( auto ophi : curr->u_doloop().out_phis ) {
            auto node = new DFG_Node (curr, ophi, dfg.gen_id()); 
            dfg.add_node(node);
            for ( auto n : dfg.nodes ) {
                if (_check_data_dependence(n, node)) {
                    dfg.add_edge(n,node);
                }
                if (_check_data_dependence(node, n)) {
                    dfg.add_edge(node,n);
                }
            }
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

void Projection::_compute_connected_components ()
{
    ChpOptimize::UnionFind<DFG_Node *> uf;
    for (int i=0; i<dfg.adj.size(); i++) {
        for (int j=0; j<dfg.adj[i].size(); j++) {
            uf.union_(dfg.nodes[i],dfg.adj[i][j]);
        }
    }

    subgraphs.clear();
    for ( auto n : dfg.nodes ) {
        auto ufn = uf.find(n);
        if (!subgraphs.contains(ufn)) {
            subgraphs.insert({ufn,{}});
        }
        subgraphs[ufn].push_back(n);
    } 
}

void Projection::_insert_guard_comms ()
{
    std::vector<std::pair<DFG_Node *, DFG_Node *>> to_add;
    std::vector<std::pair<DFG_Node *, DFG_Node *>> to_delete;
    for ( int i = 0; i<dfg.nodes.size(); i++ )
    {
        if (dfg.nodes[i]->t == NodeType::Basic) {
            auto assn_blk = dfg.nodes[i]->b;
            for ( auto n = dfg.adj[i].begin(); n != dfg.adj[i].end(); n++ ) {
                auto nn = *n;
                if (nn->t == NodeType::Guard) {

                    hassert ( assn_blk->type() == BlockType::Basic) ;
                    hassert ( assn_blk->u_basic().stmt.type() == StatementType::Assign );
                    hassert ( assn_blk->u_basic().stmt.u_assign().ids.size() == 1 );
                    hassert ( g->graph.id_pool().getBitwidth(assn_blk->u_basic().stmt.u_assign().ids[0]) == 1 );

                    ChanId ci = g->graph.id_pool().makeUniqueChan(1, false);
                    var_to_actvar vtoa(s, &g->graph.id_pool());
                    ActId *id = vtoa.chanMap(ci);
                    g->name_from_chan.insert({ci, id});
                    
                    auto send = g->graph.blockAllocator().newBlock(
                        Block::makeBasicBlock(Statement::makeSend(ci, 
                        ChpExprSingleRootDag::makeVariableAccess(assn_blk->u_basic().stmt.u_assign().ids[0], 1))));
                    
                    VarId g_var = g->graph.id_pool().makeUniqueVar(1, false);

                    auto recv = g->graph.blockAllocator().newBlock(
                        Block::makeBasicBlock(Statement::makeReceive(ci, g_var)));

                    auto send_node = new DFG_Node (send, dfg.gen_id()); 
                    dfg.add_node (send_node);
                    // printf("\nsend node id: %d", send_node->id);
                    hassert (dfg.contains(send_node));

                    auto recv_node = new DFG_Node (recv, dfg.gen_id()); 
                    dfg.add_node (recv_node);
                    // printf("\nrecv node id: %d", recv_node->id);
                    hassert (dfg.contains(recv_node));

                    to_add.push_back({dfg.nodes[i], send_node});
                    to_add.push_back({recv_node, nn});
                    to_delete.push_back({dfg.nodes[i],nn});

                    auto dist_assn = g->graph.blockAllocator().newBlock(Block::makeParBlock());
                    dist_assn->u_par().branches.push_back(g->graph.blockAllocator().newSequence({send}));
                    dist_assn->u_par().branches.push_back(g->graph.blockAllocator().newSequence({recv}));

                    auto orig_sel = nn->b;

                    _splice_in_block_between (orig_sel->parent(), orig_sel, dist_assn);

                    int i=0;
                    int br_num = nn->g.first;
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
        dfg.add_edge (x.first, x.second);
    }
    for (auto x : to_delete) {
        dfg.delete_edge (x.first, x.second);
    }
}

void Projection::_insert_copies_v0 (GraphWithChanNames &gg, Sequence seq)
{
    Block *curr = seq.startseq->child();

    while (curr->type() != BlockType::EndSequence) {
    switch (curr->type()) {
    case BlockType::Basic: {
        switch (curr->u_basic().stmt.type()) {
        case StatementType::Receive: {
            auto n = dfg.find(curr);
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

#if 0
// for ITB extraction - QDI vs DI paper
void Projection::_insert_copies_v2 (GraphWithChanNames &gg, Sequence seq)
{
    Block *curr = seq.startseq->child();

    while (curr->type() != BlockType::EndSequence) {
        switch (curr->type()) {
        case BlockType::Basic: {
        }
        break;
        
        case BlockType::Par: {
            for (auto &branch : curr->u_par().branches) {
                _insert_copies_v2 (gg, branch);
            }
        }
        break;
        
        case BlockType::Select: {
            for (auto &branch : curr->u_select().branches) {
                _insert_copies_v2 (gg, branch.seq);
            }
        }
        break;
        case BlockType::DoLoop: {
            for (auto &lphi : curr->u_doloop().loop_phis) {
                hassert (curr->u_doloop().branch.startseq->child()->type()!=BlockType::EndSequence);
                _insert_copy(gg, seq, curr->u_doloop().branch.startseq->child(), curr->parent(), lphi.bodyin_id);
            }
            _insert_copies_v2 (gg, curr->u_doloop().branch);
            // for (auto &lphi : curr->u_doloop().loop_phis) {
            //     _insert_copy(gg, seq, curr->u_doloop().branch.endseq, curr, lphi.bodyout_id);
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

/*
    Due to STF, it is sufficient to rename within the sequence.
*/
void Projection::_insert_copy (GraphWithChanNames &gg, Sequence seq, DFG_Node *from, VarId v)
{
    auto b_from = from->b;

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

    _splice_in_block_between (b_from, b_from->child(), dist_assn);

    _replace_uses (gg, seq, v, copy_var, send, dist_assn);
}

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

void Projection::print_subgraphs (FILE *ff)
{
    fprintf (ff, "\n/* --- Connected components ---\n");
    for ( auto x : subgraphs ) {
        fprintf(ff, "\ncomponent : ");
        for ( auto n : x.second ) {
            fprintf(ff, "%d, ", n->id);
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

#if 0
void Projection::_splice_out_node (DFG_Node *n)
{
    if (!(n->conn)) return;
    _splice_out_block (n->b);
    n->conn = false;
}
#endif

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

void Projection::export_dot(std::string filename)
{
    FILE *ff = fopen(filename.c_str(), "w");

    std::string edge_repr = "->";
    fprintf(ff,"\ndigraph{ ");
    for (auto node : dfg.nodes)
    {
        std::stringstream ss;
        node->print(ss);
        std::ostringstream ss1;
        ss1 << ss.rdbuf();
        auto sl = ss1.str();
        fprintf(ff, "\n_%d [label=\"%d: %s\"];", node->id, node->id, sl.c_str());
    }
    for (int i=0;i<dfg.adj.size();i++)
    {
        for (int j=0;j<dfg.adj[i].size();j++) 
        {
            fprintf(ff, "\n_%d %s _%d;", dfg.nodes[i]->id, edge_repr.c_str(), dfg.adj[i][j]->id);
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