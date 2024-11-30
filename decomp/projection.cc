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

    split_assignments();
    
    // ChpOptimize::putIntoNewStaticTokenForm(g->graph);
    ChpOptimize::putIntoStaticTokenForm(g->graph);

    // fprintf(stdout, "\n/* STF \n");
    // print_chp(std::cout, g->graph);
    // fprintf(stdout, "\n*/\n");

    _build_graph(g->graph.m_seq);

    ChpOptimize::takeOutOfStaticTokenForm(g->graph);

    _insert_guard_comms();

    // fprintf(stdout, "\n/* Non-STF \n");
    // print_chp(std::cout, g->graph);
    // fprintf(stdout, "\n*/\n");

    // ChpOptimize::putIntoNewStaticTokenForm(g->graph);
    ChpOptimize::putIntoStaticTokenForm(g->graph);

    _build_graph(g->graph.m_seq);
    _compute_connected_components();

    // dfg.print_adj(stdout);
    // fprintf(stdout, "\n\n");

    // print_subgraphs();
    // fprintf(stdout, "\n\nGOT HERE 1\n\n");

    ChpOptimize::takeOutOfStaticTokenForm(g->graph);
    fprintf(stdout, "\n\nGOT HERE 2\n\n");

    int num_subgraphs = subgraphs.size();
    if (num_subgraphs==1) return;

    // for (auto subgraph : subgraphs)
    for (int i=0; i<num_subgraphs; i++)
    {
        std::vector<ActId *> tmp_names;
        auto a1 = chp_graph_to_act (*g, tmp_names, s);
        auto g1 = chp_graph_from_act (a1, s);

        dfg.clear();
        _split_assignments(g1.graph.m_seq);
        ChpOptimize::putIntoStaticTokenForm(g1.graph);
        _build_graph(g1.graph.m_seq);
        _compute_connected_components();

        fprintf(stdout, "\n\nGOT HERE 3\n\n");
        dfg.print_adj(stdout);
        fprintf(stdout, "\n\n");
        print_subgraphs();
        fprintf(stdout, "\n\n");

        auto itr = subgraphs.begin();
        for (int j=0; j<i; j++) 
            itr++;
        std::unordered_set<DFG_Node *> tmp ((*itr).second.begin(), (*itr).second.end());
        fprintf(stdout, "\n%d\n",int(tmp.size()));
        print_chp(std::cout, g1.graph);
        fprintf(stdout, "\n\nGOT HERE 4\n\n");
        _build_sub_proc_new (g1.graph.m_seq, tmp);
        fprintf(stdout, "\n\nGOT HERE 5\n\n");

        print_chp(std::cout, g1.graph);
        fprintf(stdout, "\n\nGOT HERE 6\n\n");
        if (!g1.graph.is_static_token_form)
            ChpOptimize::putIntoNewStaticTokenForm(g1.graph);
        if (g1.graph.is_static_token_form)
            ChpOptimize::takeOutOfStaticTokenForm(g1.graph);
        seqs.push_back(g1.graph.m_seq);
        fprintf(stdout, "\n\nGOT HERE 7\n\n");

        fprintf(stdout, "\n/* subproc: \n");
        // print_chp(std::cout, g1.graph);
        std::vector<ActId *> tmp_names2;
        act_chp_lang_t *tmpact = chp_graph_to_act (g1, tmp_names2, s);
        fprintf(stdout, "\n\n");
        chp_print(stdout, tmpact);
        procs.push_back(tmpact);
        fprintf(stdout, "\n*/\n");
    }

    fprintf(stdout, "\n/* got here */\n");
    // for (auto v : seqs)   
    // {
    //     std::vector<ActId *> tmp_names;
    //     g->graph.m_seq = v;
    //     act_chp_lang_t *tmp = chp_graph_to_act (*g, tmp_names, s);
    // }
    fprintf(stdout, "\n/* got here 2*/\n");
    // print_chp(std::cout, gwcns[0].graph);
    // fprintf(stdout, "\n*/\n");

    // tmp_names.clear();
    // fprintf(stdout, "\n/* fourth \n");
    // auto a2 = chp_graph_to_act (g1, tmp_names, s);
    // auto g2 = chp_graph_from_act (a2, s);
    // print_chp(std::cout, g1.graph);
    // fprintf(stdout, "\n*/\n");

    // dfg.print_adj(stdout);
    // fprintf(stdout, "\n\n");
    // ChpOptimize::takeOutOfStaticTokenForm(g->graph);
    // hassert (false);
    // _build_sub_procs();
    // fprintf(stdout, "\n\nGOT HERE 2\n\n");

    // Sequence save = g->graph.m_seq;

    // std::vector<Sequence> tmp = {}; 
    // for ( auto s : seqs ) {
    //     g->graph.m_seq = s;

    //     print_chp(std::cout, g->graph);
    //     fprintf(stdout, "\n\n");
        
    //     if (!g->graph.is_static_token_form)
    //         ChpOptimize::putIntoNewStaticTokenForm(g->graph);
    //     if (g->graph.is_static_token_form)
    //         ChpOptimize::takeOutOfStaticTokenForm(g->graph);
    //     tmp.push_back(g->graph.m_seq);
    // }
    // seqs = tmp;
    // g->graph.m_seq = save;
    // if (g->graph.is_static_token_form)
    //     ChpOptimize::takeOutOfStaticTokenForm(g->graph);
}

void Projection::_build_sub_proc_new (Sequence seq, std::unordered_set<DFG_Node *> &s)
{
    Block *curr = seq.startseq->child();
    while (curr->type() != BlockType::EndSequence) {
    switch (curr->type()) {
    case BlockType::Basic: {
        auto dfgnode = dfg.find(curr);
        if (dfgnode && s.contains(dfgnode)) {
            s.erase(dfgnode);
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
            }
        }
        curr->u_par().splits = new_splits;
        
        for (auto &branch : curr->u_par().branches) {
            _build_sub_proc_new (branch, s);
        }

        std::vector<Block::Variant_Par::PhiMerge> new_merges = {};
        for (auto phi : curr->u_par().merges) {
            auto dfgnode = dfg.find(curr, phi);
            if (dfgnode && s.contains(dfgnode)) {
                s.erase(dfgnode);
                new_merges.push_back(phi);
            }
        }
        curr->u_par().merges = new_merges;

    }
    break;
      
    case BlockType::Select: {
        auto dfgnode = dfg.find(curr);
        if (dfgnode && s.contains(dfgnode)) {
        }
    }
    break;
    case BlockType::DoLoop: {
        _build_sub_proc_new(curr->u_doloop().branch, s);
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

int Projection::_gen_sel_set_id()
{
    sel_set_id++;
    return sel_set_id;
}

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
        hassert (false);
    }
    break;
    case NodeType::LoopOutPhi: {
        hassert (false);
    }
    break;
    case NodeType::LoopLoopPhi: {
        // auto llp = node->llp;
        // ret.push_back(llp.bodyin_id);
        // if (llp.post_id) {
        //     ret.push_back(*llp.post_id);
        // }
        hassert (false);
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
        hassert (false);
    }
    break;
    case NodeType::LoopOutPhi: {
        hassert (false);
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

    auto br_ids = phi_inv_node->phi_inv.branch_ids;
    int br = guard_node->g.first;
    hassert (br < br_ids.size());
    auto br_id = (br_ids[br]);
    return (br_id) ? true : false; // this is just for my own sanity
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
            fprintf(fp, "\n// pll_phi_inv: %llu - ",phi_inv.pre_id.m_id);
            for ( auto y : phi_inv.branch_ids ) {
                if (y) fprintf(fp, "%llu, ", y._getId());
                else   fprintf(fp, "null, ");
            }
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
            fprintf(fp, "\n// pll_phi: ");
            for ( auto y : phi.branch_ids ) {
                if (y) fprintf(fp, "%llu, ", y._getId());
                else   fprintf(fp, "null, ");
            }
            fprintf(fp, " - %llu",phi.post_id.m_id);
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
            fprintf(fp, "\n// phi_inv: %llu - ",phi_inv.pre_id.m_id);
            for ( auto y : phi_inv.branch_ids ) {
                if (y) fprintf(fp, "%llu, ", y._getId());
                else   fprintf(fp, "null, ");
            }
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
            fprintf(fp, "\n// phi: ");
            for ( auto y : phi.branch_ids ) {
                fprintf(fp, "%llu, ", y.m_id);
            }
            fprintf(fp, " - %llu",phi.post_id.m_id);
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
      
    case BlockType::DoLoop: {
        for ( auto phi : curr->u_doloop().in_phis ) {
            fprintf(fp, "\n// inphi: %llu, %llu",phi.bodyin_id.m_id,phi.pre_id.m_id);
            hassert(false);
        }
        for ( auto phi : curr->u_doloop().out_phis ) {
            fprintf(fp, "\n// outphi: %llu, %llu",phi.bodyout_id.m_id,phi.post_id.m_id);
            // hassert(false);
        }
        for ( auto phi : curr->u_doloop().loop_phis ) {
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
            fprintf(fp, "\n// loopphi: %llu, %llu, %llu, %llu",phi.pre_id.m_id,phi.bodyin_id.m_id,phi.bodyout_id.m_id, phi.post_id._getId());
        }
        fprintf(fp, "\n\n");
        _build_graph(curr->u_doloop().branch);
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
                    auto send = g->graph.blockAllocator().newBlock(
                        Block::makeBasicBlock(Statement::makeSend(ci, 
                        ChpExprSingleRootDag::makeVariableAccess(assn_blk->u_basic().stmt.u_assign().ids[0], 1))));
                    
                    VarId g_var = g->graph.id_pool().makeUniqueVar(1, false);

                    auto recv = g->graph.blockAllocator().newBlock(
                        Block::makeBasicBlock(Statement::makeReceive(ci, g_var)));

                    auto send_node = new DFG_Node (send, dfg.gen_id()); 
                    send_node->conn = false;
                    dfg.add_node (send_node);
                    printf("\nsend node id: %d", send_node->id);
                    hassert (dfg.contains(send_node));

                    auto recv_node = new DFG_Node (recv, dfg.gen_id()); 
                    recv_node->conn = false;
                    dfg.add_node (recv_node);
                    printf("\nrecv node id: %d", recv_node->id);
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

void Projection::print_subgraphs ()
{
    fprintf (fp, "\n/* --- Connected components ---\n");
    for ( auto x : subgraphs ) {
        fprintf(fp, "\ncomponent : ");
        for ( auto n : x.second ) {
            fprintf(fp, "%d, ", n->id);
        }
    }
    fprintf (fp, "\n\n   --- Connected components --- */ \n");
}

void Projection::_build_sub_procs ()
{
    seqs.clear();
    if (subgraphs.size() == 1) {
        return;
    }

    int i=0;
    for ( auto x : subgraphs ) {
        auto dfg_vec = x.second;
        // bool build = true;
        // only build linear subgraphs (for now)
        // for ( auto n : dfg_nodes ) {
            // if (n->t != NodeType::Basic) build = false;
        // }
        // if (build) 
        printf("\nbuilding proc %d", i);
        std::unordered_set<DFG_Node *> tmp (dfg_vec.begin(), dfg_vec.end());
        seqs.push_back (_build_sub_proc(g->graph.m_seq, tmp));
        // hassert (tmp.empty());
        i++;
    }
}

Sequence Projection::_build_sub_proc (Sequence seq, std::unordered_set<DFG_Node *> &nodes)
{
    Sequence ret;
    std::vector<Block *> blks = {};
    Block *curr = seq.startseq->child();
    while (curr->type() != BlockType::EndSequence) {
    switch (curr->type()) {
    case BlockType::Basic: {
        auto dfgnode = dfg.find(curr);
        if (dfgnode && nodes.contains(dfgnode)) {
            blks.push_back(curr);
            nodes.erase(dfgnode);
        }
    }
    break;
      
    case BlockType::Par: {
        hassert (false);
        for (auto &branch : curr->u_par().branches) {
            _build_sub_proc (branch, nodes);
        }
    }
    break;
      
    case BlockType::Select: {
        auto dfgnode = dfg.find(curr);
        if (dfgnode && nodes.contains(dfgnode)) {
            // collect all 
            // build the new selection
            // erase the dfg nodes
            auto b = _build_selection (dfgnode, nodes);
            // for (auto &branch : curr->u_select().branches) {
            //     _build_sub_proc (branch.seq, nodes);
            // }
            blks.push_back(b);
        }
    }
    break;
    case BlockType::DoLoop: {
        ret = _build_sub_proc(curr->u_doloop().branch, nodes);
        return _wrap_in_do_loop(ret);
    }
    break;
    
    case BlockType::StartSequence:
    case BlockType::EndSequence:
        hassert(false);
        break;
    }
    curr = curr->child();
    }
    hassert (!(blks.empty()));
    _splice_out_blocks (blks);
    return g->graph.blockAllocator().newSequence(blks);
}

Block *Projection::_build_selection (DFG_Node *n, std::unordered_set<DFG_Node *> &nodes)
{
    Block *orig = n->b;
    hassert (orig->type() == BlockType::Select);
    std::vector<DFG_Node *> v;
    std::unordered_set<int> brs;
    std::unordered_map<int, DFG_Node *> gs;
    for ( auto x : nodes )
    {
        if (x->b == n->b) {
            v.push_back(x);
            if (x->t == NodeType::Guard) {
                gs.insert( {(x->g).first, x} );
                brs.insert((x->g).first);
            }
        }
    }
    for ( auto x : v )
    {
        nodes.erase(x);
    }
    Block *newsel = g->graph.blockAllocator().newBlock(Block::makeSelectBlock());
    // just need to see which selection branches to put in the new one
    // leave the rest
    int i=0;
    for ( auto &sel_br : orig->u_select().branches ) {
        auto seq = sel_br.seq;
        if (brs.contains(i)) {
            newsel->u_select().branches.push_back({seq, IRGuard::deep_copy((gs[i])->g.second)});
        }
        i++;
    }
    newsel->u_select().branches.push_back({g->graph.blockAllocator().newSequence({}), IRGuard::makeElse()});
    return newsel;
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

void Projection::_splice_out_node (DFG_Node *n)
{
    if (!(n->conn)) return;
    _splice_out_block (n->b);
    n->conn = false;
}

void Projection::split_assignments()
{
    _split_assignments(g->graph.m_seq);
}

void Projection::split_selections()
{
    _split_selections(g->graph.m_seq);
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