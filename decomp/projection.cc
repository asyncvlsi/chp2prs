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

std::vector<Sequence> Projection::get_procs ()
{
    return seqs;
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
    
    ChpOptimize::putIntoNewStaticTokenForm(g->graph);

    fprintf(stdout, "\n/* first \n");
    print_chp(std::cout, g->graph);
    fprintf(stdout, "\n*/\n");

    split_assignments();

    _build_graph(g->graph.m_seq);

    dfg.print_adj(stdout);
    fprintf(stdout, "\n\n");

    _compute_connected_components();

    fprintf(stdout, "\n/* second \n");
    print_chp(std::cout, g->graph);
    fprintf(stdout, "\n*/\n");

    ChpOptimize::takeOutOfStaticTokenForm(g->graph);
    _build_sub_procs();

    Sequence save = g->graph.m_seq;

    std::vector<Sequence> tmp = {}; 
    for ( auto s : seqs ) {
        g->graph.m_seq = s;
        if (!g->graph.is_static_token_form)
            ChpOptimize::putIntoNewStaticTokenForm(g->graph);
        if (g->graph.is_static_token_form)
            ChpOptimize::takeOutOfStaticTokenForm(g->graph);
        tmp.push_back(g->graph.m_seq);
    }
    seqs = tmp;
    g->graph.m_seq = save;
}

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
        for (auto &branch : curr->u_par().branches) {
            _build_graph (branch);
        }
    }
    break;
      
    case BlockType::Select: {
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
        for ( auto &branch : curr->u_select().branches ) {
            _build_graph (branch.seq);
        }
        }
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
            hassert(false);
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

    for ( auto x : subgraphs ) {
        auto dfg_nodes = x.second;
        bool build = true;
        // only build linear subgraphs (for now)
        for ( auto n : dfg_nodes ) {
            if (n->t != NodeType::Basic) build = false;
        }
        if (build) seqs.push_back(_build_sub_proc(dfg_nodes));
    }
}

Sequence Projection::_build_sub_proc (std::vector<DFG_Node *> nodes)
{
    bool wrap = true;
    std::vector<Block *> blks = {};
    auto node = nodes.begin();
    while ( node != nodes.end() ) 
    {
        switch ((*node)->t) {
        case NodeType::Basic: {
            _splice_out_node(*node);
            blks.push_back((*node)->b);
        }
        break;
        case NodeType::LoopLoopPhi: {
            hassert(false);
        }
        break;
        case NodeType::LoopInPhi: {
            hassert(false);
        }
        break;
        case NodeType::LoopOutPhi: {
            hassert(false);
        }
        break;
        case NodeType::SelPhiInv: {
            hassert(false);
        }
        break;
        case NodeType::SelPhi: {
            hassert(false);
        }
        break;
        default:
            hassert(false);
        break;
        }
        node++;
    }
    return _wrap_in_do_loop(g->graph.newSequence(blks));
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