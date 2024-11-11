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
    if (!_check_linear(g->graph.m_seq,1))
        return;
    
    ChpOptimize::putIntoNewStaticTokenForm(g->graph);

    fprintf(stdout, "\n/*\n");
    print_chp(std::cout, g->graph);
    fprintf(stdout, "\n*/\n");

    split_assignments();

    _build_graph(g->graph.m_seq);

    dfg.print_adj(stdout);

    _compute_connected_components();

    fprintf(stdout, "\n/*\n");
    print_chp(std::cout, g->graph);
    fprintf(stdout, "\n*/\n");

    _build_sub_procs();

    ChpOptimize::takeOutOfStaticTokenForm(g->graph);
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

    bool ret = false;

    for ( auto v : defs ) {
        if (uses.contains(v)) ret = true;
    }
    return ret;
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
        fatal_error ("selections w.i.p.");
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
        // ChpOptimize::takeOutOfStaticTokenForm(g->graph);
        // seqs.push_back(g->graph.m_seq);
        return;
    }

    for ( auto x : subgraphs ) {
        auto dfg_nodes = x.second;
        std::vector<Block *> blks = {};
        for ( auto y : dfg_nodes ) {
            // TODO: this will be more complex when selections + loops need to be handled
            _splice_out_block(y->b);
            blks.push_back(y->b);
            
        }
        Sequence seq = g->graph.newSequence(blks);
        seqs.push_back(_wrap_in_do_loop(seq));
    }
}

void Projection::split_assignments()
{
    _split_assignments(g->graph.m_seq);
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