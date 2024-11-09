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

std::vector<Sequence> Projection::get_procs()
{
    return v_seqs;
}

bool Projection::_check_linear(Sequence seq, int root)
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

    _build_graph(g->graph.m_seq);
    dfg.print_adj(stdout);

    _compute_connected_components();

    ChpOptimize::takeOutOfStaticTokenForm(g->graph);
}

/*
    True if dependent
*/
bool Projection::_check_data_dependence (DFG_Node *prev, DFG_Node *curr) 
{
    hassert(dfg.node_exists(prev));
    hassert(dfg.node_exists(curr));
    Block *b_prev = prev->b;
    Block *b_curr = curr->b;

    hassert (b_prev->type()==BlockType::Basic);
    hassert (b_curr->type()==BlockType::Basic);

    if (b_curr->u_basic().stmt.type() == StatementType::Receive) 
        return false;

    // curr is either send or assign
    switch (b_prev->u_basic().stmt.type()) {
    case StatementType::Send:
        // why? - STF
        return false;
        break;
    case StatementType::Assign: {
            bool curr_is_send = (b_curr->u_basic().stmt.type() == StatementType::Send);
            auto pvars = b_prev->u_basic().stmt.u_assign().ids;
            auto vmap = (curr_is_send) ?
                        getIdsUsedByExpr(b_curr->u_basic().stmt.u_send().e) :
                        getIdsUsedByExpr(b_curr->u_basic().stmt.u_assign().e) ;
            bool ret = false;
            for ( auto pvar : pvars ) {
                if (vmap.contains(pvar)) ret = true;
            }
            return ret;
        }
        break;
    case StatementType::Receive: {
            bool curr_is_send = (b_curr->u_basic().stmt.type() == StatementType::Send);
            auto pvar = b_prev->u_basic().stmt.u_receive().var;
            if (!pvar) return false;
            auto vmap = (curr_is_send) ?
                        getIdsUsedByExpr(b_curr->u_basic().stmt.u_send().e) :
                        getIdsUsedByExpr(b_curr->u_basic().stmt.u_assign().e) ;
            return (vmap.contains(*pvar));
        }
        break;
    }
    hassert (false);
    return false;
}

void Projection::_build_graph(Sequence seq)
{
    Block *curr = seq.startseq->child();
    bool ret = true;

    while (curr->type() != BlockType::EndSequence) {
    switch (curr->type()) {
    case BlockType::Basic: {
        auto node = new DFG_Node (curr, dfg.gen_id()); 
        dfg.add_node(node);
        for ( auto n : dfg.nodes ) {
            if (_check_data_dependence(n, node)) {
                dfg.add_edge(n,node);
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

void Projection::_compute_connected_components()
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

void Projection::print_subgraphs(FILE *fp)
{
    fprintf (fp, "\n--- Connected components ---\n");
    for ( auto x : subgraphs ) {
        fprintf(fp, "\ncomponent : ");
        for ( auto n : x.second ) {
            fprintf(fp, "%d, ", n->id);
        }
    }
    fprintf (fp, "\n\n--- Connected components ---\n");
}