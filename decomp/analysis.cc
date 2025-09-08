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

#include "analysis.h"

using namespace ChpOptimize;

// TODO: fix this name generation, need to get the original mapped names
std::string str_of_id(OptionalVarId id) {
    return id ? "_va" + std::to_string(((*id).m_id)-1) : "vNULL";
}

std::string str_of_id(ChanId id) { 
    return "C" + std::to_string(id.m_id); 
}

std::unordered_map<const Block *, decomp_info_t *> DecompAnalysis::get_decomp_info_map()
{
    return decomp_info_map;
}

int DecompAnalysis::_compute_total_bits (std::unordered_set<VarId> vars)
{
    int tot_bw = 0;
    std::unordered_set<VarId>::iterator itr;
    IdPool idp = g->graph.id_pool();
    for (itr = vars.begin(); itr != vars.end(); itr++)
    {
        tot_bw += idp.getBitwidth(*itr);
    }
    return tot_bw;
}

void DecompAnalysis::analyze ()
{
    auto [lim, lom] = getLiveVars (g->graph);
    _populate_decomp_info_map (lim, lom);
}

void DecompAnalysis::_populate_decomp_info_map (
            std::unordered_map<const Block *, std::unordered_set<VarId>> lim,
            std::unordered_map<const Block *, std::unordered_set<VarId>> lom)
{
    for ( auto x : lim )
    {
        auto li_vars = x.second;
        //check if same block exists in both maps
        hassert (lom.count(x.first)); 
        auto lo_vars = lom.find(x.first)->second;

        decomp_info_t *di;
        di = new decomp_info_t;
        di->live_in_vars.clear();
        di->live_out_vars.clear();
        di->live_in_vars = li_vars;
        di->live_out_vars = lo_vars;
        di->live_in_vec.clear();
        di->live_out_vec.clear();
        di->total_bitwidth_in = _compute_total_bits (li_vars);
        di->total_bitwidth_out = _compute_total_bits (lo_vars);
        di->break_before = false;
        di->break_after = false;
        decomp_info_map.insert({x.first, di});
    }
}

void DecompAnalysis::_print_decomp_info (decomp_info_t *di)
{
    if (!di) return;
    listitem_t *li;
    fprintf(fp, "\n-----------");
    fprintf(fp, "\nnecessary input transmissions");
    fprintf(fp, "\nif ring is broken just before here:\n");
    std::unordered_set<VarId>::iterator itr;

    for (itr = di->live_in_vars.begin(); itr != di->live_in_vars.end(); itr++)
    {
        fprintf(fp, "%s, ", (str_of_id(*itr)).c_str());
    }	     

    fprintf(fp, "\nnecessary output transmissions");
    fprintf(fp, "\nif ring is broken just after here:\n");

    for (itr = di->live_out_vars.begin(); itr != di->live_out_vars.end(); itr++)
    {
        fprintf(fp, "%s, ", (str_of_id(*itr)).c_str());
    }	     
    fprintf(fp, "\n-----------");
    fprintf(fp, "\ntotal bits live_in: %d", di->total_bitwidth_in);
    fprintf(fp, "\n-----------");
    fprintf(fp, "\n-----------");
    fprintf(fp, "\ntotal bits live_out: %d", di->total_bitwidth_out);
    fprintf(fp, "\n-----------");
    fprintf(fp, "\nbreak before?: %d", di->break_before);
    fprintf(fp, "\nbreak after?: %d", di->break_after);
    fprintf(fp, "\n-----------");
    fprintf(fp, "\n\n");
}

void DecompAnalysis::_print_decomp_info (Sequence seq, int root)
{
    Block *curr = seq.startseq->child();
    decomp_info *di;

    while (curr->type() != BlockType::EndSequence) {
    switch (curr->type()) {
    case BlockType::Basic: {
        switch (curr->u_basic().stmt.type()) {
        case StatementType::Assign:
            fprintf (fp, "reached assign\n");
            break;
        case StatementType::Send:
            fprintf (fp, "reached send\n");
            break;
        case StatementType::Receive:
            fprintf (fp, "reached recv\n");
            break;
      }
        di = (decomp_info_map.find(curr))->second;
        _print_decomp_info (di);
    }
    break;
      
    case BlockType::Par: {
        fprintf (fp, "reached parallel start\n");

        di = (decomp_info_map.find(curr))->second;
        _print_decomp_info (di);

        for (auto &branch : curr->u_par().branches) {
            _print_decomp_info (branch, 0);
        }
        fprintf (fp, "reached parallel end\n");
    }
    break;
      
    case BlockType::Select:
        fprintf (fp, "reached select start\n");

        di = (decomp_info_map.find(curr))->second;
        _print_decomp_info (di);

        for (auto &branch : curr->u_select().branches) {
            _print_decomp_info (branch.seq, 0);
        }

        fprintf (fp, "reached select end\n");
    break;
      
    case BlockType::DoLoop:
        fprintf (fp, "\n\nreached do-loop\n");
        di = (decomp_info_map.find(curr))->second;
        _print_decomp_info (di);
        _print_decomp_info (curr->u_doloop().branch, 0);
        break;
    
    case BlockType::StartSequence:
    case BlockType::EndSequence:
        hassert(false);
        break;
    }
    curr = curr->child();
    }
}

void DecompAnalysis::print_decomp_info ()
{
    _print_decomp_info (g->graph.m_seq, 1);
}