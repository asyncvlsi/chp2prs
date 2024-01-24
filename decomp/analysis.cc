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

std::unordered_map<Block *, decomp_info_t *> DecompAnalysis::get_live_vars_map()
{
    return live_in_vars_map;
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
    total_bits = 0;
    H_live.clear();
    H_saved.clear();
    H_parents.clear();
    live_in_vars_map.clear();
    // fprintf (stdout, "\nanalyzing... \n");
    _generate_decomp_info (g->graph.m_seq, 1);
}

void DecompAnalysis::_add_to_live_vars (VarId vid)
{
    H_live.insert(vid);
}

void DecompAnalysis::_add_to_live_vars (std::unordered_set<VarId> vids)
{
    std::unordered_set<VarId>::iterator itr;
    for (itr = vids.begin(); itr != vids.end(); itr++)
    {
        H_live.insert(*itr);
    }
}

void DecompAnalysis::_map_block_to_live_vars (Block *b, decomp_info_t *di)
{
    live_in_vars_map.insert({b, di});
}

void DecompAnalysis::_remove_from_live_vars (VarId vid)
{
    H_live.erase(vid);
}

void DecompAnalysis::_generate_decomp_info (Sequence seq, int root)
{
    // Block *curr = seq.startseq->child();
    Block *curr = seq.endseq->parent();
    decomp_info *di;
    std::unordered_set<VarId> T, T_out;
    std::vector<std::unordered_set<VarId>> Si_s;

    while (curr->type() != BlockType::StartSequence) {
    switch (curr->type()) {
    case BlockType::Basic: {
        switch (curr->u_basic().stmt.type()) {
        case StatementType::Assign:
            // fprintf (stdout, "reached assign\n");
            for (auto it = curr->u_basic().stmt.u_assign().ids.begin(); 
                        it != curr->u_basic().stmt.u_assign().ids.end(); it++)
            {
                _remove_from_live_vars (*it);
            }
            _add_to_live_vars (getIdsUsedByExpr(curr->u_basic().stmt.u_assign().e));
            di = _generate_decomp_info ();
            // _print_decomp_info (di);
            _map_block_to_live_vars (curr, di);
            break;

        case StatementType::Send:
            // fprintf (stdout, "reached send\n");
            _add_to_live_vars (getIdsUsedByExpr (curr->u_basic().stmt.u_send().e) );
            di = _generate_decomp_info ();
            // _print_decomp_info (di);
            _map_block_to_live_vars (curr, di);
            break;

        case StatementType::Receive:
            // fprintf (stdout, "reached recv\n");
            if (curr->u_basic().stmt.u_receive().var != OptionalVarId::null_id())
                _remove_from_live_vars (*curr->u_basic().stmt.u_receive().var);
            di = _generate_decomp_info ();
            // _print_decomp_info (di);
            _map_block_to_live_vars (curr, di);
            break;
      }
    }
    break;
      
    case BlockType::Par: {
        // fatal_error ("working on par...");
        for (auto &branch : curr->u_par().branches) {
            _generate_decomp_info (branch, 1);
        }
    }
    break;
      
    case BlockType::Select: {
        // fprintf (stdout, "reached select 1\n");
        _init_union();
        T = H_live;
        Si_s.clear();
        // H_live.clear();

        for (auto &branch : curr->u_select().branches) {

            _save_state_live_vars ();

            _generate_decomp_info (branch.seq, 1);

            Si_s.push_back(H_live);

            _h_live_union_h_parent ();

            _restore_state_live_vars ();
        }
        H_live.clear();

        for (auto &branch : curr->u_select().branches) {
            _add_to_live_vars (getIdsUsedByExpr(branch.g.u_e().e));
        }
        _h_live_union_h_parent ();

        _restore_live_vars_from_parent ();

        // for all vs in T, if v is in none of the Si_s, remove it from T
        T_out = _prune_T (T, Si_s);

        H_live = _set_union (H_live, T_out);

        di = _generate_decomp_info ();
        // fprintf (stdout, "reached select\n");
        // _print_decomp_info (di);
        _map_block_to_live_vars (curr, di);

        _free_union();

        // TODO: can use this for sth?
        // deal with guards, phiinv, and phi
        if (!curr->u_select().splits.empty()
            || !curr->u_select().merges.empty()) {
        }
        // phiinv
        for (auto &split : curr->u_select().splits) {
        std::vector<OptionalChanId> out;
        }
        for (auto &branch : curr->u_select().branches) {
        }
        for (auto &merge : curr->u_select().merges) {
            std::vector<ChanId> inp;
        }
    }
    break;
      
    case BlockType::DoLoop:
        // fprintf (stdout, "reached do-loop\n");
        _generate_decomp_info (curr->u_doloop().branch, 1);
#if 0
        if (curr->u_doloop().loop_phis.size() > 0
        || curr->u_doloop().in_phis.size() > 0
        || curr->u_doloop().out_phis.size() > 0 ) {
        // guards = nodes_add_loopguard (curr->u_doloop(), d, dm);
        }
        // handle loop-phis first
        for (auto &loopphi : curr->u_doloop().loop_phis) {
        std::vector<OptionalChanId> outp;

        // ChanId feedback = dm.fresh (loopphi.bodyout_id);
        if (loopphi.post_id) {
        //   outp.push_back (dm.mapvar (*loopphi.post_id));
        }
        else {
        outp.push_back (OptionalChanId::null_id());
        }
        // outp.push_back (feedback);
        // d.push_back (Dataflow::mkSplit
        // 	     (guards.first, dm.mapvar (loopphi.bodyout_id),
        // 	      outp));
        std::vector<ChanId> inp;
        // inp.push_back (dm.mapvar (loopphi.pre_id));
        // inp.push_back (feedback);
        // d.push_back (Dataflow::mkMergeMix
        // 	     (guards.second, inp, dm.mapvar (loopphi.bodyin_id)));
        }

        for (auto &inphi : curr->u_doloop().in_phis) {
        std::vector<ChanId> inp;
        // ChanId feedback = dm.fresh (inphi.pre_id);
        // inp.push_back (dm.mapvar (inphi.pre_id));
        // inp.push_back (feedback);
        // d.push_back (Dataflow::mkMergeMix
        // 	     (OptionalChanId{guards.second}, inp,
        // 	      dm.mapvar (inphi.bodyin_id)));

        std::vector<OptionalChanId> outp;
        outp.push_back (OptionalChanId::null_id());
        // outp.push_back (feedback);
        // d.push_back (Dataflow::mkSplit
        // 	     (guards.first, dm.mapvar (inphi.bodyin_id),
        // 	      outp));
        }

        for (auto &outphi : curr->u_doloop().out_phis) {
        std::vector<OptionalChanId> outp;
        outp.push_back (OptionalChanId::null_id());
        // outp.push_back (dm.mapvar (outphi.post_id));
        // d.push_back (Dataflow::mkSplit
        // 	     (guards.first, dm.mapvar (outphi.bodyout_id),
        // 	      outp));
        }
#endif
        break;
    
    case BlockType::StartSequence:
    case BlockType::EndSequence:
        hassert(false);
        break;
    }
    curr = curr->parent();
    }
}

void DecompAnalysis::_save_state_live_vars ()
{
    // H_saved.clear();
    H_saved = H_live;
}

void DecompAnalysis::_restore_state_live_vars ()
{
    // H_live.clear();
    H_live = H_saved;
}

void DecompAnalysis::_init_union ()
{
    std::unordered_set<VarId> h_p;
    // h_p = H_live;
    h_p.clear();
    H_parents.push_back(h_p);
}

void DecompAnalysis::_free_union ()
{
    // hash_free (H_parent);
    H_parents.pop_back();
}

void DecompAnalysis::_h_live_union_h_parent ()
{
    // Hashtable *h_p = (Hashtable *)stack_peek (H_parents);
    // hash_bucket_t *b;
    // hash_iter_t itr;
    // hash_iter_init (H_live, &itr);
    // while ((b = hash_iter_next(H_live, &itr))) 
    // {
    //     if (!hash_lookup(h_p, b->key))
    //     {
    //         hash_add (h_p, b->key);
    //     }
    // }
    std::unordered_set<VarId> h_p = H_parents.back();
    std::unordered_set<VarId>::iterator itr;
    for (itr = H_live.begin(); itr != H_live.end(); itr++)
    {
        if(!h_p.contains(*itr))
        {
            h_p.insert(*itr);
        }
    }
    // H_parents.back() = h_p;
    H_parents.pop_back();
    H_parents.push_back(h_p);
}

std::unordered_set<VarId> DecompAnalysis::_set_union (std::unordered_set<VarId> input1, std::unordered_set<VarId> input2)
{
    std::unordered_set<VarId> output;
    output.clear();
    std::unordered_set<VarId>::iterator itr;
    for (itr = input1.begin(); itr != input1.end(); itr++)
    {
        output.insert(*itr);
    }
    std::unordered_set<VarId>::iterator itr2;
    for (itr2 = input2.begin(); itr2 != input2.end(); itr2++)
    {
        if (!output.contains(*itr2))
            output.insert(*itr2);
    }
    return output;
}

std::unordered_set<VarId> DecompAnalysis::_prune_T (std::unordered_set<VarId> T_in, std::vector<std::unordered_set<VarId>> Si_s_in)
{
    std::unordered_set<VarId> out;
    out.clear();
    bool keep;
    std::unordered_set<VarId>::iterator itr;
    for (itr = T_in.begin(); itr != T_in.end(); itr++)
    {
        keep = false;
        for (auto Si = Si_s_in.begin() ; Si != Si_s_in.end() ; ++Si )
        {
            if (Si->contains(*itr))
            {    
                keep = true;
                break;
            }
        }
        if (keep)
            out.insert(*itr);
    }
    return out;
}

void DecompAnalysis::_restore_live_vars_from_parent ()
{
    // hash_free (H_live);
    // Hashtable *h_p = (Hashtable *) stack_peek (H_parents);
    // H_live = hash_new (4);
    // hash_bucket_t *b;
    // hash_iter_t itr;
    // hash_iter_init (h_p, &itr);
    // while ((b = hash_iter_next(h_p, &itr))) 
    // {
    //     hash_add (H_live, b->key);
    // }
    std::unordered_set<VarId> h_p = H_parents.back();
    H_live.clear();
    std::unordered_set<VarId>::iterator itr;
    for (itr = h_p.begin(); itr != h_p.end(); itr++)
    {
        H_live.insert(*itr);
    }
}

decomp_info_t *DecompAnalysis::_generate_decomp_info()
{
    decomp_info_t *di;
    NEW (di, decomp_info_t);
    di->tx_vars = H_live;
    di->total_bitwidth = _compute_total_bits (H_live);
    di->is_breakpoint = false;
    return di;
}

decomp_info_t *DecompAnalysis::_generate_decomp_info(std::unordered_set<VarId> H)
{
    decomp_info_t *di;
    NEW (di, decomp_info_t);
    di->tx_vars = H;
    di->total_bitwidth = _compute_total_bits (H);
    di->is_breakpoint = false;
    return di;
}

void DecompAnalysis::_print_decomp_info (decomp_info_t *di)
{
    if (!di) return;
    listitem_t *li;
    fprintf(fp, "\n-----------");
    fprintf(fp, "\nnecessary input transmissions");
    fprintf(fp, "\nif ring is broken just before here:\n");
    std::unordered_set<VarId>::iterator itr;
    for (itr = di->tx_vars.begin(); itr != di->tx_vars.end(); itr++)
    {
        // ActId *id = t->varMap(*itr);
        // char name[1024];
        // id->sPrint(name, 1024);
        // fprintf (fp, "%s, ", name);
        fprintf(fp, "%s, ", (str_of_id(*itr)).c_str());
    }	     
    fprintf(fp, "\n-----------");
    fprintf(fp, "\ntotal bits: %d", di->total_bitwidth);
    fprintf(fp, "\n-----------");
    fprintf(fp, "\nbreak?: %d", di->is_breakpoint);
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
            fprintf (stdout, "reached assign\n");
            break;
        case StatementType::Send:
            fprintf (stdout, "reached send\n");
            break;
        case StatementType::Receive:
            fprintf (stdout, "reached recv\n");
            break;
      }
        di = (live_in_vars_map.find(curr))->second;
        _print_decomp_info (di);
    }
    break;
      
    case BlockType::Par: {
        // fatal_error ("working on par...");
        for (auto &branch : curr->u_par().branches) {
            _print_decomp_info (branch, 0);
        }
    }
    break;
      
    case BlockType::Select:
        fprintf (stdout, "reached select start\n");

        di = (live_in_vars_map.find(curr))->second;
        _print_decomp_info (di);

        for (auto &branch : curr->u_select().branches) {
            _print_decomp_info (branch.seq, 0);
        }

        fprintf (stdout, "reached select end\n");
    break;
      
    case BlockType::DoLoop:
        fprintf (stdout, "\n\nreached do-loop\n");
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

#if 0
decomp_info_t *DecompAnalysis::_tag_action_with_decomp_info_union_lcd ()
{
    hash_bucket_t *b, *b2;
    char *s;
    list_t *req_vars = list_new();
    hash_iter_t itr, itr2;
    hash_iter_init (H_lcd, &itr);
    while ((b = hash_iter_next(H_lcd, &itr))) 
    {
        s = new char;
        if (!hash_lookup(H_live, b->key))
        {
            strcpy(s, b->key);
            list_append (req_vars, s);
        }
    }
    hash_iter_init (H_live, &itr2);
    while ((b2 = hash_iter_next(H_live, &itr2))) 
    {
        s = new char;
        strcpy(s, b2->key);
        list_append (req_vars, s);
    }	     
    decomp_info_t *di;
    NEW (di, decomp_info_t);
    di->tx_vars = list_dup (req_vars);
    di->total_bitwidth = _compute_total_bits(req_vars);
    di->is_breakpoint = list_isempty(req_vars);
    return di;
}
#endif