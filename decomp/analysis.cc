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


std::string str_of_id(OptionalVarId id) {
    return id ? "v" + std::to_string((*id).m_id) : "vNULL";
}
std::string str_of_id(ChanId id) { return "C" + std::to_string(id.m_id); }


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
        fatal_error ("working on par...");
        for (auto &branch : curr->u_par().branches) {
        // ms = createDataflow (branch, dm, d);
        // acc.datamap = Algo::set_union (acc.datamap, ms.datamap);
        // acc.ctrlmap = Algo::set_union (acc.ctrlmap, ms.ctrlmap);
        }
        //   seqs.push_back (acc);
    }
    break;
      
    case BlockType::Select:
        _init_union();
        for (auto &branch : curr->u_select().branches) {

            _save_state_live_vars ();

            _generate_decomp_info (branch.seq, 1);

            _h_live_union_h_parent ();

            _restore_state_live_vars ();
        }

        _restore_live_vars_from_parent ();

        for (auto &branch : curr->u_select().branches) {
            _add_to_live_vars (getIdsUsedByExpr(branch.g.u_e().e));
        }

        di = _generate_decomp_info ();
        // fprintf (stdout, "reached select\n");
        // _print_decomp_info (di);
        _map_block_to_live_vars (curr, di);

        _free_union();

        // deal with guards, phiinv, and phi
        // ChanId guard;
        // bool swap = false;
        if (!curr->u_select().splits.empty()
            || !curr->u_select().merges.empty()) {
        // we need a guard!
        //   guard = nodes_add_guard (curr->u_select(), swap, d, dm);
        }
        // phiinv
        for (auto &split : curr->u_select().splits) {
        std::vector<OptionalChanId> out;
        //   out = Algo::map1<OptionalChanId> (split.branch_ids,
        // 		    [&] (OptionalVarId v) {
        // 		      if (v) {
        // 			// return OptionalChanId{dm.mapvar ((*v))};
        // 		      }
        // 		      else {
        // 			return OptionalChanId::null_id();
        // 		      }
        // 		    });
        // if (swap) {
        //     OptionalChanId c0 = out[0];
        //     OptionalChanId c1 = out[1];
        //     out[1] = c0;
        //     out[0] = c1;
        // }
        //   d.push_back (Dataflow::mkSplit(guard,dm.mapvar (split.pre_id), out));
        }
        for (auto &branch : curr->u_select().branches) {
        }
        for (auto &merge : curr->u_select().merges) {
            std::vector<ChanId> inp;
            //   inp = Algo::map1<ChanId> (merge.branch_ids,
            // 			    [&] (VarId v) { return dm.mapvar (v); });
            // if (swap) {
            //     ChanId c0 = inp[0];
            //     ChanId c1 = inp[1];
            //     inp[1] = c0;
            //     inp[0] = c1;
            // } 
            //   d.push_back (Dataflow::mkMergeMix (OptionalChanId{guard}, inp,
            // 				     dm.mapvar (merge.post_id)));
        }
    break;
      
    case BlockType::DoLoop:
        fprintf (stdout, "reached do-loop\n");
        _generate_decomp_info (curr->u_doloop().branch, 1);
        //   MultiChannelState ms = createDataflow (curr->u_doloop().branch, dm, d);
        // deal with loopphi, phiinv, loopphinv
        // std::pair<ChanId, ChanId> guards;

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
    h_p = H_live;
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
    di->is_breakpoint = (di->total_bitwidth == 0);
    return di;
}

decomp_info_t *DecompAnalysis::_generate_decomp_info(std::unordered_set<VarId> H)
{
    decomp_info_t *di;
    NEW (di, decomp_info_t);
    di->tx_vars = H;
    di->total_bitwidth = _compute_total_bits (H);
    di->is_breakpoint = (di->total_bitwidth == 0);
    return di;
}

void DecompAnalysis::_print_decomp_info (decomp_info_t *di)
{
    if (!di) return;
    listitem_t *li;
    fprintf(fp, "\n-----------");
    fprintf(fp, "\nnecessary input transmissions:");
    fprintf(fp, "\n(if ring is broken just before here)\n");
    std::unordered_set<VarId>::iterator itr;
    for (itr = di->tx_vars.begin(); itr != di->tx_vars.end(); itr++)
    {
        fprintf(fp, "%s, ", (str_of_id(*itr)).c_str());
    }	     
    fprintf(fp, "\n-----------");
    fprintf(fp, "\ntotal bits: %d", di->total_bitwidth);
    fprintf(fp, "\n-----------");
    fprintf(fp, "\nbreak?: %d", di->is_breakpoint);
    fprintf(fp, "\n-----------");
    fprintf(fp, "\n\n");
}

#if 0

void DecompAnalysis::print_decomp_info ()
{
    _print_decomp_info (c, 1);
}

void DecompAnalysis::_print_decomp_info (act_chp_lang_t *c_t, int root)
{
    listitem_t *li;
    list_t *copy_list;
    act_chp_lang_t *stmt;
    act_chp_gc_t *gc;

    if (!c_t) return;

    switch (c_t->type) {
    case ACT_CHP_COMMALOOP:
    case ACT_CHP_SEMILOOP:
        fatal_error ("Replication loops should've been removed..");
        break;
        
    case ACT_CHP_COMMA:
    case ACT_CHP_SEMI:
        if (root == 1)
        {        
            for (li = list_first (c_t->u.semi_comma.cmd); li; li = list_next (li)) 
            {
                stmt = (act_chp_lang_t *)(list_value(li));
                if (stmt->type == ACT_CHP_LOOP || stmt->type == ACT_CHP_DOLOOP) 
                    _print_decomp_info (stmt, 1);
            }
            break;
        }
        for (li = list_first (c_t->u.semi_comma.cmd); li; li = list_next (li)) 
        {
            stmt = (act_chp_lang_t *)(list_value(li));
            _print_decomp_info (stmt, 0);
        }
        break;

    case ACT_CHP_LOOP:
    case ACT_CHP_DOLOOP:
        if (root == 1)
        {
            gc = c_t->u.gc;
            _print_decomp_info (gc->s, 0);
            break;
        }
        else
        {
            fatal_error ("should've excised internal loops...");
        }
        break;
        
    case ACT_CHP_SELECT:
        chp_print (fp, c_t);
        _print_decomp_info ((decomp_info_t *)(c_t->space));
        for (gc = c_t->u.gc ; gc ; gc = gc->next)
        {
            _print_decomp_info (gc->s, 0);
        }
        break;

    case ACT_CHP_SELECT_NONDET:
        fatal_error ("Can't handle NDS");
        break;

    case ACT_CHP_SKIP:
        chp_print (fp, c_t);
        _print_decomp_info ((decomp_info_t *)(c_t->space));
        break;
        
    case ACT_CHP_ASSIGN:
    case ACT_CHP_ASSIGNSELF:
        chp_print (fp, c_t);
        _print_decomp_info ((decomp_info_t *)(c_t->space));
        break;
        
    case ACT_CHP_RECV:
        chp_print (fp, c_t);
        _print_decomp_info ((decomp_info_t *)(c_t->space));
        break;

    case ACT_CHP_SEND:
        chp_print (fp, c_t);
        _print_decomp_info ((decomp_info_t *)(c_t->space));
        break;

    case ACT_CHP_FUNC:
    case ACT_CHP_HOLE: /* to support verification */
    case ACT_CHP_MACRO:
    case ACT_HSE_FRAGMENTS:
        break;

    default:
        fatal_error ("Unknown type");
        break;
    }

}

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