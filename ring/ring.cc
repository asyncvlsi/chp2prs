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

#include "ring.h"

RingEngine::RingEngine ( FILE *fp, Process *p, act_chp_lang_t *c,
            ActBooleanizePass *bp, 
            const char *circuit_library,
            const char *exprfile )
            {
                _fp = fp;
                _p = p;
                _c = c;
                _bp = bp;
                _circuit_library = Strdup(circuit_library);
                _exprfile = Strdup(exprfile);

                var_infos = hash_new (4);
                var_infos_copy = hash_new (4);
                var_infos_read_ids = hash_new (4);

                _inexprmap = ihash_new (0);
                _inwidthmap = ihash_new (0);

                _block_id = 0;
                _itb_wrapper_id = 0;
                _bd_chan_id = 0;
                _sync_chan_id = 0;
                _expr_id = 0;
                // _expr_block_id = 0;
                _mux_block_id = 0;
                _branch_id = 0;
            }; 

// void RingEngine::run_forge ()
// {
//     /* Handling
//      * 'everything else besides the chp body'
//      * needs to be added here
//     */

//    construct_var_infos ();
//    _run_forge_helper ();
// }
#define NOT_FOUND -2
#define NO_ASSIGN -1

void RingEngine::_construct_var_info (act_chp_lang_t *c, ActId *id, var_info *v)
{
  Scope *s = _p->CurScope();

  switch (c->type) {

  case ACT_CHP_SKIP:
    break;

  case ACT_CHP_ASSIGN:
    // _chkdynamic (c->u.assign.id);
    // if (_isdynamic_var) {
    //   return;
    // }
    if (id->isEqual(c->u.assign.id))
    { 
      if((latch_info_t *)(c->space))
      {
        (((latch_info_t *)(c->space))->latch_number) = v->nwrite;
      }
      v->nwrite++;
    }
    if ( _var_appears_in_expr (c->u.assign.e, id) )
      v->nread++;
    // if (x != v->nread) {
    //   c->type = ACT_CHP_ASSIGNSELF;
    // }
    break;

  case ACT_CHP_SEND:
    // _chkdynamic (c->u.comm.chan);
    // if (_isdynamic_var) {
    //   return;
    // }
    if (c->u.comm.e) {
      if ( _var_appears_in_expr (c->u.comm.e, id) )
        v->nread++;
    }
    break;

  case ACT_CHP_RECV:
    // v = _var_getinfo (c->u.comm.chan);
    if ((c->u.comm.var) && id->isEqual(c->u.comm.var))
    { 
      Assert ((latch_info_t *)(c->space), "hmm2");
      (((latch_info_t *)(c->space))->latch_number) = v->nwrite;
      v->nwrite++;
    }
    break;

  case ACT_CHP_COMMA:
  case ACT_CHP_SEMI:
    for (listitem_t *li = list_first (c->u.semi_comma.cmd); li; li = list_next (li)) 
    {
      _construct_var_info ((act_chp_lang_t *) list_value (li), id, v);
    }
    break;

  case ACT_CHP_SELECT_NONDET:
    fatal_error ("NDS not supported yet"); break;
  case ACT_CHP_LOOP:
  case ACT_CHP_SELECT:
  case ACT_CHP_DOLOOP:
    {
      act_chp_gc_t *gc = c->u.gc;
      while (gc) 
      {
        if (gc->g) {
          if ( _var_appears_in_expr (gc->g, id) )
            v->nread++;
        }
        gc = gc->next;
      }
      /* handle statements */
      gc = c->u.gc;
      while (gc) {
      _construct_var_info (gc->s, id, v);
      gc = gc->next;
      }
    }
    break;

  case ACT_CHP_FUNC:
    /* ignore this---not synthesized */
    break;

  default:
    fatal_error ("What?");
    break;
  }
}

/*
  Mark IDs for merge muxes
*/
void RingEngine::_construct_merge_latch_info (act_chp_lang_t *c, int root)
{
  Scope *s = _p->CurScope();
  act_chp_lang_t *stmt;

  switch (c->type) {

  case ACT_CHP_SKIP:
    break;

  case ACT_CHP_ASSIGN:
  case ACT_CHP_SEND:
  case ACT_CHP_RECV:
    break;

  case ACT_CHP_COMMA:
  case ACT_CHP_SEMI:
    for (listitem_t *li = list_first (c->u.semi_comma.cmd); li; li = list_next (li)) 
    {   
      stmt = (act_chp_lang_t *)list_value(li);
      if (root == 1 && (stmt->type == ACT_CHP_LOOP || stmt->type == ACT_CHP_DOLOOP)) {
        _construct_merge_latch_info ((act_chp_lang_t *) list_value (li), 1);
      }
      else if (root == 0 && (stmt->type == ACT_CHP_LOOP || stmt->type == ACT_CHP_DOLOOP))
      {
        fatal_error ("should've excised internal loops...");
      }
      else {
        _construct_merge_latch_info ((act_chp_lang_t *) list_value (li), 0);
      }
    }
    break;

  case ACT_CHP_SELECT_NONDET:
    fatal_error ("NDS not supported yet"); break;
  case ACT_CHP_LOOP:
  case ACT_CHP_DOLOOP:
    if (root == 1) {
      act_chp_gc_t *gc = c->u.gc;
      _construct_merge_latch_info (gc->s, 0);
      break;
    }
  case ACT_CHP_SELECT:
    {
      int gc_len = length_of_guard_set (c);
      ((latch_info_t *)(c->space))->merge_mux_latch_number.clear();
      list_t *ll = ((latch_info_t *)(c->space))->live_vars;
      for (listitem_t *li = list_first(ll); li ; li = li->next)
      {
        hash_bucket_t *b = hash_lookup(var_infos, (char *)list_value(li));
        Assert (b, "variable not found");
        if (_var_assigned_in_subtree (c, (char *)list_value(li))) {
          // latch id for mux
          ((latch_info_t *)(c->space))->merge_mux_latch_number.push_back(((var_info *)(b->v))->nwrite);
          ((var_info *)(b->v))->nwrite++;
        }
        else {
          // mux not needed, insert -1
          ((latch_info_t *)(c->space))->merge_mux_latch_number.push_back(-1);
        }
        // fill -1's to initialize
        std::vector<int> m1s(gc_len, -1);
        ((latch_info_t *)(c->space))->merge_mux_inputs.push_back(m1s);
      }
      act_chp_gc_t *gc = c->u.gc;
      while (gc) {
        _construct_merge_latch_info (gc->s, 0);
        gc = gc->next;
      }
    }
    break;

  case ACT_CHP_FUNC:
    /* ignore this---not synthesized */
    break;

  default:
    fatal_error ("What?");
    break;
  }
}

bool RingEngine::_var_assigned_in_subtree (act_chp_lang_t *c, const char *name)
{
  Scope *s = _p->CurScope();
  act_chp_lang_t *stmt;
  char tname[1024];
  bool ret = false;

  switch (c->type) {
  case ACT_CHP_SKIP:
  return false; break;

  case ACT_CHP_SEND:
  return false; break;
  case ACT_CHP_ASSIGN:
    get_true_name (tname, c->u.assign.id, s, true);
    return (!strcmp(tname, name));
    break;
  case ACT_CHP_RECV:
    if (!c->u.comm.var) {
      return false;
    }
    get_true_name (tname, c->u.comm.var, s, true);
    return (!strcmp(tname, name));
    break;

  case ACT_CHP_COMMA:
  case ACT_CHP_SEMI:
    for (listitem_t *li = list_first (c->u.semi_comma.cmd); li; li = list_next (li)) 
    {   
      ret = ret || _var_assigned_in_subtree ((act_chp_lang_t *)list_value(li), name);
    }
    break;

  case ACT_CHP_SELECT_NONDET:
    fatal_error ("NDS not supported yet"); break;
  case ACT_CHP_LOOP:
  case ACT_CHP_DOLOOP:
    fatal_error ("shouldn't have gotten here"); break;
  case ACT_CHP_SELECT:
    {
      act_chp_gc_t *gc = c->u.gc;
      while (gc) {
      ret = ret || _var_assigned_in_subtree (gc->s, name);
      gc = gc->next;
      }
    }
    break;

  case ACT_CHP_FUNC:
    /* ignore this---not synthesized */
    break;

  default:
    fatal_error ("What?");
    break;
  }
  return ret;
}

void RingEngine::compute_mergemux_info (act_chp_lang_t *c)
{
  hash_iter_t it;
  hash_bucket_t *b;
  hash_iter_init (var_infos, &it);
  // one pass per variable
  while ((b = hash_iter_next (var_infos, &it))) 
  {
    _compute_mergemux_info (c, (var_info *)b->v, -1);
  }	     
}

int RingEngine::_compute_mergemux_info (act_chp_lang_t *c, var_info *vi, int mux_number)
{
  Scope *s = _p->CurScope();

  switch (c->type) {

  case ACT_CHP_SKIP:
  case ACT_CHP_ASSIGN:
  case ACT_CHP_SEND:
  case ACT_CHP_RECV:
    break;

  case ACT_CHP_COMMA:
  case ACT_CHP_SEMI:
    for (listitem_t *li = list_first (c->u.semi_comma.cmd); li; li = list_next (li)) 
    {   
      mux_number = _compute_mergemux_info ((act_chp_lang_t *) list_value (li), vi, mux_number);
    }
    break;

  case ACT_CHP_SELECT_NONDET:
    fatal_error ("NDS not supported yet"); break;
  case ACT_CHP_LOOP:
  case ACT_CHP_DOLOOP:
  {
    act_chp_gc_t *gc = c->u.gc;
    mux_number = _compute_mergemux_info (gc->s, vi, mux_number);
    Assert (!(gc->next), "more than one loop branch at top-level?");
  }
    break;
  case ACT_CHP_SELECT:
  {
    act_chp_gc_t *gc = c->u.gc;
    // process internals recursively first
    while (gc) {
      mux_number = _compute_mergemux_info (gc->s, vi, mux_number);
      gc = gc->next;
    }
    gc = c->u.gc;
    list_t *ll = ((latch_info_t *)(c->space))->live_vars;
    std::vector<int> latches_in_branches;
    latches_in_branches.clear();
    int vpos = _var_in_list (vi->name, ll);
    std::vector<int> mln = ((latch_info_t *)(c->space))->merge_mux_latch_number;
    if ((vpos != NOT_FOUND) && (mln.at(vpos) != -1))
    {
      (((latch_info_t *)(c->space))->merge_mux_inputs).at(vpos).clear();
      while (gc) {
        int lid = _get_latest_assign_in_branch (gc->s, vi, -1);
        (((latch_info_t *)(c->space))->merge_mux_inputs).at(vpos).push_back(lid);
        gc = gc->next;
      }
      mux_number = mln.at(vpos);
    }
  }
  break;

  case ACT_CHP_FUNC:
    /* ignore this---not synthesized */
    break;

  default:
    fatal_error ("What?");
    break;
  }
  return mux_number;
}

int RingEngine::_get_latest_assign_in_branch (act_chp_lang_t *branch, var_info *vi, int latch_number)
{
  Scope *s = _p->CurScope();
  act_chp_lang_t *stmt;

  switch (branch->type) {

  case ACT_CHP_SKIP:
  case ACT_CHP_SEND:
    break;
  case ACT_CHP_ASSIGN:
  {
    ActId *id = branch->u.assign.id;
    char tname[1024];
    get_true_name (tname, id, s, true);
    if (!strcmp(tname, vi->name)) {
      return ((latch_info_t *)(branch->space))->latch_number;
    }
  }
  break;
  case ACT_CHP_RECV:
  {
    ActId *id = branch->u.comm.var;
    if (!id) return latch_number;
    char tname[1024];
    get_true_name (tname, id, s, true);
    if (!strcmp(tname, vi->name)) {
      return ((latch_info_t *)(branch->space))->latch_number;
    }
  }
  break;

  case ACT_CHP_COMMA:
  case ACT_CHP_SEMI:
    for (listitem_t *li = list_first (branch->u.semi_comma.cmd); li; li = list_next (li)) 
    {   
      latch_number = _get_latest_assign_in_branch ((act_chp_lang_t *) list_value (li), vi, latch_number);
    }
    break;

  case ACT_CHP_SELECT_NONDET:
    fatal_error ("NDS not supported yet"); break;
  case ACT_CHP_LOOP:
  case ACT_CHP_DOLOOP:
  {
    act_chp_gc_t *gc = branch->u.gc;
    latch_number = _get_latest_assign_in_branch (gc->s, vi, latch_number);
    Assert (!(gc->next), "more than one loop branch at top-level?");
  }
    break;
  case ACT_CHP_SELECT:
  {
    latch_number = _compute_mergemux_info (branch, vi, latch_number);
  }
  break;

  case ACT_CHP_FUNC:
    /* ignore this---not synthesized */
    break;

  default:
    fatal_error ("What?");
    break;
  }
  return latch_number;
}

void RingEngine::print_merge_mux_infos (FILE *fp, act_chp_lang_t *c)
{
  Scope *s = _p->CurScope();

  switch (c->type) {

  case ACT_CHP_SKIP:
  case ACT_CHP_SEND:
  case ACT_CHP_ASSIGN:
  case ACT_CHP_RECV:
    if (c->space) {
      fprintf (fp, "\n----\n");
      chp_print (fp, c);
      _print_latch_info_struct (fp, (latch_info_t *)(c->space));
      fprintf (fp, "----\n\n");
    }
  break;

  case ACT_CHP_COMMA:
  case ACT_CHP_SEMI:
    if (c->space) {
        fprintf (fp, "\n----\n");
        chp_print (fp, c);
        _print_latch_info_struct (fp, (latch_info_t *)(c->space));
        fprintf (fp, "----\n\n");
      }
    for (listitem_t *li = list_first (c->u.semi_comma.cmd); li; li = list_next (li)) 
    {   
      print_merge_mux_infos (fp, (act_chp_lang_t *) list_value (li));
    }
    break;

  case ACT_CHP_SELECT_NONDET:
    fatal_error ("NDS not supported yet"); break;
  case ACT_CHP_LOOP:
  case ACT_CHP_DOLOOP:
  {
    if (c->space) {
        fprintf (fp, "\n----\n");
        chp_print (fp, c);
        _print_latch_info_struct (fp, (latch_info_t *)(c->space));
        fprintf (fp, "----\n\n");
    }
    act_chp_gc_t *gc = c->u.gc;
    print_merge_mux_infos (fp, gc->s);
    Assert (!(gc->next), "more than one loop branch at top-level?");
  }
    break;
  case ACT_CHP_SELECT:
  {
    act_chp_gc_t *gc = c->u.gc;
    while (gc) {
      print_merge_mux_infos (fp, gc->s);
      gc = gc->next;
    }
    if (c->space) {
        fprintf (fp, "\n----\n");
        chp_print (fp, c);
        _print_latch_info_struct (fp, (latch_info_t *)(c->space));
        fprintf (fp, "----\n\n");
    }
  }
  break;

  case ACT_CHP_FUNC:
    /* ignore this---not synthesized */
    break;

  default:
    fatal_error ("What?");
    break;
  }
}

void RingEngine::_print_latch_info_struct (FILE *fp, latch_info_t *l)
{
  fprintf (fp, "\n --------- \n");
  switch (l->type) {
  case LatchType::Latch: 
  {
    fprintf (fp, "type: latch \n");
    fprintf (fp, "latch ID: %d\n", l->latch_number);
    break;
  }
  case LatchType::Mux: 
  {
    fprintf (fp, "type: mux \n");
    int ctr = 0;
    for (listitem_t *li = list_first (l->live_vars) ; li ; li = li->next) 
    {
      fprintf (fp, "variable: %s\n", (char *)(list_value(li)));
      fprintf (fp, "mux ID: %d\n", l->merge_mux_latch_number.at(ctr));
      fprintf (fp, "inputs: ");
      for ( auto x : l->merge_mux_inputs.at(ctr) )
      {
        fprintf (fp, "%d, ", x);
      }
      ctr++;
      fprintf (fp, "\n\n");
    }  
    break;
  }
  case LatchType::ICs: 
  {
    fprintf (fp, "type: initial conditions / loop-carried dependencies \n");
    fprintf (fp, "variables: ");
    for (listitem_t *li = list_first (l->live_vars) ; li ; li = li->next) 
    {
      fprintf (fp, "%s, ", (char *)(list_value(li)));
    }
    fprintf (fp, "\n\n");
    break;
  }
  default:
    fatal_error ("huh"); break;
  }
}

/*
  Flow assignments of variables down the 
  graph so that mux inputs can be mapped.
*/
void RingEngine::flow_assignments (act_chp_lang_t *c)
{
  hash_iter_t it;
  hash_bucket_t *b;
  hash_iter_init (var_infos, &it);
  // one pass per variable per assignment
  while ((b = hash_iter_next (var_infos, &it))) 
  {
    var_info *vi = (var_info *)b->v;
    _flow_assignments (c, (var_info *)b->v, -1);
  }
}

int RingEngine::_flow_assignments (act_chp_lang_t *c, var_info *vi, int latest)
{
  Scope *s = _p->CurScope();
  char tname[1024];

  switch (c->type) {

  case ACT_CHP_SKIP:
    break;
  case ACT_CHP_ASSIGN:
    if (c->space) {
      get_true_name (tname, c->u.assign.id, s, true);
      if (!strcmp(tname, vi->name)) {
        latest = ((latch_info_t *)(c->space))->latch_number;
      }
    }
    else {
      fatal_error ("huh");
    }
    break;
  case ACT_CHP_SEND:
    break;
  case ACT_CHP_RECV:
    if (c->space) {
      if (!(c->u.comm.var)) break;
      get_true_name (tname, c->u.comm.var, s, true);
      if (!strcmp(tname, vi->name)) {
        latest = ((latch_info_t *)(c->space))->latch_number;
      }
    }
    else {
      fatal_error ("huh");
    }
    break;

  case ACT_CHP_COMMA:
  case ACT_CHP_SEMI:
    for (listitem_t *li = list_first(c->u.semi_comma.cmd) ; li ; li=li->next)
    {
      latest = _flow_assignments ((act_chp_lang_t *)list_value(li), vi, latest);
    }
    break;

  case ACT_CHP_SELECT_NONDET:
    fatal_error ("NDS not supported yet"); 
    break;
  case ACT_CHP_LOOP:
  case ACT_CHP_DOLOOP:
  {
    act_chp_gc_t *gc = c->u.gc;
    latest = _flow_assignments (gc->s, vi, latest);
    Assert (!(gc->next), "more than one loop branch at top-level?");
  }
    break;
  case ACT_CHP_SELECT:
  {
    std::vector<int> latests;
    latests.clear();
    act_chp_gc_t *gc = c->u.gc;
    // gather info
    while (gc) {
      int tmp = _flow_assignments (gc->s, vi, latest);
      latests.push_back(tmp);
      gc = gc->next;
    }
    // actually fill in the info
    int vpos = _var_in_list (vi->name, ((latch_info_t *)(c->space))->live_vars);
    if (vpos != NOT_FOUND) {
      ((latch_info_t *)(c->space))->merge_mux_inputs.at(vpos) = latests;
      // latest is now the merge mux (if it is needed)
      if (((latch_info_t *)(c->space))->merge_mux_latch_number.at(vpos) != -1) {
        latest = ((latch_info_t *)(c->space))->merge_mux_latch_number.at(vpos);
      }
    }
  }
  break;

  case ACT_CHP_FUNC:
    /* ignore this---not synthesized */
    break;

  default:
    fatal_error ("What?");
    break;
  }
  return latest;
}

bool RingEngine::_check_all_muxes_mapped (act_chp_lang_t *c, bool fail)
{
  Scope *s = _p->CurScope();

  switch (c->type) {

  case ACT_CHP_SKIP:
  case ACT_CHP_SEND:
  case ACT_CHP_ASSIGN:
  case ACT_CHP_RECV:
    break;

  case ACT_CHP_COMMA:
  case ACT_CHP_SEMI:
    for (listitem_t *li = list_first (c->u.semi_comma.cmd); li; li = list_next (li)) 
    {   
      fail = fail || _check_all_muxes_mapped ((act_chp_lang_t *) list_value (li), fail);
    }
    break;

  case ACT_CHP_SELECT_NONDET:
    fatal_error ("NDS not supported yet"); break;
  case ACT_CHP_LOOP:
  case ACT_CHP_DOLOOP:
  {
    act_chp_gc_t *gc = c->u.gc;
    fail = fail || _check_all_muxes_mapped (gc->s, fail);
    Assert (!(gc->next), "more than one loop branch at top-level?");
  }
    break;
  case ACT_CHP_SELECT:
  {
    act_chp_gc_t *gc = c->u.gc;
    while (gc) {
      fail = fail || _check_all_muxes_mapped (gc->s, fail);
      gc = gc->next;
    }
    Assert (c->space, "no mux info?");
    latch_info_t *linfo = ((latch_info_t *)(c->space));
    Assert ((linfo->type == LatchType::Mux), "hmm");
    listitem_t *li = list_first(linfo->live_vars);
    int ctr = 0;
    for ( auto x : linfo->merge_mux_latch_number )
    {
      Assert (li, "hmm weird");
      if (x != -1) 
      {
        for ( auto y : linfo->merge_mux_inputs.at(ctr) ) 
        {
          if ( y == -1 )
          {
            chp_print (_fp, c);
            fprintf (_fp, "\n\nunmapped mux for variable: %s", (char *)(list_value(li)));
            fail = true;
          }
        }
      }
      ctr++;
      li = li->next;
    }
  }
  break;

  case ACT_CHP_FUNC:
    /* ignore this---not synthesized */
    break;

  default:
    fatal_error ("What?");
    break;
  }
  return fail;
}

bool RingEngine::_var_appears_in_expr (Expr *e, ActId *id)
{
  act_connection *uid;
  ActId *i;
  bool a1, a2, a3;
  char str[1024], t[1024];
  
  if (!e) return false;
  switch (e->type) {
    /* binary */
  case E_AND:
  case E_OR:
  case E_PLUS:
  case E_MINUS:
  case E_MULT:
  case E_DIV:
  case E_MOD:
  case E_LSL:
  case E_LSR:
  case E_ASR:
  case E_XOR:
  case E_LT:
  case E_GT:
  case E_LE:
  case E_GE:
  case E_EQ:
  case E_NE:
    a1 = _var_appears_in_expr (e->u.e.l, id);
    a2 = _var_appears_in_expr (e->u.e.r, id);
    return a1 | a2;
    break;
    
  case E_NOT:
  case E_UMINUS:
  case E_COMPLEMENT:
    a1 = _var_appears_in_expr (e->u.e.l, id);
    return a1;
    break;

  case E_QUERY:
    a1 = _var_appears_in_expr (e->u.e.l, id);
    a2 = _var_appears_in_expr (e->u.e.r->u.e.l, id);
    a3 = _var_appears_in_expr (e->u.e.r->u.e.r, id);
    return a1 | a2 | a3;
    break;

  case E_COLON:
  case E_COMMA:
    fatal_error ("Should have been handled elsewhere");
    return false;
    break;

  case E_CONCAT:
    do {
      a1 = _var_appears_in_expr (e->u.e.l, id);
      e = e->u.e.r;
    } while (e && !a1);
    return a1;
    break;

  case E_BITFIELD:
    /* l is an Id */
    // v = _var_getinfo ((ActId *)e->u.e.l);
    // if ((!_shared_expr_var || !v->fcurexpr) && !v->fischan) {
    //   v->nread++;
    //   v->fcurexpr = 1;
    // }
    return id->isEqual((ActId *)(e->u.e.l));
    break;

  case E_TRUE:
  case E_FALSE:
  case E_INT:
  case E_REAL:
    return false;
    break;

  case E_VAR:
    return id->isEqual((ActId *)(e->u.e.l));
    break;

  case E_PROBE:
    fatal_error ("Not handling probes right now");
    return false;
    break;

  case E_BUILTIN_BOOL:
  case E_BUILTIN_INT:
    a1 = _var_appears_in_expr (e->u.e.l, id);
    return a1;
    break;
    
  case E_FUNCTION:
    warning ("not handling functions");
    return false;
    e = e->u.fn.r;
    while (e) {
      _var_appears_in_expr (e->u.e.l, id);
      e = e->u.e.r;
    }
    break;

  case E_SELF:
  default:
    fatal_error ("Unknown expression type %d\n", e->type);
    return false;
    break;
  }
}

void RingEngine::construct_var_infos (act_chp_lang_t *c)
{
  var_infos = hash_new(4);
  hash_bucket_t *b;
  var_info *v;
  ActId *id;
  char str[1024];

  act_boolean_netlist_t *bnl = _bp->getBNL(_p);
  Assert (bnl, "hmm BNL");
  pHashtable *pht = bnl->cH;

  act_booleanized_var_t *bv;
  act_connection *conn;

  ihash_iter_t it;
  ihash_bucket_t *ib;
  ihash_iter_init (pht, &it);
  while ((ib = ihash_iter_next (pht, &it))) {
    bv = (act_booleanized_var_t *)ib->v;
    if (bv->usedchp) {
      conn = bv->id;
      id = conn->toid();
      if (TypeFactory::isDataType (conn->getvx()->t)) 
      {
        NEW(v, var_info);
        v->width = bv->width;
        v->nread = 0;
        v->iread = 0;
        v->nwrite = 0;
        v->iwrite = 0;
        v->latest_for_read = 0;
        get_true_name (str, id, _p->CurScope());
        v->name = Strdup (str);
        
        _construct_var_info (c, id, v);
        b = hash_add (var_infos, v->name);
        b->v = v;
      }
    }

}
}

void RingEngine::print_var_infos (FILE *fp)
{
  fprintf (fp, "\nvar_info hashtable: \n");
  hash_iter_t it;
  hash_bucket_t *b;
  hash_iter_init (var_infos, &it);
    while ((b = hash_iter_next (var_infos, &it))) 
    {
      _print_var_info (fp, (var_info *)b->v);
    }	     
}

void RingEngine::_print_var_info (FILE *fp, var_info *v)
{
  if (!v) return;
  fprintf(fp, "\nName: %s", v->name);
  fprintf(fp, "\nBitwidth: %d", v->width);
  fprintf(fp, "\nNo. of reads: %d", v->nread);
  fprintf(fp, "\nNo. of writes: %d", v->nwrite);
  fprintf(fp, "\n\n");
}

Hashtable *RingEngine::_deepcopy_var_info_hashtable (Hashtable *h_in, int only_read_id)
{
  Hashtable *h_out = hash_new(4);
  hash_bucket_t *b, *b_copy;
  var_info *v_copy;
  hash_iter_t itr;
  hash_iter_init (h_in, &itr);
  while ((b = hash_iter_next (h_in, &itr))) {
    NEW (v_copy, var_info);
    v_copy = _deepcopy_var_info((var_info *)b->v, only_read_id);
    b_copy = hash_add (h_out, v_copy->name);
    b_copy->v = v_copy;
  }
  return h_out;
}

void RingEngine::save_var_infos ()
{
    var_infos_copy = hash_new (4);
    var_infos_copy = _deepcopy_var_info_hashtable (var_infos, 0);
}

void RingEngine::_save_read_ids ()
{
    var_infos_read_ids = hash_new (4);
    var_infos_read_ids = _deepcopy_var_info_hashtable (var_infos, 1);
}

void RingEngine::restore_var_infos ()
{
    var_infos = hash_new (4);
    var_infos = _deepcopy_var_info_hashtable (var_infos_copy, 0);
    hash_clear (var_infos_copy);
}

void RingEngine::_restore_read_ids ()
{
    hash_bucket_t *b, *b_saved;
    var_info *vi, *vi_saved;
    hash_iter_t itr;
    hash_iter_init (var_infos, &itr);
    while ((b = hash_iter_next (var_infos, &itr))) {
      b_saved = hash_lookup(var_infos_read_ids, b->key);
      Assert (b_saved, "No var_info_read_id ??");
      vi_saved = (var_info *)b_saved->v;
      vi = (var_info *)b->v;
      vi->latest_for_read = vi_saved->latest_for_read;
    }
    hash_clear (var_infos_read_ids);
}

var_info *RingEngine::_deepcopy_var_info (var_info *v, int only_read_id)
{
  var_info *v_copy;
  NEW (v_copy, var_info);
  v_copy->name = v->name;
  v_copy->latest_for_read = v->latest_for_read;
  if (!only_read_id) {
    v_copy->fcurexpr = v->fcurexpr;
    v_copy->fischan = v->fischan;
    v_copy->fisinport = v->fisinport;
    v_copy->fisbool = v->fisbool;
    v_copy->width = v->width;
    v_copy->block_in = v->block_in;
    v_copy->block_out = v->block_out;
    v_copy->nread = v->nread;
    v_copy->nwrite = v->nwrite;
    v_copy->iread = v->iread;
    v_copy->iwrite = v->iwrite;
  }
  return v_copy;
}

int RingEngine::_gen_block_id()
{
    _block_id++;
    return _block_id;
}

int RingEngine::_gen_itb_wrapper_id()
{
    _itb_wrapper_id++;
    return _itb_wrapper_id;
}

int RingEngine::_gen_bd_chan_id()
{
    _bd_chan_id++;
    return _bd_chan_id;
}

int RingEngine::_gen_sync_chan_id()
{
    _sync_chan_id++;
    return _sync_chan_id;
}

int RingEngine::_gen_expr_id()
{
    _expr_id++;
    return _expr_id;
}

unsigned int RingEngine::_expr_block_id = 0;

int RingEngine::_gen_expr_block_id()
{
    _expr_block_id++;
    return _expr_block_id;
}

int RingEngine::_gen_mux_block_id()
{
    _mux_block_id++;
    return _mux_block_id;
}

int RingEngine::length_of_guard_set (act_chp_lang_t *c)
{
  act_chp_gc_t *gc_itr;
  int counter = 0;
  Assert (((c->type == ACT_CHP_SELECT)||(c->type == ACT_CHP_LOOP)||(c->type == ACT_CHP_DOLOOP)), 
            "Called length_of_guard_set on a non-selection/loop");

  for (gc_itr = c->u.gc; gc_itr; gc_itr = gc_itr->next)
  { counter++; }
  return counter;
}

bool RingEngine::is_elementary_action(act_chp_lang_t *c)
{
    Assert (c, "wth");

    switch (c->type) {
    case ACT_CHP_COMMALOOP:
    case ACT_CHP_SEMILOOP:
        fatal_error ("Replication loops should've been removed..");
    case ACT_CHP_COMMA:
    case ACT_CHP_SEMI:
    case ACT_CHP_LOOP:
    case ACT_CHP_DOLOOP:
    case ACT_CHP_SELECT:
        return false;
        break;

    case ACT_CHP_SELECT_NONDET:
        fatal_error ("Can't handle NDS");
        return false;
        
    case ACT_CHP_SKIP:
    case ACT_CHP_ASSIGN:
    case ACT_CHP_ASSIGNSELF:
    case ACT_CHP_SEND:
    case ACT_CHP_RECV:
        return true;
        break;

    case ACT_CHP_FUNC:
    case ACT_CHP_HOLE: /* to support verification */
    case ACT_CHP_MACRO:
    case ACT_HSE_FRAGMENTS:
        return false;
        break;

    default:
        fatal_error ("Unknown type");
        return false;
        break;
    }

}

bool RingEngine::chp_has_branches (act_chp_lang_t *c, int root)
{
    bool has_branches = false;
    listitem_t *li, *li_prev;
    act_chp_gc_t *gc;
    act_chp_lang_t *stmt, *stmt_prev;

    if (!c) return false;

    switch (c->type) {
    case ACT_CHP_COMMALOOP:
    case ACT_CHP_SEMILOOP:
        fatal_error ("Replication loops should've been removed..");
        break;
        
    case ACT_CHP_COMMA:
        has_branches = true;
        break;

    case ACT_CHP_SEMI:
        if (root == 1)
        {   
            for (li = list_first (c->u.semi_comma.cmd); li; li = list_next (li)) 
            {   
                stmt = (act_chp_lang_t *)list_value(li);
                if (stmt->type == ACT_CHP_LOOP || stmt->type == ACT_CHP_DOLOOP)
                    has_branches = chp_has_branches (stmt, 1);
            }
        }
        else{
            for (li = list_first (c->u.semi_comma.cmd); li; li = list_next (li)) 
            {
                stmt = (act_chp_lang_t *)(list_value(li));
                has_branches = chp_has_branches (stmt, 0);
                if (has_branches) break;
            }
        }
        break;

    case ACT_CHP_LOOP:
    case ACT_CHP_DOLOOP:
        if (root == 1)
        {
            gc = c->u.gc;
            has_branches = chp_has_branches (gc->s, 0);
            break;
        }
        else
        {
            fatal_error ("should've excised internal loops... (chp_has_branches)");
        }
        break;
        
    case ACT_CHP_SELECT:
        if( length_of_guard_set (c) > 1)
        {
            has_branches = true;
        }
        else
        {
            gc = c->u.gc;
            has_branches = chp_has_branches (gc->s, 0);
        }
        break;

    case ACT_CHP_SELECT_NONDET:
        fatal_error ("Can't handle NDS");
        
    case ACT_CHP_SKIP:
    case ACT_CHP_ASSIGN:
    case ACT_CHP_ASSIGNSELF:
    case ACT_CHP_SEND:
    case ACT_CHP_RECV:
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

    return has_branches;
}

int RingEngine::_var_in_list (const char *name, list_t *l)
{
  int ctr = 0;
  for (listitem_t *li = list_first(l) ; li ; li = li->next)
  {
    if (!strcmp(name, (char *)list_value(li))) {
      return ctr;
    }
    ctr++;
  }
  return NOT_FOUND;
}

int RingEngine::get_expr_width(Expr *ex)
{
  // recursively run through the expression and collect its width
  switch ((ex)->type)
  {
  // for a var read the bitwidth of that var
  case E_VAR:
  {
    ActId *var = (ActId *)ex->u.e.l;  
    hash_bucket_t *b;
    char tname[1024];
    get_true_name(tname, var, _p->CurScope());
    b = hash_lookup(var_infos, tname);
    // b = hash_lookup(var_infos, var->rootVx(p->CurScope())->getName());
    var_info *vi = (var_info *)b->v;
    return vi->width;
  }
  // for true and false the bit width is one
  case E_TRUE:
  case E_FALSE:
    return 1;
  // for int look up the corresponding bitwidth
  case E_INT:
  {
    return ihash_lookup(_inwidthmap, (long) ex)->i;
  }
  // step through
  case E_QUERY:
    ex = ex->u.e.r;
  // get the max out of the right and the left expr part
  case E_AND:
  case E_OR:
  case E_XOR:
  {
    int lw = get_expr_width(ex->u.e.l);
    int rw = get_expr_width(ex->u.e.r);
    return std::max(lw,rw);
  }
  // get the max out of the right and the left expr part and one for the overflow bit
  case E_PLUS:
  case E_MINUS:
  {
    int lw = get_expr_width(ex->u.e.l);
    int rw = get_expr_width(ex->u.e.r);
    return std::max(lw,rw);
    // @TODO genus trys to tie the top bit and i dont know why
    // return std::max(lw,rw)+1;
  }
  // comparisons result in a bool so 1, do not walk further
  case E_LT:
  case E_GT:
  case E_LE:
  case E_GE:
  case E_EQ:
  case E_NE:
    // should be fine in ignoring the rest of the expr
    return 1;
  // for multiplication add both operand bitwidth
  case E_MULT:
  {
    int lw = get_expr_width(ex->u.e.l);
    int rw = get_expr_width(ex->u.e.r);
    return lw+rw;
  } 
  // step through
  case E_MOD:
  {
    int rw = get_expr_width(ex->u.e.r);
    return rw;
  } 
  // use left bitwidth and add number of shifted right
  case E_LSL:
  {
    int lw = get_expr_width(ex->u.e.l);
    int rw = get_expr_width(ex->u.e.r);
    return lw + (1 << rw);
  }
  // pass through
  case E_DIV:
  case E_LSR:
  case E_ASR:
  case E_UMINUS:
  case E_NOT:
  case E_COMPLEMENT:
  {
    int lw = get_expr_width(ex->u.e.l);
    return lw;
  }  

  //get the value out of the datastructure
  case E_BUILTIN_INT:
    if (ex->u.e.r) {
      Assert (ex->u.e.r->type == E_INT, "What?");
      return ex->u.e.r->u.ival.v;
    }
    else {
      return 1;
    }

  case E_BUILTIN_BOOL:
    return 1;
  // the following ones should give you errors because not handled
  case E_COLON:
  case E_COMMA:
    fatal_error ("Should have been handled elsewhere");
    break;

    /* XXX: here */
  case E_CONCAT:
    {
      int w = 0;
      Expr *tmp = ex;
      while (tmp) {
	w += get_expr_width (tmp->u.e.l);
	tmp = tmp->u.e.r;
      }
      return w;
    }
    break;

  case E_BITFIELD:
    // _var_getinfo ((ActId *)ex->u.e.l);
    // if (ex->u.e.r->u.e.l) {
    //   return (ex->u.e.r->u.e.r->u.ival.v - ex->u.e.r->u.e.l->u.ival.v + 1);
    // }
    // else {
    //   return 1;
    // }
    fatal_error ("Not handling bitfields right now.");
    break;

  case E_REAL:
    fatal_error ("No real expressions please.");
    break;

  case E_PROBE:
    fatal_error ("fix probes please");
    break;
    
  case E_FUNCTION:
    fatal_error ("function!");
    
  case E_SELF:
  default:
    fatal_error ("Unknown expression type %d\n", ex->type);
    break;
  }
  return 0;
}
