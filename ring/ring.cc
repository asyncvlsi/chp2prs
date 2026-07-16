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
 *************************************************************************
 */

#include "ring.h"

RingEngine::RingEngine ( FILE *fp, 
            const char *circuit_library,
            const char *exprfile )
            {
                _fp = fp;
                _circuit_library = Strdup(circuit_library);
                _exprfile = Strdup(exprfile);

                var_infos = {};
                var_infos_copy = {};
                var_infos_read_ids = {};

                H_stk = {};

                _inexprmap = ihash_new (0);
                _inwidthmap = ihash_new (0);

                _block_id = 0;
                _itb_wrapper_id = 0;
                _bd_chan_id = 0;
                _sync_chan_id = 0;
                _expr_id = 0;
                _var_access_id = 0;
                // _expr_block_id = 0;
                _mux_block_id = 0;
                _branch_id = 0;
            }; 

#define NOT_FOUND -2
#define NO_ASSIGN -1

std::vector<int> RingEngine::_struct_latch_numbers(ActId *id, ActId *id_s, std::vector<int> lns_in)
{
  Assert (_check_ids_equal(id, id_s), "var not a member of struct?");
  
  InstType *it = _p->CurScope()->localLookup (id_s, NULL);
  Data *d = dynamic_cast<Data *>(it->BaseType());
  int nb, ni;
  int *types;
  d->getStructCount (&nb, &ni);
  ActId **res = d->getStructFields (&types);

  std::vector<int> ret = {};
  std::vector<int> ret_tmp (ni+nb, -1);
  if (lns_in.size()==0)
    ret = ret_tmp;
  else
    ret = lns_in;

  Assert (ret.size()==ni+nb, "wth");
  auto vi = _get_var_info(id);

  ActId *tail = id_s->Tail ();
  for (int i=0; i < ni + nb; i++) {
      int sz;
      InstType *xit;
      Assert (d->getStructOffset (res[i], &sz, &xit) != -1, "What?");
      tail->Append (res[i]);
      if (_check_ids_equal(id, tail)) {
        ret[i] = vi->nwrite;
        vi->nwrite++;
      }
      tail->prune ();
      delete res[i];
      delete xit;
  }
  return ret;
}

int RingEngine::_get_latest_struct_latch(ActId *id, ActId *id_s, std::vector<int> lns_in)
{
  Assert (_check_ids_equal(id, id_s), "var not a member of struct?");
  
  InstType *it = _p->CurScope()->localLookup (id_s, NULL);
  Data *d = dynamic_cast<Data *>(it->BaseType());
  int nb, ni;
  int *types;
  d->getStructCount (&nb, &ni);
  ActId **res = d->getStructFields (&types);

  Assert (lns_in.size()==ni+nb, "wth");

  ActId *tail = id_s->Tail ();
  for (int i=0; i < ni + nb; i++) {
      int sz;
      InstType *xit;
      Assert (d->getStructOffset (res[i], &sz, &xit) != -1, "What?");
      tail->Append (res[i]);
      if (_check_ids_equal(id, tail)) {
        tail->prune();
        return lns_in[i];
      }
      tail->prune ();
      delete res[i];
      delete xit;
  }
  Assert (false, "should not have gotten here");
  return -1;
}

void RingEngine::_construct_var_info (act_chp_lang_t *c, ActId *id, var_info *v)
{
  Scope *s = _p->CurScope();

  switch (c->type) {

  case ACT_CHP_SKIP:
    break;

  case ACT_CHP_ASSIGN:
    if (_check_ids_equal(id, c->u.assign.id))
    { 
      if((latch_info_t *)(c->space))
      {
        ((latch_info_t *)(c->space))->latch_numbers = {v->nwrite};
      }
      v->nwrite++;
    }
    if ( _var_appears_in_expr (c->u.assign.e, id) )
      v->nread++;
    break;

  case ACT_CHP_SEND:
    if (v->fischan==1) {
      if (id->isEqual(c->u.comm.chan))
        v->iwrite++;
    }
    if (c->u.comm.e) {
      if ( _var_appears_in_expr (c->u.comm.e, id) )
        v->nread++;
    }
    break;

  case ACT_CHP_RECV:
    if (v->fischan==1) {
      if (_check_ids_equal(id, c->u.comm.chan))
        v->iread++;
    }
    if ((c->u.comm.var) && _check_ids_equal(id, c->u.comm.var))
    { 
      if (!TypeFactory::isStructure(_p->CurScope()->localLookup (c->u.comm.var, NULL))) {
        Assert ((latch_info_t *)(c->space), "hmm2");
        ((latch_info_t *)(c->space))->latch_numbers = {v->nwrite};
        v->nwrite++;
      }
      else {
        ((latch_info_t *)(c->space))->latch_numbers = 
          _struct_latch_numbers(id, c->u.comm.var, ((latch_info_t *)(c->space))->latch_numbers);
      }
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
  case ACT_CHP_LOOP:
  case ACT_CHP_DOLOOP:
    if (root == 1) {
      act_chp_gc_t *gc = c->u.gc;
      _construct_merge_latch_info (gc->s, 0);
      break;
    }

  case ACT_CHP_SELECT_NONDET:
  case ACT_CHP_SELECT:
    {
      int gc_len = length_of_guard_set (c);
      ((latch_info_t *)(c->space))->merge_mux_latch_number.clear();
      auto ll = ((latch_info_t *)(c->space))->live_vars;
      for ( auto li : ll )
      {
        auto vi = _get_var_info(li->toid());
        if (_var_assigned_in_subtree (c, li->toid())) {
          // latch id for mux
          ((latch_info_t *)(c->space))->merge_mux_latch_number.push_back(vi->nwrite);
          vi->nwrite++;
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

bool RingEngine::_var_assigned_in_subtree (act_chp_lang_t *c, ActId *var)
{
  Scope *s = _p->CurScope();
  act_chp_lang_t *stmt;
  char tname[1024];
  bool ret = false;

  switch (c->type) {
  case ACT_CHP_SKIP:
  case ACT_CHP_SEND:
    return false; 
    break;
  case ACT_CHP_ASSIGN:
    return _check_ids_equal(var, c->u.assign.id);
    break;
  case ACT_CHP_RECV:
    if (!c->u.comm.var) {
      return false;
    }
    return _check_ids_equal(var, c->u.comm.var);
    break;

  case ACT_CHP_COMMA:
  case ACT_CHP_SEMI:
    for (listitem_t *li = list_first (c->u.semi_comma.cmd); li; li = list_next (li)) 
    {   
      ret = ret || _var_assigned_in_subtree ((act_chp_lang_t *)list_value(li), var);
    }
    break;

  case ACT_CHP_LOOP:
  case ACT_CHP_DOLOOP:
    fatal_error ("shouldn't have gotten here"); break;

  case ACT_CHP_SELECT_NONDET:
  case ACT_CHP_SELECT:
    {
      act_chp_gc_t *gc = c->u.gc;
      while (gc) {
      ret = ret || _var_assigned_in_subtree (gc->s, var);
      gc = gc->next;
      }
    }
    break;

  case ACT_CHP_FUNC:
    break;

  default:
    fatal_error ("What?");
    break;
  }
  return ret;
}

void RingEngine::compute_mergemux_info (act_chp_lang_t *c)
{
  for ( auto v : var_infos ) {
    _compute_mergemux_info (c, v.second, -1);
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

  case ACT_CHP_LOOP:
  case ACT_CHP_DOLOOP:
  {
    act_chp_gc_t *gc = c->u.gc;
    mux_number = _compute_mergemux_info (gc->s, vi, mux_number);
    Assert (!(gc->next), "more than one loop branch at top-level?");
  }
    break;
  case ACT_CHP_SELECT_NONDET:
  case ACT_CHP_SELECT:
  {
    act_chp_gc_t *gc = c->u.gc;
    // process internals recursively first
    while (gc) {
      mux_number = _compute_mergemux_info (gc->s, vi, mux_number);
      gc = gc->next;
    }
    gc = c->u.gc;
    auto ll = ((latch_info_t *)(c->space))->live_vars;
    std::vector<int> latches_in_branches;
    latches_in_branches.clear();
    int vpos = _var_in_list (vi->id, ll);
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
    break;

  default:
    fatal_error ("What?");
    break;
  }
  return mux_number;
}

bool RingEngine::_check_ids_equal (ActId *id, ActId *id_s)
{
  // id_s may be a struct
  InstType *it = _p->CurScope()->localLookup (id_s, NULL);
  auto c1 = id->Canonical(_p->CurScope());

  if (!(TypeFactory::isStructure(it))) {
    auto c2 = id_s->Canonical(_p->CurScope());
    return (c1==c2);
  }

  Data *d = dynamic_cast<Data *>(it->BaseType());
  int nb, ni;
  int *types;
  d->getStructCount (&nb, &ni);
  ActId **res = d->getStructFields (&types);
  ActId *tail = id_s->Tail ();
  for (int i=0; i < ni + nb; i++) {
      int sz;
      InstType *xit;
      Assert (d->getStructOffset (res[i], &sz, &xit) != -1, "What?");
      tail->Append (res[i]);

      auto c2 = tail->Canonical(_p->CurScope());
      if (c1==c2) {
        tail->prune();
        return true;
      }

      tail->prune();
      delete res[i];
      delete xit;
  }
  return false;
}

int RingEngine::_get_latest_assign_in_branch (act_chp_lang_t *branch, var_info *vi, int latch_num)
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
    // assigns to struct also count!!
    if (_check_ids_equal(vi->id,id)) {
      auto lns = ((latch_info_t *)(branch->space))->latch_numbers;
      Assert (lns.size()==1, "struct assign unexpanded?");
      return lns[0];
    }
  }
  break;
  case ACT_CHP_RECV:
  {
    ActId *id = branch->u.comm.var;
    if (!id) return latch_num;
    if (_check_ids_equal(vi->id, id)) {
      auto lns = ((latch_info_t *)(branch->space))->latch_numbers;

      if (!TypeFactory::isStructure(_p->CurScope()->localLookup (id, NULL))) {
        Assert (lns.size()==1, "pure var recv but longer latch_numbers?");
        return lns[0];
      }
      else {
        return _get_latest_struct_latch(vi->id, id, lns);
      }
    }
  }
  break;

  case ACT_CHP_COMMA:
  case ACT_CHP_SEMI:
    for (listitem_t *li = list_first (branch->u.semi_comma.cmd); li; li = list_next (li)) 
    {   
      latch_num = _get_latest_assign_in_branch ((act_chp_lang_t *) list_value (li), vi, latch_num);
    }
    break;

  case ACT_CHP_LOOP:
  case ACT_CHP_DOLOOP:
  {
    act_chp_gc_t *gc = branch->u.gc;
    latch_num = _get_latest_assign_in_branch (gc->s, vi, latch_num);
    Assert (!(gc->next), "more than one loop branch at top-level?");
  }
    break;

  case ACT_CHP_SELECT_NONDET:
  case ACT_CHP_SELECT:
  {
    latch_num = _compute_mergemux_info (branch, vi, latch_num);
  }
  break;

  case ACT_CHP_FUNC:
    break;

  default:
    fatal_error ("What?");
    break;
  }
  return latch_num;
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

  case ACT_CHP_SELECT_NONDET:
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
  case LatchType::Alias: 
  {
    fprintf (fp, "type: latch \n");
    if (l->latch_numbers.size()>0) {
      fprintf (fp, "latch ID: %d\n", l->latch_numbers[0]);
    }
    else {
      fprintf (fp, "latch ID: null\n");
    }
    break;
  }
  case LatchType::Mux: 
  {
    fprintf (fp, "type: mux \n");
    int ctr = 0;
    for ( auto li : l->live_vars ) 
    {
      fprintf (fp, "variable: %s\n", li->toid()->getName());
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
    for ( auto li : l->live_vars ) 
    {
      fprintf (fp, "%s, ", li->toid()->getName());
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
  for ( auto v : var_infos ) {
    _flow_assignments (c, v.second, -1);
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
      if (_check_ids_equal(vi->id, c->u.assign.id)) {
        auto lns = ((latch_info_t *)(c->space))->latch_numbers;
        Assert (lns.size()==1, "struct assign unexpanded?");
        latest = lns[0];
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
      if (_check_ids_equal(vi->id, c->u.comm.var)) {
        if (!TypeFactory::isStructure(_p->CurScope()->localLookup (c->u.comm.var, NULL))) {
          auto lns = ((latch_info_t *)(c->space))->latch_numbers;
          Assert (lns.size()==1, "struct recv but not?");
          latest = lns[0];
        }
        else {
          latest = _get_latest_struct_latch(vi->id, c->u.comm.var, 
            ((latch_info_t *)(c->space))->latch_numbers);
        }
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

  case ACT_CHP_LOOP:
  case ACT_CHP_DOLOOP:
  {
    act_chp_gc_t *gc = c->u.gc;
    latest = _flow_assignments (gc->s, vi, latest);
    Assert (!(gc->next), "more than one loop branch at top-level?");
  }
    break;

  case ACT_CHP_SELECT_NONDET:
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
    int vpos = _var_in_list (vi->id, ((latch_info_t *)(c->space))->live_vars);
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

  case ACT_CHP_LOOP:
  case ACT_CHP_DOLOOP:
  {
    act_chp_gc_t *gc = c->u.gc;
    fail = fail || _check_all_muxes_mapped (gc->s, fail);
    Assert (!(gc->next), "more than one loop branch at top-level?");
  }
    break;

  case ACT_CHP_SELECT_NONDET:
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
    Assert ((linfo->live_vars).size() == (linfo->merge_mux_latch_number).size(), "hmm weird");
    int ctr = 0;
    for ( auto x : linfo->merge_mux_latch_number )
    {
      if (x != -1) 
      {
        for ( auto y : linfo->merge_mux_inputs.at(ctr) ) 
        {
          if ( y == -1 )
          {
            fprintf (stderr, "Unmapped mux at selection:\n");
            chp_print (stderr, c);
            fprintf (stderr, "\n\nUnmapped mux for variable: %s\n\n\n", 
                      (linfo->live_vars).at(ctr)->toid()->getName());
            fail = true;
          }
        }
      }
      ctr++;
    }
  }
  break;

  case ACT_CHP_FUNC:
    break;

  default:
    fatal_error ("What?");
    break;
  }
  return fail;
}

bool RingEngine::_check_no_self_assignments (act_chp_lang_t *c, bool fail)
{
  Scope *s = _p->CurScope();

  switch (c->type) {

  case ACT_CHP_SKIP:
  case ACT_CHP_SEND:
  case ACT_CHP_RECV:
    break;
  case ACT_CHP_ASSIGN:
    fail = fail || _var_appears_in_expr(c->u.assign.e,c->u.assign.id);
    break;

  case ACT_CHP_COMMA:
  case ACT_CHP_SEMI:
    for (listitem_t *li = list_first (c->u.semi_comma.cmd); li; li = list_next (li)) 
    {   
      fail = fail || _check_no_self_assignments ((act_chp_lang_t *) list_value (li), fail);
    }
    break;

  case ACT_CHP_LOOP:
  case ACT_CHP_DOLOOP:
  {
    act_chp_gc_t *gc = c->u.gc;
    fail = fail || _check_no_self_assignments (gc->s, fail);
    Assert (!(gc->next), "more than one loop branch at top-level?");
  }
    break;
  case ACT_CHP_SELECT_NONDET:
  case ACT_CHP_SELECT:
  {
    act_chp_gc_t *gc = c->u.gc;
    while (gc) {
      fail = fail || _check_no_self_assignments (gc->s, fail);
      gc = gc->next;
    }
  }
  break;

  case ACT_CHP_FUNC:
    break;

  default:
    fatal_error ("What?");
    break;
  }
  return fail;
}

void RingEngine::construct_var_infos (act_chp_lang_t *c)
{
  var_infos = {};
  var_info *v;
  ActId *id;
  char str[1024];

  act_boolean_netlist_t *bnl = _bp->getBNL(_p);
  Assert (bnl, "hmm BNL");
  pHashtable *pht = bnl->cH;
  bool warn_once_bools = true;

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
      if (TypeFactory::isDataType (conn->getvx()->t) 
       || TypeFactory::isChanType (conn->getvx()->t)) 
      {
        v = new var_info;
        v->id = id;
        v->width = bv->width;
        v->nread = 0;
        v->iread = 0;
        v->nwrite = 0;
        v->iwrite = 0;
        v->fischan = TypeFactory::isChanType (conn->getvx()->t);
        v->fisbool = TypeFactory::isBoolType (conn->getvx()->t);
        v->fisextbool = 0;
        v->latest_for_read = 0;
        get_true_name (str, id, _p->CurScope());
        v->name = Strdup (str);
        if (v->fisbool && bv->ischpport) {
          v->fisextbool = 1;
          if (warn_once_bools) {
            fprintf(stdout, "\nWARNING: Bool ports in process, they must be read-only. Hope you know what you're doing.\n");
            warn_once_bools = false;
          }
        }
        var_infos.insert({conn,v});
      }
    }
  }

  ihash_iter_init (pht, &it);
  while ((ib = ihash_iter_next (pht, &it))) {
    bv = (act_booleanized_var_t *)ib->v;
    if (bv->usedchp) {
      conn = bv->id;
      id = conn->toid();
      if (TypeFactory::isDataType (conn->getvx()->t)) 
      {
        var_info *v1 = _get_var_info(id);
        _construct_var_info (c, id, v1);
      }
      // for probes
      else if (TypeFactory::isChanType (conn->getvx()->t))
      {
        var_info *v1 = _get_var_info(id);
        _construct_var_info (c, id, v1);
        if (v1->iwrite>1 || v1->iread>1) {
          fprintf(stderr, "\nChannel name: %s\n", v1->name);
          fatal_error ("Multiple channel access detected. Cannot synthesize. Run decomp first.");
        }
      }
    }
  }

}

void RingEngine::print_var_infos (FILE *fp)
{
  fprintf (fp, "\nvar_info hashtable: \n");
  for ( auto v : var_infos ) {
    _print_var_info (fp, v.second);
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

var_info *RingEngine::_get_var_info (ActId *id)
{
  Assert (var_infos.count(id->Canonical(_p->CurScope())), "variable not found");
  auto v = var_infos.at(id->Canonical(_p->CurScope()));
  return v;
}

VI_Table RingEngine::_deepcopy_var_info_hashtable (VI_Table h_in, int only_read_id)
{
  VI_Table ret = {};
  for ( auto v : h_in ) {
    ret.insert({v.first,_deepcopy_var_info(v.second, only_read_id)});
  }
  return ret;
}

void RingEngine::save_var_infos ()
{
  var_infos_copy = var_infos;
}

void RingEngine::_push_read_ids()
{
  auto tmp_vis = _deepcopy_var_info_hashtable (var_infos, 1);
  H_stk.push(tmp_vis);
}

void RingEngine::_pop_and_restore_read_ids()
{
  Assert (!H_stk.empty(), "Tried to pop from empty var_infos stack");
  auto hi = H_stk.top();
  H_stk.pop();

  for ( auto v : var_infos ) {
    auto vi = v.second;
    auto vi_saved = hi.at(v.first);
    vi->latest_for_read = vi_saved->latest_for_read;
  }
}

void RingEngine::restore_var_infos ()
{
    var_infos = var_infos_copy;
    var_infos_copy.clear();
}

var_info *RingEngine::_deepcopy_var_info (var_info *v, int only_read_id)
{
  var_info *v_copy = new var_info;
  v_copy->name = v->name;
  v_copy->latest_for_read = v->latest_for_read;
  if (!only_read_id) {
    v_copy->fcurexpr = v->fcurexpr;
    v_copy->fischan = v->fischan;
    v_copy->fisinport = v->fisinport;
    v_copy->fisbool = v->fisbool;
    v_copy->fisextbool = v->fisextbool;
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
    return _expr_id++;
}

void RingEngine::_reset_expr_id()
{
  _expr_id = 0;
}

int RingEngine::_gen_var_access_id()
{
    _var_access_id++;
    return _var_access_id;
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
  Assert (((c->type == ACT_CHP_SELECT)||(c->type == ACT_CHP_SELECT_NONDET)||(c->type == ACT_CHP_LOOP)||(c->type == ACT_CHP_DOLOOP)), 
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
    case ACT_CHP_SELECT_NONDET:
        return false;
        break;
        
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
    case ACT_CHP_SELECT_NONDET:
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

int RingEngine::_var_in_list (ActId *id, std::vector<act_connection *> l)
{
  int ctr = 0;
  for (auto v : l)
  {
    if (_check_ids_equal(id, v->toid())) {
      return ctr;
    }
    ctr++;
  }
  return NOT_FOUND;
}