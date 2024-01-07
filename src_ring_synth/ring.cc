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
            const char *exprfile = "expr.act")
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
                _expr_block_id = 0;
                _mux_block_id = 0;
                _branch_id = 0;
            }; 

void RingEngine::run_forge ()
{
    /* Handling
     * 'everything else besides the chp body'
     * needs to be added here
    */

   construct_var_infos ();
   _run_forge_helper ();

}

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
    if (id->isEqual(c->u.comm.var))
    { 
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

  case ACT_CHP_LOOP:
  case ACT_CHP_SELECT:
  case ACT_CHP_SELECT_NONDET:
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

void RingEngine::construct_var_infos ()
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
        v->latest_latch_branches = list_new();
        get_true_name (str, id, _p->CurScope());
        v->name = Strdup (str);
        
        _construct_var_info (_c, id, v);
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
    v_copy->latest_latch_branches = list_dup(v->latest_latch_branches);
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
  Assert (((c->type == ACT_CHP_SELECT)||(c->type == ACT_CHP_LOOP)), 
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
                if (stmt->type == ACT_CHP_LOOP)
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
