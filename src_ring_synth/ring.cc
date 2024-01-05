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

void RingEngine::run_forge ()
{
    /* Handling the 
     * 'everything else besides the chp body'
     * needs to be added here
    */

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

void RingEngine::_construct_var_infos (ActBooleanizePass *bp)
{
  var_infos = hash_new(4);
  hash_bucket_t *b;
  var_info *v;
  ActId *id;
  char str[1024];

  act_boolean_netlist_t *bnl = bp->getBNL(_p);
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

void RingEngine::_print_var_infos (FILE *fp)
{
  fprintf (fp, "\nvar_info hashtable: \n");
  hash_iter_t it;
  hash_bucket_t *b;
  hash_iter_init (var_infos, &it);
    while ((b = hash_iter_next (var_infos, &it))) 
    {
      print_var_info (fp, (var_info *)b->v);
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