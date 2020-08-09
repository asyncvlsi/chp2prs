/*************************************************************************
 *
 *  Copyright (c) 2020 Rajit Manohar
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
#include <act/lang.h>
#include "sdt.h"

#ifndef MAX
#define MAX(a, b) (((a) > (b)) ? (a) : (b))
#endif

#define ACT_CHP_ASSIGNSELF (ACT_CHP_STMTEND+1)

varmap_info *SDTEngine::_var_getinfo (ActId *id)
{
  act_connection *c;
  ihash_bucket_t *b;
  varmap_info *v;
  InstType *it;

  c = id->Canonical (P->CurScope());
  Assert (c, "What?");
  
  b = ihash_lookup (_varmap, (long)c);
  if (!b) {
    b = ihash_add (_varmap, (long)c);
    NEW (v, varmap_info);

    v->nread = 0;
    v->nwrite = 0;
    v->iread = 0;
    v->iwrite = 0;
    v->id = id;
    it = P->Lookup (id);
    v->width = TypeFactory::bitWidth (it);
    v->fisbool = 0;
    if (TypeFactory::isChanType (it)) {
      v->fischan = 1;
      if (it->getDir() == Type::direction::IN) {
	v->fisinport = 1;
      }
      else if (it->getDir() == Type::direction::OUT) {
	v->fisinport = 0;
      }
      else {
	warning ("Please provide direction flags to channels! Assuming input.");
	v->fisinport = 1;
      }
    }
    else {
      v->fischan = 0;
      if (TypeFactory::isBoolType (it)) {
	v->fisbool = 1;
      }
    }
    b->v = v;
  }
  return (varmap_info *) b->v;
}

void SDTEngine::_construct_varmap_expr (Expr *e)
{
  act_connection *uid;
  varmap_info *v;
  
  if (!e) return;
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
    _construct_varmap_expr (e->u.e.l);
    _construct_varmap_expr (e->u.e.r);
    break;
    
  case E_NOT:
  case E_UMINUS:
  case E_COMPLEMENT:
    _construct_varmap_expr (e->u.e.l);
    break;

  case E_QUERY:
    _construct_varmap_expr (e->u.e.l);
    _construct_varmap_expr (e->u.e.r->u.e.l);
    _construct_varmap_expr (e->u.e.r->u.e.r);
    break;

  case E_COLON:
  case E_COMMA:
    fatal_error ("Should have been handled elsewhere");
    break;

  case E_CONCAT:
    do {
      _construct_varmap_expr (e->u.e.l);
      e = e->u.e.r;
    } while (e);
    break;

  case E_BITFIELD:
    /* l is an Id */
    v = _var_getinfo ((ActId *)e->u.e.l);
    if ((!_shared_expr_var || !v->fcurexpr) && !v->fischan) {
      v->nread++;
      v->fcurexpr = 1;
    }
    break;

  case E_TRUE:
  case E_FALSE:
  case E_INT:
  case E_REAL:
    break;

  case E_VAR:
    v = _var_getinfo ((ActId *)e->u.e.l);
    if ((!_shared_expr_var || !v->fcurexpr) && !v->fischan) {
      v->nread++;
      v->fcurexpr = 1;
    }
    break;

  case E_PROBE:
    v = _var_getinfo ((ActId *)e->u.e.l);
    if (!_shared_expr_var || !v->fcurexpr) {
      Assert (v->fischan, "What?");
      v->nread++;
      v->fcurexpr = 1;
    }
    break;

  case E_BUILTIN_BOOL:
  case E_BUILTIN_INT:
    _construct_varmap_expr (e->u.e.l);
    break;
    
  case E_FUNCTION:
    e = e->u.fn.r;
    while (e) {
      _construct_varmap_expr (e->u.e.l);
      e = e->u.e.r;
    }
    break;

  case E_SELF:
  default:
    fatal_error ("Unknown expression type %d\n", e->type);
    break;
  }
}

void SDTEngine::_clear_var_flags ()
{
  for (int i=0; i < _varmap->size; i++) {
    for (ihash_bucket_t *b = _varmap->head[i]; b; b = b->next) {
      varmap_info *v = (varmap_info *)b->v;
      v->fcurexpr = 0;
    }
  }
}
    
void SDTEngine::_construct_varmap (act_chp_lang_t *c)
{
  varmap_info *v;
  int x;
  if (!c) return;

  switch (c->type) {
  case ACT_CHP_SKIP:
    break;
  case ACT_CHP_ASSIGN:
    v = _var_getinfo (c->u.assign.id);
    x = v->nread;
    v->nwrite++;
    _clear_var_flags ();
    _construct_varmap_expr (c->u.assign.e);
    if (x != v->nread) {
      c->type = ACT_CHP_ASSIGNSELF;
    }
    break;
  case ACT_CHP_SEND:
    v = _var_getinfo (c->u.comm.chan);
    v->nwrite++;
    _clear_var_flags ();
    for (listitem_t *li = list_first (c->u.comm.rhs); li; li = list_next (li)){
      Expr *e = (Expr *) list_value (li);
      _construct_varmap_expr (e);
    }
    break;
  case ACT_CHP_RECV:
    v = _var_getinfo (c->u.comm.chan);
    v->nread++;
    for (listitem_t *li = list_first (c->u.comm.rhs); li; li = list_next (li)){
      ActId *id = (ActId *) list_value (li);
      v = _var_getinfo (id);
      v->nwrite++;
    }
    break;
  case ACT_CHP_COMMA:
  case ACT_CHP_SEMI:
    for (listitem_t *li = list_first (c->u.semi_comma.cmd); li; li = list_next (li)) {
      _construct_varmap ((act_chp_lang_t *) list_value (li));
    }
    break;
  case ACT_CHP_LOOP:
  case ACT_CHP_SELECT:
  case ACT_CHP_SELECT_NONDET:
  case ACT_CHP_DOLOOP:
    {
      act_chp_gc_t *gc = c->u.gc;

      /* group all guard variables together */
      _clear_var_flags ();
      while (gc) {
	if (gc->g) {
	  _construct_varmap_expr (gc->g);
	}
	gc = gc->next;
      }

      /* handle statements */
      gc = c->u.gc;
      while (gc) {
	_clear_var_flags ();
	_construct_varmap (gc->s);
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
  return;
}


void SDTEngine::_emit_guardlist (int isloop,
				 act_chp_gc_t *gc, list_t *res)
{
  act_chp_gc_t *tmp;
  int eid;
  Assert (gc, "Why am I here?");

  if (isloop && !gc->g) {
    /*-- infinite loop --*/
    eid = _gen_expr_id ();
    _emit_expr_const (eid, 1, 1);
    list_iappend (res, eid);
  }
  else {
    tmp = gc;
    while (tmp) {
      if (tmp->g) {
	_emit_expr (&eid, 1, tmp->g);
	eid = _gen_safe_bool (eid);
	list_iappend (res, eid);
      }
      else {
	eid = -1;
	list_iappend (res, eid);
      }
      tmp = tmp->next;
    }
  }
}


void SDTEngine::_run_sdt_helper (int id, act_chp_lang_t *c)
{
  varmap_info *v;
  list_t *tl;
  
  if (!c) return;
  
  switch (c->type) {
  case ACT_CHP_SKIP:
    _emit_skip (id);
    break;

  case ACT_CHP_ASSIGNSELF:
  case ACT_CHP_ASSIGN:
    {
      int eid;
      varmap_info *v;
      varmap_info xv;

      v = _var_getinfo (c->u.assign.id);
      _emit_expr (&eid, v->width, c->u.assign.e);

      if (c->type == ACT_CHP_ASSIGNSELF) {
	int fseq = _gen_stmt_id ();

	/*-- generate a fresh variable --*/
	xv = *v;
	_gen_fresh_var (&xv);

	int tstmt = _gen_stmt_id ();

	_emit_trueseq (fseq, tstmt);
	_emit_transfer (tstmt, eid, &xv);

	tstmt = _gen_stmt_id ();

	eid = _gen_expr_id ();
	_emit_var_read (eid, &xv);
	_emit_transfer (tstmt, eid, v);

	list_t *l = list_new ();
	list_iappend (l, fseq);
	list_iappend (l, tstmt);

	_emit_semi (id, l);

	list_free (l);
      }
      else {
	_emit_transfer (id, eid, v);
      }
      v->iwrite++;
    }
    break;

  case ACT_CHP_SEND:
    {
      Expr *e;
      int eid, vid;
      v = _var_getinfo (c->u.comm.chan);
      if (list_length (c->u.comm.rhs) > 1) {
	fatal_error ("Fix send tuples...");
      }
      if (list_length (c->u.comm.rhs) == 1) {
	e = (Expr *) list_value (list_first (c->u.comm.rhs));
      }
      else {
	e = NULL;
      }
      if (e) {
	_emit_expr (&eid, v->width, e);
      }
      else {
	eid = -1;
      }
      _emit_transfer (id, eid, v);
    }
    break;

  case ACT_CHP_RECV:
    {
      varmap_info *wv;
      
      v = _var_getinfo (c->u.comm.chan);
      if (list_length (c->u.comm.rhs) > 1) {
	fatal_error ("Fix recv tuples...");
      }
      if (list_length (c->u.comm.rhs) == 1) {
	wv = _var_getinfo ((ActId *)list_value (list_first (c->u.comm.rhs)));
      }
      else {
	wv = NULL;
      }
      _emit_recv (id, v, wv);
    }
    break;

  case ACT_CHP_COMMA:
  case ACT_CHP_SEMI:
    tl = list_new ();
    for (listitem_t *li = list_first (c->u.semi_comma.cmd); li; li = list_next (li)) {
      int id = _gen_stmt_id ();
      _run_sdt_helper (id, (act_chp_lang_t *) list_value (li));
      list_iappend (tl, id);
    }
    if (c->type == ACT_CHP_COMMA) {
      _emit_comma (id, tl);
    }
    else {
      _emit_semi (id, tl);
    }
    list_free (tl);
    break;
    
  case ACT_CHP_LOOP:
  case ACT_CHP_DOLOOP:
  case ACT_CHP_SELECT:
  case ACT_CHP_SELECT_NONDET:
    {
      list_t *gl;
      list_t *idl;
      int gid, sid;
      gl = list_new ();
      idl = list_new ();

      /* 1. Emit all guards */
      _emit_guardlist (1, c->u.gc, gl);

      /* 2. Emit all statements */
      for (act_chp_gc_t *gc = c->u.gc; gc; gc = gc->next) {
	sid = _gen_stmt_id ();	
	list_iappend (idl, sid);
	_run_sdt_helper (sid, gc->s);
      }

      /* 3. Emit construct! */
      if (c->type == ACT_CHP_LOOP) {
	_emit_loop (id, gl, idl);
      }
      else if (c->type == ACT_CHP_SELECT) {
	_emit_select (0, id, gl, idl);
      }
      else if (c->type == ACT_CHP_SELECT_NONDET) {
	_emit_select (1, id, gl, idl);
      }
      else if (c->type == ACT_CHP_DOLOOP) {
	_emit_doloop (id, list_ivalue (list_first (gl)),
		      list_ivalue (list_first (idl)));
      }
      else {
	Assert (0, "What?");
      }
      list_free (gl);
      list_free (idl);
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
				 
void SDTEngine::run_sdt (Process *p)
{
  struct act_chp *chp = NULL;
  if (p->getlang() != NULL && p->getlang()->getchp() != NULL) {
    chp = p->getlang()->getchp();
  }
  else {
    fatal_error ("Process `%s' does not have a CHP body?", p->getName());
  }

  if (_varmap) {
    for (int i=0; i < _varmap->size; i++) {
      for (ihash_bucket_t *b = _varmap->head[i]; b; b = b->next) {
	varmap_info *v = (varmap_info *)b->v;
	FREE (v);
      }
    }
    ihash_free (_varmap);
    _varmap = NULL;
  }
  P = p;

  _varmap = ihash_new (4);
  _construct_varmap (chp->c);

  _emit_begin ();

  /*-- emit all the variable ports and channel muxes --*/
  for (int i=0; i < _varmap->size; i++) {
    for (ihash_bucket_t *b = _varmap->head[i]; b; b = b->next) {
      varmap_info *v = (varmap_info *) b->v;
      if (v->fischan) {
	_emit_channel_mux (v);
      }
    }
  }
  
  int toplev = _gen_stmt_id ();
  _run_sdt_helper (toplev, chp->c);
  _emit_end (toplev);
}




void SDTEngine::_emit_expr_helper (int id, int *width, Expr *e)
{
  int lw, rw;
  int lid, rid;

  Assert (e, "Hmm");

#define CHECK_EXPR(ex,myid,myw)					\
  do {								\
    if ((ex)->type == E_VAR) {					\
      ihash_bucket_t *b;					\
      varmap_info *v;						\
      b = ihash_lookup (_exprmap, (long)(ex));			\
      Assert (b, "What?");					\
      v = _var_getinfo ((ActId *)(ex)->u.e.l);			\
      myid = b->i;						\
      myw = v->width;						\
    }								\
    else if ((ex)->type == E_TRUE || (ex)->type == E_FALSE) {	\
      myid = list_ivalue (_booliter);				\
      _booliter = list_next (_booliter);			\
      myw = 1;							\
    }								\
    else if ((ex)->type == E_INT) {				\
      myid = list_ivalue (_intiter);				\
      _intiter = list_next (_intiter);				\
      myw = list_ivalue (_intiter);				\
      _intiter = list_next (_intiter);				\
    }								\
    else {							\
      myid = _gen_expr_id ();					\
      _emit_expr_helper (myid, &myw, ex);			\
    }								\
  } while (0)
      

#define BINARY_OP				\
  do {						\
    CHECK_EXPR(e->u.e.l, lid, lw);		\
    CHECK_EXPR(e->u.e.r, rid, rw);		\
  } while (0)

#define UNARY_OP				\
  do {						\
    CHECK_EXPR (e->u.e.l, lid, lw);		\
  } while (0)
  
  switch (e->type) {
    /* binary */
  case E_AND:
  case E_OR:
  case E_XOR:
    BINARY_OP;
    *width = MAX(lw,rw);
    _emit_expr_binary (id, *width, e->type, lid, lw, rid, rw);
    break;
    
  case E_PLUS:
  case E_MINUS:
    BINARY_OP;
    *width = MAX(lw,rw)+1;
    _emit_expr_binary (id, *width, e->type, lid, lw, rid, rw);
    break;
    
  case E_LT:
  case E_GT:
  case E_LE:
  case E_GE:
  case E_EQ:
  case E_NE:
    BINARY_OP;
    *width = 1;
    _emit_expr_binary (id, *width, e->type, lid, lw, rid, rw);
    break;

  case E_MULT:
    BINARY_OP;
    *width = lw + rw;
    _emit_expr_binary (id, *width, e->type, lid, lw, rid, rw);
    break;
    
  case E_DIV:
    BINARY_OP;
    *width = lw;
    _emit_expr_binary (id, *width, e->type, lid, lw, rid, rw);
    break;
    
  case E_MOD:
    BINARY_OP;
    *width = rw;
    _emit_expr_binary (id, *width, e->type, lid, lw, rid, rw);
    break;
    
  case E_LSL:
    BINARY_OP;
    *width = lw + (1 << rw);
    _emit_expr_binary (id, *width, e->type, lid, lw, rid, rw);
    break;
    
  case E_LSR:
  case E_ASR:
    BINARY_OP;
    *width = lw;
    _emit_expr_binary (id, *width, e->type, lid, lw, rid, rw);
    break;
    
  case E_UMINUS:
  case E_NOT:
  case E_COMPLEMENT:
    UNARY_OP;
    *width = lw;
    _emit_expr_unary (id, *width, e->type, lid, lw);
    break;

  case E_BUILTIN_INT:
    UNARY_OP;
    if (e->u.e.r) {
      Assert (e->u.e.r->type == E_INT, "What?");
      *width = e->u.e.r->u.v;
    }
    else {
      *width = 1;
    }
    _emit_expr_width_conv (lid, lw, id, *width);
    break;

  case E_BUILTIN_BOOL:
    UNARY_OP;
    rid = _gen_expr_id ();
    _emit_expr_const (rid, 1, 0);
    *width = 1;
    _emit_expr_binary (id, *width, E_NE, lid, lw, rid, *width);
    break;

  case E_QUERY:
    CHECK_EXPR (e->u.e.r->u.e.l, lid, lw);
    CHECK_EXPR (e->u.e.r->u.e.r, rid, rw);
    *width = MAX(lw,rw);
    _emit_expr_binary (id, *width, e->type, lid, lw, rid, rw);
    break;

  case E_COLON:
  case E_COMMA:
    fatal_error ("Should have been handled elsewhere");
    break;

    /* XXX: here */
  case E_CONCAT:
    fatal_error ("fix concat");
    break;

  case E_BITFIELD:
    fatal_error ("fix bitfield");
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
  case E_VAR:
  case E_TRUE:
  case E_FALSE:
  case E_INT:
  default:
    fatal_error ("Unknown expression type %d\n", e->type);
    break;
  }
  return;
#undef BINARY_OP
#undef UNARY_OP
}


void SDTEngine::_expr_collect_vars (Expr *e)
{
  int id;
  int cid;

  Assert (e, "Hmm");

#define BINARY_OP				\
  do {						\
    _expr_collect_vars (e->u.e.l);		\
    _expr_collect_vars (e->u.e.r);		\
  } while (0)

#define UNARY_OP				\
  do {						\
    _expr_collect_vars (e->u.e.l);		\
  } while (0)
  
  switch (e->type) {
    /* binary */
  case E_AND:
  case E_OR:
  case E_XOR:
  case E_PLUS:
  case E_MINUS:
  case E_LT:
  case E_GT:
  case E_LE:
  case E_GE:
  case E_EQ:
  case E_NE:
  case E_MULT:
  case E_DIV:
  case E_MOD:
  case E_LSL:
  case E_LSR:
  case E_ASR:
    BINARY_OP;
    break;
    
  case E_UMINUS:
  case E_NOT:
  case E_COMPLEMENT:
  case E_BUILTIN_INT:
  case E_BUILTIN_BOOL:
    UNARY_OP;
    break;

  case E_QUERY:
    e = e->u.e.r;
    BINARY_OP;
    break;

  case E_COLON:
  case E_COMMA:
    fatal_error ("Should have been handled elsewhere");
    break;

    /* XXX: here */
  case E_CONCAT:
    fatal_error ("fix concat");
    break;

  case E_BITFIELD:
    fatal_error ("fix bitfield");
    break;

  case E_REAL:
    fatal_error ("No real expressions please.");
    break;

  case E_TRUE:
    id = _gen_expr_id ();
    _emit_expr_const (id, 1, 1);
    list_iappend (_boolconst, id);
    break;
    
  case E_FALSE:
    id = _gen_expr_id ();
    _emit_expr_const (id, 1, 0);
    list_iappend (_boolconst, id);
    break;
    
  case E_INT:
    {
      int w = 0;
      int val = e->u.v;
      if (val < 0) {
	val = -val;
	w = 32;
      }
      else {
      while (val) {
	val >>= 1;
	w++;
      }
      }
      if (w == 0) {
	w = 1;
      }
      id = _gen_expr_id ();
      cid = _gen_stmt_id ();
      _emit_expr_const (id, w, e->u.v);
      list_iappend (_intconst, id);
      list_iappend (_intconst, w);
    }
    break;

  case E_VAR:
    {
      varmap_info *v;
      ihash_bucket_t *b;
      v = _var_getinfo ((ActId *)e->u.e.l);
      b = ihash_add (_exprmap, (long)e);
      b->i = _gen_expr_id ();
      _emit_var_read (b->i, v);
    }
    break;

  case E_PROBE:
    fatal_error ("fix probes please");
    break;
    
  case E_FUNCTION:
    fatal_error ("function!");
  case E_SELF:
  default:
    fatal_error ("Unknown expression type %d\n", e->type);
    break;
  }
  return;
#undef BINARY_OP
#undef UNARY_OP
}


  /* id = expr_id for evaluating this expression */
void SDTEngine::_emit_expr (int *id, int tgt_width, Expr *e)
{
  int width;
  int myid;
  /*-- recursively expand expression --*/
  if (!e) {
    fatal_error ("Emit NULL expression?!");
  }

  Assert (!_exprmap, "What?");
  _exprmap = ihash_new (4);

  _intconst = list_new ();
  _boolconst = list_new ();
  
  _expr_collect_vars (e);

  _intiter = list_first (_intconst);
  _booliter = list_first (_boolconst);

  CHECK_EXPR (e, myid, width);
  *id = myid;

  list_t *eval = list_new ();

  if (_exprmap->n > 0 || list_length (_intconst) > 0 ||
      list_length (_boolconst) > 0) {
    /* connect the request to all these eid ports */
    
    /* collect all the ids */
    listitem_t *li;
    for (li = list_first (_intconst); li; li = list_next (li)) {
      list_iappend (eval, list_ivalue (li));
      li = list_next (li);
    }
    for (li = list_first (_boolconst); li; li = list_next (li)) {
      list_iappend (eval, list_ivalue (li));
    }
    for (int i=0; i < _exprmap->size; i++) {
      for (ihash_bucket_t *b = _exprmap->head[i]; b; b = b->next) {
	list_iappend (eval, b->i);
      }
    }
  }

  ihash_free (_exprmap);
  _exprmap = NULL;

  list_free (_intconst);
  _intconst = NULL;

  list_free (_boolconst);
  _boolconst = NULL;

  /*-- eval = the list of base cases you need --*/

  /*-- width-conversion --*/
  if (width != tgt_width) {
    myid = _gen_expr_id ();
    _emit_expr_width_conv (*id, width, myid, tgt_width);
    *id = myid;
  }
  
  list_free (eval);
}

