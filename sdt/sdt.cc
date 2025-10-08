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
#include <act/extmacro.h>
#include <common/int.h>
#include <string.h>
#include "sdt.h"

#ifndef MAX
#define MAX(a, b) (((a) > (b)) ? (a) : (b))
#endif


static int _expr_has_probe (Expr *e)
{
  auto is_basic_probe =
    [] (const Expr *e) {
      return e->type == E_PROBE
	|| (e->type == E_AND && e->u.e.l->type == E_PROBE);
    };

  if (!e) return 0;

  if (is_basic_probe (e)) {
    return 1;
  }
  if (e->type == E_OR && is_basic_probe (e->u.e.l)) {
    return 1;
  }
  return 0;
}

static int _guards_have_probes (act_chp_gc_t *gc)
{
  while (gc) {
    if (gc->g) {
      if (_expr_has_probe (gc->g)) {
	return 1;
      }
    }
    gc = gc->next;
  }
  return 0;
}


void SDTEngine::_emit_one_guard_expr (Expr *e, list_t *res)
{
  int eid;
  if (e) {
    _emit_expr (&eid, 1, e);
    eid = _gen_safe_bool (eid);
    list_iappend (res, eid);
  }
  else {
    eid = -1;
    list_iappend (res, eid);
  }
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
    _emit_expr_const (eid, 1, 1, true);
    list_iappend (res, eid);
  }
  else {
    tmp = gc;
    while (tmp) {
      _emit_one_guard_expr (tmp->g, res);
      tmp = tmp->next;
    }
  }
}


void SDTEngine::_run_sdt_helper (int id, act_chp_lang_t *c)
{
  list_t *tl;
  int pblock = _block_id;
  int changed = 0;
  
  if (!c) return;
  
  switch (c->type) {
  case ACT_CHP_SKIP:
    _emit_skip (id);
    break;

  case ACT_CHP_ASSIGNSELF:
  case ACT_CHP_ASSIGN:
    {
      int eid;

      _emit_expr (&eid, bitWidth (c->u.assign.id), c->u.assign.e);

      if (c->type == ACT_CHP_ASSIGNSELF) {
	/*-- generate a fresh variable --*/
	ActId *tmpv = NULL;

	if (_gen_fresh_var (bitWidth (c->u.assign.id), &tmpv)) {
	  int fseq = _gen_stmt_id ();
	  int tstmt = _gen_stmt_id ();

	  _emit_trueseq (fseq, tstmt);
	  _emit_transfer (tstmt, eid, tmpv);

	  tstmt = _gen_stmt_id ();

	  eid = _gen_expr_id ();
	  _emit_var_read (eid, tmpv);
	  _emit_transfer (tstmt, eid, c->u.assign.id);

	  list_t *l = list_new ();
	  list_iappend (l, fseq);
	  list_iappend (l, tstmt);

	  _emit_semi (id, l);
	  
	  list_free (l);
	}
	else {
	  _emit_transfer (id, eid, c->u.assign.id);
	}
      }
      else {
	_emit_transfer (id, eid, c->u.assign.id);
      }
    }
    break;

  case ACT_CHP_SEND:
    {
      Expr *e;
      int eid, vid;
      e = c->u.comm.e;

      if (e && bitWidth (c->u.comm.chan) == -1) {
	/* special case: send a structure */
	Assert (e->type == E_VAR, "What?");
	eid = _gen_expr_id ();
	_emit_var_read_struct (eid, (ActId *)e->u.e.l);
	_emit_transfer (id, eid, c->u.comm.chan);
      }
      else {
	if (e) {
	  _emit_expr (&eid, bitWidth (c->u.comm.chan), e);
	}
	else {
	  Expr *tmpe;
	  NEW (tmpe, Expr);
	  tmpe->type = E_FALSE;
	  _emit_expr (&eid, bitWidth (c->u.comm.chan), tmpe);
	  FREE (tmpe);
	}
	_emit_transfer (id, eid, c->u.comm.chan);
      }
    }
    break;

  case ACT_CHP_RECV:
    _emit_recv (id, c->u.comm.chan, c->u.comm.var);
    break;

  case ACT_CHP_COMMA:
    if (pblock == -1) {
      _block_id = 0;
      changed = 1;
    }
  case ACT_CHP_SEMI:
    if (pblock == -1 && c->type == ACT_CHP_SEMI) {
      _block_id = -2;
    }
    tl = list_new ();
    for (listitem_t *li = list_first (c->u.semi_comma.cmd); li; li = list_next (li)) {
      int id = _gen_stmt_id ();
      act_chp_lang_t *x = (act_chp_lang_t *) list_value (li);

      /* if the item is not a semi or comma, add an implicit sequencer */
      if (c->type == ACT_CHP_COMMA &&
	  (x->type != ACT_CHP_COMMA && x->type != ACT_CHP_SEMI)) {
	/*-- add an implicit semicolon --*/
	list_t *tmp = list_new ();
	int id2 = _gen_stmt_id ();
	list_iappend (tmp, id2);
	_emit_semi (id, tmp);
	
	_run_sdt_helper (id2, x);
      }
      else {
	_run_sdt_helper (id, x);
      }
      
      if (changed) {
	_block_id++;
      }
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
    if (pblock == -1) {
      _block_id = -2;
    }
    {
      list_t *gl;
      list_t *data_gl;
      list_t *idl;
      int gid, sid;
      int have_probes = 0;
      gl = list_new ();
      idl = list_new ();

      /* 1. Emit all guards */
      if (_guards_have_probes (c->u.gc)) {
	if (c->type != ACT_CHP_SELECT) {
	  act_error_ctxt (stderr);
	  fatal_error ("Unable to handle generalized probes");
	}
	have_probes = 1;
      }

      if (have_probes) {
	/* creat ea list of list of guards! */
	act_chp_gc_t *gc = c->u.gc;

	data_gl = list_new ();
	while (gc) {
	  if (!gc->g) {
	    act_error_ctxt (stderr);
	    fatal_error ("Probed guards cannot include an else clause");
	  }
	  if (_expr_has_probe (gc->g)) {
	    list_t *m, *p;
	    Expr *tmp;
	    Expr *egc;

	    /*
	      (#A & ... ) | (#B & ... ) | ... 
	      m = list of guard expressions (non-probed pieces)
	    */
	    m = list_new ();
	    egc = gc->g;
	    while (egc) {
	      if (egc->type == E_OR) {
		tmp = egc->u.e.l;
	      }
	      else {
		tmp = egc;
	      }
	      while (tmp && tmp->type == E_AND && tmp->u.e.l->type == E_PROBE) {
		tmp = tmp->u.e.r;
	      }
	      if (tmp->type == E_PROBE) {
		int eid = _gen_expr_id ();
		_emit_expr_const (eid, 1, 1, true);
		list_iappend (m, eid);
		list_iappend (data_gl, eid);
	      }
	      else {
		_emit_one_guard_expr (tmp, m);
		list_iappend (data_gl,
			      list_ivalue (list_tail (m)));
	      }
	      if (egc->type == E_OR) {
		egc = egc->u.e.r;
	      }
	      else {
		break;
	      }
	    }

	    /* 
	       p = list of list of probes
	    */
	    p = list_new ();
	    egc = gc->g;
	    while (egc) {
	      list_t *pl = list_new ();
	      if (egc->type == E_OR) {
		tmp = egc->u.e.l;
	      }
	      else {
		tmp = egc;
	      }
	      while (tmp && tmp->type == E_AND && tmp->u.e.l->type == E_PROBE) {
		int pid = _emit_chan_to_probe ((ActId *)tmp->u.e.l->u.e.l);
		list_iappend (pl, pid);
		tmp = tmp->u.e.r;
	      }
	      if (tmp->type == E_PROBE) {
		int pid = _emit_chan_to_probe ((ActId *)tmp->u.e.l);
		list_iappend (pl, pid);
	      }

	      list_append (p, pl);

	      if (egc->type == E_OR) {
		egc = egc->u.e.r;
	      }
	      else {
		break;
	      }
	    }

	    /* now emit probed clause! */
	    int pc = _emit_probed_clause (m, p);
	    list_iappend (gl, pc);

	    list_free (m);
	    for (listitem_t *li = list_first (p); li; li = list_next (li)) {
	      list_free ((list_t *) list_value (li));
	    }
	    list_free (p);
	  }
	  else {
	    list_t *m = list_new ();
	    list_t *p = list_new ();
	    /* singleton guard, no probes */
	    _emit_one_guard_expr (gc->g, m);
	    list_iappend (data_gl, list_ivalue (list_tail (m)));
	    list_append (p, list_new ());
	    
	    int pc = _emit_probed_clause (m, p);
	    
	    list_free (m);
	    list_free ((list_t *)list_first (p));
	    list_free (p);

	    list_iappend (gl, pc);
	  }
	  gc = gc->next;
	}
      }
      else {
	_emit_guardlist (1, c->u.gc, gl);
      }

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
	if (have_probes) {
	  _emit_probed_select (id, data_gl, gl, idl);
	  list_free (data_gl);
	}
	else {
	  _emit_select (0, id, gl, idl);
	}
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
    _emit_skip (id);
    break;
    
  default:
    fatal_error ("What?");
    break;
  }
  _block_id = pblock;
}
				 
void SDTEngine::run_sdt (Process *p, int emit_end_braces)
{
  struct act_chp *chp = NULL;
  ihash_iter_t iter;
  ihash_bucket_t *b;

  P = p;
  if (p->getlang() != NULL && p->getlang()->getchp() != NULL
      /* hse/prs has not already been specified */
      && !p->getlang()->gethse() && !p->getlang()->getprs()) {
    /* check if this is a macro */
    ExternMacro *macro = new ExternMacro (p);
    if (macro->isValid()) {
      /*-- then we already have an external definition! --*/
      delete macro;
      _emit_begin (emit_end_braces);
      _emit_end (-1, emit_end_braces);
      return;
    }
    delete macro;
    chp = p->getlang()->getchp();

    /*
      For all built-in translations with production rules that
      are specified in the chp2prs confguration file, we need to
      omit syntax-directed translation since we already have the
      circuit in the library.

      The CHP for the standard components must be in a namespace
      called "std" (could be nested)
    */
    if (p->getns() && p->getns() != ActNamespace::Global()) {
      if (strcmp (p->getns()->getName(), "std") == 0) {
	list_t *l = ActNamespace::Act()->getDecompTypes ();
	if (l) {
	  for (listitem_t *li = list_first (l); li; li = list_next (li)) {
	    if (p == (Process *) list_value (li)) {
	      list_free (l);
	      return;
	    }
	  }
	  list_free (l);
	}
      }
    }
  }
  else {
    _emit_begin (emit_end_braces);
    _emit_end (-1, emit_end_braces);
    return;
  }

  P = p;

  _block_id = -1;

  _emit_begin (emit_end_braces);


  int toplev = _gen_stmt_id ();
  _block_id = -1;
  _run_sdt_helper (toplev, chp->c);
  _emit_end (toplev, emit_end_braces);
}




void SDTEngine::_emit_expr_helper (int id, int *width, Expr *e)
{
  int lw, rw;
  int lid, rid;
  int tw, tid;

  Assert (e, "Hmm");


#define CHECK_EXPR(ex,myid,myw)					\
  do {								\
    if ((ex)->type == E_VAR) {					\
      ihash_bucket_t *b;					\
      b = ihash_lookup (_exprmap, (long)(ex));			\
      Assert (b, "What?");					\
      myid = b->i < 0 ? -b->i : b->i;				\
      myw = bitWidth ((ActId *)(ex)->u.e.l);			\
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
    *width = lw + (1 << rw) - 1;
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
      *width = e->u.e.r->u.ival.v;
    }
    else {
      *width = 1;
    }
    _emit_expr_width_conv (lid, lw, id, *width);
    break;

  case E_BUILTIN_BOOL:
    UNARY_OP;
    rid = list_ivalue (_intiter);
    _intiter = list_next (_intiter);
    _intiter = list_next (_intiter);
    *width = 1;
    _emit_expr_binary (id, *width, E_NE, lid, lw, rid, *width);
    break;

  case E_QUERY:
    CHECK_EXPR (e->u.e.l, tid, tw);
    if (tw != 1) {
      warning ("Typechecking should have failed on ternary expression!");
      fprintf (stderr, "  Expr: ");
      print_uexpr (stderr, e);
      fprintf (stderr, "\n");
    }
    CHECK_EXPR (e->u.e.r->u.e.l, lid, lw);
    CHECK_EXPR (e->u.e.r->u.e.r, rid, rw);
    *width = MAX(lw,rw);
    _emit_expr_ite (id, *width, e->type, tid, lid, lw, rid, rw);
    break;

  case E_COLON:
  case E_COMMA:
    fatal_error ("Should have been handled elsewhere");
    break;

  case E_CONCAT:
    {
      int count;
      list_t *l = list_new ();
      *width = 0;
      do {
	CHECK_EXPR (e->u.e.l, lid, lw);
	list_iappend (l, lid);
	list_iappend (l, lw);
	*width += lw;
	e = e->u.e.r;
      } while (e);

      count = list_length (l);
      while (count > 4) {
	int tid;
	lid = list_delete_ihead (l);
	lw = list_delete_ihead (l);

	rid = list_delete_ihead (l);
	rw = list_delete_ihead (l);

	tid = _gen_expr_id ();
	_emit_expr_concat2 (tid, lw+rw, lid, lw, rid, rw);
	list_iappend_head (l, lw+rw);
	list_iappend_head (l, tid);
	count -= 2;
      }

      Assert (count == 4, "Hmm");
      
      lid = list_delete_ihead (l);
      lw = list_delete_ihead (l);

      rid = list_delete_ihead (l);
      rw = list_delete_ihead (l);

      *width = lw + rw;
      _emit_expr_concat2 (id, *width, lid, lw, rid, rw);
      list_free (l);
    }
    break;

  case E_BITFIELD:
    {
      ihash_bucket_t *b;
      int lsb, msb;
      
      b = ihash_lookup (_exprmap, (long)(e));
      Assert (b, "What?");
      lid = b->i < 0 ? -b->i : b->i;
      lw = bitWidth ((ActId *)e->u.e.l);

      if (e->u.e.r->u.e.l) {
	/* r = msb, l = lsb */
	Assert (e->u.e.r->u.e.r->type == E_INT, "What?");
	msb = e->u.e.r->u.e.r->u.ival.v;
	Assert (e->u.e.r->u.e.l->type == E_INT, "What?");
	lsb = e->u.e.r->u.e.l->u.ival.v;
      }
      else {
	Assert (e->u.e.r->u.e.r->type == E_INT, "What?");
	msb = e->u.e.r->u.e.r->u.ival.v;
	lsb = msb;
      }
      *width = (msb - lsb + 1);
      _emit_expr_bitfield (id, lsb, msb, lid, lw);
    }
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


void SDTEngine::_expr_collect_vars (Expr *e, int collect_phase)
{
  int id;

  Assert (e, "Hmm");

#define BINARY_OP					\
  do {							\
    _expr_collect_vars (e->u.e.l, collect_phase);	\
    _expr_collect_vars (e->u.e.r, collect_phase);	\
  } while (0)

#define UNARY_OP					\
  do {							\
    _expr_collect_vars (e->u.e.l, collect_phase);	\
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
    UNARY_OP;
    break;

  case E_BUILTIN_BOOL:
    UNARY_OP;
    if (collect_phase) {
      int w = 1;
      int val = 0;
      id = _gen_expr_id ();
      list_iappend (_intconst, id);
      list_iappend (_intconst, w);
    }
    else {
      int w;
      id = list_ivalue (_intiter);
      _intiter = list_next (_intiter);
      w = list_ivalue (_intiter);
      _intiter = list_next (_intiter);
      _emit_expr_const (id, w, 0);
    }
    break;

  case E_QUERY:
    UNARY_OP;
    e = e->u.e.r;
    BINARY_OP;
    break;

  case E_COLON:
  case E_COMMA:
    fatal_error ("Should have been handled elsewhere");
    break;

    /* XXX: here */
  case E_CONCAT:
    do {
      _expr_collect_vars (e->u.e.l, collect_phase);
      e = e->u.e.r;
    } while (e);
    break;

  case E_BITFIELD:
    if (collect_phase) {
      ihash_bucket_t *b;
      if (!ihash_lookup (_exprmap, (long)e)) {
	b = ihash_add (_exprmap, (long)e);
	b->i = _gen_expr_id ();
      }
    }
    else {
      ihash_bucket_t *b;
      b = ihash_lookup (_exprmap, (long)e);
      if (b->i >= 0) {
	_emit_var_read (b->i, (ActId *)e->u.e.l);
	b->i = -b->i;
      }
    }
    break;

  case E_REAL:
    fatal_error ("No real expressions please.");
    break;

  case E_TRUE:
    if (collect_phase) {
      id = _gen_expr_id ();
      list_iappend (_boolconst, id);
    }
    else {
      id = list_ivalue (_booliter);
      _booliter = list_next (_booliter);
      _emit_expr_const (id, 1, 1);
    }
    break;
    
  case E_FALSE:
    if (collect_phase) {
      id = _gen_expr_id ();
      list_iappend (_boolconst, id);
    }
    else {
      id = list_ivalue (_booliter);
      _booliter = list_next (_booliter);
      _emit_expr_const (id, 1, 0);
    }
    break;
    
  case E_INT:
    if (collect_phase) {
      int w = 0;
      int val = e->u.ival.v;

      if (e->u.ival.v_extra) {
	w = ((BigInt *)e->u.ival.v_extra)->getWidth ();
      }
      else {
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
      }
      id = _gen_expr_id ();
      list_iappend (_intconst, id);
      list_iappend (_intconst, w);
    }
    else {
      int w;
      id = list_ivalue (_intiter);
      _intiter = list_next (_intiter);
      w = list_ivalue (_intiter);
      _intiter = list_next (_intiter);
      _emit_expr_const (id, w, e->u.ival.v);
    }
    break;

  case E_VAR:
    if (collect_phase) {
      ihash_bucket_t *b;
      if (!ihash_lookup (_exprmap, (long)e)) {
	b = ihash_add (_exprmap, (long)e);
	b->i = _gen_expr_id ();
      }
    }
    else {
      ihash_bucket_t *b;
      b = ihash_lookup (_exprmap, (long)e);
      if (b->i >= 0) {
	_emit_var_read (b->i, (ActId *)e->u.e.l);
	b->i = -b->i;
      }
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
  list_t *all_leaves;
  listitem_t *li;

  /*-- recursively expand expression --*/
  if (!e) {
    fatal_error ("Emit NULL expression?!");
  }

  if (sdt_error()) {
    return;
  }

  Assert (!_exprmap, "What?");
  _exprmap = ihash_new (4);
  _intconst = list_new ();
  _boolconst = list_new ();
  
  _expr_collect_vars (e, 1);

  all_leaves = list_new ();
  {
    ihash_iter_t iter;
    ihash_bucket_t *ib;

    for (li = list_first (_intconst); li; li = list_next (li)) {
      int ival = list_ivalue (li);
      li = list_next (li);
      int iw = list_ivalue (li);
      list_iappend (all_leaves, ival);
      list_iappend (all_leaves, iw);
    }
    for (li = list_first (_boolconst); li; li = list_next (li)) {
      int ival = list_ivalue (li);
      list_iappend (all_leaves, ival);
      list_iappend (all_leaves, 1);
    }

    ihash_iter_init (_exprmap, &iter);
    while ((ib = ihash_iter_next (_exprmap, &iter))) {
      Expr *e = (Expr *)ib->key;
      list_iappend (all_leaves, ib->i < 0 ? -ib->i : ib->i);
      list_iappend (all_leaves, bitWidth ((ActId *)e->u.e.l));
    }
  }

  int xid = _gen_expr_blk_id();
  if (_efp) {
    /* emit a block! */
    fprintf (_efp, "export defproc blk%d (\n", xid);
    for (li = list_first (all_leaves); li; li = list_next (li)) {
      int ival = list_ivalue (li);
      li = list_next (li);
      int iw = list_ivalue (li);
      fprintf (_efp, "\t syn::sdtexprchan<%d> eo%d;\n", iw, ival);
    }
    fprintf (_efp, "\t syn::sdtexprchan<%d> out)\n{\n", tgt_width);

    for (li = list_first (all_leaves); li; li = list_next (li)) {
      int ival = list_ivalue (li);
      li = list_next (li);
      int iw = list_ivalue (li);
      fprintf (_efp, "\t syn::expr::nullint<%d> e%d(eo%d);\n", iw, ival, ival);
    }
  }

  /*-- emit leaves --*/
  _intiter = list_first (_intconst);
  _booliter = list_first (_boolconst);
  _expr_collect_vars (e, 0);


  /*-- emit expression --*/
  _intiter = list_first (_intconst);
  _booliter = list_first (_boolconst);
  CHECK_EXPR (e, myid, width);
  *id = myid;

  /*-- width-conversion --*/
  myid = _gen_expr_id ();
  _emit_expr_width_conv (*id, width, myid, tgt_width);
  *id = myid;

  if (_efp) {
    listitem_t *li;
    list_t *ids;
    ihash_iter_t iter;
    ihash_bucket_t *ib;

    fprintf (_efp, "   out=e%d.out;\n", *id);
    fprintf (_efp, "}\n\n");
    
    ids = list_new ();
    
    for (li = list_first (all_leaves); li; li = list_next (li)) {
      list_iappend (ids, list_ivalue (li));
      li = list_next (li);
    }
    _emit_expr_block (*id, xid, ids);
  }
  
  ihash_free (_exprmap);
  _exprmap = NULL;

  list_free (_intconst);
  _intconst = NULL;

  list_free (_boolconst);
  _boolconst = NULL;

  list_free (all_leaves);
}



int SDTEngine::_gen_inst_id ()
{
  return _inst_id++;
}


int SDTEngine::_gen_stmt_id ()
{
  return _stmt_id++;
}


int SDTEngine::_gen_expr_id ()
{
  return _expr_id++;
}

int SDTEngine::_blk_id = 0;

int SDTEngine::_gen_expr_blk_id ()
{
  return _blk_id++;
}


SDTEngine::SDTEngine (const char *exprfile)
{
  P = NULL;
  _exprmap = NULL;
  _exprfile = exprfile;
  if (exprfile) {
    _efp = fopen (exprfile, "a");
  }
  else {
    _efp = NULL;
  }
}

int SDTEngine::bitWidth (ActId *id)
{
  if (!id) {
    return -1;
  }
  InstType *it = P->CurScope()->FullLookup (id, NULL);
  if (!it) {
    return -1;
  }
  return TypeFactory::bitWidth (it);
}
