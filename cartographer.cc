/*************************************************************************
 *
 *  This file is part of chp2prs
 *
 *  Copyright (c) 2018, 2020 Rajit Manohar
 *  Copyright (c) 2018-2019 Zeb Mehring
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
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <act/types.h>
#include "cartographer.h"
#include <act/iter.h>
#include <act/value.h>

#ifdef CHP_OPTIMIZE
#include <act/chp-opt/optimize.h>
#endif

#include "basicsdt.h"

#define ACT_CHP_ASSIGNSELF (ACT_CHP_STMTEND+1)

/*
 *
 *  Core syntax-directed translation code written by Rajit Manohar
 *
 *  Extensions to full expressions, optimizations, and direct use of
 *  the ACT library  by Zeb Mehring
 *
 */


#define MAX(a, b) (((a) > (b)) ? (a) : (b))


/* Recursively called fn to handle different chp statement types */

/* Print proc definition & override CHP variables */
bool write_process_definition(FILE *fp, Process * p, const char * proc_name)
{
  bool has_overrides = 0;
  bool has_bool_overrides = 0;

  fprintf(fp, "defproc sdt_%s <: %s ()\n", proc_name, proc_name);

  int bw = 0;
  
  /* iterate through Scope Hashtable to find all chp variables */
  ActInstiter iter(p->CurScope());
  for (iter = iter.begin(); iter != iter.end(); iter++) {
    ValueIdx *vx = *iter;
    /* chan variable found */
    if (TypeFactory::isChanType (vx->t))
    {
      bw = TypeFactory::bitWidth(vx->t);
      if (!has_overrides) {
	fprintf(fp, "+{\n");
	has_overrides = true;
      }
      fprintf(fp, "  syn::sdtchan<%d> %s;\n", bw, vx->getName());
    }
    
    /* int variable found */
    if (TypeFactory::isIntType (vx->t)) {
      /* chp-optimize creates sel0, sel1,... & loop0, loop1, ... which do not have dualrail overrides */
      bw = TypeFactory::bitWidth(vx->t);
      if (!has_overrides) {
	fprintf(fp, "+{\n");
	has_overrides = true;
      }
      fprintf(fp, "  syn::sdtvar<%d> %s;\n", bw, vx->getName());
    }
    else if (TypeFactory::isBoolType (vx->t)) {
      if (!has_overrides) {
	fprintf(fp, "+{\n");
	has_overrides = true;
      }
      fprintf (fp, " syn::sdtboolvar %s;\n", vx->getName());
      has_bool_overrides = 1;
    }
  }
  /* end param declaration */
  if (has_overrides) {
    fprintf(fp, "}\n{\n");
  }
  else {
    fprintf(fp, "{\n");
  }

  if (has_bool_overrides) {
    int vconnect = 0;
    for (iter = iter.begin(); iter != iter.end(); iter++) {
      ValueIdx *vx = *iter;
      if (TypeFactory::isBoolType (vx->t)) {
	fprintf (fp, " syn::sdtvar<1> b_%s;\n", vx->getName());
	fprintf (fp, " syn::varconnect vc_%d(%s,b_%s);\n",
		 vconnect++, vx->getName(), vx->getName());
      }
    }
  }
  return has_overrides;
}

/* Initialize var_init_false vars for each CHP int */
void initialize_chp_ints(FILE *fp, Process * p, bool has_overrides)
{
  int bw = 0;

  /* iterate through Scope Hashtable to find all chp ints */
  fprintf(fp, "  /* Initialize chp vars */\n");

  ActInstiter iter(p->CurScope());
  for (iter = iter.begin(); iter != iter.end(); iter++) {
    ValueIdx *vx = *iter;
    
    /* int variable found */
    if (TypeFactory::isIntType (vx->t)) {
      bw = TypeFactory::bitWidth(vx->t);
      fprintf(fp, "  syn::var_init<%d,false> var_%s(%s);\n", bw,
	      vx->getName(), vx->getName());
    }
    else if (TypeFactory::isBoolType (vx->t)) {
      fprintf(fp, "  syn::var_init<1,false> var_%s(b_%s);\n",
	      vx->getName(), vx->getName());
    }
  }
  fprintf(fp, "\n");
}


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



int BasicSDT::_gen_inst_id ()
{
  return _inst_id++;
}


int BasicSDT::_gen_stmt_id ()
{
  fprintf (output_stream, "   syn::a1of1 c%d;\n", _stmt_id);
  return _stmt_id++;
}


int BasicSDT::_gen_expr_id ()
{
  return _expr_id++;
}


void BasicSDT::_emit_begin ()
{
  /* initialize the output location */ 
  if (output_file) {
    output_stream = fopen(output_file, "w");
    if (!output_stream) {
      fatal_error ("Could not open file `%s' for reading", output_file);
    }
  }
  else {
    output_stream = stdout;
  }


  /* get proc_name */
  size_t pn_len = strlen(P->getName());
  char proc_name[pn_len];
  strncpy(proc_name, P->getName(), pn_len-2);
  proc_name[pn_len-1] = '\0';
  
  /* print imports */
  fprintf(output_stream, "import syn;\n");
  if (bundled_data) fprintf(output_stream, "import bundled;\n");
  fprintf(output_stream, "\n");
  
  /* Print params for toplevel from process port list */
  int pnum = P->getNumPorts();
  bool has_overrides = false;
       
  /* Write process definition and variable declarations */
  int overrides = write_process_definition(output_stream, P, proc_name);
  initialize_chp_ints(output_stream, P, overrides);
}

void BasicSDT::_emit_end (int id)
{
  /* connect toplevel "go" signal and print wrapper process instantiation */
  
  fprintf (output_stream, "/*--- connect reset to go signal ---*/\n");

  fprintf (output_stream, "   prs { Reset => c%d.r- }\n", id);

  fprintf (output_stream, "}\n");
  
  if (output_file) fclose(output_stream);
}
  
void BasicSDT::_emit_skip (int id)
{
  int inst = _gen_inst_id ();
  fprintf (output_stream, "   syn::sskip s_%d(c%d);\n", inst, id);
}

const char *sdt_expr_name (int type)
{
  switch (type) {
  case E_AND:
    return "and";
    break;
    
  case E_OR:
    return "or";
    break;
    
  case E_XOR:
    return "xor";
    break;
    
  case E_PLUS:
    return "add";
    break;
    
  case E_MINUS:
    return "sub";
    break;
    
  case E_MULT:
    return "mult";
    break;
    
  case E_DIV:
    return "div";
    break;
    
  case E_MOD:
    return "mod";
    break;
    
  case E_LSL:
    return "lsl";
    break;
    
  case E_LSR:
    return "lsr";
    break;
    
  case E_ASR:
    return "asr";
    break;
    
  case E_LT:
    return "lt";
    break;
    
  case E_GT:
    return "gt";
    break;
    
  case E_LE:
    return "le";
    break;
    
  case E_GE:
    return "ge";
    break;
    
  case E_EQ:
    return "eq";
    break;
    
  case E_NE:
    return "ne";
    break;

  case E_UMINUS:
    return "uminus";
    break;
    
  case E_NOT:
  case E_COMPLEMENT:
    return "not";
    break;

  case E_QUERY:
    return "ite"; // if then else
    break;
    break;

  case E_COLON:
  case E_COMMA:
    fatal_error ("Should have been handled elsewhere");
    break;

  case E_CONCAT:
    return "concat";
    break;

  case E_BITFIELD:
    return "bitfield";
    break;

  case E_TRUE:
  case E_FALSE:
  case E_INT:
  case E_REAL:
  case E_VAR:
  case E_PROBE:

    fatal_error ("Shouldn't be here");
    break;

  case E_FUNCTION:
    fatal_error ("function!");
    break;

  case E_SELF:
  default:
    fatal_error ("Unknown expression type %d\n", type);
    break;
  }
  return "no-idea";
}


void BasicSDT::_emit_expr_binary (int id, int width,
				  int type,
				  int lid, int lw,
				  int rid, int rw)
{
  fprintf (output_stream, "   syn::expr::%s<%d,%d> e%d (e%d.out,e%d.out);\n",
	   sdt_expr_name (type), lw, rw, id, lid, rid);
}

void BasicSDT::_emit_expr_unary (int id, int width,
				 int type, int lid, int lw)
{
  fprintf (output_stream, "   syn::expr::%s<%d> e%d (e%d.out);\n",
	   sdt_expr_name (type), lw, id, lid);
}


void BasicSDT::_emit_expr_const (int id, int width, int val)
{
  fprintf (output_stream, "   syn::expr::const<%d,%d> e%d;\n",
	   width, val, id);
}

void BasicSDT::_emit_var_read (int eid, varmap_info *v)
{
  fprintf (output_stream, "   syn::expr::readport<%d> e%d(", v->width, eid);
  v->id->Print (output_stream);
  fprintf (output_stream, ");\n");
  v->iread++;
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

void BasicSDT::_emit_expr_width_conv (int from, int from_w,
				      int to, int to_w)
{
  fprintf (output_stream, "   syn::expr::widthconv<%d,%d> e%d(e%d.out);\n",
	   from_w, to_w, to, from);
}



void BasicSDT::_emit_transfer (int cid, int eid, varmap_info *ch)
{
  int wport = 0;
  if (!ch->fischan) {
    /* emit a write port */
    wport = _gen_inst_id ();
    fprintf (output_stream, "   syn::expr::writeport<%d> w_%d(", ch->width, wport);
    if (ch->fisbool) {
      fprintf (output_stream, "b_");
    }
    ch->id->Print (output_stream);
    fprintf (output_stream, ");\n");
  }
  fprintf (output_stream, "   syn::transfer<%d> s_%d(c%d, e%d.out,",
	   ch->width, _gen_inst_id(), cid, eid);
  if (ch->fischan) {
    /* pick the channel mux */
    ch->id->Print (output_stream);
    fprintf (output_stream, "_mux.m[%d]",
	     ch->fisinport ? ch->iread++ : ch->iwrite++);
  }
  else {
    fprintf (output_stream, "w_%d.in", wport);
  }
  fprintf (output_stream, ");\n");
}

void BasicSDT::_emit_recv (int cid, varmap_info *ch, varmap_info *v)
{
  fprintf (output_stream, "   syn::recv<%d> s_%d(c%d,", ch->width,
	   _gen_inst_id(), cid);
  ch->id->Print (output_stream);
  fprintf (output_stream, "_mux.m[%d]", ch->fisinport ? ch->iread++ :
	   ch->iwrite++);
  fprintf (output_stream, ",");
  if (v) {
    if (v->fisbool) {
      fprintf (output_stream, "b_");
    }
    v->id->Print (output_stream);
  }
  fprintf (output_stream, ");\n");
}


void BasicSDT::_emit_comma (int cid, list_t *stmts)
{
  listitem_t *li;
  fprintf (output_stream, "   syn::comma<%d> s_%d(c%d,{",
	   list_length (stmts), _gen_inst_id(), cid);
  for (li = list_first (stmts); li; li = list_next (li)) {
    if (li != list_first (stmts)) {
      fprintf (output_stream, ",");
    }
    fprintf (output_stream, "c%d", list_ivalue (li));
  }
  fprintf (output_stream, "});\n");
}

void BasicSDT::_emit_semi (int cid, list_t *stmts)
{
  listitem_t *li;
  fprintf (output_stream, "   syn::semi<%d> s_%d(c%d,{",
	   list_length (stmts), _gen_inst_id(), cid);
  for (li = list_first (stmts); li; li = list_next (li)) {
    if (li != list_first (stmts)) {
      fprintf (output_stream, ",");
    }
    fprintf (output_stream, "c%d", list_ivalue (li));
  }
  fprintf (output_stream, "});\n");
}

void BasicSDT::_emit_loop (int cid, list_t *guards, list_t *stmts)
{
  listitem_t *li;
  Assert (list_length (guards) == list_length (stmts), "emit_loop issue");

  fprintf (output_stream, "   syn::loop<%d> s_%d(c%d,{", list_length (guards),
	   _gen_inst_id(), cid);

  for (li = list_first (guards); li; li = list_next (li)) {
    if (li != list_first (guards)) {
      fprintf (output_stream, ",");
    }
    fprintf (output_stream, "e%d.out", list_ivalue (li));
  }
  fprintf (output_stream, "},{");

  for (li = list_first (stmts); li; li = list_next (li)) {
    if (li != list_first (stmts)) {
      fprintf (output_stream, ",");
    }
    fprintf (output_stream, "c%d", list_ivalue (li));
  }
  fprintf (output_stream, "});\n");
}

void BasicSDT::_emit_select (int is_nondet, int cid, list_t *guards, list_t *stmts)
{
  listitem_t *li;
  int else_case = 0;
  Assert (list_length (guards) == list_length (stmts), "emit_loop issue");

  li = list_tail (guards);
  if (list_ivalue (li) < 0) {
    /* else clause */
    else_case = 1;
  }
  else {
    else_case = 0;
  }

  fprintf (output_stream, "   syn::%sselect<%d,%s> s_%d(c%d,{",
	   is_nondet ? "arb_" : "",
	   list_length (guards)-else_case, else_case ? "true" : "false",
	   _gen_inst_id(), cid);

  for (li = list_first (guards); li; li = list_next (li)) {
    if (list_ivalue (li) < 0) continue;
    if (li != list_first (guards)) {
      fprintf (output_stream, ",");
    }
    fprintf (output_stream, "e%d.out", list_ivalue (li));
  }
  fprintf (output_stream, "},{");

  for (li = list_first (stmts); li; li = list_next (li)) {
    if (li != list_first (stmts)) {
      fprintf (output_stream, ",");
    }
    fprintf (output_stream, "c%d", list_ivalue (li));
  }
  fprintf (output_stream, "});\n");
}

void BasicSDT::_emit_doloop (int cid, int guard, int stmt)
{
  fprintf (output_stream, "   syn::doloop s_%d(c%d,e%d.out,c%d);\n",
	   _gen_inst_id(), cid, guard, stmt);
}

void BasicSDT::_emit_channel_mux (varmap_info *v)
{
  Assert (v->fischan, "What?");
  if (v->fisinport) {
    fprintf (output_stream, "   syn::muxinport<%d,%d> ", v->width, v->nread);
  }
  else {
    fprintf (output_stream, "   syn::muxoutport<%d,%d> ", v->width, v->nwrite);
  }
  v->id->Print (output_stream);
  fprintf (output_stream, "_mux(");
  v->id->Print (output_stream);
  fprintf (output_stream, ");\n");
}


void BasicSDT::_emit_trueseq (int cid, int sid)
{
  fprintf (output_stream, "   syn::fullseq s_%d(c%d,c%d);\n",
	   _gen_inst_id(), cid, sid);
}

void BasicSDT::_gen_fresh_var (varmap_info *v)
{
  static int vid = 0;
  char buf[32];
  
  snprintf (buf, 32, "fvar%d", vid++);
  
  v->id = new ActId (buf);
  fprintf (output_stream, "   syn::sdtvar<%d> %s;\n", v->width, buf);
  fprintf (output_stream, "   syn::var_init<%d,false> var_%s(%s);\n", v->width, buf, buf);
}


/* 
   returns new eid for the safe bool
*/
int BasicSDT::_gen_safe_bool (int eid)
{
  varmap_info xv;

  xv.width = 1;
  xv.fischan = 0;
  _gen_fresh_var (&xv);

  /*
    Sequence:
      1. transfer expression to variable
      2. read variable into expression
  */
  int tid = _gen_stmt_id ();
  _emit_transfer (tid, eid, &xv);

  int fid = _gen_stmt_id ();
  _emit_trueseq (fid, tid);

  eid = _gen_expr_id ();
  _emit_var_read (eid, &xv);

  int eid2 = _gen_expr_id ();
  fprintf (output_stream, "   syn::expr::null e%d;\n", eid2);

  fprintf (output_stream, "   e%d.out.r = c%d.r; c%d.a = e%d.out.r; e%d.out.d=e%d.out.d;\n",
	   eid2, fid, fid, eid, eid2, eid);
  
  return eid2;
}
