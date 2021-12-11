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
#include <act/act.h>
#include <act/iter.h>
#include <string.h>
#include "basicsdt.h"

/*
 *
 *  Core syntax-directed translation code written by Rajit Manohar
 *
 *  Extensions to full expressions, optimizations, and direct use of
 *  the ACT library  by Zeb Mehring
 *
 */

int BasicSDT::_gen_stmt_id ()
{
  int tmp = SDTEngine::_gen_stmt_id ();
  fprintf (output_stream, "   a1of1 c%d;\n", tmp);
  return tmp;
}

void BasicSDT::_emit_skip (int id)
{
  int inst = _gen_inst_id ();
  fprintf (output_stream, "   syn::sskip s_%d(c%d);\n", inst, id);
}

static const char *sdt_expr_name (int type)
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
  FILE *o = (_efp == NULL ? output_stream : _efp);
  
  fprintf (o, "   syn::expr::%s<%d,%d> e%d (e%d.out,e%d.out);\n",
	   sdt_expr_name (type), lw, rw, id, lid, rid);
}

void BasicSDT::_emit_expr_unary (int id, int width,
				 int type, int lid, int lw)
{
  FILE *o = (_efp == NULL ? output_stream : _efp);
  
  fprintf (o, "   syn::expr::%s<%d> e%d (e%d.out);\n",
	   sdt_expr_name (type), lw, id, lid);
}

void BasicSDT::_emit_expr_bitfield (int eid, int lsb, int msb, int lid, int lw)
{
  FILE *o = (_efp == NULL ? output_stream : _efp);
  
  fprintf (o, "   syn::expr::bitfield<%d,%d,%d> e%d (e%d.out);\n",
	   lw, lsb, msb, eid, lid);
}
  
void BasicSDT::_emit_expr_concat2 (int eid, int width,
				   int lid, int lw, int rid, int rw)
{
  FILE *o = (_efp == NULL ? output_stream : _efp);
  
  fprintf (o, "   syn::expr::concat2<%d,%d> e%d (e%d.out,e%d.out);\n",
	   lw, rw, eid, lid, rid);
}

void BasicSDT::_emit_expr_const (int id, int width, int val)
{
  fprintf (output_stream, "   syn::expr::const<%d,%d> e%d;\n", width, val, id);
}

void BasicSDT::_emit_expr_width_conv (int from, int from_w,
				      int to, int to_w)
{
  FILE *o = (_efp == NULL ? output_stream : _efp);
  
  fprintf (o, "   syn::expr::widthconv<%d,%d> e%d(e%d.out);\n",
	   from_w, to_w, to, from);
}

void BasicSDT::_emit_var_read (int eid, ActId *id)
{
  varmap_info *v = _var_getinfo (id);
  fprintf (output_stream, "  syn::expr::nullint<%d> e%d(var_",
	   v->width, eid);
  _emit_mangled_id (output_stream, v->id);
  fprintf (output_stream, ".out[%d]);\n", v->iread++);
}

void BasicSDT::_emit_transfer (int cid, int eid, ActId *id)
{
  varmap_info *ch = _var_getinfo (id);
  fprintf (output_stream, "   syn::transfer<%d> s_%d(c%d, e%d.out,",
	   ch->width, _gen_inst_id(), cid, eid);

  if (ch->fischan) {
    /* pick the channel mux */
    _emit_mangled_id (output_stream, ch->id);
    fprintf (output_stream, "_mux%c.m[%d]",
	     _get_isinport (ch) ? 'i' : 'o',
	     _get_isinport (ch) ? ch->iread++ : ch->iwrite++);
  }
  else {
    fprintf (output_stream, "var_");
    _emit_mangled_id (output_stream, ch->id);
    fprintf (output_stream, ".in[%d]", ch->iwrite++);
  }
  fprintf (output_stream, ");\n");
}

void BasicSDT::_emit_recv (int cid, ActId *chid, ActId *id)
{
  varmap_info *v;
  if (!id) {
    Assert (_gen_fresh_var_writeonly (bitWidth (chid), &id), "Could not allocate fresh variable");
  }
  v = _var_getinfo (id);
  varmap_info *ch = _var_getinfo (chid);
  int c;
  if (ch->nread > 1) {
    list_t *tmp = list_new ();
    list_iappend (tmp, _gen_stmt_id());
    list_iappend (tmp, _gen_stmt_id());
    _emit_comma (cid, tmp);
    cid = list_ivalue (list_first (tmp));
    c = list_ivalue (list_next (list_first (tmp)));
    list_free (tmp);
  }
  
  fprintf (output_stream, "   syn::recvport<%d> s_%d(c%d,", ch->width,
	   _gen_inst_id(), cid);
  Assert (_get_isinport (ch), "What?");
  _emit_mangled_id (output_stream, ch->id);
  fprintf (output_stream, "_muxi.m[%d]", ch->iread++);
  fprintf (output_stream, ",");
  if (v) {
    fprintf (output_stream, "var_");
    _emit_mangled_id (output_stream, v->id);
    fprintf (output_stream, ".in[%d]", v->iwrite++);
  }
  fprintf (output_stream, ");\n");
  if (ch->nread > 1) {
    fprintf (output_stream, "   ");
    _emit_mangled_id (output_stream, ch->id);
    fprintf (output_stream, "_muxi.ctrl[%d]=c%d;\n", ch->iread-1, c);
  }
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

void BasicSDT::_emit_semiopt (int cid, list_t *stmts)
{
  listitem_t *li;
  fprintf (output_stream, "   syn::semiopt<%d> s_%d(c%d,{",
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
  if (v->nread > 0) {
    if (v->fisbool) {
      fprintf (output_stream, "   syn::mux_bool_inport<%d> ", v->nread);
    }
    else {
      fprintf (output_stream, "   syn::muxinport<%d,%d> ", v->width, v->nread);
    }
    _emit_mangled_id (output_stream, v->id);
    fprintf (output_stream, "_muxi(");
    v->id->Print (output_stream);
    fprintf (output_stream, ");\n");
  }
  if (v->nwrite > 0) {
    if (v->fisbool) {
      fprintf (output_stream, "   syn::mux_bool_outport<%d> ", v->nwrite);
    }
    else {
      fprintf (output_stream, "   syn::muxoutport<%d,%d> ", v->width, v->nwrite);
    }
    _emit_mangled_id (output_stream, v->id);
    fprintf (output_stream, "_muxo(");
    v->id->Print (output_stream);
    fprintf (output_stream, ");\n");
  }
}

void BasicSDT::_emit_variable_mux (varmap_info *v)
{
  char tmpbuf[4096];
  /* if you need a mux for accessing variables, add it here */
  if (!v->fisbool) {
    // zero length arrays are not allowed, this is for simulation only, writing but not reading would not make sense in a real chip
    if (v->nread == 0) {
      fprintf (output_stream, "   syn::var_int_in_ports<%d,%d> var_", v->width, v->nwrite);
      v->id->sPrint (tmpbuf, 4096);
      warning("Process `%s': variable `%s' is written but never read; hope you know what you're doing!", P ? P->getName() : "-toplevel-", tmpbuf);
    }
    else if (v->nwrite == 0) {
      fprintf (output_stream, "   syn::var_int_out_ports<%d,%d> var_", v->width, v->nread);
      v->id->sPrint (tmpbuf, 4096);
      warning("Process `%s': variable `%s' is read but never written; hope you know what you're doing!", P ? P->getName() : "-toplevel-", tmpbuf);
    }
    else {
      fprintf (output_stream, "   syn::var_int_ports<%d,%d,%d> var_",
	       v->width, v->nwrite, v->nread);
    }
  }
  else {
    // zero length arrays are not allowed, this is for simulation only, writing but not reading would not make sense in a real chip
    if (v->nread == 0){
      fprintf(output_stream, "   syn::var_bool_in_ports<%d> var_", v->nwrite);
      v->id->sPrint (tmpbuf, 4096);
      warning("Process `%s': variable `%s' is written but never read; hope you know what you're doing!", P ? P->getName() : "-toplevel-", tmpbuf);
    }
    else if (v->nwrite == 0) {
      fprintf(output_stream, "   syn::var_bool_out_ports<%d> var_", v->nread);
      v->id->sPrint (tmpbuf, 4096);
      warning("Process `%s': variable `%s' is read but written read; hope you know what you're doing!", P ? P->getName() : "-toplevel-", tmpbuf);
    }
    else {
      fprintf (output_stream, "   syn::var_bool_ports<%d,%d> var_",
	       v->nwrite, v->nread);
    }
  }
  _emit_mangled_id (output_stream, v->id);
  fprintf (output_stream, "(");
  v->id->Print (output_stream);
  fprintf (output_stream, ");\n");
}


void BasicSDT::_emit_trueseq (int cid, int sid)
{
  fprintf (output_stream, "   syn::fullseq s_%d(c%d,c%d);\n",
	   _gen_inst_id(), cid, sid);
}

int BasicSDT::_gen_fresh_var (int width, ActId **id)
{
  static int vid = 0;
  varmap_info *v;
  char buf[32];

  do {
    snprintf (buf, 32, "fvar%d", vid++);
  } while (P->CurScope()->Lookup (buf));

  InstType *it = TypeFactory::Factory()->NewInt (P->CurScope(), Type::NONE,
					    0, const_expr ((long)width));
  it = it->Expand (NULL, NULL);

  Assert (P->CurScope()->Add (buf, it), "What?");

  *id = new ActId (buf);

  v = _var_getinfo (*id);
  v->iread = 0;
  v->iwrite = 0;
  v->nread = 1;
  v->nwrite = 1;
  fprintf (output_stream, "   syn::sdtvar<%d> %s;\n", v->width, buf);
  fprintf (output_stream, "   syn::var_int_ports<%d,1,1> var_%s(%s);\n", v->width, buf, buf);
  return 1;
}

int BasicSDT::_gen_fresh_var_writeonly (int width, ActId **id)
{
  static int vid = 0;
  varmap_info *v;
  char buf[32];

  do {
    snprintf (buf, 32, "fvar%d", vid++);
  } while (P->CurScope()->Lookup (buf));

  InstType *it = TypeFactory::Factory()->NewInt (P->CurScope(), Type::NONE,
					    0, const_expr ((long)width));
  it = it->Expand (NULL, NULL);

  Assert (P->CurScope()->Add (buf, it), "What?");

  *id = new ActId (buf);

  v = _var_getinfo (*id);
  v->iread = 0;
  v->iwrite = 0;
  v->nread = 0;
  v->nwrite = 1;
  fprintf (output_stream, "   syn::sdtvar<%d> %s;\n", v->width, buf);
  fprintf (output_stream, "   syn::var_int_in_ports<%d,1> var_%s(%s);\n", v->width, buf, buf);
  return 1;
}


/* 
   returns new eid for the safe bool
*/
int BasicSDT::_gen_safe_bool (int eid)
{
  ActId *id;

  Assert (_gen_fresh_var (1, &id), "What?");

  /*
    Sequence:
      1. transfer expression to variable
      2. read variable into expression
  */
  int tid = _gen_stmt_id ();
  _emit_transfer (tid, eid, id);

  int fid = _gen_stmt_id ();
  _emit_trueseq (fid, tid);

  eid = _gen_expr_id ();
  _emit_var_read (eid, id);

  int eid2 = _gen_expr_id ();
  fprintf (output_stream, "   syn::expr::null e%d;\n", eid2);

  fprintf (output_stream, "   e%d.out.r = c%d.r; c%d.a = e%d.out.r; e%d.out.d=e%d.out.d;\n",
	   eid2, fid, fid, eid, eid2, eid);
  
  return eid2;
}



void BasicSDT::_emit_expr_block (int id, int blkid, list_t *exprs)
{
  listitem_t *li;
  fprintf (output_stream, "   syn::expr::blk%d e%d(", blkid, id);
  for (li = list_first (exprs); li; li = list_next (li)) {
    if (li != list_first (exprs)) {
      fprintf (output_stream, ", ");
    }
    fprintf (output_stream, "e%d.out", list_ivalue (li));
  }
  fprintf (output_stream, ");\n");
}


BasicSDT::BasicSDT (int isbundled, int isopt, FILE *fpout, const char *ef)
   : SDTEngine(ef)
{
  bundled_data = isbundled;
  optimize = isopt;
  _varmap = NULL;
  
  _shared_expr_var = 0;

  _expr_id = 0;
  _stmt_id = 0;
  _inst_id = 0;
  
  output_stream = fpout;
}



/* Recursively called fn to handle different chp statement types */

/* Print proc definition & override CHP variables */
bool BasicSDT::write_process_definition(FILE *fp, Process * p)
{
  bool has_overrides = 0;

  fprintf(fp, "defproc sdt_");
  ActNamespace::Act()->mfprintfproc (fp, p);
  fprintf (fp, " <: ");
  p->printActName (fp);
  fprintf (fp, " ()\n");

  int bw = 0;

#define OVERRIDE_OPEN				\
  do {						\
    if (!has_overrides) {			\
      fprintf(fp, "+{\n");			\
      has_overrides = true;			\
    }						\
  } while (0)
    
  
  /* iterate through Scope Hashtable to find all chp variables */
  ActInstiter iter(p->CurScope());
  for (iter = iter.begin(); iter != iter.end(); iter++) {
    ValueIdx *vx = *iter;
    /* chan variable found */
    if (TypeFactory::isChanType (vx->t)) {
      bw = TypeFactory::bitWidth(vx->t);
      OVERRIDE_OPEN;
      if (TypeFactory::isBoolType (TypeFactory::getChanDataType (vx->t))) {
         fprintf(fp, "  syn::sdtboolchan %s;\n", vx->getName());
      }
      else {
         fprintf(fp, "  syn::sdtchan<%d> %s;\n", bw, vx->getName());
      }
    }
    else if (TypeFactory::isIntType (vx->t)) {
      /* chp-optimize creates sel0, sel1,... & loop0, loop1, ... which do not have dualrail overrides */
      bw = TypeFactory::bitWidth(vx->t);
      OVERRIDE_OPEN;
      fprintf(fp, "  syn::sdtvar<%d> %s;\n", bw, vx->getName());
    }
    else if (TypeFactory::isBoolType (vx->t)) {
      OVERRIDE_OPEN;
      fprintf (fp, " syn::sdtboolvar %s;\n", vx->getName());
    }
    else if (TypeFactory::isProcessType (vx->t)) {
      OVERRIDE_OPEN;
      fprintf (fp, " sdt_");
      Process *proc = dynamic_cast <Process *> (vx->t->BaseType());
      Assert (proc, "Why am I here?");
      ActNamespace::Act()->mfprintfproc (fp, proc);
      fprintf (fp, " %s;\n", vx->getName());
    }
    else if (TypeFactory::isStructure (vx->t)) {
      OVERRIDE_OPEN;
      fprintf (fp, " sdt_");
      Data *d = dynamic_cast <Data *> (vx->t->BaseType());
      Assert (d, "Why am I here?");
      ActNamespace::Act()->mfprintfproc (fp, d);
      fprintf (fp, " %s;\n", vx->getName());
    }
  }
  /* end param declaration */
  if (has_overrides) {
    fprintf(fp, "}\n{\n");
  }
  else {
    fprintf(fp, "{\n");
  }

  if (p->getlang() && p->getlang()->getchp()) {
    fprintf (fp, " refine {\n");
  }
  
  return has_overrides;
}

/* Initialize var_init_false vars for each CHP int */
void BasicSDT::initialize_chp_ints(FILE *fp, Process * p, bool has_overrides)
{
  int bw = 0;

  /* iterate through Scope Hashtable to find all chp ints */
  fprintf(fp, "  /* Initialize chp vars */\n");

  ActInstiter iter(p->CurScope());
  for (iter = iter.begin(); iter != iter.end(); iter++) {
    ValueIdx *vx = *iter;
    
    /* int variable found */
    if (TypeFactory::isIntType (vx->t)) {
      ActId *tmpid = new ActId (vx->getName());
      bw = bitWidth (tmpid);
      delete tmpid;
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

void BasicSDT::_emit_begin ()
{
  ihash_iter_t iter;
  ihash_bucket_t *b;
  struct act_chp *chp;
  
  /* Write process definition and variable declarations */
  int overrides = write_process_definition(output_stream, P);

  if (!P->getlang() || !P->getlang()->getchp()) {
    return;
  }

  chp = P->getlang()->getchp();

  if (!chp->c) {
    return;
  }

  if (_varmap) {
    ihash_iter_init (_varmap, &iter);
    while ((b = ihash_iter_next (_varmap, &iter))) {
      varmap_info *v = (varmap_info *)b->v;
      FREE (v);
    }
    ihash_free (_varmap);
    _varmap = NULL;
  }

  _varmap = ihash_new (4);
  _construct_varmap (chp->c);

  /*-- emit all the variable ports and channel muxes --*/
  ihash_iter_init (_varmap, &iter);
  while ((b = ihash_iter_next (_varmap, &iter))) {
    varmap_info *v = (varmap_info *) b->v;
    if (v->fischan) {
      _emit_channel_mux (v);
#if 0
      if (v->nread > 0 && v->nwrite > 0) {
	char buf[10240];
	v->id->sPrint (buf, 10240);
	fatal_error  ("Channel `%s': send and receive on the same channel within a process not supported", buf);
      }
#endif      
    }
    else {
      _emit_variable_mux (v);
#if 0      
      if (v->nread == 0 || v->nwrite == 0) {
	char buf[10240];
	v->id->sPrint (buf, 10240);
	warning ("Variable `%s': only read or only written?", buf);
      }
#endif      
    }
  }
}


void BasicSDT::_emit_end (int id)
{
  /* connect toplevel "go" signal and print wrapper process instantiation */

  if (id >= 0) {
    fprintf (output_stream, "/*--- connect reset to go signal ---*/\n");

    fprintf (output_stream, "   syn::sinit s%d (c%d);\n", _gen_stmt_id(), id);

    /* matches refine block start */
    fprintf (output_stream, " }\n");
  }
  fprintf (output_stream, "}\n\n");
  fclose (_efp);
  _efp = NULL;


  if (_varmap) {
    ihash_iter_t iter;
    ihash_bucket_t *b;
    ihash_iter_init (_varmap, &iter);
    while ((b = ihash_iter_next (_varmap, &iter))) {
      varmap_info *v = (varmap_info *)b->v;
      FREE (v);
    }
    ihash_free (_varmap);
    _varmap = NULL;
  }

  
}
  




void BasicSDT::_construct_varmap_expr (Expr *e)
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

void BasicSDT::_clear_var_flags ()
{
  ihash_iter_t iter;
  ihash_bucket_t *b;
  ihash_iter_init (_varmap, &iter);

  while ((b = ihash_iter_next (_varmap, &iter))) {
    varmap_info *v = (varmap_info *)b->v;
    v->fcurexpr = 0;
  }
}


void BasicSDT::_construct_varmap (act_chp_lang_t *c)
{
  varmap_info *v;
  int x;
  int pblock = 0;
  int changed = 0;
  if (!c) return;

  pblock = _block_id;

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
    if (v->fisinport == 2) {
      if (v->block_out != -1 && v->block_out != pblock) {
	warning ("Channel has multiple potentially concurrent senders?");
	fprintf (stderr, "\t Channel: ");
	v->id->Print (stderr);
	fprintf (stderr, "\n");
      }
      v->block_out = pblock;
    }
    v->nwrite++;
    _clear_var_flags ();
    if (c->u.comm.e) {
      _construct_varmap_expr (c->u.comm.e);
    }
    break;
  case ACT_CHP_RECV:
    v = _var_getinfo (c->u.comm.chan);
    if (v->fisinport == 2) {
      if (v->block_in != -1 && v->block_in != pblock) {
	warning ("Channel has multiple potentially concurrent receivers?");
	fprintf (stderr, "\t Channel: ");
	v->id->Print (stderr);
	fprintf (stderr, "\n");
      }
      v->block_in = pblock;
    }
    v->nread++;
    if (c->u.comm.var) {
      v = _var_getinfo (c->u.comm.var);
      v->nwrite++;
    }
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
    for (listitem_t *li = list_first (c->u.semi_comma.cmd); li; li = list_next (li)) {
      _construct_varmap ((act_chp_lang_t *) list_value (li));
      if (changed) {
	_block_id++;
      }
    }
    break;
  case ACT_CHP_LOOP:
  case ACT_CHP_SELECT:
  case ACT_CHP_SELECT_NONDET:
  case ACT_CHP_DOLOOP:
    if (pblock == -1) {
      _block_id = -2;
    }
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
  

  _block_id = pblock;
  
  return;
}


varmap_info *BasicSDT::_var_getinfo (ActId *id)
{
  act_connection *c;
  ihash_bucket_t *b;
  varmap_info *v;
  InstType *it;

  if (id->isDynamicDeref()) {
    fprintf (stderr, "id: ");
    id->Print (stderr);
    fprintf (stderr, "\n");
    fatal_error ("Dynamic de-references need explicit memory decomposition");
  }
  
  c = id->Canonical (P->CurScope());
  Assert (c, "What?");
  
  b = ihash_lookup (_varmap, (long)c);
  if (!b) {
    InstType *it2;
    b = ihash_add (_varmap, (long)c);
    NEW (v, varmap_info);

    v->nread = 0;
    v->nwrite = 0;
    v->iread = 0;
    v->iwrite = 0;
    v->id = id;
    it = P->CurScope()->FullLookup (id, NULL);
    if (id->Rest()) {
      it2 = P->CurScope()->Lookup (id->getName());
    }
    else {
      it2 = NULL;
    }
    v->width = bitWidth (id);
    v->fisbool = 0;
    if (TypeFactory::isChanType (it)) {
      v->fischan = 1;
      if (TypeFactory::isBoolType (TypeFactory::getChanDataType (it))) {
	v->fisbool = 1;
      }
      if (it->getDir() == Type::direction::IN &&
	  (!it2 || !TypeFactory::isProcessType (it2))) {
	v->fisinport = 1;
      }
      else if (it->getDir() == Type::direction::OUT &&
	       (!it2 || !TypeFactory::isProcessType (it2))) {
	v->fisinport = 0;
      }
      else {
	v->fisinport = 2;
	v->block_in = -1;
	v->block_out = -1;
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


int BasicSDT::_get_isinport (varmap_info *v)
{
  Assert (v->fischan, "_get_isinport() called for non-channel variable");
  if (v->fisinport == 0) {
    return 0;
  }
  else if (v->fisinport == 1) {
    return 1;
  }
  else if (v->fisinport == 2 && (v->nread > 0 && v->nwrite == 0)) {
    return 1;
  }
  else if (v->fisinport == 2 && (v->nread == 0)) {
    return 0;
  }
  else {
    if (v->block_in < 0 || v->block_out < 0) {
      fprintf (stderr, "Channel: ");
      v->id->Print (stderr);
      fprintf (stderr, "\n");
      warning ("Channel has a missing %s port",
	       (v->block_in < 0 ? "input" : "output"));
    }
    if (_block_id == v->block_in) {
      return 1;
    }
    else if (_block_id == v->block_out) {
      return 0;
    }
    else {
      printf ("in=%d, out=%d, cur=%d\n", v->block_in, v->block_out, _block_id);
      fprintf (stderr, "Channel: ");
      v->id->Print (stderr);
      fprintf (stderr, "\n");
      fatal_error ("Shared channels are not supported");
    }
    return 0;
  }
}


void BasicSDT::_emit_mangled_id (FILE *fp, ActId *id)
{
  char buf[4096];

  id->sPrint (buf, 4096);
  ActNamespace::Act()->mfprintf (fp, "%s", buf);
}
