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
#include "basicsdt.h"

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

void BasicSDT::_emit_var_read (int eid, varmap_info *v)
{
  fprintf (output_stream, "   syn::expr::nullint<%d> e%d(var_",
	   v->width, eid);
  v->id->Print (output_stream);
  fprintf (output_stream, ".out[%d]);\n", v->iread++);
}

void BasicSDT::_emit_transfer (int cid, int eid, varmap_info *ch)
{
  fprintf (output_stream, "   syn::transfer<%d> s_%d(c%d, e%d.out,",
	   ch->width, _gen_inst_id(), cid, eid);

  if (ch->fischan) {
    /* pick the channel mux */
    ch->id->Print (output_stream);
    fprintf (output_stream, "_mux%c.m[%d]",
	     _get_isinport (ch) ? 'i' : 'o',
	     _get_isinport (ch) ? ch->iread++ : ch->iwrite++);
  }
  else {
    fprintf (output_stream, "var_");
    ch->id->Print (output_stream);
    fprintf (output_stream, ".in[%d]", ch->iwrite++);
  }
  fprintf (output_stream, ");\n");
}

void BasicSDT::_emit_recv (int cid, varmap_info *ch, varmap_info *v)
{
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
  ch->id->Print (output_stream);
  fprintf (output_stream, "_muxi.m[%d]", ch->iread++);
  fprintf (output_stream, ",");
  if (v) {
    fprintf (output_stream, "var_");
    v->id->Print (output_stream);
    fprintf (output_stream, ".in[%d]", v->iwrite++);
  }
  fprintf (output_stream, ");\n");
  if (ch->nread > 1) {
    fprintf (output_stream, "   ");
    ch->id->Print (output_stream);
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
    fprintf (output_stream, "   syn::muxinport<%d,%d> ", v->width, v->nread);
    v->id->Print (output_stream);
    fprintf (output_stream, "_muxi(");
    v->id->Print (output_stream);
    fprintf (output_stream, ");\n");
  }
  if (v->nwrite > 0) {
    fprintf (output_stream, "   syn::muxoutport<%d,%d> ", v->width, v->nwrite);
    v->id->Print (output_stream);
    fprintf (output_stream, "_muxo(");
    v->id->Print (output_stream);
    fprintf (output_stream, ");\n");
  }
}

void BasicSDT::_emit_variable_mux (varmap_info *v)
{
  /* if you need a mux for accessing variables, add it here */
  if (!v->fisbool) {
    // zero length arrays are not allowed, this is for simulation only, writing but not reading would not make sense in a real chip
    if (v->nread == 0) {
      fprintf (output_stream, "   syn::var_int_in_ports<%d,%d> var_", v->width, v->nwrite);
      warning("you are generating a variable that is written but never read, in case this is a circuit for tapeout reexamine your design");
    }
    else fprintf (output_stream, "   syn::var_int_ports<%d,%d,%d> var_",
	     v->width, v->nwrite, v->nread);
  }
  else {
    // zero length arrays are not allowed, this is for simulation only, writing but not reading would not make sense in a real chip
    if (v->nread == 0){
      fprintf(output_stream, "   syn::var_bool_in_ports<%d> var_", v->nwrite);
      warning("you are generating a variable that is written but never read, in case this is a circuit for tapeout reexamine your design");
    } 
    else fprintf (output_stream, "   syn::var_bool_ports<%d,%d> var_",
	     v->nwrite, v->nread);
  }    
  v->id->Print (output_stream);
  fprintf (output_stream, "(");
  v->id->Print (output_stream);
  fprintf (output_stream, ");\n");
}


void BasicSDT::_emit_trueseq (int cid, int sid)
{
  fprintf (output_stream, "   syn::fullseq s_%d(c%d,c%d);\n",
	   _gen_inst_id(), cid, sid);
}

int BasicSDT::_gen_fresh_var (varmap_info *v)
{
  static int vid = 0;
  char buf[32];
  
  snprintf (buf, 32, "fvar%d", vid++);
  
  v->id = new ActId (buf);
  v->iread = 0;
  v->iwrite = 0;
  v->nread = 1;
  v->nwrite = 1;
  fprintf (output_stream, "   syn::sdtvar<%d> %s;\n", v->width, buf);
  fprintf (output_stream, "   syn::var_int_ports<%d,1,1> var_%s(%s);\n", v->width, buf, buf);
  return 1;
}


/* 
   returns new eid for the safe bool
*/
int BasicSDT::_gen_safe_bool (int eid)
{
  varmap_info xv;

  xv.id = NULL;
  xv.width = 1;
  xv.fischan = 0;
  Assert (_gen_fresh_var (&xv), "What?");

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
