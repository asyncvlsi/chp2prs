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
