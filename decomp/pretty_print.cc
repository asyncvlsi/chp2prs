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

#include "pretty_print.h"

static const int indent_inc = 1;
char ib_inc[indent_inc+1];

void chp_pretty_print (FILE *fp, act_chp_lang_t *c)
{
  memset(ib_inc, ' ', indent_inc);
  ib_inc[indent_inc] = '\0';
  _chp_pretty_print (fp, c, 0, 1);
}

static void _chp_pretty_print (FILE *fp, act_chp_lang_t *c, int prec = 0, int indent = 1)
{
  int lprec;
  char *ib;
  
  if (!c) return;
  if (indent < 0) {
    indent = 0;
  }
  MALLOC (ib, char, indent+1);
  memset(ib, ' ', indent);
  ib[indent] = '\0';

  if (c->label) {
    fprintf (fp, "%s:", c->label);
  }
  
  switch (c->type) {
  case ACT_CHP_COMMALOOP:
  case ACT_CHP_SEMILOOP:
    fprintf (fp, "(");
    if (c->type == ACT_CHP_COMMALOOP) {
      fprintf (fp, ",");
    }
    else {
      fprintf (fp, ";");
    }
    fprintf (fp, "%s", c->u.loop.id);
    fprintf (fp, ":");
    print_expr (fp, c->u.loop.lo);
    if (c->u.loop.hi) {
      fprintf (fp, "..");
      print_expr (fp, c->u.loop.hi);
    }
    fprintf (fp, ":");
    _chp_pretty_print (fp, c->u.loop.body);
    fprintf (fp, ")");
    break;
    
  case ACT_CHP_COMMA:
  case ACT_CHP_SEMI:
    {
      listitem_t *li;

      lprec = (c->type == ACT_CHP_SEMI ? 0 : 1);

      if (prec > lprec) {
	fprintf (fp, "(");
      }

      if (c->type == ACT_CHP_SEMI && (list_length(c->u.semi_comma.cmd)>1)) {
            fprintf (fp, "\n%s", ib);
      }

      for (li = list_first (c->u.semi_comma.cmd); li; li = list_next (li)) {
	_chp_pretty_print (fp, (act_chp_lang_t *)list_value (li), lprec, indent+indent_inc);
	if (list_next (li)) {
	  if (c->type == ACT_CHP_COMMA) {
	    fprintf (fp, ",");
	  }
	  else {
	    // fprintf (fp, ";\n%s",ib);
	    fprintf (fp, ";");
      if (c->type == ACT_CHP_SEMI) {
            fprintf (fp, "\n%s", ib);
      }
	  }
	}
      }

      if (prec > lprec) {
	fprintf (fp, ")");
      }

    }
    break;

  case ACT_CHP_LOOP:
  case ACT_CHP_DOLOOP:
    fprintf (fp, "\n%s*", ib_inc);
  case ACT_CHP_SELECT:
  case ACT_CHP_SELECT_NONDET:
  if (c->type == ACT_CHP_SELECT || c->type == ACT_CHP_SELECT_NONDET) {
    fprintf (fp, "%s",ib_inc);
  }
    fprintf (fp, "[");
    if (c->type == ACT_CHP_SELECT_NONDET) {
      fprintf (fp, "|");
    }
    {
      int n_br=0;
      act_chp_gc_t *gc = c->u.gc;

      if (c->type == ACT_CHP_DOLOOP) {
	fprintf (fp, " ");
	_chp_pretty_print (fp, gc->s, 0, indent+indent_inc);
	fprintf (fp, " <- ");
	if (gc->g) {
	  print_uexpr (fp, gc->g);
	}
	else {
	  fprintf (fp, "true");
	}
      }
      else {
	while (gc) {
    n_br++;
	  if (!gc->g) {
	    if (c->type == ACT_CHP_LOOP) {
	      fprintf (fp, "true");
	    }
	    else {
	      fprintf (fp, "else");
	    }
	  }
	  else {
	    print_uexpr (fp, gc->g);
	  }
	  if (gc->s) {
	    fprintf (fp, " -> ");
	    _chp_pretty_print (fp, gc->s, 0, indent+indent_inc);
	  }
	  if (gc->next) {
	    fprintf (fp, "\n%s[]", ib);
	  }
	  gc = gc->next;
	}
      }
      if (n_br>1) fprintf (fp, "\n");
    }
    if (indent>0) {
        fprintf (fp, "%s",ib);
    }
    if (c->type == ACT_CHP_SELECT_NONDET) {
      fprintf (fp, "|");
    }
    if (c->type == ACT_CHP_DOLOOP) {
      fprintf (fp, "]\n");
    }
    else {
        fprintf (fp, "]");
    }
    break;
    
  case ACT_CHP_SKIP:
    fprintf (fp, "skip");
    break;

  case ACT_CHP_ASSIGN:
  case ACT_CHP_ASSIGNSELF:
    c->u.assign.id->Print (fp);
    fprintf (fp, ":=");
    print_uexpr (fp, c->u.assign.e);
    break;
    
  case ACT_CHP_SEND:
    c->u.comm.chan->Print (fp);
    fprintf (fp, "!");
    if (c->u.comm.flavor == 1) {
      fprintf (fp, "+");
    }
    else if (c->u.comm.flavor == 2) {
      fprintf (fp, "-");
    }
    {
      if (c->u.comm.e) {
	print_uexpr (fp, c->u.comm.e);
      }
      if (c->u.comm.var) {
	fprintf (fp, "?");
	if (c->u.comm.flavor == 1) {
	  fprintf (fp, "+");
	}
	else if (c->u.comm.flavor == 2) {
	  fprintf (fp, "-");
	}
	if (c->u.comm.convert == 1) {
	  fprintf (fp, "bool(");
	}
	else if (c->u.comm.convert == 2) {
	  fprintf (fp, "int(");
	}
	c->u.comm.var->Print (fp);
	if (c->u.comm.convert != 0) {
	  fprintf (fp, ")");
	}
      }
    }
    break;
    
  case ACT_CHP_RECV:
    c->u.comm.chan->Print (fp);
    fprintf (fp, "?");
    if (c->u.comm.flavor == 1) {
      fprintf (fp, "+");
    }
    else if (c->u.comm.flavor == 2) {
      fprintf (fp, "-");
    }
    {
      if (c->u.comm.var) {
	if (c->u.comm.convert == 1) {
	  fprintf (fp, "bool(");
	}
	else if (c->u.comm.convert == 2) {
	  fprintf (fp, "int(");
	}
	c->u.comm.var->Print (fp);
	if (c->u.comm.convert != 0) {
	  fprintf (fp, ")");
	}
      }
      if (c->u.comm.e) {
	fprintf (fp, "!");
	if (c->u.comm.flavor == 1) {
	  fprintf (fp, "+");
	}
	else if (c->u.comm.flavor == 2) {
	  fprintf (fp, "-");
	}
	print_uexpr (fp, c->u.comm.e);
      }
    }
    break;

  case ACT_CHP_FUNC:
    fprintf (fp, "%s(", string_char (c->u.func.name));
    for (listitem_t *li = list_first (c->u.func.rhs); li; li = list_next (li)) {
      act_func_arguments_t *a = (act_func_arguments_t *) list_value (li);
      if (li != list_first (c->u.func.rhs)) {
	fprintf (fp, ",");
      }
      if (a->isstring) {
	fprintf (fp, "\"%s\"", string_char (a->u.s));
      }
      else {
	print_uexpr (fp, a->u.e);
      }
    }
    fprintf (fp, ")");
    break;

  case ACT_CHP_HOLE: /* to support verification */
    fprintf (fp, "_");
    break;
    
  case ACT_CHP_MACRO:
    c->u.macro.id->Print (fp);
    fprintf (fp, ".%s(", string_char (c->u.macro.name));
    if (c->u.macro.rhs) {
      listitem_t *li;
      for (li = list_first (c->u.macro.rhs); li; li = list_next (li)) {
	print_expr (fp, (Expr *)list_value (li));
	if (list_next (li)) {
	  fprintf (fp, ",");
	}
      }
    }
    fprintf (fp, ")");
    break;

  case ACT_HSE_FRAGMENTS:
    do {
      _chp_pretty_print (fp, c->u.frag.body);
      if (c->u.frag.nextlabel) {
	fprintf (fp, " : %s", c->u.frag.nextlabel);
      }
      else {
	listitem_t *li;
	fprintf (fp, " : [ ");
	for (li = list_first (c->u.frag.exit_conds); li; li = list_next (li)) {
	  print_expr (fp, (Expr *) list_value (li));
	  fprintf (fp, " -> ");
	  li = list_next (li);
	  fprintf (fp, "%s", (char *) list_value (li));
	  if (list_next (li)) {
	    fprintf (fp, " [] ");
	  }
	}
	fprintf (fp, " ] ");
      }
      c = c->u.frag.next;
      if (c) {
	fprintf (fp, "||\n%s : ", c->label);
      }
    } while (c);
    break; 
    
  default:
    fatal_error ("Unknown type");
    break;
  }
  FREE (ib);
}

void _fill_in_else_explicit (act_chp_lang_t *c, Scope *s)
{
    listitem_t *li;
    act_chp_lang_t *stmt;
    act_chp_gc_t *gc;
    Expr *g, *disj_gs, *tmp, *itr;
    Expr *inv_disj_gs, *expr_false, *else_explicit;
    if (!c) return;

    switch (c->type) {
    case ACT_CHP_COMMALOOP:
    case ACT_CHP_SEMILOOP:
        fatal_error ("Replication loops should've been removed..");
        break;
        
    case ACT_CHP_COMMA:
    case ACT_CHP_SEMI:
        for (li = list_first (c->u.semi_comma.cmd); li; li = list_next (li)) {
            stmt = (act_chp_lang_t *)(list_value(li));
            _fill_in_else_explicit (stmt, s);
        }
        break;

    case ACT_CHP_LOOP:
    case ACT_CHP_DOLOOP:
        gc = c->u.gc;
        _fill_in_else_explicit (gc->s, s);
        break;
        
    case ACT_CHP_SELECT:
        NEW (disj_gs, Expr);
        disj_gs->type = E_OR;

        NEW (expr_false, Expr);
        expr_false->type = E_FALSE;

        gc = c->u.gc;
        disj_gs->u.e.r = expr_dup(gc->g);
        itr = disj_gs;

        for (gc = gc->next ; gc ; gc = gc->next) {
            if (gc->g) {
                itr->u.e.l = gc->g;
                NEW (tmp, Expr);
                tmp->type = E_OR;
                tmp->u.e.r = expr_dup(itr);
                NEW (itr, Expr);
                itr = tmp;
            }
            else {
                // else exists => complement and insert
                itr = itr->u.e.r;
                NEW (inv_disj_gs, Expr);
                inv_disj_gs->type = E_NOT;
                inv_disj_gs->u.e.l = itr;
                gc->g = expr_dup(inv_disj_gs);
            }
        }

        for (gc = c->u.gc ; gc ; gc = gc->next) {
            _fill_in_else_explicit (gc->s, s);
        }

        break;

    case ACT_CHP_SELECT_NONDET:
        for (gc = c->u.gc ; gc ; gc = gc->next){
            _fill_in_else_explicit (gc->s, s);
        }
        break;
        
    case ACT_CHP_SKIP:
    case ACT_CHP_ASSIGN:
    case ACT_CHP_ASSIGNSELF:
    case ACT_CHP_RECV:
    case ACT_CHP_SEND:
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
    return;
}

void _trim_nested_same_int (act_chp_lang_t *&c, Scope *s)
{
  if (!c) return;

  switch (c->type) {
  case ACT_CHP_COMMALOOP:
  case ACT_CHP_SEMILOOP:
    fatal_error ("Replication loops should've been removed..");
    break;
      
  case ACT_CHP_COMMA:
  case ACT_CHP_SEMI:
    for (listitem_t *li = list_first (c->u.semi_comma.cmd); li; li = list_next (li)) {
      act_chp_lang_t *stmt = (act_chp_lang_t *)(list_value(li));
      _trim_nested_same_int (stmt, s);
    }
    break;

  case ACT_CHP_LOOP:
  case ACT_CHP_DOLOOP: {
    act_chp_gc_t *gc = c->u.gc;
    _trim_nested_same_int (gc->g, s);
    _trim_nested_same_int (gc->s, s);
    break;
  }
      
  case ACT_CHP_SELECT:
  case ACT_CHP_SELECT_NONDET:
  for (act_chp_gc_t *gc = c->u.gc ; gc ; gc = gc->next){
    _trim_nested_same_int (gc->g, s);
    _trim_nested_same_int (gc->s, s);
  }
  break;
      
  case ACT_CHP_SKIP:
  case ACT_CHP_RECV:
    break;
  case ACT_CHP_ASSIGN:
  case ACT_CHP_ASSIGNSELF: {
      _trim_nested_same_int(c->u.assign.e, s);
    }
    break;
  case ACT_CHP_SEND: {
    if (c->u.comm.e)
      _trim_nested_same_int(c->u.comm.e, s);
    }
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
  return;
}

void _trim_nested_same_int (Expr *&e, Scope *s)
{
  if (!e) return;

#define BINARY_OP                         \
  do {							                      \
    _trim_nested_same_int (e->u.e.l, s);	\
    _trim_nested_same_int (e->u.e.r, s);	\
  } while (0)

#define UNARY_OP                          \
  do {							                      \
    _trim_nested_same_int (e->u.e.l, s);	\
  } while (0)
  
  switch (e->type) {
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
  case E_BUILTIN_BOOL:
  UNARY_OP;
  break;
  case E_BUILTIN_INT: 
  UNARY_OP;
  if (e->u.e.r) {
    /* int (x,w1) */
    int y;
    Assert (expr_is_a_const(e->u.e.r), "e what!");
    Assert (act_expr_getconst_int (e->u.e.r, &y), "int val y extract failure");
    if (e->u.e.l->type == E_BUILTIN_INT) {
      /* int(int(.),w1) */
      Expr *ne = e->u.e.l;
      if (ne && ne->u.e.r) {
	/* int(int(y,w2),w1) */
        int x;
        Assert (expr_is_a_const(ne->u.e.r), "ne what!");
        Assert (act_expr_getconst_int (ne->u.e.r, &x), "int val x extract failure");
        if (x==y) {
          e = ne;
        }
      }
    }
  }
  break;

  case E_QUERY:
    _trim_nested_same_int (e->u.e.l, s);
    _trim_nested_same_int (e->u.e.r->u.e.l, s);
    _trim_nested_same_int (e->u.e.r->u.e.r, s);
    break;

  case E_COLON:
  case E_COMMA:
    fatal_error ("Should have been handled elsewhere");
    break;

  case E_CONCAT: {
      Expr *tmp = e;
      while (tmp) {
        _trim_nested_same_int (tmp->u.e.l, s);
        tmp = tmp->u.e.r;
      }
    }
    break;

  case E_REAL:
    fatal_error ("No real expressions please.");
    break;

  case E_TRUE:
  case E_FALSE:
  case E_INT:
  case E_BITFIELD:
  case E_VAR:
  case E_PROBE:
  case E_FUNCTION:
  case E_USERMACRO:
    break;
    
  case E_SELF:
  default:
    fatal_error ("Unknown expression type %d\n", e->type);
    break;
  }
  return;
#undef BINARY_OP
#undef UNARY_OP

}

void _lift_probes (GraphWithChanNames &g) 
{
  _lift_probes (g, g.graph.m_seq);
}

void _lift_probes (GraphWithChanNames &g, Sequence seq) 
{
  Block *curr = seq.startseq->child();
  Block *tmp;

  while (curr->type() != BlockType::EndSequence) {
  switch (curr->type()) {
  case BlockType::Basic:
  break;
    
  case BlockType::Par: {
    for ( auto &branch : curr->u_par().branches )
      _lift_probes (g, branch);
  }
  break;
    
  case BlockType::Select: {
    bool process = false;
    int n_br{};
    for ( auto &branch : curr->u_select().branches ) {
      if (branch.g.type()==IRGuardType::Expression &&
       !isProbeFree(branch.g.u_e().e.m_dag)) {
        process = true; 
      }
      n_br++;
    }
    if (process) {
      Block *sel = g.graph.blockAllocator().newBlock(Block::makeSelectBlock());
      sel->u_select().is_nondet = curr->u_select().is_nondet;
      curr->u_select().is_nondet = false;
      int sv_bw = log_2_round_up(n_br);
      VarId sv = g.graph.id_pool().makeUniqueVar(sv_bw);
      unsigned long long br_ctr{};
      for ( auto &branch : curr->u_select().branches ) {
        Block *assn_sv = 
          g.graph.blockAllocator().newBlock(Block::makeBasicBlock(
          Statement::makeAssignment(sv, 
          ChpExprSingleRootDag::makeConstant(BigInt{br_ctr}, sv_bw))));
        sel->u_select().branches.emplace_back(
          g.graph.blockAllocator().newSequence({assn_sv}),
          IRGuard::deep_copy(branch.g));
        branch.g = IRGuard::makeExpression(
          ChpExprSingleRootDag::makeBinaryOp(
            IRBinaryOpType::EQ, 
            std::make_unique<ChpExprSingleRootDag>(
              ChpExprSingleRootDag::makeVariableAccess(sv, sv_bw)),
            std::make_unique<ChpExprSingleRootDag>(
              ChpExprSingleRootDag::makeConstant(BigInt{br_ctr}, sv_bw))
          ));
        br_ctr++;
      }
      Block *currp = curr->parent();
      Block::disconnect(currp, curr);
      Block::connect(currp, sel);
      Block::connect(sel, curr);
    }
    for ( auto &branch : curr->u_select().branches )
      _lift_probes (g, branch.seq);
  }
  break;
    
  case BlockType::DoLoop: {
    _lift_probes (g, curr->u_doloop().branch);
  }
  break;
  
  case BlockType::StartSequence:
  case BlockType::EndSequence:
    hassert(false);
    break;
  }
  curr = curr->child();
  }   
  return;
}

bool _has_probe_of_chan (const ChpExprDag &e, ChanId c)
{
  bool ret = false;

  ChpExprDag::iterNodes (e,
			 [&] (const ChpExprDag::Node &n) -> void {
			   if (n.type() == IRExprTypeKind::ChanProbe) {
			    if (n.u_probe().id == c) {
            ret = true;
          } 
			   }
			 });
			  
  return ret;
}

bool _has_probe_of_chan (const Sequence &seq, ChanId c)
{
  Block *curr = seq.startseq->child();
  while (curr->type() != BlockType::EndSequence) {
    switch (curr->type()) {
    case BlockType::Basic:
      switch (curr->u_basic().stmt.type()) {
      case StatementType::Assign:
        if (_has_probe_of_chan (curr->u_basic().stmt.u_assign().e, c)) return true;
      break;
      case StatementType::Send:
        if (_has_probe_of_chan (curr->u_basic().stmt.u_send().e.m_dag, c)) return true;
      break;
      case StatementType::Receive:
      break;
      }
    break;
      
    case BlockType::Par:
      for (auto &branch : curr->u_par().branches) {
        if (_has_probe_of_chan (branch, c)) return true;
      }
    break;
      
    case BlockType::Select:
      for (auto &branch : curr->u_select().branches) {
        if (branch.g.type() == IRGuardType::Expression) {
          if (_has_probe_of_chan (branch.g.u_e().e.m_dag, c)) return true;
        }
        if (_has_probe_of_chan (branch.seq, c)) return true;
      }
    break;
      
    case BlockType::DoLoop:
      if (_has_probe_of_chan (curr->u_doloop().branch, c)) return true;
    break;
      
    case BlockType::StartSequence:
    case BlockType::EndSequence:
      hassert(false);
    break;
    }
    curr = curr->child();
  }
  return false;
}

bool _has_probe_of_chan (const GraphWithChanNames &g, ChanId c)
{
  return _has_probe_of_chan (g.graph.m_seq, c);
}
