/*************************************************************************
 *
 *  Copyright (c) 2025 Karthi Srinivasan
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

#include "expr_pipe.h"
#include "eqn_parser.h"


void ExprPipe::run()
{
    var_to_actvar table(s, g->graph.id_pool());
    _run_seq(g->graph.m_seq, table);
}

void ExprPipe::_run_seq(Sequence seq, var_to_actvar &table)
{
    auto varToId = [&] (const VarId &v) { 
        return table.varMap (v); 
    };
    auto chanToId = [&] (const ChanId &v) { 
        return table.chanMap (v); 
    };

    Block *curr = seq.startseq->child();

    while (curr->type() != BlockType::EndSequence) {
    switch (curr->type()) {
    case BlockType::Basic: {
        switch (curr->u_basic().stmt.type()) {
        case StatementType::Receive:
        break;
        case StatementType::Send: {
            Expr *e = ChpOptimize::template_func_new_expr_from_irexpr (
                        *curr->u_basic().stmt.u_send().e.m_dag.roots[0],
						ActExprIntType::Int, varToId, chanToId);
            _run_expr(e, g->graph.id_pool().getBitwidth(curr->u_basic().stmt.u_send().chan));
            
        }
        break;
        case StatementType::Assign: {
            auto ids = curr->u_basic().stmt.u_assign().ids;
            Assert (ids.size()==1, "assignments unsplit");
            Expr *e = ChpOptimize::template_func_new_expr_from_irexpr ( 
                        *curr->u_basic().stmt.u_assign().e.roots[0], 
                        ActExprIntType::Int, varToId, chanToId);
            // _run_expr(e, g->graph.id_pool().getBitwidth(ids[0]));
        }
        break;
        }
    }
    break;
      
    case BlockType::Par: {
        for (auto &branch : curr->u_par().branches) {
            _run_seq (branch, table);
        }
    }
    break;
      
    case BlockType::Select: {
        for ( auto &branch : curr->u_select().branches ) {
            _run_seq (branch.seq, table);
        }
    }
    break;
      
    case BlockType::DoLoop: {
        _run_seq (curr->u_doloop().branch, table);
    }
    break;
    
    case BlockType::StartSequence:
    case BlockType::EndSequence:
        hassert(false);
        break;
    }
    curr = curr->child();
    }
}

void ExprPipe::_run_expr (Expr *e, int width)
{
    auto mapped_verilog = _expr_to_verilog (e, width);
    _verilog_to_eqn (mapped_verilog);
    cleanup_tmp_files();
    EqnParser p("out.eqn", g->graph.id_pool());
    p.parseFile();
    var_to_actvar table(s, g->graph.id_pool());
    auto varToId = [&] (const VarId &v) { return table.varMap(v); };
    auto chanToId = [&] (const ChanId &v) { return table.chanMap(v); };

    for ( auto &[x,y] : p.get_name_map() ) {
        fprintf(stdout, "v%llu : %s\n", y.m_id, x.c_str());
    }

    auto vblks = p.get_assigns(g->graph);
    for ( auto b : vblks ) {
        std::cout << std::endl;
        print_chp_block(std::cout, b);
    }

}


/*
    TODO replace this with ABC api call
*/
void ExprPipe::_verilog_to_eqn (std::string mapped_verilog)
{
    int n_cuts = 2;

    std::string cmd = "read_lib " + std::string(config_get_string("synth.liberty.typical"));
    cmd += "; read_verilog -m "+mapped_verilog+"; pipe -L "+std::to_string(n_cuts)+"; \
        balance; rewrite; refactor; balance; rewrite -z; balance; \
        rewrite -z; balance; strash; ifraig; scorr; dc2; \
        retime -o; strash; write_eqn out.eqn";

    auto abc_run = "abc -q \"" + cmd + "\" > /dev/null";
    system(abc_run.c_str());
}

std::string ExprPipe::_expr_to_verilog (Expr *e, int width)
{
    std::string name = "test";

    _inexprmap = ihash_new (4);
    _inwidthmap = ihash_new (4);

    e = expr_dag(e);

    // also does a primitive dag-ing in thread mode
    _expr_collect_vars (e);

    // collect input vars in list
    list_t *all_leaves = list_new();
    {
        ihash_iter_t iter;
        ihash_bucket_t *ib;
        ihash_iter_init (_inexprmap, &iter);
        while ((ib = ihash_iter_next (_inexprmap, &iter))) {
        Expr *e1 = (Expr *)ib->key;
        list_append (all_leaves, e1);
        }
    }

    config_set_int("synth.expropt.verbose", 0);
    // run abc, then v2act to create the combinational-logic-for-math process
    auto ebi = run_external_opt(name, width, e, all_leaves, _inexprmap, _inwidthmap, false);
    auto ret = ebi->getMappedFile();
    ebi->~ExprBlockInfo();
    ebi = NULL;

    // free all temporary data structures 
    ihash_free (_inexprmap);
    _inexprmap = NULL;
    ihash_free (_inwidthmap);
    _inwidthmap = NULL;
    list_free (all_leaves);
    return ret;
}

void ExprPipe::_expr_collect_vars (Expr *e)
{
  Assert (e, "Hmm");

#define BINARY_OP					\
  do {							\
    _expr_collect_vars (e->u.e.l);	\
    _expr_collect_vars (e->u.e.r);	\
  } while (0)

#define UNARY_OP					\
  do {							\
    _expr_collect_vars (e->u.e.l);	\
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
    _expr_collect_vars (e->u.e.l);
    _expr_collect_vars (e->u.e.r->u.e.l);
    _expr_collect_vars (e->u.e.r->u.e.r);
    break;

  case E_COLON:
  case E_COMMA:
    fatal_error ("Should have been handled elsewhere");
    break;

    /* XXX: here */
  case E_CONCAT:
    {
      Expr *tmp = e;
      while (tmp) {
	_expr_collect_vars (tmp->u.e.l);
	tmp = tmp->u.e.r;
      }
    }
    break;

  case E_REAL:
    fatal_error ("No real expressions please.");
    break;

  case E_TRUE:
    break;
    
  case E_FALSE:
    break;
    
  case E_INT:
    break;

  case E_BITFIELD:
  case E_VAR: {
    ActId *var = (ActId *)e->u.e.l;
    ihash_bucket_t *ib;
    ihash_bucket_t *b_width;
    if (!ihash_lookup (_inexprmap, (long)e)) 
    {
        ib = ihash_add (_inexprmap, (long)e);
        ib->i = _gen_expr_id();
        b_width = ihash_add (_inwidthmap, (long) e);
        b_width->i = bitwidth(var);
    }

    }
    break;

  case E_PROBE: {
      Assert (false, "No probes in decomp lmao");
    }
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

int ExprPipe::_gen_expr_id()
{
    return _m_expr_id++;
}

int ExprPipe::bitwidth (ActId *id)
{
  if (!id) {
    return -1;
  }
  Assert (s, "Shouldn't have happened");
  InstType *it = s->FullLookup (id, NULL);
  if (!it) {
    return -1;
  }
  return TypeFactory::bitWidth (it);
}
