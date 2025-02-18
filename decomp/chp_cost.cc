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

#include "chp_cost.h"


void ChpCost::add_procs (std::vector<act_chp_lang_t *> cs)
{
    procs.insert( procs.end(), cs.begin(), cs.end() );
}

void ChpCost::clear()
{
    procs.clear();
}

std::vector<double> ChpCost::get_latency_costs ()
{
    std::vector<double> latency_costs = {};
    for ( auto c : procs ) {
        latency_costs.push_back(latency_cost(c));
    }
    return latency_costs;
}

/*
    Calculate latency for a given CHP process.
    Assumes no internal loops.
*/
double ChpCost::latency_cost (act_chp_lang_t *c)
{
    double ret = 0;
    return _latency_cost (c);
}

double ChpCost::_latency_cost (act_chp_lang_t *c)
{
    if (!c) return 0;
    switch (c->type) {
    case ACT_CHP_SKIP:
        return 0;
    break;
    case ACT_CHP_ASSIGN:
        return ( assn_delay + capture_delay + expr_delay (c->u.assign.e, bitwidth(c->u.assign.id)) );
    break;
    case ACT_CHP_SEND:
        return ( send_delay + expr_delay (c->u.comm.e, bitwidth(c->u.comm.chan)) );
    break;
    case ACT_CHP_RECV:
        return ( recv_delay + capture_delay );
    break;

    case ACT_CHP_COMMA: {
        double max_del = 0;
        for (listitem_t *li = list_first (c->u.semi_comma.cmd); li; li = list_next (li)) 
        {
            double br_del = _latency_cost ((act_chp_lang_t *) list_value (li));
            if (br_del > max_del) max_del = br_del;
        }
        return ( max_del );
    }
    break;

    case ACT_CHP_SEMI: {
        double total_del = 0;
        for (listitem_t *li = list_first (c->u.semi_comma.cmd); li; li = list_next (li)) 
        {
            total_del += _latency_cost ((act_chp_lang_t *) list_value (li));
        }
        return total_del;
    }
    break;

    case ACT_CHP_LOOP:
    case ACT_CHP_DOLOOP: {
        return _latency_cost (c->u.gc->s);
    }
    break;

    case ACT_CHP_SELECT_NONDET:
    case ACT_CHP_SELECT: {
        Assert (false, "wip");
        int way = selection_way (c);
        Assert (way<max_way, "Selection way beyond allowed range");
        double max_del = 0;
        act_chp_gc_t *gc = c->u.gc;
        while (gc) 
        {
            double br_del = _latency_cost (gc->s) + expr_delay (gc->g, 1) + capture_delay;
            if (br_del > max_del) max_del = br_del;
            gc = gc->next;
        }
        return ( max_del + sel_delays[way] + or_delays[way] );
    }
    break;

    case ACT_CHP_FUNC:
    fatal_error ("function");
    break;
    default:
    fatal_error ("What?");
    break;
    }
    Assert (false, "brr");
    return -1;
}

/*
    General purpose synthesis function
*/
double ChpCost::expr_delay (Expr *e, int out_bw)
{
    const char *efile = "/dev/null";
    const char *eblk_pfx = "eblk_";
    const char *eblk_inp_pfx = "in_";
    // create mapper object
    ExternalExprOpt *eeo = new ExternalExprOpt("abc", bd, false, 
                                                efile, eblk_inp_pfx, eblk_pfx);
    Assert ((eeo), "Could not create mapper");

    _inexprmap = ihash_new (0);
    _inwidthmap = ihash_new (0);

    e = expr_expand(e, ActNamespace::Global(), _s);
    e = expr_dag(e);

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

    config_set_int("expropt.verbose", 0);
    config_set_int("expropt.abc_use_constraints", 1);
    config_set_int("expropt.vectorize_all_ports", 1);

    // run abc, then v2act to create the combinational-logic-for-math process
    ExprBlockInfo *ebi = eeo->run_external_opt(0, out_bw, e, all_leaves, _inexprmap, _inwidthmap);

    Assert (ebi->getDelay().exists(), "Delay not extracted by abc!");
    double typ_delay_ps = (ebi->getDelay().typ_val)*1e12;
    // if (typ_delay_ps <= 0) { fprintf(stdout, "warning: non-positive delay from abc: %fps", typ_delay_ps); }
    
    eeo->~ExternalExprOpt();
    ebi->~ExprBlockInfo();
    eeo = NULL;
    ebi = NULL;

    // free all temporary data structures 
    ihash_free (_inexprmap);
    _inexprmap = NULL;
    ihash_free (_inwidthmap);
    _inwidthmap = NULL;
    list_free (all_leaves);

    return typ_delay_ps;
}

/*
    Collect all the variables in a given expression and put them 
    in the exprmap and widthmap global variables.
*/
void ChpCost::_expr_collect_vars (Expr *e)
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
        // fprintf(stdout, "\nle: %lu\n", long(e));
        ActId *var = (ActId *)e->u.e.l;
        ihash_bucket_t *ib;
        ihash_bucket_t *b_width;
        if (!ihash_lookup (_inexprmap, (long)e)) 
        {
            ib = ihash_add (_inexprmap, (long)e);
            ib->i = _gen_expr_id();
            b_width = ihash_add (_inwidthmap, (long) e);
            b_width->i = TypeFactory::bitWidth(var->rootCanonical(_s)->getvx()->t);
        }
    }
    break;

  case E_PROBE: {
        fatal_error("no probes");

        // make dummy variable to stand in for probe
        InstType *it = TypeFactory::Factory()->NewInt (_s, Type::NONE, 0, const_expr(1));
        static char buf[1024];
        it = it->Expand(NULL, _s);

        ActId *chan = (ActId *)e->u.e.l;
        // var_info *vi;
        // hash_bucket_t *b;
        // char tname[1024];
        // get_true_name(tname, chan, _p->CurScope());
        // snprintf(buf, 1024, "probe_of_%s", tname);
        // _p->CurScope()->Add (buf, it);

        // // Expr *ee = new Expr;
        // // Replace the probe in the original expression with dummy var
        // e->type = E_VAR;
        // e->u.e.l = (Expr *)(new ActId (buf));

        auto tst = _s->Lookup((ActId *)e->u.e.l);
        Assert (tst, "hmm new id");

        // auto b = hash_add(var_infos, buf);
        // b = hash_lookup(var_infos, tname);
        // vi = (var_info *)b->v;
        ihash_bucket_t *ib;
        ihash_bucket_t *b_width;
        if (!ihash_lookup (_inexprmap, (long)e)) 
        {
            ib = ihash_add (_inexprmap, (long)e);
            ib->i = _gen_expr_id();
            b_width = ihash_add (_inwidthmap, (long)e);
            b_width->i = 1;
        }
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

int ChpCost::_gen_expr_id()
{
    return _expr_id++;
}

int ChpCost::bitwidth (ActId *id)
{
  if (!id) {
    return -1;
  }
  InstType *it = _s->FullLookup (id, NULL);
  if (!it) {
    return -1;
  }
  return TypeFactory::bitWidth (it);
}

int ChpCost::selection_way (act_chp_lang_t *c)
{
  act_chp_gc_t *gc_itr;
  int counter = 0;
  Assert (((c->type == ACT_CHP_SELECT)), "Called selection_way on a non-selection");

  for (gc_itr = c->u.gc; gc_itr; gc_itr = gc_itr->next)
  { counter++; }
  return counter;
}
