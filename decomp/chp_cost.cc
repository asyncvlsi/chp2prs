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

void ChpCost::dump_actsim_conf(std::string conf_file, act_chp_lang_t *c, Process *p)
{
  Assert (!thread_mode, "limited functionality in multi-threaded mode!");
  char buf[10240];
  ActNamespace::Act()->msnprintfproc (buf, 10240, p);

  fill_in_else_explicit(c);
  FILE *ff = fopen(conf_file.c_str(), "a");
  Assert (ff, "Could not open output file to write actsim configuration");
  fprintf(ff, "begin sim\n");
  fprintf(ff, "   begin chp\n");
  fprintf(ff, "      begin decomp_%s<>\n", buf);
  std::vector<int> delays{};
  std::vector<int> energies{};
  double leakage{};
  _gen_actsim_conf (c, delays, energies, leakage);
  Assert (delays.size()==energies.size(), "what");
  fprintf(ff, "      real leakage %f", leakage);
  fprintf(ff, "\n      int_table delays ");
  for (auto x : delays) { fprintf(ff, "%d ", x); }
  fprintf(ff, "\n      int_table energies ");
  for (auto x : energies) { fprintf(ff, "%d ", x); }
  fprintf(ff, "\n      end");
  fprintf(ff, "\n   end");
  fprintf(ff, "\nend");
  fprintf(ff, "\n");
  fclose(ff);
}

bool ChpCost::_gen_actsim_conf(act_chp_lang_t *c, std::vector<int> &ds, std::vector<int> &es, double &leak)
{
  Assert (!thread_mode, "limited functionality in multi-threaded mode!");
  if (!c) return false;
  switch (c->type) {
  case ACT_CHP_SKIP:
    return false;
  break;
  case ACT_CHP_ASSIGN: {
    auto ebi = expr_metrics(c->u.assign.e,bitwidth(c->u.assign.id));
    auto edel = (ebi && ebi->getDelay().exists()) ? (ebi->getDelay().typ_val)*1e12 : 0;
    auto epow = (ebi && ebi->getDynamicPower().exists()) ? (ebi->getDynamicPower().typ_val)*1e9 : 0;
    auto eleak = (ebi && ebi->getStaticPower().exists()) ? (ebi->getStaticPower().typ_val) : 0;
    leak += eleak;
    ds.push_back( int(assn_delay + edel) );
    es.push_back( int(epow) );
    return true;
  }
  break;
  case ACT_CHP_SEND: {
    auto ebi = expr_metrics(c->u.comm.e,bitwidth(c->u.comm.chan));
    auto edel = (ebi && ebi->getDelay().exists()) ? (ebi->getDelay().typ_val)*1e12 : 0;
    auto epow = (ebi && ebi->getDynamicPower().exists()) ? (ebi->getDynamicPower().typ_val)*1e9 : 0;
    auto eleak = (ebi && ebi->getStaticPower().exists()) ? (ebi->getStaticPower().typ_val) : 0;
    leak += eleak;
    ds.push_back( int(send_delay + edel) );
    es.push_back( int(epow) );
    return true;
  }
  break;
  case ACT_CHP_RECV: {
    ds.push_back( int(recv_delay + capture_delay) );
    es.push_back( int(0) );
    return true;
  }
  break;

  case ACT_CHP_COMMA: {
    bool exists = false;
    for (listitem_t *li = list_first (c->u.semi_comma.cmd); li; li = list_next (li)) 
    {
      exists |= _gen_actsim_conf ((act_chp_lang_t *) list_value (li), ds, es, leak);
    }
    if (list_length(c->u.semi_comma.cmd)>1 && exists) {
      ds.push_back(int(list_length(c->u.semi_comma.cmd)));
      es.push_back(0);
    }
    return exists;
  }
  break;

  case ACT_CHP_SEMI: {
    bool exists = false;
    for (listitem_t *li = list_first (c->u.semi_comma.cmd); li; li = list_next (li)) 
    {
      exists |= _gen_actsim_conf ((act_chp_lang_t *) list_value (li), ds, es, leak);
    }
    return exists;
  }
  break;

  case ACT_CHP_DOLOOP: {
    _gen_actsim_conf (c->u.gc->s, ds, es, leak);
  } 
  case ACT_CHP_LOOP: {
    ds.push_back(1); // dummy val for now
    es.push_back(0);
    bool exists = _gen_actsim_conf (c->u.gc->s, ds, es, leak);
    return exists;
  }
  break;

  case ACT_CHP_SELECT_NONDET:
  case ACT_CHP_SELECT: {
    bool exists = false;
    // Assert (selection_way(c) < max_way, "Selection way beyond allowed range");
    int way = selection_way(c);
    if (way >= max_way) {
      warning ("Selection way (%d) beyond allowed max way (%d)", way, max_way);
    }
    double max_del = 0;
    double tot_energy = 0;
    act_chp_gc_t *gc = c->u.gc;
    while (gc) {
      auto ebi = expr_metrics(gc->g, 1);
      auto edel = (ebi && ebi->getDelay().exists()) ? (ebi->getDelay().typ_val)*1e12 : 0;
      auto epow = (ebi && ebi->getDynamicPower().exists()) ? (ebi->getDynamicPower().typ_val)*1e9 : 0;
      auto eleak = (ebi && ebi->getStaticPower().exists()) ? (ebi->getStaticPower().typ_val) : 0;
      double br_del = edel + capture_delay;
      if (br_del > max_del) max_del = br_del;
      tot_energy += epow;
      leak += eleak;
      gc = gc->next;
    }
    ds.push_back(int(max_del));
    es.push_back(int(tot_energy));
    gc = c->u.gc;
    while (gc) {
      exists |= _gen_actsim_conf (gc->s, ds, es, leak);
      gc = gc->next;
    }
    return exists;
  }
  break;

  case ACT_CHP_FUNC:
  fatal_error ("function");
  break;
  default:
  fatal_error ("What?");
  break;
  }
  return false;
}

void ChpCost::add_procs (std::vector<act_chp_lang_t *> cs)
{
    procs.insert( procs.end(), cs.begin(), cs.end() );
}

void ChpCost::clear()
{
    procs.clear();
}

double ChpCost::get_max_latency_cost ()
{
    Assert (!thread_mode, "limited functionality in multi-threaded mode!");
    auto costs = get_latency_costs();
    return *std::max_element(costs.begin(), costs.end());
}

std::vector<double> ChpCost::get_latency_costs ()
{
    Assert (!thread_mode, "limited functionality in multi-threaded mode!");
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
    Assert (!thread_mode, "limited functionality in multi-threaded mode!");
    double ret = 0;
    fill_in_else_explicit (c);
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
        return ( assn_delay + expr_delay (c->u.assign.e, bitwidth(c->u.assign.id)) );
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
        int way = selection_way (c);
        // Assert (way<max_way, "Selection way beyond allowed range");
        if (way>=max_way) {
          way=max_way-1;
          warning ("Selection way (%d) beyond allowed max way (%d)", way, max_way);
        }
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
    General purpose expression synthesis and delay extraction function
*/
double ChpCost::expr_delay (Expr *e, int out_bw)
{
    if (out_bw == 0) {
      return 0.0;
    }
    /*
      This is needed coz act_chp <-> chp_graph conversion
      can introduce int(struct) and struct(int)
      Returning 0.0 is correct for this coz its just wires
    */
    if (!e || e->type==E_USERMACRO || e->type==E_FUNCTION) {
      return 0.0;
    }
    _inexprmap = ihash_new (0);
    _inwidthmap = ihash_new (0);
    _reset_expr_id();

    if (!thread_mode) {
      e = expr_dag(e);
    }

    // also does a primitive dag-ing in thread mode
    canonical_expr.clear();
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
    ExprBlockInfo *ebi = eeo->synth_expr(out_bw, e, all_leaves, _inexprmap, _inwidthmap);

    Assert (ebi->getDelay().exists(), "Delay not extracted by abc!");
    double typ_delay_ps = (ebi->getDelay().typ_val)*1e12;
    
    ebi->~ExprBlockInfo();
    ebi = NULL;

    // free all temporary data structures 
    ihash_free (_inexprmap);
    _inexprmap = NULL;
    ihash_free (_inwidthmap);
    _inwidthmap = NULL;
    list_free (all_leaves);

    // lk.unlock();
    return typ_delay_ps;
}

ExprBlockInfo *ChpCost::expr_metrics (Expr *e, int out_bw)
{
  if (out_bw == 0) {
    return nullptr;
  }
  /*
    This is needed coz act_chp <-> chp_graph conversion
    can introduce int(struct) and struct(int)
    Returning 0.0 is correct for this coz its just wires
  */
  if (!e || e->type==E_USERMACRO || e->type==E_FUNCTION) {
    return nullptr;
  }
  _inexprmap = ihash_new (0);
  _inwidthmap = ihash_new (0);

  if (!thread_mode) {
    e = expr_dag(e);
  }

  // also does a primitive dag-ing in thread mode
  canonical_expr.clear();
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
  ExprBlockInfo *ebi = eeo->synth_expr(out_bw, e, all_leaves, _inexprmap, _inwidthmap);

  // free all temporary data structures 
  ihash_free (_inexprmap);
  _inexprmap = NULL;
  ihash_free (_inwidthmap);
  _inwidthmap = NULL;
  list_free (all_leaves);
  return ebi;
}

/*
    Collect all the variables in a given expression and put them 
    in the exprmap and widthmap global variables.
*/
void ChpCost::_expr_collect_vars (Expr *&e)
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
  case E_BUILTIN_INT: {
    if ((e->u.e.r)) {
      int val;
      Assert (act_expr_getconst_int(e->u.e.r, &val), "huh");
      if (val==0) {
        e = const_expr(0);
        break;
      }
    }
  }
  case E_BUILTIN_BOOL:
  case E_BITFIELD:
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

  case E_VAR: {
      ActId *var = (ActId *)e->u.e.l;
      ihash_bucket_t *ib;
      ihash_bucket_t *b_width;
      int bw = bitwidth(var);
      if (bw == 0) {
        e = const_expr(0);
        break;
      }
      if (thread_mode) {
        if (!canonical_expr.count(var)) {
          canonical_expr.insert({var,e->u.e.l});
        }
        e->u.e.l = canonical_expr.at(var);
      }
      if (!ihash_lookup (_inexprmap, (long)e)) 
      {
        ib = ihash_add (_inexprmap, (long)e);
        ib->i = _gen_expr_id();
        b_width = ihash_add (_inwidthmap, (long) e);
        b_width->i = bw;
      }
    }
    break;

  case E_PROBE: {
      Assert (!thread_mode, "shouldn't have happened");
      // make dummy variable to stand in for probe
      InstType *it = TypeFactory::Factory()->NewInt (_s, Type::NONE, 0, const_expr(1));
      static char buf[1024];
      it = it->Expand(NULL, _s);
      ActId *chan = (ActId *)e->u.e.l;
      char tname[1024];
      chan->sPrint(tname, 1024);
      snprintf(buf, 1024, "probe_of_%s", tname);
      // Replace the probe in the original expression with dummy var
      e->type = E_VAR;
      e->u.e.l = (Expr *)(new ActId (buf));
      ihash_bucket_t *ib;
      ihash_bucket_t *b_width;
      if (!ihash_lookup (_inexprmap, (long)e)) {
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

void ChpCost::_reset_expr_id()
{
  _expr_id = 0;
}

int ChpCost::bitwidth (ActId *id)
{
  if (thread_mode) {
    // if (act_var_bw.count(id)) return act_var_bw.at(id);
    for ( auto &[v, up] : varid_to_actid) {
      if (id->isEqual(up)) return g->graph.id_pool().getBitwidth(v);
    }
    fprintf(stdout, "\ncould not find var : %p\n",id);
    Assert (false, "unprovided id in threaded mode!");
    return -1;
  }
  if (!id) {
    return -1;
  }
  Assert (_s, "Shouldn't have happened");
  InstType *it = _s->FullLookup (id, NULL);
  if (!it) {
    return -1;
  }
  return TypeFactory::totBitWidth (it);
}

int ChpCost::selection_way (act_chp_lang_t *c)
{
  act_chp_gc_t *gc_itr;
  int counter = 0;
  Assert (((c->type == ACT_CHP_SELECT) || (c->type == ACT_CHP_SELECT_NONDET)), 
    "Called selection_way on a non-selection");

  for (gc_itr = c->u.gc; gc_itr; gc_itr = gc_itr->next)
  { counter++; }
  return counter;
}

void ChpCost::fill_in_else_explicit (act_chp_lang_t *c)
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
        for (li = list_first (c->u.semi_comma.cmd); li; li = list_next (li)) 
        {
            stmt = (act_chp_lang_t *)(list_value(li));
            fill_in_else_explicit (stmt);
        }
        break;

    case ACT_CHP_LOOP:
    case ACT_CHP_DOLOOP:
        gc = c->u.gc;
        fill_in_else_explicit (gc->s);
        break;
        
    case ACT_CHP_SELECT:
        NEW (disj_gs, Expr);
        disj_gs->type = E_OR;

        NEW (expr_false, Expr);
        expr_false->type = E_FALSE;

        gc = c->u.gc;
        disj_gs->u.e.r = expr_dup(gc->g);
        itr = disj_gs;

        for (gc = gc->next ; gc ; gc = gc->next)
        {
            if (gc->g)
            {
                itr->u.e.l = gc->g;
                NEW (tmp, Expr);
                tmp->type = E_OR;
                tmp->u.e.r = expr_dup(itr);
                NEW (itr, Expr);
                itr = tmp;
            }
            else
            {
                // else exists => complement and insert
                itr = itr->u.e.r;
                NEW (inv_disj_gs, Expr);
                inv_disj_gs->type = E_NOT;
                inv_disj_gs->u.e.l = itr;
                gc->g = expr_dup(inv_disj_gs);
            }
        }

        for (gc = c->u.gc ; gc ; gc = gc->next)
        {
            fill_in_else_explicit (gc->s);
        }

        break;

    case ACT_CHP_SELECT_NONDET:
        for (gc = c->u.gc ; gc ; gc = gc->next)
        {
            fill_in_else_explicit (gc->s);
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
