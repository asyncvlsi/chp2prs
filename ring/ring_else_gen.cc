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

#include "ring_else_gen.h"

static int counter = 0;

void fill_in_else_explicit (act_chp_lang_t *c, Process *p, int root)
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
            fill_in_else_explicit (stmt, p, 0);
        }
        break;

    case ACT_CHP_LOOP:
    case ACT_CHP_DOLOOP:
        gc = c->u.gc;
        fill_in_else_explicit (gc->s, p, 0);
        break;
        
    case ACT_CHP_SELECT:
        NEW (disj_gs, Expr);
        disj_gs->type = E_OR;

        NEW (expr_false, Expr);
        expr_false->type = E_FALSE;

        gc = c->u.gc;
        disj_gs->u.e.r = expr_expand(gc->g, ActNamespace::Global(), p->CurScope());
        itr = disj_gs;

        for (gc = gc->next ; gc ; gc = gc->next)
        {
            if (gc->g)
            {
                itr->u.e.l = gc->g;
                NEW (tmp, Expr);
                tmp->type = E_OR;
                tmp->u.e.r = expr_expand(itr, ActNamespace::Global(), p->CurScope());
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
                gc->g = expr_expand(inv_disj_gs, ActNamespace::Global(), p->CurScope());
            }
        }

        for (gc = c->u.gc ; gc ; gc = gc->next)
        {
            fill_in_else_explicit (gc->s, p, 0);
        }

        break;

    case ACT_CHP_SELECT_NONDET:
        for (gc = c->u.gc ; gc ; gc = gc->next)
        {
            fill_in_else_explicit (gc->s, p, 0);
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

void expand_self_assignments (act_chp_lang_t *&c, Process *p)
{
  Scope *s = p->CurScope();

  switch (c->type) {

  case ACT_CHP_SKIP:
  case ACT_CHP_SEND:
  case ACT_CHP_RECV:
    break;
  case ACT_CHP_ASSIGN:
    if (_var_appears_in_expr(c->u.assign.e,c->u.assign.id)) {
      Assert ( (TypeFactory::isDataType(c->u.assign.id->rootVx(s)->t)) , "not a data type?");
      Assert ( (TypeFactory::isIntType(c->u.assign.id->rootVx(s)->t)) , "not int type?");
      char nm[1024];
      get_true_name (nm, c->u.assign.id, s, true);
      int w = TypeFactory::bitWidth(c->u.assign.id->rootVx(s)->t);
      InstType *it = TypeFactory::Factory()->NewInt (s, Type::NONE, 0, const_expr(w));
      it = it->Expand(NULL, s);
      std::string pref = "_tmp_";
      pref.append(nm);
      ActId *new_var = new ActId (pref.c_str());
      s->Add (pref.c_str(), it);

      act_chp_lang_t *assn = new act_chp_lang_t;
      assn->type = ACT_CHP_ASSIGN;
      assn->label = NULL;
      assn->space = NULL;
      assn->u.assign.id = c->u.assign.id;
      Expr *e = new Expr;
      e->type = E_VAR;
      e->u.e.l = (Expr *)(new_var);
      assn->u.assign.e = e;

      c->u.assign.id = new_var;

      list_t *ll = list_new();
      list_append(ll, chp_expand(c, ActNamespace::Global(), s));
      list_append(ll, assn);

      c->type = ACT_CHP_SEMI;
      c->u.semi_comma.cmd = ll;
    }
    break;

  case ACT_CHP_COMMA:
  case ACT_CHP_SEMI:
    for (listitem_t *li = list_first (c->u.semi_comma.cmd); li; li = list_next (li)) 
    {   
      act_chp_lang_t *stmt = (act_chp_lang_t *) list_value (li);
      expand_self_assignments (stmt, p);
    }
    break;

  case ACT_CHP_LOOP:
  case ACT_CHP_DOLOOP:
  {
    act_chp_gc_t *gc = c->u.gc;
    expand_self_assignments (gc->s, p);
    Assert (!(gc->next), "more than one loop branch at top-level?");
  }
    break;
  case ACT_CHP_SELECT_NONDET:
  case ACT_CHP_SELECT:
  {
    act_chp_gc_t *gc = c->u.gc;
    while (gc) {
      expand_self_assignments (gc->s, p);
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
}

void make_receives_unique (act_chp_lang_t *&c, Process *p)
{
  Scope *s = p->CurScope();

  switch (c->type) {

  case ACT_CHP_SKIP:
  case ACT_CHP_SEND:
    break;
  case ACT_CHP_RECV: {
    Assert ( (TypeFactory::isDataType(c->u.comm.var->rootVx(s)->t)) , "not a data type?");
    Assert ( (TypeFactory::isIntType(c->u.comm.var->rootVx(s)->t)) , "not int type?");
    char nm[1024];
    get_true_name (nm, c->u.comm.var, s, true);
    int w = TypeFactory::bitWidth(c->u.assign.id->rootVx(s)->t);
    InstType *it = TypeFactory::Factory()->NewInt (s, Type::NONE, 0, const_expr(w));
    it = it->Expand(NULL, s);
    std::string pref = "_tmp_";
    pref.append(std::to_string(counter));
    counter++;
    pref.append("_");
    pref.append(nm);
    ActId *new_var = new ActId (pref.c_str());
    s->Add (pref.c_str(), it);

    act_chp_lang_t *assn = new act_chp_lang_t;
    assn->type = ACT_CHP_ASSIGN;
    assn->label = NULL;
    assn->space = NULL;
    assn->u.assign.id = c->u.comm.var;
    Expr *e = new Expr;
    e->type = E_VAR;
    e->u.e.l = (Expr *)(new_var);
    assn->u.assign.e = e;

    c->u.comm.var = new_var;

    list_t *ll = list_new();
    list_append(ll, chp_expand(c, ActNamespace::Global(), s));
    list_append(ll, assn);

    c->type = ACT_CHP_SEMI;
    c->u.semi_comma.cmd = ll;
    }
    break;
  case ACT_CHP_ASSIGN:
    break;

  case ACT_CHP_COMMA:
  case ACT_CHP_SEMI:
    for (listitem_t *li = list_first (c->u.semi_comma.cmd); li; li = list_next (li)) 
    {   
      act_chp_lang_t *stmt = (act_chp_lang_t *) list_value (li);
      make_receives_unique (stmt, p);
    }
    break;

  case ACT_CHP_LOOP:
  case ACT_CHP_DOLOOP:
  {
    act_chp_gc_t *gc = c->u.gc;
    make_receives_unique (gc->s, p);
    Assert (!(gc->next), "more than one loop branch at top-level?");
  }
    break;
  case ACT_CHP_SELECT_NONDET:
  case ACT_CHP_SELECT:
  {
    act_chp_gc_t *gc = c->u.gc;
    while (gc) {
      make_receives_unique (gc->s, p);
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
}

bool _var_appears_in_expr (Expr *e, ActId *id)
{
  act_connection *uid;
  ActId *i;
  bool a1, a2, a3;
  char str[1024], t[1024];
  
  if (!e) return false;
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
    a1 = _var_appears_in_expr (e->u.e.l, id);
    a2 = _var_appears_in_expr (e->u.e.r, id);
    return a1 | a2;
    break;
    
  case E_NOT:
  case E_UMINUS:
  case E_COMPLEMENT:
    a1 = _var_appears_in_expr (e->u.e.l, id);
    return a1;
    break;

  case E_QUERY:
    a1 = _var_appears_in_expr (e->u.e.l, id);
    a2 = _var_appears_in_expr (e->u.e.r->u.e.l, id);
    a3 = _var_appears_in_expr (e->u.e.r->u.e.r, id);
    return a1 | a2 | a3;
    break;

  case E_COLON:
  case E_COMMA:
    fatal_error ("Should have been handled elsewhere");
    return false;
    break;

  case E_CONCAT:
    do {
      a1 = _var_appears_in_expr (e->u.e.l, id);
      e = e->u.e.r;
    } while (e && !a1);
    return a1;
    break;

  case E_BITFIELD:
    /* l is an Id */
    // v = _var_getinfo ((ActId *)e->u.e.l);
    // if ((!_shared_expr_var || !v->fcurexpr) && !v->fischan) {
    //   v->nread++;
    //   v->fcurexpr = 1;
    // }
    return id->isEqual((ActId *)(e->u.e.l));
    break;

  case E_TRUE:
  case E_FALSE:
  case E_INT:
  case E_REAL:
    return false;
    break;

  case E_VAR:
  case E_PROBE:
    return id->isEqual((ActId *)(e->u.e.l));
    break;

    // fatal_error ("Not handling probes right now");
    // return false;
    // break;

  case E_BUILTIN_BOOL:
  case E_BUILTIN_INT:
    a1 = _var_appears_in_expr (e->u.e.l, id);
    return a1;
    break;
    
  case E_FUNCTION:
    warning ("not handling functions");
    return false;
    e = e->u.fn.r;
    while (e) {
      _var_appears_in_expr (e->u.e.l, id);
      e = e->u.e.r;
    }
    break;

  case E_SELF:
  default:
    fatal_error ("Unknown expression type %d\n", e->type);
    return false;
    break;
  }
}