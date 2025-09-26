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
 *************************************************************************
 */

#include <act/chp/ring_misc.h>

static int counter = 0;

void fill_in_else_explicit (act_chp_lang_t *c, Scope *s)
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
            fill_in_else_explicit (stmt, s);
        }
        break;

    case ACT_CHP_LOOP:
    case ACT_CHP_DOLOOP:
        gc = c->u.gc;
        fill_in_else_explicit (gc->s, s);
        break;
        
    case ACT_CHP_SELECT:
        NEW (disj_gs, Expr);
        disj_gs->type = E_OR;

        NEW (expr_false, Expr);
        expr_false->type = E_FALSE;

        gc = c->u.gc;
        disj_gs->u.e.r = expr_expand(gc->g, ActNamespace::Global(), s);
        itr = disj_gs;

        for (gc = gc->next ; gc ; gc = gc->next)
        {
            if (gc->g)
            {
                itr->u.e.l = gc->g;
                NEW (tmp, Expr);
                tmp->type = E_OR;
                tmp->u.e.r = expr_expand(itr, ActNamespace::Global(), s);
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
                gc->g = expr_expand(inv_disj_gs, ActNamespace::Global(), s);
            }
        }

        for (gc = c->u.gc ; gc ; gc = gc->next)
        {
            fill_in_else_explicit (gc->s, s);
        }

        break;

    case ACT_CHP_SELECT_NONDET:
        for (gc = c->u.gc ; gc ; gc = gc->next)
        {
            fill_in_else_explicit (gc->s, s);
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

void expand_self_assignments (act_chp_lang_t *&c, Scope *s)
{
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
      expand_self_assignments (stmt, s);
    }
    break;

  case ACT_CHP_LOOP:
  case ACT_CHP_DOLOOP:
  {
    act_chp_gc_t *gc = c->u.gc;
    expand_self_assignments (gc->s, s);
    Assert (!(gc->next), "more than one loop branch at top-level?");
  }
    break;
  case ACT_CHP_SELECT_NONDET:
  case ACT_CHP_SELECT:
  {
    act_chp_gc_t *gc = c->u.gc;
    while (gc) {
      expand_self_assignments (gc->s, s);
      gc = gc->next;
    }
  }
  break;

  case ACT_CHP_FUNC:
    break;

  default:
    fatal_error ("What?");
    break;
  }
}

void flatten_lists (act_chp_lang_t *&c, Scope *s)
{
  switch (c->type) {

  case ACT_CHP_SKIP:
  case ACT_CHP_SEND:
  case ACT_CHP_RECV:
  case ACT_CHP_ASSIGN:
    break;

  case ACT_CHP_COMMA:
  case ACT_CHP_SEMI:
    for (listitem_t *li = list_first (c->u.semi_comma.cmd); li; li = list_next (li)) 
    {   
      act_chp_lang_t *stmt = (act_chp_lang_t *) list_value (li);
      flatten_lists(stmt, s);
      if (stmt->type == c->type) 
      {
        list_splice (c->u.semi_comma.cmd, li, stmt->u.semi_comma.cmd);
        if (li==list_first(c->u.semi_comma.cmd)) 
        {
          list_delete_head(c->u.semi_comma.cmd);
          li = list_first(c->u.semi_comma.cmd);
        }
        else 
        {
          listitem_t *ll = list_first(c->u.semi_comma.cmd);
          while (ll->next != li) { ll = ll->next; }
          list_delete_next(c->u.semi_comma.cmd, ll);
          li = ll->next;
        }  
      }
    }
    break;

  case ACT_CHP_LOOP:
  case ACT_CHP_DOLOOP:
  {
    act_chp_gc_t *gc = c->u.gc;
    flatten_lists (gc->s, s);
    Assert (!(gc->next), "more than one loop branch at top-level?");
  }
    break;
  case ACT_CHP_SELECT_NONDET:
  case ACT_CHP_SELECT:
  {
    act_chp_gc_t *gc = c->u.gc;
    while (gc) {
      flatten_lists (gc->s, s);
      gc = gc->next;
    }
  }
  break;

  case ACT_CHP_FUNC:
    break;

  default:
    fatal_error ("What?");
    break;
  }
}

void make_receives_unique (act_chp_lang_t *&c, Scope *s)
{
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
      make_receives_unique (stmt, s);
    }
    break;

  case ACT_CHP_LOOP:
  case ACT_CHP_DOLOOP:
  {
    act_chp_gc_t *gc = c->u.gc;
    make_receives_unique (gc->s, s);
    Assert (!(gc->next), "more than one loop branch at top-level?");
  }
    break;
  case ACT_CHP_SELECT_NONDET:
  case ACT_CHP_SELECT:
  {
    act_chp_gc_t *gc = c->u.gc;
    while (gc) {
      make_receives_unique (gc->s, s);
      gc = gc->next;
    }
  }
  break;

  case ACT_CHP_FUNC:
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

static Act *a_mangle = NULL;

static const int style_global = 0;

void mangle_init ()
{ 
  char u[8];
  a_mangle = new Act;
  snprintf(u,8,"[],.<>"); /// characters to mangle
  a_mangle->mangle(u);
}

void revert_mangle ()
{
  if (config_exists ("act.mangle_letter")) {
  const char *tmp = config_get_string ("act.mangle_letter");
  if (!a_mangle->mangle_set_char (*tmp)) {
    fatal_error ("act.mangle_letter: could not be used as a character!");
  }
  }
  else {
    a_mangle->mangle_set_char ('_');
  }
  if (config_exists ("act.mangle_chars")) {
    a_mangle->mangle (config_get_string ("act.mangle_chars"));
  }
} 

void get_true_name (char *buf, ActId *id, Scope *s, bool mangle)
{
  char str[1024];
  id->sPrint(str,1024,NULL,style_global);
  if (mangle && !(TypeFactory::isChanType(s->FullLookup((id->getName()))))) {
    a_mangle->mangle_string(str,buf,1024);
  }
  else  
    snprintf (buf, 1024, "%s", str);
}

void mangle_it (char *buf, InstType *it)
{
  char name[10240];
  it->sPrint(name, 10240);
  if (!a_mangle) mangle_init();
  a_mangle->mangle_string(name, buf, 10240);
}

void mangle_data (char *buf, Data *d)
{
  char name[10240];
  d->snprintActName(name, 10240);
  if (!a_mangle) mangle_init();
  a_mangle->mangle_string(name, buf, 10240);
}

void generate_array_suffix (char *buf, Array *a)
{
  char s[1024];
  a->sPrint(s,1024,style_global);
  a_mangle->mangle_string(s,buf,1024);
}