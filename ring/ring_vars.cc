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

#include "ring_vars.h"

void RingVarAnalysis::_add_to_live_vars (ActId *id)
{
    InstType *it = p->CurScope()->localLookup(id, NULL);

    if (TypeFactory::isStructure(it)) {

        Data *d = dynamic_cast<Data *>(it->BaseType());
        int w = TypeFactory::totBitWidth(d);
        int nb, ni;
        int *types;
        d->getStructCount(&nb, &ni);
        ActId **res = d->getStructFields(&types);
        ActId *tail = id->Tail();
        for (int i=0; i < ni + nb; i++) {
            int sz;
            InstType *xit;
            Assert (d->getStructOffset (res[i], &sz, &xit) != -1, "What?");
            tail->Append (res[i]);
            _add_to_live_vars (tail);
            tail->prune();
            delete res[i];
            delete xit;
        }
    }
    else {
        Assert (id, "what");
        H_live.insert(id->Canonical(p->CurScope()));
    }
}

void RingVarAnalysis::_add_to_live_vars (Expr *e)
{
  ActId *var;
  if (!e) return;

#define BINARY_OP					\
  do {							    \
    _add_to_live_vars (e->u.e.l);	\
    _add_to_live_vars (e->u.e.r);	\
  } while (0)

#define UNARY_OP					\
  do {							    \
    _add_to_live_vars (e->u.e.l);	\
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
    _add_to_live_vars (e->u.e.l);
    _add_to_live_vars (e->u.e.r->u.e.l);
    _add_to_live_vars (e->u.e.r->u.e.r);
    break;

  case E_COLON:
  case E_COMMA:
    fatal_error ("Should have been handled elsewhere");
    break;

  case E_CONCAT:
    {
      Expr *tmp = e;
      while (tmp) {
        _add_to_live_vars (tmp->u.e.l);
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
  case E_VAR:
    _add_to_live_vars ((ActId *)e->u.e.l);
    break;

  case E_PROBE:
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

void RingVarAnalysis::_remove_from_live_vars (ActId *id)
{
    if (!id) return;
    InstType *it = p->CurScope()->localLookup(id, NULL);

    if (TypeFactory::isStructure(it)) {

        Data *d = dynamic_cast<Data *>(it->BaseType());
        int w = TypeFactory::totBitWidth(d);
        int nb, ni;
        int *types;
        d->getStructCount(&nb, &ni);
        ActId **res = d->getStructFields(&types);
        ActId *tail = id->Tail();
        for (int i=0; i < ni + nb; i++) {
            int sz;
            InstType *xit;
            Assert (d->getStructOffset(res[i], &sz, &xit) != -1, "What?");
            tail->Append (res[i]);
            _remove_from_live_vars (tail);
            tail->prune();
            delete res[i];
            delete xit;
        }
    }
    else {
        if (H_live.count(id->Canonical(p->CurScope())))
            H_live.erase(id->Canonical(p->CurScope()));
    }
    return;
}

void RingVarAnalysis::_add_to_live_vars_lcd (ActId *id)
{
    InstType *it = p->CurScope()->localLookup (id, NULL);
    Assert (!TypeFactory::isStructure(it), "Structure assign not broken up?"); 
    H_lcd.insert(id->Canonical(p->CurScope()));
}

void RingVarAnalysis::_tag_action_with_reqd_vars (act_chp_lang_t *action, int is_latch)
{
    if (!action) return;
    Assert (((action->type == ACT_CHP_ASSIGN)||
             (action->type == ACT_CHP_SEND)||
             (action->type == ACT_CHP_RECV)),"not action");

    std::vector<act_connection *> req_vars;
    for ( auto v : H_live ) {
        req_vars.push_back(v);
    }
    latch_info_t *l_info;
    l_info = new latch_info_t;
    l_info->latch_numbers = {};
    l_info->merge_mux_latch_number.clear();
    l_info->merge_mux_inputs.clear();
    l_info->type = (is_latch==0) ? (LatchType::Alias) : (LatchType::Latch);
    l_info->live_vars = req_vars;

    action->space = l_info;
}

void RingVarAnalysis::_tag_action_with_reqd_vars_union_lcd (act_chp_lang_t *action)
{
    if (!action) return;
    Assert (((action->type == ACT_CHP_LOOP)||
             (action->type == ACT_CHP_DOLOOP)||
             (action->type == ACT_CHP_SELECT)||
             (action->type == ACT_CHP_SELECT_NONDET)),"not selection");

    std::vector<act_connection *> req_vars;

    for ( auto v : H_lcd ) {
        if (!H_live.count(v))
            req_vars.push_back(v);
    }
    for ( auto v : H_live ) {
        req_vars.push_back(v);
    }

    latch_info_t *l_info;
    l_info = new latch_info_t;
    l_info->merge_mux_latch_number.clear();
    l_info->merge_mux_inputs.clear();
    if (action->type != ACT_CHP_SELECT && action->type != ACT_CHP_SELECT_NONDET) {
        l_info->type = LatchType::ICs;
    }
    else {
        l_info->type = LatchType::Mux;
    }
    l_info->live_vars = req_vars;

    action->space = l_info;
}

void RingVarAnalysis::_generate_live_var_info (act_chp_lang_t *c_t, int root)
{
    listitem_t *li;
    list_t *copy_list;
    act_chp_lang_t *stmt;
    act_chp_gc_t *gc;
    std::set<act_connection *> H_dup, H_out;
    
    if (!c_t) return;

    switch (c_t->type) {
    case ACT_CHP_COMMALOOP:
    case ACT_CHP_SEMILOOP:
        fatal_error ("Replication loops should've been removed..");
        break;
        
    case ACT_CHP_COMMA:
    case ACT_CHP_SEMI:
        if (root == 1)
        {
            for (li = list_first (c_t->u.semi_comma.cmd); li; li = list_next (li)) 
            {
                stmt = (act_chp_lang_t *)(list_value(li));
                if (stmt->type == ACT_CHP_ASSIGN) {
                    _add_to_live_vars_lcd (stmt->u.assign.id);
                    _tag_action_with_reqd_vars (stmt, 1);
                }
            }
            for (li = list_first (c_t->u.semi_comma.cmd); li; li = list_next (li)) 
            {
                stmt = (act_chp_lang_t *)(list_value(li));
                if (stmt->type == ACT_CHP_LOOP || stmt->type == ACT_CHP_DOLOOP) {
                    _generate_live_var_info (stmt, 1);
                    _tag_action_with_reqd_vars_union_lcd (stmt);
                }
            }
            break;
        }
        else {
            copy_list = list_dup (c_t->u.semi_comma.cmd);
            list_reverse (copy_list);
            for (li = list_first (copy_list); li; li = list_next (li)) 
            {
                stmt = (act_chp_lang_t *)(list_value(li));
                _generate_live_var_info (stmt, 0);
            }
        }
        break;

    case ACT_CHP_LOOP:
    case ACT_CHP_DOLOOP:
        if (root == 1)
        {
            gc = c_t->u.gc;
            _generate_live_var_info (gc->s, 0);
            _tag_action_with_reqd_vars_union_lcd (c_t);
            break;
        }
        else
        {
            fatal_error ("should've excised internal loops...");
        }
        break;
        
    case ACT_CHP_SELECT:
    case ACT_CHP_SELECT_NONDET:
        // for selections alone, live = live_out of merge
        _tag_action_with_reqd_vars_union_lcd (c_t);
        H_dup = H_live;
        H_out = {};
        for (gc = c_t->u.gc ; gc ; gc = gc->next)
        {
            _add_to_live_vars (gc->g);
            _generate_live_var_info (gc->s, 0);
            // reverse...
            H_out.insert(H_live.begin(), H_live.end());
            H_live = H_dup;
        }
        H_live = H_out;
        break;
        
    case ACT_CHP_SKIP:
        break;
    case ACT_CHP_ASSIGN:
    case ACT_CHP_ASSIGNSELF:
        // order important!!
        _remove_from_live_vars (c_t->u.assign.id);
        _add_to_live_vars (c_t->u.assign.e);
        _tag_action_with_reqd_vars (c_t, 0);
        break;
        
    case ACT_CHP_RECV:
        _remove_from_live_vars (c_t->u.comm.var);
        _tag_action_with_reqd_vars (c_t, 1);
        break;

    case ACT_CHP_SEND:
        _add_to_live_vars (c_t->u.comm.e);
        _tag_action_with_reqd_vars (c_t, 0);
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
}

void RingVarAnalysis::_print_live_var_info (act_chp_lang_t *c_t, int root)
{
    listitem_t *li;
    act_chp_lang_t *stmt;
    act_chp_gc_t *gc;

    if (!c_t) return;

    switch (c_t->type) {
    case ACT_CHP_COMMALOOP:
    case ACT_CHP_SEMILOOP:
        fatal_error ("Replication loops should've been removed..");
        break;
        
    case ACT_CHP_COMMA:
    case ACT_CHP_SEMI:    
        for (li = list_first (c_t->u.semi_comma.cmd); li; li = list_next (li)) {
            stmt = (act_chp_lang_t *)(list_value(li));
            // if (stmt->type == ACT_CHP_LOOP || stmt->type == ACT_CHP_DOLOOP) 
            _print_live_var_info (stmt, root);
        }
        break;

    case ACT_CHP_LOOP:
    case ACT_CHP_DOLOOP:
        if (root == 1) {
            chp_print (fp, c_t);
            _print_var_list(((latch_info_t *)(c_t->space))->live_vars);
            gc = c_t->u.gc;
            _print_live_var_info (gc->s, 0);
            break;
        }
        else {
            fatal_error ("should've excised internal loops...");
        }
        break;
        
    case ACT_CHP_SELECT:
    case ACT_CHP_SELECT_NONDET:
        chp_print (fp, c_t);
        _print_var_list(((latch_info_t *)(c_t->space))->live_vars);
        for (gc = c_t->u.gc ; gc ; gc = gc->next) {
            _print_live_var_info (gc->s, 0);
        }
        break;

    case ACT_CHP_SKIP:
        break;
        
    case ACT_CHP_ASSIGN:
    case ACT_CHP_ASSIGNSELF:
        chp_print (fp, c_t);
        _print_var_list(((latch_info_t *)(c_t->space))->live_vars);
        break;
        
    case ACT_CHP_RECV:
        chp_print (fp, c_t);
        _print_var_list(((latch_info_t *)(c_t->space))->live_vars);
        break;

    case ACT_CHP_SEND:
        chp_print (fp, c_t);
        _print_var_list(((latch_info_t *)(c_t->space))->live_vars);
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
}

void RingVarAnalysis::_print_var_list (std::vector<act_connection *> var_list)
{   
    listitem_t *li;
    fprintf(fp, "\n-----------");
    fprintf(fp, "\nlive-in at this point:\n");
    for (auto v : var_list)
    {
        v->Print(fp);
        fprintf(fp, ", ");
    }	     
    fprintf(fp, "\n-----------");
    fprintf(fp, "\n\n");
}

void RingVarAnalysis::print_var_info ()
{
    _print_live_var_info (c, 1);
}

void RingVarAnalysis::generate_var_info ()
{
    _generate_live_var_info (c, 1);
    _generate_live_var_info (c, 1);
}
