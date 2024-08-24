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

#include "ring_live_vars.h"

void deepcopy_hashtable (Hashtable *H_in, Hashtable *H_out)
{
    hash_clear (H_out);
    hash_iter_t itr;
    hash_bucket_t *b;
    hash_iter_init (H_in, &itr);
    while ((b = hash_iter_next(H_in, &itr))) 
    {
        hash_add (H_out, Strdup(b->key));
    }
}

void union_hashtable (Hashtable *H_acc, Hashtable *H_inc)
{
    hash_iter_t itr;
    hash_bucket_t *b;
    hash_iter_init (H_inc, &itr);
    while ((b = hash_iter_next(H_inc, &itr))) 
    {
        if (!hash_lookup(H_acc, b->key)) {
            hash_add (H_acc, Strdup(b->key));
        }
    }
}

void LiveVarAnalysis::_add_to_live_vars (ActId *id, bool mangle = true)
{
    char tname[1024];
    get_true_name(tname, id, p->CurScope(), mangle);
    hash_bucket_t *b;
    b = hash_lookup(H_live, tname);
    if (!b)
    {
        hash_add (H_live, tname);
    } 
}

void LiveVarAnalysis::_add_to_live_vars (Expr *e, bool mangle = true)
{
  int id;
  ActId *var;
  hash_bucket_t *b;
  char tname[1024];
//   Assert (e, "Hmm");
  if (!e) return;

#define BINARY_OP					\
  do {							\
    _add_to_live_vars (e->u.e.l, mangle);	\
    _add_to_live_vars (e->u.e.r, mangle);	\
  } while (0)

#define UNARY_OP					\
  do {							\
    _add_to_live_vars (e->u.e.l, mangle);	\
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
    _add_to_live_vars (e->u.e.l, mangle);
    _add_to_live_vars (e->u.e.r->u.e.l, mangle);
    _add_to_live_vars (e->u.e.r->u.e.r, mangle);
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
        _add_to_live_vars (tmp->u.e.l, mangle);
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
        var = (ActId *)e->u.e.l;
        get_true_name(tname, var, p->CurScope(), mangle);
        b = hash_lookup(H_live, tname);
        if (!b)
        {
            hash_add (H_live, tname);
        }
    break;

  case E_PROBE:
    fatal_error ("fix probes please");
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

void LiveVarAnalysis::_remove_from_live_vars (ActId *id)
{
    char tname[1024];
    if (!id) return;
    get_true_name (tname, id, p->CurScope());
    if (hash_lookup (H_live, tname))
    {
        hash_delete (H_live, tname);
        return;
    }
    // fatal_error ("Attempted to delete variable not in live-var list");
    return;
}

void LiveVarAnalysis::_add_to_live_vars_lcd (ActId *id)
{
    char tname[1024];
    get_true_name (tname, id, p->CurScope());
    hash_bucket_t *b;
    b = hash_lookup (H_lcd, tname);
    if (!b)
    {
        hash_add (H_lcd, tname);
    } 
}

void LiveVarAnalysis::_tag_action_with_reqd_vars (act_chp_lang_t *action)
{
    if (!action) return;
    Assert (((action->type == ACT_CHP_ASSIGN)||
             (action->type == ACT_CHP_SEND)||
             (action->type == ACT_CHP_RECV)),"not action");

    hash_bucket_t *b;
    hash_iter_t itr;
    list_t *req_vars = list_new();
    hash_iter_init (H_live, &itr);
    while ((b = hash_iter_next(H_live, &itr))) 
    {
        list_append (req_vars, Strdup(b->key));
    }
    latch_info_t *l_info;
    l_info = new latch_info_t;
    l_info->merge_mux_latch_number.clear();
    l_info->merge_mux_inputs.clear();
    l_info->type = LatchType::Latch;
    l_info->live_vars = list_dup(req_vars);

    action->space = l_info;
    // action->space = list_dup(req_vars);
}

void LiveVarAnalysis::_tag_action_with_reqd_vars_union_lcd (act_chp_lang_t *action)
{
    if (!action) return;
    Assert (((action->type == ACT_CHP_LOOP)||
             (action->type == ACT_CHP_DOLOOP)||
             (action->type == ACT_CHP_SELECT)),"not selection");

    hash_bucket_t *b, *b2;
    list_t *req_vars = list_new();
    hash_iter_t itr, itr2;
    hash_iter_init (H_lcd, &itr);
    while ((b = hash_iter_next(H_lcd, &itr))) 
    {
        if (!hash_lookup(H_live, b->key))
        {
            list_append (req_vars, Strdup(b->key));
        }
    }
    hash_iter_init (H_live, &itr2);
    while ((b2 = hash_iter_next(H_live, &itr2))) 
    {
        list_append (req_vars, Strdup(b2->key));
    }	     

    latch_info_t *l_info;
    l_info = new latch_info_t;
    l_info->merge_mux_latch_number.clear();
    l_info->merge_mux_inputs.clear();
    if (action->type != ACT_CHP_SELECT)
    {
        l_info->type = LatchType::ICs;
    }
    else {
        l_info->type = LatchType::Mux;
    }
    l_info->live_vars = list_dup(req_vars);

    action->space = l_info;
    // action->space = list_dup(req_vars);
}

void LiveVarAnalysis::_generate_live_var_info (act_chp_lang_t *c_t, int root)
{
    listitem_t *li;
    list_t *copy_list;
    act_chp_lang_t *stmt;
    act_chp_gc_t *gc;
    Hashtable *H_dup, *H_out;
    
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
                    _tag_action_with_reqd_vars (stmt);
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
            // _print_var_list ((list_t *)c_t->space);
            break;
        }
        else
        {
            fatal_error ("should've excised internal loops...");
        }
        break;
        
    case ACT_CHP_SELECT:
        // for selections alone, live = live_out of merge
        _tag_action_with_reqd_vars_union_lcd (c_t);
        H_dup = hash_new (4);
        H_out = hash_new (4);
        deepcopy_hashtable (H_live, H_dup);
        hash_clear (H_out);
        for (gc = c_t->u.gc ; gc ; gc = gc->next)
        {
            _add_to_live_vars (gc->g);
            _generate_live_var_info (gc->s, 0);
            // reverse...
            union_hashtable (H_out, H_live);
            deepcopy_hashtable (H_dup, H_live);
        }
        deepcopy_hashtable (H_out, H_live);
        break;

    case ACT_CHP_SELECT_NONDET:
        fatal_error ("Can't handle NDS");
        
    case ACT_CHP_SKIP:
        break;
    case ACT_CHP_ASSIGN:
    case ACT_CHP_ASSIGNSELF:
        // order important (!!)
        _remove_from_live_vars (c_t->u.assign.id);
        _add_to_live_vars (c_t->u.assign.e);
        _tag_action_with_reqd_vars (c_t);
        break;
        
    case ACT_CHP_RECV:
        _remove_from_live_vars (c_t->u.comm.var);
        _tag_action_with_reqd_vars (c_t);
        break;

    case ACT_CHP_SEND:
        _add_to_live_vars (c_t->u.comm.e);
        _tag_action_with_reqd_vars (c_t);
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

void LiveVarAnalysis::_print_live_var_info (act_chp_lang_t *c_t, int root)
{
    listitem_t *li;
    list_t *copy_list;
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
        if (root == 1)
        {        
            for (li = list_first (c_t->u.semi_comma.cmd); li; li = list_next (li)) 
            {
                stmt = (act_chp_lang_t *)(list_value(li));
                if (stmt->type == ACT_CHP_LOOP || stmt->type == ACT_CHP_DOLOOP) _print_live_var_info (stmt, 1);
            }
            break;
        }
        for (li = list_first (c_t->u.semi_comma.cmd); li; li = list_next (li)) 
        {
            stmt = (act_chp_lang_t *)(list_value(li));
            _print_live_var_info (stmt, 0);
        }
        break;

    case ACT_CHP_LOOP:
    case ACT_CHP_DOLOOP:
        if (root == 1)
        {
            chp_print (fp, c_t);
            _print_var_list(((latch_info_t *)(c_t->space))->live_vars);
            // _print_var_list((list_t *)c_t->space);
            gc = c_t->u.gc;
            _print_live_var_info (gc->s, 0);
            break;
        }
        else
        {
            fatal_error ("should've excised internal loops...");
        }
        break;
        
    case ACT_CHP_SELECT:
        chp_print (fp, c_t);
        _print_var_list(((latch_info_t *)(c_t->space))->live_vars);
        // _print_var_list((list_t *)c_t->space);
        for (gc = c_t->u.gc ; gc ; gc = gc->next)
        {
            _print_live_var_info (gc->s, 0);
        }
        break;

    case ACT_CHP_SELECT_NONDET:
        fatal_error ("Can't handle NDS");
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
        // _print_var_list((list_t *)c_t->space);
        break;

    case ACT_CHP_SEND:
        chp_print (fp, c_t);
        _print_var_list(((latch_info_t *)(c_t->space))->live_vars);
        // _print_var_list((list_t *)c_t->space);
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

void LiveVarAnalysis::_print_var_list (list_t *var_list)
{   
    listitem_t *li;
    fprintf(fp, "\n-----------");
    fprintf(fp, "\nnecessary input transmissions:");
    fprintf(fp, "\n(if ring is broken just before here)\n");
    for (li = list_first(var_list); li; li = list_next(li)) 
    {
        fprintf(fp, "%s, ", (char *)list_value(li));
    }	     
    fprintf(fp, "\n-----------");
    fprintf(fp, "\n\n");
}

void LiveVarAnalysis::print_live_var_info ()
{
    _print_live_var_info (c, 1);
}

void LiveVarAnalysis::generate_live_var_info ()
{
    _generate_live_var_info (c, 1);
}

#if 0
void LiveVarAnalysis::_generate_live_var_bits (act_chp_lang_t *c_t, int root)
{
    listitem_t *li, *li_prev;
    list_t *copy_list;
    act_chp_lang_t *stmt;
    act_chp_gc_t *gc;
    int bits;

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
                if (stmt->type == ACT_CHP_LOOP) _generate_live_var_bits (stmt, 1);
            }
            break;
        }
        li_prev = NULL;
        li = list_first (c_t->u.semi_comma.cmd);
        li_prev = li;
        // start from second stmt
        for (li = list_next (li) ; li ; li = list_next (li)) 
        {
            stmt = (act_chp_lang_t *)(list_value(li));
            _generate_live_var_bits (stmt, 0);
        }
        break;

    case ACT_CHP_LOOP:
    case ACT_CHP_DOLOOP:
        if (root == 1)
        {
            gc = c_t->u.gc;
            _generate_live_var_bits (gc->s, 0);
        }
        else
        {
            fatal_error ("should've excised internal loops...");
        }
        break;
        
    case ACT_CHP_SELECT:
        for (gc = c_t->u.gc ; gc ; gc = gc->next)
        {
            _generate_live_var_bits (gc->s, 0);
            // reverse...
        }
        break;

    case ACT_CHP_SELECT_NONDET:
        fatal_error ("Can't handle NDS");
        
    case ACT_CHP_SKIP:
        break;

    case ACT_CHP_ASSIGN:
    case ACT_CHP_ASSIGNSELF:
    case ACT_CHP_SEND:
    case ACT_CHP_RECV:
        bits = compute_total_bits ((list_t *)c_t->space, p);
        update_tx_bits (bits);
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
#endif