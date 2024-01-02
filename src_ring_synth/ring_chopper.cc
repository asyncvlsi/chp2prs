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

#include "reqs.h"

// Live variable tracking and tagging

Hashtable *H_live = hash_new(0);
Hashtable *H_lcd = hash_new(0);

static int min_tx_bits = std::numeric_limits<int>::max();
static int tx_bits = 0;
/*
    Not used yet.
*/
void update_tx_bits(ActId *id, Process *p, int mode)
{
    Assert ((id), "what 1");
    // TODO: Is this ok?
    InstType *it = id->rootVx(p->CurScope())->t;
    int bw = TypeFactory::bitWidth(it);

    if (mode == 0)
    {
        tx_bits += bw;
    }
    else if (mode == 1)
    {
        tx_bits -= bw;
    }
    else { fatal_error ("shouldn't have gotten here"); }
    if (min_tx_bits >= tx_bits) min_tx_bits = tx_bits;
    Assert ((tx_bits>=0), "what 3");
    return;
    // return;
}

/*
    Not used yet. 
*/
void update_tx_bits(int bits)
{
    if (min_tx_bits >= bits) min_tx_bits = bits;
    return;
}

/*
    Remove a given variable from the live variable hashtable.
*/
void remove_from_live_vars (ActId *id, Process *p)
{
    char tname[1024];
    if (!id) return;
    get_true_name(tname, id, p->CurScope());
    // if (hash_lookup (H_live, id->rootVx(p->CurScope())->getName()))
    if (hash_lookup (H_live, tname))
    {
        hash_delete (H_live, tname);
        // update_tx_bits (id, p, 1);
    }
    return;
}

/*
    Add a given variable to the loop-carried dependency hashtable.
*/
void add_to_live_vars_lcd (ActId *id, Process *p)
{
    char tname[1024];
    get_true_name(tname, id, p->CurScope());
    hash_bucket_t *b;
    // b = hash_lookup(H_lcd, id->rootVx(p->CurScope())->getName());
    b = hash_lookup(H_lcd, tname);
        if (!b)
        {
            hash_add (H_lcd, tname);
            // update_tx_bits(var, p, 0);
        } 
}

/*
    Add a given variable to the live variable hashtable.
*/
void add_to_live_vars (ActId *id, Process *p)
{
    char tname[1024];
    get_true_name(tname, id, p->CurScope());
    hash_bucket_t *b;
    b = hash_lookup(H_live, tname);
        if (!b)
        {
            hash_add (H_live, tname);
            // update_tx_bits(var, p, 0);
        } 
}

/*
    Add all the variables that appear in a given
    expression to the live variable hashtable.
*/
void add_to_live_vars (Expr *e, Process *p)
{
  int id;
  ActId *var;
  hash_bucket_t *b;
  char tname[1024];
//   Assert (e, "Hmm");
  if (!e) return;

#define BINARY_OP					\
  do {							\
    add_to_live_vars (e->u.e.l, p);	\
    add_to_live_vars (e->u.e.r, p);	\
  } while (0)

#define UNARY_OP					\
  do {							\
    add_to_live_vars (e->u.e.l, p);	\
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
    add_to_live_vars (e->u.e.l, p);
    add_to_live_vars (e->u.e.r->u.e.l, p);
    add_to_live_vars (e->u.e.r->u.e.r, p);
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
	add_to_live_vars (tmp->u.e.l, p);
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
        get_true_name(tname, var, p->CurScope());
        b = hash_lookup(H_live, tname);
        if (!b)
        {
            hash_add (H_live, tname);
            // update_tx_bits(var, p, 0);
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

/*
    Print out the current state of the live variable
    hashtable. 
*/
void print_live_vars ()
{
    int i;
    hash_bucket_t *b;

    fprintf(stdout, "\n-----------");
    fprintf(stdout, "\nnecessary input transmissions:");
    for (i=0; i<H_live->size; i++) {
    for (b = H_live->head[i]; b; b = b->next) 
    {
        fprintf(stdout, "\n%s", b->key);
    }	     
    }
    fprintf(stdout, "\n-----------");
    fprintf(stdout, "\n");
}

/*
    Print out the variables in a list of variables.
*/
void print_live_vars (list_t *var_list)
{   
    listitem_t *li;
    fprintf(stdout, "\n-----------");
    fprintf(stdout, "\nnecessary input transmissions:");
    fprintf(stdout, "\n(if ring is broken just before here)");
    for (li = list_first(var_list); li; li = list_next(li)) 
    {
        fprintf(stdout, "\n%s", (char *)list_value(li));
    }	     
    fprintf(stdout, "\n-----------");
    fprintf(stdout, "\n\n");
}

/*
    Compute total number of bits in a list of variables.
*/
int compute_total_bits (list_t *var_list, Process *p)
{
    listitem_t *li;
    ValueIdx *vx;
    int tot_bw = 0;
    for (li = list_first(var_list); li; li = list_next(li)) 
    {   
        vx = p->CurScope()->FullLookupVal((char *)list_value(li));
        tot_bw += TypeFactory::bitWidth(vx->t);
    }	     
    return tot_bw;
}

/*
    Tags the space pointer of every elementary action,
    with variables that are live-in at that action
    i.e. if the ring is broken just before that action,
    the list of variables that are needed as input 
    transmissions for correctness.
*/
void tag_action_with_reqd_vars (act_chp_lang_t *c, Process *p)
{
    int i;
    hash_bucket_t *b;
    char *s;
    list_t *req_vars = list_new();
    for (i=0; i<H_live->size; i++) {
    for (b = H_live->head[i]; b; b = b->next) 
    {
        s = new char;
        strcpy(s, b->key);
        list_append (req_vars, s);
    }	     
    }
    c->space = list_dup(req_vars);
}

/*
    Tags a given CHP tree with the union of the variables that
    are live-out of the action and variables that are loop
    carried dependencies. Only used for tagging selections, 
    since a merging mux is required even if a variable is not
    live-out of the selection, but is live-out of the 
    top-level loop. 
*/
void tag_action_with_reqd_vars_union_lcd (act_chp_lang_t *c, Process *p)
{
    int i;
    hash_bucket_t *b;
    char *s;
    list_t *req_vars = list_new();
    for (i=0; i<H_lcd->size; i++) {
    for (b = H_lcd->head[i]; b; b = b->next) 
    {
        s = new char;
        if (!hash_lookup(H_live, b->key))
        {
            strcpy(s, b->key);
            list_append (req_vars, s);
        }
    }	     
    }
    for (i=0; i<H_live->size; i++) {
    for (b = H_live->head[i]; b; b = b->next) 
    {
        s = new char;
        strcpy(s, b->key);
        list_append (req_vars, s);
    }	     
    }
    c->space = list_dup(req_vars);
}

/*
    Computes the variables that are live-in at every action in a
    given CHP tree, and tags the space pointer with a list of
    those variables. For selections alone, the generated list is
    the list of variables live-out of the selection (merge). This
    is used to generate the correct merging mux when exiting a 
    selection.
*/
void generate_live_var_info (act_chp_lang_t *c, Process *p, int root)
{
    listitem_t *li;
    list_t *copy_list;
    act_chp_lang_t *stmt;
    act_chp_gc_t *gc;
    
    if (!c) return;

    switch (c->type) {
    case ACT_CHP_COMMALOOP:
    case ACT_CHP_SEMILOOP:
        fatal_error ("Replication loops should've been removed..");
        break;
        
    case ACT_CHP_COMMA:
        // has_branches = 1;
        // break;

    case ACT_CHP_SEMI:
        if (root == 1)
        {
            for (li = list_first (c->u.semi_comma.cmd); li; li = list_next (li)) 
            {
                stmt = (act_chp_lang_t *)(list_value(li));
                if (stmt->type == ACT_CHP_ASSIGN) add_to_live_vars_lcd (stmt->u.assign.id, p);
            }
            for (li = list_first (c->u.semi_comma.cmd); li; li = list_next (li)) 
            {
                stmt = (act_chp_lang_t *)(list_value(li));
                if (stmt->type == ACT_CHP_LOOP) generate_live_var_info (stmt, p, 1);
            }
            break;
        }
        else {
            copy_list = list_dup (c->u.semi_comma.cmd);
            list_reverse (copy_list);
            for (li = list_first (copy_list); li; li = list_next (li)) 
            {
                stmt = (act_chp_lang_t *)(list_value(li));
                generate_live_var_info (stmt, p, 0);
            }
        }
        break;

    case ACT_CHP_LOOP:
    case ACT_CHP_DOLOOP:
        if (root == 1)
        {
            gc = c->u.gc;
            generate_live_var_info (gc->s, p, 0);
            break;
        }
        else
        {
            fatal_error ("should've excised internal loops...");
        }
        break;
        
    case ACT_CHP_SELECT:
            // fatal_error ("not handling selections yet");
            // for selections alone, live = live_out of merge
            tag_action_with_reqd_vars_union_lcd (c,p);
            // tag_action_with_reqd_vars (c,p);
            for (gc = c->u.gc ; gc ; gc = gc->next)
            {
                add_to_live_vars (gc->g, p);
                generate_live_var_info (gc->s, p, 0);
                // reverse...
            }
            // gc = c->u.gc;
        break;

    case ACT_CHP_SELECT_NONDET:
        fatal_error ("Can't handle NDS");
        
    case ACT_CHP_SKIP:
        break;
    case ACT_CHP_ASSIGN:
    case ACT_CHP_ASSIGNSELF:
        // order important (!!)
        remove_from_live_vars (c->u.assign.id, p);
        add_to_live_vars (c->u.assign.e, p);
        // print_live_vars();
        tag_action_with_reqd_vars (c,p);
        break;
        
    case ACT_CHP_RECV:
        remove_from_live_vars (c->u.comm.var, p);
        // print_live_vars();
        tag_action_with_reqd_vars (c,p);
        break;

    case ACT_CHP_SEND:
        add_to_live_vars (c->u.comm.e, p);
        // print_live_vars();
        tag_action_with_reqd_vars (c,p);
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

/*
    Prints the live variable information for the given CHP tree.
*/
void print_live_var_info (act_chp_lang_t *c, Process *p, int root)
{
    listitem_t *li;
    list_t *copy_list;
    act_chp_lang_t *stmt;
    act_chp_gc_t *gc;

    if (!c) return;

    switch (c->type) {
    case ACT_CHP_COMMALOOP:
    case ACT_CHP_SEMILOOP:
        fatal_error ("Replication loops should've been removed..");
        break;
        
    case ACT_CHP_COMMA:
        // has_branches = 1;
        // break;

    case ACT_CHP_SEMI:
        if (root == 1)
        {        
            for (li = list_first (c->u.semi_comma.cmd); li; li = list_next (li)) 
            {
                stmt = (act_chp_lang_t *)(list_value(li));
                if (stmt->type == ACT_CHP_LOOP) print_live_var_info (stmt, p, 1);
            }
            break;
        }
        
        for (li = list_first (c->u.semi_comma.cmd); li; li = list_next (li)) 
        {
            stmt = (act_chp_lang_t *)(list_value(li));
            print_live_var_info (stmt, p, 0);
        }
        break;

    case ACT_CHP_LOOP:
    case ACT_CHP_DOLOOP:
        if (root == 1)
        {
            gc = c->u.gc;
            print_live_var_info (gc->s, p, 0);
            break;
        }
        else
        {
            fatal_error ("should've excised internal loops...");
        }
        break;
        
    case ACT_CHP_SELECT:
            chp_print (stdout, c);
            print_live_vars((list_t *)c->space);
            for (gc = c->u.gc ; gc ; gc = gc->next)
            {
                print_live_var_info (gc->s, p, 0);
            }
        break;

    case ACT_CHP_SELECT_NONDET:
        fatal_error ("Can't handle NDS");
        
    case ACT_CHP_SKIP:
        break;
    case ACT_CHP_ASSIGN:
    case ACT_CHP_ASSIGNSELF:
        chp_print (stdout, c);
        print_live_vars((list_t *)c->space);
        break;
        
    case ACT_CHP_RECV:
        chp_print (stdout, c);
        print_live_vars((list_t *)c->space);

        break;

    case ACT_CHP_SEND:
        chp_print (stdout, c);
        print_live_vars((list_t *)c->space);

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

/*
    Not used yet. 
    Compute the total number of bits that are live-in at any
    action. This is achieved by just summing the bitwidths of
    all the variables that are live-in at that action. This
    info is used to determine good points for breaking the ring.
*/
void generate_live_var_bits (act_chp_lang_t *c, Process *p, int root)
{
    listitem_t *li, *li_prev;
    list_t *copy_list;
    act_chp_lang_t *stmt;
    act_chp_gc_t *gc;
    int bits;

    if (!c) return;

    switch (c->type) {
    case ACT_CHP_COMMALOOP:
    case ACT_CHP_SEMILOOP:
        fatal_error ("Replication loops should've been removed..");
        break;
        
    case ACT_CHP_COMMA:
        // has_branches = 1;
        // break;

    case ACT_CHP_SEMI:
        if (root == 1)
        {        
            for (li = list_first (c->u.semi_comma.cmd); li; li = list_next (li)) 
            {
                stmt = (act_chp_lang_t *)(list_value(li));
                if (stmt->type == ACT_CHP_LOOP) generate_live_var_bits (stmt, p, 1);
            }
            break;
        }
        li_prev = NULL;
        li = list_first (c->u.semi_comma.cmd);
        li_prev = li;
        // start from second stmt
        for (li = list_next (li) ; li ; li = list_next (li)) 
        {
            stmt = (act_chp_lang_t *)(list_value(li));
            generate_live_var_bits (stmt, p, 0);
        }
        break;

    case ACT_CHP_LOOP:
    case ACT_CHP_DOLOOP:
        if (root == 1)
        {
            gc = c->u.gc;
            generate_live_var_bits (gc->s, p, 0);
            break;
        }
        else
        {
            fatal_error ("should've excised internal loops...");
        }
        break;
        
    case ACT_CHP_SELECT:
            // fatal_error ("not handling selections yet");

            for (gc = c->u.gc ; gc ; gc = gc->next)
            {
                generate_live_var_bits (gc->s, p, 0);
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
        // chp_print (stdout, c);
        bits = compute_total_bits ((list_t *)c->space, p);
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

/*
    Breaking the ring apart based on live-variable info.
    Tries to break just before variable assignments.
    Not used yet, will probably port this over to 
    CHP-stage processing within chp_optimize.
*/
act_chp_lang_t *chop_ring (act_chp_lang_t *c, Process *p, int n_breaks, int root)
{
    int b = 0;
    listitem_t *li, *li_prev;
    list_t *copy_list;
    act_chp_lang_t *stmt;
    act_chp_lang_t *stmt_break;
    act_chp_lang_t *c_list, *c_loop;
    act_chp_gc_t *gc;
    int tot_bw;

    if (!c) return NULL;

    switch (c->type) {
    case ACT_CHP_COMMALOOP:
    case ACT_CHP_SEMILOOP:
        fatal_error ("Replication loops should've been removed..");
        return NULL;
        break;
        
    case ACT_CHP_COMMA:
        // has_branches = 1;
        // break;

    case ACT_CHP_SEMI:
        li_prev = NULL;
        copy_list = list_dup (c->u.semi_comma.cmd);
        li = list_first (c->u.semi_comma.cmd);
        li_prev = li;
        // start checking for breaks from second stmt
        for (li = list_next(li) ; li; li = list_next (li)) 
        {
            stmt = (act_chp_lang_t *)(list_value(li));
            stmt_break = chop_ring (stmt, p, n_breaks, 0);
            if (stmt_break)
            {
                b = 1;
                copy_list->hd = li;
                if (li_prev) {
                    // fprintf (stdout, "got to tail marker  ");
                    (c->u.semi_comma.cmd)->tl = li_prev;
                    li_prev->next = NULL;
                }
                break;
            }
            li_prev = li;
        }

        NEW (c_list, act_chp_lang_t);
        c_list->type = ACT_CHP_SEMI;
        c_list->u.semi_comma.cmd = copy_list;

        NEW (gc, act_chp_gc_t);
        gc->g = NULL;
        gc->s = c_list;

        NEW (c_loop, act_chp_lang_t);
        c_loop->type = ACT_CHP_LOOP;
        c_loop->u.gc = gc;
        return c_loop;

        break;

    case ACT_CHP_LOOP:
    case ACT_CHP_DOLOOP:
        if (root == 1)
        {
            fprintf (stdout, "\n// min. tx. bits: %d\n", min_tx_bits);
            gc = c->u.gc;
            c_loop = chop_ring (gc->s, p, n_breaks, 0);
            return c_loop;
            break;
        }
        else
        {
            fatal_error ("should've excised internal loops...");
            return NULL;
        }
        break;
        
    case ACT_CHP_SELECT:
            // gc = c->u.gc;
        fatal_error ("selection not supported yet");
        return NULL;
        break;

    case ACT_CHP_SELECT_NONDET:
        fatal_error ("Can't handle NDS");
        
    case ACT_CHP_SKIP:
        return NULL;
        break;
    case ACT_CHP_ASSIGN:
    case ACT_CHP_ASSIGNSELF:
    case ACT_CHP_RECV:
        // suppose : break at assignments
        if (c->space) {
            print_live_vars((list_t *)(c->space));
            // compute min. tx point...
            tot_bw = compute_total_bits((list_t *)(c->space), p);
            if (tot_bw == min_tx_bits)
                {
                    fprintf (stdout, "\n// breakpoint tx. bits: %d\n", tot_bw);
                    return c;
                }
        }
        return NULL;
        break;

    case ACT_CHP_SEND:
        return NULL;
        break;

    case ACT_CHP_FUNC:
    case ACT_CHP_HOLE: /* to support verification */
    case ACT_CHP_MACRO:
    case ACT_HSE_FRAGMENTS:
        return NULL;
        break;

    default:
        fatal_error ("Unknown type");
        return NULL;
        break;
    }

    

}