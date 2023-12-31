#include "reqs.h"

/* 
    For every selection that has an else guard,
    replace an else guard with the correct explicit version,
    so that synthesis can happen correctly.
    For example,

     [G1->S1
    []G2->S2
    []G3->S3
    []else->S4
    ]

    gets converted to:

     [G1->S1
    []G2->S2
    []G3->S3
    []~(G1|G2|G3)->S4
    ]

*/
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
        if (root == 1)
        {
            for (li = list_first (c->u.semi_comma.cmd); li; li = list_next (li)) 
            {
                stmt = (act_chp_lang_t *)(list_value(li));
                if (stmt->type == ACT_CHP_LOOP) 
                    fill_in_else_explicit (stmt, p, 1);
            }
        }
        else {
            for (li = list_first (c->u.semi_comma.cmd); li; li = list_next (li)) 
            {
                stmt = (act_chp_lang_t *)(list_value(li));
                fill_in_else_explicit (stmt, p, 0);
            }
        }
        break;

    case ACT_CHP_LOOP:
    case ACT_CHP_DOLOOP:
        if (root == 1)
        {
            gc = c->u.gc;
            fill_in_else_explicit (gc->s, p, 0);
            break;
        }
        else
        {
            fatal_error ("should've excised internal loops...");
        }
        break;
        
    case ACT_CHP_SELECT:
            // _chp_print (stdout, c);
            // fprintf (stdout, "\n\n");

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
        fatal_error ("Can't handle NDS");
        
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
