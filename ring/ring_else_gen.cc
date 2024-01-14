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
                if (stmt->type == ACT_CHP_LOOP || stmt->type == ACT_CHP_DOLOOP) 
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
