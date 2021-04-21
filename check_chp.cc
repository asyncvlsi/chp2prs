/*************************************************************************
 *
 *  This file is part of chp2prs
 *
 *  Copyright (c) 2018 Zeb Mehring
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
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <stdbool.h>
#include <act/types.h>
#include "check_chp.h"

static Process *P;

int get_expr_bitwidth (Expr *e)
{
  int left_bitwidth, right_bitwidth, ret;
  InstType *it;
  Expr *tmp;
  char *t;

  switch (e->type)
  {
    case E_AND:
    case E_OR:
    case E_XOR:
    case E_PLUS:
    case E_MINUS:
    case E_MULT:
      left_bitwidth = get_expr_bitwidth (e->u.e.l);
      right_bitwidth = get_expr_bitwidth (e->u.e.r);
      /* if the right expression is an integer, the bitwidth is determined by the left expression */
      if (left_bitwidth == 0)
      {
        return right_bitwidth;
      }
      /* if the left expression is an integer, the bitwidth is determined by the right expression */
      else if (right_bitwidth == 0)
      {
        return left_bitwidth;
      }
      /* do a basic check for division by zero */
      else if ((e->type == E_DIV) && (e->u.e.r->type == E_INT) && (e->u.e.r->u.v == 0))
      {
        fprintf (stderr, "check_chp error: Attempted division by 0\n");
        exit (-1);
      }
      /* if the bitwidths of both expressions match, return that value */
      else
      {
        return left_bitwidth == right_bitwidth ? left_bitwidth : -1;
      }
    case E_NOT:
    case E_COMPLEMENT:
    case E_UMINUS:
      return get_expr_bitwidth (e->u.e.l);
    case E_EQ:
    case E_GT:
    case E_LT:
    case E_NE:
    case E_LE:
    case E_GE:
      left_bitwidth = get_expr_bitwidth (e->u.e.l);
      right_bitwidth = get_expr_bitwidth (e->u.e.r);
      /* comparison operators produce a boolean result */
      return (left_bitwidth == 0) || (right_bitwidth == 0) || (left_bitwidth == right_bitwidth) ? 1 : -1;
    case E_INT:
      /* base case: integers can be arbitrarily expanded/truncated (as in C) */
      return 0;
    case E_VAR:
      it = P->Lookup((ActId *) e->u.e.l);
      /* ensure variable declaration */
      if (!it)
      {
        fprintf (stderr, "check_chp error: Symbol not found: %s\n", ((ActId *) e->u.e.l)->getName());
        exit (-1);
      }
      /* ensure _valid_ variable declaration */
      else if (TypeFactory::isChanType(it))
      {
        fprintf (stderr, "check_chp error: Channel expression used as variable: %s", ((ActId *) e->u.e.l)->getName());
      }
      else if (TypeFactory::bitWidth(it) < 1)
      {
        fprintf (stderr, "check_chp error: Invalid variable bitwidth (%d): %s\n", TypeFactory::bitWidth(it), ((ActId *) e->u.e.l)->getName());
        exit (-1);
      }
      /* base case: bitwidth for an expression is determined by its constituent variables */
      return TypeFactory::bitWidth(it);

  case E_TRUE:
  case E_FALSE:
    return 1;
    break;
    
    case E_PROBE:
      it = P->Lookup((ActId *) e->u.e.l);
      /* ensure the probed channel exists */
      if (!it)
      {
        fprintf (stderr, "check_chp error: Symbol not found: %s\n", ((ActId *) e->u.e.l)->getName());
        exit (-1);
      }
      /* ensure the probed channel is a channel */
      return TypeFactory::isChanType(it) ? 1 : -1;
    case E_FUNCTION:
      /* ensure the function is of the form <name>_<bitwidth> */
      t = e->u.fn.s + strlen (e->u.fn.s) - 1;
      while (t > e->u.fn.s)
      {
        if (!isdigit (*t))
        {
          if (*t != '_')
          {
            fprintf (stderr, "check_chp error: functions must be of the form <name>_<bitwidth>\n");
            exit (1);
          }
          ret = atoi (t+1);
          break;
        }
        t--;
      }
      if (t == e->u.fn.s)
      {
        fprintf (stderr, "check_chp error: functions must be of the form <name>_<bitwidth>\n");
        exit (1);
      }
      /* ensure the funciton only takes variable parameters */
      tmp = e->u.fn.r;
      while (tmp)
      {
        if (tmp->u.e.l->type != E_VAR)
        {
          fprintf (stderr, "check_chp error: functions must take variable parameters\n");
          exit (-1);
        }
        it = P->Lookup((ActId *) tmp->u.e.l->u.e.l);
        if (!it)
        {
          fprintf (stderr, "check_chp error: no such variable '%s'\n", ((ActId *) tmp->u.e.l->u.e.l)->getName());
          exit (1);
        }
        tmp = tmp->u.e.r;
      }
      return ret;
    default:
      fprintf (stderr, "check_chp error: Unsupported token: %d\n", e->type);
      exit (-1);
  }
  return -1;
}

void check (act_chp_lang_t *c)
{
  int expr_width;
  InstType *it;

  switch (c->type)
  {
    case ACT_CHP_ASSIGN:
      it = P->Lookup(c->u.assign.id);
      /* ensure assigned expression exists */
      if (!it)
      {
        fprintf (stderr, "check_chp error: Symbol not found: %s\n", c->u.assign.id->getName());
        exit (-1);
      }
      /* ensure assigned expression is a variable */
      else if (TypeFactory::isChanType(it))
      {
        fprintf (stderr, "check_chp error: Attempted assignment to channel: %s\n", c->u.assign.id->getName());
        exit (-1);
      }
      expr_width = get_expr_bitwidth (c->u.assign.e);
      /* ensure correct sub-expressions on the RHS */
      if (expr_width == -1)
      {
        fprintf (stderr, "check_chp error: Expression operands have incompatible bit widths\n");
        exit (-1);
      }
      /* ensure assigned variable and assignment expression have identical bitwidths */
      else if (expr_width != TypeFactory::bitWidth(it) && expr_width != 0)
      {
        fprintf (stderr, "check_chp error: Assignment variable (%s) and expression have incompatible bit widths\n", c->u.assign.id->getName());
        exit (-1);
      }
      break;
    case ACT_CHP_SEND:
    case ACT_CHP_RECV:
      it = P->Lookup(c->u.comm.chan);
      /* ensure channel exists */
      if (!it)
      {
        fprintf (stderr, "check_chp error: Channel not found: %s\n", c->u.comm.chan->getName());
        exit (-1);
      }
      /* ensure channel is a channel */
      else if (!TypeFactory::isChanType(it))
      {
        fprintf (stderr, "check_chp error: Channel expression expected: %s\n", c->u.comm.chan->getName());
        exit (-1);
      }
      else
      {
	if (c->u.comm.e) {
	  expr_width = get_expr_bitwidth (c->u.comm.e);
	  /* ensure correct sub-expressions in the send buffer */
	  if (expr_width == -1)
            {
              fprintf (stderr, "check_chp error: Expression operands have incompatible bit widths\n");
              exit (-1);
            }
            /* ensure the send buffer and channel have the same width */
	  else if (expr_width != TypeFactory::bitWidth(it) && expr_width != 0)
            {
              fprintf (stderr, "check_chp error: Channel and expression have incompatible bit widths\n");
              exit (-1);
            }
	}
	if (c->u.comm.var) {
	  ActId *rec_var = c->u.comm.var;
	  InstType *lit = P->Lookup(rec_var);
	  /* ensure valid receiving variable */
	  if (!lit)
            {
	      fprintf (stderr, "check_chp error: Variable %s not found\n", rec_var->getName());
	      exit (1);
            }
	  /* ensure channel bitwidth is equal to the variable bitwidth */
	  if (TypeFactory::bitWidth(lit) != TypeFactory::bitWidth(it))
            {
              fprintf (stderr, "check_chp error: Receiving variable has insufficient width: %s\n", rec_var->getName());
              exit (-1);
            }
	}
      }
      break;
    case ACT_CHP_SEMI:
    case ACT_CHP_COMMA:
      {
        listitem_t *li;
        for (li = list_first (c->u.semi_comma.cmd); li; li = list_next (li))
        {
          act_chp_lang_t *cmd = (act_chp_lang_t *) list_value (li);
          check (cmd);
        }
      }
      break;
    case ACT_CHP_SELECT:
    case ACT_CHP_LOOP:
      {
        act_chp_gc_t *gc = c->u.gc;
        if (!gc)
        {
          fprintf (stderr, "check_chp error: Empty loop/selection statment\n");
          exit (-1);
        }
        while (gc)
        {
          /* ensure guards are boolean-valued */
          if (gc->g && (get_expr_bitwidth (gc->g) != 1))
          {
            fprintf (stderr, "check_chp error: Boolean guard expected\n");
            exit (-1);
          }
          /* recursively check guarded statements */
          if (gc->s)
          {
            check (gc->s);
          }
          gc = gc->next;
        }
      }
      break;
    }
  return;
}

void check_chp(Process *p)
{
  P = p;
  check(p->getlang()->getchp()->c);
}
