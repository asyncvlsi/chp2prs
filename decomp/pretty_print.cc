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

#include "pretty_print.h"

static const int indent_inc = 2;
char ib_inc[indent_inc];

void chp_pretty_print (FILE *fp, act_chp_lang_t *c)
{
  memset(ib_inc, ' ', indent_inc);
  _chp_pretty_print (fp, c, 0, 1);
}

static void _chp_pretty_print (FILE *fp, act_chp_lang_t *c, int prec = 0, int indent = 1)
{
  int lprec;
  char *ib;
  
  if (!c) return;
  if (indent < 0) {
    indent = 0;
  }
  MALLOC (ib, char, indent+1);
  memset(ib, ' ', indent);
  ib[indent] = '\0';

  if (c->label) {
    fprintf (fp, "%s:", c->label);
  }
  
  switch (c->type) {
  case ACT_CHP_COMMALOOP:
  case ACT_CHP_SEMILOOP:
    fprintf (fp, "(");
    if (c->type == ACT_CHP_COMMALOOP) {
      fprintf (fp, ",");
    }
    else {
      fprintf (fp, ";");
    }
    fprintf (fp, "%s", c->u.loop.id);
    fprintf (fp, ":");
    print_expr (fp, c->u.loop.lo);
    if (c->u.loop.hi) {
      fprintf (fp, "..");
      print_expr (fp, c->u.loop.hi);
    }
    fprintf (fp, ":");
    _chp_pretty_print (fp, c->u.loop.body);
    fprintf (fp, ")");
    break;
    
  case ACT_CHP_COMMA:
  case ACT_CHP_SEMI:
    {
      listitem_t *li;

      lprec = (c->type == ACT_CHP_SEMI ? 0 : 1);

      if (prec > lprec) {
	fprintf (fp, "(");
      }

      if (c->type == ACT_CHP_SEMI && (list_length(c->u.semi_comma.cmd)>1)) {
            fprintf (fp, "\n%s", ib);
      }

      for (li = list_first (c->u.semi_comma.cmd); li; li = list_next (li)) {
	_chp_pretty_print (fp, (act_chp_lang_t *)list_value (li), lprec, indent+indent_inc);
	if (list_next (li)) {
	  if (c->type == ACT_CHP_COMMA) {
	    fprintf (fp, ",");
	  }
	  else {
	    // fprintf (fp, ";\n%s",ib);
	    fprintf (fp, ";");
      if (c->type == ACT_CHP_SEMI) {
            fprintf (fp, "\n%s", ib);
      }
	  }
	}
      }

      if (prec > lprec) {
	fprintf (fp, ")");
      }

    }
    break;

  case ACT_CHP_LOOP:
  case ACT_CHP_DOLOOP:
    fprintf (fp, "*");
  case ACT_CHP_SELECT:
  case ACT_CHP_SELECT_NONDET:
  if (c->type == ACT_CHP_SELECT || c->type == ACT_CHP_SELECT_NONDET) {
    fprintf (fp, "%s",ib_inc);
  }
    fprintf (fp, "[");
    if (c->type == ACT_CHP_SELECT_NONDET) {
      fprintf (fp, "|");
    }
    {
      act_chp_gc_t *gc = c->u.gc;

      if (c->type == ACT_CHP_DOLOOP) {
	fprintf (fp, " ");
	_chp_pretty_print (fp, gc->s, 0, indent+indent_inc);
	fprintf (fp, " <- ");
	if (gc->g) {
	  print_uexpr (fp, gc->g);
	}
	else {
	  fprintf (fp, "true");
	}
      }
      else {
	while (gc) {
	  if (!gc->g) {
	    if (c->type == ACT_CHP_LOOP) {
	      fprintf (fp, "true");
	    }
	    else {
	      fprintf (fp, "else");
	    }
	  }
	  else {
	    print_uexpr (fp, gc->g);
	  }
	  if (gc->s) {
	    fprintf (fp, " -> ");
	    _chp_pretty_print (fp, gc->s, 0, indent+indent_inc);
	  }
	  if (gc->next) {
	    fprintf (fp, "\n%s[]", ib);
	  }
	  gc = gc->next;
	}
      }
    }
    fprintf (fp, "\n");
    if (c->type == ACT_CHP_SELECT_NONDET) {
      fprintf (fp, "|");
    }
    if (indent>0) {
        fprintf (fp, "%s]",ib);
    }
    else {
        fprintf (fp, "]");
    }
    break;
    
  case ACT_CHP_SKIP:
    fprintf (fp, "skip");
    break;

  case ACT_CHP_ASSIGN:
  case ACT_CHP_ASSIGNSELF:
    c->u.assign.id->Print (fp);
    fprintf (fp, ":=");
    print_uexpr (fp, c->u.assign.e);
    break;
    
  case ACT_CHP_SEND:
    c->u.comm.chan->Print (fp);
    fprintf (fp, "!");
    if (c->u.comm.flavor == 1) {
      fprintf (fp, "+");
    }
    else if (c->u.comm.flavor == 2) {
      fprintf (fp, "-");
    }
    {
      if (c->u.comm.e) {
	print_uexpr (fp, c->u.comm.e);
      }
      if (c->u.comm.var) {
	fprintf (fp, "?");
	if (c->u.comm.flavor == 1) {
	  fprintf (fp, "+");
	}
	else if (c->u.comm.flavor == 2) {
	  fprintf (fp, "-");
	}
	if (c->u.comm.convert == 1) {
	  fprintf (fp, "bool(");
	}
	else if (c->u.comm.convert == 2) {
	  fprintf (fp, "int(");
	}
	c->u.comm.var->Print (fp);
	if (c->u.comm.convert != 0) {
	  fprintf (fp, ")");
	}
      }
    }
    break;
    
  case ACT_CHP_RECV:
    c->u.comm.chan->Print (fp);
    fprintf (fp, "?");
    if (c->u.comm.flavor == 1) {
      fprintf (fp, "+");
    }
    else if (c->u.comm.flavor == 2) {
      fprintf (fp, "-");
    }
    {
      if (c->u.comm.var) {
	if (c->u.comm.convert == 1) {
	  fprintf (fp, "bool(");
	}
	else if (c->u.comm.convert == 2) {
	  fprintf (fp, "int(");
	}
	c->u.comm.var->Print (fp);
	if (c->u.comm.convert != 0) {
	  fprintf (fp, ")");
	}
      }
      if (c->u.comm.e) {
	fprintf (fp, "!");
	if (c->u.comm.flavor == 1) {
	  fprintf (fp, "+");
	}
	else if (c->u.comm.flavor == 2) {
	  fprintf (fp, "-");
	}
	print_uexpr (fp, c->u.comm.e);
      }
    }
    break;

  case ACT_CHP_FUNC:
    fprintf (fp, "%s(", string_char (c->u.func.name));
    for (listitem_t *li = list_first (c->u.func.rhs); li; li = list_next (li)) {
      act_func_arguments_t *a = (act_func_arguments_t *) list_value (li);
      if (li != list_first (c->u.func.rhs)) {
	fprintf (fp, ",");
      }
      if (a->isstring) {
	fprintf (fp, "\"%s\"", string_char (a->u.s));
      }
      else {
	print_uexpr (fp, a->u.e);
      }
    }
    fprintf (fp, ")");
    break;

  case ACT_CHP_HOLE: /* to support verification */
    fprintf (fp, "_");
    break;
    
  case ACT_CHP_MACRO:
    c->u.macro.id->Print (fp);
    fprintf (fp, ".%s(", string_char (c->u.macro.name));
    if (c->u.macro.rhs) {
      listitem_t *li;
      for (li = list_first (c->u.macro.rhs); li; li = list_next (li)) {
	print_expr (fp, (Expr *)list_value (li));
	if (list_next (li)) {
	  fprintf (fp, ",");
	}
      }
    }
    fprintf (fp, ")");
    break;

  case ACT_HSE_FRAGMENTS:
    do {
      _chp_pretty_print (fp, c->u.frag.body);
      if (c->u.frag.nextlabel) {
	fprintf (fp, " : %s", c->u.frag.nextlabel);
      }
      else {
	listitem_t *li;
	fprintf (fp, " : [ ");
	for (li = list_first (c->u.frag.exit_conds); li; li = list_next (li)) {
	  print_expr (fp, (Expr *) list_value (li));
	  fprintf (fp, " -> ");
	  li = list_next (li);
	  fprintf (fp, "%s", (char *) list_value (li));
	  if (list_next (li)) {
	    fprintf (fp, " [] ");
	  }
	}
	fprintf (fp, " ] ");
      }
      c = c->u.frag.next;
      if (c) {
	fprintf (fp, "||\n%s : ", c->label);
      }
    } while (c);
    break; 
    
  default:
    fatal_error ("Unknown type");
    break;
  }
  FREE (ib);
}
