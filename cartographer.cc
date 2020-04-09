/*************************************************************************
 *
 *  This file is part of chp2prs
 *
 *  Copyright (c) 2018 Rajit Manohar
 *  Copyright (c) 2018-2019 Zeb Mehring
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
#include <act/types.h>
#include "cartographer.h"
#include <act/iter.h>
#include <act/value.h>


/*
 *
 *  Core syntax-directed translation code written by Rajit Manohar
 *
 *  Extensions to full expressions, optimizations, and direct use of
 *  the ACT library  by Zeb Mehring
 *
 */

static Process *P;

static FILE *output_stream;

static int expr_count = 1;
static int stmt_count = 0;
static int chan_count = 0;
static int gc_chan_count = 0;

static bool bundle_data = false;
static int optimization = 0;

static struct Hashtable *evaluated_exprs;

//#define ACT_LIB_PATH "lib/"
#define ACT_LIB_PATH ""

#define max(a, b) (((a) > (b)) ? (a) : (b))

#define INITIAL_HASH_SIZE 10

#define MAX_EXPR_SIZE 1024
#define MAX_KEY_SIZE 2048
#define MAX_PATH_SIZE 4096

#if 0
/* TODO - reimplement initializations */
void initialize_vars(struct act_chp *c)
{
  hash_bucket_t *b;
  int i;
  symbol *sym;

  if (c == NULL || c->sym == NULL)
    return;
  if (c->sym->n == 0)
    return;

  fprintf(output_stream, "  /* --- declaring all variables and channels --- */\n");

  /* iterate through the symbol hash table */
  for (i = 0; i < c->sym->size; i++)
  {
    for (b = c->sym->head[i]; b; b = b->next)
    {
      sym = (symbol *)b->v;
      /* declare channels */
      if (sym->ischan)
      {
        if (sym->bitwidth > 1)
        {
          fprintf(output_stream, "  aN1of2<%d> chan_%s;\n", sym->bitwidth, sym->name);
        }
        else
        {
          fprintf(output_stream, "  a1of2 chan_%s;\n", sym->name);
        }
      }
      /* declare variables, initialized to false */
      else
      {
        if (sym->bitwidth > 1)
        {
          fprintf(output_stream, "  syn::var_init_false var_%s[%d];\n", sym->name, sym->bitwidth);
        }
        else
        {
          fprintf(output_stream, "  syn::var_init_false var_%s;\n", sym->name);
        }
      }
    }
  }
  fprintf(output_stream, "  /* --- end of declarations --- */\n\n");
}
#endif

void emit_const_1(void)
{
  static int emitted = 0;
  if (!emitted)
  {
    fprintf(output_stream, "  syn::var_init_true const_1;\n");
  }
  emitted = 1;
}

void emit_const_0(void)
{
  static int emitted = 0;
  if (!emitted)
  {
    fprintf(output_stream, "  syn::var_init_false const_0;\n");
  }
  emitted = 1;
}

int get_bitwidth(int n, int base)
{
  int width = n == 0 ? 1 : 0;
  while (n > 0)
  {
    n /= base;
    width++;
  }
  return width;
}

int get_func_bitwidth(char *s)
{
  while (isdigit(*s))
    s--;
  return atoi(s + 1);
}

int get_max_bits(const char *s, int lbits, int rbits)
{
  if (!strcmp(s, "add") || !strcmp(s, "sub"))
  {
    return (lbits > rbits) ? lbits + 1 : rbits + 1;
  }
  else if (!strcmp(s, "mul"))
  {
    return lbits + rbits;
  }
  else if (!strcmp(s, "div"))
  {
    return lbits > rbits ? lbits : rbits;
  }
  else
  {
    fprintf(stderr, "chp2prs: unsupported binary operation\n");
    return -1;
  }
}

/* TODO - choose more accurate delays */
int get_bundle_delay(int n, int type)
{
  switch (type)
  {
  case E_AND:
  case E_OR:
  case E_XOR:
  case E_NOT:
  case E_COMPLEMENT:
    return 2 * n;
  case E_PLUS:
    return 2 * n;
  case E_MINUS:
    return 2 * (2 * n);
  case E_MULT:
    return 2 * (n * n);
  case E_UMINUS:
    return 2 * (2 * n);
  case E_LT:
  case E_GE:
    return 2 * (2 * n);
  case E_LE:
  case E_GT:
    return 2 * (3 * n);
  case E_NE:
  case E_EQ:
    return 2 * (2 * n);
  case E_VAR:
  case E_INT:
    return 0;
  default:
    fprintf(stderr, "chp2prs: unsupported bundled operation: %d\n", type);
    return -1;
  }
}

void get_expr(Expr *e, int v, char *buf)
{
  switch (e->type)
  {
    case E_AND:
    case E_OR:
    case E_XOR:
    case E_NOT:
    case E_COMPLEMENT:
    case E_PLUS:
    case E_MINUS:
    case E_MULT:
    case E_EQ:
    case E_LT:
    case E_GT:
    case E_LE:
    case E_GE:
    case E_NE:
      snprintf(buf, MAX_EXPR_SIZE, "e_%d", v);
      break;
    case E_VAR:
      snprintf(buf, MAX_EXPR_SIZE, "%s", ((ActId *) e->u.e.l)->getName());
      break;
    case E_INT:
      snprintf(buf, MAX_EXPR_SIZE, "%d", e->u.v);
      break;
    default:
      return;
  }
}

void hash_add_expr(struct Hashtable *h, const char *expr)
{
  if (!expr || strlen(expr) == 0)
    return;
  hash_bucket_t *b = hash_add(h, expr);
  b->v = (char *) calloc(MAX_EXPR_SIZE, sizeof(char));
  snprintf((char *) b->v, MAX_EXPR_SIZE, "e_%d", expr_count);
}

void hash_remove_expr(struct Hashtable *h, const char *expr)
{
  for (int i = 0; i < h->size; i++)
    _hash_remove_expr(h, expr, h->head[i]);
}

void _hash_remove_expr(struct Hashtable *h, const char *expr, hash_bucket_t *b)
{
  if (!b)
    return;
  /* recursively search each entry in the chain */
  _hash_remove_expr(h, expr, b->next);
  if (strstr(b->key, expr))
  {
    /* recursively remove all dependent entries */
    hash_remove_expr(h, (char *)b->v);
    free(b->v);
    hash_delete(h, b->key);
  }
}

int hash_get_or_add(struct Hashtable *h, const char *s, Expr *l, Expr *r, int nl, int nr, bool commutative)
{
  hash_bucket_t *b;
  char *left_expr, *right_expr, *k, *_k;
  int ret;

  /* get the string(s) "[e_<expr_count> | <var_name>]" for the expression(s) */
  left_expr = (char *) calloc(MAX_EXPR_SIZE, sizeof(char));
  if (r)
    right_expr = (char *) calloc(MAX_EXPR_SIZE, sizeof(char));
  get_expr(l, nl, left_expr);
  if (r)
    get_expr(r, nr, right_expr);

  /* form the key string from the operation and operand strings */
  k = (char *) calloc(MAX_KEY_SIZE, sizeof(char));
  if (commutative)
    _k = (char *) calloc(MAX_KEY_SIZE, sizeof(char));
  if (r)
  {
    snprintf(k, MAX_KEY_SIZE, "%s(%s,%s)", s, left_expr, right_expr);
    if (commutative)
      snprintf(_k, MAX_KEY_SIZE, "%s(%s,%s)", s, right_expr, left_expr);
  }
  else
  {
    snprintf(k, MAX_KEY_SIZE, "%s(%s)", s, left_expr);
  }

  free(left_expr);
  if (r)
    free(right_expr);

  /* if an entry is in the table, return the integer associated with the value */
  if ((b = hash_lookup(h, k)))
  {
    char *t = (char *)b->v + strlen((char *)b->v) - 1;
    while (isdigit(*t))
      t--;
    ret = atoi(++t);
  }
  /* if the key isn't in the table, add it (and its commutative counterpart) */
  else
  {
    hash_add_expr(h, k);
    if (commutative)
      hash_add_expr(h, _k);
    ret = -1;
  }

  free(k);
  if (commutative)
    free(_k);
  return ret;
}

int unop(const char *s, Expr *e, int *bitwidth, int *base_var, int *delay)
{
  int l = _print_expr(e->u.e.l, bitwidth, base_var, delay);

  /* print ACT module for boolean unary operations */
  if (*bitwidth == 1)
  {
    if (optimization > 0)
    {
      int ret = hash_get_or_add(evaluated_exprs, s, e->u.e.l, NULL, l, -1, false);
      if (ret > 0) return ret;
    }
    fprintf(output_stream, "  syn::expr_%s e_%d;\n", s, expr_count);
    fprintf(output_stream, "  e_%d.in = e_%d.out;\n", expr_count, l);
  }
  /* print ACT module for multi-bit unary operations with the bundle data protocol */
  else if (bundle_data)
  {
    /* accumulate delay */
    *delay += get_bundle_delay(*bitwidth, e->u.e.l->type);
    fprintf(output_stream, "  bundled_%s<%d> be_%d;\n", s, *bitwidth, expr_count);
    fprintf(output_stream, "  (i:%d: be_%d.in[i] = be_%d.out[i];)\n", *bitwidth, expr_count, l);
  }
  /* print delay-insensitive ACT module for multi-bit unary operations */
  else
  {
    if (optimization > 0)
    {
      int ret = hash_get_or_add(evaluated_exprs, s, e->u.e.l, NULL, l, -1, false);
      if (ret > 0) return ret;
    }
    fprintf(output_stream, "  syn::%s<%d> e_%d;\n", s, *bitwidth, expr_count);
    fprintf(output_stream, "  (i:%d: e_%d.in[i] = e_%d.out[i];)\n", *bitwidth, expr_count, l);
  }

  return expr_count++;
}

int binop(const char *s, Expr *e, int *bitwidth, int *base_var, int *delay, bool commutative)
{
  int l = _print_expr(e->u.e.l, bitwidth, base_var, delay);
  int r = _print_expr(e->u.e.r, bitwidth, base_var, delay);

  /* print ACT module for boolean binary operations */
  if (*bitwidth == 1)
  {
    if (optimization > 0)
    {
      int ret = hash_get_or_add(evaluated_exprs, s, e->u.e.l, e->u.e.r, l, r, commutative);
      if (ret > 0) return ret;
    }
    fprintf(output_stream, "  syn::expr_%s e_%d;\n", s, expr_count);
    fprintf(output_stream, "  e_%d.in1 = e_%d.out;\n", expr_count, l);
    fprintf(output_stream, "  e_%d.in2 = e_%d.out;\n", expr_count, r);
  }
  /* print ACT module for multi-bit binary operations with the bundle data protocol */
  else if (bundle_data)
  {
    int dl = get_bundle_delay(*bitwidth, e->u.e.l->type);
    int dr = get_bundle_delay(*bitwidth, e->u.e.r->type);
    /* accumulate the maximum delay of this level */
    *delay += max(dl, dr);
    fprintf(output_stream, "  bundled_%s<%d> be_%d;\n", s, *bitwidth, expr_count);
    fprintf(output_stream, "  (i:%d: be_%d.in1[i] = be_%d.out[i];)\n", *bitwidth, expr_count, l);
    fprintf(output_stream, "  (i:%d: be_%d.in2[i] = be_%d.out[i];)\n", *bitwidth, expr_count, r);
  }
  /* print delay-insensitive ACT module for multi-bit binary operations */
  else
  {
    if (optimization > 0)
    {
      int ret = hash_get_or_add(evaluated_exprs, s, e->u.e.l, e->u.e.r, l, r, commutative);
      if (ret > 0) return ret;
    }
    fprintf(output_stream, "  syn::%s<%d> e_%d;\n", s, *bitwidth, expr_count);
    fprintf(output_stream, "  (i:%d: e_%d.in1[i] = e_%d.out[i];)\n", *bitwidth, expr_count, l);
    fprintf(output_stream, "  (i:%d: e_%d.in2[i] = e_%d.out[i];)\n", *bitwidth, expr_count, r);
  }

  return expr_count++;
}

int _print_expr(Expr *e, int *bitwidth, int *base_var, int *delay)
{
  int ret;
  switch (e->type)
  {
    case E_AND:
      ret = binop("and", e, bitwidth, base_var, delay, true);
      break;
    case E_OR:
      ret = binop("or", e, bitwidth, base_var, delay, true);
      break;
    case E_XOR:
      ret = binop("xor", e, bitwidth, base_var, delay, true);
      break;
    case E_PLUS:
      ret = binop("add", e, bitwidth, base_var, delay, true);
      break;
    case E_MINUS:
      ret = binop("sub", e, bitwidth, base_var, delay, false);
      break;
    case E_MULT:
      ret = binop("mul", e, bitwidth, base_var, delay, true);
      break;
    case E_NOT:
    case E_COMPLEMENT:
      ret = unop("not", e, bitwidth, base_var, delay);
      break;
    case E_UMINUS:
      ret = unop("uminus", e, bitwidth, base_var, delay);
      break;
    case E_EQ:
      ret = binop("eq", e, bitwidth, base_var, delay, true);
      break;
    case E_NE:
      ret = binop("ne", e, bitwidth, base_var, delay, true);
      break;
    case E_LT:
      ret = binop("lt", e, bitwidth, base_var, delay, false);
      break;
    case E_GT:
      ret = binop("gt", e, bitwidth, base_var, delay, false);
      break;
    case E_LE:
      ret = binop("le", e, bitwidth, base_var, delay, false);
      break;
    case E_GE:
      ret = binop("ge", e, bitwidth, base_var, delay, false);
      break;
    case E_VAR:
    {
      InstType *it = P->Lookup((ActId *) e->u.e.l);
      *bitwidth = TypeFactory::bitWidth(it);
      /* TODO - requires modification of "go" signals to avoid deadlock */
      /*
          if (optimization > 0)
          {
            ret = hash_get_or_add (evaluated_exprs, "var", e, NULL, -1, -1, false);
            if (ret > 0) return ret;
          }
          */
      /* receive variable into a boolean latch */
      if (TypeFactory::bitWidth(it) == 1)
      {
        fprintf(output_stream, "  syn::expr_var e_%d;\n", expr_count);
        fprintf(output_stream, "  e_%d.v = var_%s.v;\n", expr_count, ((ActId *) e->u.e.l)->getName());
        /* the current expression becomes the base go signal, if none is set */
        if (*base_var == -1)
        {
          *base_var = expr_count;
        }
        /* otherwise, the current expression is connected to the base go signal */
        else
        {
          fprintf(output_stream, "  /* testpoint 8 */\n");
          fprintf(output_stream, "  e_%d.go_r = e_%d.go_r;\n", expr_count, *base_var); // replaced go.r with go_r 4/8
        }
      }
      /* recieve variable into a mult-bit bundled latch */
      else if (bundle_data)
      {
        fprintf(output_stream, "  bundled_expr_vararray<%d> be_%d;\n", TypeFactory::bitWidth(it), expr_count);
        fprintf(output_stream, "  (i:%d: be_%d.v[i] = var_%s[i].v;)\n", TypeFactory::bitWidth(it), expr_count, ((ActId *) e->u.e.l)->getName());
      }
      /* receive variable into a multi-bit delay-insensitive latch */
      else
      {
        fprintf(output_stream, "  syn::expr_vararray<%d> e_%d;\n", TypeFactory::bitWidth(it), expr_count);
        fprintf(output_stream, "  (i:%d: e_%d.v[i] = var_%s[i].v;)\n", TypeFactory::bitWidth(it), expr_count, ((ActId *) e->u.e.l)->getName());
        /* the current expression becomes the base go signal, if none is set */
        if (*base_var == -1)
        {
          *base_var = expr_count;
        }
        /* otherwise, the current expression is connected to the base go signal */
        else
        {
          fprintf(output_stream, "  e_%d.go_r = e_%d.go_r;\n", expr_count, *base_var);
        }
      }
      ret = expr_count++;
    }
    break;
    case E_INT:
      /* TODO - requires modification of "go" signals to avoid deadlock */
      /*
        if (optimization > 0)
        {
          ret = hash_get_or_add (evaluated_exprs, "int", e, NULL, -1, -1, false);
          if (ret > 0) return ret;
        }
        */
      /* receive a boolean value into a latch */
      if (*bitwidth == 1)
      {
        if (e->u.v == 0)
        {
          emit_const_0();
          fprintf(output_stream, "  syn::expr_var e_%d;\n", expr_count);
          fprintf(output_stream, "  e_%d.v = const_0.v;\n", expr_count);
        }
        else
        {
          emit_const_1();
          fprintf(output_stream, "  syn::expr_var e_%d;\n", expr_count);
          fprintf(output_stream, "  e_%d.v = const_1.v;\n", expr_count);
        }
        /* the current expression becomes the base go signal, if none is set */
        if (*base_var == -1)
        {
          *base_var = expr_count;
        }
        /* otherwise, the current expression is connected to the base go signal */
        else
        {
          fprintf(output_stream, "  /* testpoint 9 */\n");
          fprintf(output_stream, "  e_%d.go_r = e_%d.go_r;\n", expr_count, *base_var); // replaced go.r with go_r 4/8
        }
      }
      /* initialize a multi-bit bundled latch */
      else if (bundle_data)
      {
        emit_const_0();
        emit_const_1();
        fprintf(output_stream, "  bundled_expr_vararray<%d> be_%d;\n", *bitwidth, expr_count);
        /* set each bit individually */
        int t = e->u.v;
        for (int i = *bitwidth; i > 0; i--, t >>= 1)
        {
          if (t & 1)
          {
            fprintf(output_stream, "  be_%d.v[%d] = const_1.v;\n", expr_count, *bitwidth - i);
          }
          else
          {
            fprintf(output_stream, "  be_%d.v[%d] = const_0.v;\n", expr_count, *bitwidth - i);
          }
        }
      }
      /* initialize a multi-bit delay-insensitive latch */
      else
      {
        emit_const_0();
        emit_const_1();
        fprintf(output_stream, "  syn::expr_vararray<%d> e_%d;\n", *bitwidth, expr_count);
        /* set each bit individually */
        int t = e->u.v;
        for (int i = *bitwidth; i > 0; i--, t >>= 1)
        {
          if (t & 1)
          {
            fprintf(output_stream, "  e_%d.v[%d] = const_1.v;\n", expr_count, *bitwidth - i);
          }
          else
          {
            fprintf(output_stream, "  e_%d.v[%d] = const_0.v;\n", expr_count, *bitwidth - i);
          }
        }
        /* the current expression becomes the base go signal, if none is set */
        if (*base_var == -1)
        {
          *base_var = expr_count;
        }
        /* otherwise, the current expression is connected to the base go signal */
        else
        {
          fprintf(output_stream, "  e_%d.go_r = e_%d.go_r;\n", expr_count, *base_var);
        }
      }
      ret = expr_count++;
      break;
    case E_TRUE:
      emit_const_1();
      fprintf(output_stream, "  syn::expr_var e_%d;\n", expr_count);
      fprintf(output_stream, "  e_%d.v = const_1.v;\n", expr_count);
      /* the current expression becomes the base go signal, if none is set */
      if (*base_var == -1)
      {
        *base_var = expr_count;
      }
      /* otherwise, the current expression is connected to the base go signal */
      else
      {
        fprintf(output_stream, "  e_%d.go_r = e_%d.go_r;\n", expr_count, *base_var);
      }
      ret = expr_count++;
      break;
    case E_FALSE:
      emit_const_0();
      fprintf(output_stream, "  syn::expr_var e_%d;\n", expr_count);
      fprintf(output_stream, "  e_%d.v = const_0.v;\n", expr_count);
      /* the current expression becomes the base go signal, if none is set */
      if (*base_var == -1)
      {
        *base_var = expr_count;
      }
      /* otherwise, the current expression is connected to the base go signal */
      else
      {
        fprintf(output_stream, "  e_%d.go_r = e_%d.go_r;\n", expr_count, *base_var);
      }
      ret = expr_count++;
      break;
    case E_PROBE:
    {
      InstType *it = P->Lookup((ActId *) e->u.e.l);
      if (TypeFactory::bitWidth(it) == 1)
      {
        fprintf(output_stream, "  syn::a1of2_probe e_%d;\n", expr_count);
        fprintf(output_stream, "  e_%d.t = chan_%s.t;\n", expr_count, ((ActId *) e->u.e.l)->getName());
        fprintf(output_stream, "  e_%d.f = chan_%s.f;\n", expr_count, ((ActId *) e->u.e.l)->getName());
        fprintf(output_stream, "  e_%d.a = chan_%s.a;\n", expr_count, ((ActId *) e->u.e.l)->getName());
      }
      else
      {
        fprintf(output_stream, "  syn::aN1of2_probe<%d> e_%d;\n", TypeFactory::bitWidth(it), expr_count);
        fprintf(output_stream, "  e_%d.c = chan_%s;\n", expr_count, ((ActId *) e->u.e.l)->getName());
      }
      *bitwidth = TypeFactory::bitWidth(it);
      /* the current expression becomes the base go signal, if none is set */
      if (*base_var == -1)
      {
        *base_var = expr_count;
      }
      /* otherwise, the current expression is connected to the base go signal */
      else
      {
        fprintf(output_stream, "  e_%d.go_r = e_%d.go_r;\n", expr_count, *base_var);
      }
      ret = expr_count++;
    }
    break;
    case E_FUNCTION:
    {
      Expr *tmp;
      *bitwidth = get_func_bitwidth(e->u.fn.s + strlen(e->u.fn.s) - 1);
      fprintf(output_stream, "  bundled_%s e_%d;\n", e->u.fn.s, expr_count);
      tmp = e->u.fn.r;
      while (tmp)
      {
        InstType *it = P->Lookup((ActId *) tmp->u.e.l->u.e.l);
        if (TypeFactory::bitWidth(it) == 1)
        {
          fprintf(output_stream, "  e_%d.%s = var_%s.v;\n", expr_count, ((ActId *) tmp->u.e.l->u.e.l)->getName(), ((ActId *) tmp->u.e.l->u.e.l)->getName());
        }
        else
        {
          fprintf(output_stream, "  (i:%d: e_%d.%s[i] = var_%s[i].v;)\n", TypeFactory::bitWidth(it), expr_count, ((ActId *) tmp->u.e.l->u.e.l)->getName(), ((ActId *) tmp->u.e.l->u.e.l)->getName());
        }
        tmp = tmp->u.e.r;
      }
      /* the current expression becomes the base go signal, if none is set */
      if (*base_var == -1)
      {
        *base_var = expr_count;
      }
      /* otherwise, the current expression is connected to the base go signal */
      else
      {
        fprintf(output_stream, "  /* testpoint 10 */\n");
        fprintf(output_stream, "  e_%d.go_r = e_%d.go.r;\n", expr_count, *base_var);
      }
      ret = expr_count++;
    }
    break;
    default:
      fprintf(stderr, "chp2prs: unknown type %d\n", e->type);
      break;
  }
  return ret;
}

int print_expr(Expr *e, int *bitwidth, int *base_var, int *delay)
{
  *base_var = -1;
  *delay = 0;
  return _print_expr(e, bitwidth, base_var, delay);
}

int print_expr_tmpvar(char *req, int ego, int eout, int bits)
{
  int seq = stmt_count++;
  int evar = expr_count++;

  fprintf(output_stream, "  /* testpoint 1 */\n");
  fprintf(output_stream, "  syn::fullseq s_%d;\n", seq);
  fprintf(output_stream, "  %s = s_%d.go.r;\n", req, seq);

  if (bits == 1)
  {
    fprintf(output_stream, "  syn::recv rtv_%d;\n", seq);
    fprintf(output_stream, "  syn::expr_var e_%d;\n", evar);
    fprintf(output_stream, "  syn::var_init_false tv_%d;\n", seq);
    fprintf(output_stream, "  tv_%d.v = rtv_%d.v;\n", seq, seq);
    fprintf(output_stream, "  e_%d.v = tv_%d.v;\n", evar, seq);
    fprintf(output_stream, "  s_%d.r.r = e_%d.go_r;\n", seq, ego);
    fprintf(output_stream, "  s_%d.r = rtv_%d.go;\n", seq, seq);
    fprintf(output_stream, "  e_%d.out.t = rtv_%d.in.t;\n", eout, seq);
    fprintf(output_stream, "  e_%d.out.f = rtv_%d.in.f;\n", eout, seq);
  }
  else if (bundle_data)
  {
    fprintf(output_stream, "  bundled_recv<%d> brtv_%d;\n", bits, seq);
    fprintf(output_stream, "  syn::expr_vararray<%d> e_%d;\n", bits, evar);
    fprintf(output_stream, "  syn::var_init_false tv_%d[%d];\n", seq, bits);
    fprintf(output_stream, "  (i:%d: e_%d.v[i] = tv_%d[i].v;)\n", bits, evar, seq);
    fprintf(output_stream, "  (i:%d: e_%d.v[i] = brtv_%d.v[i];)\n", bits, evar, seq);
    fprintf(output_stream, "  /* testpoint 2 */\n");
    fprintf(output_stream, "  s_%d.r.r = brtv_%d.go.r;\n", seq, seq);
    fprintf(output_stream, "  s_%d.r.a = brtv_%d.go.a;\n", seq, seq);
    fprintf(output_stream, "  (i:%d: e_%d.out[i].t = brtv_%d.in.d[i].t;\n", bits, eout, seq);
    fprintf(output_stream, "       %*ce_%d.out[i].f = brtv_%d.in.d[i].f;)\n", get_bitwidth(bits, 10), ' ', eout, seq);
  }
  else
  {
    fprintf(output_stream, "  syn::recv rtv_%d[%d];\n", seq, bits);
    fprintf(output_stream, "  syn::expr_vararray<%d> e_%d;\n", bits, evar);
    fprintf(output_stream, "  syn::var_init_false tv_%d[%d];\n", seq, bits);
    fprintf(output_stream, "  (i:%d: e_%d.v[i] = tv_%d[i].v;)\n", bits, evar, seq);
    fprintf(output_stream, "  (i:%d: e_%d.v[i] = rtv_%d[i].v;)\n", bits, evar, seq);
    fprintf(output_stream, "  s_%d.r.r = e_%d.go_r;\n", seq, ego);
    fprintf(output_stream, "  /* testpoint 3 */\n");
    fprintf(output_stream, "  (i:%d: s_%d.r.r = rtv_%d[i].go.r;)\n", bits, seq, seq);
    fprintf(output_stream, "  syn::ctree<%d> ct_%d;\n", bits, seq);
    fprintf(output_stream, "  (i:%d: ct_%d.in[i] = rtv_%d[i].go.a;)\n", bits, seq, seq);
    fprintf(output_stream, "  s_%d.r.a = ct_%d.out;\n", seq, seq);
    fprintf(output_stream, "  (i:%d: e_%d.out[i].t = rtv_%d[i].in.t;\n", bits, eout, seq);
    fprintf(output_stream, "       %*ce_%d.out[i].f = rtv_%d[i].in.f;)\n", get_bitwidth(bits, 10), ' ', eout, seq);
  }
  fprintf(output_stream, "  s_%d.go.a = e_%d.go_r;\n", seq, evar);

  return evar;
}

int print_one_gc(act_chp_gc_t *gc, int *bitwidth, int *base_var)
{
  int a, b;
  int ret = gc_chan_count++;

  fprintf(output_stream, "  r1of2 gc_%d;\n", ret);
  /* guarded statement case */
  if (gc->g)
  {
    int delay;
    char buf[MAX_EXPR_SIZE];
    a = print_expr(gc->g, bitwidth, base_var, &delay);
    snprintf(buf, MAX_EXPR_SIZE, "gc_%d.r", ret);

    if (bundle_data && *bitwidth > 1)
    {
      /* accumulate delay of the last operation */
      delay += get_bundle_delay(*bitwidth, gc->g->type);
      /* add a delay wire for the guard statement */
      if (delay > 0)
      {
        fprintf(output_stream, "  delay<%d> de_%d;\n", delay, a);
        fprintf(output_stream, "  de_%d.in = %s;\n", a, buf);
        snprintf(buf, MAX_EXPR_SIZE, "de_%d.out", a);
      }
      /* add a delay wire to invert the output of the guard statement */
      delay = get_bundle_delay(1, E_NOT);
      fprintf(output_stream, "  delay<%d> dn_%d;\n", delay, a);
      fprintf(output_stream, "  dn_%d.in = %s;\n", a, buf);
      /* receive the guard output into a dualrail node */
      fprintf(output_stream, "  bundled_var_to_dualrail be_%d;\n", expr_count);
      fprintf(output_stream, "  be_%d.d = dn_%d.out;\n", expr_count, a);
      fprintf(output_stream, "  be_%d.in = be_%d.out;\n", expr_count, a);
      fprintf(output_stream, "  syn::expr_var e_%d;\n", expr_count);
      fprintf(output_stream, "  e_%d.v = be_%d.out;\n", expr_count, expr_count);
      snprintf(buf, MAX_EXPR_SIZE, "dn_%d.out", a);
      a = expr_count++;
      *base_var = a;
    }

    /* replace guard output with latched value */
    a = print_expr_tmpvar(buf, *base_var, a, 1);

    /* print guarded statement */
    b = print_chp_stmt(gc->s, bitwidth, base_var);

    /* empty guard */
    if (b == -1)
    {
      fprintf(output_stream, "  gc_%d.t = e_%d.out.t;\n", ret, a);
    }
    /* non-empty guard */
    else
    {
      fprintf(output_stream, "  e_%d.out.t = c_%d.r;\n", a, b);
      fprintf(output_stream, "  gc_%d.t = c_%d.a;\n", ret, b);
    }
    fprintf(output_stream, "  gc_%d.f = e_%d.out.f;\n", ret, a);
  }
  /* unguarded statement case (implicit true guard) */
  else
  {
    b = print_chp_stmt(gc->s, bitwidth, base_var);
    fprintf(output_stream, "  gc_%d.r = c_%d.r;\n", ret, b);
    fprintf(output_stream, "  gc_%d.t = c_%d.a;\n", ret, b);
    fprintf(output_stream, "  gc_%d.f = GND;\n", ret);
  }
  return ret;
}

int print_gc(bool loop, act_chp_gc_t *gc, int *bitwidth, int *base_var)
{
  static int gc_num = 0;
  int na;
  int this_gc = gc_num++;

  fprintf(output_stream, "\n  /* emit individual gc (#%d) [%s] */\n", this_gc, loop ? "loop" : "selection");
  int start_gc_chan = print_one_gc(gc, bitwidth, base_var);
  int end_gc_chan = start_gc_chan;
  gc = gc->next;
  while (gc)
  {
    end_gc_chan = print_one_gc(gc, bitwidth, base_var);
    gc = gc->next;
  }
  int ret = chan_count++;
  fprintf(output_stream, "  a1of1 c_%d;\n", ret);
  fprintf(output_stream, "  /* gc cascade, start = %d, end = %d */\n", start_gc_chan, end_gc_chan);

  /* cascade guards to evaluate serially */
  for (int i = start_gc_chan; i < end_gc_chan; i++)
  {
    fprintf(output_stream, "  gc_%d.f = gc_%d.r;\n", i, i + 1);
  }

  /* connect statment request to first guard */
  na = stmt_count++;
  fprintf(output_stream, "  syn::bool_notand na_%d;\n", na);
  fprintf(output_stream, "  na_%d.in1 = c_%d.r;\n", na, ret);
  fprintf(output_stream, "  na_%d.out = gc_%d.r;\n", na, start_gc_chan);

  /* single guard case */
  if (start_gc_chan == end_gc_chan)
  {
    if (!loop)
    {
      fprintf(output_stream, "  gc_%d.t = c_%d.a;\n", start_gc_chan, ret);
      fprintf(output_stream, "  gc_%d.f = na_%d.in2;\n", start_gc_chan, na);
    }
    else
    {
      fprintf(output_stream, "  gc_%d.t = na_%d.in2;\n", start_gc_chan, na);
      fprintf(output_stream, "  gc_%d.f = c_%d.a;\n", start_gc_chan, ret);
    }
  }
  /* multi-guard case */
  else
  {
    /* construct a multi-stage or gate for guard outputs */
    int a, b;
    a = stmt_count++;
    fprintf(output_stream, "  syn::bool_or or_%d;\n", a);
    fprintf(output_stream, "  or_%d.in1 = gc_%d.t;\n", a, start_gc_chan);
    fprintf(output_stream, "  or_%d.in2 = gc_%d.t;\n", a, start_gc_chan + 1);
    for (int i = start_gc_chan + 2; i < end_gc_chan; i++)
    {
      b = stmt_count++;
      fprintf(output_stream, "  syn::bool_or or_%d;\n", b);
      fprintf(output_stream, "  or_%d.in1 = or_%d.out;\n", b, a);
      fprintf(output_stream, "  or_%d.in2 = gc_%d.t;\n", b, i);
      a = b;
    }

    /* connect multi-stage or gate output to statement acknowledge */
    if (!loop)
    {
      fprintf(output_stream, "  or_%d.out = c_%d.a;\n", a, ret);
      fprintf(output_stream, "  gc_%d.f = na_%d.in2;\n", end_gc_chan, na);
    }
    else
    {
      fprintf(output_stream, "  or_%d.out = na_%d.in2;\n", a, na);
      fprintf(output_stream, "  gc_%d.f = c_%d.a;\n", end_gc_chan, ret);
    }
  }

  fprintf(output_stream, "  /* end of gc (#%d) */\n\n", this_gc);
  return ret;
}

int print_chp_stmt(act_chp_lang_t *c, int *bitwidth, int *base_var)
{
  int ret, a, b, delay;
  InstType *v, *u;
  char buf[MAX_EXPR_SIZE];
  if (!c)
    return -1;
  switch (c->type)
  {
    case ACT_CHP_SKIP:
      fprintf(output_stream, "  /* skip */");
      fprintf(output_stream, "  syn::skip s_%d;\n", stmt_count);
      fprintf(output_stream, "  s_%d.go = c_%d;\n", stmt_count, chan_count);
      stmt_count++;
      ret = chan_count++;
      break;
    case ACT_CHP_ASSIGN:
      fprintf(output_stream, "  /* assign */\n");
      v = P->Lookup(c->u.assign.id);
      *bitwidth = TypeFactory::bitWidth(v);
      /* evaluate the RHS statement */
      a = print_expr(c->u.assign.e, bitwidth, base_var, &delay);
      ret = chan_count++;
      b = stmt_count++;
      /* create request/acknowledge channel for the statement */
      fprintf(output_stream, "  a1of1 c_%d;\n", ret);
      snprintf(buf, MAX_EXPR_SIZE, "c_%d.r", ret);
      if (bundle_data && TypeFactory::bitWidth(v) > 1)
      {
        /* accumulate delay of the last operation */
        delay += get_bundle_delay(*bitwidth, c->u.assign.e->type);
        /* add a delay wire for the RHS statement */
        if (delay > 0)
        {
          fprintf(output_stream, "  delay<%d> de_%d;\n", delay, a);
          fprintf(output_stream, "  de_%d.in = %s;\n", a, buf);
          snprintf(buf, MAX_EXPR_SIZE, "de_%d.out", a);
        }
        /* add a delay wire to invert the output of the RHS statement */
        delay = get_bundle_delay(*bitwidth, E_NOT);
        fprintf(output_stream, "  delay<%d> dn_%d;\n", delay, a);
        fprintf(output_stream, "  dn_%d.in = %s;\n", a, buf);
        /* receive the RHS output into a dualrail node */
        fprintf(output_stream, "  bundled_vararray_to_dualrail<%d> be_%d;\n", TypeFactory::bitWidth(v), expr_count);
        fprintf(output_stream, "  be_%d.d = dn_%d.out;\n", expr_count, a);
        fprintf(output_stream, "  (i:%d: be_%d.in[i] = be_%d.out[i];)\n", TypeFactory::bitWidth(v), expr_count, a);
        fprintf(output_stream, "  syn::expr_vararray<%d> e_%d;\n", TypeFactory::bitWidth(v), expr_count);
        fprintf(output_stream, "  e_%d.go_r = dn_%d.out;\n", expr_count, a);
        fprintf(output_stream, "  (i:%d: e_%d.v[i] = be_%d.out[i];)\n", TypeFactory::bitWidth(v), expr_count, expr_count);
        snprintf(buf, MAX_EXPR_SIZE, "e_%d.go_r", expr_count);
        a = expr_count++;
      }
      /* receive statement output into a latched value */
      a = print_expr_tmpvar(buf, *base_var, a, *bitwidth);
      /* receive latched value into assignment variable */
      if (TypeFactory::bitWidth(v) == 1)
      {
        fprintf(output_stream, "  syn::recv s_%d;\n", b);
        fprintf(output_stream, "  s_%d.go = c_%d;\n", b, ret);
        fprintf(output_stream, "  s_%d.in.t = e_%d.out.t;\n", b, a);
        fprintf(output_stream, "  s_%d.in.f = e_%d.out.f;\n", b, a);
        fprintf(output_stream, "  s_%d.v = var_%s.v;\n", b, c->u.assign.id->getName());
      }
      else if (bundle_data)
      {
        fprintf(output_stream, "  bundled_recv<%d> s_%d;\n", TypeFactory::bitWidth(v), b);
        fprintf(output_stream, "  /* testpoint 4 */\n");
        fprintf(output_stream, "  s_%d.go.r = e_%d.go_r;\n", b, a);
        fprintf(output_stream, "  s_%d.go.a = c_%d.a;\n", b, ret);
        fprintf(output_stream, "  (i:%d: s_%d.in.d[i].t = e_%d.out[i].t;\n", TypeFactory::bitWidth(v), b, a);
        fprintf(output_stream, "       %*cs_%d.in.d[i].f = e_%d.out[i].f;\n", get_bitwidth(TypeFactory::bitWidth(v), 10), ' ', b, a);
        fprintf(output_stream, "       %*cs_%d.v[i] = var_%s[i].v;)\n", get_bitwidth(TypeFactory::bitWidth(v), 10), ' ', b, c->u.assign.id->getName());
      }
      else
      {
        fprintf(output_stream, "  syn::recv s_%d[%d];\n", b, TypeFactory::bitWidth(v));
        fprintf(output_stream, "  /* testpoint 5 */\n");
        fprintf(output_stream, "  (i:%d: s_%d[i].go.r = c_%d.r;)\n", TypeFactory::bitWidth(v), b, ret);
        fprintf(output_stream, "  (i:%d: s_%d[i].in.t = e_%d.out[i].t;\n", TypeFactory::bitWidth(v), b, a);
        fprintf(output_stream, "       %*cs_%d[i].in.f = e_%d.out[i].f;\n", get_bitwidth(TypeFactory::bitWidth(v), 10), ' ', b, a);
        fprintf(output_stream, "       %*cs_%d[i].v = var_%s[i].v;)\n", get_bitwidth(TypeFactory::bitWidth(v), 10), ' ', b, c->u.assign.id->getName());
        fprintf(output_stream, "  syn::ctree<%d> ct_%d;\n", TypeFactory::bitWidth(v), b);
        fprintf(output_stream, "  (i:%d: ct_%d.in[i] = s_%d[i].go.a;)\n", TypeFactory::bitWidth(v), b, b);
        fprintf(output_stream, "  ct_%d.out = c_%d.a;\n", b, ret);
      }
      fprintf(output_stream, "\n");
      /* clear assigned entry from the list of evaluated expressions */
      if (optimization > 0) hash_remove_expr(evaluated_exprs, c->u.assign.id->getName());
      break;
    case ACT_CHP_SEND:
      fprintf(output_stream, "  /* send */\n");
      if (list_length(c->u.comm.rhs) == 1)
      {
        v = P->Lookup(c->u.comm.chan);
        *bitwidth = TypeFactory::bitWidth(v);
        /* evaluate the expression to be sent */
        a = print_expr((Expr *)list_value(list_first(c->u.comm.rhs)), bitwidth, base_var, &delay);
        ret = chan_count++;
        /* create request/acknowledge channel for the statement */
        fprintf(output_stream, "  a1of1 c_%d;\n", ret);
        snprintf(buf, MAX_EXPR_SIZE, "c_%d.r", ret);
        if (bundle_data && TypeFactory::bitWidth(v) > 1)
        {
          /* accumulate delay of the last operation */
          delay += get_bundle_delay(*bitwidth, ((Expr *)list_value(list_first(c->u.comm.rhs)))->type);
          /* add a delay wire for the statement to be sent */
          if (delay > 0)
          {
            fprintf(output_stream, "  delay<%d> de_%d;\n", delay, a);
            fprintf(output_stream, "  de_%d.in = %s;\n", a, buf);
            snprintf(buf, MAX_EXPR_SIZE, "de_%d.out", a);
          }
          /* add a delay wire to invert the output of the statement to be sent */
          delay = get_bundle_delay(*bitwidth, E_NOT);
          fprintf(output_stream, "  delay<%d> dn_%d;\n", delay, a);
          fprintf(output_stream, "  dn_%d.in = %s;\n", a, buf);
          /* receive the statement output into a dualrail node */
          fprintf(output_stream, "  bundled_vararray_to_dualrail<%d> be_%d;\n", TypeFactory::bitWidth(v), expr_count);
          fprintf(output_stream, "  be_%d.d = dn_%d.out;\n", expr_count, a);
          fprintf(output_stream, "  (i:%d: be_%d.in = be_%d.out;)\n", TypeFactory::bitWidth(v), expr_count, a);
          fprintf(output_stream, "  syn::expr_vararray<%d> e_%d;\n", TypeFactory::bitWidth(v), expr_count);
          fprintf(output_stream, "  e_%d.go_r = dn_%d.out;\n", expr_count, a);
          fprintf(output_stream, "  (i:%d: e_%d.v[i] = be_%d.out[i];)\n", TypeFactory::bitWidth(v), expr_count, expr_count);
          snprintf(buf, MAX_EXPR_SIZE, "e_%d.go_r", expr_count);
          a = expr_count++;
        }
        /* receive statement output into a latched value */
        a = print_expr_tmpvar(buf, *base_var, a, *bitwidth);
        fprintf(output_stream, "  c_%d.a = e_%d.go_r;\n", ret, a);
        /* connect latched value to channel */
        if (*bitwidth == 1)
        {
          fprintf(output_stream, "  chan_%s.t = e_%d.out.t;\n", c->u.comm.chan->getName(), a);
          fprintf(output_stream, "  chan_%s.f = e_%d.out.f;\n", c->u.comm.chan->getName(), a);
        }
        else if (bundle_data)
        {
          fprintf(output_stream, "  (i:%d: chan_%s.d[i] = e_%d.out[i];)\n", TypeFactory::bitWidth(v), c->u.comm.chan->getName(), a);
        }
        else
        {
          fprintf(output_stream, "  (i:%d: chan_%s.d[i] = e_%d.out[i];)\n", TypeFactory::bitWidth(v), c->u.comm.chan->getName(), a);
        }
      }
      fprintf(output_stream, "\n");
      break;
    case ACT_CHP_RECV:
      fprintf(output_stream, "  /* recv */\n");
      if (list_length(c->u.comm.rhs) == 1)
      {
        v = P->Lookup(c->u.comm.chan);
        u = P->Lookup((ActId *)list_value(list_first(c->u.comm.rhs)));
        *bitwidth = TypeFactory::bitWidth(v);
        ret = chan_count++;
        a = stmt_count++;
        /* create request/acknowledge channel for the statement */
        fprintf(output_stream, "  a1of1 c_%d;\n", ret);
        /* receive channel value into receiving variable */
        if (TypeFactory::bitWidth(v) == 1)
        {
          fprintf(output_stream, "  syn::recv s_%d;\n", a);
          fprintf(output_stream, "  s_%d.go = c_%d;\n", a, ret);
          fprintf(output_stream, "  s_%d.in = chan_%s;\n", a, c->u.comm.chan->getName());
          fprintf(output_stream, "  s_%d.v = var_%s.v;\n", a, ((ActId *)list_value(list_first(c->u.comm.rhs)))->getName());
        }
        else if (bundle_data)
        {
          fprintf(output_stream, "  bundled_recv<%d> s_%d;\n", TypeFactory::bitWidth(v), a);
          fprintf(output_stream, "  /* testpoint 6 */\n");
          fprintf(output_stream, "  s_%d.go.r = c_%d.r;\n", a, ret);
          fprintf(output_stream, "  s_%d.go.a = c_%d.a; c_%d.a = chan_%s.a;\n", a, ret, ret, c->u.comm.chan->getName());
          fprintf(output_stream, "  (i:%d: s_%d.in.d[i].t = chan_%s.d[i].t;\n", TypeFactory::bitWidth(v), a, c->u.comm.chan->getName());
          fprintf(output_stream, "       %*cs_%d.in.d[i].f = chan_%s.d[i].f;\n", get_bitwidth(TypeFactory::bitWidth(v), 10), ' ', a, c->u.comm.chan->getName());
          fprintf(output_stream, "       %*cs_%d.v[i] = var_%s[i].v;)\n", get_bitwidth(TypeFactory::bitWidth(v), 10), ' ', a, ((ActId *)list_value(list_first(c->u.comm.rhs)))->getName());
        }
        else
        {
          fprintf(output_stream, "  syn::recv s_%d[%d];\n", a, TypeFactory::bitWidth(v));
          fprintf(output_stream, "  /* testpoint 7 */\n");
          fprintf(output_stream, "  (i:%d: s_%d[i].go.r = c_%d.r;)\n", TypeFactory::bitWidth(v), a, ret);
          fprintf(output_stream, "  (i:%d: s_%d[i].in.t = chan_%s.d[i].t;\n", TypeFactory::bitWidth(v), a, c->u.comm.chan->getName());
          fprintf(output_stream, "       %*cs_%d[i].in.f = chan_%s.d[i].f;\n", get_bitwidth(TypeFactory::bitWidth(v), 10), ' ', a, c->u.comm.chan->getName());
          fprintf(output_stream, "       %*cs_%d[i].v = var_%s[i].v;)\n", get_bitwidth(TypeFactory::bitWidth(v), 10), ' ', a, ((ActId *)list_value(list_first(c->u.comm.rhs)))->getName());
          fprintf(output_stream, "  syn::ctree<%d> ct_%d;\n", TypeFactory::bitWidth(v), a);
          fprintf(output_stream, "  (i:%d: ct_%d.in[i] = s_%d[i].go.a;)\n", TypeFactory::bitWidth(v), a, a);
          fprintf(output_stream, "  ct_%d.out = c_%d.a; c_%d.a = chan_%s.a;\n", a, ret, ret, c->u.comm.chan->getName());
        }
        /* clear received entry from the list of evaluated expressions */
        if (optimization > 0) hash_remove_expr(evaluated_exprs, ((ActId *)list_value(list_first(c->u.comm.rhs)))->getName());
      }
      fprintf(output_stream, "\n");
      break;
    case ACT_CHP_COMMA:
    case ACT_CHP_SEMI:
    {
      listitem_t *li;
      /* special case for a single (non-composed) statement, generated by parser */
      if (list_length(c->u.semi_comma.cmd) == 1)
      {
        return print_chp_stmt((act_chp_lang_t *)list_value(list_first(c->u.semi_comma.cmd)), bitwidth, base_var);
      }
      fprintf(output_stream, "  /* %s */\n", c->type == ACT_CHP_COMMA ? "comma" : "semicolon");
      a = chan_count++;
      ret = a;
      /* create request/acknowledge channel for the sequencer */
      fprintf(output_stream, "  a1of1 c_%d;\n", ret);
      fprintf(output_stream, "\n");
      /* iterate through all composite statements */
      for (li = list_first(c->u.semi_comma.cmd); list_next(li); li = list_next(li))
      {
        int s;
        /* print the left statement */
        b = print_chp_stmt((act_chp_lang_t *)list_value(li), bitwidth, base_var);
        s = stmt_count++;
        /* connect the go signal to the statement appropriately */
        fprintf(output_stream, "  syn::%s s_%d;\n", c->type == ACT_CHP_COMMA ? "par" : "seq", s);
        fprintf(output_stream, "  s_%d.go = c_%d;\n", s, a);
        fprintf(output_stream, "  s_%d.s1 = c_%d;\n", s, b);
        /* clear the list of evaluated expressions between sequential statements */
        /* to be removed once deadlock can be avoided */
        if (optimization > 0 && c->type == ACT_CHP_SEMI)
        {
          hash_bucket_t *b;
          for (int j = 0; j < evaluated_exprs->size; j++)
          {
            for (b = evaluated_exprs->head[j]; b; b = b->next)
            {
              free(b->v);
            }
          }
          hash_clear(evaluated_exprs);
        }
        /* on the last loop iteration, print the final statement */
        if (!list_next(list_next(li)))
        {
          fprintf(output_stream, "\n");
          b = print_chp_stmt((act_chp_lang_t *)list_value(list_next(li)), bitwidth, base_var);
          fprintf(output_stream, "  s_%d.s2 = c_%d;\n", s, b);
        }
        /* otherwise, connect the channels appropriately */
        else
        {
          fprintf(output_stream, "  a1of1 c_%d;\n", chan_count);
          fprintf(output_stream, "  s_%d.s2 = c_%d;\n", s, chan_count);
          a = chan_count++;
          fprintf(output_stream, "\n");
        }
      }
      fprintf(output_stream, "\n");
    }
    break;
    case ACT_CHP_LOOP:
    case ACT_CHP_SELECT:
      ret = print_gc((c->type == ACT_CHP_LOOP) ? true : false, c->u.gc, bitwidth, base_var);
      break;
    default:
      fprintf(stderr, "chp2prs: unsupported token: %d\n", c->type);
      exit(1);
  }
  return ret;
}

void generate_act(Process *p, const char *output_file, bool bundled, int opt)
{
  struct act_chp *chp = NULL;
  if (p->lang != NULL && p->lang->getchp() != NULL)
    chp = p->lang->getchp();

  /* set global variables */
  P = p;
  bundle_data = bundled;
  optimization = opt;

  /* initialize the output location */ 
  if (output_file)
  {
    char *output_path = (char *)calloc(MAX_PATH_SIZE, sizeof(char));
    const char *output_dir = "";
    const char *bundled_prefix = bundle_data ? "bundled_" : "";
    snprintf(output_path, MAX_PATH_SIZE, "%s%s%s", output_dir, bundled_prefix, output_file);
    output_stream = fopen(output_path, "w");
    free(output_path);
  }
  else
  {
    output_stream = stdout;
  }
  
  /* TODO - print import statements and wrapper process declaration */
  fprintf(output_stream, "import syn;\n");
  if (bundle_data) fprintf(output_stream, "import bundled.act;\n");
  fprintf(output_stream, "\n");
  fprintf(output_stream, "defproc toplevel (a1of1 go)\n{\n");
  
  /* initialize all variables and channels */
  ActInstiter iter(p->CurScope());
  int bw = 0;
  
  fprintf(output_stream, "/* Initialize chp vars */\n");

  /* iterate through Scope Hashtable to find all chp variables */
  for (iter = iter.begin(); iter != iter.end(); iter++) {
    ValueIdx *vx = *iter;
    if (TypeFactory::isChanType (vx->t)) {
      bw = TypeFactory::bitWidth(vx->t);
      if (bw == 1) {
        fprintf(output_stream, "  aN1of2 chan_%s;\n", vx->getName());
      } else if (bw > 1) {
        fprintf(output_stream, "  aN1of2<%d> chan_%s;\n", bw, vx->getName());
      }
    } else if (TypeFactory::isIntType (vx->t)) {
      bw = TypeFactory::bitWidth(vx->t);
      if (bw == 1) {
        fprintf(output_stream, "  syn::var_init_false var_%s;\n", vx->getName());
      } else if (bw > 1) {
        fprintf(output_stream, "  syn::var_init_false var_%s[%d];\n", vx->getName(), bw);
      }
    }
  }
  fprintf(output_stream, "\n");

  int *bitwidth = (int *) calloc(1, sizeof(int));
  int *base_var = (int *) calloc(1, sizeof(int));
  if (optimization > 0) evaluated_exprs = hash_new(INITIAL_HASH_SIZE);
  
  /* translate the CHP */
  int i = 0;
  if (chp != NULL && chp->c != NULL)
    print_chp_stmt(chp->c, bitwidth, base_var);
  
  if (bitwidth != NULL)
    free(bitwidth);
  if (base_var != NULL)
    free(base_var);
  
  if (optimization > 0)
  {
    /* free allocated memory for buckets still remaining in the table */
    hash_bucket_t *b;
    for (int j = 0; j < evaluated_exprs->size; j++)
    {
      for (b = evaluated_exprs->head[j]; b; b = b->next)
      {
        free(b->v);
      }
    }
    /* free the table itself */
    hash_free(evaluated_exprs);
  }

  /* connect toplevel "go" signal and print wrapper process instantiation */
  fprintf(output_stream, "  go = c_%d;\n", i);
  fprintf(output_stream, "}\n\n");
  fprintf(output_stream, "toplevel t;\n");

  if (output_file) fclose(output_stream);
}
