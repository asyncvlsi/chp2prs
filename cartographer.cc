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
#include <act/chp-opt/optimize.h>
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
static bool chpoptimize = false;

static struct Hashtable *evaluated_exprs;
static struct Hashtable *chan_sends;
static struct Hashtable *current_chan_sends;

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
          fprintf(output_stream, "  aN1of2<%d> %s;\n", sym->bitwidth, sym->name);
        }
        else
        {
          fprintf(output_stream, "  a1of2 %s;\n", sym->name);
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
    exit(EXIT_FAILURE);
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
    exit(EXIT_FAILURE);
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

/* create merge for channels with multiple sends */
void merge_chan_sends(hash_bucket_t * curr, int bw) {
  int it;
  hash_bucket_t *b;
//  printf("merge chans for %s, i=%d, bw=%d\n", curr->key, curr->i, bw);
  
  b = hash_lookup(chan_sends, curr->key);
  
  if (curr->i == 0)
  {
//    fprintf(output_stream, "  (i:%d: %s_%d.c[i] = %s;\n", TypeFactory::bitwidth(v), k, bkt->i);
    fprintf(output_stream, "  aN1of2<%d> %s_chans[%d];\n", bw, curr->key, b->i);
    return;
  }
  else if (curr->i != (b->i - 1))
  {
//    printf("not time to merge yet...\n");
    return;
  }

//  printf("time to merge!\n");
  fprintf(output_stream, "  syn::merge_chans<%d, %d> m_%s;\n", b->i, bw, b->key);
  fprintf(output_stream, "  (i:%d: m_%s.c[i] = %s_chans[i];)\n", b->i, b->key, b->key);
  fprintf(output_stream, "  m_%s.out = %s;\n", b->key, b->key);

}

int unop(const char *s, Expr *e, int *bitwidth, int *base_var, int *delay)
{
  int l = _print_expr(e->u.e.l, bitwidth, base_var, delay);

  /* print ACT module for boolean unary operations */
  if (*bitwidth == 1 && !bundle_data)
  {
    if (optimization > 0)
    {
      int ret = hash_get_or_add(evaluated_exprs, s, e->u.e.l, NULL, l, -1, false);
      if (ret > 0) return ret;
    }
//    printf("TEST1: potential problem=expr_%s\n", s);
    fprintf(output_stream, "  syn::expr_%s e_%d;\n", s, expr_count);
    fprintf(output_stream, "  e_%d.in = e_%d.out;\n", expr_count, l);
  }
  else if (*bitwidth == 1)
  {
    /* accumulate delay */
    *delay += get_bundle_delay(*bitwidth, e->u.e.l->type);
    fprintf(output_stream, "  bundled::%s be_%d;\n", s, expr_count);
    fprintf(output_stream, "  be_%d.in = be_%d.out;\n", expr_count, l);
  }
  /* print ACT module for multi-bit unary operations with the bundle data protocol */
  else if (bundle_data)
  {
    /* accumulate delay */
    *delay += get_bundle_delay(*bitwidth, e->u.e.l->type);
    fprintf(output_stream, "  bundled::%s<%d> be_%d;\n", s, *bitwidth, expr_count);
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

int binop(const char *s, Expr *e, int *bitwidth, int *base_var, int *delay, bool commutative, bool alwayssize)
{
  int l = _print_expr(e->u.e.l, bitwidth, base_var, delay);
  int r = _print_expr(e->u.e.r, bitwidth, base_var, delay);

  /* Handle expr_ with only template<N> declarations */
  if (alwayssize && *bitwidth == 1 && !bundle_data)
  {
    if (optimization > 0)
    {
      int ret = hash_get_or_add(evaluated_exprs, s, e->u.e.l, e->u.e.r, l, r, commutative);
      if (ret > 0) return ret;
    }
//    printf("TEST2: potential problem=%s\n", s);
    fprintf(output_stream, "  syn::%s<1> e_%d;\n", s, expr_count); // changed expr_%s to %s
    fprintf(output_stream, "  e_%d.in1 = {e_%d.out};\n", expr_count, l);
    fprintf(output_stream, "  e_%d.in2 = {e_%d.out};\n", expr_count, r);
  }
  else if (alwayssize && *bitwidth == 1)
  {
    fprintf(output_stream, "  bundled::%s<1> be_%d;\n", s, expr_count); // changed expr_%s to %s
    fprintf(output_stream, "  be_%d.in1 = {be_%d.out};\n", expr_count, l);
    fprintf(output_stream, "  be_%d.in2 = {be_%d.out};\n", expr_count, r);
  }
  /* print ACT module for boolean binary operations */
  else if (*bitwidth == 1 && !bundle_data)
  {
    if (optimization > 0)
    {
      int ret = hash_get_or_add(evaluated_exprs, s, e->u.e.l, e->u.e.r, l, r, commutative);
      if (ret > 0) return ret;
    }
//    printf("TEST3: potential problem=%s\n", s);
    fprintf(output_stream, "  syn::%s e_%d;\n", s, expr_count); // changed expr_%s to %s
    fprintf(output_stream, "  e_%d.in1 = e_%d.out;\n", expr_count, l);
    fprintf(output_stream, "  e_%d.in2 = e_%d.out;\n", expr_count, r);
  }
  else if (*bitwidth == 1) {
    fprintf(output_stream, "  bundled::%s be_%d;\n", s, expr_count);
    fprintf(output_stream, "  be_%d.in1 = be_%d.out;\n", expr_count, l);
    fprintf(output_stream, "  be_%d.in2 = be_%d.out;\n", expr_count, r);
  }
  /* print ACT module for multi-bit binary operations with the bundle data protocol */
  else if (bundle_data)
  {
    int dl = get_bundle_delay(*bitwidth, e->u.e.l->type);
    int dr = get_bundle_delay(*bitwidth, e->u.e.r->type);
    /* accumulate the maximum delay of this level */
    *delay += max(dl, dr);
    fprintf(output_stream, "  bundled::%s<%d> be_%d;\n", s, *bitwidth, expr_count);
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
//    printf("TEST4: potential problem=%s\n", s);
    fprintf(output_stream, "  syn::%s<%d> e_%d;\n", s, *bitwidth, expr_count); // left as %s
    fprintf(output_stream, "  (i:%d: e_%d.in1[i] = e_%d.out[i];)\n", *bitwidth, expr_count, l);
    fprintf(output_stream, "  (i:%d: e_%d.in2[i] = e_%d.out[i];)\n", *bitwidth, expr_count, r);
  }

  return expr_count++;
}

int _print_expr(Expr *e, int *bitwidth, int *base_var, int *delay)
{
  int ret;
  fprintf(output_stream, "  /* e->type=%d */\n", e->type);
  switch (e->type)
  {
    case E_AND:
      if (*bitwidth == 1)
      {
        ret = binop("expr_and", e, bitwidth, base_var, delay, true, false);
      } else {
        ret = binop("and", e, bitwidth, base_var, delay, true, false);
      }
      break;
    case E_OR:
      if (*bitwidth == 1)
      {
        ret = binop("expr_or", e, bitwidth, base_var, delay, true, false);
      } else {
        ret = binop("or", e, bitwidth, base_var, delay, true, false);
      }
      break;
    case E_XOR:
      ret = binop("expr_xor", e, bitwidth, base_var, delay, true, false);
      break;
    case E_PLUS:
      ret = binop("add", e, bitwidth, base_var, delay, true, false);
      break;
    case E_MINUS:
      ret = binop("sub", e, bitwidth, base_var, delay, false, false);
      break;
    case E_MULT:
      ret = binop("mul", e, bitwidth, base_var, delay, true, false);
      break;
    case E_NOT:
    case E_COMPLEMENT:
      ret = unop("expr_not", e, bitwidth, base_var, delay);
      break;
    case E_UMINUS:
      ret = unop("uminus", e, bitwidth, base_var, delay);
      break;
    case E_EQ:
      ret = binop("expr_eq", e, bitwidth, base_var, delay, true, true);
      *bitwidth = 1;
      break;
    case E_NE:
      ret = binop("expr_ne", e, bitwidth, base_var, delay, true, true);
      *bitwidth = 1;
      break;
    case E_LT:
      ret = binop("expr_lt", e, bitwidth, base_var, delay, false, true);
      *bitwidth = 1;
      break;
    case E_GT:
      ret = binop("expr_gt", e, bitwidth, base_var, delay, false, true);
      *bitwidth = 1;
      break;
    case E_LE:
      ret = binop("expr_le", e, bitwidth, base_var, delay, false, true);
      *bitwidth = 1;
      break;
    case E_GE:
      ret = binop("expr_ge", e, bitwidth, base_var, delay, false, true);
      *bitwidth = 1;
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
      if (TypeFactory::bitWidth(it) == 1 && !bundle_data)
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
          fprintf(output_stream, "  e_%d.go_r = e_%d.go_r;\n", expr_count, *base_var); // replaced go.r with go_r 4/8
        }
      }
      else if (TypeFactory::bitWidth(it) == 1)
      {
        fprintf(output_stream, "  bundled::expr_var be_%d;\n", expr_count);
        fprintf(output_stream, "  be_%d.v = var_%s.v;\n", expr_count, ((ActId *) e->u.e.l)->getName());
      }
      /* recieve variable into a mult-bit bundled latch */
      else if (bundle_data)
      {
        fprintf(output_stream, "  bundled::expr_vararray<%d> be_%d;\n", TypeFactory::bitWidth(it), expr_count);
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
      if (*bitwidth == 1 && !bundle_data)
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
          fprintf(output_stream, "  e_%d.go_r = e_%d.go_r;\n", expr_count, *base_var); // replaced go.r with go_r 4/8
        }
      }
      else if (*bitwidth == 1)
      {
        if (e->u.v == 0)
        {
          emit_const_0();
          fprintf(output_stream, "  bundled::expr_var be_%d;\n", expr_count);
          fprintf(output_stream, "  be_%d.v = const_0.v;\n", expr_count);
        }
        else
        {
          emit_const_1();
          fprintf(output_stream, "  bundled::expr_var be_%d;\n", expr_count);
          fprintf(output_stream, "  be_%d.v = const_1.v;\n", expr_count);
        }
      }
      /* initialize a multi-bit bundled latch */
      else if (bundle_data)
      {
        emit_const_0();
        emit_const_1();
        fprintf(output_stream, "  bundled::expr_vararray<%d> be_%d;\n", *bitwidth, expr_count);
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
        if (*base_var == -1)
        {
          *base_var = expr_count;
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
        fprintf(output_stream, "  e_%d.t = %s.t;\n", expr_count, ((ActId *) e->u.e.l)->getName());
        fprintf(output_stream, "  e_%d.f = %s.f;\n", expr_count, ((ActId *) e->u.e.l)->getName());
        fprintf(output_stream, "  e_%d.a = %s.a;\n", expr_count, ((ActId *) e->u.e.l)->getName());
      }
      else
      {
        fprintf(output_stream, "  syn::aN1of2_probe<%d> e_%d;\n", TypeFactory::bitWidth(it), expr_count);
        fprintf(output_stream, "  e_%d.c = %s;\n", expr_count, ((ActId *) e->u.e.l)->getName());
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
      fprintf(output_stream, "  bundled::%s e_%d;\n", e->u.fn.s, expr_count);
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
//  printf("initial print expr type=%d bw=%d\n", e->type, *bitwidth);
  return _print_expr(e, bitwidth, base_var, delay);
//  printf("final print expr type=%d bw=%d\n", e->type, *bitwidth);
}

int print_expr_tmpvar(char *req, int ego, int eout, int bits, int multi_to_one_bit_expr)
{
  int seq = stmt_count++;
  int evar = expr_count++;

  fprintf(output_stream, "  /* testpoint 1, bits=%d  seq=%d  ego=%d*/\n", bits, seq, ego);
  if (!chpoptimize)
  {
    fprintf(output_stream, "  syn::fullseq s_%d;\n", seq);
    fprintf(output_stream, "  %s = s_%d.go.r;\n", req, seq);
  }

  if (bits == 1)
  {
    fprintf(output_stream, "  syn::recv rtv_%d;\n", seq);
    fprintf(output_stream, "  syn::expr_var e_%d;\n", evar);
    fprintf(output_stream, "  syn::var_init_false tv_%d;\n", seq);
    fprintf(output_stream, "  tv_%d.v = rtv_%d.v;\n", seq, seq);
    fprintf(output_stream, "  e_%d.v = tv_%d.v;\n", evar, seq);
    
    if (chpoptimize){
      fprintf(output_stream, "  %s = e_%d.go_r;\n", req, ego);
      fprintf(output_stream, "  %s = rtv_%d.go.r;\n", req, seq);
      fprintf(output_stream, "  rtv_%d.go.a = e_%d.go_r;\n", seq, evar);
    } else {
      if (!bundle_data)
        fprintf(output_stream, "  s_%d.r.r = e_%d.go_r;\n", seq, ego);
      fprintf(output_stream, "  s_%d.r = rtv_%d.go;\n", seq, seq);
      fprintf(output_stream, "  s_%d.go.a = e_%d.go_r;\n", seq, evar);
    }
    if (bundle_data)
    {
      fprintf(output_stream, "  be_%d.out = rtv_%d.in.t;\n", eout, seq);
          fprintf(output_stream, "  bundled::expr_not n_%d;\n", seq);
      fprintf(output_stream, "  n_%d.in = be_%d.out;\n", seq, eout);
      fprintf(output_stream, "  n_%d.out = rtv_%d.in.f;\n", seq, seq);
    }
    else
    {
      fprintf(output_stream, "  e_%d.out.t = rtv_%d.in.t;\n", eout, seq);
      fprintf(output_stream, "  e_%d.out.f = rtv_%d.in.f;\n", eout, seq);
    }
  }
  else if (bundle_data)
  {
    fprintf(output_stream, "  bundled::recv<%d> brtv_%d;\n", bits, seq);
    fprintf(output_stream, "  syn::expr_vararray<%d> e_%d;\n", bits, evar);
    fprintf(output_stream, "  syn::var_init_false tv_%d[%d];\n", seq, bits);
    fprintf(output_stream, "  (i:%d: e_%d.v[i] = tv_%d[i].v;)\n", bits, evar, seq);
    fprintf(output_stream, "  (i:%d: e_%d.v[i] = brtv_%d.v[i];)\n", bits, evar, seq);
    fprintf(output_stream, "  s_%d.r.r = brtv_%d.go.r;\n", seq, seq);
    fprintf(output_stream, "  s_%d.r.a = brtv_%d.go.a;\n", seq, seq);
    fprintf(output_stream, "  (i:%d: e_%d.out[i].t = brtv_%d.in.d[i].t;\n", bits, eout, seq);
    fprintf(output_stream, "       %*ce_%d.out[i].f = brtv_%d.in.d[i].f;)\n", get_bitwidth(bits, 10), ' ', eout, seq);
    if (chpoptimize)
    {
      fprintf(output_stream, "  brtv_%d.go.a = e_%d.go_r;\n", seq, evar);
    } else {
      fprintf(output_stream, "  s_%d.go.a = e_%d.go_r;\n", seq, evar);
    }
  }
  else
  {
    fprintf(output_stream, "  syn::recv rtv_%d[%d];\n", seq, bits);
    fprintf(output_stream, "  syn::expr_vararray<%d> e_%d;\n", bits, evar);
    fprintf(output_stream, "  syn::var_init_false tv_%d[%d];\n", seq, bits);
    fprintf(output_stream, "  (i:%d: e_%d.v[i] = tv_%d[i].v;)\n", bits, evar, seq);
    fprintf(output_stream, "  (i:%d: e_%d.v[i] = rtv_%d[i].v;)\n", bits, evar, seq);
    
    if (chpoptimize)
    {
      fprintf(output_stream, "  %s = e_%d.go_r;\n", req, ego);
      fprintf(output_stream, "  (i:%d: %s = rtv_%d[i].go.r;)\n", bits, req, seq);
    } else {
      fprintf(output_stream, "  s_%d.r.r = e_%d.go_r;\n", seq, ego);
      fprintf(output_stream, "  (i:%d: s_%d.r.r = rtv_%d[i].go.r;)\n", bits, seq, seq);
    }
    
    fprintf(output_stream, "  syn::ctree<%d> ct_%d;\n", bits, seq);
    fprintf(output_stream, "  (i:%d: ct_%d.in[i] = rtv_%d[i].go.a;)\n", bits, seq, seq);
    
    if (chpoptimize)
    {
      fprintf(output_stream, "  ct_%d.out = e_%d.go_r;\n", seq, evar);
    } else {
      fprintf(output_stream, "  s_%d.r.a = ct_%d.out;\n", seq, seq);
      fprintf(output_stream, "  s_%d.go.a = e_%d.go_r;\n", seq, evar);
    }
    
    fprintf(output_stream, "  (i:%d: e_%d.out[i].t = rtv_%d[i].in.t;\n", bits, eout, seq);
    fprintf(output_stream, "       %*ce_%d.out[i].f = rtv_%d[i].in.f;)\n", get_bitwidth(bits, 10), ' ', eout, seq);
  }

  return evar;
}

int print_one_gc(act_chp_gc_t *gc, int *bitwidth, int *base_var)
{
  int a, b;
  int ret = gc_chan_count++;
  int init_basevar = *base_var;
  fprintf(output_stream, "  r1of2 gc_%d;\n", ret);
  /* guarded statement case */
  if (gc->g)
  {
    int multi_to_one_bit_expr = (gc->g->type == E_EQ || gc->g->type == E_NE || gc->g->type == E_LT || gc->g->type == E_GT || gc->g->type == E_LE || gc->g->type == E_GE);
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
        fprintf(output_stream, "  bundled::delay<%d> de_%d;\n", delay, a);
        fprintf(output_stream, "  de_%d.in = %s;\n", a, buf);
        snprintf(buf, MAX_EXPR_SIZE, "de_%d.out", a);
      }
      /* add a delay wire to invert the output of the guard statement */
      delay = get_bundle_delay(1, E_NOT);
      fprintf(output_stream, "  bundled::delay<%d> dn_%d;\n", delay, a);
      fprintf(output_stream, "  dn_%d.in = %s;\n", a, buf);
      /* receive the guard output into a dualrail node */
      fprintf(output_stream, "  bundled::var_to_dualrail be_%d;\n", expr_count);
      fprintf(output_stream, "  be_%d.d = dn_%d.out;\n", expr_count, a);
      fprintf(output_stream, "  be_%d.in = be_%d.out;\n", expr_count, a);
      fprintf(output_stream, "  syn::expr_var e_%d;\n", expr_count);
      fprintf(output_stream, "  e_%d.v = be_%d.out;\n", expr_count, expr_count);
      snprintf(buf, MAX_EXPR_SIZE, "dn_%d.out", a);
      a = expr_count++;
      *base_var = a;
    }

    /* replace guard output with latched value */
    a = print_expr_tmpvar(buf, *base_var, a, 1, multi_to_one_bit_expr);

    /* print guarded statement */
    b = print_chp_stmt(gc->s, bitwidth, base_var, -1, -1);

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
    b = print_chp_stmt(gc->s, bitwidth, base_var, -1, -1);
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
    fprintf(output_stream, "  /* cascade gc: type=%d */\n", gc->s->type);
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
//    printf("... ... start_gc_chan=%d, sgc+2=%d, end_gc_chan=%d\n", start_gc_chan, start_gc_chan+2, end_gc_chan);
    fprintf(output_stream, "  syn::bool_or or_%d;\n", a);
    fprintf(output_stream, "  or_%d.in1 = gc_%d.t;\n", a, start_gc_chan);
    fprintf(output_stream, "  or_%d.in2 = gc_%d.t;\n", a, start_gc_chan + 1);
    for (int i = start_gc_chan + 2; i <= end_gc_chan; i++)
    {
//      printf("... ... in sgc+2 loop!\n");
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

int act_chp_assign(act_chp_lang_t *c, int *bitwidth, int *base_var, int need_sequencer, int seq_num)
{
  int ret, a, b, delay, left_bits;
  InstType *v, *u;
  char buf[MAX_EXPR_SIZE];
    
  fprintf(output_stream, "  /* assign */\n");
  v = P->Lookup(c->u.assign.id);
  *bitwidth = TypeFactory::bitWidth(v);
  left_bits = TypeFactory::bitWidth(v);
  
  /* evaluate the RHS statement */
  a = print_expr(c->u.assign.e, bitwidth, base_var, &delay);
  ret = chan_count++;
  b = stmt_count++;
  
  /* create request/acknowledge channel for the statement */
  if (need_sequencer >=0)
  {
    seq_num++;
    fprintf(output_stream, "  /* YES we need a sequencer in this assignment */\n");
    fprintf(output_stream, "  a1of1 c_%d;\n", ret);
    fprintf(output_stream, "  syn::fullseq fs_%d;\n", seq_num);
    fprintf(output_stream, "  c_%d = fs_%d.go;\n", ret, seq_num);
    snprintf(buf, MAX_EXPR_SIZE, "fs_%d.r.r", seq_num);
  } else {
    fprintf(output_stream, "  a1of1 c_%d;\n", ret);
    snprintf(buf, MAX_EXPR_SIZE, "c_%d.r", ret);
  }
  
  if (bundle_data && left_bits > 1)
  {
    /* accumulate delay of the last operation */
    delay += get_bundle_delay(*bitwidth, c->u.assign.e->type);
    /* add a delay wire for the RHS statement */
    if (delay > 0)
    {
      fprintf(output_stream, "  bundled::delay<%d> de_%d;\n", delay, a);
      fprintf(output_stream, "  de_%d.in = %s;\n", a, buf);
      snprintf(buf, MAX_EXPR_SIZE, "de_%d.out", a);
    }
    /* add a delay wire to invert the output of the RHS statement */
    delay = get_bundle_delay(*bitwidth, E_NOT);
    fprintf(output_stream, "  bundled::delay<%d> dn_%d;\n", delay, a);
    fprintf(output_stream, "  dn_%d.in = %s;\n", a, buf);
    /* receive the RHS output into a dualrail node */
    fprintf(output_stream, "  bundled::vararray_to_dualrail<%d> be_%d;\n", TypeFactory::bitWidth(v), expr_count);
    fprintf(output_stream, "  be_%d.d = dn_%d.out;\n", expr_count, a);
    fprintf(output_stream, "  (i:%d: be_%d.in[i] = be_%d.out[i];)\n", TypeFactory::bitWidth(v), expr_count, a);
    fprintf(output_stream, "  syn::expr_vararray<%d> e_%d;\n", TypeFactory::bitWidth(v), expr_count);
    fprintf(output_stream, "  e_%d.go_r = dn_%d.out;\n", expr_count, a);
    fprintf(output_stream, "  (i:%d: e_%d.v[i] = be_%d.out[i];)\n", TypeFactory::bitWidth(v), expr_count, expr_count);
    snprintf(buf, MAX_EXPR_SIZE, "e_%d.go_r", expr_count);
    a = expr_count++;
  }
  /* receive statement output into a latched value */
  a = print_expr_tmpvar(buf, *base_var, a, left_bits, 0);
  /* receive latched value into assignment variable */
  if (left_bits == 1)
  {
    fprintf(output_stream, "  syn::recv s_%d;\n", b);
    fprintf(output_stream, "  s_%d.go = c_%d;\n", b, ret);
    fprintf(output_stream, "  s_%d.in.t = e_%d.out.t;\n", b, a);
    fprintf(output_stream, "  s_%d.in.f = e_%d.out.f;\n", b, a);
    fprintf(output_stream, "  s_%d.v = var_%s.v;\n", b, c->u.assign.id->getName());
  }
  else if (bundle_data)
  {
    fprintf(output_stream, "  bundled::recv<%d> s_%d;\n", TypeFactory::bitWidth(v), b);
    fprintf(output_stream, "  s_%d.go.r = e_%d.go_r;\n", b, a);
    fprintf(output_stream, "  s_%d.go.a = c_%d.a;\n", b, ret);
    fprintf(output_stream, "  (i:%d: s_%d.in.d[i].t = e_%d.out[i].t;\n", TypeFactory::bitWidth(v), b, a);
    fprintf(output_stream, "       %*cs_%d.in.d[i].f = e_%d.out[i].f;\n", get_bitwidth(TypeFactory::bitWidth(v), 10), ' ', b, a);
    fprintf(output_stream, "       %*cs_%d.v[i] = var_%s[i].v;)\n", get_bitwidth(TypeFactory::bitWidth(v), 10), ' ', b, c->u.assign.id->getName());
  }
  else
  {
    fprintf(output_stream, "  syn::recv s_%d[%d];\n", b, TypeFactory::bitWidth(v));
    if (need_sequencer >=0)
    {
      fprintf(output_stream, "  (i:%d: s_%d[i].go.r = fs_%d.r.r;)\n", TypeFactory::bitWidth(v), b, seq_num);
    } else {
      fprintf(output_stream, "  (i:%d: s_%d[i].go.r = c_%d.r;)\n", TypeFactory::bitWidth(v), b, ret);
    }
    
    fprintf(output_stream, "  (i:%d: s_%d[i].in.t = e_%d.out[i].t;\n", TypeFactory::bitWidth(v), b, a);
    fprintf(output_stream, "       %*cs_%d[i].in.f = e_%d.out[i].f;\n", get_bitwidth(TypeFactory::bitWidth(v), 10), ' ', b, a);
    fprintf(output_stream, "       %*cs_%d[i].v = var_%s[i].v;)\n", get_bitwidth(TypeFactory::bitWidth(v), 10), ' ', b, c->u.assign.id->getName());
    fprintf(output_stream, "  syn::ctree<%d> ct_%d;\n", TypeFactory::bitWidth(v), b);
    fprintf(output_stream, "  (i:%d: ct_%d.in[i] = s_%d[i].go.a;)\n", TypeFactory::bitWidth(v), b, b);
    if (need_sequencer >=0)
    {
      fprintf(output_stream, "  ct_%d.out = fs_%d.r.a;\n", b, seq_num);
    } else {
      fprintf(output_stream, "  ct_%d.out = c_%d.a;\n", b, ret);
    }
  }
  fprintf(output_stream, "\n");
  /* clear assigned entry from the list of evaluated expressions */
  if (optimization > 0) hash_remove_expr(evaluated_exprs, c->u.assign.id->getName());
  
  return ret;
}

int act_chp_send(act_chp_lang_t *c, int *bitwidth, int *base_var, int need_sequencer, int seq_num)
{
  int ret, a, b, delay;
  InstType *v, *u;
  char buf[MAX_EXPR_SIZE], chan_name[MAX_EXPR_SIZE];
  
  hash_bucket_t *bkt;
  char * k = (char *) calloc(MAX_KEY_SIZE, sizeof(char));
  snprintf(k, MAX_KEY_SIZE, "%s", c->u.comm.chan->getName());
  bkt = hash_lookup(current_chan_sends, k);
  
  if (chan_sends == NULL)
  {
    fprintf(stderr, "ERROR: no chan HASH TABLE\n");
  }
  if (!bkt)
  {
    printf("chan %s has only 1 send\n", k);
  } else {
    printf("chan %s is @ index %d\n", k, bkt->i);
  }

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
        fprintf(output_stream, "  bundled::delay<%d> de_%d;\n", delay, a);
        fprintf(output_stream, "  de_%d.in = %s;\n", a, buf);
        snprintf(buf, MAX_EXPR_SIZE, "de_%d.out", a);
      }
      /* add a delay wire to invert the output of the statement to be sent */
      delay = get_bundle_delay(*bitwidth, E_NOT);
      fprintf(output_stream, "  bundled::delay<%d> dn_%d;\n", delay, a);
      fprintf(output_stream, "  dn_%d.in = %s;\n", a, buf);
      /* receive the statement output into a dualrail node */
      fprintf(output_stream, "  bundled::vararray_to_dualrail<%d> be_%d;\n", TypeFactory::bitWidth(v), expr_count);
      fprintf(output_stream, "  be_%d.d = dn_%d.out;\n", expr_count, a);
      fprintf(output_stream, "  (i:%d: be_%d.in = be_%d.out;)\n", TypeFactory::bitWidth(v), expr_count, a);
      fprintf(output_stream, "  syn::expr_vararray<%d> e_%d;\n", TypeFactory::bitWidth(v), expr_count);
      fprintf(output_stream, "  e_%d.go_r = dn_%d.out;\n", expr_count, a);
      fprintf(output_stream, "  (i:%d: e_%d.v[i] = be_%d.out[i];)\n", TypeFactory::bitWidth(v), expr_count, expr_count);
      snprintf(buf, MAX_EXPR_SIZE, "e_%d.go_r", expr_count);
      a = expr_count++;
    }
    /* receive statement output into a latched value */
    a = print_expr_tmpvar(buf, *base_var, a, *bitwidth, 0);
    fprintf(output_stream, "  c_%d.a = e_%d.go_r;\n", ret, a);
    
    /* get chan name depending on # of channel sends */
    if (bkt == NULL)
    {
      snprintf(chan_name, MAX_EXPR_SIZE, "%s", k);
    }
    /* declare multiple channels and their merge */
    else
    {
      snprintf(chan_name, MAX_EXPR_SIZE, "%s_chans[%d]", k,  bkt->i);
      printf("CALLING MERGE CHAN SENDS\n");
      merge_chan_sends(bkt, TypeFactory::bitWidth(v));
      bkt->i = bkt->i + 1;
    }
    
    /* connect latched value to channel */
    if (*bitwidth == 1 && !bundle_data)
    {
      fprintf(output_stream, "  %s.t = e_%d.out.t;\n", chan_name, a);
      fprintf(output_stream, "  %s.f = e_%d.out.f;\n", chan_name, a);
    }
    else if (*bitwidth == 1)
    {
      fprintf(output_stream, "  %s.d = e_%d.out;)\n", chan_name, a);
    }
    else if (bundle_data)
    {
      fprintf(output_stream, "  (i:%d: %s.d[i] = e_%d.out[i];)\n", TypeFactory::bitWidth(v), chan_name, a);
    }
    else
    {
      fprintf(output_stream, "  (i:%d: %s.d[i] = e_%d.out[i];)\n", TypeFactory::bitWidth(v), chan_name, a);
    }
  }
  fprintf(output_stream, "\n");
  free(k);
  return ret;
}

int act_chp_recv(act_chp_lang_t *c, int *bitwidth, int *base_var, int need_sequencer, int seq_num)
{
  int ret, a, b, delay;
  InstType *v, *u;
  char buf[MAX_EXPR_SIZE], chan_name[MAX_EXPR_SIZE];
  char * k = (char *) calloc(MAX_KEY_SIZE, sizeof(char));
  snprintf(k, MAX_KEY_SIZE, "%s", c->u.comm.chan->getName());
  hash_bucket_t *bkt = hash_lookup(current_chan_sends, k);

  if (bkt == NULL) {
    snprintf(chan_name, MAX_EXPR_SIZE, "%s", k);
  } else {
    snprintf(chan_name, MAX_EXPR_SIZE, "%s_chans[%d]", k,  bkt->i);
  }
  
//  printf("CHAN NAME: %s ... k=%s\n", chan_name, k);
  
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
      fprintf(output_stream, "  s_%d.in = %s;\n", a, chan_name);
      fprintf(output_stream, "  s_%d.v = var_%s.v;\n", a, ((ActId *)list_value(list_first(c->u.comm.rhs)))->getName());
    }
    else if (bundle_data)
    {
      fprintf(output_stream, "  bundled::recv<%d> s_%d;\n", TypeFactory::bitWidth(v), a);
      fprintf(output_stream, "  s_%d.go.r = c_%d.r;\n", a, ret);
      fprintf(output_stream, "  s_%d.go.a = c_%d.a; c_%d.a = %s.a;\n", a, ret, ret, c->u.comm.chan->getName());
      fprintf(output_stream, "  (i:%d: s_%d.in.d[i].t = %s.d[i].t;\n", TypeFactory::bitWidth(v), a, chan_name);
      fprintf(output_stream, "       %*cs_%d.in.d[i].f = %s.d[i].f;\n", get_bitwidth(TypeFactory::bitWidth(v), 10), ' ', a, chan_name);
      fprintf(output_stream, "       %*cs_%d.v[i] = var_%s[i].v;)\n", get_bitwidth(TypeFactory::bitWidth(v), 10), ' ', a, ((ActId *)list_value(list_first(c->u.comm.rhs)))->getName());
    }
    else
    {
      fprintf(output_stream, "  syn::recv s_%d[%d];\n", a, TypeFactory::bitWidth(v));
      fprintf(output_stream, "  (i:%d: s_%d[i].go.r = c_%d.r;)\n", TypeFactory::bitWidth(v), a, ret);
      fprintf(output_stream, "  (i:%d: s_%d[i].in.t = %s.d[i].t;\n", TypeFactory::bitWidth(v), a, chan_name);
      fprintf(output_stream, "       %*cs_%d[i].in.f = %s.d[i].f;\n", get_bitwidth(TypeFactory::bitWidth(v), 10), ' ', a, chan_name);
      fprintf(output_stream, "       %*cs_%d[i].v = var_%s[i].v;)\n", get_bitwidth(TypeFactory::bitWidth(v), 10), ' ', a, ((ActId *)list_value(list_first(c->u.comm.rhs)))->getName());
      fprintf(output_stream, "  syn::ctree<%d> ct_%d;\n", TypeFactory::bitWidth(v), a);
      fprintf(output_stream, "  (i:%d: ct_%d.in[i] = s_%d[i].go.a;)\n", TypeFactory::bitWidth(v), a, a);
      fprintf(output_stream, "  ct_%d.out = c_%d.a; c_%d.a = %s.a;\n", a, ret, ret, c->u.comm.chan->getName());
    }
    /* clear received entry from the list of evaluated expressions */
    if (optimization > 0) hash_remove_expr(evaluated_exprs, ((ActId *)list_value(list_first(c->u.comm.rhs)))->getName());
  }
  fprintf(output_stream, "\n");
  return ret;
}

int act_chp_semicomma(act_chp_lang_t *c, int *bitwidth, int *base_var, int need_sequencer, int seq_num)
{
  int ret, a, b, delay;
  InstType *v, *u;
  char buf[MAX_EXPR_SIZE];
  
  /* check for sequencer if chpoptimize being used */
  if (chpoptimize && need_sequencer < 0 && seq_num < 0 && c != NULL && c->space != NULL && ((SequencerInfo *)c->space) != NULL && &(((SequencerInfo *)c->space)->sequence) != NULL) {
    fprintf(output_stream, "  /* YES we need a sequencer here, ns=%d, seq_num=%d */\n", chan_count, stmt_count+1);
    int s = stmt_count++;
    need_sequencer = chan_count;
    seq_num = s;
  }
  
  listitem_t *li;
  /* special case for a single (non-composed) statement, generated by parser */
  if (list_length(c->u.semi_comma.cmd) == 1)
  {
    return print_chp_stmt((act_chp_lang_t *)list_value(list_first(c->u.semi_comma.cmd)), bitwidth, base_var, need_sequencer, seq_num);
  }
  fprintf(output_stream, "  /* %s */\n", c->type == ACT_CHP_COMMA ? "comma" : "semicolon");
  a = chan_count++;
  ret = a;
  /* create request/acknowledge channel for the sequencer */
  if (need_sequencer >= 0) {
    seq_num++;
    fprintf(output_stream, "  a1of1 c_%d;\n", ret);
    fprintf(output_stream, "  syn::fullseq fs_%d;\n", seq_num);
    fprintf(output_stream, "  c_%d = fs_%d.go;\n", ret, seq_num);
  } else {
    fprintf(output_stream, "  a1of1 c_%d;\n", ret); // no sequencer needed
  }
  
  /* iterate through all composite statements */
  for (li = list_first(c->u.semi_comma.cmd); list_next(li); li = list_next(li))
  {
    int s;
    /* print the left statement */
    b = print_chp_stmt((act_chp_lang_t *)list_value(li), bitwidth, base_var, -1, -1);
    s = stmt_count++;
    /* connect the go signal to the statement appropriately */
    fprintf(output_stream, "  syn::%s s_%d;\n", c->type == ACT_CHP_COMMA ? "par" : "seq", s);
    
    if (need_sequencer >= 0 && a == need_sequencer) {
      fprintf(output_stream, "  s_%d.go = fs_%d.r;\n", s, seq_num);
    } else {
      fprintf(output_stream, "  s_%d.go = c_%d;\n", s, a);
    }
    
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
      b = print_chp_stmt((act_chp_lang_t *)list_value(list_next(li)), bitwidth, base_var, -1, -1);
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
  
  return ret;
}

int act_chp_doloop(act_chp_gc_t *gc, int *bitwidth, int *base_var, int need_sequencer, int seq_num) {
  int a, b, na;
  int this_gc = gc_chan_count++;
  int ret = chan_count++;
  fprintf(output_stream, "\n  /* start do loop w/ base_var=%d */\n", *base_var);
  
  /* create request/acknowledge channel for do loop w/ or w/o sequencer */
  if (need_sequencer >= 0)
  {
    seq_num++;
    fprintf(output_stream, "  a1of1 c_%d;\n", ret);
    fprintf(output_stream, "  syn::fullseq fs_%d;\n", seq_num);
    fprintf(output_stream, "  c_%d = fs_%d.go;\n", ret, seq_num);
  } else {
    fprintf(output_stream, "  a1of1 c_%d;\n", ret); // no sequencer needed
  }
  
  /* print statment -- do loop initiates statement at least once */
  b = print_chp_stmt(gc->s, bitwidth, base_var, -1, -1);
  fprintf(output_stream, "  /* do loop: b for stmt1 = %d */\n", b);
  fprintf(output_stream, "  r1of2 gc_%d;\n", this_gc);
  
  /* print expr for the guard */
  if (gc->g)
  {
    int multi_to_one_bit_expr = (gc->g->type == E_EQ || gc->g->type == E_NE || gc->g->type == E_LT || gc->g->type == E_GT || gc->g->type == E_LE || gc->g->type == E_GE);
    int delay;
    char buf[MAX_EXPR_SIZE];
    a = print_expr(gc->g, bitwidth, base_var, &delay);
    snprintf(buf, MAX_EXPR_SIZE, "gc_%d.r", this_gc);

    if (bundle_data && *bitwidth > 1)
    {
      /* accumulate delay of the last operation */
      delay += get_bundle_delay(*bitwidth, gc->g->type);
      /* add a delay wire for the guard statement */
      if (delay > 0)
      {
        fprintf(output_stream, "  bundled::delay<%d> de_%d;\n", delay, a);
        fprintf(output_stream, "  de_%d.in = %s;\n", a, buf);
        snprintf(buf, MAX_EXPR_SIZE, "de_%d.out", a);
      }
      /* add a delay wire to invert the output of the guard statement */
      delay = get_bundle_delay(1, E_NOT);
      fprintf(output_stream, "  bundled::delay<%d> dn_%d;\n", delay, a);
      fprintf(output_stream, "  dn_%d.in = %s;\n", a, buf);
      /* receive the guard output into a dualrail node */
      fprintf(output_stream, "  bundled::var_to_dualrail be_%d;\n", expr_count);
      fprintf(output_stream, "  be_%d.d = dn_%d.out;\n", expr_count, a);
      fprintf(output_stream, "  be_%d.in = be_%d.out;\n", expr_count, a);
      fprintf(output_stream, "  syn::expr_var e_%d;\n", expr_count);
      fprintf(output_stream, "  e_%d.v = be_%d.out;\n", expr_count, expr_count);
      snprintf(buf, MAX_EXPR_SIZE, "dn_%d.out", a);
      a = expr_count++;
      *base_var = a;
    }

    /* replace guard output with latched value */
    a = print_expr_tmpvar(buf, *base_var, a, 1, 0);

    /* connect truthfulness of expr to guard true rail*/
    fprintf(output_stream, "  gc_%d.t = e_%d.out.t;\n", this_gc, a);
    fprintf(output_stream, "  gc_%d.f = e_%d.out.f;\n", this_gc, a);
  }
  /* if no guard: implicit true */
  else
  {
    b = print_chp_stmt(gc->s, bitwidth, base_var, -1, -1);
    fprintf(output_stream, "  gc_%d.r = c_%d.a;\n", this_gc, b);
    fprintf(output_stream, "  gc_%d.t = c_%d.r;\n", this_gc, b);
    fprintf(output_stream, "  gc_%d.f = GND;\n", this_gc);
  }
  
  /* connect statment to guard with notand to allow resetting of loop */
  fprintf(output_stream, "  /* do loop cntd... */\n", b);
  na = stmt_count++;
  fprintf(output_stream, "  c_%d.a = gc_%d.r;\n", b, this_gc);
  fprintf(output_stream, "  syn::bool_notand na_%d;\n", na);
  fprintf(output_stream, "  na_%d.out = c_%d.r;\n", na, b);
  fprintf(output_stream, "  na_%d.in2 = gc_%d.t;\n", na, this_gc);

  if (need_sequencer >= 0)
  {
    fprintf(output_stream, "  na_%d.in1 = fs_%d.r.r;\n", na, seq_num);
    fprintf(output_stream, "  gc_%d.f = fs_%d.r.a;\n", this_gc, seq_num);
  }
  else
  {
    fprintf(output_stream, "  na_%d.in1 = c_%d.r;\n", na, ret);
    fprintf(output_stream, "  gc_%d.f = c_%d.a;\n", this_gc, ret);
  }
  
  fprintf(output_stream, "  /* end do loop: ret = %d */\n", ret);
  return ret;
}

/* Recursively called fn to handle different chp statement types */
int print_chp_stmt(act_chp_lang_t *c, int *bitwidth, int *base_var, int need_sequencer, int seq_num)
{
  int ret, a, b, delay;
  InstType *v, *u;
  char buf[MAX_EXPR_SIZE];
    
  if (!c)
    return -1;
  
  fprintf(output_stream, "  /* type=%d */\n", c->type);

  switch (c->type)
  {
    case ACT_CHP_SKIP:
      fprintf(output_stream, "  /* skip */\n");
      fprintf(output_stream, "  a1of1 c_%d;\n", chan_count);
      fprintf(output_stream, "  syn::syn_skip s_%d;\n", stmt_count);
      fprintf(output_stream, "  s_%d.go = c_%d;\n", stmt_count, chan_count);
      stmt_count++;
      ret = chan_count++;
      break;
    case ACT_CHP_ASSIGN:
      ret = act_chp_assign(c, bitwidth, base_var, need_sequencer, seq_num);
      break;
    case ACT_CHP_SEND:
      ret = act_chp_send(c, bitwidth, base_var, need_sequencer, seq_num);
      break;
    case ACT_CHP_RECV:
      ret = act_chp_recv(c, bitwidth, base_var, need_sequencer, seq_num);
      break;
    case ACT_CHP_COMMA:
    case ACT_CHP_SEMI:
      ret = act_chp_semicomma(c, bitwidth, base_var, need_sequencer, seq_num);
      break;
    case ACT_CHP_LOOP:
    case ACT_CHP_SELECT:
      ret = print_gc((c->type == ACT_CHP_LOOP) ? true : false, c->u.gc, bitwidth, base_var);
      break;
    case ACT_CHP_DOLOOP:
//      printf("Do loop in the works.");
      ret = act_chp_doloop(c->u.gc, bitwidth, base_var, need_sequencer, seq_num);
      break;
    default:
      fprintf(stderr, "chp2prs: unsupported token: %d\n", c->type);
      exit(EXIT_FAILURE);
  }
  return ret;
}

/* Print proc definition & override CHP variables */
bool write_process_definition(Process * p, const char * proc_name)
{
  bool has_overrides = 0;
  fprintf(output_stream, "defproc sdt_%s <: %s (a1of1 go)\n", proc_name, proc_name);

  /* iterate through Scope Hashtable to find all chp variables */
  ActInstiter iter(p->CurScope());
  int bw = 0;
  for (iter = iter.begin(); iter != iter.end(); iter++)
  {
    ValueIdx *vx = *iter;
    bw = TypeFactory::bitWidth(vx->t);
    /* chan variable found */
    if (TypeFactory::isChanType (vx->t))
    {
     if (!has_overrides)
     {
       fprintf(output_stream, "+{\n");
       has_overrides = true;
     }
     if (bw == 1)
     {
       fprintf(output_stream, "  aN1of2<1> %s;\n", vx->getName());
     }
     else if (bw > 1)
     {
       fprintf(output_stream, "  aN1of2<%d> %s;\n", bw, vx->getName());
     }
    }
    
    /* int variable found */
    if (TypeFactory::isIntType (vx->t))
    {
      /* chp-optimize creates sel0, sel1,... & loop0, loop1, ... which do not have dualrail overrides */
      if (!chpoptimize || (strncmp(vx->getName(), "sel", 3) != 0 &&                               strncmp(vx->getName(), "loop", 5) != 0))
      {
        if (!has_overrides)
        {
          fprintf(output_stream, "+{\n");
          has_overrides = true;
        }
        if (bw == 1)
        {
          fprintf(output_stream, "  dualrail %s;\n", vx->getName());
        } else {
          fprintf(output_stream, "  dualrails<%d> %s;\n", bw, vx->getName());
        }
      }
    }
  }
  /* end param declaration */
  if (has_overrides)
  {
    fprintf(output_stream, "}\n{\n");
  } else {
    fprintf(output_stream, "{\n");
  }
  return has_overrides;
}

/* Initialize var_init_false vars for each CHP int */
void initialize_chp_ints(Process * p, bool has_overrides)
{
  /* iterate through Scope Hashtable to find all chp ints */
  fprintf(output_stream, "  /* Initialize chp vars */\n");
  ActInstiter iter(p->CurScope());
  int bw = 0;
  for (iter = iter.begin(); iter != iter.end(); iter++)
  {
    ValueIdx *vx = *iter;
    bw = TypeFactory::bitWidth(vx->t);
    
    /* int variable found */
    if (TypeFactory::isIntType (vx->t))
    {
      if (bw == 1)
      {
        if (chpoptimize && (strncmp(vx->getName(), "sel", 2) == 0 ||                             strncmp(vx->getName(), "loop", 5) == 0))
        {
          fprintf(output_stream, "  syn::var_init_false var_%s;\n", vx->getName());
        } else {
          fprintf(output_stream, "  syn::var_init_false var_%s(%s);\n", vx->getName(), vx->getName());
        }
      }
      else if (bw > 1)
      {
        fprintf(output_stream, "  syn::var_init_false var_%s[%d];\n", vx->getName(), bw);
 
        /* chp-optimize creates sel0, sel1,... & loop0, loop1, ... which do not have dualrail overrides */
        if (!chpoptimize || (strncmp(vx->getName(), "sel", 2) != 0 &&                             strncmp(vx->getName(), "loop", 2) != 0))
        {
          fprintf(output_stream, "  ( i:%d: var_%s[i](%s.r[i]); )\n", bw, vx->getName(), vx->getName());
        }
      }
    }
  }
  fprintf(output_stream, "\n");
}

void generate_act(Process *p, const char * input_file, const char *output_file, bool bundled, int opt, int chpopt, struct Hashtable * chans)
{
  struct act_chp *chp = NULL;
  if (p->lang != NULL && p->lang->getchp() != NULL)
    chp = p->lang->getchp();

  /* set global variables */
  P = p;
  bundle_data = bundled;
  optimization = opt;
  chpoptimize = chpopt;
  chan_sends = chans;
  current_chan_sends = hash_new(INITIAL_HASH_SIZE);

  /* hash table to track multiple sends */
  int it;
  hash_bucket_t *b, *cb;
  
  /* TODO: get current chan sends table initialized by data from chan_sends */
  //  printf("hash... %d, %d, %d\n", chan_sends != NULL, chan_sends->size > 0, chan_sends->head[0] != NULL);
  if (chan_sends != NULL && chan_sends->size > 0 && chan_sends->head[0] != NULL)
  {
    // printf("creating current chan sends\n");
    for (it = 0; it < chan_sends->size; it++)
    {
      // printf("it=%d\n", it);
      for (b = chan_sends->head[it]; b; b = b->next)
      {
        // printf("... hash bkt=%s, count=%d\n", b->key, b->i);
        if (b->i > 1)
        {
          cb = hash_add(current_chan_sends, b->key);
          cb->i = 0;
        }
      }
    }
  }
  
  /* initialize the output location */ 
  if (output_file)
  {
    char *output_path = (char *)calloc(MAX_PATH_SIZE, sizeof(char));
    const char *output_dir = "";
    const char *bundled_prefix = bundle_data ? "" : "";
    snprintf(output_path, MAX_PATH_SIZE, "%s%s%s", output_dir, bundled_prefix, output_file);
    output_stream = fopen(output_path, "w");
    free(output_path);
  }
  else
  {
    output_stream = stdout;
  }
  
  if (output_stream == NULL) {
    fprintf(stderr, "Could not open output_file=%s\n", output_file);
    return;
  }
  
  /* get proc_name */
  size_t pn_len = strlen(p->getName());
  char proc_name[pn_len-1];
  strncpy(proc_name, p->getName(), pn_len-2);
  proc_name[pn_len-2] = '\0';
  
  /* print imports */
  fprintf(output_stream, "import \"%s\";\n", input_file);
  fprintf(output_stream, "import syn;\n");
  if (bundle_data) fprintf(output_stream, "import bundled;\n");
  fprintf(output_stream, "\n");
  
  /* Print params for toplevel from process port list */
  int pnum = p->getNumPorts();
  bool has_overrides = false;
       
  /* Write process definition and variable declarations */
  int overrides = write_process_definition(p, proc_name);
  initialize_chp_ints(p, overrides);

  int *bitwidth = (int *) calloc(1, sizeof(int));
  int *base_var = (int *) calloc(1, sizeof(int));
  if (optimization > 0) evaluated_exprs = hash_new(INITIAL_HASH_SIZE);
  
  /* translate the CHP */
  int i = 0;
  if (chp != NULL && chp->c != NULL)
    print_chp_stmt(chp->c, bitwidth, base_var, -1, -1);
  
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
  fprintf(output_stream, "sdt_%s t;\n", proc_name);
  
  
  if (current_chan_sends) hash_free(current_chan_sends);
  if (output_file) fclose(output_stream);
}
