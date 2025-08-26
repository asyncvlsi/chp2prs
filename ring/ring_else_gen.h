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

#ifndef __ACT_RING_ELSE_GEN_H__
#define __ACT_RING_ELSE_GEN_H__

#include "reqs.h"

/* 
 * Recurses through the given chp tree.
 * For every selection that has an else guard,
 * replace the else guard with the correct explicit version.
 * For example,
 * 
 *  [G1->S1
 * []G2->S2
 * []G3->S3
 * []else->S4
 * ]
 * 
 * gets converted to:
 * 
 *  [G1->S1
 * []G2->S2
 * []G3->S3
 * []~(G1|G2|G3)->S4
 * ]
 * 
*/
void fill_in_else_explicit (act_chp_lang_t *c, Process *p, int root);

void expand_self_assignments (act_chp_lang_t *&c, Process *p);

void make_receives_unique (act_chp_lang_t *&c, Process *p);

void flatten_lists (act_chp_lang_t *&c, Process *p);

bool _var_appears_in_expr (Expr *e, ActId *id);

/*
// Assert but error print location is parent
template <typename T>
void kassert_t (const T &x, const char *xs, const char *msg,
const char *file, int line,
const std::source_location cl = std::source_location::current())
{
  if (!(x)) {
    fprintf(stderr, "Assertion failed : %s\n", xs);
    fprintf(stderr, "Message : %s\n", msg);
    fprintf(stderr, "File: %s, Line: %u\n\n", file, line);
    fprintf(stderr, "Caller: %s\n", cl.function_name());
    fprintf(stderr, "Caller File: %s, Caller Line: %u \n", 
    cl.file_name(), cl.line());
    exit(4);
  }
}

#define kassert(x, msg, cl) kassert_t(x, #x, msg, __FILE__, __LINE__, cl)

#define call_with_loc(fn, arg) fn(arg, std::source_location::current())
*/

#endif