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
#ifndef __CHECK_TYPES_H__
#define __CHECK_TYPES_H__

#include <act/lang.h>
#include <act/body.h>

/*
 * int get_expr_bitwidth (Expr *e)
 *
 * Summary:
 *
 *    Returns the bitwidth of the expression represented by e. If e is a
 *    primitive variable, the bitwidth of the variable is returned. If e is a
 *    primitive integer, 0 is returned as a special value and the integer is
 *    eventually truncated by the compiler to the width of the expression to
 *    which it is a part. If e is a complex expression, the bitwidth of the
 *    result is returned, or -1 is returned if the complex expression contains
 *    a sub-expression with incompatible bit widths or other errors.
 *
 *    In recursively exploring e, get_expr_bitwidth() also checks for the
 *    following errors: undeclared variables, undelcared channels, improper
 *    use of operators (i.e. E_TRUE/E_FALSE), improper use of functions, and
 *    incompatible bit widths of expressions. Any of the following result in
 *    the function immediately returning -1.
 *
 * Parameters: e - an Expr object containing the expression
 *
 * Return Value: the bitwidth of the expression; 0 if the expression can be
 *               arbitrarily truncated; -1 if any sub-expression has
 *               incompatible bitwidths
 */
int get_expr_bitwidth (Expr *e);

/*
 * void check (act_chp_lang_t *c)
 *
 * Summary:
 *
 *    Does some basic error checks on the CHP program represented by c. Assumes
 *    that all "interacting" variables are of identical bitwidth. Composite
 *    multi-bit expressions adopt the bitwidth of their constituent variables.
 *    The following elements of the CHP program are verified:
 *        - all utilized variables and channels are declared
 *        - all binary expression operands have equal bitwidths
 *        - probes are only taken of channel expressions
 *        - functions conform to <name>_<bitwidth>
 *        - assignment is only done to variable expressions
 *        - sends and receives happen over channels
 *        - sends and receives only happen over channels with bitwidth equal
 *          to the data sent/received
 *        - the send buffer is a non-channel expression
 *        - the receive buffer is a variable
 *        - guards are boolean-valued
 *
 * Parameters: c - a structure representing the CHP program
 */
void check (act_chp_lang_t *c);

void check_chp (Process *p);

#endif
