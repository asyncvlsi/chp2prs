/*************************************************************************
 *
 *  This file is part of chp2prs
 *
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
#ifndef __CARTOGRAPHER_H__
#define __CARTOGRAPHER_H__

#include <stdbool.h>
#include <act/lang.h>
#include <act/body.h>

/*
 * void print_vars (struct act_chp *c)
 *
 * Summary:
 *
 *    Declares and initializes all varialbes and channels in the CHP program
 *    described by c. All variables are initialized with syn_var_init_false.
 *    Returns immediately if c contains no symbols or is otherwise invalid.
 *
 *    All lines of ACT are printed to the location specified by output_stream.
 *
 * Parameters: c - a data structure containing the read-in CHP program
 */
// void print_vars(struct act_chp *c);


/*
 * void emit_const_1 (void)
 *
 * Summary:
 *
 *    Declares a static node connected to Vdd. If such a node has already been
 *    created by a previous call, no action is taken.
 *
 *    All lines of ACT are printed to the location specified by output_stream.
 */
void emit_const_1(void);

/*
 * void emit_const_0 (void)
 *
 * Summary:
 *
 *    Declares a static node connected to GND. If such a node has already been
 *    created by a previous call, no action is taken.
 *
 *    All lines of ACT are printed to the location specified by output_stream.
 */
void emit_const_0(void);


/*
 * int get_bitwidth (int n, int base)
 *
 * Summary:
 *
 *    Returns the bitwidth of the integer n in the specified base.
 *
 */
int get_bitwidth(int n, int base);

/*
 * int get_func_bitwidth (char *s)
 *
 * Summary:
 *
 *    Returns the bitwidth of the function call represented by s.
 *
 * Parameters: s - a string containing the parsed CHP function call, of the form
 *                 <name>_<bitwidth>
 *
 * Return Value: the bitwidth of the function as specified in s
 */
int get_func_bitwidth(char *s);

/*
 * int get_max_bits (const char *s, int lbits, int rbits)
 *
 * Summary:
 *
 *    Gets maximum the bitwidth of the result of the operation specified by s on
 *    operands of bitwidths lbits and rbits, respectively. Acceptable operations
 *    are: "add", "sub", "mul", and "div".
 *
 * Parameters: s - a string specifying the arithmetic operation
 *             lbits - the number of bits in the left operand
 *             rbits - the number of bits in the right operand
 *
 * Return Value: the maximum bitwidth result of n(<lbits>) <s> m(<rbits>)
 */
int get_max_bits(const char *s, int lbits, int rbits);

/*
 * int get_bundle_delay (int n, int type)
 *
 * Summary:
 *
 *    Returns the delay for a bundle_data module of type type on operand(s) of
 *    bitwidth n. The total delay must be even for delays implemented as an
 *    inverter chain, so the result is computed by multiplying the asymptotic
 *    runtime of the module by two. Variable and integer primitives require no
 *    delay, since they are connected direclty to registers. The result is the
 *    number of inverters required, to a [large] factor of saftey, to implement
 *    the delay line of the specified bundle_data operation.
 *
 * Parameters: n - the bitwidth of the operand(s)
 *             type - the type of operation, as defined in expr.h
 *
 * Return Value: double the asymptotic delay for the module
 */
int get_bundle_delay(int n, int type);

/*
 * void get_expr (Expr *e, int v, char *buf)
 *
 * Summary:
 *
 *    Prints to buf a string representing the ACT module instance for e. If e is
 *    a primitive variable, the string is of the form "<var_name>", if it is a
 *    primitive integer, the string is of the form "<decimal_value>". Otherwise,
 *    the string is of the form "e_<expr_count>" where expr_count is the integer
 *    specified for the module instance in the compilation.
 *
 * Parameters: e - an Expr object containing the expression
 *             v - integer value of expr_count for e in the compilation
 *             buf - buffer in which to store the result
 */
void get_expr(Expr *e, int v, char *buf);

/*
 * void hash_add_expr (struct Hashtable *h, const char *expr)
 *
 * Summary:
 *
 *    Inserts the pair (expr, e_<expr>) into the table of evaluated expressions.
 *    The value associated with the key is e_<expr_count>, which corresponds to
 *    the module instantiated in the ACT file.
 *
 * Parameters: h - the hash table of evaluated expressions
 *             expr - a string representing the stored operation, of the form:
 *                    <op>(<operand1>[,<operand2>])
 */
void hash_add_expr(struct Hashtable *h, const char *expr);

/*
 * void hash_remove_expr (struct Hashtable *h, const char *expr)
 *
 * Summary:
 *
 *    Removes any instance of expr or its dependents from the hash table.
 *    Iterates through all entries in the table and recursively combs through
 *    each list of buckets.
 *
 * Parameters: h - the hash table of evaluated expressions
 *             expr - a string representing the stored operation, of the form:
 *                    <op>(<operand1>[,<operand2>])
 */
void hash_remove_expr(struct Hashtable *h, const char *expr);

/*
 * void _hash_remove_expr (struct Hashtable *h, const char *expr, hash_bucket_t *b)
 *
 * Summary:
 *
 *    Removes any instance of expr or its dependents from the hash table.
 *    Recursively inspects an individual index in the table, removing a bucket
 *    from the chain if expr is a constituent. Also recursively calls
 *    hash_remove_expr() on the value of any removed bucket.
 *
 * Parameters: h - the hash table of evaluated expressions
 *             expr - a string representing the stored operation, of the form:
 *                    <op>(<operand1>[,<operand2>])
 */
void _hash_remove_expr(struct Hashtable *h, const char *expr, hash_bucket_t *b);

/*
 * int hash_get_or_add (struct Hashtable *h, const char* s, Expr *l, Expr *r, int nl, int nr, bool commutative)
 *
 * Summary:
 *
 *    Looks up the entry <s>(<l>[,<r>]) in the hash table h. Returns the integer
 *    associated with the key's value, if found, or -1 if not. If the entry is
 *    not in the table, it (and its symmetric counterpart, if the operation is
 *    commutative) is added to the table.
 *
 * Parameters: h - the hash table of evaluated expressions
 *             s - a string representing the operation
 *             l - an Expr object containing the LHS of the expression
 *             r - an Expr object containing the RHS of the expression
 *             nl - the (previous) value of expr_count for the left expression
 *             nr - the (previous) value of expr_count for the right expression
 *             commutative - boolean representing the commutativity of s, false
 *                           for unary operations
 *
 * Return Value: the value of expr_count when the expression was evaluated, or
 *               -1 if the expression is not in the table
 */
int hash_get_or_add(struct Hashtable *h, const char *s, Expr *l, Expr *r, int nl, int nr, bool commutative);


/*
 * int unop (const char *s, Expr *e, int *bitwidth, int *base_var)
 *
 * Summary:
 *
 *    Prints the ACT for a unary operation represented by e.
 *
 *    All lines of ACT are printed to the location specified by output_stream.
 *
 * Parameters: s - a string representing the operation
 *             e - an Expr object containing the expression
 *             bitwidth - reference to an integer containing the bitwidth of the
 *                        current expression, as defined by its operands
 *             base_var - reference to the expr_count value to connect the go
 *                        signal to
 *
 * Return Value: the value of expr_count for the expression
 */
int unop(const char *s, Expr *e, int *bitwidth, int *base_var, int *delay);

/*
 * int binop (const char *s, Expr *e, int *bitwidth, int *base_var, bool commutative)
 *
 * Summary:
 *
 *    Prints the ACT for a binary operation represented by e.
 *
 *    All lines of ACT are printed to the location specified by output_stream.
 *
 * Parameters: s - a string representing the operation
 *             e - an Expr object containing the expression
 *             bitwidth - reference to an integer containing the bitwidth of the
 *                        current expression, as defined by its operands
 *             base_var - reference to the expr_count value to connect the go
 *                        signal to
 *             commutative - boolean representing the commutativity of e
 *
 * Return Value: the value of expr_count for the expression
 */
int binop(const char *s, Expr *e, int *bitwidth, int *base_var, int *delay, bool commutative);

/*
 * int _print_expr (Expr *e, int *bitwidth, int *base_var)
 *
 * Summary:
 *
 *    Prints the ACT for an expression tree rooted at e.
 *
 *    All lines of ACT are printed to the location specified by output_stream.
 *
 * Parameters: e - an Expr object containing the expression
 *             bitwidth - reference to an integer containing the bitwidth of the
 *                        current expression, as defined by its operands
 *             base_var - reference to the expr_count value to connect the go
 *                        signal to
 *
 * Return Value: the value of expr_count for the root expression
 */
int _print_expr(Expr *e, int *bitwidth, int *base_var, int *delay);

/*
 * int print_expr (Expr *e, int *bitwidth, int *base_var)
 *
 * Summary:
 *
 *    Prints the ACT for an expression tree rooted at e. Resets base_var, which
 *    stores the instance integer of the go signal for the expression block.
 *
 *    All lines of ACT are printed to the location specified by output_stream.
 *
 * Parameters: e - an Expr object containing the expression
 *             bitwidth - reference to an integer containing the bitwidth of the
 *                        current expression, as defined by its variables
 *             base_var - reference to the expr_count value to connect the go
 *                        signal to
 *
 * Return Value: the value of expr_count for the root expression
 */
int print_expr(Expr *e, int *bitwidth, int *base_var, int *delay);


/*
 * int print_expr_tmpvar (char *req, int ego, int eout, int bits)
 *
 * Summary:
 *
 *    Prints a fully-sequenced receive of the node e_<eout> into a temporary
 *    variable e_<evar>. The base variable evaluation and the sequencing "go"
 *    signals are connected, so a master "go" signal triggers the following:
 *        - inputs to e_<eout> are evaluated when the request to the sequencer
 *          goes high
 *        - e_<eout> is received into a buffer
 *        - the buffer is closed by a completion tree
 *        - the buffer is copied into a temporary variable e_<evar>
 *
 *    All lines of ACT are printed to the location specified by output_stream.
 *
 * Parameters: req - a string containing the request for the sequencing
 *             ego - expr_count for the base variable for the expression
 *             eout - expr_count for the signal to be stored
 *             bits - the bitwidth of e_<eout>
 *
 * Return Value: the value of expr_count for the temporary variable
 */
int print_expr_tmpvar(char *req, int ego, int eout, int bits);


/*
 * int print_one_gc (act_chp_gc_t *gc, int *bitwidth, int *base_var)
 *
 * Summary:
 *
 *    Prints a single guarded command. Connects the appropriate outputs to the
 *    evaluation request for the guarded command. Returns an integer that
 *    corresponds to the r1of2 channel that is used to evaluate the guard.
 *
 *    All lines of ACT are printed to the location specified by output_stream.
 *
 * Parameters: gc - object containing the CHP guard and statement
 *             bitwidth - reference to the width of the statement
 *             base_var - reference to the go signal for the statement
 *
 * Return Value: the integer corresponding to the guard's request channel
 */
int print_one_gc(act_chp_gc_t *gc, int *bitwidth, int *base_var);

/*
 * int print_gc (int loop, act_chp_gc_t *gc, int *bitwidth, int *base_var)
 *
 * Summary:
 *
 *    Prints a sequence of guarded commands by iteratively calling print_one_gc().
 *
 *    All lines of ACT are printed to the location specified by output_stream.
 *
 * Parameters: loop - discriminator between a loop and a selection statement
 *             gc - object containing the CHP guard and statement
 *             bitwidth - reference to the width of the statement
 *             base_var - reference to the go signal for the statement
 *
 * Return Value: the integer corresponding to the first guard's request channel
 */
int print_gc(bool loop, act_chp_gc_t *gc, int *bitwidth, int *base_var);


/*
 * int print_chp_stmt (act_chp_lang_t *c, int *bitwidth, int *base_var)
 *
 * Summary:
 *
 *    Prints a CHP statement specified by c. Returns an integer corresponding to
 *    the master request channel for the statement. CHP operations are
 *    constructed in the following manner:
 *        - ASSIGNMENT:  RHS is evaluated and received into a temporary variable,
 *                       then received into the LHS variable
 *        - SEND:        data is evaluated and received into a temporary variable,
 *                       then connected to the send channel
 *        - RECEIVE:     data from channel is received into the specified variable
 *        - COMPOSITION: go signals for individual modules are connected, and
 *                       the function is called recursively
 *        - GUARDS:      guarded statement chains (loops or selection statements)
 *                       are evaluated with print_gc()
 *
 *    All lines of ACT are printed to the location specified by output_stream.
 *
 * Parameters: c - object containing the CHP statement
 *             bitwidth - reference to the width of the statement
 *             base_var - reference to the go signal for the statement
 *             need_sequencer - control signal # if fullseq needed, o.w. = -1
 *             seq_num - fullseq variable # if fullseq needed, o.w. = -1
 *
 * Return Value: the integer corresponding to the base request channel for the
 *               statement
 */
int print_chp_stmt(act_chp_lang_t *c, int *bitwidth, int *base_var, int need_sequencer, int seq_num);


/*
 * void generate_act (struct act_chp *c)
 *
 * Summary:
 *
 *    Prints a CHP program, compiled into ACT. Prints all the syntactical
 *    pieces that come at the start and end of the file, as specified by
 *    the parameters passed in on the command line.
 *
 *    All lines of ACT are printed to the location specified by output_stream.
 *
 * Parameters: c - object containing the CHP program
 *             input_file - the file passed to chp2prs, is imported in the outputted ACT file
 *             output_file - the destination of the output of chp2prs
 *             chpopt - boolean indicating if the "--optimize" was used... enables sequencer optimization by the conversion of the CHP with the tools from the chp-optimize library CHP
 */
void generate_act(Process *p, const char *input_file, const char *output_file, bool bund, int opt, int chpopt);

#endif
