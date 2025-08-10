/*************************************************************************
 *
 *  Copyright (c) 2020 Rajit Manohar
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
#ifndef __CHP2PRS_STD_H__
#define __CHP2PRS_STD_H__

#include <string>
#include <act/act.h>

/*
 *  Generic abstract base class for syntax-directed translation
 */
class SDTEngine {
 public:

  /// Create SDT traslator
  /// @param exprfile is the file where block expressions definitions
  /// are emitted.
  SDTEngine(const char *exprfile);
  

  /*
   * Main method that executes the syntax-directed translation engine
   */
  void run_sdt (Process *p, int emit_end_braces = 1);

  virtual int sdt_error () { return 0; }

private:
  /*-- helper function for run_sdt --*/
  void _run_sdt_helper (int id, act_chp_lang_t *c);

protected:
  /*-- 
    SDT methods that you shouldn't have to override
    --*/
  int bitWidth (ActId *id);

  virtual void _emit_guardlist (int isloop, act_chp_gc_t *gc, list_t *reslist);
  virtual void _emit_one_guard_expr (Expr *e, list_t *reslist);

  /*-- used for block expression construction --*/
  struct iHashtable *_exprmap;
  list_t *_boolconst;
  listitem_t *_booliter;
  list_t *_intconst;
  listitem_t *_intiter;
  void _expr_collect_vars (Expr *e, int collect_phase);

  /// Integer numbering of expression instances
  int _expr_id;			
  
  /// Integer numbering of control channels for statements
  int _stmt_id;	

  /// Integer numbering of instances
  int _inst_id;

  /// Block expression definition number, shared across the entire design
  static int _blk_id;

  /* The process being translated */
  Process *P;


  /*-- top level block ID --*/
  int _block_id;

  /*
   * This is used to emit expression building blocks in a separate
   * file 
   */
  const char *_exprfile;
  FILE *_efp;
  
  /* building blocks */

  /*-- 
    Return a unique identifier used to control the execution of a
    statement. This should return a fresh control channel ID.
    These are referred to as `statement IDs.'
    --*/
  virtual int _gen_stmt_id ();

  /*-- 
    Return a unique identifier used to control the evaluation of an
    expression. Use non-negative integers only. -1 is used as a
    special case.
    --*/
  virtual int _gen_expr_id ();

  /*-- Return unique instance identifier --*/
  virtual int _gen_inst_id ();

  /*-- Return unique instance identifier --*/
  virtual int _gen_expr_blk_id ();

  /*------------------------------------------------------------------------
   *
   *  Core syntax-directed translation methods
   *
   *
   *  The first argument is always either the statement id (for
   *  statements), or the expression ID (for expressions).
   *  Exception will be noted.
   *------------------------------------------------------------------------
   */

  /*================ expressions ====================*/

  /* 
   * Top-level for emitting expressions
   *
   *   Set "*id" to the expression ID to evaluate the expression
   * 
   *   target_width = result should be of this bitwidth
   *   e = expression parse  tree
   * 
   *   NOTE: there is a default implementation that can be used, but the
   *   entire process of evaluating an expression can be overridden by
   *   replacing this method.
   */     
  virtual void _emit_expr (int *id, int target_width, Expr *e);

  /*-- helper function used to emit the expression tree ---*/
  virtual void _emit_expr_helper (int id, int *width, Expr *e);

  /*-- 
    Override these if you want to use the _emit_expr block, but
       want to change the base cases for expression evaluation
  --*/

  /*
     In expression block mode, used to emit the expression block
     instance 
  */
  virtual void _emit_expr_block (int eid, int blkid, list_t *eleaf) = 0;

  /*-- 
    Emit binary operation 
        width : result bit-width
        type  : e->type field from expression (E_AND, etc.)
      lid, lw : ID and bit-width for left argument
      rid, rw : ID and bit-width for right argument
    --*/
  virtual void _emit_expr_binary (int eid, int width, int type,
				  int lid, int lw, int rid, int rw) = 0;


  /* -- ite: if then else, for query expressions -- */
  virtual void _emit_expr_ite (int eid, int width, int type, int tid,
				  int lid, int lw, int rid, int rw) = 0;

  /*--
    Emit unary operation (args follow binary operation)
  --*/
  virtual void _emit_expr_unary (int eid, int width, int type,
				 int lid, int lw) = 0;

  /*--
    Emit bitfield extraction
  --*/
  virtual void _emit_expr_bitfield (int eid, int lsb, int msb, int lid, int lw) = 0;

  /*-- 
    Emit concat.
       list = id, width list (integer)
    --*/
  virtual void _emit_expr_concat2 (int eid, int width, int lid, int lw, int rid, int rw) = 0;

  

  /*--
    Emit a constant expression
       val : value of the constant
     width : bit-width of the constant
  */
  virtual void _emit_expr_const (int eid, int width, int val, bool forced = false) = 0;

  /*--
    Bit-width conversion
        from_id, from_width : existing expression with specified
                              bit-width
	    to_id, to_width : result with new bit-width 
  --*/
  virtual void _emit_expr_width_conv (int from_id, int from_width,
				      int to_id, int to_width) = 0;


  /*-- 
    Emit a read access to a variable
         v : the variable to be read
    --*/
  virtual void _emit_var_read   (int eid, ActId *v) = 0;

  virtual void _emit_var_read_struct (int eid, ActId *v) = 0;
  

  
  /*================ Statements ====================*/
  
  /* skip */
  virtual void _emit_skip (int id) = 0;

  /* transfer expression to channel or variable */
  virtual void _emit_transfer (int cid, int eid, ActId *v) = 0;

  /* receive */
  virtual void _emit_recv (int cid, ActId *ch, ActId *v) = 0;

  /*--- 
    Loop and Comma:

      cid : initiate command id
    stmts : a list of integers, corresponding to the statements being
            composed
  */
  virtual void _emit_comma (int cid, list_t *stmts) = 0;
  virtual void _emit_semi (int cid, list_t *stmts) = 0;

  /*---
    Encapsulation / true sequencer element
    
       sid = statement ID to be encapsulated
       cid = initiate sequencer
    --*/
  virtual void _emit_trueseq (int cid, int sid) = 0;

  /*--
    Generate a fresh (single use) variable. This is used for storing
    guard results and assignments when the RHS uses the variable being
    assigned.

    Return 0 if you don't need a fresh variable, 1 otherwise.
  */
  virtual int _gen_fresh_var (int width, ActId **v) = 0;

  /*-- 
    Given an expression ID where the result is a Boolean, use the
    sequencer and generate a safe boolean value.
  --*/
  virtual int _gen_safe_bool (int eid) = 0;

  /*-- Loop
     guards : list of integers corresponding to expression IDs for
              guards. The last value can be -1 to indicate `else'
 
      stmts : list of integers corresponding to statement IDs for
              corresponding statements
  --*/
  virtual void _emit_loop (int cid, list_t *guards, list_t *stmts) = 0;

  /* Do loop: like loop, but only one guard and one statement */
  virtual void _emit_doloop (int cid, int guard, int stmt) = 0;

  /* Selection

       is_nondet : 1 if this is a non-deterministic selection, 0 otherwise
             cid : statement ID
    guards, stmts: same as loop
  */
  virtual void _emit_select (int is_nondet, int cid, list_t *guards, list_t *stmts) = 0;


  /* For probed selections */
  
  virtual void _emit_probed_select (int cid, list_t *dataguards,
				    list_t *guards, list_t *stmts) = 0;
  
  virtual int _emit_chan_to_probe (ActId *chid) = 0;
  virtual int _emit_probed_clause (list_t *guards, list_t *probe_list) = 0;

  

  /*--- need emit_select_probe: guards, probes, stmts ---*/

  /*-- header and footer --*/
  virtual void _emit_begin (int emit_header) = 0;

  /// @param topid is a non-negative parameter with the top channel
  /// for the process. If it is negative, there were no statements to
  /// be translated for this process.
  virtual void _emit_end (int topid, int emit_end_braces) = 0;
};

#endif /* __CHP2PRS_STD_H__ */
