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

#include <hash.h>
#include <string>


/*
 *
 * Each original ACT variable will have a unique entry available that
 * indicates various properties of the variable. The varmap_info
 * structure includes the necessary information.
 *
 */
struct varmap_info {
  ActId *id;

  /*-- flags --*/
  unsigned int fcurexpr:1;	// found in current expression
  unsigned int fischan:1;	// channel or int?
  unsigned int fisinport:1;	// 1 if input, 0 if output
  unsigned int fisbool:1;	// bool variable

  int width;			// bitwidth

  int nread, nwrite;		// for variables
                                //     nread  = total # of reads
                                //     nwrite = total # of writes
				// for channels
                                //     nread  = total # of receives
                                //     nwrite = total # of sends
  
  int iread, iwrite;		// running counter used for muxing
};

/*
 *  Generic abstract base class for syntax-directed translation
 */
class SDTEngine {
 public:
  SDTEngine() {
    P = NULL;
    _varmap = NULL;
    _exprmap = NULL;
    _shared_expr_var = 0;
  }

  /*-- 
    In the mode when expression variables are not shared, the
    core modules assume that the leaf of the expression tree "pulls" 
    values from the variables
    --*/
  void modeSharedExprVar () { _shared_expr_var = 1; }


  /*
   * Main method that executes the syntax-directed translation engine
   */
  void run_sdt (Process *p);

private:
  /*-- internal functions --*/

  /* constructs the varmap structure */
  void _construct_varmap (act_chp_lang_t *c);
  void _construct_varmap_expr (Expr *e);
  void _clear_var_flags ();

  /*-- helper function for run_sdt --*/
  void _run_sdt_helper (int id, act_chp_lang_t *c);

  /*-- mode: currently not used --*/
  int _shared_expr_var;

  /*-- the varmap table --*/
  struct iHashtable *_varmap;

  /*-- 
    these are constructed but not used at the moment; needed for
    shared expression construction
    --*/
  struct iHashtable *_exprmap;
  list_t *_boolconst;
  listitem_t *_booliter;
  list_t *_intconst;
  listitem_t *_intiter;
  void _expr_collect_vars (Expr *e);
  int _expr_get_id (Expr *e);

  /*-- 
    SDT methods that you shouldn't have to override
    --*/
  void _emit_guardlist (int isloop, act_chp_gc_t *gc, list_t *reslist);

protected:
  /*
   * This function returns the varmap_info structure
   * for an ACT identifier
   */
  varmap_info *_var_getinfo (ActId *id);
  
  /* The process being translated */
  Process *P;

  /* building blocks */

  /*-- 
    Return a unique identifier used to control the execution of a
    statement. This should return a fresh control channel ID.
    These are referred to as `statement IDs.'
    --*/
  virtual int _gen_stmt_id () = 0;

  /*-- 
    Return a unique identifier used to control the evaluation of an
    expression. Use non-negative integers only. -1 is used as a
    special case.
    --*/
  virtual int _gen_expr_id () = 0;

  /*-- Return unique instance identifier --*/
  virtual int _gen_inst_id () = 0;


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

  /*-- 
    Emit binary operation 
        width : result bit-width
        type  : e->type field from expression (E_AND, etc.)
      lid, lw : ID and bit-width for left argument
      rid, rw : ID and bit-width for right argument
    --*/
  virtual void _emit_expr_binary (int eid, int width, int type,
				  int lid, int lw, int rid, int rw) = 0;

  /*--
    Emit unary operation (args follow binary operation)
  --*/
  virtual void _emit_expr_unary (int eid, int width, int type,
				 int lid, int lw) = 0;

  /*--
    Emit a constant expression
       val : value of the constant
     width : bit-width of the constant
  */
  virtual void _emit_expr_const (int eid, int width, int val) = 0;

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
  virtual void _emit_var_read   (int eid, varmap_info *v) = 0;
  

  
  /*================ Statements ====================*/
  
  /* skip */
  virtual void _emit_skip (int id) = 0;

  /* transfer expression to channel or variable */
  virtual void _emit_transfer (int cid, int eid, varmap_info *v) = 0;

  /* receive */
  virtual void _emit_recv (int cid, varmap_info *ch, varmap_info *v) = 0;

  /* channel mux: note that the # of ports needed for the mux are
     already part of the varmap_info */
  virtual void _emit_channel_mux (varmap_info *v) = 0;

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
  */
  virtual void _gen_fresh_var (varmap_info *v) = 0;

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

  /*--- need emit_select_probe: guards, probes, stmts ---*/

  /*-- header and footer --*/
  virtual void _emit_begin () = 0;
  virtual void _emit_end (int topid) = 0;
};  

#endif /* __CHP2PRS_STD_H__ */
