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

struct varmap_info {
  ActId *id;

  unsigned int fcurexpr:1;	// flag (found in current expression)
  unsigned int fischan:1;	// channel or int?
  unsigned int fisinport:1;	// 1 if input, 0 if output

  int muxid;			// muxid
  
  int width;			// bitwidth

  int nread, nwrite;		// read = recv, write = send
  int iread, iwrite;
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

  void run_sdt (Process *p);

private:
  void _construct_varmap (act_chp_lang_t *c);
  void _construct_varmap_expr (Expr *e);
  void _clear_var_flags ();
  void _run_sdt_helper (int id, act_chp_lang_t *c);
  int _shared_expr_var;
  
 protected:
  varmap_info *_var_getinfo (ActId *id);
  
  /* variable map + usage counts */
  struct iHashtable *_varmap;

  struct iHashtable *_exprmap;
  list_t *_boolconst;
  listitem_t *_booliter;
  list_t *_intconst;
  listitem_t *_intiter;

  /* The process being translated */
  Process *P;

  /* building blocks */

  /*-- 
    The unique identifier used to control the execution of a
    statement 
    --*/
  virtual int _gen_stmt_id () = 0;

  /*-- 
    The unique identifier used to control the evaluation of an
    expression. Use non-negative integers only. -1 is used as a
    special case
    --*/
  virtual int _gen_expr_id () = 0;

  /*-- unique instance identifier --*/
  virtual int _gen_inst_id () = 0;
  
  /* id = stmt_id for skip */
  virtual void _emit_skip (int id) = 0;

  /* id = expr_id for evaluating this expression */
  void _expr_collect_vars (Expr *e);
  int _expr_get_id (Expr *e);
  void _emit_guardlist (int isloop, act_chp_gc_t *gc, list_t *reslist);
  
  virtual void _emit_expr (int *id, int target_width, Expr *e);
  
  virtual void _emit_expr_helper (int id, int *width, Expr *e);


  virtual void _emit_expr_binary (int id, int width, int type,
				  int lid, int lw, int rid, int rw) = 0;
  
  virtual void _emit_expr_unary (int id, int width, int type,
				 int lid, int lw) = 0;

  virtual void _emit_expr_const (int eid, int width, int val) = 0;

  virtual void _emit_expr_width_conv (int from_id, int from_width,
				      int to_id, int to_width) = 0;

  /* id = variable port for this identifier */
  virtual void _emit_var_read   (int eid, varmap_info *v) = 0;

  /* transfer expression to channel or variable */
  virtual void _emit_transfer (int cid, int eid, varmap_info *v) = 0;
  virtual void _emit_recv (int cid, varmap_info *ch, varmap_info *v) = 0;

  virtual void _emit_channel_mux (varmap_info *v) = 0;


  /*
     cid  : initiate command id
    stmts : a list of integers, corresponding to the statements being
            composed
  */
  virtual void _emit_comma (int cid, list_t *stmts) = 0;
  virtual void _emit_semi (int cid, list_t *stmts) = 0;

  virtual void _emit_loop (int cid, list_t *guards, list_t *stmts) = 0;
  virtual void _emit_doloop (int cid, int guard, int stmt) = 0;
  virtual void _emit_select (int is_nondet, int cid, list_t *guards, list_t *stmts) = 0;
  /*--- need emit_select_probe: guards, probes, stmts ---*/

  /*-- header and footer --*/
  virtual void _emit_begin () = 0;
  virtual void _emit_end (int topid) = 0;
};  

#endif /* __CHP2PRS_STD_H__ */
