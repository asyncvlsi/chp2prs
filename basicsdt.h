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
#ifndef __ACT_BASIC_SDT_H__
#define __ACT_BASIC_SDT_H__

#include "sdt.h"


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
  unsigned int fisinport:2;	// 1 if input, 0 if output, 2 if both
  unsigned int fisbool:1;	// bool variable or bool chan

  int width;			// bitwidth

  int block_in, block_out;	// for internal channels

  int nread, nwrite;		// for variables
                                //     nread  = total # of reads
                                //     nwrite = total # of writes
				// for channels
                                //     nread  = total # of receives
                                //     nwrite = total # of sends
  
  int iread, iwrite;		// running counter used for muxing
};


class BasicSDT : public SDTEngine {
 public:
  /**
   *  Basic SDT translator
   *
   * @param isbundled indicates if the datapath is bundled data (1) or
   * QDI (0)
   *
   * @param isopt is 1 if SDT control optimizations are enabled
   *
   * @param doimport is set if the output should emit another import
   * directive
   *
   * @param out is the output file name
   *
   */
  BasicSDT (int isbundled, int isopt, FILE *fpout, const char *expr_file);

 protected:
  /// Run SDT control optimizations
  int optimize;

  /// Datapath is bundled data (vs QDI)  
  int bundled_data;
  
  /// Output file stream
  FILE *output_stream;


  /*-- mode: currently not used --*/
  int _shared_expr_var;

  /// Aux functions for variable information
  int _get_isinport (varmap_info *v);

  /*
   * This function returns the varmap_info structure
   * for an ACT identifier
   */
  varmap_info *_var_getinfo (ActId *id);

  /*
   * Use to emit ids
   */
  void _emit_mangled_id (FILE *fp, ActId *id);
  
  /* constructs the varmap structure */
  void _construct_varmap (act_chp_lang_t *c);
  void _construct_varmap_expr (Expr *e);
  void _clear_var_flags ();

  /*-- the varmap table --*/
  struct iHashtable *_varmap;

  
  /// Override stmt id to also emit the channel definition in the
  /// output stream
  int _gen_stmt_id ();

  void _emit_skip (int id);

  void _emit_expr_block (int eid, int blkid, list_t *eleaf);

  void _emit_expr_binary (int id, int width,
			  int type,
			  int lid, int lw,
			  int rid, int rw);
  
  void _emit_expr_unary (int id, int width,
			 int type, int lid, int lw);

  void _emit_expr_ite (int id, int width, 
		       int type, int tid,
		       int lid, int lw,
		       int rid, int rw);
  
  void _emit_expr_bitfield (int eid, int lsb, int msb, int lid, int lw);
  
  void _emit_expr_concat2 (int eid, int width, int lid, int lw, int rid, int rw);

  

  void _emit_expr_width_conv (int from, int from_w,
			      int to, int to_w);
  
  void _emit_expr_const (int eid, int width, int val);

  /* id = variable port for this identifier */
  void _emit_var_read   (int eid, ActId *id);
  
  void _emit_transfer (int cid, int eid, ActId *);

  void _emit_recv (int cid, ActId *chid, ActId *);

  /*-- internal --*/
  void _emit_channel_mux (varmap_info *ch);
  void _emit_variable_mux (varmap_info *v);

  void _emit_comma (int cid, list_t *stmts);
  void _emit_semi (int cid, list_t *stmts);
  void _emit_semiopt (int cid, list_t *stmts);
  void _emit_trueseq (int cid, int sid);

  int _gen_fresh_var (int width, ActId **);
  int _gen_fresh_var_writeonly (int width, ActId **id);
  int _gen_safe_bool (int eid);

  void _emit_loop (int cid, list_t *guards, list_t *stmts);
  void _emit_doloop (int cid, int guard, int stmt);
  
  void _emit_select (int is_nondet, int cid, list_t *guards, list_t *stmts);

  void _emit_begin ();
  void _emit_end (int toplev);

  bool write_process_definition(FILE *fp, Process * p);
  void initialize_chp_ints(FILE *fp, Process * p, bool has_overrides);

};  

#endif /* __ACT_BASIC_SDT_H__ */
