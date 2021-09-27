#ifndef __ACT_EXTERNOPT_SDT_H__
#define __ACT_EXTERNOPT_SDT_H__

#include "basicsdt.h"
#include "config_pkg.h"
#include <act/expropt.h>

/**
 * the ExternOpt Syntax directed translation class takes a CHP process
 * and expans and translates it to production rules.  all data path
 * parts are handed of to an external syntesis and mapping tool, like
 * yosys or genus.  the controll path is handeled identiacal the the
 * basic Syntax directed translation which this class inherits from.
 * 
 */
class ExternOptSDT : public BasicSDT {
  public:
    /**
     * constructs a SDT translator. 
     * 
     * @param isbundled indicate if the translation is to bundled data
     * (1) or qdi (0)
     * @param isopt is handed of to BasicSDT - should have no function
     * @param doimport an additional act file that should be imported
     * by the output 
     * @param out the path to the output file that the translated prs
     * act is writen to. 
     * @param exprfile the path to the file all the datapath elements
     * are writen to 
     * @param map the selected external sysntesis tool to be used for
     * the logic syntesis and mapping - defaults to yosys. 
     * @param tmp_path the path where all the temporary files are
     * generated, not implemented yet.
     */
    ExternOptSDT (int isbundled, 
                  int isopt, 
                  FILE *fpout,
		  const char *ef,
                  expr_mapping_software map = yosys,
                  const char *tmp_path = ".") :
      BasicSDT(isbundled, isopt, fpout, ef) {
      
      tmp_file_path = tmp_path;

      _map = map;

      mapper = NULL;
    }

  private:
  expr_mapping_software _map;

    /**
     * the path where the temporary syntesis files are generated in, not yet implemented uses the exceution directory of the program currently.
     */
    const char *tmp_file_path;

    /**
     * the datapath expression translator form the expropt library
     */
    ExternalExprOpt *mapper;

    /**
     * the map that contains the pointer addess of the act expression var as key and a int id as the varname to be used in the translation
     */
    struct iHashtable *_inexprmap;

    /**
     * the map that contains the pointer addess of the act expression as key and a int indicating the bitwidth of the expression var
     */
    struct iHashtable *_inwidthmap;

    /**
     * the map that contains the pointer addess of the act full expression as key and a int id as the varname it is to be assined to
     */
    struct iHashtable *_outexprmap;

    /**
     * the map that contains the pointer addess of the act expression as key and a int indicating the bitwidth of the var it is assigned to
     */
    struct iHashtable *_outwidthmap;
  
  protected:

    /**
     * construct in and out data structurs, hand off expression to external tool, wrap mapped logic and bypass handshaking.
     * overwrite sdt behavior
     * 
     * @param id expression id used to identify the expression
     * @param tgt_width target width the expression should output
     * @param e the expression itself (act data structure)
     */
    void _emit_expr (int *id, int tgt_width, Expr *e);

    /**
     * go through the expression datastructure and find all input vars and register them in the in maps.
     * @param e the expression itself (act data structure)
     * @param collect_phase if it is searching the vars or if it is printing the var wrappers
     */
    void _expr_collect_vars (Expr *e, int collect_phase);

    /**
     * A helper to go through the expression datastructure and compute the resulting bitwidth.
     * @param ex the expression itself (act data structure)
     * @return int the calculated bitwidth resulting out of the expression
     */
    int get_expr_width(Expr *ex);

    /**
     *  prints and creates a constant in qdi mode, or in bundeled data mode if its a constant guard => infinite loop
     * 
     * @param id the id the constant should carry
     * @param width the bit width of the constant
     * @param val the value the constant emmits
     * @param isguard if it is a guard so it is created also in bundled data mode
     */
    void _emit_expr_const (int id, int width, int val);
    void _emit_expr_const (int id, int width, int val, bool isguard);

    /**
     * creates a wrapper for the expression block from wires in to a channel, 
     * converts the width in qdi mode if expression width and target width do not match.
     * 
     * @param from the id of the expression block
     * @param from_w the output width of the expression block
     * @param to the id of the wrapper
     * @param to_w and the width of the resulting signal (to which it should be converted to)
     */
    void _emit_expr_width_conv (int from, int from_w,int to, int to_w);

    /**
     *  instanciates the expression block and prints the input connections in the constructor
     * 
     * @param id the inst id the block should have.
     * @param blkid the id of the block that is in the expression file.
     * @param exprs the input variables (pointers) that need to be connected the the block - the id will be taken from the in map for the given pointer
     */
    void _emit_expr_block (int id, int blkid, list_t *exprs);

    /**
     * generates a temp var for guards so the result of the guard is cached properly
     * @param eid the id of the signal to be guarded
     * @return int the id of the guarded signal
     */
    int _gen_safe_bool (int eid);

    /**
     * emmits all guards and wraps them appropiately
     * @TODO implemnt concurrent eval of guards and combine them for optimisation
     * @param isloop if the guards are part of a loop
     * @param gc the guards
     * @param reslist list the output ids of the guards are stored in
     */
    void _emit_guardlist (int isloop, act_chp_gc_t *gc, list_t *reslist);

    /**
     * 
     * @param new_id the id that wrapps the original id
     * @param old_id the original id
     * @param width the expression width of the channel
     */
    void _emit_expr_array_wrap(int new_id, int old_id, int width);

    /**
     * connects the ctl channel around the expr block
     * @param id the id of the out channel
     * @param all_leaves all the in channels
     */
    void _emit_bd_ctl_bypass (int id, list_t *all_leaves);

};  

#endif /* __ACT_EXTERNOPT_SDT_H__ */
