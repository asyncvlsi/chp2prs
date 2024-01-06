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
 **************************************************************************
 */

#include "ring_forge.h"

RingForge::RingForge ( FILE *fp, Process *p, act_chp_lang_t *c,
            const char *circuit_library,
            const char *exprfile = "expr.act" )
    : RingEngine ( fp, p, c, circuit_library, exprfile )
{
    ring_block_prefix = "block_";
    conn_block_prefix = "conn_z_";

    // Datapath name prefixes
    capture_block_prefix = "latch_";
    expr_block_prefix = "blk_";
    expr_block_instance_prefix = "inst_";
    expr_block_input_prefix = "in_";

    // Channel name prefixes
    sync_chan_name_prefix = "sync_";
    parallel_chan_name_prefix = "sync_";
    init_cond_chan_prefix = "C_init_";

    // Bundled datapath parameters
    invx1_delay_ps = 21;
    capture_delay = 5;
    pulse_width = 6;

    // Instance counters
    block_count = 0;
    itb_wrapper_count = 0;
    bd_chan_count = 0;
    sync_chan_count = 0;
    expr_id = 0;
    expr_block_id = 0;
    mux_block_id = 0;
    branch_id = 0;
}

/*
    Generate dataless ITB that initializes the ring.
*/
int RingForge::_generate_itb()
{   
    fprintf(_fp,"\n// Initial token buffer to initialize ring\n");
    int id = _gen_block_id();
    fprintf(_fp,"elem_c_itb %s%d;\n",ring_block_prefix,id);
    return id;
}

/*
    Generate a parallelizing split block for comma-separated
    statements. 
*/
int RingForge::_generate_parallel_split(int n)
{
    int block_id = _gen_block_id();
    fprintf(_fp,"parallel_split<%d> %s%d;\n", n, ring_block_prefix, block_id);
    return block_id;
}

/*
    Generate a parallelizing merge block for comma-separated
    statements. 
*/
int RingForge::_generate_parallel_merge(int n)
{
    int block_id = _gen_block_id();
    fprintf(_fp,"parallel_merge<%d> %s%d;\n", n, ring_block_prefix, block_id);
    return block_id;
}

/*
    Generate a selection split block for branches
*/
int RingForge::_generate_selection_split(int n)
{
    int block_id = _gen_block_id();
    fprintf(_fp,"selection_split<%d> %s%d;\n", n, ring_block_prefix, block_id);
    return block_id;
}

/*
    Generate a selection merge block for branches
*/
int RingForge::_generate_selection_merge(int n)
{
    int block_id = _gen_block_id();
    fprintf(_fp,"selection_merge<%d> %s%d;\n", n, ring_block_prefix, block_id);
    return block_id;
}

/*
    Generate a pass-through connection block. This is just for 
    convenience of connection due to some port naming conventions. 
    The actual block is just wires. 
*/
int RingForge::_generate_gp_connect()
{
    int block_id = _gen_block_id();
    fprintf(_fp,"gp_connect %s%d;\n", ring_block_prefix, block_id);
    return block_id;
}

/*
    Function to call an external logic synthesis tool to
    generate combinational logic to implement functions.
    Currently supports only abc. 
*/
int RingForge::_generate_expr_block(Expr *e, int out_bw)
{
    // fprintf (fp, "// hello from expropt\n");
    // create mapper object
    ExternalExprOpt *eeo = new ExternalExprOpt(abc, bd, false, expr_file, 
                                                expr_block_input_prefix,
                                                expr_block_prefix);
    Assert ((eeo), "Could not create mapper");

    // collect input vars info
    _inexprmap = ihash_new (0);
    _inwidthmap = ihash_new (0);
    _expr_collect_vars (e, 1);

    // collect input vars in list
    list_t *all_leaves = list_new();
    {
        ihash_iter_t iter;
        ihash_bucket_t *ib;
        ihash_iter_init (_inexprmap, &iter);
        while ((ib = ihash_iter_next (_inexprmap, &iter))) {
        Expr *e1 = (Expr *)ib->key;
        list_append (all_leaves, e1);
        }
        // fprintf (fp, "\n// gettin here..\n" );
    }

    // no dots
    config_set_int("expropt.verbose", 0);
    // config_set_int ("expropt.clean_tmp_files", 0);
    // config_set_int("expropt.abc_use_constraints", 1);

    // output bitwidth and block id for name
    int xid = _gen_expr_block_id();
    // int out_expr_width = get_expr_width (e, p);
    int out_expr_width = out_bw;
    int delay_line_n;

    fprintf(_fp, "// output bitwidth: %d bits\n",out_expr_width);
    fprintf(stdout, "\n\n");

    if (e->type == E_INT)
    {
        config_set_int("expropt.abc_use_constraints", 0);
    }

    // run abc, then v2act to create the combinational-logic-for-math process
    ExprBlockInfo *ebi = eeo->run_external_opt(xid, out_expr_width, e, all_leaves, _inexprmap, _inwidthmap);
    // ExprBlockInfo *ebi = eeo->run_external_opt(xid, out_expr_width, e, NULL, NULL, NULL);
    // ExprBlockInfo *ebi = eeo->run_external_opt(xid, out_expr_width, e, all_leaves, NULL, NULL);
    
    if (e->type == E_INT) 
    {
        config_set_int("expropt.abc_use_constraints", 1);
        delay_line_n = 1;
    }
    else 
    {
        Assert ((ebi->delay_typ != -1), "Delay not extracted by abc!");
        double typ_delay_ps = (ebi->delay_typ)*1e12;
        delay_line_n = int( (typ_delay_ps/(2*invx1_delay_ps)) + 1 ); 
        if (delay_line_n == 0) { delay_line_n = 1; }

        fprintf(_fp, "\n// typical delay: %gps\n",typ_delay_ps);
    }

    _instantiate_expr_block (xid, all_leaves);

    fprintf(_fp,"delay_line_chan<%d> delay_expr_%d;\n",delay_line_n,xid);
    // fprintf (stdout, "\n// bye from expropt\n");

    eeo->~ExternalExprOpt();
    ebi->~ExprBlockInfo();

    return xid;
}

/*
    Instantiate a combinational logic block. (TODO) Currently 
    assumes only one instance of each generated expr block will 
    be used, so using block_id for naming the instance also. 
    I think this is actually fine. 
*/
void RingForge::_instantiate_expr_block (int block_id, list_t *all_leaves)
{
    ihash_bucket_t *ib, *ibw;
    listitem_t *li;

    // generate instance
    fprintf(_fp,"%s%d %s%d;\n",expr_block_prefix,block_id,expr_block_instance_prefix,block_id);
    
    // connect inputs
    for (li = list_first(all_leaves); li ; li = list_next(li))
    {
        Expr *e_var = (Expr *)list_value(li);
        ib = ihash_lookup (_inexprmap, (long)e_var);

        // connect variables to math block inputs 
        if ( e_var->type == E_VAR )
        {
            ActId *var = (ActId *)e_var->u.e.l;
            char tname[1024];
            get_true_name(tname, var, _p->CurScope());
            hash_bucket_t *b = hash_lookup(var_infos, tname);
            // hash_bucket_t *b = hash_lookup(var_infos, var->rootVx(p->CurScope())->getName());
            var_info *vi = (var_info *)b->v;

            // int latch_id = (vi->iwrite)-1;
            // TEST ----
            int latch_id = vi->latest_for_read;
            // TEST ----

            Assert ((latch_id>=0),"var. read before being written?");

            // connect variable-latch output to expr block input
            fprintf(_fp,"%s%d.%s%d = %s%s_%d.dout;\n",expr_block_instance_prefix,block_id,
                                                    expr_block_input_prefix, ib->i, 
                                                    capture_block_prefix,
                                                    vi->name,latch_id);
        }
        // connect constants to math block inputs
        else if ( e_var->type == E_INT )
        {
            fatal_error ("This shouldn't have been used (constant as input to expr block)");
        }
        else { fatal_error ("leaf (primary input) is neither variable nor constant int?? (instantiate_expr_block)"); }
    }
}


/*
    Collect all the variables in a given expression and put them 
    in the exprmap and widthmap global variables.
*/
void RingForge::_expr_collect_vars (Expr *e, int collect_phase)
{
  int id;
  Assert (e, "Hmm");

#define BINARY_OP					\
  do {							\
    _expr_collect_vars (e->u.e.l, collect_phase);	\
    _expr_collect_vars (e->u.e.r, collect_phase);	\
  } while (0)

#define UNARY_OP					\
  do {							\
    _expr_collect_vars (e->u.e.l, collect_phase);	\
  } while (0)
  
  switch (e->type) {
    /* binary */
  case E_AND:
  case E_OR:
  case E_XOR:
  case E_PLUS:
  case E_MINUS:
  case E_LT:
  case E_GT:
  case E_LE:
  case E_GE:
  case E_EQ:
  case E_NE:
  case E_MULT:
  case E_DIV:
  case E_MOD:
  case E_LSL:
  case E_LSR:
  case E_ASR:
    BINARY_OP;
    break;
    
  case E_UMINUS:
  case E_NOT:
  case E_COMPLEMENT:
  case E_BUILTIN_INT:
  case E_BUILTIN_BOOL:
    UNARY_OP;
    break;

  case E_QUERY:
    _expr_collect_vars (e->u.e.l, collect_phase);
    _expr_collect_vars (e->u.e.r->u.e.l, collect_phase);
    _expr_collect_vars (e->u.e.r->u.e.r, collect_phase);
    break;

  case E_COLON:
  case E_COMMA:
    fatal_error ("Should have been handled elsewhere");
    break;

    /* XXX: here */
  case E_CONCAT:
    {
      Expr *tmp = e;
      while (tmp) {
	_expr_collect_vars (tmp->u.e.l, collect_phase);
	tmp = tmp->u.e.r;
      }
    }
    break;

  case E_REAL:
    fatal_error ("No real expressions please.");
    break;

  case E_TRUE:
    break;
    
  case E_FALSE:
    break;
    
  case E_INT:
    if (0) {
    if (collect_phase) {
        int w = 0;
        int val = e->u.ival.v;
        if (val < 0) {
          val = -val;
          w = 32;
        }
        else {
          while (val) {
            val >>= 1;
            w++;
          }
        }
        if (w == 0) {
          w = 1;
        }
        ihash_bucket_t *ib = ihash_add (_inexprmap, (long) e);
        ib->i = gen_expr_id ();
        ihash_bucket_t *b_width;
        b_width = ihash_add (_inwidthmap, (long) e);
        b_width->i = w;
    }
    }
    break;

  case E_BITFIELD:
  case E_VAR:
    if (collect_phase) {
        ActId *var = (ActId *)e->u.e.l;
        var_info *vi;
        hash_bucket_t *b;
        char tname[1024];
        get_true_name(tname, var, _p->CurScope());
        b = hash_lookup(var_infos, tname);
        // b = hash_lookup(var_infos, var->rootVx(p->CurScope())->getName());
        vi = (var_info *)b->v;
        ihash_bucket_t *ib = ihash_add (_inexprmap, (long)e);
        ib->i = gen_expr_id();
        ihash_bucket_t *b_width;
        b_width = ihash_add (_inwidthmap, (long) e);
        b_width->i = vi->width;
    }
    else {
    //   ihash_bucket_t *b;
    //   b = ihash_lookup (_inexprmap, (long)e);
    //   _emit_var_read (b->i, (ActId *)e->u.e.l);
    }
    break;

  case E_PROBE:
    fatal_error ("fix probes please");
    break;
    
  case E_FUNCTION:
    fatal_error ("function!");
  case E_SELF:
  default:
    fatal_error ("Unknown expression type %d\n", e->type);
    break;
  }
  return;
#undef BINARY_OP
#undef UNARY_OP
}


/*
    Given two pipeline block elements, connect them via
    their plus-1 (p1) and minus-1 (m1) ports. 
*/
int RingForge::_connect_pipe_elements (int prev_block_id, int next_block_id)
{
    fprintf(_fp,"\n// Connecting block_%d & block_%d\n",prev_block_id, next_block_id);
    fprintf(_fp,"block_%d.m1 = block_%d.p1;\n",next_block_id,prev_block_id);
    return 0;
}

/*
    Connect a given port of a parallel split block to another
    pipeline element. 
*/
int RingForge::_connect_pll_split_outputs_to_pipe (int pll_split_block_id, int pipe_block_id, int pll_split_block_port)
{
    fprintf(_fp,"\n// Connecting parallel split block_%d (output) & pipe block_%d\n",
                                                pll_split_block_id, pipe_block_id);
    fprintf(_fp,"block_%d.co[%d] = block_%d.m1;\n",
                    pll_split_block_id,pll_split_block_port,pipe_block_id);
    return 0;
}

/*
    Connect a pipeline element to a given port on a parallel
    merge block. 
*/
int RingForge::_connect_pipe_to_pll_merge_inputs (int pll_merge_block_id, int pipe_block_id, int pll_merge_block_port)
{
    fprintf(_fp,"\n// Connecting parallel merge block_%d (input) & pipe block_%d\n",
                                                pll_merge_block_id, pipe_block_id);
    fprintf(_fp,"block_%d.ci[%d] = block_%d.p1;\n",
                    pll_merge_block_id,pll_merge_block_port,pipe_block_id);
    return 0;
}

/*
    Connect a given port of a selection split block to another
    pipeline element. 
*/
int RingForge::_connect_sel_split_outputs_to_pipe (int sel_split_block_id, int pipe_block_id, int sel_split_block_port)
{
    fprintf(_fp,"\n// Connecting selection split block_%d (output) & pipe block_%d\n",
                                                sel_split_block_id, pipe_block_id);
    fprintf(_fp,"block_%d.co[%d] = block_%d.m1;\n",
                    sel_split_block_id,sel_split_block_port,pipe_block_id);
    return 0;
}

/*
    Connect the output of guard evaluator to a given guard input
    of a selection split block. 
*/
int RingForge::_connect_guards_to_sel_split_input (int sel_split_block_id,
                                        int expr_block_id, int sel_split_guard_port)
{
    // inst_i.out is always size-1 array, not just a bool (1-bit datapath compatibility)
    fprintf(_fp,"block_%d.gs[%d] = %s%d.out[0];\n",sel_split_block_id,sel_split_guard_port,
                                            expr_block_instance_prefix, expr_block_id);
    return 0;
}

/*
    Connect a pipeline element to a given port on a selection
    merge block. 
*/
int RingForge::_connect_pipe_to_sel_merge_inputs (int sel_merge_block_id, int pipe_block_id, int sel_merge_block_port)
{
    fprintf(_fp,"\n// Connecting selection merge block_%d (input) & pipe block_%d\n",
                                                sel_merge_block_id, pipe_block_id);
    fprintf(_fp,"block_%d.ci[%d] = block_%d.p1;\n",
                    sel_merge_block_id,sel_merge_block_port,pipe_block_id);
    return 0;
}








/*
*/



