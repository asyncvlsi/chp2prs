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
    capture_delay = 5; // 2*n = 10 inverters in delay-line
    pulse_width = 6; // 2*n+1 = 13 inverters in pulse generator

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

void RingForge::_run_forge_helper ()
{
    int has_branches = chp_has_branches(_c, 1);
    
    // TODO: fix one pipe synthesis
    // int is_pipeable = check_if_pipeable(c, p, 1);
    // if (is_pipeable == 1)

    if (false)
    {
        fprintf (_fp, "// One Pipe ---------------------\n");
        // generate_pipe (_c,1);
    }
    else if (has_branches == 0)
    {
        fprintf (_fp, "// One Ring ---------------------\n");
        generate_one_ring (_c,1,0);
    }
    else
    {
        LiveVarAnalysis *lva = new LiveVarAnalysis (_fp, _p, _c);
        lva->generate_live_var_info();
        fprintf (_fp, "// Branched Ring ----------------\n");
        generate_branched_ring (_c,1,0,0);
    }
}

/*
    Generate a data capture element for a given variable.
    If provided with an initial value, generate a data capture
    element that initializes to that value on reset.
*/
int RingForge::_generate_single_latch (var_info *v, int init_val=-1)
{
    list_iappend_head (v->latest_latch_branches, branch_id);
    if (v->iwrite < v->nwrite)
    {
        if (init_val == -1)
        {
            fprintf(_fp, "capture<%d,%d,%d> %s%s_%d;\n", capture_delay, pulse_width, v->width, 
                                                        capture_block_prefix, v->name,v->iwrite);
        }
        else
        {
            fprintf(_fp, "capture_init<%d,%d,%d,%d> %s%s_%d;\n", capture_delay, pulse_width, v->width,
                                        init_val, capture_block_prefix, v->name,v->iwrite);
        }
        v->iwrite++;
        v->latest_for_read = v->iwrite-1;
        return (v->iwrite)-1;
    }
    fatal_error("shouldn't have reached here (generate_single_latch)");
    return -1;
} 

/*
    Generate a pipeline element for a given action, along
    with the necessary datapath elements. This is the main
    function that generates the circuit for a given action.
*/
int RingForge::_generate_pipe_element(act_chp_lang_t *c, int init_latch)
{
    ActId *chan;
    ActId *var = NULL;
    Expr *e = NULL;
    int block_id;
    int expr_inst_id;
    char chan_name[1024];
    int latch_id;
    int bw;
    InstType *it;
    hash_bucket_t *b;
    var_info *vi;

    block_id = _gen_block_id();

    switch(c->type)
    {
    case ACT_CHP_ASSIGN:
        // TODO - finish
        e = c->u.assign.e;
        var = c->u.assign.id;
        fprintf(_fp,"\n// Pipe block for action: ");
        chp_print(_fp,c);
        fprintf(_fp,"\n");
        fprintf(_fp,"elem_c_paa_brs_bd %s%d;\n",ring_block_prefix,block_id);

        fprintf(_fp,"\n// Data for action: ");
        chp_print(_fp,c);
        fprintf(_fp,"\n");
        char tname[1024];
        get_true_name(tname, var, _p->CurScope());
        b = hash_lookup(var_infos, tname);
        // b = hash_lookup(var_infos, var->rootVx(p->CurScope())->getName());
        vi = (var_info *)b->v;
        bw = vi->width;
        expr_inst_id = _generate_expr_block(e,bw);
        if (init_latch == -1)
        {
            latch_id = _generate_single_latch(vi);  
        }
        else 
        {
            latch_id = init_latch;
        }
        // connect output of math block to latch input
        fprintf(_fp,"%s%d.out = %s%s_%d.din;\n",expr_block_instance_prefix,expr_inst_id,
                                            capture_block_prefix,
                                            vi->name,latch_id);
        // connect pipe block to delay_expr input
        fprintf(_fp,"delay_expr_%d.m1 = %s%d.zero;\n",expr_inst_id,ring_block_prefix,block_id);
        // connect delay_expr output to capture block
        fprintf(_fp,"delay_expr_%d.p1 = %s%s_%d.go;\n",expr_inst_id,capture_block_prefix,
                                            vi->name,latch_id);
        break;

    case ACT_CHP_SEND:
        chan = c->u.comm.chan;
        e = c->u.comm.e;
        get_true_name (chan_name, chan, _p->CurScope());
        // chan_name = chan->rootVx(p->CurScope())->getName();
        fprintf(_fp,"\n// Pipe block for action: ");
        chp_print(_fp,c);
        fprintf(_fp,"\n");
        fprintf(_fp,"elem_c_paa_brs_bd %s%d;\n",ring_block_prefix,block_id);
        if (e) {
            it = _p->CurScope()->Lookup(chan);
            bw = TypeFactory::bitWidth(it);
            fprintf(_fp,"connect_outchan_to_ctrl<%d> %s%d;\n",bw, conn_block_prefix,block_id);
                fprintf(_fp,"%s%d.ch = %s;\n",conn_block_prefix,block_id,chan_name);

            fprintf(_fp,"\n// Data for action: ");
            chp_print(_fp,c);
            fprintf(_fp,"\n");
            if (e->type == E_VAR) { // pure variable send
            fprintf(_fp,"%s%d.ctrl = %s%d.zero;\n",conn_block_prefix,block_id,ring_block_prefix,block_id);
                var = (ActId *)e->u.e.l;
                char tname[1024];
                get_true_name(tname, var, _p->CurScope());
                b = hash_lookup(var_infos, tname);
                vi = (var_info *)b->v;
                latch_id = vi->latest_for_read;
                Assert ((latch_id>=0),"variable read but never written? what...");
                fprintf(_fp, "\n%s%s_%d.dout = %s.d;\n",capture_block_prefix,
                                            vi->name,latch_id,chan_name);
                vi->iread++;
            }
            else { // function of variable(s) send
                expr_inst_id = _generate_expr_block(e,bw);
                // connect output of math block to channel data
                fprintf(_fp,"%s%d.out = %s.d;\n",expr_block_instance_prefix,expr_inst_id,chan_name);

                // connect to delay_line
                fprintf(_fp,"delay_expr_%d.m1 = %s%d.zero;\n",expr_inst_id,ring_block_prefix,block_id);
                fprintf(_fp,"delay_expr_%d.p1 = %s%d.ctrl;\n",expr_inst_id,conn_block_prefix,block_id);

            }
        }
        else { // dataless action
            fprintf(_fp,"%s%d.zero = %s;\n",ring_block_prefix,block_id, chan_name);
        }
        break;

    case ACT_CHP_RECV:
        chan = c->u.comm.chan;
        var = c->u.comm.var;
        get_true_name (chan_name, chan, _p->CurScope());
        // chan_name = chan->rootVx(p->CurScope())->getName();
        fprintf(_fp,"\n// Pipe block for action: ");
        chp_print(_fp,c);
        fprintf(_fp,"\n");
        fprintf(_fp,"elem_c_ppa_brs_bd %s%d;\n",ring_block_prefix,block_id);
        if (var) {
            it = _p->CurScope()->Lookup(chan);
            bw = TypeFactory::bitWidth(it);
            fprintf(_fp,"connect_inchan_to_ctrl<%d> %s%d;\n",bw, conn_block_prefix,block_id);
            fprintf(_fp,"%s%d.ctrl = %s%d.zero;\n",conn_block_prefix,block_id,ring_block_prefix,block_id);
            fprintf(_fp,"%s%d.ch = %s;\n",conn_block_prefix,block_id,chan_name);

            fprintf(_fp,"\n// Data for action: ");
            chp_print(_fp,c);
            fprintf(_fp,"\n");
            char tname[1024];
            get_true_name(tname, var, _p->CurScope());
            b = hash_lookup(var_infos, tname);
            vi = (var_info *)b->v;
            if (init_latch == -1)
            {
                latch_id = _generate_single_latch(vi);
            }
            else 
            {
                latch_id = init_latch;
            }
            fprintf(_fp, "%s%s_%d.go = %s%d.data;\n",capture_block_prefix,
                            vi->name,latch_id,ring_block_prefix,block_id);
            fprintf(_fp, "%s%s_%d.din = %s.d;\n",capture_block_prefix,
                                            vi->name,latch_id,chan_name);
        }
        else { // dataless action
            fprintf(_fp,"%s%d.zero = %s;\n",ring_block_prefix,block_id, chan_name);
        }
        break;

    case ACT_CHP_SKIP:
        fprintf(_fp,"\n// Pipe block for action: ");
        chp_print(_fp,c);
        fprintf(_fp,"\n");
        fprintf(_fp,"elem_c_skip %s%d;\n",ring_block_prefix,block_id);
        break;

    default:
        fatal_error("Shouldn't be here... (generate_pipe_element)");
        break;
    }
    return block_id;
}

/*
    Similar to the previous, but used to receive and send
    initial conditions / loop-carried dependencies from 
    one iteration of the ring to the next. 
*/
int RingForge::_generate_pipe_element_custom(int bd_chan_id, int type, int width, ActId *var_init)
{
    Expr *e = NULL;
    int block_id;
    char chan_name[1024];
    int latch_id;
    int bw;
    hash_bucket_t *b;
    var_info *vi;

    block_id = _gen_block_id();
    Assert (var_init, "no variable (_generate_pipe_element_custom)");

    switch(type)
    {
    case ACT_CHP_SEND:
        snprintf(chan_name, 1024, "%s%d",init_cond_chan_prefix,bd_chan_id);
        fprintf(_fp,"\n// Pipe block for init cond. send.");
        fprintf(_fp,"\n");
        fprintf(_fp,"elem_c_paa_brs_bd %s%d;\n",ring_block_prefix,block_id);
        bw = width;
        fprintf(_fp,"connect_outchan_to_ctrl<%d> %s%d;\n",bw, conn_block_prefix,block_id);
        fprintf(_fp,"%s%d.ch = %s;\n",conn_block_prefix,block_id,chan_name);
        fprintf(_fp,"%s%d.ctrl = %s%d.zero;\n",conn_block_prefix,block_id,ring_block_prefix,block_id);
        char tname[1024];
        get_true_name(tname, var_init, _p->CurScope());
        b = hash_lookup(var_infos, tname);
        vi = (var_info *)b->v;
        latch_id = vi->latest_for_read;
        Assert ((latch_id>=0),"variable read but never written? what...");
        fprintf(_fp, "\n%s%s_%d.dout = %s.d;\n",capture_block_prefix,
                                    vi->name,latch_id,chan_name);
        vi->iread++;
        break;

    case ACT_CHP_RECV:
        snprintf(chan_name, 1024, "%s%d",init_cond_chan_prefix,bd_chan_id);
        fprintf(_fp,"\n// Pipe block for init cond. recv.\n");
        fprintf(_fp,"elem_c_ppa_brs_bd %s%d;\n",ring_block_prefix,block_id);
        bw = width;
        fprintf(_fp,"connect_inchan_to_ctrl<%d> %s%d;\n",bw, conn_block_prefix,block_id);
        fprintf(_fp,"%s%d.ctrl = %s%d.zero;\n",conn_block_prefix,block_id,ring_block_prefix,block_id);
        fprintf(_fp,"%s%d.ch = %s;\n",conn_block_prefix,block_id,chan_name);
        char tname[1024];
        get_true_name(tname, var_init, _p->CurScope());
        b = hash_lookup(var_infos, tname);
        vi = (var_info *)b->v;
        latch_id = _generate_single_latch(vi);
        fprintf(_fp, "%s%s_%d.go = %s%d.data;\n",capture_block_prefix,
                        vi->name,latch_id,ring_block_prefix,block_id);
        fprintf(_fp, "%s%s_%d.din = %s.d;\n",capture_block_prefix,
                                        vi->name,latch_id,chan_name);
        break;

    default:
        fatal_error("Shouldn't be here... (generate_pipe_element_custom)");
        break;
    }
    return block_id;
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
    Generate an initial condition handling ITB to
    wrap around the main ring.
*/
int RingForge::_generate_init_cond_itb(int value, int width, int chan_id_out, int chan_id_in)
{
    fprintf(_fp,"\n// Initial token buffer for initial condition transmission\n");
    int id = _gen_itb_wrapper_id();
    fprintf(_fp,"itb_wrapper<%d,%d,%d,%d> itb_w_%d(%s%d,%s%d);\n",capture_delay,pulse_width,width,value,
            id, init_cond_chan_prefix,chan_id_out,init_cond_chan_prefix,chan_id_in);
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
    Function to call an external logic synthesis tool to
    generate combinational logic to implement guard
    evaluators. Currently supports only abc. 
*/
int RingForge::_generate_expr_block_for_sel(Expr *e, int xid)
{
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
    }
    // no dots
    config_set_int("expropt.verbose", 0);

    int out_expr_width = 1;

    fprintf(stdout, "\n");

    // run abc, then v2act to create the combinational-logic-for-math process
    ExprBlockInfo *ebi = eeo->run_external_opt(xid, out_expr_width, e, all_leaves, _inexprmap, _inwidthmap);
    
    Assert ((ebi->delay_typ != -1), "Delay not extracted by abc!");
    double typ_delay_ps = (ebi->delay_typ)*1e12;
    int delay_line_n = int((typ_delay_ps/(2*invx1_delay_ps)) + 1); 
    if (delay_line_n == 0) { delay_line_n = 1; }

    fprintf(_fp, "\n// typical delay: %gps\n",typ_delay_ps);
    _instantiate_expr_block (xid, all_leaves);

    eeo->~ExternalExprOpt();
    ebi->~ExprBlockInfo();

    return delay_line_n;
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
    Generate a bundled data channel of a given width.
*/
int RingForge::_generate_bd_chan(int width)
{
    int id = _gen_bd_chan_id();
    fprintf (_fp, "bd<%d> %s%d;\n",width,init_cond_chan_prefix,id);
    return id;
}

/*
    Generate a dataless synchronization channel. 
*/
int RingForge::_generate_sync_chan()
{
    int id = _gen_sync_chan_id();
    fprintf(_fp,"a1of1 %s%d;\n",sync_chan_name_prefix,id);
    return id;
}

/*
    Synthesis of linear programs. Generates a sequence of pipeline
    elements, according to the actions in the CHP program, and ties 
    them up into a ring using an initial token buffer. If initial 
    conditions exist, processes them according to the optimized
    handling method, where the last assignment actions connect to 
    latches that are initialized to the initial value.
*/
int RingForge::generate_one_ring(act_chp_lang_t *c, int root, int prev_block_id)
{
    int block_id;
    int first_block_id;
    int init_chan, lcd_chan;
    int init_latch = -1;
    list_t *lcd_chan_list, *lcd_var_list;
    list_t *tag_list = NULL;
    var_info *vi;
    ActId *id;
    listitem_t *li, *lj, *lk;
    act_chp_gc_t *gc;
    act_chp_lang_t *stmt, *main_loop;
    
    if (!c) { return prev_block_id; }

    switch(c->type)
    {
    case ACT_CHP_COMMALOOP:
    case ACT_CHP_SEMILOOP:
        fatal_error ("Replication loops should've been removed.. (generate_one_ring)");
        break;
    case ACT_CHP_COMMA:
        fatal_error ("No commas allowed.. (generate_one_ring)");
        break;

    case ACT_CHP_SEMI:
        // initial condition handling
        if (root == 1)
        {              
            // find main loop
            for (li = list_first (c->u.semi_comma.cmd); li; li = list_next (li)) 
            {   
                stmt = (act_chp_lang_t *)list_value(li);
                if (stmt->type == ACT_CHP_LOOP)
                    main_loop = (act_chp_lang_t *)list_value(li);
            }
            first_block_id = _generate_itb();
            prev_block_id = first_block_id;

            // new I.C. handling method -----
            #if 0
            lcd_var_list = list_new();
            // loop through initial condition assignments to create latches with correct initial values
            for (lj = list_first (c->u.semi_comma.cmd); lj; lj = list_next (lj)) 
            {
                act_chp_lang_t *stmt1 = (act_chp_lang_t *)list_value(lj);
                if (stmt1->type != ACT_CHP_LOOP)
                {
                    Assert (stmt1->type == ACT_CHP_ASSIGN, "Only assignments in initial conditions");
                    id = stmt1->u.assign.id;
                    Expr *e = stmt1->u.assign.e;
                    Assert (e->type == E_INT, "Constants only in initial conditions");
                    int ival = e->u.ival.v;
                    char tname[1024];
                    get_true_name(tname, id, p->CurScope());
                    hash_bucket_t *b = hash_lookup(var_infos, tname);
                    // hash_bucket_t *b = hash_lookup(var_infos, id->rootVx(p->CurScope())->getName());
                    vi = (var_info *)b->v;
                    int latch_id = generate_single_latch (fp, vi, ival);
                    list_append(lcd_var_list, vi->name);
                }
            }

            tag_lcds(main_loop, p, lcd_var_list);
            #endif
            // new --------------------------

            // old ---------
            lcd_chan_list = list_new();
            // loop through initial condition assignments to create ITBs and receives
            for (lj = list_first (c->u.semi_comma.cmd); lj; lj = list_next (lj)) 
            {   
                act_chp_lang_t *stmt1 = (act_chp_lang_t *)list_value(lj);
                if (stmt1->type != ACT_CHP_LOOP)
                {
                    Assert (stmt1->type == ACT_CHP_ASSIGN, "Only assignments in initial conditions");
                    id = stmt1->u.assign.id;
                    Expr *e = stmt1->u.assign.e;
                    Assert (e->type == E_INT, "Constants only in initial conditions");
                    int ival = e->u.ival.v;
                    char tname[1024];
                    get_true_name(tname, id, _p->CurScope());
                    hash_bucket_t *b = hash_lookup(var_infos, tname);
                    // hash_bucket_t *b = hash_lookup(var_infos, id->rootVx(p->CurScope())->getName());
                    vi = (var_info *)b->v;
                    init_chan = _generate_bd_chan (vi->width);
                    lcd_chan = _generate_bd_chan (vi->width);
                    list_iappend(lcd_chan_list, lcd_chan);
                    int itb_block_id = _generate_init_cond_itb (ival, vi->width, init_chan, lcd_chan);

                    block_id = _generate_pipe_element_custom (init_chan, ACT_CHP_RECV, vi->width, id);
                    _connect_pipe_elements (prev_block_id, block_id);
                    prev_block_id = block_id;
                }
            }
            // old ---------

            // main program synthesis
            gc = main_loop->u.gc;
            block_id = generate_one_ring(gc->s, 0, prev_block_id);
            prev_block_id = block_id;

            // old ---------
            lk = list_first(lcd_chan_list);
            // loop through initial condition assignments again to create loop-carried dependency sends
            for (lj = list_first (c->u.semi_comma.cmd); lj; lj = list_next (lj)) 
            {   
                act_chp_lang_t *stmt1 = (act_chp_lang_t *)list_value(lj);
                if (stmt1->type != ACT_CHP_LOOP)
                {
                    Assert (stmt1->type == ACT_CHP_ASSIGN, "Only assignments in initial conditions");
                    id = stmt1->u.assign.id;
                    char tname[1024];
                    get_true_name(tname, id, _p->CurScope());
                    hash_bucket_t *b = hash_lookup(var_infos, tname);
                    vi = (var_info *)b->v;
                    lcd_chan = list_ivalue(lk);
                    block_id = _generate_pipe_element_custom (lcd_chan, ACT_CHP_SEND, vi->width, id);
                    _connect_pipe_elements (prev_block_id, block_id);
                    prev_block_id = block_id;
                    lk = list_next(lk);
                }
            }
            // old ---------

            _connect_pipe_elements(block_id, first_block_id);
            break;

        }
        // regular synthesis
        else {
            for (li = list_first (c->u.semi_comma.cmd); li; li = list_next (li)) 
            {
                block_id = generate_one_ring ((act_chp_lang_t *)list_value(li), 0, prev_block_id);
                prev_block_id = block_id;
            }
        }
        break;

    case ACT_CHP_LOOP:
    case ACT_CHP_DOLOOP:
        if (root == 1)
        {   
            first_block_id = _generate_itb();
            gc = c->u.gc;
            block_id = generate_one_ring(gc->s, 0, first_block_id);
            _connect_pipe_elements(block_id, first_block_id);
            break;
        }
        else
        {
            fatal_error ("should've excised internal loops.. (generate_one_ring)");
        }
        break;
        
    case ACT_CHP_SELECT:
        fprintf(_fp, "\n// WARNING: single guard selection in program - hope you know what you're doing :)\n\n");
        gc = c->u.gc;
        block_id = generate_one_ring (gc->s, 0, prev_block_id);
        break;

    case ACT_CHP_SELECT_NONDET:
        fatal_error ("Can't handle NDS in generate_one_ring");
        
    case ACT_CHP_SKIP:
    case ACT_CHP_ASSIGN:
    case ACT_CHP_ASSIGNSELF:
    case ACT_CHP_SEND:
    case ACT_CHP_RECV:
        // only for the new I.C. handling method
        if (c->space) 
        {
            tag_list = (list_t *)c->space;
            init_latch = list_ivalue(list_first(tag_list));
            Assert (init_latch>-1, "wut");
        }// if (tag_list) 
        block_id = _generate_pipe_element(c, init_latch);
        _connect_pipe_elements(prev_block_id, block_id);
        break;

    case ACT_CHP_FUNC:
    case ACT_CHP_HOLE: /* to support verification */
    case ACT_CHP_MACRO:
    case ACT_HSE_FRAGMENTS:
        break;

    default:
        fatal_error ("Unknown type in generate_one_ring");
        break;
    }

    return block_id;
}

/*
    General synthesis for branched programs. Generates a branched ring
    consisting of pipeline elements, splits and merges. If initial 
    conditions exist, processes them by creating an ITB to wrap around
    the branched ring (non-trivial initial conditions and loop
    carried dependencies are inextricably linked). 
*/
int RingForge::generate_branched_ring(act_chp_lang_t *c, int root, int prev_block_id, int connect_prev)
{
    int block_id, expr_block_id, first_block_id;
    int pll_split_block_id, pll_merge_block_id;
    int sel_split_block_id, sel_merge_block_id;
    int delay_merge_block_id;
    int comma_len, gc_len;
    int pll_port;
    int delay_n_sel, max_delay_n_sel, delay_n_merge;
    list_t *live_vars;
    list_t *gp_connect_ids;
    listitem_t *li, *lj, *lk;
    act_chp_gc_t *gc;
    act_chp_lang_t *stmt, *main_loop;
    int init_chan, lcd_chan;
    list_t *lcd_chan_list;
    var_info *vi;
    ActId *id;

    if (!c) { return prev_block_id; }

    switch(c->type)
    {
    case ACT_CHP_COMMALOOP:
    case ACT_CHP_SEMILOOP:
        fatal_error ("Replication loops should've been removed.. (generate_branched_ring)");
        break;
    case ACT_CHP_COMMA:
        if (root == 1)
        { 
            fatal_error ("Only semi-colon list of initializations... (generate_branched_ring)"); 
        }
        else 
        {
        comma_len = list_length(c->u.semi_comma.cmd);
        fprintf (_fp, "// %d-way parallel split for actions: ",comma_len);
        chp_print(_fp, c);
        fprintf (_fp, "\n");

        fprintf (_fp, "// %d-way parallel merge \n",comma_len);
        pll_split_block_id = _generate_parallel_split(comma_len);
        pll_merge_block_id = _generate_parallel_merge(comma_len);
        // connect_pipe_to_pll_split_input(fp, pll_split_block_id, prev_block_id);
        _connect_pipe_elements(prev_block_id, pll_split_block_id);

        pll_port = 0;
        for (li = list_first (c->u.semi_comma.cmd); li; li = list_next (li)) 
        {
            block_id = generate_branched_ring ((act_chp_lang_t *)list_value(li), 0, pll_split_block_id, 0);
            _connect_pll_split_outputs_to_pipe (pll_split_block_id, block_id, pll_port);
            _connect_pipe_to_pll_merge_inputs (pll_merge_block_id, block_id, pll_port);
            pll_port++;
        }

        block_id = pll_merge_block_id;
        }
        break;

    case ACT_CHP_SEMI:
        if (root == 1)
        {              
            // find main loop
            for (li = list_first (c->u.semi_comma.cmd); li; li = list_next (li)) 
            {   
                stmt = (act_chp_lang_t *)list_value(li);
                if (stmt->type == ACT_CHP_LOOP)
                    main_loop = (act_chp_lang_t *)list_value(li);
            }

            first_block_id = _generate_itb();
            prev_block_id = first_block_id;

            // loop through initial condition assignments to create ITBs and receives
            lcd_chan_list = list_new();
            for (lj = list_first (c->u.semi_comma.cmd); lj; lj = list_next (lj)) 
            {   
                act_chp_lang_t *stmt1 = (act_chp_lang_t *)list_value(lj);
                if (stmt1->type != ACT_CHP_LOOP)
                {
                    Assert (stmt1->type == ACT_CHP_ASSIGN, "Only assignments in initial conditions");
                    id = stmt1->u.assign.id;
                    Expr *e = stmt1->u.assign.e;
                    Assert (e->type == E_INT, "Constants only in initial conditions");
                    int ival = e->u.ival.v;
                    char tname[1024];
                    get_true_name(tname, id, _p->CurScope());
                    hash_bucket_t *b = hash_lookup(var_infos, tname);
                    vi = (var_info *)b->v;
                    init_chan = _generate_bd_chan (vi->width);
                    lcd_chan = _generate_bd_chan (vi->width);
                    list_iappend(lcd_chan_list, lcd_chan);
                    int itb_block_id = _generate_init_cond_itb (ival, vi->width, init_chan, lcd_chan);

                    block_id = _generate_pipe_element_custom (init_chan, ACT_CHP_RECV, vi->width, id);
                    _connect_pipe_elements (prev_block_id, block_id);
                    prev_block_id = block_id;
                }
            }

            // main program synthesis
            gc = main_loop->u.gc;
            block_id = generate_branched_ring(gc->s, 0, prev_block_id, 0);
            prev_block_id = block_id;

            // loop through initial condition assignments again to create loop-carried dependency sends
            lk = list_first(lcd_chan_list);
            for (lj = list_first (c->u.semi_comma.cmd); lj; lj = list_next (lj)) 
            {   
                act_chp_lang_t *stmt1 = (act_chp_lang_t *)list_value(lj);
                if (stmt1->type != ACT_CHP_LOOP)
                {
                    Assert (stmt1->type == ACT_CHP_ASSIGN, "Only assignments in initial conditions");
                    id = stmt1->u.assign.id;
                    char tname[1024];
                    get_true_name(tname, id, _p->CurScope());
                    hash_bucket_t *b = hash_lookup(var_infos, tname);
                    vi = (var_info *)b->v;
                    lcd_chan = list_ivalue(lk);
                    block_id = _generate_pipe_element_custom (lcd_chan, ACT_CHP_SEND, vi->width, id);
                    _connect_pipe_elements (prev_block_id, block_id);
                    prev_block_id = block_id;
                    lk = list_next(lk);
                }
            }
            _connect_pipe_elements(block_id, first_block_id);
            break;
        }
        // regular synthesis
        else {
            for (li = list_first (c->u.semi_comma.cmd); li; li = list_next (li)) 
            {
                block_id = generate_branched_ring ((act_chp_lang_t *)list_value(li), 0, prev_block_id, 0);
                if (is_elementary_action((act_chp_lang_t *)list_value(li)) == 1)
                {
                    _connect_pipe_elements(prev_block_id, block_id);
                }
                prev_block_id = block_id;
            }
        }
        break;

    case ACT_CHP_LOOP:
    case ACT_CHP_DOLOOP:
        if (root == 1)
        {
            first_block_id = _generate_itb();
            gc = c->u.gc;
            block_id = generate_branched_ring(gc->s, 0, first_block_id, 0);
            _connect_pipe_elements(block_id, first_block_id);
            break;
        }
        else
        {
            fatal_error ("should've excised internal loops.. (generate_branched_ring)");
        }
        break;
        
    case ACT_CHP_SELECT:
        // fatal_error ("not supported yet");
        gc = c->u.gc;
        gc_len = length_of_guard_set (c);
        max_delay_n_sel = 0;

        fprintf (_fp, "\n// %d-way selection split for : ", gc_len);
        chp_print(_fp, c);
        fprintf (_fp, "\n");
        fprintf (_fp, "// %d-way selection merge \n", gc_len);
        sel_split_block_id = _generate_selection_split(gc_len);
        sel_merge_block_id = _generate_selection_merge(gc_len);

        live_vars = (list_t *)c->space;
        // fprintf (fp, "\n\nlive vars at merge:");
        // print_live_vars_temp(live_vars);
        save_var_infos();

        gp_connect_ids = list_new();
        for (int i = 0; gc; gc = gc->next)
        {
            // branch_id++;
            if (gc->g)
            {   
                expr_block_id = _gen_expr_block_id();
                delay_n_sel = _generate_expr_block_for_sel (gc->g, expr_block_id);
                if (max_delay_n_sel < delay_n_sel) max_delay_n_sel = delay_n_sel;
            }
            else
            {   // compute the else guard .. 
                fatal_error ("should've been fixed in else generation");
            }
            _connect_guards_to_sel_split_input (sel_split_block_id, expr_block_id, i);
            block_id = _generate_gp_connect ();
            _connect_sel_split_outputs_to_pipe (sel_split_block_id, block_id, i);
            list_iappend(gp_connect_ids, block_id);
            i++;
        }
        lj = list_first(gp_connect_ids);
        gc = c->u.gc;
        for (int i = 0; gc; gc = gc->next)
        {   
            branch_id++;
            save_read_ids();
            block_id = generate_branched_ring (gc->s, 0, list_ivalue(lj), 1);
            _connect_pipe_to_sel_merge_inputs (sel_merge_block_id, block_id, i);
            restore_read_ids();
            i++; lj = list_next(lj);
        }
        branch_id = branch_id - gc_len;

        // muxing variables live-out of merge so downstream can access correctly
        delay_n_merge = _compute_merge_mux_info(live_vars, gc_len, sel_merge_block_id);

        // generate delay line for max guard evaluator delay (split)
        fprintf(_fp,"\n// Delaying pre-split-block sync. by max. delay of all guard evaluators\n");
        fprintf(_fp,"delay_line_chan<%d> delay_select_%d;\n",max_delay_n_sel,sel_split_block_id);
        // connect prev. block p1 to delay_line then connect to select block from the output
        fprintf(_fp,"delay_select_%d.m1 = %s%d.p1;\n",sel_split_block_id,ring_block_prefix,prev_block_id);
        fprintf(_fp,"delay_select_%d.p1 = %s%d.m1;\n",sel_split_block_id,ring_block_prefix,sel_split_block_id);

        if (delay_n_merge > 0)
        {
            delay_merge_block_id = _gen_block_id();
            fprintf(_fp,"\n// Delaying post-merge-block sync. by max. delay of all merge muxes\n");
            fprintf(_fp,"delay_line_chan<%d> %s%d;\n",delay_n_merge,ring_block_prefix, delay_merge_block_id);
            fprintf(_fp,"%s%d.m1 = %s%d.p1;\n",ring_block_prefix,delay_merge_block_id,
                                                ring_block_prefix,sel_merge_block_id);
            // tail is the delay_merge block
            block_id = delay_merge_block_id;
        }
        else 
        {
            block_id = sel_merge_block_id;
        }
        break;

    case ACT_CHP_SELECT_NONDET:
        fatal_error ("Can't handle NDS in generate_branched_ring");
        
    case ACT_CHP_SKIP:
    case ACT_CHP_ASSIGN:
    case ACT_CHP_ASSIGNSELF:
    case ACT_CHP_SEND:
    case ACT_CHP_RECV:
        // do stuff
        block_id = _generate_pipe_element(c, -1);
        if (connect_prev == 1)
        {
            _connect_pipe_elements(prev_block_id, block_id);
        }
        break;

    case ACT_CHP_FUNC:
    case ACT_CHP_HOLE: /* to support verification */
    case ACT_CHP_MACRO:
    case ACT_HSE_FRAGMENTS:
        break;

    default:
        fatal_error ("Unknown type in generate_branched_ring");
        break;
    }
    return block_id;
}

/*
    Generate merging muxes to be placed when exiting selections,
    so that variables that are assigned in one/many branches inside
    a selection can be addressed correctly when exiting the selection, 
    based on which branch was taken in this iteration of the loop.
*/
int RingForge::_compute_merge_mux_info (list_t *live_vars, int n_branches, int merge_block_id)
{
    var_info *vi, *vi_pre;
    hash_bucket_t *b, *b_pre;
    listitem_t *li, *lj;
    list_t *latch_branches;
    int iwrite, iwrite_pre;
    int latest_branch_id;
    int latest_branch_id_prev = -1;
    int branch_ctr = 0;
    int max_mux_size = 0;
    int max_or_size = 0;

    if ( list_isempty(live_vars) ) return 0;

    for ( li = list_first(live_vars) ; li ; li = list_next(li) )
    {
        b = hash_lookup (var_infos, (const char *)list_value(li));
        if (!b) fatal_error ("variable not found - whatt");
        b_pre = hash_lookup (var_infos_copy, (const char *)list_value(li));
        if (!b_pre) fatal_error ("variable not found - whatt");

        vi = (var_info *)b->v;
        vi_pre = (var_info *)b_pre->v;
        latch_branches = vi->latest_latch_branches;
        iwrite = (vi->iwrite)-1;
        iwrite_pre = (vi_pre->iwrite)-1;

        Assert ((iwrite>=0), "hmmst");
        fprintf (_fp, "\n// variable: %s\n", vi->name);
        lj = list_first (latch_branches);
        list_t *branch_map = list_new();

        for ( int i=0 ; i < (iwrite-iwrite_pre) ; i++ )
        {
            latest_branch_id = int(list_ivalue(lj));
            // all branch check
            if (latest_branch_id != latest_branch_id_prev) 
            {
                list_iappend_head (branch_map, iwrite-i);
                list_iappend_head (branch_map, latest_branch_id-branch_id-1);
                branch_ctr++;
                // fprintf (fp, "// latch branch id %d, merge port id %d, latch id %d\n", 
                //                     latest_branch_id, latest_branch_id-branch_id-1, iwrite-i);
            }
            lj = list_next (lj);
            latest_branch_id_prev = latest_branch_id;
            // match latches to branches
        }

        int need_mux, need_or, mux_size, or_size;

        // compare with n_branches, see if OR-gate is needed
        if (branch_ctr == n_branches)
        {
            fprintf(_fp, "// assigned in all branches\n"); 
            need_mux = 1; need_or = 0; mux_size = branch_ctr; or_size = 0;
        }
        else if (branch_ctr == 0)
        { 
            fprintf(_fp, "// not assigned in any branch\n"); 
            need_mux = 0; need_or = 0; mux_size = 0; or_size = 0;
        }
        else
        { 
            fprintf(_fp, "// not assigned in all branches\n"); 
            need_mux = 1; need_or = 1; mux_size = branch_ctr+1; or_size = n_branches-branch_ctr;
        }
        // find the variable with the biggest mux+or combo (lookup TODO)
        {
            if (max_mux_size < mux_size) max_mux_size = mux_size;
            if (max_or_size < or_size) max_or_size = or_size;
        }

        list_t *unassigned_branches = list_new();
        // collect unassigned branch ids for OR-gate
        if ( need_or )
        {
            for ( int i=0 ; i<n_branches ; i++ )
            {
                int flag=0;
                for ( lj = list_first(branch_map) ; lj ; lj = list_next(list_next(lj)) )
                {
                    int mux_port = list_ivalue(lj)+branch_id;
                    if (mux_port == i)
                    { flag=1; break; }
                }
                if (flag==0)
                {
                    list_iappend(unassigned_branches, i);
                    fprintf(_fp, "// unassigned in branch %d\n", i);
                }
            }
        }
        Assert ((list_length(unassigned_branches) == or_size), "what the..");

        // generate the mux if needed (looks like latch to downstream) and connect latch outputs correctly
        int mux_id, i;
        int j = 0;
        if (need_mux) 
        {   
            mux_id = _gen_mux_block_id();
            fprintf (_fp, "merge_mux_ohc_opt<%d,%d> %s%s_%d;\n", mux_size, vi->width, 
                                                    capture_block_prefix, vi->name, iwrite+1);

            // increase nwrite and iwrite for the variable so it can be connected to correctly downstream
            vi->iwrite++; vi->nwrite++;
            vi->latest_for_read = (vi->iwrite)-1;

            lj = list_first(branch_map);
            for ( int i=0 ; i<mux_size ; i++ )
            {
                // generate OR-gate if needed and connect to last input of mux, on last iteration
                if (need_or && i == mux_size-1)
                {   
                    // generate OR-gate
                    fprintf (_fp, "std::gates::ortree<%d, false> or_%s_%d;\n", or_size, vi->name, mux_id);
                    // connect OR-gate inputs (merge_block inputs)
                    for (listitem_t *lk = list_first(unassigned_branches) ; lk ; lk = list_next(lk))
                    {
                        fprintf (_fp, "or_%s_%d.in[%d] = %s%d.ci[%d].r;\n", vi->name, mux_id, j, 
                                            ring_block_prefix, merge_block_id, list_ivalue(lk));
                        j++;
                    }
                    // connect pre-split data to mux last data input
                    fprintf (_fp, "%s%s_%d.din[%d][0..%d] = %s%s_%d.dout;\n", capture_block_prefix, vi->name, 
                                                        iwrite+1, i, (vi->width)-1,
                                            capture_block_prefix, vi->name, iwrite_pre);
                    // connect OR-gate output to mux input
                    fprintf (_fp, "or_%s_%d.out = %s%s_%d.c[%d];\n", vi->name, mux_id,
                                            capture_block_prefix, vi->name, iwrite+1, i);
                    break;
                }

                // connect mux input control and data
                fprintf (_fp, "%s%s_%d.c[%d] = %s%d.ci[%d].r;\n", capture_block_prefix, vi->name, 
                                                    iwrite+1, i, ring_block_prefix, 
                                                        merge_block_id, list_ivalue(lj));
                fprintf (_fp, "%s%s_%d.din[%d][0..%d] = %s%s_%d.dout;\n",capture_block_prefix, vi->name, 
                                                    iwrite+1, i, (vi->width)-1,
                                        capture_block_prefix, vi->name, list_ivalue(list_next(lj)));
                lj = list_next(list_next(lj));
            }
        }
        branch_ctr = 0;
    }
    float max_delay = _lookup_mux_delays (max_mux_size, max_or_size);
    Assert ((max_delay != -1), "mux lookup out of range" );
    // fprintf (fp, "\nmax mux delay: %f", max_delay);
    // fprintf (fp, "\nmax mux size: %d", max_mux_size);
    // fprintf (fp, "\nmax or size: %d", max_or_size);
    return int(max_delay/(2*invx1_delay_ps)) + 1;
}

/*
    Temp: lookup the mux delay table
*/
float RingForge::_lookup_mux_delays (int mux_sz, int or_sz)
{
    if (mux_sz <= max_mux_size && or_sz <= max_or_size && mux_sz>0 && or_sz>=0)
    {
        return mux_delays[mux_sz-1][or_sz];
    }
    return -1;
}



/*
*/



