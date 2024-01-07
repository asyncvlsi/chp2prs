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
*/



