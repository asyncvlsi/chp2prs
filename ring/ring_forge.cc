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
#include <cmath>

#define SSA 0
#define NON_SSA 1

RingForge::RingForge ( FILE *fp, Process *p, act_chp_lang_t *c,
            ActBooleanizePass *bp, int bdpath,
            int delay_margin, int dp_style,
            const char *circuit_library,
            const char *exprfile )
    : RingEngine ( fp, p, c, bp, circuit_library, exprfile )
{
    ring_block_prefix = "block_";
    conn_block_prefix = "conn_z_";

    // Datapath name prefixes
    capture_block_prefix = "latch_";
    expr_block_prefix = "blk_";
    expr_block_instance_prefix = "inst_";
    expr_block_input_prefix = "in_";
    expr_block_output_prefix = "conn_out_";
    var_access_prefix = "va_blk_";

    bundled = bdpath;
    datapath_style = dp_style;

    // Channel name prefixes
    sync_chan_name_prefix = "sync_";
    init_cond_chan_prefix = "C_init_";

    // Bundled datapath parameters
    // invx1_delay_ps = 21;
    // capture_delay = 5; // 2*n = 10 inverters in delay-line
    // pulse_width = 6; // 2*n+1 = 13 inverters in pulse generator
    _delay_margin = delay_margin;
    delay_multiplier = float(_delay_margin)/100;

    // Instance counters
    _block_id = 0;
    _itb_wrapper_id = 0;
    _bd_chan_id = 0;
    _sync_chan_id = 0;
    _expr_id = 0;
    // _expr_block_id = 0;
    _mux_block_id = 0;
    _branch_id = 0;
}

void RingForge::run_forge ()
{
    /* Handling
     * 'everything else besides the chp body'
     * needs to be added here
    */
    if (_c->label && (strcmp(_c->label,"top_decomp")==0)) {
        Assert ((_c->type == ACT_CHP_COMMA)||
                (_c->type == ACT_CHP_LOOP)||
                (_c->type == ACT_CHP_DOLOOP), "top-level not loop or parallel loops?");
        if (_c->type == ACT_CHP_COMMA) {
            for (listitem_t *li = list_first (_c->u.semi_comma.cmd) ; li ; li = list_next(li))
            {
                _run_forge_helper ((act_chp_lang_t *)(list_value(li)));
            }
        }
        else {
            _run_forge_helper (_c);
        }
    }
    else {
        _run_forge_helper (_c);
    }
}

void RingForge::_run_forge_helper (act_chp_lang_t *c)
{
    bool printt = false;
    LiveVarAnalysis *lva = new LiveVarAnalysis (_fp, _p, c);
    // yes, run twice :)
    lva->generate_live_var_info();
    lva->generate_live_var_info();

    if (printt) fprintf (_fp, "\n/* \n");
    if (printt) fprintf (_fp, "// Live Vars Info -----------------\n");
    if (printt) lva->print_live_var_info();
    if (printt) fprintf (_fp, "// --------------------------------\n\n");

    construct_var_infos (c);

    if (datapath_style==SSA) 
    {
        if (printt) fprintf (_fp, "// Read/Write Info pre-mux gen ----\n");
        if (printt) print_var_infos (_fp);
        if (printt) fprintf (_fp, "// --------------------------------\n\n");

        _construct_merge_latch_info (c, 1);
        
        if (printt) fprintf (_fp, "// Read/Write Info post-mux gen ---\n");
        if (printt) print_var_infos (_fp);
        if (printt) fprintf (_fp, "// --------------------------------\n\n");

        compute_mergemux_info (c);

        if (printt) fprintf (_fp, "// Merge Mux Info pre-mapping -----\n");
        if (printt) print_merge_mux_infos(_fp, c);
        if (printt) fprintf (_fp, "// --------------------------------\n\n");
        
        flow_assignments (c);

        if (printt) fprintf (_fp, "// Merge Mux Info post-mapping ----\n");
        if (printt) print_merge_mux_infos(_fp, c);
        if (printt) fprintf (_fp, "// --------------------------------\n\n");
        if (printt) fprintf (_fp, "\n*/ \n");

        if ( _check_all_muxes_mapped(c, false) ) { 
            fprintf (stderr, "Merge muxes were not fully mapped (SSA-style datapath). \n");
            fprintf (stderr, "This is most likely due to uninitialized \nvariables outside your main loop.\n");
            fprintf (stderr, "Please add initial conditions to requisite variables.\n\n");
            fatal_error("Mux input mapping incomplete."); 
        }

        fprintf (_fp, "// Branched Ring ------------------\n");
        generate_branched_ring (c,1,0,0);
    }
    else {
        if ( _check_no_self_assignments(c, false) ) { 
            fprintf (stderr, "Self assignments detected (Non SSA-style datapath). \n");
            fprintf (stderr, "This is not currently supported with this datapath style.\n");
            fprintf (stderr, "Work in progress.\n\n");
            fprintf (stderr, "Rewrite x:=f(x) as x1:=f(x);x:=x1 \n\n");
            fatal_error("Self assignments detected."); 
        }

        fprintf (_fp, "// Branched Ring Non-SSA ----------\n");
        generate_branched_ring_non_ssa (c,1,0,0);
    }
}

unsigned long act_expr_getconst_long (Expr *e)
{
  if (!e) return 0;
  if (e->type == E_INT) {
    return e->u.ival.v;
  }
  else {
    fatal_error ("ints only in initial condition leaves");
    return 0;
  }
}

unsigned long eval_ic (Expr *);

bool eval_bool_expr (Expr *e)
{
    Assert (e, "huh");
    unsigned long vall, valr;
    switch (e->type) {
    case E_AND:
        return eval_bool_expr (e->u.e.l) && eval_bool_expr (e->u.e.r); break;
    case E_OR:
        return eval_bool_expr (e->u.e.l) || eval_bool_expr (e->u.e.r); break;
    case E_NOT:
        return !(eval_bool_expr (e->u.e.l)); break;
    case E_XOR:
        return (eval_bool_expr (e->u.e.l) ^ eval_bool_expr (e->u.e.r)); break;
    case E_LT:
        return (eval_ic(e->u.e.l) < eval_ic(e->u.e.r)); break;
    case E_GT:
        return (eval_ic(e->u.e.l) > eval_ic(e->u.e.r)); break;
    case E_LE:
        return (eval_ic(e->u.e.l) <= eval_ic(e->u.e.r)); break;
    case E_GE:
        return (eval_ic(e->u.e.l) >= eval_ic(e->u.e.r)); break;
    case E_EQ:
        return (eval_ic(e->u.e.l) == eval_ic(e->u.e.r)); break;
    case E_NE:
        return (eval_ic(e->u.e.l) != eval_ic(e->u.e.r)); break;
    case E_TRUE:
        return true; break;
    case E_FALSE:
        return false; break;
    default:
        print_uexpr (stdout, e);
        fprintf (stdout, "\ntype: %d\n", e->type);
        fatal_error ("could not evaluate above boolean expression for initial condition");
        break;
    }
    return false;
}

unsigned long eval_ic (Expr *q)
{
    unsigned long val;
    Assert (q, "huh");
    switch (q->type) {
    case E_INT:
        return act_expr_getconst_long(q); break;
    case E_BUILTIN_INT:
        warning ("Assuming your bitwidths are ok in expressions of the form int(x,v)");
        return eval_ic (q->u.e.l); break;
    case E_QUERY:
        if (eval_bool_expr(q->u.e.l)) {
            return eval_ic (q->u.e.r->u.e.l);
        }
        else {
            return eval_ic (q->u.e.r->u.e.r);
        }
        break;
    case E_PLUS:
        return eval_ic (q->u.e.l) + eval_ic (q->u.e.r); break;
    case E_MINUS:
        return eval_ic (q->u.e.l) - eval_ic (q->u.e.r); break;
    case E_MULT:
        return eval_ic (q->u.e.l) * eval_ic (q->u.e.r); break;
    case E_DIV:
        return eval_ic (q->u.e.l) / eval_ic (q->u.e.r); break;
    case E_MOD:
        return eval_ic (q->u.e.l) % eval_ic (q->u.e.r); break;
    default:
        print_uexpr (stdout, q);
        fprintf (stdout, "\ntype: %d\n", q->type);
        fatal_error ("could not evaluate above expression for initial condition");
        break;
    }
    return 0;
}

static bool _expr_has_probe (Expr *e)
{
  auto is_basic_probe =
    [] (const Expr *e) {
      return e->type == E_PROBE
	|| (e->type == E_AND && e->u.e.l->type == E_PROBE);
    };

  if (!e) return false;

  if (is_basic_probe (e)) {
    return true;
  }
  if (e->type == E_OR && is_basic_probe (e->u.e.l)) {
    return true;
  }
  return false;
}

static bool _guards_have_probes (act_chp_gc_t *gc)
{
  while (gc) {
    if (gc->g) {
      if (_expr_has_probe (gc->g)) {
	return true;
      }
    }
    gc = gc->next;
  }
  return false;
}

/*
    Generate a data capture element for a given variable.
    If provided with an initial value, generate a data capture
    element that initializes to that value on reset.
*/
int RingForge::_generate_single_latch (var_info *v, latch_info *l, long long init_val=-1)
{
    Assert (l->type == LatchType::Latch, "generate latch for non-assignment?");
    int latch_id = l->latch_number;
    if (v->iwrite < v->nwrite)
    {
        if (init_val == -1)
        {
            fprintf(_fp, "capture<%d,%d,%d> %s%s_%d;\n", int(std::ceil(capture_delay*delay_multiplier)), 
                                                         int(std::ceil(pulse_width*delay_multiplier)), 
                                                    v->width, capture_block_prefix, v->name,latch_id);
        }
        else
        {
            fprintf(_fp, "capture_init<%d,%d,%d,%lli> %s%s_%d;\n", int(std::ceil(capture_delay*delay_multiplier)), 
                                                                   int(std::ceil(pulse_width*delay_multiplier)), 
                                                    v->width, init_val, capture_block_prefix, v->name,latch_id);
        }
        v->iwrite++;
        v->latest_for_read = latch_id;
        return latch_id;
    }
    fatal_error("shouldn't have reached here (generate_single_latch)");
    return -1;
} 

/*
    Generate a data capture element for a given variable.
    If provided with an initial value, generate a data capture
    element that initializes to that value on reset.
    Non-SSA style datapath.
    // Note to self: using iread as flag to check against double creation of latch
*/
int RingForge::_generate_single_latch_non_ssa (var_info *v, long long init_val=0)
{
    int latch_id = 0;
    int write_ports = v->nwrite;
    if (write_ports==0) write_ports = 1;
    
    Assert (v->iread==0, "Already created latch for this variable?");
    if (!(v->fischan))
    {
        fprintf(_fp, "capture_init_non_ssa<%d,%d,%d,%lld,%d> %s%s_%d;\n", 
                            int(std::ceil(capture_delay*delay_multiplier)), 
                            int(std::ceil(pulse_width*delay_multiplier)), 
                            v->width, init_val, write_ports, 
                            capture_block_prefix, v->name,latch_id);

        v->iread++;
        v->latest_for_read = latch_id;
    }
    return latch_id;
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
        fprintf(_fp,"elem_c_paa %s%d;\n",ring_block_prefix,block_id);

        fprintf(_fp,"\n// Data for action: ");
        chp_print(_fp,c);
        fprintf(_fp,"\n");
        char tname[1024];
        get_true_name(tname, var, _p->CurScope());
        b = hash_lookup(var_infos, tname);
        Assert (b, "variable not found");
        // b = hash_lookup(var_infos, var->rootVx(p->CurScope())->getName());
        vi = (var_info *)b->v;
        bw = vi->width;
        expr_inst_id = _generate_expr_block(e,bw);
        Assert ((c->space), "No latch info? (_generate_pipe_element)");
        if (init_latch == -1)
        {
            latch_id = _generate_single_latch(vi, (latch_info_t *)(c->space));  
        }
        else 
        {
            latch_id = init_latch;
            Assert (datapath_style==NON_SSA, "Hmm");
        }
        // connect output of math block to latch input
        if (datapath_style==SSA)
        {
            fprintf(_fp,"%s%d.out = %s%s_%d.din;\n",expr_block_instance_prefix,expr_inst_id,
                                                capture_block_prefix,
                                                vi->name,latch_id);
            // connect pipe block to delay_expr input
            fprintf(_fp,"delay_expr_%d.m1 = %s%d.zero;\n",expr_inst_id,ring_block_prefix,block_id);
            // connect delay_expr output to capture block
            fprintf(_fp,"delay_expr_%d.p1 = %s%s_%d.go;\n",expr_inst_id,capture_block_prefix,
                                                vi->name,latch_id);
        }
        else 
        {
            fprintf(_fp,"%s%d.out = %s%s_%d.din[%d][0..%d];\n",expr_block_instance_prefix,expr_inst_id,
                                                capture_block_prefix,
                                                vi->name,latch_id,
                                                vi->iwrite, (vi->width)-1);
            // connect pipe block to delay_expr input
            fprintf(_fp,"delay_expr_%d.m1 = %s%d.zero;\n",expr_inst_id,ring_block_prefix,block_id);
            // connect delay_expr output to capture block
            fprintf(_fp,"delay_expr_%d.p1 = %s%s_%d.go[%d];\n",expr_inst_id,capture_block_prefix,
                                                vi->name,latch_id,
                                                vi->iwrite);
            vi->iwrite++;
        }
        

        break;

    case ACT_CHP_SEND:
        chan = c->u.comm.chan;
        e = c->u.comm.e;
        get_true_name (chan_name, chan, _p->CurScope());
        // chan_name = chan->rootVx(p->CurScope())->getName();
        fprintf(_fp,"\n// Pipe block for action: ");
        chp_print(_fp,c);
        fprintf(_fp,"\n");
        fprintf(_fp,"elem_c_paa_send %s%d;\n",ring_block_prefix,block_id);
        if (e) {
            it = _p->CurScope()->Lookup(chan);
            bw = TypeFactory::bitWidth(it);
            fprintf(_fp,"connect_outchan_to_ctrl<%d> %s%d;\n",bw, conn_block_prefix,block_id);
                fprintf(_fp,"%s%d.ch = %s;\n",conn_block_prefix,block_id,chan_name);

            fprintf(_fp,"\n// Data for action: ");
            chp_print(_fp,c);
            fprintf(_fp,"\n");
            // if (e->type == E_VAR) { // pure variable send
            if (false) { // pure variable send
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
            }
            else { // function of variable(s) send
                expr_inst_id = _generate_expr_block(e,bw);
                // connect output of math block to channel data
                // fprintf (_fp,"\n");
                // fprintf(_fp,"%s%d.out = %s.d;\n",expr_block_instance_prefix,expr_inst_id,chan_name);
                fprintf(_fp,"connect_exprblk_dout<%d> %s%d(%s%d.out,%s%d.e);\n",bw,expr_block_output_prefix, expr_inst_id,
                                                        expr_block_instance_prefix,expr_inst_id,
                                                        conn_block_prefix,block_id);

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
        fprintf(_fp,"\n// Pipe block for action: ");
        chp_print(_fp,c);
        fprintf(_fp,"\n");
        fprintf(_fp,"elem_c_ppa %s%d;\n",ring_block_prefix,block_id);
        it = _p->CurScope()->Lookup(chan);
        bw = TypeFactory::bitWidth(it);
        fprintf(_fp,"connect_inchan_to_ctrl<%d> %s%d;\n",bw, conn_block_prefix,block_id);
        fprintf(_fp,"%s%d.ctrl = %s%d.zero;\n",conn_block_prefix,block_id,ring_block_prefix,block_id);
        fprintf(_fp,"%s%d.ch = %s;\n",conn_block_prefix,block_id,chan_name);
        
        if (var) {
            fprintf(_fp,"\n// Data for action: ");
            chp_print(_fp,c);
            fprintf(_fp,"\n");
            char tname[1024];
            get_true_name(tname, var, _p->CurScope());
            b = hash_lookup(var_infos, tname);
            Assert (b, "No var info?");
            vi = (var_info *)b->v;
            Assert ((c->space), "No latch info? (_generate_pipe_element)");
            if (init_latch == -1)
            {
                latch_id = _generate_single_latch(vi, (latch_info_t *)(c->space));
            }
            else 
            {
                latch_id = init_latch;
                Assert (datapath_style==NON_SSA, "Hmm");
            }
            if (datapath_style==SSA)
            {
                fprintf(_fp, "%s%s_%d.go = %s%d.data;\n",capture_block_prefix,
                                vi->name,latch_id,ring_block_prefix,block_id);
                fprintf(_fp, "%s%s_%d.din = %s.d;\n",capture_block_prefix,
                                                vi->name,latch_id,chan_name);
            }
            else 
            {
                fprintf(_fp, "%s%s_%d.go[%d] = %s%d.data;\n",capture_block_prefix,
                                vi->name,latch_id,vi->iwrite,
                                ring_block_prefix,block_id);
                fprintf(_fp, "%s%s_%d.din[%d][0..%d] = %s.d;\n",capture_block_prefix,
                                                vi->name,latch_id,
                                                vi->iwrite,(vi->width)-1,
                                                chan_name);
                vi->iwrite++;
            }
        }
        else { // dataless action
            fprintf(_fp,"%s%d.data.r = %s%d.data.a;\n",ring_block_prefix,block_id,ring_block_prefix,block_id);
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

#if 0
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
    char tname[1024];

    block_id = _gen_block_id();
    Assert (var_init, "no variable (_generate_pipe_element_custom)");

    switch(type)
    {
    case ACT_CHP_SEND:
        snprintf(chan_name, 1024, "%s%d",init_cond_chan_prefix,bd_chan_id);
        fprintf(_fp,"\n// Pipe block for init cond. send.");
        fprintf(_fp,"\n");
        fprintf(_fp,"elem_c_paa_brs_send %s%d;\n",ring_block_prefix,block_id);
        bw = width;
        fprintf(_fp,"connect_outchan_to_ctrl<%d> %s%d;\n",bw, conn_block_prefix,block_id);
        fprintf(_fp,"%s%d.ch = %s;\n",conn_block_prefix,block_id,chan_name);
        fprintf(_fp,"%s%d.ctrl = %s%d.zero;\n",conn_block_prefix,block_id,ring_block_prefix,block_id);
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
#endif

/*
    Similar to the previous, but used to pulse latch for
     implementing loop-carried dependencies from 
    one iteration of the ring to the next. 
*/
int RingForge::_generate_pipe_element_lcd(int type, ActId *var)
{
    int block_id;
    int first_latch_id, last_latch_id;
    hash_bucket_t *b;
    var_info *vi;
    char tname[1024];

    block_id = _gen_block_id();
    Assert (var, "no variable (_generate_pipe_element_lcd");

    switch(type)
    {
    case ACT_CHP_ASSIGN:
        fprintf(_fp,"\n// Pipe block for lcd. transmission.");
        fprintf(_fp,"\n");
        fprintf(_fp,"elem_c_paa %s%d;\n",ring_block_prefix,block_id);
        char tname[1024];
        get_true_name(tname, var, _p->CurScope());
        b = hash_lookup(var_infos, tname);
        Assert (b, "variable not found");
        vi = (var_info *)b->v;
        first_latch_id = 0;
        last_latch_id = vi->latest_for_read;
        // connect last latch output to first latch input
        fprintf(_fp,"%s%s_%d.dout = %s%s_%d.din;\n",capture_block_prefix,vi->name,last_latch_id,
                                            capture_block_prefix,
                                            vi->name,first_latch_id);
        // connect pipe block action port to latch go port
        fprintf(_fp,"%s%d.zero = %s%s_%d.go;\n",ring_block_prefix,block_id,capture_block_prefix,
                                            vi->name,first_latch_id);
        break;

    default:
        fatal_error("Shouldn't be here... (generate_pipe_element_lcd)");
        break;
    }
    return block_id;
}

int RingForge::_generate_pipe_element_lcd(int type, const char *tname)
{
    int block_id;
    int first_latch_id, last_latch_id;
    hash_bucket_t *b;
    var_info *vi;

    block_id = _gen_block_id();

    switch(type)
    {
    case ACT_CHP_ASSIGN:
        fprintf(_fp,"\n// Pipe block for lcd. transmission.");
        fprintf(_fp,"\n");
        fprintf(_fp,"elem_c_paa %s%d;\n",ring_block_prefix,block_id);
        b = hash_lookup(var_infos, tname);
        Assert (b, "variable not found");
        vi = (var_info *)b->v;
        first_latch_id = 0;
        last_latch_id = vi->latest_for_read;
        // connect last latch output to first latch input
        fprintf(_fp,"%s%s_%d.dout = %s%s_%d.din;\n",capture_block_prefix,vi->name,last_latch_id,
                                            capture_block_prefix,
                                            vi->name,first_latch_id);
        // connect pipe block action port to latch go port
        fprintf(_fp,"%s%d.zero = %s%s_%d.go;\n",ring_block_prefix,block_id,capture_block_prefix,
                                            vi->name,first_latch_id);
        break;

    default:
        fatal_error("Shouldn't be here... (generate_pipe_element_lcd)");
        break;
    }
    return block_id;
}

int RingForge::_generate_pause_element()
{   
    fprintf(_fp,"\n// Pause element to suspend execution of ring\n");
    int id = _gen_block_id();
    fprintf(_fp,"elem_c_pause %s%d;\n",ring_block_prefix,id);
    return id;
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
    fprintf(_fp,"itb_wrapper<%d,%d,%d,%d> itb_w_%d(%s%d,%s%d);\n",int(std::ceil(capture_delay*delay_multiplier)),int(std::ceil(pulse_width*delay_multiplier)),
                            width,value, id, init_cond_chan_prefix,chan_id_out,init_cond_chan_prefix,chan_id_in);
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
    Generate a non-deterministic selection split block for branches
*/
int RingForge::_generate_nds_split(int n)
{
    int block_id = _gen_block_id();
    fprintf(_fp,"nds_split<%d> %s%d;\n", n, ring_block_prefix, block_id);
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
    ExternalExprOpt *eeo = new ExternalExprOpt("abc", ((bundled==1)?bd:qdi), false, _exprfile, 
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
    config_set_int("expropt.abc_use_constraints", 1);
    config_set_int("expropt.vectorize_all_ports", 1);

    // output bitwidth and block id for name
    int xid = _gen_expr_block_id();
    // int out_expr_width = get_expr_width (e, p);
    int out_expr_width = out_bw;
    int delay_line_n;

    fprintf(_fp, "// output bitwidth: %d bits\n",out_expr_width);
    fprintf(stdout, "\n\n");
    // fprintf(stdout, "\n%d\n", e->type);
    // print_uexpr (stdout, e);
    
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
        Assert (ebi->getDelay().exists(), "Delay not extracted by abc!");
        double typ_delay_ps = (ebi->getDelay().typ_val)*1e12;
        // double typ_delay_ps = (ebi->getDelay().typ_val);
        if (typ_delay_ps <= 0) { warning("non-positive delay from abc: %dps", typ_delay_ps); }

        delay_line_n = int( (typ_delay_ps/(2*invx1_delay_ps)) + 1 ); 
        if (delay_line_n <= 0) { delay_line_n = 1; }

        fprintf(_fp, "\n// typical delay: %gps\n",typ_delay_ps);
    }

    _instantiate_expr_block (xid, all_leaves);

    fprintf(_fp,"delay_line_chan<%d> delay_expr_%d;\n",int(std::ceil(delay_line_n*delay_multiplier)),xid);
    // fprintf (stdout, "\n// bye from expropt\n");

    eeo->~ExternalExprOpt();
    ebi->~ExprBlockInfo();
    eeo = NULL;
    ebi = NULL;

    // free all temporary data structures 
    ihash_free (_inexprmap);
    _inexprmap = NULL;
    ihash_free (_inwidthmap);
    _inwidthmap = NULL;
    list_free (all_leaves);

    // force write output file
    fflush(_fp);
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
    ExternalExprOpt *eeo = new ExternalExprOpt("abc", ((bundled==1)?bd:qdi), false, _exprfile, 
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
    config_set_int("expropt.abc_use_constraints", 1);
    config_set_int("expropt.vectorize_all_ports", 1);

    int out_expr_width = 1;

    fprintf(stdout, "\n");

    // run abc, then v2act to create the combinational-logic-for-math process
    ExprBlockInfo *ebi = eeo->run_external_opt(xid, out_expr_width, e, all_leaves, _inexprmap, _inwidthmap);
    
    Assert (ebi->getDelay().exists(), "Delay not extracted by abc!");
    double typ_delay_ps = (ebi->getDelay().typ_val)*1e12;
    // double typ_delay_ps = (ebi->getDelay().typ_val);
    // if (typ_delay_ps <= 0) { warning("non-positive delay from abc: %dps", typ_delay_ps); }

    int delay_line_n = int( (typ_delay_ps/(2*invx1_delay_ps)) + 1 ); 
    if (delay_line_n <= 0) { delay_line_n = 1; }

    fprintf(_fp, "\n// typical delay: %gps\n",typ_delay_ps);
    _instantiate_expr_block (xid, all_leaves);

    eeo->~ExternalExprOpt();
    ebi->~ExprBlockInfo();
    eeo = NULL;
    ebi = NULL;

    // free all temporary data structures 
    ihash_free (_inexprmap);
    _inexprmap = NULL;
    ihash_free (_inwidthmap);
    _inwidthmap = NULL;
    list_free (all_leaves);

    // force write output file
    fflush(_fp);
    // return int(std::ceil(delay_line_n*delay_multiplier));
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
        if ( e_var->type == E_VAR || e_var->type == E_BITFIELD )
        {
            ActId *var = (ActId *)e_var->u.e.l;
            char tname[1024];
            get_true_name(tname, var, _p->CurScope());

            // Should probably have a more disciplined way to do this
            if (!strncmp(tname, "probe__of__",11))
            {
                Assert (false, "Probe synthesis failure");
                // printf ("\n found probe to connect\n");
                // char *tname1 = &tname[11];
                // connect channel request to expr block
                // fprintf(_fp,"%s%d.%s%d[0] = %s.r;\n",expr_block_instance_prefix,block_id,
                //                                         expr_block_input_prefix, ib->i, 
                //                                         tname1);
            }
            else 
            {
                hash_bucket_t *b = hash_lookup(var_infos, tname);
                Assert (b, "var not found?");
                // hash_bucket_t *b = hash_lookup(var_infos, var->rootVx(p->CurScope())->getName());
                var_info *vi = (var_info *)b->v;

                // TEST ----
                int latch_id = vi->latest_for_read;
                // TEST ----

                Assert ((latch_id>=0),"var. read before being written?");

                int va_id = _gen_var_access_id();
                fprintf(_fp,"var_access<%d> %s%d(%s%s_%d.dout,%s%d.%s%d);\n",vi->width,var_access_prefix,va_id,
                                                        capture_block_prefix,vi->name,latch_id,
                                                        expr_block_instance_prefix,block_id,
                                                        expr_block_input_prefix, ib->i);
                // connect variable-latch output to expr block input
                // fprintf(_fp,"%s%d.%s%d = %s%s_%d.dout;\n",expr_block_instance_prefix,block_id,
                //                                         expr_block_input_prefix, ib->i, 
                //                                         capture_block_prefix,
                //                                         vi->name,latch_id);
            }
        }
        // connect constants to math block inputs
        else if ( e_var->type == E_INT )
        {
            fatal_error ("This shouldn't have been used (constant as input to expr block)");
        }
        else { 
            fprintf (stdout, "e_var type: %d \n", e_var->type);
            fatal_error ("leaf (primary input) is neither variable nor constant int?? (instantiate_expr_block)"); }
    }
}

int RingForge::_generate_probe_circuit (Expr *g, int xid)
{
    list_t *data_gl = list_new ();
    list_t *m, *p;
    Expr *tmp, *egc;
    /*
        (#A & ... ) | (#B & ... ) | ... 
        m = list of guard expressions (non-probed pieces)
    */
    m = list_new ();
    egc = g;
    while (egc) {
        if (egc->type == E_OR) {
            tmp = egc->u.e.l;
        }
        else {
            tmp = egc;
        }
        while (tmp && tmp->type == E_AND && tmp->u.e.l->type == E_PROBE) {
            tmp = tmp->u.e.r;
        }
        if (tmp->type == E_PROBE) {
            int eid = _gen_expr_block_id ();
            Expr *e1 = new Expr;
            e1->type = E_INT;
            e1->u.ival.v = 1;
            // _emit_expr_const (eid, 1, 1, true);
            _generate_expr_block_for_sel (e1,eid);
            list_iappend (m, eid);
            list_iappend (data_gl, eid);
        }
        else {
            // _emit_one_guard_expr (tmp, m);
            int eid = _gen_expr_block_id ();
            _generate_expr_block_for_sel (tmp,eid);
            // list_iappend (data_gl,
            //             list_ivalue (list_tail (m)));
            list_iappend (m, eid);
            list_iappend (data_gl,eid);
        }
        if (egc->type == E_OR) {
            egc = egc->u.e.r;
        }
        else {
            break;
        }
    }
    /* 
        p = list of list of probes
    */
    p = list_new ();
    egc = g;
    while (egc) {
        list_t *pl = list_new ();
        if (egc->type == E_OR) {
            tmp = egc->u.e.l;
        }
        else {
            tmp = egc;
        }
        while (tmp && tmp->type == E_AND && tmp->u.e.l->type == E_PROBE) {
            int pid = _generate_probe_access ((ActId *)tmp->u.e.l->u.e.l);
            list_iappend (pl, pid);
            tmp = tmp->u.e.r;
        }
        if (tmp->type == E_PROBE) {
            int pid = _generate_probe_access ((ActId *)tmp->u.e.l);
            list_iappend (pl, pid);
        }

        list_append (p, pl);
        if (egc->type == E_OR) {
            egc = egc->u.e.r;
        }
        else {
            break;
        }
    }

    /* now emit probed clause */
    int pc = _generate_probe_clause (m, p);
    list_free (m);
    for (listitem_t *li = list_first (p); li; li = list_next (li)) {
        list_free ((list_t *) list_value (li));
    }
    list_free (p);
    return pc;
}

int RingForge::_generate_probe_clause (list_t *guards, list_t *probe_list)
{
  int count = 0;
  listitem_t *pi;
  int idx;

  idx = _gen_expr_block_id ();
  for (pi = list_first (probe_list); pi; pi = list_next (pi)) {
    count += list_length ((list_t *)list_value (pi));
  }
  if (count == 0) {
    Assert (list_length (guards) == 1, "What?");
    fprintf (_fp, "dummy_probe_clause pc_%d(%s%d.out);\n",
	     idx, expr_block_instance_prefix, list_ivalue (list_first (guards)));
  }
  else {
    fprintf (_fp, "probe_clause<%d,{", list_length (guards));
    for (pi = list_first (probe_list); pi; pi = list_next (pi)) {
      if (pi != list_first (probe_list)) {
	fprintf (_fp, ",");
      }
      fprintf (_fp, "%d", list_length ((list_t *)list_value (pi)));
    }
    fprintf (_fp, "},%d> %s%d({", count, expr_block_instance_prefix, idx);
    
    for (listitem_t *li = list_first (guards); li; li = list_next (li)) {
      if (li != list_first (guards)) {
	fprintf (_fp, ",");
    }
      fprintf (_fp, "%s%d.out[0]", expr_block_instance_prefix, list_ivalue (li));
    }
    fprintf (_fp, "},{");

    int emit_comma = 0;
    for (pi = list_first (probe_list); pi; pi = list_next (pi)) {
      for (listitem_t *qi = list_first ((list_t *)list_value (pi));
	   qi; qi = list_next (qi)) {
	if (emit_comma) {
	  fprintf (_fp, ",");
	}
	fprintf (_fp, "probe_%d.dout[0]", list_ivalue (qi));
	emit_comma = 1;
      }
    }
    fprintf (_fp, "});\n");
  }
  return idx;
}

int RingForge::_generate_probe_access (ActId *chan)
{
    char tname[1024];
    get_true_name(tname, chan, _p->CurScope());
    hash_bucket_t *b = hash_lookup(var_infos, tname);
    Assert (b, "var not found?");
    var_info *vi = (var_info *)b->v;
    int w = vi->width;
    int pid = _gen_var_access_id();
    fprintf (_fp, "probe_access<%d> probe_%d(",w,pid);
    chan->Print(_fp);
    fprintf (_fp, ");\n");
    return pid;
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
        ib->i = _gen_expr_id ();
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
        ihash_bucket_t *ib;
        ihash_bucket_t *b_width;
        if (!ihash_lookup (_inexprmap, (long)e)) 
        {
            ib = ihash_add (_inexprmap, (long)e);
            ib->i = _gen_expr_id();
            b_width = ihash_add (_inwidthmap, (long) e);
            b_width->i = vi->width;
        }
    }
    else {
    //   ihash_bucket_t *b;
    //   b = ihash_lookup (_inexprmap, (long)e);
    //   _emit_var_read (b->i, (ActId *)e->u.e.l);
    }
    break;

  case E_PROBE:
    // fatal_error ("fix probes please");
    if (collect_phase) {
        // make dummy variable to stand in for probe
        InstType *it = TypeFactory::Factory()->NewInt (_p->CurScope(), Type::NONE, 0, const_expr(1));
        static char buf[1024];
        it = it->Expand(NULL, _p->CurScope());

        ActId *chan = (ActId *)e->u.e.l;
        var_info *vi;
        hash_bucket_t *b;
        char tname[1024];
        get_true_name(tname, chan, _p->CurScope());
        snprintf(buf, 1024, "probe_of_%s", tname);
        _p->CurScope()->Add (buf, it);

        // Expr *ee = new Expr;
        // Replace the probe in the original expression with dummy var
        e->type = E_VAR;
        e->u.e.l = (Expr *)(new ActId (buf));

        auto tst = _p->CurScope()->Lookup((ActId *)e->u.e.l);
        Assert (tst, "hmm new id");

        // auto b = hash_add(var_infos, buf);
        // b = hash_lookup(var_infos, tname);
        // vi = (var_info *)b->v;
        ihash_bucket_t *ib;
        ihash_bucket_t *b_width;
        if (!ihash_lookup (_inexprmap, (long)e)) 
        {
            ib = ihash_add (_inexprmap, (long)e);
            ib->i = _gen_expr_id();
            b_width = ihash_add (_inwidthmap, (long)e);
            b_width->i = 1;
        }
    }

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

list_t *RingForge::_create_channel_accesses(list_t *ics)
{
    list_t *ret = list_new();
    for (listitem_t *li = list_first(ics); li; li = list_next (li))
    {
        hash_bucket_t *b = hash_lookup(var_infos, (const char *)list_value(li));
        var_info *vi = (var_info *)b->v;
        if (!(vi->fischan)) {
            list_append(ret, (const char *)list_value(li));
        }
        else {
            fprintf(_fp, "chan_access<%d> %s%s_%d;\n", vi->width, capture_block_prefix, vi->name,0);
        }
    }
    return ret;
}

/*
    General synthesis for branched programs.
    Non-SSA style Datapath. 
*/
int RingForge::generate_branched_ring_non_ssa(act_chp_lang_t *c, int root, int prev_block_id, int connect_prev)
{
    int block_id, expr_block_id, first_block_id;
    int pll_split_block_id, pll_merge_block_id;
    int sel_split_block_id, sel_merge_block_id;
    int comma_len, gc_len;
    int pll_port, gp_con_id;
    int delay_n_sel, max_delay_n_sel;
    list_t *gp_connect_ids;
    listitem_t *li, *lj;
    act_chp_gc_t *gc;
    act_chp_lang_t *stmt, *main_loop;
    var_info *vi;
    ActId *id;
    bool have_probes = false;

    if (!c) { return prev_block_id; }

    switch(c->type)
    {
    case ACT_CHP_COMMALOOP:
    case ACT_CHP_SEMILOOP:
        fatal_error ("Replication loops should've been removed.. (generate_branched_ring_non_ssa)");
        break;
    case ACT_CHP_COMMA:
        if (root == 1)
        { 
            fatal_error ("Only semi-colon list of initializations... (generate_branched_ring_non_ssa)"); 
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
                gp_con_id = _generate_gp_connect ();
                _connect_pll_split_outputs_to_pipe (pll_split_block_id, gp_con_id, pll_port);
                
                block_id = generate_branched_ring_non_ssa ((act_chp_lang_t *)list_value(li), 0, gp_con_id, 1);
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
                if (stmt->type == ACT_CHP_LOOP || stmt->type == ACT_CHP_DOLOOP)
                    main_loop = (act_chp_lang_t *)list_value(li);
            }

            first_block_id = _generate_itb();
            prev_block_id = first_block_id;

            // new I.C. handling method -----
#if 1
            // loop through initial condition assignments to create latches with correct initial values
            list_t *ic_list  = list_dup((list_t *)(((latch_info_t *)(main_loop->space))->live_vars));
            for (lj = list_first (c->u.semi_comma.cmd); lj; lj = list_next (lj)) 
            {   
                list_t *tmp = list_new();
                act_chp_lang_t *stmt1 = (act_chp_lang_t *)list_value(lj);
                if (stmt1->type != ACT_CHP_LOOP && stmt1->type != ACT_CHP_DOLOOP)
                {
                    Assert (stmt1->type == ACT_CHP_ASSIGN, "Only assignments in initial conditions");
                    id = stmt1->u.assign.id;
                    Expr *e = stmt1->u.assign.e;
                    // Assert (e->type == E_INT, "Constants only in initial conditions");
                    // long long ival = e->u.ival.v;
                    unsigned long ival = eval_ic(e);
                    char tname[1024];
                    get_true_name(tname, id, _p->CurScope());
                    hash_bucket_t *b = hash_lookup(var_infos, tname);
                    for (listitem_t *lk = list_first(ic_list); lk; lk = list_next (lk))
                    {
                        // copy over all vars except the one being initialized
                        if (strcmp(tname, (const char *)list_value(lk))) { 
                            list_append (tmp, list_value(lk));
                        }
                    }
                    vi = (var_info *)b->v;
                    // fprintf (_fp, "\ngot here: %s\n", tname);
                    Assert ((stmt1->space), "No latch info? (_generate_branched_ring_non_ssa, initial condition handling)");
                    
                    // initial condition is not a real assignment for this datapath style
                    vi->nwrite--;

                    _generate_single_latch_non_ssa (vi, ival);

                    ic_list = list_new();
                    ic_list = list_dup(tmp);
                    list_free(tmp);
                }
            }
            if (!list_isempty(ic_list)) {
                _print_list_of_vars (stderr, ic_list);
                fatal_error ("The above variables were uninitialized in the program. Initialize them please.");
            }
#endif

            //  latches for remaining variables : init. cond. = 0
            hash_iter_t it;
            hash_bucket_t *b;
            hash_iter_init (var_infos, &it);
            while ((b = hash_iter_next (var_infos, &it))) 
            {
                if (((var_info *)b->v)->iread==0)
                    _generate_single_latch_non_ssa ((var_info *)b->v, 0);
            }	 

            // new --------------------------

            // main program synthesis
            gc = main_loop->u.gc;
            block_id = generate_branched_ring_non_ssa(gc->s, 0, prev_block_id, 1);
            prev_block_id = block_id;

            _connect_pipe_elements(block_id, first_block_id);
            break;
        }
        // regular synthesis
        else {
            for (li = list_first (c->u.semi_comma.cmd); li; li = list_next (li)) 
            {
                block_id = generate_branched_ring_non_ssa ((act_chp_lang_t *)list_value(li), 0, prev_block_id, 0);
                if (is_elementary_action((act_chp_lang_t *)list_value(li)))
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
            list_t *iclist  = list_dup((list_t *)(((latch_info_t *)(c->space))->live_vars));
            if (!list_isempty(iclist)) {                
                _print_list_of_vars (stderr, iclist);
                fatal_error ("The above variables were uninitialized in the program. Initialize them please. (Should only be here for non-LCD programs)");
            }

        //  latches for all variables : init. cond. = 0
            hash_iter_t it;
            hash_bucket_t *b;
            hash_iter_init (var_infos, &it);
            while ((b = hash_iter_next (var_infos, &it))) 
            {
                _generate_single_latch_non_ssa ((var_info *)b->v, 0);
            }	 

            first_block_id = _generate_itb();
            gc = c->u.gc;
            block_id = generate_branched_ring_non_ssa(gc->s, 0, first_block_id, 1);
            prev_block_id = block_id;

            _connect_pipe_elements(block_id, first_block_id);
            break;
        }
        else { fatal_error ("bleh"); }
        break; 
        
    case ACT_CHP_SELECT:
    case ACT_CHP_SELECT_NONDET:
        gc = c->u.gc;
        gc_len = length_of_guard_set (c);
        max_delay_n_sel = 0;

        fprintf (_fp, "\n// %d-way selection split for : ", gc_len);
        chp_print(_fp, c);
        fprintf (_fp, "\n");
        fprintf (_fp, "// %d-way selection merge \n", gc_len);
        if (_guards_have_probes(gc)) {
            have_probes = true;
            sel_split_block_id = _generate_nds_split(gc_len);
        }
        else {
            sel_split_block_id = _generate_selection_split(gc_len);
        }
        sel_merge_block_id = _generate_selection_merge(gc_len);

        gp_connect_ids = list_new();
        for (int i = 0; gc; gc = gc->next)
        {
            // branch_id++;
            Assert ((gc->g) , "should've been fixed in else generation");
            if (have_probes)
            {   
                expr_block_id = _generate_probe_circuit (gc->g, expr_block_id);
            }
            else
            {
                expr_block_id = _gen_expr_block_id();
                delay_n_sel = _generate_expr_block_for_sel (gc->g, expr_block_id);
                if (max_delay_n_sel < delay_n_sel) max_delay_n_sel = delay_n_sel;
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
            block_id = generate_branched_ring_non_ssa (gc->s, 0, list_ivalue(lj), 1);
            _connect_pipe_to_sel_merge_inputs (sel_merge_block_id, block_id, i);
            i++; lj = list_next(lj);
        }

        // generate delay line for max guard evaluator delay (split)
        Assert (max_delay_n_sel>=0, "negative delay?");
        
        if (!have_probes) {
            fprintf(_fp,"\n// Delaying pre-split-block sync. by max. delay of all guard evaluators\n");
            fprintf(_fp,"delay_line_chan<%d> delay_select_%d;\n",int(std::ceil(max_delay_n_sel*delay_multiplier)),sel_split_block_id);
            // connect prev. block p1 to delay_line then connect to select block from the output
            fprintf(_fp,"delay_select_%d.m1 = %s%d.p1;\n",sel_split_block_id,ring_block_prefix,prev_block_id);
            fprintf(_fp,"delay_select_%d.p1 = %s%d.m1;\n",sel_split_block_id,ring_block_prefix,sel_split_block_id);
        }
        else {
            fprintf(_fp,"\n// Probed selection - no need to insert guard evaluator delay \n");
            fprintf(_fp,"%s%d.p1 = %s%d.m1;\n",ring_block_prefix,prev_block_id,ring_block_prefix,sel_split_block_id);
        }
        block_id = sel_merge_block_id;

        break;
        
    case ACT_CHP_SKIP:
    case ACT_CHP_ASSIGN:
    case ACT_CHP_ASSIGNSELF:
    case ACT_CHP_SEND:
    case ACT_CHP_RECV:
        // do stuff
        if (c->label && !strcmp(c->label,"pause"))
        {
            block_id = _generate_pause_element();
            fprintf (stdout, "\n a1of1 pause port placed here : %s%d.pause \n",ring_block_prefix,block_id);
            fprintf (stdout, "\n pause.r must be grounded for ring execution \n");
            if (connect_prev == 1)
            {
                _connect_pipe_elements(prev_block_id, block_id);
            }
            prev_block_id = block_id;
            block_id = _generate_pipe_element(c, 0);
            _connect_pipe_elements(prev_block_id, block_id);
        }
        else {
            block_id = _generate_pipe_element(c, 0);
            if (connect_prev == 1)
            {
                _connect_pipe_elements(prev_block_id, block_id);
            }
        }
        break;

    case ACT_CHP_FUNC:
    case ACT_CHP_HOLE: /* to support verification */
    case ACT_CHP_MACRO:
    case ACT_HSE_FRAGMENTS:
        block_id = prev_block_id;
        break;

    default:
        fatal_error ("Unknown type in generate_branched_ring");
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
    int pll_port, gp_con_id;
    int delay_n_sel, max_delay_n_sel, delay_n_merge;
    list_t *gp_connect_ids;
    listitem_t *li, *lj;
    act_chp_gc_t *gc;
    act_chp_lang_t *stmt, *main_loop;
    bool have_probes = false;
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
                gp_con_id = _generate_gp_connect ();
                _connect_pll_split_outputs_to_pipe (pll_split_block_id, gp_con_id, pll_port);
                
                block_id = generate_branched_ring ((act_chp_lang_t *)list_value(li), 0, gp_con_id, 1);
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
                if (stmt->type == ACT_CHP_LOOP || stmt->type == ACT_CHP_DOLOOP)
                    main_loop = (act_chp_lang_t *)list_value(li);
            }

            first_block_id = _generate_itb();
            prev_block_id = first_block_id;

            // new I.C. handling method -----
#if 1
            // loop through initial condition assignments to create latches with correct initial values
            list_t *ic_list  = list_dup((list_t *)(((latch_info_t *)(main_loop->space))->live_vars));

            // handle channel variables - i.e. value probes
            ic_list = _create_channel_accesses(ic_list);

            for (lj = list_first (c->u.semi_comma.cmd); lj; lj = list_next (lj)) 
            {   
                list_t *tmp = list_new();
                act_chp_lang_t *stmt1 = (act_chp_lang_t *)list_value(lj);
                if (stmt1->type != ACT_CHP_LOOP && stmt1->type != ACT_CHP_DOLOOP)
                {
                    Assert (stmt1->type == ACT_CHP_ASSIGN, "Only assignments in initial conditions");
                    id = stmt1->u.assign.id;
                    Expr *e = stmt1->u.assign.e;
                    // Assert (e->type == E_INT, "Constants only in initial conditions");
                    // long long ival = e->u.ival.v;
                    unsigned long ival = eval_ic(e);
                    char tname[1024];
                    get_true_name(tname, id, _p->CurScope());
                    hash_bucket_t *b = hash_lookup(var_infos, tname);
                    for (listitem_t *lk = list_first(ic_list); lk; lk = list_next (lk))
                    {
                        // copy over all vars except the one being initialized
                        if (strcmp(tname, (const char *)list_value(lk))) { 
                            list_append (tmp, list_value(lk));
                        }
                    }
                    vi = (var_info *)b->v;
                    Assert ((stmt1->space), "No latch info? (_generate_branched_ring, initial condition handling)");
                    int latch_id = _generate_single_latch (vi, (latch_info_t *)(stmt1->space), ival);
                    Assert (latch_id == 0, "Same variable has more than one initial condition?");

                    ic_list = list_new();
                    ic_list = list_dup(tmp);
                    list_free(tmp);
                }
            }
            if (!list_isempty(ic_list)) {
                _print_list_of_vars (stderr, ic_list);
                fatal_error ("The above variables were uninitialized in the program. Initialize them please.");
            }
#endif
            // new --------------------------

            // main program synthesis
            gc = main_loop->u.gc;
            block_id = generate_branched_ring(gc->s, 0, prev_block_id, 1);
            prev_block_id = block_id;

#if 1
            // new I.C. handling method -----
            for (lj = list_first (c->u.semi_comma.cmd); lj; lj = list_next (lj)) 
            {
                act_chp_lang_t *stmt1 = (act_chp_lang_t *)list_value(lj);
                if (stmt1->type != ACT_CHP_LOOP && stmt1->type != ACT_CHP_DOLOOP)
                {
                    Assert (stmt1->type == ACT_CHP_ASSIGN, "Only assignments in initial conditions");
                    id = stmt1->u.assign.id;
                    char tname[1024];
                    get_true_name(tname, id, _p->CurScope());
                    block_id = _generate_pipe_element_lcd (ACT_CHP_ASSIGN, id);
                    _connect_pipe_elements (prev_block_id, block_id);
                    prev_block_id = block_id;
                }
            }
            if (!list_isempty(ic_list))
            {
                _print_list_of_vars (stderr, ic_list);
                fatal_error ("The above variables were uninitialized in the program. Initialize them please.");
            }
#endif

            _connect_pipe_elements(block_id, first_block_id);
            break;
        }
        // regular synthesis
        else {
            for (li = list_first (c->u.semi_comma.cmd); li; li = list_next (li)) 
            {
                block_id = generate_branched_ring ((act_chp_lang_t *)list_value(li), 0, prev_block_id, 0);
                if (is_elementary_action((act_chp_lang_t *)list_value(li)))
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
            list_t *iclist  = list_dup((list_t *)(((latch_info_t *)(c->space))->live_vars));
            if (!list_isempty(iclist)) {                
                _print_list_of_vars (stderr, iclist);
                fatal_error ("The above variables were uninitialized in the program. Initialize them please. (Should only be here for non-LCD programs)");
            }

            first_block_id = _generate_itb();
            gc = c->u.gc;
            block_id = generate_branched_ring(gc->s, 0, first_block_id, 1);
            prev_block_id = block_id;

            _connect_pipe_elements(block_id, first_block_id);
            break;
        }
        else 
        { 
            fatal_error ("Should've run loop excision first"); 
        }
        break;
        
    case ACT_CHP_SELECT:
    case ACT_CHP_SELECT_NONDET:
        gc = c->u.gc;
        gc_len = length_of_guard_set (c);
        max_delay_n_sel = 0;

        fprintf (_fp, "\n// %d-way selection split for : ", gc_len);
        chp_print(_fp, c);
        fprintf (_fp, "\n");
        fprintf (_fp, "// %d-way selection merge \n", gc_len);
        if (_guards_have_probes(gc)) {
            have_probes = true;
            sel_split_block_id = _generate_nds_split(gc_len);
        }
        else {
            sel_split_block_id = _generate_selection_split(gc_len);
        }

        sel_merge_block_id = _generate_selection_merge(gc_len);
        save_var_infos();

        gp_connect_ids = list_new();
        for (int i = 0; gc; gc = gc->next)
        {
            // branch_id++;
            Assert ((gc->g) , "should've been fixed in else generation");
            if (have_probes)
            {   
                expr_block_id = _generate_probe_circuit (gc->g, expr_block_id);
            }
            else
            {   
                expr_block_id = _gen_expr_block_id();
                delay_n_sel = _generate_expr_block_for_sel (gc->g, expr_block_id);
                if (max_delay_n_sel < delay_n_sel) max_delay_n_sel = delay_n_sel;
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
            // _branch_id++;
            _save_read_ids();
            block_id = generate_branched_ring (gc->s, 0, list_ivalue(lj), 1);
            _connect_pipe_to_sel_merge_inputs (sel_merge_block_id, block_id, i);
            _restore_read_ids();
            i++; lj = list_next(lj);
        }

        // muxing variables live-out of merge so downstream can access correctly
        delay_n_merge = _compute_merge_mux_info((latch_info_t *)(c->space), sel_split_block_id);
        // delay_n_merge = 0;

        // generate delay line for max guard evaluator delay (split)
        Assert (max_delay_n_sel>=0, "negative delay?");

        if (!have_probes) {
            Assert (max_delay_n_sel>0, "non-positive delay for non-probed guard evaluators?");
            fprintf(_fp,"\n// Delaying pre-split-block sync. by max. delay of all guard evaluators\n");
            fprintf(_fp,"delay_line_chan<%d> delay_select_%d;\n",int(std::ceil(max_delay_n_sel*delay_multiplier)),sel_split_block_id);
            // connect prev. block p1 to delay_line then connect to select block from the output
            fprintf(_fp,"delay_select_%d.m1 = %s%d.p1;\n",sel_split_block_id,ring_block_prefix,prev_block_id);
            fprintf(_fp,"delay_select_%d.p1 = %s%d.m1;\n",sel_split_block_id,ring_block_prefix,sel_split_block_id);
        }
        else {
            fprintf(_fp,"\n// Probed selection - no need to insert guard evaluator delay \n");
            fprintf(_fp,"%s%d.p1 = %s%d.m1;\n",ring_block_prefix,prev_block_id,ring_block_prefix,sel_split_block_id);
        }

        if (delay_n_merge > 0)
        {
            delay_merge_block_id = _gen_block_id();
            fprintf(_fp,"\n// Delaying post-merge-block sync. by max. delay of all merge muxes\n");
            fprintf(_fp,"delay_line_chan<%d> %s%d;\n",int(std::ceil(delay_n_merge*delay_multiplier)),ring_block_prefix, delay_merge_block_id);
            fprintf(_fp,"%s%d.m1 = %s%d.p1;\n",ring_block_prefix,delay_merge_block_id,
                                                ring_block_prefix,sel_merge_block_id);
            // tail is the delay_merge block
            block_id = delay_merge_block_id;
        }
        else 
        {
            block_id = sel_merge_block_id;
        }

        _save_read_ids();
        break;
        
    case ACT_CHP_SKIP:
    case ACT_CHP_ASSIGN:
    case ACT_CHP_ASSIGNSELF:
    case ACT_CHP_SEND:
    case ACT_CHP_RECV:
        // do stuff
        if (c->label && !strcmp(c->label,"pause"))
        {
            block_id = _generate_pause_element();
            fprintf (stdout, "\n a1of1 pause port placed here : %s%d.pause \n",ring_block_prefix,block_id);
            fprintf (stdout, "\n pause.r must be grounded for ring execution \n");
            if (connect_prev == 1)
            {
                _connect_pipe_elements(prev_block_id, block_id);
            }
            prev_block_id = block_id;
            block_id = _generate_pipe_element(c, -1);
            _connect_pipe_elements(prev_block_id, block_id);
        }
        else {
            block_id = _generate_pipe_element(c, -1);
            if (connect_prev == 1)
            {
                _connect_pipe_elements(prev_block_id, block_id);
            }
        }
        break;

    case ACT_CHP_FUNC:
    case ACT_CHP_HOLE: /* to support verification */
    case ACT_CHP_MACRO:
    case ACT_HSE_FRAGMENTS:
        block_id = prev_block_id;
        break;

    default:
        fatal_error ("Unknown type in generate_branched_ring");
        break;
    }
    return block_id;
}

std::pair<int,int> RingForge::_get_pre_sel_latch_and_size (std::vector<int> in)
{
    int pre_sel_latch = -1;
    int size = 0;
    std::set<int> seen;
    seen.clear();
    //  assumption: only 1 duplicate exists
    for ( auto x : in )
    {
        if (!seen.contains(x)) {
            seen.insert(x);
            size++; // size goes up by 1 for every new element
        }
        else {
            pre_sel_latch = x; // already exists, so it's the duplicate
        }
    }
    return {pre_sel_latch, size}; 
}

/*
    Generate merging muxes to be placed when exiting selections,
    so that variables that are assigned in one/many branches inside
    a selection can be addressed correctly when exiting the selection, 
    based on which branch was taken in this iteration of the loop.
*/
int RingForge::_compute_merge_mux_info (latch_info_t *l, int split_block_id)
{
    var_info *vi, *vi_pre;
    hash_bucket_t *b, *b_pre;
    listitem_t *li, *lj;
    list_t *latch_branches;
    int iwrite, iwrite_pre;
    int latest_branch_id;
    int max_mux_size = 0;
    int max_or_size = 0;

    if ( list_isempty(l->live_vars) ) return 0;
    Assert (l->type == LatchType::Mux, "wth");

    int ctr = 0;
    for ( li = list_first(l->live_vars) ; li ; li = list_next(li) )
    {
        b = hash_lookup (var_infos, (const char *)list_value(li));
        if (!b) fatal_error ("variable not found - whatt");
        vi = (var_info *)b->v;

        fprintf (_fp, "\n// variable: %s\n", vi->name);
        if (l->merge_mux_latch_number.at(ctr) == -1) 
        {
            fprintf (_fp, "// mux not needed\n");
            ctr++;
            continue;
        }

        auto tmp = _get_pre_sel_latch_and_size (l->merge_mux_inputs.at(ctr));
        int pre_sel_latch = tmp.first;
        int mux_size = tmp.second;
        int or_size = (l->merge_mux_inputs.at(ctr).size()) - mux_size + 1;

        // see if OR-gate is needed
        if (or_size == 1)
        {
            Assert ((pre_sel_latch==-1), "check that there were no duplicates");
            fprintf(_fp, "// assigned in all branches\n"); 
        }
        else
        { 
            fprintf(_fp, "// not assigned in all branches\n"); 
        }
        // find the variable with the biggest mux+or combo (lookup TODO)
        if (max_mux_size < mux_size) max_mux_size = mux_size;
        if (max_or_size < or_size) max_or_size = or_size;

        // generate the mux (looks like latch to downstream) and connect latch outputs correctly
        int mux_id = l->merge_mux_latch_number.at(ctr);
        fprintf (_fp, "merge_mux_ohc_opt<%d,%d> %s%s_%d;\n", mux_size, vi->width, 
                                                capture_block_prefix, vi->name, mux_id);

        // increase latest_for_read for the variable so it can be connected to correctly downstream
        vi->iwrite++;
        vi->latest_for_read = mux_id;

        // generate OR-gate
        fprintf (_fp, "std::gates::ortree<%d, false> or_%s_%d;\n", or_size, vi->name, mux_id);

        // connect OR-gate inputs (split outputs)
        int ctr2 = 0; int ctr3 = 0;
        for ( auto z : l->merge_mux_inputs.at(ctr) )
        {
            if (z == pre_sel_latch) {
                fprintf (_fp, "or_%s_%d.in[%d] = %s%d.co[%d].r;\n", vi->name, mux_id, ctr2, 
                                    ring_block_prefix, split_block_id, ctr3);
                ctr2++;
            }
            ctr3++;
        }
        Assert ((ctr2 == or_size || or_size == 1), "or size mismatch");
        fprintf (_fp, "\n");
        int ctr_mux_port = 0; bool once = false;
        int ctr_sel_br = 0;
        /*
            map from selection branches to mux ports:
            start at zero (mux port) and first branch (selection) and keep incrementing
        */
        for ( auto zz : l->merge_mux_inputs.at(ctr) )
        {
            // pre-split connection - do once
            if ((zz == pre_sel_latch) && !once)
            {
                // connect pre-split data to mux last data input
                // fprintf (_fp, "%s%s_%d.din[%d][0..%d] = %s%s_%d.dout;\n", capture_block_prefix, vi->name, 
                //                                     mux_id, ctr_mux_port, (vi->width)-1,
                //                         capture_block_prefix, vi->name, pre_sel_latch);
                int va_id = _gen_var_access_id();
                fprintf(_fp,"var_access<%d> %s%d(%s%s_%d.dout,%s%s_%d.din[%d][0..%d]);\n",
                                        vi->width,var_access_prefix,va_id,
                                        capture_block_prefix,vi->name, pre_sel_latch,
                                        capture_block_prefix,vi->name, mux_id,
                                        ctr_mux_port,(vi->width)-1);


                // connect OR-gate output to mux input control
                fprintf (_fp, "or_%s_%d.out = %s%s_%d.c[%d];\n\n", vi->name, mux_id,
                                        capture_block_prefix, vi->name, mux_id, ctr_mux_port);
                once = true;
                ctr_mux_port++;
                ctr_sel_br++;
                continue;
            }
            else if (zz != pre_sel_latch) 
            {
                // connect mux input control
                fprintf (_fp, "%s%s_%d.c[%d] = %s%d.co[%d].r;\n", capture_block_prefix, vi->name, 
                                                        mux_id, ctr_mux_port, ring_block_prefix, 
                                                    split_block_id, ctr_sel_br);
                // connect mux input data
                // fprintf (_fp, "%s%s_%d.din[%d][0..%d] = %s%s_%d.dout;\n\n",capture_block_prefix, vi->name, 
                //                                         mux_id, ctr_mux_port, (vi->width)-1,
                //                                     capture_block_prefix, vi->name, zz);
                int va_id = _gen_var_access_id();
                fprintf(_fp,"var_access<%d> %s%d(%s%s_%d.dout,%s%s_%d.din[%d][0..%d]);\n",
                                        vi->width,var_access_prefix,va_id,
                                        capture_block_prefix,vi->name, zz,
                                        capture_block_prefix,vi->name, mux_id,
                                        ctr_mux_port,(vi->width)-1);

                ctr_mux_port++;
            }
            ctr_sel_br++;
        }
        Assert (ctr_mux_port == mux_size, "mux size mismatch");
        ctr++; 
    }

    if ( max_mux_size>0 ) {
        float max_delay = _lookup_mux_delays (max_mux_size, max_or_size);
        if (max_delay == -1) {
            // TODO: temporary large value for large muxes, gotta fix 
            max_delay = 2000;
        }
        return int(max_delay/(2*invx1_delay_ps)) + 1;
    }
    else {
        return 0;
    }
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

void RingForge::_print_list_of_vars (FILE *fp, list_t *vars)
{
    listitem_t *li;
    if (list_isempty(vars))
    {
        fprintf (fp, "\nempty list\n");
        return;
    }
  
    fprintf(fp, "\n-----------\n");
    for (li = list_first(vars); li; li = list_next(li)) 
    {
        fprintf(fp, "%s, ", (char *)list_value(li));
    }	     
    fprintf(fp, "\n-----------");
    fprintf(fp, "\n\n");
    return;
}

