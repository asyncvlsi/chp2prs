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
 *************************************************************************
 */

#include "ring_forge.h"
#include <cmath>

#include <chrono>
using namespace std::chrono;

#define SSA 0
#define NON_SSA 1

static std::unordered_map<act_connection *, Expr *> _invarsmap;

RingForge::RingForge ( FILE *fp, 
            int bdpath,
            int delay_margin, int dp_style,
            BD_MODE bdpath_mode, 
            const char *externopt_toolname,
            const char *circuit_library,
            const char *exprfile )
    : RingEngine ( fp, circuit_library, exprfile )
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

    struct_chan_name = config_get_string("synth.struct_chan_name");

    // Bundled datapath parameters
    // invx1_delay_ps = config_get_int("synth.ring.bundled.invx1_delay_ps");
    verbose = config_get_int("synth.ring.verbose");
    capture_delay = config_get_real("synth.ring.bundled.capture_delay");
    pulse_width = config_get_real("synth.ring.bundled.pulse_width");

    _bdpath_mode = bdpath_mode;
    // Delay line parameters check
    std::string delay_params_table = bd_mode_config_params.at(_bdpath_mode);
    std::string delay_vals_table = bd_mode_config_vals.at(_bdpath_mode);

    int dp_sz = config_get_table_size(delay_params_table.c_str());
    int dv_sz = config_get_table_size(delay_vals_table.c_str());
    Assert (dp_sz==dv_sz, "Delay line table size mismatch");
    delay_table_sz = dp_sz;

    mux_table_sz = config_get_table_size("synth.ring.bundled.mux_delays");
    or_table_sz = config_get_table_size("synth.ring.bundled.or_delays");

    _delay_margin = delay_margin;
    delay_multiplier = float(_delay_margin)/100;

    // Instance countersq
    _block_id = 0;
    _itb_wrapper_id = 0;
    _bd_chan_id = 0;
    _sync_chan_id = 0;
    _expr_id = 0;
    // _expr_block_id = 0;
    _mux_block_id = 0;
    _branch_id = 0;

    runtime1 = 0;
    runtime2 = 0;

    eeo = new ExprCache(externopt_toolname, ((bundled==1)?bd:qdi), false, _exprfile);
    Assert ((eeo), "Could not create mapper!");
}

long long RingForge::get_runtime()
{
    return runtime1;
}

long long RingForge::get_io_runtime()
{
    return runtime2;
}

void RingForge::run_forge ()
{
    Assert (_structure_check_q(_c), "Program not of allowed form. \
        \nMust be Q1 or (Q1||Q2||..) \
        \nwhere Qi is: \
        \nsemi_list(xi := vi); *[ true -> Pi ] \
        \nand Pi has no internal loops");

    if (_c->type == ACT_CHP_COMMA) {
        if (!(_c->label) || (_c->label && strcmp(_c->label,"top_decomp")!=0)) {
            warning ("Detected top-level parallel loops within same process: *[P1] || *[P2]. \
                \nThis is not recommended in user input CHP.");
        }
    }
    if (_c->type == ACT_CHP_COMMA) {
        for (listitem_t *li = list_first (_c->u.semi_comma.cmd) ; li ; li = list_next(li)) {
            _run_forge_helper ((act_chp_lang_t *)(list_value(li)));
        }
    }
    else {
        _run_forge_helper (_c);
    }
}

static void chp_err (act_chp_lang_t *c) {
    fprintf(stderr, "\nchp : (");
    chp_print(stderr, c);
    fprintf(stderr, ")\nc->type: %d\n", c->type);
}

bool RingForge::_structure_check_q (act_chp_lang_t *c)
{
    if (c->type == ACT_CHP_LOOP || c->type == ACT_CHP_DOLOOP || c->type == ACT_CHP_SEMI) {
        return _structure_check_p (c);
    }
    if (c->type == ACT_CHP_COMMA) {
        bool all_ok = true;
        for (listitem_t *li = list_first (c->u.semi_comma.cmd) ; li ; li = list_next(li)) {
            all_ok &= _structure_check_p ((act_chp_lang_t *)(list_value(li)));
        }
        if (all_ok) return true;
    }
    chp_err(c);
    return false;
}

bool RingForge::_structure_check_p (act_chp_lang_t *c)
{
    if (c->type == ACT_CHP_LOOP || c->type == ACT_CHP_DOLOOP)
        return _internal_loop_check (c->u.gc->s);
    
    if (c->type == ACT_CHP_SEMI) {
        int n_loops = 0;
        bool loop_ok = false;
        for (listitem_t *li = list_first (c->u.semi_comma.cmd) ; li ; li = list_next(li)) {
            act_chp_lang_t *stmt = (act_chp_lang_t *)(list_value(li));
            if (!(stmt->type==ACT_CHP_ASSIGN || stmt->type==ACT_CHP_LOOP 
               || stmt->type==ACT_CHP_DOLOOP || stmt->type==ACT_CHP_FUNC)) {
                chp_err(stmt);
                return false;
            }
            if ((stmt->type==ACT_CHP_LOOP || stmt->type==ACT_CHP_DOLOOP)) {
                n_loops++;
                loop_ok = !(li->next) && _internal_loop_check (stmt->u.gc->s) && !(stmt->u.gc->next);
            }
        }
        if (n_loops==1 && loop_ok) return true;
    }
    chp_err(c);
    return false;
}

bool RingForge::_internal_loop_check (act_chp_lang_t *c)
{
  switch (c->type) {
  case ACT_CHP_SKIP:
  case ACT_CHP_ASSIGN:
  case ACT_CHP_SEND:
  case ACT_CHP_RECV:
    return true;
    break;
  case ACT_CHP_COMMA:
  case ACT_CHP_SEMI: {
        bool ret = true;
        for (listitem_t *li = list_first (c->u.semi_comma.cmd); li; li = list_next (li)) 
        {
            ret &= _internal_loop_check ((act_chp_lang_t *) list_value (li));
        }
        return ret;
    }
    break;
  case ACT_CHP_LOOP:
  case ACT_CHP_DOLOOP:
    return false;
    break;
  case ACT_CHP_SELECT_NONDET:
  case ACT_CHP_SELECT: {
        act_chp_gc_t *gc = c->u.gc;
        bool ret = true;
        while (gc) 
        {
            ret &= _internal_loop_check (gc->s);
            gc = gc->next;
        }
        return ret;
    }
    break;

  case ACT_CHP_FUNC:
    return true;
    break;
  default:
    fatal_error ("What?");
    break;
  }
  return false;
}

bool RingForge::_fill_in_ics (act_chp_lang_t *&c)
{
    bool any_added = false;
    int type = -1;
    std::vector<act_connection *> ic_var_list = {};
    list_t *ics_assns = NULL;
    act_chp_lang_t *main_loop = NULL;

    // Get data 
    switch (c->type) {
    case ACT_CHP_LOOP:
    case ACT_CHP_DOLOOP:
        type = 0;
        ic_var_list = ((latch_info_t *)(c->space))->live_vars;
        ics_assns = list_new();
        main_loop = c;
        break;
    case ACT_CHP_SEMI:
        type = 1;
        ics_assns = list_dup(c->u.semi_comma.cmd);
        list_delete_tail(ics_assns);
        for (listitem_t *li = list_first (c->u.semi_comma.cmd); li; li = list_next (li)) 
        {
            act_chp_lang_t *stmt = (act_chp_lang_t *)(list_value(li));
            if (stmt->type == ACT_CHP_LOOP || stmt->type == ACT_CHP_DOLOOP) {
                ic_var_list  = ((latch_info_t *)(stmt->space))->live_vars;
                main_loop = stmt;
            }
        }
        break;
    default:
        fatal_error ("Something went really wrong in filling in I.C.s");
        break;
    }

    Assert (main_loop, "Hmm");

    // Look through ICs and fill in missing ones
    for ( auto li  : ic_var_list ) 
    {
        var_info *vi = _get_var_info(li->toid());
        ActId *id = vi->id;

        if (vi->fischan==0) {
            bool exists = false;
            for (listitem_t *li = list_first (ics_assns); li; li = list_next (li)) 
            {
                act_chp_lang_t *assn = (act_chp_lang_t *)(list_value(li));
                Assert ((assn->type == ACT_CHP_ASSIGN), "IC is not assignment?");
                char rname[1024];
                char lname[1024];
                get_true_name(lname, assn->u.assign.id, _p->CurScope());
                get_true_name(rname, id, _p->CurScope());
                if (strcmp(lname, rname)==0) exists = true;
            }

            if (!exists) {
                any_added = true;
                act_chp_lang_t *assn = new act_chp_lang_t;
                assn->type = ACT_CHP_ASSIGN;
                assn->label = NULL;
                assn->space = NULL;
                assn->u.assign.id = id;
                Expr *e = const_expr (0);
                assn->u.assign.e = e;
                list_append_head (ics_assns, assn);
            }
        }
    }
    list_append (ics_assns, main_loop);
    
    if (type==1) {
        c->u.semi_comma.cmd = ics_assns;
    }
    else {
        act_chp_lang_t *newc = new act_chp_lang_t;
        newc->type = ACT_CHP_SEMI;
        newc->label = NULL;
        newc->space = NULL;
        newc->u.semi_comma.cmd = list_dup(ics_assns);
        c = newc;
    }

    return any_added;
}

void RingForge::_run_forge_helper (act_chp_lang_t *c)
{
    auto ss1 = high_resolution_clock::now();
    bool printt = false;
    RingVarAnalysis *lva = new RingVarAnalysis (_fp, _p, c);
    lva->generate_var_info();

    if (printt) fprintf (_fp, "\n/* \n");
    if (printt) fprintf (_fp, "// Live Vars Info -----------------\n");
    if (printt) lva->print_var_info();
    if (printt) fprintf (_fp, "// --------------------------------\n\n");

    construct_var_infos (c);
    if(_fill_in_ics(c)) {
        fprintf (stdout, "\nWARNING: Some variables were uninitialized in CHP");
        fprintf (stdout, "; initializing these to zero.\n");
    }

    RingVarAnalysis *lva2 = new RingVarAnalysis (_fp, _p, c);
    lva2->generate_var_info();

    construct_var_infos (c);
    auto st1 = high_resolution_clock::now();
    auto d2 = duration_cast<microseconds>(st1 - ss1);
    if (printt) fprintf(stdout, "\n\n// Ring Forge Pre-processing Duration 1:");
    if (printt) fprintf(stdout, "%lld microseconds \n\n", d2.count());

    Assert (datapath_style==SSA, "Non-SSA is deprecated!"); 
    auto ss2 = high_resolution_clock::now();

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

    Assert (!_check_all_muxes_mapped(c,false), "Mux input mapping incomplete!");

    fprintf (_fp, "// Branched Ring ------------------\n");

    auto st2 = high_resolution_clock::now();
    auto d3 = duration_cast<microseconds>(st2 - ss2);
    if (printt) fprintf(stdout, "\n\n// Ring Forge Pre-processing Duration 2:");
    if (printt) fprintf(stdout, "%lld microseconds \n\n", d3.count());

    generate_branched_ring (c,1,0,0);
}

unsigned long eval_ic (Expr *);

int RingForge::_compute_delay_line_param(double delay)
{
    if (delay==0) return 0;
    
    std::string delay_params_table = bd_mode_config_params.at(_bdpath_mode);
    std::string delay_vals_table = bd_mode_config_vals.at(_bdpath_mode);

    int *dparams = config_get_table_int(delay_params_table.c_str());
    double *dvals = config_get_table_real(delay_vals_table.c_str());

    int itr = 0;
    while(itr<delay_table_sz && delay>dvals[itr])
    {
        itr++;
    }
    Assert ((itr>0 && itr<delay_table_sz), "Delay out of range of producible delay lines");
    Assert ((delay>=dvals[itr-1] && delay<=dvals[itr]), "Delay PWL curve non-monotonic?");
    // linear interpolation
    double left = dvals[itr-1];
    double right = dvals[itr];
    double theta = (right-delay)/(right-left);
    Assert ((theta>=0 && theta<=1), "What");

    int n = std::ceil( theta*dparams[itr-1] + (1-theta)*dparams[itr] - 0.001 );
    Assert ((n>=dparams[itr-1] && n<=dparams[itr]), "Delay PWL curve non-monotonic?");

    return int(std::ceil(n*delay_multiplier));
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
    case E_FALSE:
    case E_TRUE:
        return (unsigned long)eval_bool_expr(q); break;
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

static bool _expr_has_negated_probe (Expr *e)
{
    return bool(act_expr_has_neg_probes(e));
}

static bool _guards_have_negated_probes (act_chp_gc_t *gc)
{
  while (gc) {
    if (gc->g) {
      if (_expr_has_negated_probe (gc->g)) {
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
    Assert ((l->type == LatchType::Latch) || (l->type == LatchType::Alias), "generate latch for non-assignment?");
    Assert (l->latch_numbers.size()==1, "latch generation on struct!");
    int latch_id = l->latch_numbers[0];
    bool is_latch = (l->type == LatchType::Latch);
    static char buf[1024];
    Assert (v->iwrite < v->nwrite, "Something went wrong in latch info tracking!");
    if (v->fisextbool) {
        get_true_name(buf, v->id, _p->CurScope(), false);
        fprintf(_fp, "capture_bool_ext %s%s_%d(%s);\n", capture_block_prefix, v->name, latch_id, buf);
    }
    else if (init_val == -1)
    {
        if (is_latch) {
            fprintf(_fp, "capture<%d,%d,%d> %s%s_%d;\n", _compute_delay_line_param(capture_delay), 
                                                        _compute_delay_line_param(pulse_width), 
                                                    v->width, capture_block_prefix, v->name,latch_id);
        }
        else {
            fprintf(_fp, "capture_dummy<%d,%d,%d> %s%s_%d;\n", _compute_delay_line_param(capture_delay), 
                                                        _compute_delay_line_param(pulse_width), 
                                                    v->width, capture_block_prefix, v->name,latch_id);
        }
    }
    else
    {
        Assert (is_latch, "init val. latch but not real latch??");
        fprintf(_fp, "capture_init_new<%d,%d,%d,%lli> %s%s_%d;\n", _compute_delay_line_param(capture_delay), 
                                                                _compute_delay_line_param(pulse_width), 
                                                v->width, init_val, capture_block_prefix, v->name,latch_id);
    }
    v->iwrite++;
    v->latest_for_read = latch_id;
    return latch_id;
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
    static char buf[1024];
    
    Assert (v->iread==0, "Already created latch for this variable?");
    if (!(v->fischan))
    {
        if (v->fisbool) {
            get_true_name(buf, v->id, _p->CurScope(), false);
            fprintf(_fp, "capture_bool %s%s_%d(%s);\n", capture_block_prefix, 
                                        v->name, latch_id, buf);
        }
        else {
            fprintf(_fp, "capture_init_non_ssa<%d,%d,%d,%lld,%d> %s%s_%d;\n", 
                                _compute_delay_line_param(capture_delay), 
                                _compute_delay_line_param(pulse_width),
                                v->width, init_val, write_ports, 
                                capture_block_prefix, v->name,latch_id);
        }
        v->iread++;
        v->latest_for_read = latch_id;
    }
    return latch_id;
} 

int RingForge::handle_struct_recv (ActId *var, ActId *chan, latch_info_t *l, int block_id)
{
    Data *d;
    ActId **res;
    int *types;
    int nb, ni;
    var_info *vi;
    char tmpchan[1024];
    get_true_name (tmpchan, chan, _p->CurScope(), false);
    auto chan_name = strcat(tmpchan, ".");
    strcat(chan_name, struct_chan_name);

    Assert ((l->type == LatchType::Latch) || (l->type == LatchType::Alias), 
                "generate latch for non-assignment?");
    
    InstType *it = _p->CurScope()->localLookup (var, NULL);
    Assert (it, "Hmm");
    Assert (TypeFactory::isStructure (it), "Hmm");
    d = dynamic_cast<Data *>(it->BaseType());
    d->getStructCount (&nb, &ni);
    res = d->getStructFields (&types);
    FREE (types);

    int pos = 0;
    ActId *tail = var->Tail ();

    int bw = struct_bw(var);
    // use a wide latch to capture everything at once
    // then place dummies on output for compatibility
    fprintf(_fp, "capture<%d,%d,%d> %s_struct_%d;\n", 
                    _compute_delay_line_param(capture_delay), 
                    _compute_delay_line_param(pulse_width), 
                    bw, capture_block_prefix, block_id);
    // FIXME: this might cause issues in qdi datapath...
    fprintf(_fp, "%s_struct_%d.go = %s%d.data;\n",capture_block_prefix,
                    block_id, ring_block_prefix, block_id);
    fprintf(_fp, "%s_struct_%d.din = %s.d;\n",capture_block_prefix,
                    block_id, chan_name);
    fprintf(_fp, "%s_struct_%d.tx.a = %s.a;\n",capture_block_prefix,
                    block_id, chan_name);

    for (int i=ni+nb-1 ; i>=0 ; i--) 
    {
        int sz;
        InstType *xit;
        Assert (d->getStructOffset (res[i], &sz, &xit) != -1, "What?");

        int lw = TypeFactory::bitWidth (xit);

        tail->Append (res[i]);
        vi = _get_var_info(tail);
        tail->prune ();

        int latch_id = l->latch_numbers[(ni+nb-1)-i];
        
        fprintf(_fp, "capture_dummy<%d,%d,%d> %s%s_%d;\n", 
                    _compute_delay_line_param(capture_delay), 
                    _compute_delay_line_param(pulse_width), 
                    vi->width, capture_block_prefix, vi->name, latch_id);
        fprintf(_fp, "%s%s_%d.go.r = %s_struct_%d.go.a;\n",
                        capture_block_prefix, vi->name, latch_id,
                        capture_block_prefix, block_id);
        fprintf(_fp, "%s%s_%d.din = %s_struct_%d.dout[%d..%d];\n",
                        capture_block_prefix, vi->name, latch_id,
                        capture_block_prefix, block_id, pos, pos+lw-1);
        fprintf(_fp, "%s%s_%d.tx.a = %s.a;\n",
                        capture_block_prefix, vi->name, latch_id,
                        chan_name);

        pos += lw;

        delete res[i];
        delete xit;
    }
    return 0;
}

int RingForge::struct_bw (ActId *id)
{
    if (!id) return -1;

    InstType *it = _p->CurScope()->localLookup (id, NULL);
    Assert (it, "Hmm");
    Assert (TypeFactory::isStructure (it), "Hmm");
    Data *d = dynamic_cast<Data *>(it->BaseType());

    int w = TypeFactory::totBitWidth (d);
    return w;
} 

Expr *RingForge::struct_to_int_concat(Expr *e_in)
{
    auto d = act_expr_is_structure(_p->CurScope(), e_in);
    Assert (d, "What?");
    int nb, ni;
    d->getStructCount (&nb, &ni);
    Assert (nb+ni>0, "Empty struct?!");
    
    int *typecodes;
    ActId **xfield = d->getStructFields (&typecodes);

    Expr *e, *f;
    NEW (e, Expr);
    e->type = E_CONCAT;
    e->u.e.l =  NULL;
    e->u.e.r = NULL;
    f = e;
    for (int i=0; i < nb + ni; i++) {
      ActId *tid = ((ActId *)e_in->u.e.l)->Clone();
      tid->Append (xfield[i]);
      f->u.e.l = act_expr_var (tid);
      if (i != (nb + ni - 1)) {
        NEW (f->u.e.r, Expr);
        f = f->u.e.r;
        f->u.e.l = NULL;
        f->u.e.r = NULL;
        f->type = E_CONCAT;
      }
    }

    if (verbose) {
        fprintf(_fp, "\n// inlined struct expr: ");
        print_uexpr(_fp, e);
        fprintf(_fp, "\n");
    }
    return e;
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
    bool is_struct = false;
    int block_id;
    int expr_inst_id;
    char chan_name[10240];
    int latch_id;
    int bw;
    var_info *vi;

    block_id = _gen_block_id();

    switch(c->type)
    {
    case ACT_CHP_ASSIGN:
        e = c->u.assign.e;
        var = c->u.assign.id;
        if (verbose) {
            fprintf(_fp,"\n// Pipe block and data for action: ");
            chp_print(_fp,c);
        }
        fprintf(_fp,"\n");
        fprintf(_fp,"elem_c_paa %s%d;\n",ring_block_prefix,block_id);

        vi = _get_var_info(var);
        bw = vi->width;
        if (bw>0) {
        expr_inst_id = _generate_expr_block(e,bw,true);
        Assert ((c->space), "No latch info? (_generate_pipe_element)");
        if (init_latch == -1) {
            latch_id = _generate_single_latch(vi, (latch_info_t *)(c->space));  
        }
        else {
            latch_id = init_latch;
            Assert (datapath_style==NON_SSA, "Hmm");
        }
        // connect output of math block to latch input
        Assert (datapath_style==SSA, "wth");
        fprintf(_fp, "connect_exprblk_assign<%d> %s%d(%s%d.out,%s%s_%d.din,%s%s_%d.tx);\n", bw, 
                        expr_block_output_prefix, expr_inst_id,
                        expr_block_instance_prefix,expr_inst_id,
                        capture_block_prefix, vi->name,latch_id,
                        capture_block_prefix, vi->name,latch_id
                        );
        // connect pipe block to delay_expr input
        fprintf(_fp,"delay_expr_%d.m1 = %s%d.zero;\n",expr_inst_id,ring_block_prefix,block_id);
        // connect delay_expr output to capture block
        fprintf(_fp,"delay_expr_%d.p1 = %s%s_%d.go;\n",expr_inst_id,capture_block_prefix,
                                            vi->name,latch_id);
        }
        else {
        fprintf(_fp,"%s%d.zero.r = %s%d.zero.a;\n",ring_block_prefix,block_id,ring_block_prefix,block_id);
        }
        break;

    case ACT_CHP_SEND:
        chan = c->u.comm.chan;
        e = c->u.comm.e;
        get_true_name (chan_name, chan, _p->CurScope(), false);
        if (verbose) {
            fprintf(_fp,"\n// Pipe block and data for action: ");
            chp_print(_fp,c);
        }
        fprintf(_fp,"\n");
        fprintf(_fp,"elem_c_paa_send %s%d;\n",ring_block_prefix,block_id);
        bw = _bitWidth(chan);
        if (e && bw>0) {
            if (TypeFactory::isStructure(TypeFactory::getChanDataType(_p->CurScope()->localLookup(chan, NULL)))) {
                Assert (e->type==E_VAR, "Structure send must be pure var, not a function");
                e = struct_to_int_concat(e);
            }
            if (need_nesting(chan)) {
                strcat (chan_name, ".");
                strcat (chan_name, struct_chan_name);
            }
                // compute explicit form of int() of struct
            fprintf(_fp,"connect_outchan_to_ctrl<%d> %s%d;\n",bw, conn_block_prefix,block_id);
            fprintf(_fp,"%s%d.ch = %s;\n\n",conn_block_prefix,block_id,chan_name);

            expr_inst_id = _generate_expr_block(e,bw,true);
            fprintf(_fp,"connect_exprblk_dout<%d> %s%d(%s%d.out,%s%d.e);\n",bw,
                            expr_block_output_prefix, expr_inst_id,
                            expr_block_instance_prefix,expr_inst_id,
                            conn_block_prefix,block_id);

            // connect to delay_line
            fprintf(_fp,"delay_expr_%d.m1 = %s%d.zero;\n",expr_inst_id,ring_block_prefix,block_id);
            fprintf(_fp,"delay_expr_%d.p1 = %s%d.ctrl;\n",expr_inst_id,conn_block_prefix,block_id);
        }
        else { // dataless action
            fprintf(_fp,"connect_outchan_to_ctrl<%d> %s%d;\n",bw, conn_block_prefix,block_id);
            fprintf(_fp,"%s%d.ch = %s;\n",conn_block_prefix,block_id,chan_name);
            fprintf(_fp,"%s%d.ctrl = %s%d.zero;\n\n",conn_block_prefix,block_id,ring_block_prefix,block_id);
        }
        break;

    case ACT_CHP_RECV:
        chan = c->u.comm.chan;
        var = c->u.comm.var;
        get_true_name (chan_name, chan, _p->CurScope(), false);
        if (verbose) {
            fprintf(_fp,"\n// Pipe block and data for action: ");
            chp_print(_fp,c);
        }
        fprintf(_fp,"\n");
        fprintf(_fp,"elem_c_ppa %s%d;\n",ring_block_prefix,block_id);
        bw = _bitWidth(chan);
        is_struct = (TypeFactory::isStructure(TypeFactory::getChanDataType(_p->CurScope()->localLookup(chan, NULL))));
        if (need_nesting(chan)) {
            strcat (chan_name, ".");
            strcat (chan_name, struct_chan_name);
        }
        fprintf(_fp,"connect_inchan_to_ctrl<%d> %s%d;\n",bw, conn_block_prefix,block_id);
        fprintf(_fp,"%s%d.ctrl = %s%d.zero;\n",conn_block_prefix,block_id,ring_block_prefix,block_id);
        fprintf(_fp,"%s%d.ch = %s;\n\n",conn_block_prefix,block_id,chan_name);
        
        if (var && is_struct) {
            handle_struct_recv (var, chan, (latch_info_t *)(c->space), block_id);
        }
        else if (var) {
            vi = _get_var_info(var);
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
            Assert (datapath_style==SSA, "wth");
            fprintf(_fp, "%s%s_%d.go = %s%d.data;\n",capture_block_prefix,
                            vi->name,latch_id,ring_block_prefix,block_id);
            if (bw>0) fprintf(_fp, "%s%s_%d.din = %s.d;\n",capture_block_prefix,
                                            vi->name,latch_id,chan_name);
            // fprintf(_fp, "connect_chan_to_capture<%d,%d> cctc_%d(%s%s_%d.din,%s.d);\n", vi->width, bw, block_id,
            //     capture_block_prefix, vi->name,latch_id,chan_name);
            fprintf(_fp, "%s%s_%d.tx.a = %s.a;\n",capture_block_prefix,
                                            vi->name,latch_id,chan_name);
        }
        else { // dataless action
            fprintf(_fp,"%s%d.data.r = %s%d.data.a;\n",ring_block_prefix,
                            block_id,ring_block_prefix,block_id);
        }
        break;

    case ACT_CHP_SKIP:
        if (verbose) {
            fprintf(_fp,"\n// Pipe block for action: ");
            chp_print(_fp,c);
        }
        fprintf(_fp,"\n");
        fprintf(_fp,"elem_c_skip %s%d;\n",ring_block_prefix,block_id);
        break;

    default:
        fatal_error("Shouldn't be here... (generate_pipe_element)");
        break;
    }
    return block_id;
}

int RingForge::_generate_pipe_element_lcd(int n)
{
    int block_id = _gen_block_id();
    if (verbose) {
        fprintf(_fp,"\n// Pipe block for lcd. transmission.");
            }
    fprintf(_fp,"\n");
    fprintf(_fp,"elem_c_paa_c<%d> %s%d;\n",n,ring_block_prefix,block_id);
    return block_id;
}

/*
    Similar to the previous, but used to pulse latch for
     implementing loop-carried dependencies from 
    one iteration of the ring to the next. 
*/
int RingForge::_generate_pipe_element_lcd(int type, ActId *var, int itr, int blk_id, int blk_port)
{
    int block_id;
    int first_latch_id, last_latch_id;
    var_info *vi;
    int va_id;

    block_id = blk_id;
    Assert (var, "no variable (_generate_pipe_element_lcd");
    Assert (type==ACT_CHP_ASSIGN, "LCD pipe block for non-assignment?");

    vi = _get_var_info(var);
    first_latch_id = 0;
    last_latch_id = vi->latest_for_read;
    // connect last latch output to first latch input
    va_id = _gen_var_access_id();
    if (itr==0) {
        fprintf(_fp,"var_access<%d> %s%d(%s%s_%d.dout,);\n",
                                vi->width,var_access_prefix,va_id,
                                capture_block_prefix,vi->name, last_latch_id);
        fprintf(_fp, "connect_exprblk_assign<%d> %s_lcd_%d(%s%d.dout,%s%s_%d.din,%s%s_%d.tx);\n", 
                            vi->width, expr_block_output_prefix, va_id,
                            var_access_prefix,va_id,
                            capture_block_prefix,vi->name,first_latch_id,
                            capture_block_prefix,vi->name,first_latch_id
                            );
        // connect pipe block action port to latch go port
        fprintf(_fp,"%s%d.zero[%d] = %s%s_%d.go;\n",ring_block_prefix,block_id,
                                            blk_port,capture_block_prefix,
                                            vi->name,first_latch_id);
    }
    else if (itr==1) {
        fprintf(_fp,"%s%d.zero[%d] = %s%s_%d.go2;\n",ring_block_prefix,block_id,
                                            blk_port,capture_block_prefix,
                                            vi->name,first_latch_id);
    }
    else {
        fatal_error ("what");
    }
    return block_id;
}

int RingForge::_generate_pause_element()
{   
    if (verbose) fprintf(_fp,"\n// Pause element to suspend execution of ring");
    fprintf(_fp,"\n");
    int id = _gen_block_id();
    fprintf(_fp,"elem_c_pause %s%d;\n",ring_block_prefix,id);
    return id;
}

/*
    Generate dataless ITB that initializes the ring.
*/
int RingForge::_generate_itb()
{   
    if (verbose) fprintf(_fp,"\n// Initial token buffer to initialize ring");
    fprintf(_fp,"\n");
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
    Generate a non-deterministic selection split block for branches
*/
int RingForge::_generate_nds_split(int n)
{
    int block_id = _gen_block_id();
    fprintf(_fp,"nds_split<%d> %s%d;\n", n, ring_block_prefix, block_id);
    return block_id;
}

/*
    Generate a non-deterministic selection split (stable-guard version) block for branches
*/
int RingForge::_generate_nds_split_stable(int n)
{
    int block_id = _gen_block_id();
    fprintf(_fp,"nds_split_stable<%d> %s%d;\n", n, ring_block_prefix, block_id);
    return block_id;
}

/*
    Generate a probed-but-deterministic selection split block for branches
*/
int RingForge::_generate_probed_ds_split(int n)
{
    int block_id = _gen_block_id();
    fprintf(_fp,"probed_ds_split<%d> %s%d;\n", n, ring_block_prefix, block_id);
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
int RingForge::_generate_expr_block(Expr *e, int out_bw, bool connect_inputs)
{
    Assert ((eeo), "No mapper exists");

    // collect input vars info
    ac.clear();
    _inexprmap = ihash_new (0);
    _inwidthmap = ihash_new (0);

    auto ss1 = high_resolution_clock::now();
    e = expr_dup(e);
    e = expr_dag(e);

    _reset_expr_id();
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

    config_set_int("synth.expropt.verbose", 1);
    config_set_int("synth.expropt.abc.use_constraints", 1);
    config_set_int("synth.expropt.vectorize_all_ports", 1);

    // output bitwidth and block id for name
    int xid = _gen_expr_block_id();
    int out_expr_width = out_bw;
    int delay_line_n;

    if (verbose) fprintf(_fp, "// output bitwidth: %d bits\n",out_expr_width);
    
    if (e->type == E_INT)
    {
        config_set_int("expropt.abc_use_constraints", 0);
    }

    auto st1 = high_resolution_clock::now();
    auto de = duration_cast<microseconds>(st1 - ss1);
    // fprintf(stdout, "\n\n// one expr duration: %lld microseconds \n\n", de.count());

    // run abc, then v2act to create the combinational-logic-for-math process
    ExprBlockInfo *ebi = eeo->synth_expr(out_expr_width, e, all_leaves, _inexprmap, _inwidthmap);
    runtime1 += ebi->getRuntime();
    runtime2 += ebi->getIORuntime();
    runtime2 += de.count();
    
    if (e->type == E_INT) 
    {
        config_set_int("expropt.abc_use_constraints", 1);
        delay_line_n = 0;
    }
    else 
    {
        Assert (ebi->getDelay().exists(), "Delay not extracted by expression synthesis!");
        double typ_delay_ps = (ebi->getDelay().typ_val)*1e12;
        if (typ_delay_ps <= 0) { warning("non-positive delay from expression synthesis: %lfps", typ_delay_ps); }

        delay_line_n = _compute_delay_line_param(typ_delay_ps); 
        Assert(delay_line_n>=0, "negative delay line param?");

        if (verbose) fprintf(_fp, "\n// typical delay: %gps\n",typ_delay_ps);
    }

    _instantiate_expr_block (ebi->getID(), xid, all_leaves, connect_inputs);

    fprintf(_fp,"delay_line_chan<%d> delay_expr_%d;\n",delay_line_n,xid);
    // fprintf (stdout, "\n// bye from expropt\n");

    ebi->~ExprBlockInfo();
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
int RingForge::_generate_expr_block_for_sel(Expr *e, int xid, bool connect_inputs)
{
    Assert ((eeo), "No mapper exists");

    // collect input vars info
    ac.clear();
    _inexprmap = ihash_new (0);
    _inwidthmap = ihash_new (0);
    e = expr_dup(e);
    e = expr_dag(e);
    
    _reset_expr_id();
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
    config_set_int("synth.expropt.verbose", 0);
    config_set_int("synth.expropt.abc.use_constraints", 1);
    config_set_int("synth.expropt.vectorize_all_ports", 1);

    int out_expr_width = 1;

    // run abc, then v2act to create the combinational-logic-for-math process
    ExprBlockInfo *ebi = eeo->synth_expr(out_expr_width, e, all_leaves, _inexprmap, _inwidthmap);
    runtime1 += ebi->getRuntime();
    runtime2 += ebi->getIORuntime();
    
    Assert (ebi->getDelay().exists(), "Delay not extracted by expression synthesis!");
    double typ_delay_ps = (ebi->getDelay().typ_val)*1e12;

    int delay_line_n = _compute_delay_line_param(typ_delay_ps); 
    Assert(delay_line_n>=0, "negative delay line param?");

    if (verbose) fprintf(_fp, "\n// typical delay: %lfps\n",typ_delay_ps);
    fprintf(_fp,"\n");
    _instantiate_expr_block (ebi->getID(), xid, all_leaves, connect_inputs);

    ebi->~ExprBlockInfo();
    ebi = NULL;

    // free all temporary data structures 
    ihash_free (_inexprmap);
    _inexprmap = NULL;
    ihash_free (_inwidthmap);
    _inwidthmap = NULL;
    list_free (all_leaves);

    // force write output file
    fflush(_fp);
    return delay_line_n;
}

/*
    Function to call an external logic synthesis tool to
    generate combinational logic to implement guard
    evaluators. Currently supports only abc. 
*/
int RingForge::_generate_expr_block_for_sel_all(act_chp_gc_t *gc, int xid, bool connect_inputs)
{
    Assert ((eeo), "No mapper exists");

    std::vector<Expr *> e_list = {};
    while (gc) {
        Assert (gc->g, "else_gen failed");
        e_list.push_back(gc->g);
        gc = gc->next;
    }

    // collect input vars info
    ac.clear();
    _reset_expr_id();
    _inexprmap = ihash_new (0);
    _inwidthmap = ihash_new (0);
    for (auto &e : e_list) {
        e = expr_dup(e);
        e = expr_dag(e);
        _expr_collect_vars (e, 1);
    }
    
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
    config_set_int("synth.expropt.verbose", 0);
    config_set_int("synth.expropt.abc.use_constraints", 1);
    config_set_int("synth.expropt.vectorize_all_ports", 1);

    // run abc, then v2act to create the combinational-logic-for-math process
    _outexprmap = ihash_new(0);
    _outwidthmap = ihash_new(0);
    _inexprmap_str = ihash_new(0);

    for (listitem_t *li = list_first (all_leaves); li; li = list_next (li)) { 
        Expr *tmp = (Expr *) list_value(li);
        // change from int to C string
        ihash_bucket_t *b_map,*b_new;
        b_map = ihash_lookup(_inexprmap, (long) tmp);
        char *charbuf = (char *) malloc( sizeof(char) * ( 100 + 1 ) );
        snprintf(charbuf, 100, "%s%u",expr_block_input_prefix,b_map->i);
        b_new = ihash_add(_inexprmap_str, (long) tmp);
        b_new->v = charbuf;
    }
    list_t *_outlist = list_new();
    int i=0;
    for ( auto e : e_list ) {
        list_append (_outlist, e);
        ihash_bucket_t *b_map;
        char *cb = (char *) malloc( sizeof(char) * ( 100 + 1 ) );
        snprintf(cb, 100, "out_%u",i);
        b_map = ihash_add(_outexprmap,(long) e);
        b_map->v = strdup(cb);
        ihash_bucket_t *b_width;
        b_width = ihash_add(_outwidthmap,(long) e);
        b_width->i = 1;
        i++;
    }

    std::string sname = std::string(expr_block_prefix) + std::to_string(xid);

    eeo->set_expr_outfile(_exprfile);
    ExprBlockInfo *ebi = eeo->run_external_opt(sname, all_leaves, _inexprmap_str, _inwidthmap, _outlist, _outexprmap, _outwidthmap, NULL, true);
    ebi->setID(std::to_string(xid));
    eeo->set_expr_outfile("");

    ihash_free (_outexprmap);
    _outexprmap = NULL;
    ihash_free (_outwidthmap);
    _outwidthmap = NULL;
    list_free (_outlist);

    runtime1 += ebi->getRuntime();
    runtime2 += ebi->getIORuntime();
    
    Assert (ebi->getDelay().exists(), "Delay not extracted by expression synthesis tool!");
    double typ_delay_ps = (ebi->getDelay().typ_val)*1e12;
    
    int delay_line_n = _compute_delay_line_param(typ_delay_ps); 
    Assert(delay_line_n>=0, "negative delay line param?");
    
    if (verbose) fprintf(_fp, "\n// typical delay: %lfps\n",typ_delay_ps);
    _instantiate_expr_block (ebi->getID(), xid, all_leaves, connect_inputs);
    
    ebi->~ExprBlockInfo();
    ebi = NULL;
    
    // free all temporary data structures 
    ihash_free (_inexprmap);
    _inexprmap = NULL;
    ihash_free (_inwidthmap);
    _inwidthmap = NULL;
    list_free (all_leaves);
    
    // force write output file
    fflush(_fp);
    return delay_line_n;
// #endif
}

/*
    Instantiate a combinational logic block.
*/
void RingForge::_instantiate_expr_block (std::string expr_id, int block_id, list_t *all_leaves, bool connect_inputs)
{
    ihash_bucket_t *ib;
    listitem_t *li;

    // generate instance
    fprintf(_fp,"%s%s %s%d;\n",expr_block_prefix,expr_id.c_str(),expr_block_instance_prefix,block_id);
    
    // connect inputs
    for (li = list_first(all_leaves); li && connect_inputs; li = list_next(li))
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
            }
            else 
            {
                var_info *vi = _get_var_info(var);

                int latch_id = vi->latest_for_read;
                Assert ((latch_id>=0),"var. read before being written?");

                int lbuf = config_get_int("synth.ring.bundled.buffer_latch_reads");
                std::string s_lbuf = lbuf ? "true" : "false";
                int va_id = _gen_var_access_id();
                fprintf(_fp,"var_access<%d,%s> %s%d(%s%s_%d.dout,%s%d.%s%d);\n",
                            vi->width, s_lbuf.c_str(), var_access_prefix, va_id,
                            capture_block_prefix, vi->name, latch_id,
                            expr_block_instance_prefix, block_id,
                            expr_block_input_prefix, ib->i);
            }
        }
        // connect constants to math block inputs
        else if ( e_var->type == E_INT ) {
            fatal_error ("This shouldn't have been used (constant as input to expr block)");
        }
        else { 
            fprintf (stdout, "e_var type: %d \n", e_var->type);
            fatal_error ("leaf (primary input) is neither variable nor constant int?? (instantiate_expr_block)"); 
        }
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
        if ( (tmp->type == E_PROBE) ||
             (tmp->type == E_NOT && tmp->u.e.l->type == E_PROBE) ) {
            int eid = _gen_expr_block_id ();
            Expr *e1 = const_expr (1);
            _generate_expr_block_for_sel (e1,eid,true);
            list_iappend (m, eid);
            list_iappend (data_gl, eid);
        }
        else {
            int eid = _gen_expr_block_id ();
            _generate_expr_block_for_sel (tmp,eid,true);
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
        else if (tmp->type == E_NOT && tmp->u.e.l->type == E_PROBE) {
            int pid = _generate_probe_access_neg ((ActId *)((tmp->u.e.l)->u.e.l));
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
    var_info *vi = _get_var_info(chan);
    int w = vi->width;
    int pid = _gen_var_access_id();
    bool need_nest = need_nesting(chan);
    if (need_nest) w = _bitWidth(chan);
    fprintf (_fp, "probe_access<%d> probe_%d(",w,pid);
    chan->Print(_fp);
    if (need_nest) fprintf(_fp, ".%s", struct_chan_name);
    fprintf (_fp, ");\n");
    return pid;
}

int RingForge::_generate_probe_access_neg (ActId *chan)
{
    var_info *vi = _get_var_info(chan);
    int w = vi->width;
    int pid = _gen_var_access_id();
    bool need_nest = need_nesting(chan);
    if (need_nest) w = _bitWidth(chan);
    fprintf (_fp, "probe_access_neg<%d> probe_%d(",w,pid);
    chan->Print(_fp);
    if (need_nest) fprintf(_fp, ".%s", struct_chan_name);
    fprintf (_fp, ");\n");
    return pid;
}

/*
    Collect all the variables in a given expression and put them 
    in the exprmap and widthmap global variables.
*/
void RingForge::_expr_collect_vars (Expr *&e, int collect_phase)
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
    break;

  case E_BITFIELD:
  case E_VAR:
    if (collect_phase) {
        ActId *var = (ActId *)e->u.e.l;
        var_info *vi = _get_var_info(var);
        if (vi->width == 0) {
            e = const_expr(0);
            break;
        }
        ihash_bucket_t *ib;
        ihash_bucket_t *b_width;
        auto conn = var->Canonical(_p->CurScope());
        if (!ihash_lookup (_inexprmap, (long)e)) 
        {
            if (!ac.count( conn )) {
                ib = ihash_add (_inexprmap, (long)e);
                ib->i = _gen_expr_id();
                b_width = ihash_add (_inwidthmap, (long) e);
                b_width->i = vi->width;
                ac.insert({conn,ib->i});
            }
            else {
                ib = ihash_add (_inexprmap, (long)e);
                ib->i = ac.at(conn);
                b_width = ihash_add (_inwidthmap, (long) e);
                b_width->i = vi->width;
            }
        }
    }
    break;

  case E_PROBE:
    if (collect_phase) {
        // make dummy variable to stand in for probe
        InstType *it = TypeFactory::Factory()->NewInt (_p->CurScope(), Type::NONE, 0, const_expr(1));
        static char buf[1024];
        it = it->Expand(NULL, _p->CurScope());

        ActId *chan = (ActId *)e->u.e.l;
        var_info *vi;
        char tname[1024];
        get_true_name(tname, chan, _p->CurScope());
        snprintf(buf, 1024, "probe_of_%s", tname);
        _p->CurScope()->Add (buf, it);

        // Replace the probe in the original expression with dummy var
        e->type = E_VAR;
        e->u.e.l = (Expr *)(new ActId (buf));

        auto tst = _p->CurScope()->Lookup((ActId *)e->u.e.l);
        Assert (tst, "hmm new id");

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
    if (verbose) fprintf(_fp,"\n// Connecting block_%d & block_%d",prev_block_id, next_block_id);
    fprintf(_fp,"\n");
    fprintf(_fp,"block_%d.m1 = block_%d.p1;\n",next_block_id,prev_block_id);
    return 0;
}

/*
    Connect a given port of a parallel split block to another
    pipeline element. 
*/
int RingForge::_connect_pll_split_outputs_to_pipe (int pll_split_block_id, int pipe_block_id, int pll_split_block_port)
{
    if (verbose) fprintf(_fp,"\n// Connecting parallel split block_%d (output) & pipe block_%d",
                                                pll_split_block_id, pipe_block_id);
    fprintf(_fp,"\n");
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
    if (verbose) fprintf(_fp,"\n// Connecting parallel merge block_%d (input) & pipe block_%d",
                                                pll_merge_block_id, pipe_block_id);
    fprintf(_fp,"\n");
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
    if (verbose) fprintf(_fp,"\n// Connecting selection split block_%d (output) & pipe block_%d",
                                                sel_split_block_id, pipe_block_id);
    fprintf(_fp,"\n");
    fprintf(_fp,"block_%d.co[%d] = block_%d.m1;\n",
                    sel_split_block_id,sel_split_block_port,pipe_block_id);
    return 0;
}

/*
    Connect the output of guard evaluator to a given guard input
    of a selection split block. 
*/
int RingForge::_connect_guards_to_sel_split_input (int sel_split_block_id, int expr_block_id, int sel_split_guard_port)
{
    // inst_i.out is always size-1 array, not just a bool (1-bit datapath compatibility)
    fprintf(_fp,"block_%d.gs[%d] = %s%d.out[0];\n",sel_split_block_id,sel_split_guard_port,
                                            expr_block_instance_prefix, expr_block_id);
    return 0;
}

int RingForge::_connect_guards_to_sel_split_input_multi (int sel_split_block_id, int expr_block_id, int sel_split_guard_port)
{
    // inst_i.out is always size-1 array, not just a bool (1-bit datapath compatibility)
    fprintf(_fp,"block_%d.gs[%d] = %s%d.out_%d[0];\n",sel_split_block_id,sel_split_guard_port,
                                            expr_block_instance_prefix, expr_block_id, sel_split_guard_port);
    return 0;
}

/*
    Connect a pipeline element to a given port on a selection
    merge block. 
*/
int RingForge::_connect_pipe_to_sel_merge_inputs (int sel_merge_block_id, int pipe_block_id, int sel_merge_block_port)
{
    if (verbose) fprintf(_fp,"\n// Connecting selection merge block_%d (input) & pipe block_%d",
                                                sel_merge_block_id, pipe_block_id);
    fprintf(_fp,"\n");
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

std::vector<act_connection *> RingForge::_create_channel_accesses(std::vector<act_connection *> ics)
{
    std::vector<act_connection *> ret = {};
    for ( auto li : ics )
    {
        var_info *vi = _get_var_info(li->toid());
        if (!(vi->fischan)) {
            ret.push_back(li);
        }
        else {
            fprintf(_fp, "chan_access<%d> %s%s_%d(", vi->width, capture_block_prefix, vi->name,0);
            vi->id->Print(_fp);
            fprintf(_fp,");\n");
        }
    }
    return ret;
}

/*
    General synthesis for branched programs.
    Non-SSA style Datapath. 
    [Deprecated]
*/
#if 0
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
            if (verbose) {
                fprintf (_fp, "// %d-way parallel split for actions: ",comma_len);
                chp_print(_fp, c);
            }
            fprintf (_fp, "\n");

            if (verbose) fprintf (_fp, "// %d-way parallel merge",comma_len);
            fprintf (_fp, "\n");
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
            // handle channel variables - i.e. value probes
            iclist = _create_channel_accesses(iclist);
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

        if (verbose) {
            fprintf (_fp, "\n// %d-way selection split for : ", gc_len);
            chp_print(_fp, c);
        }
        fprintf (_fp, "\n");
        if (verbose) fprintf (_fp, "// %d-way selection merge", gc_len);
        fprintf (_fp, "\n");
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
                delay_n_sel = _generate_expr_block_for_sel (gc->g, expr_block_id,true);
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
            if (verbose) fprintf(_fp,"\n// Delaying pre-split-block sync. by max. delay of all guard evaluators");
            fprintf(_fp,"\n");
            fprintf(_fp,"delay_line_chan<%d> delay_select_%d;\n",max_delay_n_sel,sel_split_block_id);
            // connect prev. block p1 to delay_line then connect to select block from the output
            fprintf(_fp,"delay_select_%d.m1 = %s%d.p1;\n",sel_split_block_id,ring_block_prefix,prev_block_id);
            fprintf(_fp,"delay_select_%d.p1 = %s%d.m1;\n",sel_split_block_id,ring_block_prefix,sel_split_block_id);
        }
        else {
            if (verbose) fprintf(_fp,"\n// Probed selection - no need to insert guard evaluator delay \n");
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
            if (verbose) {
                fprintf (stdout, "\n a1of1 pause port placed here : %s%d.pause \n",ring_block_prefix,block_id);
                fprintf (stdout, "\n pause.r must be grounded for ring execution");
            }
            fprintf(_fp,"\n");
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
#endif

/*
    General synthesis for branched programs. Generates a branched ring
    consisting of pipeline elements, splits and merges. If initial 
    conditions exist, processes them by creating an ITB to wrap around
    the branched ring (non-trivial initial conditions and loop
    carried dependencies are inextricably linked). 
*/
// FIXME: comment about what return value and arguments mean
int RingForge::generate_branched_ring(act_chp_lang_t *c, int root, int prev_block_id, int connect_prev)
{
    int block_id, expr_block_id, first_block_id;
    int pll_split_block_id, pll_merge_block_id;
    int sel_split_block_id, sel_merge_block_id;
    int delay_merge_block_id;
    int comma_len, gc_len;
    int pll_port, gp_con_id;
    int delay_n_sel, max_delay_n_sel, delay_n_merge;
    std::pair<int,int> tmp;
    int n_muxes;
    std::vector<ActId *> muxed_vars;
    list_t *gp_connect_ids;
    listitem_t *li, *lj;
    act_chp_gc_t *gc;
    act_chp_lang_t *stmt;
    act_chp_lang_t *main_loop = NULL;
    bool have_probes = false;
    bool need_delay = false;
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
            if (verbose) {
                fprintf (_fp, "// %d-way parallel split for actions: ",comma_len);
                chp_print(_fp, c);
            }
            fprintf (_fp, "\n");

            if (verbose) fprintf (_fp, "// %d-way parallel merge",comma_len);
            fprintf(_fp,"\n");
            pll_split_block_id = _generate_parallel_split(comma_len);
            pll_merge_block_id = _generate_parallel_merge(comma_len);
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
            Assert (main_loop, "Process does not contain top-level infinite loop?");

            first_block_id = _generate_itb();
            prev_block_id = first_block_id;

            // loop through initial condition assignments to create latches with correct initial values
            auto ic_list  = ((latch_info_t *)(main_loop->space))->live_vars;

            // handle channel variables - i.e. value probes
            ic_list = _create_channel_accesses(ic_list);

            for (lj = list_first (c->u.semi_comma.cmd); lj; lj = list_next (lj)) 
            {   
                std::vector<act_connection *> tmp = {};
                act_chp_lang_t *stmt1 = (act_chp_lang_t *)list_value(lj);
                if (stmt1->type != ACT_CHP_LOOP && stmt1->type != ACT_CHP_DOLOOP)
                {
                    Assert (stmt1->type == ACT_CHP_ASSIGN, "Only assignments in initial conditions");
                    id = stmt1->u.assign.id;
                    Expr *e = stmt1->u.assign.e;
                    unsigned long ival = eval_ic(e);
                    char tname[1024];
                    get_true_name(tname, id, _p->CurScope());
                    for ( auto lk : ic_list )
                    {
                        char tname1[1024];
                        get_true_name(tname1, lk->toid(), _p->CurScope());
                        // copy over all vars except the one being initialized
                        if (strcmp(tname, tname1)) { 
                            tmp.push_back(lk);
                        }
                    }
                    vi =  _get_var_info(id);
                    Assert ((stmt1->space), "No latch info? (_generate_branched_ring, initial condition handling)");
                    int latch_id = _generate_single_latch (vi, (latch_info_t *)(stmt1->space), ival);
                    Assert (latch_id == 0, "Same variable has more than one initial condition?");

                    ic_list = tmp;
                }
            }
            if (ic_list.size()!=0) {
                _print_list_of_vars (stderr, ic_list);
                fatal_error ("The above variables were uninitialized in the program. Initialize them please.");
            }

            // main program synthesis
            gc = main_loop->u.gc;
            block_id = generate_branched_ring(gc->s, 0, prev_block_id, 1);
            prev_block_id = block_id;

            int n_lcd = 0;
            // Find actual no. of LCDs
            for (lj = list_first (c->u.semi_comma.cmd); lj; lj = list_next (lj)) {
                act_chp_lang_t *stmt1 = (act_chp_lang_t *)list_value(lj);
                if (stmt1->type != ACT_CHP_LOOP && stmt1->type != ACT_CHP_DOLOOP) {
                    Assert (stmt1->type == ACT_CHP_ASSIGN, "Only assignments in initial conditions");
                    id = stmt1->u.assign.id;
                    vi = _get_var_info(id);
                    if (vi->fisextbool==0) {
                        n_lcd++;
                    }
                }
            }

            for (int ii = 0; n_lcd>0 && ii<2; ii++) 
            {
                int pulser_id = _generate_pipe_element_lcd (n_lcd);
                _connect_pipe_elements (prev_block_id, pulser_id);
                prev_block_id = pulser_id;
                list_t *lcd_blks = list_new();
                int lcd_itr = 0;
                for (lj = list_first (c->u.semi_comma.cmd); lj; lj = list_next (lj)) 
                {
                    act_chp_lang_t *stmt1 = (act_chp_lang_t *)list_value(lj);
                    if (stmt1->type != ACT_CHP_LOOP && stmt1->type != ACT_CHP_DOLOOP)
                    {
                        Assert (stmt1->type == ACT_CHP_ASSIGN, "Only assignments in initial conditions");
                        id = stmt1->u.assign.id;
                        vi = _get_var_info(id);
                        if (vi->fisextbool==0) {
                            block_id = _generate_pipe_element_lcd (ACT_CHP_ASSIGN, id, ii, pulser_id, lcd_itr);
                            lcd_itr++;
                            list_iappend(lcd_blks, block_id);
                        }
                    }
                }
                fprintf(_fp, "\n");
            }

            if (ic_list.size()!=0)
            {
                _print_list_of_vars (stderr, ic_list);
                fatal_error ("The above variables were uninitialized in the program. Initialize them please.");
            }

            _connect_pipe_elements(prev_block_id, first_block_id);
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
            auto iclist  = ((latch_info_t *)(c->space))->live_vars;
            // handle channel variables - i.e. value probes
            iclist = _create_channel_accesses(iclist);
            if (iclist.size()!=0) {                
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

        if (verbose) {
            fprintf (_fp, "\n// %d-way selection split and merge for : ", gc_len);
            chp_print(_fp, c);
        }
        fprintf (_fp, "\n");

        if (!(_guards_have_probes(gc)) && c->type==ACT_CHP_SELECT) {
            need_delay = true;
            sel_split_block_id = _generate_selection_split(gc_len);
        }
        else if (!(_guards_have_negated_probes(gc)) && c->type==ACT_CHP_SELECT_NONDET){
            need_delay = false;
            have_probes = true;
            sel_split_block_id = _generate_nds_split_stable(gc_len);
        }
        else if (!(_guards_have_negated_probes(gc)) && c->type==ACT_CHP_SELECT){
            need_delay = true;
            have_probes = true;
            sel_split_block_id = _generate_probed_ds_split(gc_len);
        }
        else {
            need_delay = false;
            have_probes = true;
            sel_split_block_id = _generate_nds_split(gc_len);
        }

        sel_merge_block_id = _generate_selection_merge(gc_len);

        gp_connect_ids = list_new();

        // if (!have_probes && USE_CACHE==0) {
        if (!have_probes) {
            expr_block_id = _gen_expr_block_id();
            max_delay_n_sel = _generate_expr_block_for_sel_all(gc, expr_block_id, true);
            gc = c->u.gc;

            for (int i = 0; gc; gc = gc->next) {
                _connect_guards_to_sel_split_input_multi (sel_split_block_id, expr_block_id, i);
                block_id = _generate_gp_connect ();
                _connect_sel_split_outputs_to_pipe (sel_split_block_id, block_id, i);
                list_iappend(gp_connect_ids, block_id);
                i++;
            }
        }
        else {
            for (int i = 0; gc; gc = gc->next) {
                Assert ((gc->g) , "should've been fixed in else generation");
                if (have_probes) {   
                    expr_block_id = _generate_probe_circuit (gc->g, expr_block_id);
                }
                else {   
                    expr_block_id = _gen_expr_block_id();
                    delay_n_sel = _generate_expr_block_for_sel (gc->g, expr_block_id,true);
                    if (max_delay_n_sel < delay_n_sel) max_delay_n_sel = delay_n_sel;
                }
                _connect_guards_to_sel_split_input (sel_split_block_id, expr_block_id, i);
                block_id = _generate_gp_connect ();
                _connect_sel_split_outputs_to_pipe (sel_split_block_id, block_id, i);
                list_iappend(gp_connect_ids, block_id);
                i++;
            }
        }

        lj = list_first(gp_connect_ids);
        gc = c->u.gc;
        for (int i = 0; gc; gc = gc->next)
        {   
            _push_read_ids();
            block_id = generate_branched_ring (gc->s, 0, list_ivalue(lj), 1);
            _connect_pipe_to_sel_merge_inputs (sel_merge_block_id, block_id, i);
            _pop_and_restore_read_ids();
            i++; lj = list_next(lj);
        }
        list_free(gp_connect_ids);

        // muxing variables live-out of merge so downstream can access correctly
        muxed_vars = {};
        tmp = _compute_merge_mux_info((latch_info_t *)(c->space), sel_merge_block_id, muxed_vars);
        delay_n_merge = tmp.second;
        n_muxes = tmp.first;
        Assert (n_muxes>=0, "what");
        Assert (n_muxes == muxed_vars.size(), "what2");

        // generate delay line for max guard evaluator delay (split)
        Assert (max_delay_n_sel>=0, "negative delay?");

        if (need_delay) {
            Assert (max_delay_n_sel>=0, "negative delay for non-probed guard evaluators?");
            if (verbose) fprintf(_fp,"\n// Delaying pre-split-block sync. by max. delay of all guard evaluators");
            fprintf(_fp,"\n");
            fprintf(_fp,"delay_line_chan<%d> delay_select_%d;\n",max_delay_n_sel,sel_split_block_id);
            // connect prev. block p1 to delay_line then connect to select block from the output
            fprintf(_fp,"delay_select_%d.m1 = %s%d.p1;\n",sel_split_block_id,ring_block_prefix,prev_block_id);
            fprintf(_fp,"delay_select_%d.p1 = %s%d.m1;\n",sel_split_block_id,ring_block_prefix,sel_split_block_id);
        }
        else {
            if (verbose) fprintf(_fp,"\n// Probed selection - no need to insert guard evaluator delay");
            fprintf(_fp,"\n");
            fprintf(_fp,"%s%d.p1 = %s%d.m1;\n",ring_block_prefix,prev_block_id,ring_block_prefix,sel_split_block_id);
        }

        if (delay_n_merge > 0)
        {
            delay_merge_block_id = _gen_block_id();
            if (verbose) fprintf(_fp,"\n// Delaying post-merge-block sync. by max. delay of all merge muxes");
            fprintf(_fp,"\n");
            if (n_muxes==0) { // can't create zero-length arrays
                fprintf(_fp,"delay_line_chan<%d> %s%d;\n",delay_n_merge,ring_block_prefix, delay_merge_block_id);
            }
            else {
                fprintf(_fp,"delay_line_merge<%d,%d> %s%d;\n",delay_n_merge,n_muxes,ring_block_prefix, delay_merge_block_id);
                int i = 0;
                for ( auto var : muxed_vars ) {
                    var_info *vi = _get_var_info(var);
                    char tname2[1024];
                    get_true_name(tname2, var, _p->CurScope());
                    auto mux_id = vi->latest_for_read;
                    fprintf(_fp, "%s%s_%d.a = %s%d.mux_acks[%d];\n", capture_block_prefix, tname2, mux_id, ring_block_prefix, delay_merge_block_id, i);
                    i++;
                }
            }
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
        
    case ACT_CHP_SKIP:
    case ACT_CHP_ASSIGN:
    case ACT_CHP_ASSIGNSELF:
    case ACT_CHP_SEND:
    case ACT_CHP_RECV:
        if (c->label && !strcmp(c->label,"pause"))
        {
            block_id = _generate_pause_element();
            if (verbose) {
                fprintf (stdout, "\n a1of1 pause port placed here : %s%d.pause \n",ring_block_prefix,block_id);
                fprintf (stdout, "\n pause.r must be grounded for ring execution");
            }
            fprintf(_fp,"\n");
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
        if (!seen.count(x)) {
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
std::pair<int,int> RingForge::_compute_merge_mux_info (latch_info_t *l, int split_block_id, std::vector<ActId *> &mux_vars)
{
    int max_mux_size = 0;
    int max_or_size = 0;

    if ( (l->live_vars).size()==0 ) return {0,0};
    Assert (l->type == LatchType::Mux, "wth");

    int n_muxes = 0;

    int ctr = 0;
    for ( auto li : l->live_vars )
    {
        auto vi = _get_var_info(li->toid());

        if (verbose) fprintf (_fp, "\n// variable: %s", vi->name);
        if (l->merge_mux_latch_number.at(ctr) == -1) 
        {
            if (verbose) fprintf (_fp, "\n// mux not needed");
            ctr++;
            continue;
        }

        auto tmp = _get_pre_sel_latch_and_size (l->merge_mux_inputs.at(ctr));
        int pre_sel_latch = tmp.first;
        int mux_size = tmp.second;
        int or_size = (l->merge_mux_inputs.at(ctr).size()) - mux_size + 1;

        // see if OR-gate is needed
        if (or_size == 1) {
            Assert ((pre_sel_latch==-1), "check that there were no duplicates");
            if (verbose) fprintf(_fp, "\n// assigned in all branches\n"); 
        }
        else { 
            if (verbose) fprintf(_fp, "\n// not assigned in all branches\n");
        }
        // find the variable with the biggest mux+or combo (lookup TODO)
        if (max_mux_size < mux_size) max_mux_size = mux_size;
        if (max_or_size < or_size) max_or_size = or_size;

        // generate the mux (looks like latch to downstream) and connect latch outputs correctly
        int mux_id = l->merge_mux_latch_number.at(ctr);
        fprintf (_fp, "\nmerge_mux_ohc_opt<%d,%d> %s%s_%d;\n", mux_size, vi->width, 
                                                capture_block_prefix, vi->name, mux_id);
        n_muxes++;
        mux_vars.push_back(li->toid());

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
                // NOTE: THIS IS MERGE BLOCK ID NOW
                fprintf (_fp, "or_%s_%d.in[%d] = %s%d.ci[%d].r;\n", vi->name, mux_id, ctr2, 
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
                // NOTE: THIS IS MERGE BLOCK ID NOW
                fprintf (_fp, "%s%s_%d.c[%d] = %s%d.ci[%d].r;\n", capture_block_prefix, vi->name, 
                                                        mux_id, ctr_mux_port, ring_block_prefix, 
                                                    split_block_id, ctr_sel_br);
                // connect mux input data
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
        double max_delay = _lookup_mux_delays (max_mux_size, max_or_size);
        return {n_muxes, _compute_delay_line_param(max_delay)};
    }
    else {
        return {n_muxes, 0};
    }
}

double RingForge::_lookup_mux_delays (int mux_sz, int or_sz)
{
    Assert (mux_sz>0, "What is this mux");
    Assert (or_sz>=0, "What is this or");
    double *mux_table = config_get_table_real("synth.ring.bundled.mux_delays");
    double *or_table  = config_get_table_real("synth.ring.bundled.or_delays");

    double mux_del = ((mux_sz-1)<mux_table_sz) ? mux_table[mux_sz-1] : mux_table[mux_table_sz-1];
    double or_del  = ((or_sz)<or_table_sz) ? or_table[or_sz] : or_table[or_table_sz-1];

    return mux_del + or_del;
}

void RingForge::_print_list_of_vars (FILE *fp, std::vector<act_connection *> vars)
{
    if (vars.size()==0) {
        fprintf (fp, "\nempty list\n");
        return;
    }
    fprintf(fp, "\n-----------\n");
    for ( auto li : vars ) {

        fprintf(fp, "%s, ", li->toid()->getName());
    }	     
    fprintf(fp, "\n-----------\n\n");
    return;
}

int RingForge::_bitWidth (ActId *id)
{
  if (!id) {
    return -1;
  }
  InstType *it = _p->CurScope()->FullLookup (id, NULL);
  if (!it) {
    return -2;
  }
  return TypeFactory::totBitWidth (it);
}

bool RingForge::need_nesting (ActId *chan)
{
    bool is_struct = (TypeFactory::isStructure(
        TypeFactory::getChanDataType(_p->CurScope()->localLookup(chan, NULL))));
    bool is_bool = (TypeFactory::isBoolType(
        TypeFactory::getChanDataType(_p->CurScope()->localLookup(chan, NULL))));
    return (is_bool || is_struct);
}
