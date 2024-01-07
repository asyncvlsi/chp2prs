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

#include "reqs.h"
#include "ring_synthesis_struct.h"

// Synchronization modes 
#define SYNC_SEQ 0
#define SYNC_PLL 1


// TODO: Need to generate/extract this at runtime ---------
static const int max_mux_size = 4;
static const int max_or_size = 4;

/*
    Lookup table for mux block delays (8-bit data) in ps, 
    extracted from abc using equivalent expression.
    index: (mux_size, or_size)
*/
float mux_delays[max_mux_size][max_or_size] = {
                        {0     , 0     , 0     , 0}, // 1-input muxes => no mux 
                        {96.43 , 96.43 , 102.36, 106.81}, // 2-input muxes
                        {102.36, 102.36, 106.81, 124.33}, // 3-input muxes
                        {106.81, 106.81, 124.33, 135.41}  // 4-input muxes
};                     // 0-OR    1-OR    2-OR    3-OR 

float lookup_mux_delays (int mux_sz, int or_sz)
{
    if (mux_sz <= max_mux_size && or_sz <= max_or_size && mux_sz>0 && or_sz>=0)
    {
        return mux_delays[mux_sz-1][or_sz];
    }
    return -1;
}
// --------------------------------------------------------


// Prototypes ---------------------------------------------

// Top-level flow functions 
void print_refine_body(FILE *, Process *, act_chp_lang_t *, Hashtable *);
void print_headers_and_imports(Process *, FILE *);
void circuit_forge(Process *, act_chp_lang_t *, FILE *);
void print_headers_and_imports_expr();

// Main synthesis functions
void generate_pipe(act_chp_lang_t *, FILE *, int, Process *);
int generate_one_ring(act_chp_lang_t *, FILE *, int, int, Process *);
int generate_branched_ring(act_chp_lang_t *, FILE *, int, int, Process *, int);

// Data collection / query functions
int is_elementary_action (act_chp_lang_t *);
int chp_has_branches (act_chp_lang_t *, int);
int length_of_guard_set (act_chp_lang_t *);
int expr_is_pure_variable(Expr *, Process *);
int get_expr_width (Expr *, Process *);

// Pipeline block generation functions
int generate_itb(FILE *);
int generate_pipe_element(act_chp_lang_t *, FILE *, Process *, int);
int generate_gp_connect(FILE *);
int generate_parallel_split(int, FILE *);
int generate_parallel_merge(int, FILE *);
int generate_selection_split(int, FILE *);
int generate_selection_merge(int, FILE *);
int generate_init_cond_itb(FILE *, int, int, int, int);
int generate_pipe_element_custom(int, int, int, ActId *, FILE *, Process *);

// Expropt functions
int generate_expr_block(Expr *, int, Process *, FILE *);
int generate_expr_block_for_sel(Expr *, Process *, FILE *, int);
void expr_collect_vars (Expr *, int , Process *);
void instantiate_expr_block (FILE *, int, list_t *, Process *);

// Channel generation functions
int generate_bd_chan(int, FILE *);
int generate_sync_chan(FILE *);

// Pipeline block connection functions
int connect_pipe_elements (FILE *, int, int, int);
int connect_pll_split_outputs_to_pipe (FILE *, int, int, int);
int connect_pipe_to_pll_merge_inputs (FILE *, int, int, int);
int connect_sel_split_outputs_to_pipe (FILE *, int, int, int);
int connect_guards_to_sel_split_input (FILE *, int, int, int);
int connect_pipe_to_sel_merge_inputs (FILE *, int, int, int);

// Prototypes Copy ------
void generate_live_var_info (act_chp_lang_t *, Process *, int);
void generate_live_var_bits (act_chp_lang_t *, Process *, int);
void print_live_var_info (act_chp_lang_t *, Process *, int);
// ----------------------

// --------------------------------------------------------



// Global parameters --------------------------------------

// I/O file names
static const char *circuit_library_name = "true_pipe_c_brs_bd.act";
static const char *expr_file = "expr_math.act";

// Pipeline block name prefixes
static const char *ring_block_prefix = "block_";
static const char *conn_block_prefix = "conn_z_";

// Datapath name prefixes
static const char *capture_block_prefix = "latch_";
static const char *expr_block_prefix = "blk_";
static const char *expr_block_instance_prefix = "inst_";
static const char *expr_block_input_prefix = "in_";

// Channel name prefixes
static const char *sync_chan_name_prefix = "sync_";
static const char *parallel_chan_name_prefix = "sync_";
static const char *init_cond_chan_prefix = "C_init_";


// Delay of an INVX1 inverter, in ps (rounded down)
// Extracted from abc
static unsigned int invx1_delay_ps = 21;

// Capture delay of a latch in multiples of 2*(INVX1 delay) - TODO (5 works for now)
static unsigned int capture_delay = 5;

//  Matching with Balsa for testing (for now)
// static unsigned int capture_delay = 2;

// Pulse-width for the pulse-generator for the latch as fn. (2n+1)*d of invx1_delay_ps - TODO
static unsigned int pulse_width = 6;
// --------------------------------------------------------


// Iterators for instance name generation -----------------
static unsigned int block_count = 0;
static unsigned int itb_wrapper_count = 0;
static unsigned int bd_chan_count = 0;
static unsigned int sync_chan_count = 0;
static unsigned int expr_id = 0;
static unsigned int expr_block_id = 0;
static unsigned int mux_block_id = 0;
static unsigned int branch_id = 0;
// --------------------------------------------------------

// verbosity for pipe block connection (do not change)
static unsigned int connection_style = 0;

/*
    Bunch of functions for generating unique names for instances. 
    Needs to be cleaned up.
*/
void inc_block_count()
{
    block_count++;
}

void inc_itb_wrapper_count()
{
    itb_wrapper_count++;
}

int get_block_count()
{
    return block_count;
}

int get_itb_wrapper_count()
{
    return itb_wrapper_count;
}

void inc_sync_chan_count()
{
    sync_chan_count++;
}

int gen_bd_chan_id()
{
    bd_chan_count++;
    return bd_chan_count;
}

int gen_expr_id()
{
    expr_id++;
    return expr_id;
}

int gen_expr_block_id()
{
    expr_block_id++;
    return expr_block_id;
}

int gen_mux_block_id()
{
    mux_block_id++;
    return mux_block_id;
}

/*
    Global structures that contain information about
    the variables in the program.
*/
Hashtable *var_infos;
Hashtable *var_infos_copy;
Hashtable *var_infos_read_ids;
iHashtable *_inexprmap;
iHashtable *_inwidthmap;

/*
    Create a deep copy of a given var_info object
*/
var_info *deepcopy_var_info (var_info *v)
{
  var_info *v_copy;
  NEW (v_copy, var_info);
  v_copy->name = v->name;
  v_copy->fcurexpr = v->fcurexpr;
  v_copy->fischan = v->fischan;
  v_copy->fisinport = v->fisinport;
  v_copy->fisbool = v->fisbool;
  v_copy->width = v->width;
  v_copy->block_in = v->block_in;
  v_copy->block_out = v->block_out;
  v_copy->nread = v->nread;
  v_copy->nwrite = v->nwrite;
  v_copy->latest_for_read = v->latest_for_read;
  v_copy->iread = v->iread;
  v_copy->iwrite = v->iwrite;
  v_copy->latest_latch_branches = list_dup(v->latest_latch_branches);
  return v_copy;
}

/*
    Create a deep copy of only the latest_for_read
    (this field is used to identify which latch is 
    the most 'recent' one so it can be read from 
    to get the correct value for the variable)
    a given var_info object.
*/
var_info *deepcopy_var_info_read_id (var_info *v)
{
  var_info *v_copy;
  NEW (v_copy, var_info);
  v_copy->name = v->name;
  v_copy->latest_for_read = v->latest_for_read;
  return v_copy;
}

/*
    Create a deep copy of the var_info hashtable
*/
Hashtable *deepcopy_var_info_hashtable (Hashtable *var_infos)
{
  Hashtable *var_infos_copy = hash_new(4);
  hash_bucket_t *b, *b_copy;
  var_info *v_copy;
  int i;

  for (i = 0; i < var_infos->size; i++)
    for (b = var_infos->head[i]; b; b = b->next) 
    {
      NEW (v_copy, var_info);
      v_copy = deepcopy_var_info((var_info *)b->v);
      b_copy = hash_add (var_infos_copy, v_copy->name);
      b_copy->v = v_copy;
    }	     

  return var_infos_copy;
}

/*
    Create a deep copy of the var_info hashtable,
    but only the latest_for_read field
*/
Hashtable *deepcopy_var_info_hashtable_read_ids (Hashtable *v_infos)
{
  Hashtable *var_infos_read_ids = hash_new(4);
  hash_bucket_t *b, *b_copy;
  var_info *v_copy;
  int i;

  for (i = 0; i < v_infos->size; i++)
    for (b = v_infos->head[i]; b; b = b->next) 
    {
      NEW (v_copy, var_info);
      v_copy = deepcopy_var_info_read_id((var_info *)b->v);
      b_copy = hash_add (var_infos_read_ids, v_copy->name);
      b_copy->v = v_copy;
    }	     

  return var_infos_read_ids;
}

/*
    Save the current state of the latest latch IDs 
    for all the variables
*/
void save_read_ids()
{
    var_infos_read_ids = deepcopy_var_info_hashtable_read_ids (var_infos);
}

/*
    Restore the saved state of the latest latch IDs 
    for all the variables
*/
void restore_read_ids()
{
    hash_bucket_t *b, *b_saved;
    var_info *vi, *vi_saved;
    int i;

    for (i = 0; i < var_infos->size; i++)
        for (b = var_infos->head[i]; b; b = b->next) 
        {
            b_saved = hash_lookup(var_infos_read_ids, b->key);
            vi_saved = (var_info *)b_saved->v;
            vi = (var_info *)b->v;
            vi->latest_for_read = vi_saved->latest_for_read;
        }	     
}

/*
    Save the current state of the var_info objects
    for all the variables
*/
void save_var_infos()
{
    NEW (var_infos_copy, Hashtable);
    var_infos_copy = deepcopy_var_info_hashtable (var_infos);
}

/*
    Restore the saved state of the var_info objects
    for all the variables
*/
void restore_var_infos()
{
    NEW (var_infos, Hashtable);
    var_infos = deepcopy_var_info_hashtable (var_infos_copy);
    FREE (var_infos_copy);
}

/*
    Note: Not used as of now due to the combinational loop issue
    For linear programs only. 
    For every variable that is assigned in the initial condition 
    list, go backward through the program and find the last 
    assignment of that variable so that the pipeline element 
    for that action can connect to the initial value latch 
    instead of generating one for itself. 
*/
void tag_lcds(act_chp_lang_t *c, Process *p, list_t *var_list)
{
    act_chp_lang_t *stmt;
    ActId *var;
    hash_bucket_t *b;
    var_info *vi;
    Assert (c->type == ACT_CHP_LOOP, "huh");
    act_chp_lang_t *stmts = c->u.gc->s;
    list_t *reverse_list;
    listitem_t *l_v, *l_s;
    list_t *tag;
    tag = list_new();
    list_iappend(tag, 0);
    // Assert (stmts->type == ACT_CHP_SEMI, "huh1");
    // TODO: dirty fix, need to clean up
    switch (stmts->type)
    {   
    case ACT_CHP_SEMI:
        reverse_list = list_dup(stmts->u.semi_comma.cmd);
        list_reverse(reverse_list);

        for (l_v = list_first (var_list); l_v; l_v = list_next (l_v)) 
        {
            const char *var_init_name = (const char *)list_value(l_v);
            for (l_s = list_first (reverse_list); l_s; l_s = list_next (l_s)) 
            {
                stmt = (act_chp_lang_t *)list_value (l_s);
                if (stmt->type == ACT_CHP_ASSIGN)
                {
                    var = stmt->u.assign.id;
                    char tname[1024];
                    get_true_name(tname, var, p->CurScope());
                    b = hash_lookup(var_infos, tname);
                    // b = hash_lookup(var_infos, var->rootVx(p->CurScope())->getName());
                    vi = (var_info *)b->v;
                    if (strcmp(var_init_name, vi->name)==0)
                    {
                        stmt->space = list_dup(tag);
                        break;
                    }
                }
                else if (stmt->type == ACT_CHP_RECV)
                {
                    var = stmt->u.comm.var;
                    char tname[1024];
                    get_true_name(tname, var, p->CurScope());
                    b = hash_lookup(var_infos, tname);
                    // b = hash_lookup(var_infos, var->rootVx(p->CurScope())->getName());
                    vi = (var_info *)b->v;
                    if (strcmp(var_init_name, vi->name)==0)
                    {
                        stmt->space = list_dup(tag);
                        break;
                    }
                }
                // need to add option to generate_pipe_element to not generate latch and connect correctly
            }
        }
        break;
    case ACT_CHP_RECV:
    case ACT_CHP_ASSIGN:
        for (l_v = list_first (var_list); l_v; l_v = list_next (l_v)) 
        {
            const char *var_init_name = (const char *)list_value(l_v);
            if (stmts->type == ACT_CHP_ASSIGN)
            {
                var = stmts->u.assign.id;
                char tname[1024];
                get_true_name(tname, var, p->CurScope());
                b = hash_lookup(var_infos, tname);
                // b = hash_lookup(var_infos, var->rootVx(p->CurScope())->getName());
                vi = (var_info *)b->v;
            }
            else if (stmts->type == ACT_CHP_RECV)
            {
                var = stmts->u.comm.var;
                char tname[1024];
                get_true_name(tname, var, p->CurScope());
                b = hash_lookup(var_infos, tname);
                // b = hash_lookup(var_infos, var->rootVx(p->CurScope())->getName());
                vi = (var_info *)b->v;
            }
            if (strcmp(var_init_name, vi->name)==0)
            {
                stmts->space = list_dup(tag);
            }
        }
        break;
    case ACT_CHP_SEND:
        break;
    default:
        fatal_error ("wut (tag_lcds)");
        break;
    }
}

/*
    Print out some comments and imports at the top of 
    the expression blocks file.
*/
void print_headers_and_imports_expr()
{
    FILE *fp = fopen (expr_file, "w");
    fprintf (fp, "// Combinational logic blocks\n");
    // fprintf (fp, "import std::cells -> syn;\n\n");
    fclose (fp);
}

/*
    Print out some comments and imports at the top of 
    the primary output file. Also prints out the 
    implemented process header.
*/
void print_headers_and_imports(FILE *fp, Process *p)
{
    const char *p_name = p->getName();

    fprintf(fp, "import globals;\n");
    fprintf(fp, "import std;\n");
    fprintf(fp, "open std::channel;\n");
    fprintf(fp, "import \"%s\"; \n\n", circuit_library_name);
    fprintf(fp, "import \"%s\";\n", expr_file);

    fprintf(fp, "defproc ");
    fprintf(fp, "ring_");
    ActNamespace::Act()->mfprintfproc(fp, p);

    fprintf(fp, " <: ");
    ActNamespace::Act()->mfprintfproc(fp, p);
    fprintf(fp, "()");

}

/*
    Print out overrides for channels and variables.
    chan -> bd
    int -> bd_int
    bd_int actually doesn't do anything as of now.
*/
bool print_overrides(FILE *fp, Process * p)
{
    bool has_overrides = 0;
    list_t *special_vx;

    special_vx = ActNamespace::Act()->getDecomp (p);

    int bw = 0;

    #define OVERRIDE_OPEN				\
    do {						\
    if (!has_overrides) {			\
        fprintf(fp, "\n+{\n");			\
        has_overrides = true;			\
    }						\
    } while (0)

    /* iterate through Scope Hashtable to find all chp variables */
    ActInstiter iter(p->CurScope());
    for (iter = iter.begin(); iter != iter.end(); iter++) {
    ValueIdx *vx = *iter;
    if (special_vx) {
        /* these are fresh instances introduced during decomposition;
        we need to declare them, not refine them!
        */
        int sp = 0;
        for (listitem_t *si = list_first (special_vx); si; si = list_next (si)) {
    for (listitem_t *li = list_first ((list_t *) list_value (si)); li;
            li = list_next (li)) {
        if (vx == (ValueIdx *) list_value (li)) {
        sp = 1;
        break;
        }
    }
    if (sp) {
        break;
    }
        }
        if (sp) {
    continue;
        }
    }

    /* chan variable found */
    if (TypeFactory::isChanType (vx->t)) {
        bw = TypeFactory::bitWidth(vx->t);
        if (TypeFactory::isBoolType (TypeFactory::getChanDataType (vx->t))) {
            OVERRIDE_OPEN;
            fprintf(fp, "  bd<1> %s;\n", vx->getName());
        }
        else if (bw>0)
        {
            OVERRIDE_OPEN;
            fprintf(fp, "  bd<%d> %s;\n", bw, vx->getName());
        }
        else {
            // warning ("zero width channel: %s", vx->getName());
            OVERRIDE_OPEN;
            fprintf(fp, "  a1of1 %s;\n", vx->getName());
        }
    }
    else if (TypeFactory::isIntType (vx->t)) {
        /* chp-optimize creates sel0, sel1,... & loop0, loop1, ... which do not have dualrail overrides */
        bw = TypeFactory::bitWidth(vx->t);
        OVERRIDE_OPEN;
        // if (vx->t->arrayInfo())
        //     fprintf(fp, "  bd_int<%d> %s[%d];\n", bw, vx->getName(), vx->t->arrayInfo()->size());
        // else
        fprintf(fp, "  bd_int<%d> %s;\n", bw, vx->getName());
    }
    else if (TypeFactory::isBoolType (vx->t)) {
        // OVERRIDE_OPEN;
        // fprintf (fp, " syn::sdtboolvar %s;\n", vx->getName());
    }
    else if (TypeFactory::isProcessType (vx->t)) {
        OVERRIDE_OPEN;
        // fprintf (fp, " sdt_");
        Process *proc = dynamic_cast <Process *> (vx->t->BaseType());
        Assert (proc, "Why am I here?");
        ActNamespace::Act()->mfprintfproc (fp, proc);
        fprintf (fp, " %s_ring;\n", vx->getName());
    }
    else if (TypeFactory::isStructure (vx->t)) {
        OVERRIDE_OPEN;
        fatal_error ("Structures not supported yet..");
        fprintf (fp, " sdt_");
        Data *d = dynamic_cast <Data *> (vx->t->BaseType());
        Assert (d, "Why am I here?");
        ActNamespace::Act()->mfprintfproc (fp, d);
        fprintf (fp, " %s;\n", vx->getName());
    }
    }
    /* end param declaration */
    if (has_overrides) {
    fprintf(fp, "}\n");
    }
    else {
    fprintf(fp, "\n");
    }

    return has_overrides;
}

/*
    Print out the refine body for the implemented process. 
*/
void print_refine_body(FILE *fp, Process *p, act_chp_lang_t *c, Hashtable *hvi)
{
    Assert (c, "No CHP");
    var_infos = hvi;
    circuit_forge(p,c,fp);
}

/*
    Main synthesis function. Performs some checks to see 
    whether optimized synthesis can be done. If the program
    is 'small' and of a particular form, it can be synthesized
    using a single pipeline element. If not, and there are no
    branches, it can be synthesized as one linear ring. If neither
    of these, then fall back to the general branched ring synthesis.
*/
void circuit_forge(Process *p, act_chp_lang_t *c, FILE *fp)
{
    int has_branches = chp_has_branches(c, 1);
    int is_pipeable = check_if_pipeable(c, p, 1);

    fprintf (fp, "// Control Path -----------------\n");
    
    // if (is_pipeable == 1)
    // TODO: verify one pipe synthesis is still working
    if (false)
    {
        fprintf (fp, "// One Pipe ---------------------\n");
        generate_pipe (c,fp,1,p);
        // generate_one_ring (c,fp,1,0,p);
    }
    else if (has_branches == 0)
    {
        fprintf (fp, "// One Ring ---------------------\n");
        generate_one_ring (c,fp,1,0,p);
    }
    else
    {
        generate_live_var_info (c, p, 1);
        // LiveVarAnalysis *lva = new LiveVarAnalysis (fp, p, c);
        // lva->generate_live_var_info();
        fprintf (fp, "// Branched Ring ----------------\n");
        generate_branched_ring (c,fp,1,0,p,0);
    }
    fprintf (fp, "\n// End Control Path -------------\n");
}

/*
    Generate a data capture element for a given variable.
    If provided with an initial value, generate a data capture
    element that initializes to that value on reset.
*/
int generate_single_latch (FILE *fp, var_info *v, int init_val=-1)
{
    list_iappend_head (v->latest_latch_branches, branch_id);
    std::string s = capture_block_prefix;
    if (v->iwrite < v->nwrite)
    {
        s.append(v->name);
        s.append(std::to_string(v->iwrite));
        if (init_val == -1)
        {
            fprintf(fp, "capture<%d,%d,%d> %s%s_%d;\n", capture_delay, pulse_width, v->width, 
                                                        capture_block_prefix, v->name,v->iwrite);
        }
        else
        {
            fprintf(fp, "capture_init<%d,%d,%d,%d> %s%s_%d;\n", capture_delay, pulse_width, v->width,
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
    Generate merging muxes to be placed when exiting selections,
    so that variables that are assigned in one/many branches inside
    a selection can be addressed correctly when exiting the selection, 
    based on which branch was taken in this iteration of the loop.
*/
int compute_merge_mux_info (FILE *fp, Process *p, list_t *live_vars, 
                                int n_branches, int merge_block_id)
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
        fprintf (fp, "\n// %s\n", vi->name);
        lj = list_first (latch_branches);
        list_t *branch_map = list_new();

        for ( int i=0 ; i < (iwrite-iwrite_pre) ; i++ )
        {
            latest_branch_id = int(list_ivalue(lj));
            // all branch check
            if (latest_branch_id != latest_branch_id_prev) 
            {
                // list_iappend_head (branch_map, (iwrite_pre+1)+i); // is this right? turns out, no lol
                // list_iappend_head (branch_map, latest_branch_id); // this isnt right either lmao
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
            fprintf(fp, "// assigned in all branches\n"); 
            need_mux = 1; need_or = 0; mux_size = branch_ctr; or_size = 0;
        }
        else if (branch_ctr == 0)
        { 
            fprintf(fp, "// not assigned in any branch\n"); 
            need_mux = 0; need_or = 0; mux_size = 0; or_size = 0;
        }
        else
        { 
            fprintf(fp, "// not assigned in all branches\n"); 
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
                    fprintf(fp, "// unassigned in branch %d\n", i);
                }
            }
        }
        Assert ((list_length(unassigned_branches) == or_size), "what the..");

        // generate the mux if needed (looks like latch to downstream) and connect latch outputs correctly
        int mux_id, i;
        int j = 0;
        if (need_mux) 
        {   
            mux_id = gen_mux_block_id();
            fprintf (fp, "merge_mux_ohc_opt<%d,%d> %s%s_%d;\n", mux_size, vi->width, 
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
                    // {
                    fprintf (fp, "std::gates::ortree<%d, false> or_%s_%d;\n", or_size, vi->name, mux_id);
                    // }
                    // connect OR-gate inputs (merge_block inputs)
                    for (listitem_t *lk = list_first(unassigned_branches) ; lk ; lk = list_next(lk))
                    {
                        fprintf (fp, "or_%s_%d.in[%d] = %s%d.ci[%d].r;\n", vi->name, mux_id, j, 
                                            ring_block_prefix, merge_block_id, list_ivalue(lk));
                        j++;
                    }
                    // connect pre-split data to mux last data input
                    fprintf (fp, "%s%s_%d.din[%d][0..%d] = %s%s_%d.dout;\n", capture_block_prefix, vi->name, 
                                                        iwrite+1, i, (vi->width)-1,
                                            capture_block_prefix, vi->name, iwrite_pre);
                    // connect OR-gate output to mux input
                    fprintf (fp, "or_%s_%d.out = %s%s_%d.c[%d];\n", vi->name, mux_id,
                                            capture_block_prefix, vi->name, iwrite+1, i);
                    break;
                }

                // connect mux input control and data
                fprintf (fp, "%s%s_%d.c[%d] = %s%d.ci[%d].r;\n", capture_block_prefix, vi->name, 
                                                    iwrite+1, i, ring_block_prefix, 
                                                        merge_block_id, list_ivalue(lj));
                fprintf (fp, "%s%s_%d.din[%d][0..%d] = %s%s_%d.dout;\n",capture_block_prefix, vi->name, 
                                                    iwrite+1, i, (vi->width)-1,
                                        capture_block_prefix, vi->name, list_ivalue(list_next(lj)));

                lj = list_next(list_next(lj));
            }
        }
        // {
        // fprintf (fp, "branch_id %d :: %d latch_id \n", list_ivalue(lj),  list_ivalue(list_next(lj)));
        // }
        branch_ctr = 0;
    }

    float max_delay = lookup_mux_delays (max_mux_size, max_or_size);
    // fprintf (fp, "\nmax mux delay: %f", max_delay);
    // fprintf (fp, "\nmax mux size: %d", max_mux_size);
    // fprintf (fp, "\nmax or size: %d", max_or_size);
    return int(max_delay/(2*invx1_delay_ps)) + 1;
}

/*
    Synthesis of linear programs. Generates a sequence of pipeline
    elements, according to the actions in the CHP program, and ties 
    them up into a ring using an initial token buffer. If initial 
    conditions exist, processes them according to the optimized
    handling method, where the last assignment actions connect to 
    latches that are initialized to the initial value.
*/
int generate_one_ring(act_chp_lang_t *c, FILE *fp, int root, int prev_block_id, Process *p)
{
    int block_id;
    int first_block_id;
    int chan_id;
    int init_var_width;
    int init_chan, lcd_chan;
    int init_latch = -1;
    list_t *lcd_chan_list, *lcd_var_list;
    list_t *tag_list = NULL;
    var_info *vi;
    ActId *id;
    listitem_t *li, *lj, *lk;
    act_chp_gc_t *gc;
    act_chp_lang_t *stmt, *main_loop, *init_recv_stmt;
    
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

                first_block_id = generate_itb(fp);
                prev_block_id = first_block_id;

                // new ---------
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
                // new ---------

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
                        get_true_name(tname, id, p->CurScope());
                        hash_bucket_t *b = hash_lookup(var_infos, tname);
                        // hash_bucket_t *b = hash_lookup(var_infos, id->rootVx(p->CurScope())->getName());
                        vi = (var_info *)b->v;
                        init_chan = generate_bd_chan (vi->width, fp);
                        lcd_chan = generate_bd_chan (vi->width, fp);
                        list_iappend(lcd_chan_list, lcd_chan);
                        int itb_block_id = generate_init_cond_itb (fp, ival, vi->width, init_chan, lcd_chan);

                        block_id = generate_pipe_element_custom (init_chan, ACT_CHP_RECV, vi->width, id, fp, p);
                        connect_pipe_elements (fp, prev_block_id, block_id, SYNC_SEQ);
                        prev_block_id = block_id;
                    }
                }
                // old ---------

                // main program synthesis
                gc = main_loop->u.gc;
                block_id = generate_one_ring(gc->s, fp, 0, prev_block_id, p);
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
                        get_true_name(tname, id, p->CurScope());
                        hash_bucket_t *b = hash_lookup(var_infos, tname);
                        // hash_bucket_t *b = hash_lookup(var_infos, id->rootVx(p->CurScope())->getName());
                        vi = (var_info *)b->v;
                        lcd_chan = list_ivalue(lk);
                        block_id = generate_pipe_element_custom (lcd_chan, ACT_CHP_SEND, vi->width, id, fp, p);
                        connect_pipe_elements (fp, prev_block_id, block_id, SYNC_SEQ);
                        prev_block_id = block_id;
                        lk = list_next(lk);
                    }
                }
                // old ---------

                connect_pipe_elements(fp, block_id, first_block_id, SYNC_SEQ);
                break;

            }
            // regular synthesis
            else {
                for (li = list_first (c->u.semi_comma.cmd); li; li = list_next (li)) 
                {
                    block_id = generate_one_ring ((act_chp_lang_t *)list_value(li), fp, 0, prev_block_id, p);
                    prev_block_id = block_id;
                }
            }
            break;

        case ACT_CHP_LOOP:
        case ACT_CHP_DOLOOP:
            if (root == 1)
            {   
                first_block_id = generate_itb(fp);
                gc = c->u.gc;
                block_id = generate_one_ring(gc->s, fp, 0, first_block_id, p);
                connect_pipe_elements(fp, block_id, first_block_id, SYNC_SEQ);
                break;
            }
            else
            {
                fatal_error ("should've excised internal loops.. (generate_one_ring)");
            }
            break;
            
        case ACT_CHP_SELECT:
            fprintf(fp, "\n// WARNING: single guard selection in program - hope you know what you're doing :)\n\n");
            gc = c->u.gc;
            block_id = generate_one_ring (gc->s, fp, 0, prev_block_id, p);
            break;

        case ACT_CHP_SELECT_NONDET:
            fatal_error ("Can't handle NDS in generate_one_ring");
            
        case ACT_CHP_SKIP:
        case ACT_CHP_ASSIGN:
        case ACT_CHP_ASSIGNSELF:
        case ACT_CHP_SEND:
        case ACT_CHP_RECV:
            // do stuff
            if (c->space) 
            {
                tag_list = (list_t *)c->space;
                init_latch = list_ivalue(list_first(tag_list));
                Assert (init_latch>-1, "wut");
            }// if (tag_list) 

            block_id = generate_pipe_element(c,fp, p, init_latch);
            connect_pipe_elements(fp, prev_block_id, block_id, SYNC_SEQ);

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
int generate_branched_ring(act_chp_lang_t *c, FILE *fp, int root, int prev_block_id, Process *p, int connect_prev)
{
    int block_id, expr_block_id, first_block_id;
    int pll_split_block_id, pll_merge_block_id;
    int sel_split_block_id, sel_merge_block_id;
    int delay_merge_block_id;
    int chan_id;
    int comma_len, gc_len;
    int pll_end, pll_port;
    int delay_n_sel, max_delay_n_sel, delay_n_merge;
    list_t *live_vars;
    list_t *gp_connect_ids;
    listitem_t *li, *lj, *lk;
    act_chp_gc_t *gc;
    act_chp_lang_t *stmt, *stmt1, *main_loop;
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
            { fatal_error ("Only semi-colon list of initializations... (generate_branched_ring)"); }
            else {
            comma_len = list_length(c->u.semi_comma.cmd);

            fprintf (fp, "// %d-way parallel split for actions: ",comma_len);
            chp_print(fp, c);
            fprintf (fp, "\n");

            fprintf (fp, "// %d-way parallel merge \n",comma_len);
            pll_split_block_id = generate_parallel_split(comma_len, fp);
            pll_merge_block_id = generate_parallel_merge(comma_len, fp);
            // connect_pipe_to_pll_split_input(fp, pll_split_block_id, prev_block_id);
            connect_pipe_elements(fp, prev_block_id, pll_split_block_id, SYNC_SEQ);

            pll_port = 0;
            for (li = list_first (c->u.semi_comma.cmd); li; li = list_next (li)) 
            {
                block_id = generate_branched_ring ((act_chp_lang_t *)list_value(li), fp, 0, pll_split_block_id, p, 0);
                connect_pll_split_outputs_to_pipe (fp, pll_split_block_id, block_id, pll_port);
                connect_pipe_to_pll_merge_inputs (fp, pll_merge_block_id, block_id, pll_port);
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

                first_block_id = generate_itb(fp);
                prev_block_id = first_block_id;

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
                        get_true_name(tname, id, p->CurScope());
                        hash_bucket_t *b = hash_lookup(var_infos, tname);
                        // hash_bucket_t *b = hash_lookup(var_infos, id->rootVx(p->CurScope())->getName());
                        vi = (var_info *)b->v;
                        init_chan = generate_bd_chan (vi->width, fp);
                        lcd_chan = generate_bd_chan (vi->width, fp);
                        list_iappend(lcd_chan_list, lcd_chan);
                        int itb_block_id = generate_init_cond_itb (fp, ival, vi->width, init_chan, lcd_chan);

                        block_id = generate_pipe_element_custom (init_chan, ACT_CHP_RECV, vi->width, id, fp, p);
                        connect_pipe_elements (fp, prev_block_id, block_id, SYNC_SEQ);
                        prev_block_id = block_id;
                    }
                }

                // main program synthesis
                gc = main_loop->u.gc;
                block_id = generate_branched_ring(gc->s, fp, 0, prev_block_id, p, 0);
                prev_block_id = block_id;

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
                        get_true_name(tname, id, p->CurScope());
                        hash_bucket_t *b = hash_lookup(var_infos, tname);
                        // hash_bucket_t *b = hash_lookup(var_infos, id->rootVx(p->CurScope())->getName());
                        vi = (var_info *)b->v;
                        lcd_chan = list_ivalue(lk);
                        block_id = generate_pipe_element_custom (lcd_chan, ACT_CHP_SEND, vi->width, id, fp, p);
                        connect_pipe_elements (fp, prev_block_id, block_id, SYNC_SEQ);
                        prev_block_id = block_id;
                        lk = list_next(lk);
                    }
                }

                connect_pipe_elements(fp, block_id, first_block_id, SYNC_SEQ);
                break;
            }
            // regular synthesis
            else {
                for (li = list_first (c->u.semi_comma.cmd); li; li = list_next (li)) 
                {
                    block_id = generate_branched_ring ((act_chp_lang_t *)list_value(li), fp, 0, prev_block_id, p, 0);
                    if (is_elementary_action((act_chp_lang_t *)list_value(li)) == 1)
                    {
                        connect_pipe_elements(fp, prev_block_id, block_id, SYNC_SEQ);
                    }
                    prev_block_id = block_id;
                }
            }
            break;

        case ACT_CHP_LOOP:
        case ACT_CHP_DOLOOP:
            if (root == 1)
            {
                first_block_id = generate_itb(fp);
                gc = c->u.gc;
                block_id = generate_branched_ring(gc->s, fp, 0, first_block_id, p, 0);
                connect_pipe_elements(fp, block_id, first_block_id, SYNC_SEQ);
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

            fprintf (fp, "\n// %d-way selection split for : ", gc_len);
            chp_print(fp, c);
            fprintf (fp, "\n");
            fprintf (fp, "// %d-way selection merge \n",gc_len);
            sel_split_block_id = generate_selection_split(gc_len, fp);
            sel_merge_block_id = generate_selection_merge(gc_len, fp);

            live_vars = (list_t *)c->space;
            // fprintf (fp, "\n\nlive vars at merge:");
            // print_live_vars_temp(live_vars);
            save_var_infos();
            // if iwrite is unchanged, nothing to do (done)
            // if iwrite is changed, need which latch was in which branch info.. (done)
            // possible that several branches dont assign => all those inputs need to be OR'd (done)
            // N_mux = No. of assigning branches + (some branch doesn't assign)*(1) (done)
            // N_OR  = No. of non assigning branches (done)
            // Need to increase v->nwrite each time mux is generated.. this might not be great (it's fine)
            gp_connect_ids = list_new();
            for (int i = 0; gc; gc = gc->next)
            {
                // branch_id++;
                if (gc->g)
                {   
                    expr_block_id = gen_expr_block_id();
                    delay_n_sel = generate_expr_block_for_sel (gc->g, p, fp, expr_block_id);
                    if (max_delay_n_sel < delay_n_sel) max_delay_n_sel = delay_n_sel;
                }
                else
                {
                    // compute the else guard .. 
                    fatal_error ("should've been fixed in else generation");
                    // if (max_delay_n_sel < delay_n_sel) max_delay_n_sel = delay_n_sel;
                }

                connect_guards_to_sel_split_input (fp, sel_split_block_id, expr_block_id, i);
                block_id = generate_gp_connect (fp);
                connect_sel_split_outputs_to_pipe (fp, sel_split_block_id, block_id, i);
                list_iappend(gp_connect_ids, block_id);
                i++;
            }
            lj = list_first(gp_connect_ids);
            gc = c->u.gc;
            for (int i = 0; gc; gc = gc->next)
            {   
                branch_id++;
                // save read ids here..
                save_read_ids();
                // restore_var_infos();
                block_id = generate_branched_ring (gc->s, fp, 0, list_ivalue(lj), p, 1);
                // save_var_infos();
                connect_pipe_to_sel_merge_inputs (fp, sel_merge_block_id, block_id, i);
                // restore read ids here..
                restore_read_ids();
    
                i++; lj = list_next(lj);
            }
            // save_var_infos();

            branch_id = branch_id - gc_len;

            // muxing variables live-out of merge so downstream can access correctly
            delay_n_merge = compute_merge_mux_info(fp, p, live_vars, gc_len, sel_merge_block_id);

            // generate delay line for max guard evaluator delay (split)
            fprintf(fp,"\n// Delaying pre-split-block sync. by max. delay of all guard evaluators\n");
            fprintf(fp,"delay_line_chan<%d> delay_select_%d;\n",max_delay_n_sel,sel_split_block_id);
            // connect prev. block p1 to delay_line then connect to select block from the output
            fprintf(fp,"delay_select_%d.m1 = %s%d.p1;\n",sel_split_block_id,ring_block_prefix,prev_block_id);
            fprintf(fp,"delay_select_%d.p1 = %s%d.m1;\n",sel_split_block_id,ring_block_prefix,sel_split_block_id);

            // TODO : generate delay-line based on mux delay (done)
            // and put it on the merge-block output request (p1.r)
            // just use another delay_line_chan
            // (here:a1) 
            // generate delay line for max mux delay (merge)

            if (delay_n_merge > 0)
            {
                inc_block_count();
                delay_merge_block_id = get_block_count();
                fprintf(fp,"\n// Delaying post-merge-block sync. by max. delay of all merge muxes\n");
                fprintf(fp,"delay_line_chan<%d> %s%d;\n",delay_n_merge,ring_block_prefix, delay_merge_block_id);
                fprintf(fp,"%s%d.m1 = %s%d.p1;\n",ring_block_prefix,delay_merge_block_id,
                                                    ring_block_prefix,sel_merge_block_id);
                // tail is the delay_merge block
                block_id = delay_merge_block_id;
            }
            else 
                block_id = sel_merge_block_id;

            break;

        case ACT_CHP_SELECT_NONDET:
            fatal_error ("Can't handle NDS in generate_branched_ring");
            
        case ACT_CHP_SKIP:
        case ACT_CHP_ASSIGN:
        case ACT_CHP_ASSIGNSELF:
        case ACT_CHP_SEND:
        case ACT_CHP_RECV:
            // do stuff
            block_id = generate_pipe_element(c,fp,p);
            if (connect_prev == 1)
            {
                connect_pipe_elements(fp, prev_block_id, block_id, SYNC_SEQ);
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
    Generate a pass-through connection block. This is just for 
    convenience of connection due to some port naming conventions. 
    The actual block is just wires. 
*/
int generate_gp_connect(FILE *fp)
{
    int block_id;
    inc_block_count();
    block_id = get_block_count();
    fprintf(fp,"gp_connect %s%d;\n", ring_block_prefix, block_count);
    
    return block_id;
}

/*
    Generate a parallelizing split block for comma-separated
    statements. 
*/
int generate_parallel_split(int n, FILE *fp)
{
    int block_id;
    inc_block_count();
    block_id = get_block_count();
    fprintf(fp,"parallel_split<%d> %s%d;\n", n, ring_block_prefix, block_count);
    
    return block_id;
}

/*
    Generate a parallelizing merge block for comma-separated
    statements. 
*/
int generate_parallel_merge(int n, FILE *fp)
{
    int block_id;
    inc_block_count();
    block_id = get_block_count();
    fprintf(fp,"parallel_merge<%d> %s%d;\n", n, ring_block_prefix, block_count);
    
    return block_id;
}

/*
    Generate a selection split block for branches
*/
int generate_selection_split(int n, FILE *fp)
{
    int block_id;
    inc_block_count();
    block_id = get_block_count();
    fprintf(fp,"selection_split<%d> %s%d;\n", n, ring_block_prefix, block_count);
    
    return block_id;
}

/*
    Generate a selection merge block for branches
*/
int generate_selection_merge(int n, FILE *fp)
{
    int block_id;
    inc_block_count();
    block_id = get_block_count();
    fprintf(fp,"selection_merge<%d> %s%d;\n", n, ring_block_prefix, block_count);
    
    return block_id;
}

/*
    Compute the number of branches in a selection.
*/
int length_of_guard_set (act_chp_lang_t *c)
{
  act_chp_gc_t *gc_itr;
  int counter = 0;
  Assert (((c->type == ACT_CHP_SELECT)||(c->type == ACT_CHP_LOOP)), 
            "Called length_of_guard_set on a non-selection/loop");

  for (gc_itr = c->u.gc; gc_itr; gc_itr = gc_itr->next)
  {
    counter++;
  }
  return counter;
}

/*
    Check if a given CHP object is an elementary
    action. An action is either a send, a receive
    or an assignment.
*/
int is_elementary_action(act_chp_lang_t *c)
{
    if (!c) return 1;

    switch (c->type) {
    case ACT_CHP_COMMALOOP:
    case ACT_CHP_SEMILOOP:
        fatal_error ("Replication loops should've been removed..");
    case ACT_CHP_COMMA:
    case ACT_CHP_SEMI:
    case ACT_CHP_LOOP:
    case ACT_CHP_DOLOOP:
    case ACT_CHP_SELECT:
        return 0;
        break;

    case ACT_CHP_SELECT_NONDET:
        fatal_error ("Can't handle NDS");
        return 0;
        
    case ACT_CHP_SKIP:
    case ACT_CHP_ASSIGN:
    case ACT_CHP_ASSIGNSELF:
    case ACT_CHP_SEND:
    case ACT_CHP_RECV:
        return 1;
        break;

    case ACT_CHP_FUNC:
    case ACT_CHP_HOLE: /* to support verification */
    case ACT_CHP_MACRO:
    case ACT_HSE_FRAGMENTS:
        return 0;
        break;

    default:
        fatal_error ("Unknown type");
        return 0;
        break;
    }

}

/*
    Check if a given CHP program is linear. Linear 
    is defined as having no selections or parallelizations.
    This should probably be modified so that programs 
    with commas also count as linear programs.
*/
int chp_has_branches (act_chp_lang_t *c, int root)
{
    int has_branches = 0;
    listitem_t *li, *li_prev;
    act_chp_gc_t *gc;
    act_chp_lang_t *stmt, *stmt_prev;

    if (!c) return 0;

    switch (c->type) {
    case ACT_CHP_COMMALOOP:
    case ACT_CHP_SEMILOOP:
        fatal_error ("Replication loops should've been removed..");
        break;
        
    case ACT_CHP_COMMA:
        has_branches = 1;
        break;

    case ACT_CHP_SEMI:
        if (root == 1)
        {   
            for (li = list_first (c->u.semi_comma.cmd); li; li = list_next (li)) 
            {   
                stmt = (act_chp_lang_t *)list_value(li);
                if (stmt->type == ACT_CHP_LOOP)
                    has_branches = chp_has_branches (stmt, 1);
            }
        }
        else{
            for (li = list_first (c->u.semi_comma.cmd); li; li = list_next (li)) 
            {
                stmt = (act_chp_lang_t *)(list_value(li));
                has_branches = chp_has_branches (stmt, 0);
                if (has_branches == 1) break;
            }
        }
        break;

    case ACT_CHP_LOOP:
    case ACT_CHP_DOLOOP:
        if (root == 1)
        {
            gc = c->u.gc;
            has_branches = chp_has_branches (gc->s, 0);
            break;
        }
        else
        {
            fatal_error ("should've excised internal loops... (chp_has_branches)");
        }
        break;
        
    case ACT_CHP_SELECT:
        if( length_of_guard_set (c) > 1)
        {
            has_branches = 1;
        }
        else
        {
            gc = c->u.gc;
            has_branches = chp_has_branches (gc->s, 0);
        }
        break;

    case ACT_CHP_SELECT_NONDET:
        fatal_error ("Can't handle NDS");
        
    case ACT_CHP_SKIP:
    case ACT_CHP_ASSIGN:
    case ACT_CHP_ASSIGNSELF:
    case ACT_CHP_SEND:
    case ACT_CHP_RECV:
        break;

    case ACT_CHP_FUNC:
    case ACT_CHP_HOLE: /* to support verification */
    case ACT_CHP_MACRO:
    case ACT_HSE_FRAGMENTS:
        break;

    default:
        fatal_error ("Unknown type");
        break;
    }

    return has_branches;
}

/*
    Generate a bundled data channel of a given width.
*/
int generate_bd_chan(int width, FILE *fp)
{
    int id = gen_bd_chan_id();
    fprintf (fp, "bd<%d> %s%d;\n",width,init_cond_chan_prefix,id);
    return id;
}

/*
    Generate a dataless synchronization channel. 
*/
int generate_sync_chan(FILE *fp)
{
    inc_sync_chan_count();
    int count = sync_chan_count;
    fprintf(fp,"a1of1 %s%d;\n",sync_chan_name_prefix,count);
    return count;
}

/*
    Generate an initial condition handling ITB to
    wrap around the main ring.
*/
int generate_init_cond_itb(FILE *fp, int value, int width, int chan_id_out, int chan_id_in)
{
    fprintf(fp,"\n// Initial token buffer for initial condition transmission\n");
    inc_itb_wrapper_count();
    fprintf(fp,"itb_wrapper<%d,%d,%d,%d> itb_w_%d(%s%d,%s%d);\n",capture_delay,pulse_width,width,value,
            get_itb_wrapper_count(), init_cond_chan_prefix,chan_id_out,init_cond_chan_prefix,chan_id_in);
    return get_itb_wrapper_count();
}

/*
    Generate dataless ITB that initializes the ring.
*/
int generate_itb(FILE *fp)
{   
    fprintf(fp,"\n// Initial token buffer to initialize ring\n");
    inc_block_count();
    fprintf(fp,"elem_c_itb %s%d;\n",ring_block_prefix,get_block_count());
    return get_block_count();
}

/*
    Generate a pipeline element for a given action, along
    with the necessary datapath elements. This is the main
    function that generates the circuit for a given action.
*/
int generate_pipe_element(act_chp_lang_t *c, FILE *fp, Process *p, int init_latch)
{
    ActId *chan;
    ActId *var = NULL;
    Expr *e = NULL;
    int burner_chan_id;
    int block_id, sync_out;
    int expr_inst_id;
    char chan_name[1024];
    int latch_id;
    const char *prefix;
    int bw;
    InstType *it;
    hash_bucket_t *b;
    var_info *vi;
    // CLMap clm(stdout);

    // if(c->type == ACT_CHP_SKIP) { return get_block_count(); }

    inc_block_count();
    block_id = get_block_count();

    switch(c->type)
    {
        case ACT_CHP_ASSIGN:
            // TODO - finish
            e = c->u.assign.e;
            var = c->u.assign.id;
            fprintf(fp,"\n// Pipe block for action: ");
            chp_print(fp,c);
            fprintf(fp,"\n");
            fprintf(fp,"elem_c_paa_brs_bd %s%d;\n",ring_block_prefix,block_id);

            fprintf(fp,"\n// Data for action: ");
            chp_print(fp,c);
            fprintf(fp,"\n");
            char tname[1024];
            get_true_name(tname, var, p->CurScope());
            b = hash_lookup(var_infos, tname);
            // b = hash_lookup(var_infos, var->rootVx(p->CurScope())->getName());
            vi = (var_info *)b->v;
            bw = vi->width;
            expr_inst_id = generate_expr_block(e,bw,p,fp);
            if (init_latch == -1)
            {
                latch_id = generate_single_latch(fp, vi);  
            }
            else 
            {
                latch_id = init_latch;
            }
            // connect output of math block to latch
            fprintf(fp,"%s%d.out = %s%s_%d.din;\n",expr_block_instance_prefix,expr_inst_id,
                                                capture_block_prefix,
                                                vi->name,latch_id);
            // connect pipe block to delay_expr input
            fprintf(fp,"delay_expr_%d.m1 = %s%d.zero;\n",expr_inst_id,ring_block_prefix,block_id);
            // connect delay_expr output to capture block
            fprintf(fp,"delay_expr_%d.p1 = %s%s_%d.go;\n",expr_inst_id,capture_block_prefix,
                                                vi->name,latch_id);
            
            break;

        case ACT_CHP_SEND:
            chan = c->u.comm.chan;
            e = c->u.comm.e;
            get_true_name (chan_name, chan, p->CurScope());
            // chan_name = chan->rootVx(p->CurScope())->getName();
            fprintf(fp,"\n// Pipe block for action: ");
            chp_print(fp,c);
            fprintf(fp,"\n");
            fprintf(fp,"elem_c_paa_brs_bd %s%d;\n",ring_block_prefix,block_id);
            if (e) {
                it = p->CurScope()->Lookup(chan);
                bw = TypeFactory::bitWidth(it);
                fprintf(fp,"connect_outchan_to_ctrl<%d> %s%d;\n",bw, conn_block_prefix,block_id);
                    fprintf(fp,"%s%d.ch = %s;\n",conn_block_prefix,block_id,chan_name);

                fprintf(fp,"\n// Data for action: ");
                chp_print(fp,c);
                fprintf(fp,"\n");
                if (expr_is_pure_variable(e, p)) { // pure variable send
                fprintf(fp,"%s%d.ctrl = %s%d.zero;\n",conn_block_prefix,block_id,ring_block_prefix,block_id);
                    var = (ActId *)e->u.e.l;
                    char tname[1024];
                    get_true_name(tname, var, p->CurScope());
                    b = hash_lookup(var_infos, tname);
                    // b = hash_lookup(var_infos, var->rootVx(p->CurScope())->getName());
                    vi = (var_info *)b->v;
                    // latch_id = vi->iread;
                    // latch_id = (vi->iwrite)-1;
                    // TEST ----
                    latch_id = vi->latest_for_read;
                    // TEST ----
                    Assert ((latch_id>=0),"variable read but never written? perhaps a loop-carried dependency, not supported yet...");
                    fprintf(fp, "\n%s%s_%d.dout = %s.d;\n",capture_block_prefix,
                                                vi->name,latch_id,chan_name);
                    vi->iread++;
                }
                else { // function of variable(s) send
                    // fprintf(fp, "\n\n// %d bits\n\n", bw);
                    expr_inst_id = generate_expr_block(e,bw,p,fp);
                    // connect output of math block to channel data
                    fprintf(fp,"%s%d.out = %s.d;\n",expr_block_instance_prefix,expr_inst_id,chan_name);

                    // connect to delay_line
                    fprintf(fp,"delay_expr_%d.m1 = %s%d.zero;\n",expr_inst_id,ring_block_prefix,block_id);
                    fprintf(fp,"delay_expr_%d.p1 = %s%d.ctrl;\n",expr_inst_id,conn_block_prefix,block_id);

                }

            }
            else { // dataless action
                fprintf(fp,"%s%d.zero = %s;\n",ring_block_prefix,block_id, chan_name);
            }
            break;

        case ACT_CHP_RECV:
            chan = c->u.comm.chan;
            var = c->u.comm.var;
            get_true_name (chan_name, chan, p->CurScope());
            // chan_name = chan->rootVx(p->CurScope())->getName();
            fprintf(fp,"\n// Pipe block for action: ");
            chp_print(fp,c);
            fprintf(fp,"\n");
            fprintf(fp,"elem_c_ppa_brs_bd %s%d;\n",ring_block_prefix,block_id);
            if (var) {
                it = p->CurScope()->Lookup(chan);
                bw = TypeFactory::bitWidth(it);
                fprintf(fp,"connect_inchan_to_ctrl<%d> %s%d;\n",bw, conn_block_prefix,block_id);
                fprintf(fp,"%s%d.ctrl = %s%d.zero;\n",conn_block_prefix,block_id,ring_block_prefix,block_id);
                fprintf(fp,"%s%d.ch = %s;\n",conn_block_prefix,block_id,chan_name);

                fprintf(fp,"\n// Data for action: ");
                chp_print(fp,c);
                fprintf(fp,"\n");
                char tname[1024];
                get_true_name(tname, var, p->CurScope());
                b = hash_lookup(var_infos, tname);
                // b = hash_lookup(var_infos, var->rootVx(p->CurScope())->getName());
                // b = hash_lookup(var_infos, var->rootCanonical(p->CurScope())->vx->getName());
                vi = (var_info *)b->v;
                if (init_latch == -1)
                {
                    latch_id = generate_single_latch(fp, vi);
                }
                else 
                {
                    latch_id = init_latch;
                }
                fprintf(fp, "%s%s_%d.go = %s%d.data;\n",capture_block_prefix,
                                vi->name,latch_id,ring_block_prefix,block_id);
                fprintf(fp, "%s%s_%d.din = %s.d;\n",capture_block_prefix,
                                                vi->name,latch_id,chan_name);
            }
            else { // dataless action
                fprintf(fp,"%s%d.zero = %s;\n",ring_block_prefix,block_id, chan_name);
            }
            break;

        case ACT_CHP_SKIP:
            fprintf(fp,"\n// Pipe block for action: ");
            chp_print(fp,c);
            fprintf(fp,"\n");
            fprintf(fp,"elem_c_skip %s%d;\n",ring_block_prefix,block_id);
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
int generate_pipe_element_custom(int bd_chan_id, int type, int width, ActId *var_init, FILE *fp, Process *p)
{
    ActId *chan;
    ActId *var = NULL;
    Expr *e = NULL;
    int burner_chan_id;
    int block_id, sync_out;
    int expr_inst_id;
    char chan_name[1024];
    int latch_id;
    const char *prefix;
    int bw;
    InstType *it;
    hash_bucket_t *b;
    var_info *vi;

    inc_block_count();
    block_id = get_block_count();

    switch(type)
    {
        case ACT_CHP_SEND:
            var = (ActId *)var_init; 
            snprintf(chan_name, 1024, "%s%d",init_cond_chan_prefix,bd_chan_id);
            fprintf(fp,"\n// Pipe block for init cond. send.");
            fprintf(fp,"\n");
            fprintf(fp,"elem_c_paa_brs_bd %s%d;\n",ring_block_prefix,block_id);
            if (var) {
                bw = width;
                fprintf(fp,"connect_outchan_to_ctrl<%d> %s%d;\n",bw, conn_block_prefix,block_id);
                    fprintf(fp,"%s%d.ch = %s;\n",conn_block_prefix,block_id,chan_name);

                fprintf(fp,"%s%d.ctrl = %s%d.zero;\n",conn_block_prefix,block_id,ring_block_prefix,block_id);
                    char tname[1024];
                    get_true_name(tname, var, p->CurScope());
                    b = hash_lookup(var_infos, tname);
                    // b = hash_lookup(var_infos, var->rootVx(p->CurScope())->getName());
                    vi = (var_info *)b->v;
                    // latch_id = vi->iread;
                    // latch_id = (vi->iwrite)-1;
                    // TEST ----
                    latch_id = vi->latest_for_read;
                    // TEST ----
                    Assert ((latch_id>=0),"variable read but never written? perhaps a loop-carried dependency, not supported yet...");
                    fprintf(fp, "\n%s%s_%d.dout = %s.d;\n",capture_block_prefix,
                                                vi->name,latch_id,chan_name);
                    vi->iread++;
                
            }
            else { // dataless action
                fatal_error ("What 2");
            }
            break;

        case ACT_CHP_RECV:
            snprintf(chan_name, 1024, "%s%d",init_cond_chan_prefix,bd_chan_id);
            var = (ActId *)var_init; 

            fprintf(fp,"\n// Pipe block for init cond. recv.\n");

            fprintf(fp,"elem_c_ppa_brs_bd %s%d;\n",ring_block_prefix,block_id);
            if (var) {
                bw = width;
                fprintf(fp,"connect_inchan_to_ctrl<%d> %s%d;\n",bw, conn_block_prefix,block_id);
                fprintf(fp,"%s%d.ctrl = %s%d.zero;\n",conn_block_prefix,block_id,ring_block_prefix,block_id);
                fprintf(fp,"%s%d.ch = %s;\n",conn_block_prefix,block_id,chan_name);
                char tname[1024];
                get_true_name(tname, var, p->CurScope());
                b = hash_lookup(var_infos, tname);
                // b = hash_lookup(var_infos, var->rootVx(p->CurScope())->getName());
                // b = hash_lookup(var_infos, var->rootCanonical(p->CurScope())->vx->getName());
                vi = (var_info *)b->v;
                latch_id = generate_single_latch(fp, vi);
                fprintf(fp, "%s%s_%d.go = %s%d.data;\n",capture_block_prefix,
                                vi->name,latch_id,ring_block_prefix,block_id);
                fprintf(fp, "%s%s_%d.din = %s.d;\n",capture_block_prefix,
                                                vi->name,latch_id,chan_name);
            }
            else { // dataless action
                fatal_error ("What");
            }
            break;

        default:
            fatal_error("Shouldn't be here... (generate_pipe_element_custom)");
            break;
    }

    return block_id;

}

/*
    Function to call an external logic synthesis tool to
    generate combinational logic to implement functions.
    Currently supports only abc. 
*/
int generate_expr_block(Expr *e, int out_bw, Process *p, FILE *fp)
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
    expr_collect_vars (e, 1, p);

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
    int xid = gen_expr_block_id();
    // int out_expr_width = get_expr_width (e, p);
    int out_expr_width = out_bw;
    int delay_line_n;

    fprintf(fp, "// output bitwidth: %d bits\n",out_expr_width);
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
        delay_line_n = int((typ_delay_ps/(2*invx1_delay_ps)) + 1); 
        if (delay_line_n == 0) { delay_line_n = 1; }

        fprintf(fp, "\n// typical delay: %gps\n",typ_delay_ps);
    }

    instantiate_expr_block (fp, xid, all_leaves, p);

    fprintf(fp,"delay_line_chan<%d> delay_expr_%d;\n",delay_line_n,xid);
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
int generate_expr_block_for_sel(Expr *e, Process *p, FILE *fp, int xid)
{
    // create mapper object
    ExternalExprOpt *eeo = new ExternalExprOpt(abc, bd, false, expr_file, 
                                                expr_block_input_prefix,
                                                expr_block_prefix);
    Assert ((eeo), "Could not create mapper");

    // collect input vars info
    _inexprmap = ihash_new (0);
    _inwidthmap = ihash_new (0);
    expr_collect_vars (e, 1, p);

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

    fprintf(fp, "\n// typical delay: %gps\n",typ_delay_ps);
    instantiate_expr_block (fp, xid, all_leaves, p);

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
void instantiate_expr_block (FILE *fp, int block_id, list_t *all_leaves, Process *p)
{
    ihash_bucket_t *ib, *ibw;
    listitem_t *li;

    // generate instance
    fprintf(fp,"%s%d %s%d;\n",expr_block_prefix,block_id,expr_block_instance_prefix,block_id);
    
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
            get_true_name(tname, var, p->CurScope());
            hash_bucket_t *b = hash_lookup(var_infos, tname);
            // hash_bucket_t *b = hash_lookup(var_infos, var->rootVx(p->CurScope())->getName());
            var_info *vi = (var_info *)b->v;

            // int latch_id = (vi->iwrite)-1;
            // TEST ----
            int latch_id = vi->latest_for_read;
            // TEST ----

            Assert ((latch_id>=0),"var. read before being written?");

            // connect variable-latch output to expr block input
            fprintf(fp,"%s%d.%s%d = %s%s_%d.dout;\n",expr_block_instance_prefix,block_id,
                                                    expr_block_input_prefix, ib->i, 
                                                    capture_block_prefix,
                                                    vi->name,latch_id);
        }
        // connect constants to math block inputs
        else if ( e_var->type == E_INT )
        {
            fatal_error ("This shouldn't have been used (constant as input to expr block)");
            // unsigned long v = e_var->u.ival.v;
            // int v_bw = get_expr_width (e_var, p);
            // int cnst_blk_id = gen_const_block_id();
            // // fprintf (stdout, "\n// constant value: %lu, bitwidth: %d \n", v, v_bw);
            // // instantiate constant-block subckt
            // fprintf (fp, "constant_value<%lu,%d> %s%d;\n", v, v_bw, const_block_prefix, cnst_blk_id);
            // // connect constant-block output to expr block input
            // fprintf (fp, "%s%d.%s%d = %s%d.d;\n", expr_block_instance_prefix,block_id,
            //                                             expr_block_input_prefix, ib->i,
            //                                             const_block_prefix, cnst_blk_id);
        }
        else { fatal_error ("leaf (primary input) is neither variable nor constant int?? (instantiate_expr_block)"); }
    }

}

/*
    Collect all the variables in a given expression and put them 
    in the exprmap and widthmap global variables.
*/
void expr_collect_vars (Expr *e, int collect_phase, Process *p)
{
  int id;
  Assert (e, "Hmm");

#define BINARY_OP					\
  do {							\
    expr_collect_vars (e->u.e.l, collect_phase, p);	\
    expr_collect_vars (e->u.e.r, collect_phase, p);	\
  } while (0)

#define UNARY_OP					\
  do {							\
    expr_collect_vars (e->u.e.l, collect_phase, p);	\
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
    expr_collect_vars (e->u.e.l, collect_phase, p);
    expr_collect_vars (e->u.e.r->u.e.l, collect_phase, p);
    expr_collect_vars (e->u.e.r->u.e.r, collect_phase, p);
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
	expr_collect_vars (tmp->u.e.l, collect_phase, p);
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
        get_true_name(tname, var, p->CurScope());
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
    Calculate the bitwidth of a given expression. Not actually
    used for anything other getting the bitwidth of a variable.
*/
int get_expr_width(Expr *ex, Process *p) {
  // recursively run through the expression and collect its width
  switch ((ex)->type)
  {
  // for a var read the bitwidth of that var
  case E_VAR:
  {
    ActId *var = (ActId *)ex->u.e.l;  
    hash_bucket_t *b;
    char tname[1024];
    get_true_name(tname, var, p->CurScope());
    b = hash_lookup(var_infos, tname);
    // b = hash_lookup(var_infos, var->rootVx(p->CurScope())->getName());
    var_info *vi = (var_info *)b->v;
    return vi->width;
  }
  // for true and false the bit width is one
  case E_TRUE:
  case E_FALSE:
    return 1;
  // for int look up the corresponding bitwidth
  case E_INT:
  {
    return ihash_lookup(_inwidthmap, (long) ex)->i;
  }
  // step through
  case E_QUERY:
    ex = ex->u.e.r;
  // get the max out of the right and the left expr part
  case E_AND:
  case E_OR:
  case E_XOR:
  {
    int lw = get_expr_width(ex->u.e.l, p);
    int rw = get_expr_width(ex->u.e.r, p);
    return std::max(lw,rw);
  }
  // get the max out of the right and the left expr part and one for the overflow bit
  case E_PLUS:
  case E_MINUS:
  {
    int lw = get_expr_width(ex->u.e.l, p);
    int rw = get_expr_width(ex->u.e.r, p);
    return std::max(lw,rw);
    // @TODO genus trys to tie the top bit and i dont know why
    // return std::max(lw,rw)+1;
  }
  // comparisons result in a bool so 1, do not walk further
  case E_LT:
  case E_GT:
  case E_LE:
  case E_GE:
  case E_EQ:
  case E_NE:
    // should be fine in ignoring the rest of the expr
    return 1;
  // for multiplication add both operand bitwidth
  case E_MULT:
  {
    int lw = get_expr_width(ex->u.e.l, p);
    int rw = get_expr_width(ex->u.e.r, p);
    return lw+rw;
  } 
  // step through
  case E_MOD:
  {
    int rw = get_expr_width(ex->u.e.r, p);
    return rw;
  } 
  // use left bitwidth and add number of shifted right
  case E_LSL:
  {
    int lw = get_expr_width(ex->u.e.l, p);
    int rw = get_expr_width(ex->u.e.r, p);
    return lw + (1 << rw);
  }
  // pass through
  case E_DIV:
  case E_LSR:
  case E_ASR:
  case E_UMINUS:
  case E_NOT:
  case E_COMPLEMENT:
  {
    int lw = get_expr_width(ex->u.e.l, p);
    return lw;
  }  

  //get the value out of the datastructure
  case E_BUILTIN_INT:
    if (ex->u.e.r) {
      Assert (ex->u.e.r->type == E_INT, "What?");
      return ex->u.e.r->u.ival.v;
    }
    else {
      return 1;
    }

  case E_BUILTIN_BOOL:
    return 1;
  // the following ones should give you errors because not handled
  case E_COLON:
  case E_COMMA:
    fatal_error ("Should have been handled elsewhere");
    break;

    /* XXX: here */
  case E_CONCAT:
    {
      int w = 0;
      Expr *tmp = ex;
      while (tmp) {
	w += get_expr_width (tmp->u.e.l, p);
	tmp = tmp->u.e.r;
      }
      return w;
    }
    break;

  case E_BITFIELD:
    // _var_getinfo ((ActId *)ex->u.e.l);
    // if (ex->u.e.r->u.e.l) {
    //   return (ex->u.e.r->u.e.r->u.ival.v - ex->u.e.r->u.e.l->u.ival.v + 1);
    // }
    // else {
    //   return 1;
    // }
    fatal_error ("Not handling bitfields right now.");
    break;

  case E_REAL:
    fatal_error ("No real expressions please.");
    break;

  case E_PROBE:
    fatal_error ("fix probes please");
    break;
    
  case E_FUNCTION:
    fatal_error ("function!");
    
  case E_SELF:
  default:
    fatal_error ("Unknown expression type %d\n", ex->type);
    break;
  }
  return 0;
}

/*
    Check if a given expression is actually just a single 
    variable. Used for a small optimization in send actions
    where the output of latches can directly be connected
    to the channel data. 
*/
int expr_is_pure_variable(Expr *e, Process *p)
{
    ValueIdx *vix;
    hash_bucket_t *b;
    var_info *vi;
    if (!e) {
        fatal_error ("no expression, should've been handled elsewhere");
        return 0;
    }
    switch (e->type){
    /* binary */
  case E_AND:
  case E_OR:
  case E_PLUS:
  case E_MINUS:
  case E_MULT:
  case E_DIV:
  case E_MOD:
  case E_LSL:
  case E_LSR:
  case E_ASR:
  case E_XOR:
  case E_LT:
  case E_GT:
  case E_LE:
  case E_GE:
  case E_EQ:
  case E_NE:
  case E_NOT:
  case E_UMINUS:
  case E_COMPLEMENT:
  case E_QUERY:
    break;

  case E_COLON:
  case E_COMMA:
    fatal_error ("what");
    break;

  case E_CONCAT:
  case E_BITFIELD:
    /* l is an Id */
  case E_TRUE:
  case E_FALSE:
    break;

  case E_INT:
  case E_REAL:
    break;

  case E_VAR:
    return 1;
    break;

  case E_PROBE:
    fatal_error ("Not handling probes right now");
    return 0;
    break;

  case E_BUILTIN_BOOL:
  case E_BUILTIN_INT:
    fatal_error ("not handled yet");
    break;
    
  case E_FUNCTION:
    fatal_error ("not handled yet");
    break;

  case E_SELF:
  default:
    fatal_error ("Unknown expression type %d\n", e->type);
    break;
    }

    return 0;

}

/*
    Given two pipeline block elements, connect them via
    their plus-1 (p1) and minus-1 (m1) ports. 
*/
int connect_pipe_elements (FILE *fp, int prev_block_id, int next_block_id, int chan_type)
{

    const char *prefix;

    if (chan_type == SYNC_SEQ)
    {
        prefix = sync_chan_name_prefix;
    }
    else if (chan_type == SYNC_PLL)
    {
        prefix = parallel_chan_name_prefix;
    }
    else 
    {
        fatal_error ("Unknown synchronization mode (connect_pipe_elements)");
    }
 
    if (connection_style == 1)
    {
        fprintf(fp,"\n// Channel for connecting block_%d & block_%d\n",prev_block_id, next_block_id);
        int chan_id = generate_sync_chan(fp);

        switch(chan_type)
        {
            case SYNC_SEQ:
                fprintf(fp,"block_%d.m1 = %s%d;\n",next_block_id,prefix,chan_id);
                fprintf(fp,"block_%d.p1 = %s%d;\n\n",prev_block_id,prefix,chan_id);
                break;

            case SYNC_PLL:
                fprintf(fp,"block_%d.m1 = %s%d;\n",next_block_id,prefix,chan_id);
                fprintf(fp,"block_%d.p1 = %s%d;\n\n",prev_block_id,prefix,chan_id);
                break;

            default:
                fatal_error("Shouldn't be here... (connect_pipe_elements)");
                break;
        }

        return chan_id;
    }
    else if (connection_style == 0)
    {
        fprintf(fp,"\n// Connecting block_%d & block_%d\n",prev_block_id, next_block_id);
        switch(chan_type)
        {
            case SYNC_SEQ:
                fprintf(fp,"block_%d.m1 = block_%d.p1;\n",next_block_id,prev_block_id);
                break;

            case SYNC_PLL:
                fprintf(fp,"block_%d.m1 = block_%d.p1;\n",next_block_id,prev_block_id);
                break;

            default:
                fatal_error("Shouldn't be here... (connect_pipe_elements)");
                break;
        }
        return 0;
    }
    else { fatal_error("shouldn't be here (connect_pipe_elements)"); return 0; }
}

/*
    Connect a given port of a parallel split block to another
    pipeline element. 
*/
int connect_pll_split_outputs_to_pipe (FILE *fp, int pll_split_block_id, int pipe_block_id, int pll_split_block_port)
{

    const char *prefix;
    prefix = parallel_chan_name_prefix;

    if (connection_style == 1)
    {
        fprintf(fp,"\n// Channel for connecting parallel split block_%d (output) & pipe block_%d\n",
                                                                pll_split_block_id, pipe_block_id);
        int chan_id = generate_sync_chan(fp);

        fprintf(fp,"block_%d.co[%d] = %s%d;\n",pll_split_block_id,pll_split_block_port,prefix,chan_id);
        fprintf(fp,"block_%d.m1 = %s%d;\n\n",pipe_block_id,prefix,chan_id);

        return chan_id;
    }
    else if (connection_style == 0)
    {
        fprintf(fp,"\n// Connecting parallel split block_%d (output) & pipe block_%d\n",
                                                    pll_split_block_id, pipe_block_id);

        fprintf(fp,"block_%d.co[%d] = block_%d.m1;\n",
                        pll_split_block_id,pll_split_block_port,pipe_block_id);
        return 0;
    }
    else { fatal_error("shouldn't be here (connect_pll_split_outputs_to_pipe)"); return 0; }
}

/*
    Connect a pipeline element to a given port on a parallel
    merge block. 
*/
int connect_pipe_to_pll_merge_inputs (FILE *fp, int pll_merge_block_id, int pipe_block_id, int pll_merge_block_port)
{

    const char *prefix;
    prefix = parallel_chan_name_prefix;
    
    if (connection_style == 1)
    {
        fprintf(fp,"\n// Channel for connecting parallel merge block_%d (input) & pipe block_%d\n",
                                                                pll_merge_block_id, pipe_block_id);
        int chan_id = generate_sync_chan(fp);

        fprintf(fp,"block_%d.ci[%d] = %s%d;\n",pll_merge_block_id,pll_merge_block_port,prefix,chan_id);
        fprintf(fp,"block_%d.p1 = %s%d;\n\n",pipe_block_id,prefix,chan_id);

        return chan_id;
    }
    else if (connection_style == 0)
    {
        fprintf(fp,"\n// Connecting parallel merge block_%d (input) & pipe block_%d\n",
                                                    pll_merge_block_id, pipe_block_id);
        fprintf(fp,"block_%d.ci[%d] = block_%d.p1;\n",
                        pll_merge_block_id,pll_merge_block_port,pipe_block_id);
        return 0;
    }
    else { fatal_error("shouldn't be here (connect_pipe_to_pll_merge_inputs)"); return 0; }
}

/*
    Connect a given port of a selection split block to another
    pipeline element. 
*/
int connect_sel_split_outputs_to_pipe (FILE *fp, int sel_split_block_id, int pipe_block_id, int sel_split_block_port)
{

    const char *prefix;
    prefix = sync_chan_name_prefix;

    if (connection_style == 1)
    {
        fprintf(fp,"\n// Channel for connecting selection split block_%d (output) & pipe block_%d\n",
                                                                sel_split_block_id, pipe_block_id);
        int chan_id = generate_sync_chan(fp);

        fprintf(fp,"block_%d.co[%d] = %s%d;\n",sel_split_block_id,sel_split_block_port,prefix,chan_id);
        fprintf(fp,"block_%d.m1 = %s%d;\n\n",pipe_block_id,prefix,chan_id);

        return chan_id;
    }
    else if (connection_style == 0)
    {
        fprintf(fp,"\n// Connecting selection split block_%d (output) & pipe block_%d\n",
                                                    sel_split_block_id, pipe_block_id);

        fprintf(fp,"block_%d.co[%d] = block_%d.m1;\n",
                        sel_split_block_id,sel_split_block_port,pipe_block_id);
        return 0;
    }
    else { fatal_error("shouldn't be here (connect_sel_split_outputs_to_pipe)"); return 0; }
}

/*
    Connect the output of guard evaluator to a given guard input
    of a selection split block. 
*/
int connect_guards_to_sel_split_input (FILE *fp, int sel_split_block_id,
                                        int expr_block_id, int sel_split_guard_port)
{
    const char *prefix;
    prefix = sync_chan_name_prefix;

    if (connection_style == 1)
    {
        // fprintf(fp,"\n// Channel for connecting selection split block_%d (input) & pipe block_%d\n",
        //                                                         sel_split_block_id, pipe_block_id);
        int chan_id = generate_sync_chan(fp);
        fatal_error ("dont use this mode (connect_guards_to_sel_split_input)");

        // fprintf(fp,"block_%d.p1 = %s%d;\n\n",pipe_block_id,prefix,chan_id);
        // fprintf(fp,"block_%d.m1 = %s%d;\n",sel_split_block_id,prefix,chan_id);
        // fprintf(fp,"block_%d.gs[%d] = inst_%d.out;\n",sel_split_block_id,sel_split_guard_port,
        //                                                 expr_block_id);
        // TODO: guard connection for this style

        return chan_id;
     }
    else if (connection_style == 0)
    {
        // inst_i.out is always size-1 array, not just a bool (1-bit datapath compatibility)
        fprintf(fp,"block_%d.gs[%d] = %s%d.out[0];\n",sel_split_block_id,sel_split_guard_port,
                                                        expr_block_instance_prefix, expr_block_id);
        return 0;
    }
    else { fatal_error("shouldn't be here (connect_pipe_to_sel_split_input)"); return 0; }

}

/*
    Connect a pipeline element to a given port on a selection
    merge block. 
*/
int connect_pipe_to_sel_merge_inputs (FILE *fp, int sel_merge_block_id, int pipe_block_id, int sel_merge_block_port)
{

    const char *prefix;
    prefix = sync_chan_name_prefix;
    
    if (connection_style == 1)
    {
        fprintf(fp,"\n// Channel for connecting selection merge block_%d (input) & pipe block_%d\n",
                                                                sel_merge_block_id, pipe_block_id);
        int chan_id = generate_sync_chan(fp);

        fprintf(fp,"block_%d.ci[%d] = %s%d;\n",sel_merge_block_id,sel_merge_block_port,prefix,chan_id);
        fprintf(fp,"block_%d.p1 = %s%d;\n\n",pipe_block_id,prefix,chan_id);

        return chan_id;
    }
    else if (connection_style == 0)
    {
        fprintf(fp,"\n// Connecting selection merge block_%d (input) & pipe block_%d\n",
                                                    sel_merge_block_id, pipe_block_id);
        fprintf(fp,"block_%d.ci[%d] = block_%d.p1;\n",
                        sel_merge_block_id,sel_merge_block_port,pipe_block_id);
        return 0;
    }
    else { fatal_error("shouldn't be here (connect_pipe_to_pll_merge_inputs)"); return 0; }
}