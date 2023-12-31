#include "reqs.h"

// TODO: Handling last action selection

#define ACTION_PASSIVE 1
#define ACTION_ACTIVE 2
#define ACTION_NONE 3

#define TERM_SINK 0
#define TERM_SRC 1

#define PORT_M1 -1
#define PORT_Z 0
#define PORT_P1 1

static unsigned int term_inst_id = 0;

// Name prefixes
static const char *term_inst_prefix = "term_inst_";
static const char *ring_blk_prefix = "block_";
static const char *conn_blk_prefix = "conn_z_";
static const char *capture_blk_prefix = "latch_";
static const char *expr_blk_instance_prefix = "inst_";

int gen_term_inst_id()
{
    term_inst_id++;
    return term_inst_id;
}

// Prototypes copy ----------
int generate_pipe_element(act_chp_lang_t *, FILE *, Process *, int=-1);
int expr_is_pure_variable(Expr *, Process *);
int generate_expr_block(Expr *, int, Process *, FILE *);
// --------------------------

// New prototypes
int terminate_port (FILE *, int, int, int);

/*
    Returns the type of a given action statement. Receives
    are passive, while sends and assignments are active.
*/
int action_type(act_chp_lang_t *c)
{
    if (!c) return ACTION_NONE;

    switch (c->type) {
    case ACT_CHP_COMMALOOP:
    case ACT_CHP_SEMILOOP:
        fatal_error ("Replication loops should've been removed..");
    case ACT_CHP_COMMA:
    case ACT_CHP_SEMI:
    case ACT_CHP_LOOP:
    case ACT_CHP_DOLOOP:
    case ACT_CHP_SELECT:
        return ACTION_NONE;
        break;

    case ACT_CHP_SELECT_NONDET:
        fatal_error ("Can't handle NDS");
        return ACTION_NONE;
        
    case ACT_CHP_SKIP:
    case ACT_CHP_ASSIGN:
    case ACT_CHP_ASSIGNSELF:
    case ACT_CHP_SEND:
        return ACTION_ACTIVE;
    case ACT_CHP_RECV:
        return ACTION_PASSIVE;
        break;

    case ACT_CHP_FUNC:
    case ACT_CHP_HOLE: /* to support verification */
    case ACT_CHP_MACRO:
    case ACT_HSE_FRAGMENTS:
        return ACTION_NONE;
        break;

    default:
        fatal_error ("Unknown type");
        return ACTION_NONE;
        break;
    }

}

/* 
    Checks if a given chp tree is of a particular form that
    allows it to be implemented with a single pipeline
    element that intrinsically sequences those actions.
    A loose way of thinking about this is checking 
    whether the CHP has 3 or fewer actions in every path.
    (not a sufficient condition, merges break this, for example)
*/
int check_if_pipeable (act_chp_lang_t *c, Process *p, int root)
{
    int is_pipeable, i_action_type;
    listitem_t *li;
    act_chp_lang_t *stmt;
    act_chp_gc_t *gc;
    if (!c) return 0;

    switch (c->type) {
    case ACT_CHP_COMMALOOP:
    case ACT_CHP_SEMILOOP:
        fatal_error ("Replication loops should've been removed..");
        break;
        
    case ACT_CHP_COMMA:
        fatal_error ("TODO: parallellizers");
        break;

    case ACT_CHP_SEMI:

        if ( list_length(c->u.semi_comma.cmd) <= 3 )
        {
            for (li = list_first (c->u.semi_comma.cmd); li; li = list_next (li)) 
            {
                i_action_type = action_type ( (act_chp_lang_t *)(list_value(li)) );
                if ( i_action_type != ACTION_PASSIVE && i_action_type != ACTION_ACTIVE )
                    return 0;
            }
            return 1;
        }
        else
        {
            return 0;
        }
        break;

    case ACT_CHP_LOOP:
    case ACT_CHP_DOLOOP:
        if (root == 1)
        {
            gc = c->u.gc;
            is_pipeable = check_if_pipeable (gc->s, p, 0);
            return is_pipeable;
            break;
        }
        else
        {
            fatal_error ("should've excised internal loops...");
        }
        break;
        
    case ACT_CHP_SELECT:
        // check if
        return 0; // TODO

        for (gc = c->u.gc ; gc ; gc = gc->next)
        {
            is_pipeable = check_if_pipeable (gc->s, p, 0);
        }
        break;

    case ACT_CHP_SELECT_NONDET:
        fatal_error ("Can't handle NDS");
        
    case ACT_CHP_SKIP:
    case ACT_CHP_ASSIGN:
    case ACT_CHP_ASSIGNSELF:
    case ACT_CHP_RECV:
    case ACT_CHP_SEND:
        return 1;
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

    return 0;
}

/*
    Optimized synthesis for small programs. Single pipe element which 
    sequences 3 actions. Currently only works for linear programs. 
    Can be extended to work with branched programs with the correct 
    (action signature on each branch is the same) structure.  
*/
void generate_pipe (act_chp_lang_t *c, FILE *fp, int root, Process *p)
{   
    int is_pipeable, i_action_type;
    listitem_t *li;
    act_chp_lang_t *stmt;
    act_chp_gc_t *gc;
    int block_id, bw, latch_id, expr_inst_id;
    InstType *it;
    Expr *e;
    ActId *chan, *var;
    const char* chan_name;
    
    if (!c) return;

    switch (c->type) {
    case ACT_CHP_COMMALOOP:
    case ACT_CHP_SEMILOOP:
        fatal_error ("Replication loops should've been removed..");
        break;
        
    case ACT_CHP_COMMA:
    case ACT_CHP_SEMI:

        switch (list_length(c->u.semi_comma.cmd)) {
        case 2:
            li = (list_first (c->u.semi_comma.cmd));
            stmt = (act_chp_lang_t *)list_value (li);
            block_id = generate_pipe_element (stmt, fp, p);

            terminate_port (fp, block_id, PORT_M1, TERM_SRC);
            li = list_next(li);
            stmt = (act_chp_lang_t *)list_value (li);
            Assert ((action_type(stmt)==ACTION_ACTIVE), "2nd action active only (for now)");

            if (stmt->type == ACT_CHP_ASSIGN)
            {
                e = stmt->u.assign.e;
            }
            else if (stmt->type == ACT_CHP_SEND)
            {
                e = stmt->u.comm.e;
                chan = stmt->u.comm.chan;
                chan_name = chan->rootVx(p->CurScope())->getName();
            }
            if (e) {

                it = p->CurScope()->Lookup(chan);
                bw = TypeFactory::bitWidth(it);
                fprintf(fp,"connect_outchan_to_ctrl<%d> %s%d;\n",bw, conn_blk_prefix,block_id+1);
                    fprintf(fp,"%s%d.ch = %s;\n",conn_blk_prefix,block_id+1,chan_name);

                fprintf(fp,"\n// Data for action: ");
                chp_print(fp,stmt);
                fprintf(fp,"\n");
                if (expr_is_pure_variable(e, p)) { // pure variable send
                fprintf(fp,"%s%d.ctrl = %s%d.p1;\n",conn_blk_prefix,block_id+1,ring_blk_prefix,block_id);
                    var = (ActId *)e->u.e.l;
                    latch_id = 0;
                    fprintf(fp, "\n%s%s_%d.dout = %s.d;\n",capture_blk_prefix,
                                                var->rootVx(p->CurScope())->getName(),latch_id,chan_name);
                }
                else { // function of variable(s) send

                    expr_inst_id = generate_expr_block(e,bw,p,fp);
                    // connect output of math block to channel data
                    fprintf(fp,"%s%d.out = %s.d;\n",expr_blk_instance_prefix,expr_inst_id,chan_name);
                    // connect to delay_line
                    fprintf(fp,"delay_expr_%d.m1 = %s%d.p1;\n",expr_inst_id,ring_blk_prefix,block_id);
                    fprintf(fp,"delay_expr_%d.p1 = %s%d.ctrl;\n",expr_inst_id,conn_blk_prefix,block_id);
                }
            }

            break;

        case 3:
            fatal_error ("not working yet..");
            li = list_next (list_first (c->u.semi_comma.cmd));
            stmt = (act_chp_lang_t *)list_value (li);
            block_id = generate_pipe_element (stmt, fp, p);

            break;

        default:
            fatal_error ("shouldn't be here, pipe check must've failed (generate_pipe)");
            break;
        }
        
        break;

    case ACT_CHP_LOOP:
    case ACT_CHP_DOLOOP:
        if (root == 1)
        {
            gc = c->u.gc;
            generate_pipe (gc->s, fp, 0, p);
            break;
        }
        else
        {
            fatal_error ("should've excised internal loops...");
        }
        break;
        
    case ACT_CHP_SELECT:

        return; // TODO - need to build the checker correctly first

        for (gc = c->u.gc ; gc ; gc = gc->next)
        {
        }
        break;

    case ACT_CHP_SELECT_NONDET:
        fatal_error ("Can't handle NDS");
        
    case ACT_CHP_SKIP:
    case ACT_CHP_ASSIGN:
    case ACT_CHP_ASSIGNSELF:
    case ACT_CHP_SEND:
            block_id = generate_pipe_element (c, fp, p);
            terminate_port (fp, block_id, PORT_M1, TERM_SRC);
            terminate_port (fp, block_id, PORT_P1, TERM_SINK);
            break;
    case ACT_CHP_RECV:
            block_id = generate_pipe_element (c, fp, p);
            terminate_port (fp, block_id, PORT_M1, TERM_SRC);
            terminate_port (fp, block_id, PORT_P1, TERM_SINK);
            terminate_port (fp, block_id, PORT_Z, TERM_SINK);
            break;


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

    return;
}

/*
    Given a pipeline element, port and mode (either source or sink), 
    terminate that port with the correct element. This is used for 
    tying off unused ports in the pipeline element. 
*/
int terminate_port (FILE *fp, int block_id, int port, int mode)
{   
    int term_inst;
    const char *port_name;
    const char *term_block_name;

    switch (mode)
    {
    case TERM_SRC:
        term_block_name = "source_brs";
        break;
    case TERM_SINK:
        term_block_name = "sink_brs";
        break;
    default:
        fatal_error ("brr (terminate_port)");
        break;
    }

    switch (port)
    {
    case PORT_M1:
        port_name = "m1";
        break;
    case PORT_P1:
        port_name = "p1";
        break;
    case PORT_Z:
        port_name = "zero";
        break;
    default:
        fatal_error ("brr (terminate_port)");
        break;
    }
    term_inst = gen_term_inst_id();
    fprintf (fp, "%s %s%d;\n", term_block_name, term_inst_prefix, term_inst);
    fprintf (fp, "%s%d.c = %s%d.%s;\n", term_inst_prefix, term_inst, ring_blk_prefix, block_id, port_name);

    return 0;
}