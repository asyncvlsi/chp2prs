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

#include "tiny_forge.h"

// TODO: Handling last action selection

#define TERM_SINK 0
#define TERM_SRC 1

// Name prefixes

TinyForge::TinyForge ( FILE *fp, Process *p, act_chp_lang_t *c,
            ActBooleanizePass *bp, 
            int delay_margin, 
            const char *circuit_library,
            const char *exprfile )
    : RingForge ( fp, p, c, bp, delay_margin, circuit_library, exprfile )
{
    term_inst_prefix = "term_inst_";
}

void TinyForge::run_forge ()
{
    construct_var_infos ();
    _run_forge (_c, 1);
}

int TinyForge::_gen_term_inst_id()
{
    term_inst_id++;
    return term_inst_id;
}

bool TinyForge::check_if_pipeable (act_chp_lang_t *c)
{
    prog_signature.clear();
    if (_build_prog_signature (c, 1)) {
        return valid_signatures.contains(prog_signature);
    }
    return false;
}

/* 
    Checks if a given chp tree is of a particular form that
    allows it to be implemented with a single pipeline
    element that intrinsically sequences those actions.
    A loose way of thinking about this is checking 
    whether the CHP has 3 or fewer actions in every path.
    (not a sufficient condition, merges break this, for example)
*/
bool TinyForge::_build_prog_signature (act_chp_lang_t *c, int root)
{
    listitem_t *li;
    act_chp_lang_t *stmt;
    act_chp_gc_t *gc;
    if (!c) return false;

    switch (c->type) {
    case ACT_CHP_COMMALOOP:
    case ACT_CHP_SEMILOOP:
        fatal_error ("Replication loops should've been removed..");
        return false;
        break;
        
    case ACT_CHP_COMMA:
        return false;
        break;

    case ACT_CHP_SEMI:
        if ( list_length(c->u.semi_comma.cmd) > 2 )
        {
            return false;
        }
        for (li = list_first(c->u.semi_comma.cmd) ; li ; li = li->next)
        {
            stmt = (act_chp_lang_t *) list_value(li);
            switch (stmt->type) {
            case ACT_CHP_SEND:
                prog_signature.push_back(Action::Send); break;
            case ACT_CHP_RECV:
                prog_signature.push_back(Action::Receive); break;
            case ACT_CHP_ASSIGN:
                prog_signature.push_back(Action::Assign); break;
            default:
                return false;
            }
        }
        return true;
        break;

    case ACT_CHP_LOOP:
    case ACT_CHP_DOLOOP:
        if (root == 1)
        {
            gc = c->u.gc;
            return _build_prog_signature (gc->s, 0); 
            break;
        }
        fatal_error ("should've excised internal loops...");
        break;
        
    case ACT_CHP_SELECT:
        return false; break;

    case ACT_CHP_SELECT_NONDET:
        fatal_error ("Can't handle NDS"); break;
        
    // should only get here for single action programs..
    case ACT_CHP_SKIP:
    case ACT_CHP_ASSIGN:
            prog_signature.push_back(Action::Assign); return true; break;
    case ACT_CHP_ASSIGNSELF:
    case ACT_CHP_RECV:
            prog_signature.push_back(Action::Receive); return true; break;
    case ACT_CHP_SEND:
            prog_signature.push_back(Action::Send); return true; break;
        
    case ACT_CHP_FUNC:
    case ACT_CHP_HOLE: /* to support verification */
    case ACT_CHP_MACRO:
    case ACT_HSE_FRAGMENTS:
        return false;
        break;

    default:
        fatal_error ("Unknown type");
        break;
    }
    return false;
}

/*
    Optimized synthesis for small programs. Single pipe element which 
    sequences 3 actions. Currently only works for linear programs. 
    Can be extended to work with branched programs with the correct 
    (action signature on each branch is the same) structure.  
*/
void TinyForge::_run_forge (act_chp_lang_t *c, int root)
{   
    listitem_t *li;
    act_chp_lang_t *stmt1, *stmt2;
    act_chp_gc_t *gc;
    int block_id, bw, latch_id, expr_inst_id;
    hash_bucket_t *b;
    InstType *it;
    var_info *vi;
    Expr *e = NULL;
    ActId *chan, *var;
    char chan_name[1024];
    char var_name[1024];
    
    if (!c) return;

    switch (c->type) {
    case ACT_CHP_COMMALOOP:
    case ACT_CHP_SEMILOOP:
        fatal_error ("Replication loops should've been removed.."); break;
        
    case ACT_CHP_COMMA:
        fatal_error ("Shouldn't have gotten here.."); break; 

    case ACT_CHP_SEMI:
        fatal_error ("Shouldn't have gotten here.."); break; 
#if 0
        Assert ((list_length(c->u.semi_comma.cmd) == 2), "hmm, pipe check failed?"); 
        li = (list_first (c->u.semi_comma.cmd));
        stmt1 = (act_chp_lang_t *)list_value (li);
        stmt2 = (act_chp_lang_t *)list_value (list_next(li));

        Assert (stmt1->type == ACT_CHP_RECV, "hmm");
        var = stmt1->u.comm.var;
        chan = stmt1->u.comm.chan;
        get_true_name (chan_name, chan, _p->CurScope());
        it = _p->CurScope()->Lookup(chan);
        bw = TypeFactory::bitWidth(it);
        if (var) {
            get_true_name (var_name, var, _p->CurScope());
            b = hash_lookup (var_infos, var_name);
            Assert (b, "No var info?");
            vi = (var_info *)b->v;
            latch_id = _generate_single_latch (vi, -1);
        }

        // generate pipe element according to second action
        block_id = _generate_pipe_element (stmt2, -1);
        _terminate_port (block_id, Port::P1, Term::Sink);

        fprintf(_fp,"connect_inchan_to_ctrl<%d> %s%d;\n", bw, conn_block_prefix,block_id+1);
        fprintf(_fp,"%s%d.ctrl = %s%d.m1;\n",conn_block_prefix,block_id+1,ring_block_prefix,block_id);
        fprintf(_fp,"%s%d.ch = %s;\n",conn_block_prefix,block_id+1,chan_name);

        if (var) {
        fprintf(_fp, "%s%s_%d.go = %s%d.data;\n",capture_block_prefix,
                        vi->name,latch_id,ring_block_prefix,block_id);
        fprintf(_fp, "%s%s_%d.din = %s.d;\n",capture_block_prefix,
                                        vi->name,latch_id,chan_name);
        }
#endif
        break;

    case ACT_CHP_LOOP:
    case ACT_CHP_DOLOOP:
        if (root == 1)
        {
            gc = c->u.gc;
            _run_forge (gc->s, 0);
            break;
        }
        fatal_error ("Shouldn't have gotten here.."); break;
        
    case ACT_CHP_SELECT:
    case ACT_CHP_SELECT_NONDET:
        fatal_error ("Shouldn't have gotten here..");
        
    case ACT_CHP_SKIP:
    case ACT_CHP_ASSIGN:
    case ACT_CHP_ASSIGNSELF:
    case ACT_CHP_SEND:
            block_id = _generate_pipe_element (c, -1);
            _terminate_port (block_id, Port::M1, Term::Source);
            _terminate_port (block_id, Port::P1, Term::Sink);
            break;

    case ACT_CHP_RECV:
            block_id = _generate_pipe_element (c, -1);
            _terminate_port (block_id, Port::M1, Term::Source);
            _terminate_port (block_id, Port::P1, Term::Sink);
            _terminate_port (block_id, Port::Zero, Term::Sink);
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
void TinyForge::_terminate_port (int block_id, Port port, Term mode)
{   
    int term_inst;
    const char *ti_prefix = "term_inst_";
    term_inst = _gen_term_inst_id();

    switch (mode)
    {
    case Term::Source:
        fprintf (_fp, "source_brs %s%d;\n", ti_prefix, term_inst); break;
    case Term::Sink:
        fprintf (_fp, "sink_brs %s%d;\n", ti_prefix, term_inst); break;
    default:
        fatal_error ("brr (terminate_port)"); break;
    }

    switch (port)
    {
    case Port::M1:
        fprintf (_fp, "%s%d.c = %s%d.m1;\n", ti_prefix, term_inst, ring_block_prefix, block_id); break;
    case Port::P1:
        fprintf (_fp, "%s%d.c = %s%d.p1;\n", ti_prefix, term_inst, ring_block_prefix, block_id); break;
    case Port::Zero:
        fprintf (_fp, "%s%d.c = %s%d.zero;\n", ti_prefix, term_inst, ring_block_prefix, block_id); break;
    default:
        fatal_error ("brr (terminate_port)"); break;
    }

    return;
}