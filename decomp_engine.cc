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
#include "synth.h"
#include "engines.h"

#include "decomp/breakpoint.h"
#include "decomp/chopping_block.h"
#include "decomp/pretty_print.h"
#include "decomp/multichan.h"

#include "opt/chp-opt.h"

class Decomp : public ActSynthesize {
 public:
  Decomp (const char *prefix,
	    char *infile,
	    char *outfile,
	    char *exprfile)
    : ActSynthesize (prefix, infile, outfile, exprfile) { 
    if (!exprfile) {
      fatal_error ("Ring Synthesis: requires an expression file");
    }
    }
  
  void emitTopImports(ActPass *ap) {
    ActDynamicPass *dp = dynamic_cast <ActDynamicPass *> (ap);
    Assert (dp, "Hmm");

    /* print imports */
    fprintf (_expr, "namespace syn {\n\nexport namespace expr {\n\n");
    fclose (_expr);
    _expr = NULL;
  }

  bool overrideTypes() { return false; }

  void typeInt (char *buf, int sz, int bitwidth) {
    snprintf (buf, sz, "int<%d>", bitwidth);
  }
  void typeBool (char *buf, int sz) {
    snprintf (buf, sz, "bool");
  }
  void typeIntChan (char *buf, int sz, int bitwidth) {
    snprintf (buf, sz, "chan(int<%d>)", bitwidth);
  }
  void typeBoolChan (char *buf, int sz) {
    fatal_error ("chan(bool)");
  }
  void runPreSynth (ActPass *ap, Process *p) {
    pp_printf (_pp, "/* decomposition output - might have fresh instances */");
    pp_forced (_pp, 0);

    pp_flush (_pp);
    // fprintf (_pp->fp, "/* start decomp */\n");
    fflush (_pp->fp);

    int chpopt, pll;
    ActDynamicPass *dp;

    dp = dynamic_cast <ActDynamicPass *> (ap);
    Assert (dp, "What?");

    chpopt = dp->getIntParam ("chp_optimize");
    pll = dp->getIntParam ("parallelism");

    if (p->getlang() && p->getlang()->getchp()) {
      auto g = ChpOptimize::chp_graph_from_act (p->getlang()->getchp()->c,
						p->CurScope ());

      if (chpopt) {
        ChpOptimize::optimize_chp_O2 (g.graph, p->getName(), false);
      }
      else {
        ChpOptimize::optimize_chp_O0 (g.graph, p->getName(), false);
        ChpOptimize::eliminateDeadCode (g.graph);
      }
      uninlineBitfieldExprsHack (g.graph);

      std::vector<ActId *> tmp_names;
      std::unordered_set<ActId *> newnames;
    
      std::vector<Sequence> vs, vs1;

      // necessary decompositions for synthesis -------------------------------
      MultiChan *mc = new MultiChan (_pp->fp, g, p->CurScope());
      mc->process_multichans();
      vs = mc->get_auxiliary_procs();

      BreakPoints *bkp = new BreakPoints (_pp->fp, g, p->CurScope(), 0);
      ChoppingBlock *cb = new ChoppingBlock (_pp->fp, g, 
                                bkp->get_decomp_info_map(), p->CurScope());
      cb->excise_internal_loops();
      vs1 = cb->get_chopped_seqs();

      Block *top = g.graph.blockAllocator().newBlock(Block::makeParBlock());
      top->u_par().branches.push_back(g.graph.m_seq);

      chp_graph_to_act (g, tmp_names, p->CurScope());
      for ( auto x : tmp_names ) { newnames.insert(x); }
      act_chp_lang_t *top_chp;
      NEW (top_chp, act_chp_lang_t);
      top_chp->label = NULL;
      top_chp->type = ACT_CHP_COMMA;
      top_chp->u.semi_comma.cmd = list_new();

      for ( auto vv : {vs,vs1} ) 
      {
        for (auto v : vv)
        {
          g.graph.m_seq = v;
          act_chp_lang_t *tmp = chp_graph_to_act (g, tmp_names, p->CurScope());
          for ( auto x : tmp_names ) { newnames.insert(x); }
          list_append(top_chp->u.semi_comma.cmd, tmp);
        }
      }
      // ----------------------------------------------------------------------

#if 1
      // concurrent decomposition for slack elastic programs ------------------
      std::vector<Sequence> vs2;
      Block *top2 = g.graph.blockAllocator().newBlock(Block::makeParBlock());
      
      // only decomposing main program for now
      for ( auto d_seq : top->u_par().branches )
      {
        g.graph.m_seq = d_seq;

        BreakPoints *bkp2 = new BreakPoints (_pp->fp, g, p->CurScope(), pll);
        bkp2->mark_breakpoints();
        ChoppingBlock *cb2 = new ChoppingBlock (_pp->fp, g, 
                                  bkp2->get_decomp_info_map(), p->CurScope());
        cb2->chop_graph();
        vs2 = cb2->get_chopped_seqs();

        for (auto v : vs2)
        {
          g.graph.m_seq = v;
          act_chp_lang_t *tmp2 = chp_graph_to_act (g, tmp_names, p->CurScope());
          list_append(top_chp->u.semi_comma.cmd, tmp2);
          for ( auto x : tmp_names ) { newnames.insert(x); }
        }
      }
      // ----------------------------------------------------------------------
#endif

      act_chp_lang_t *l = top_chp;
      p->getlang()->getchp()->c = l;

      if (!_decomp_vx) {
        _decomp_vx = list_new();
      }
      for (auto id : newnames) {
        ValueIdx *vx = p->CurScope()->LookupVal (id->getName());
        Assert (vx, "can't find ValueIdx in scope?");
        list_append(_decomp_vx, vx);
      }
      for (auto id : g.name_from_chan) {
        const char *channame = (id.second)->getName();
        ValueIdx *vx = p->CurScope()->LookupVal (channame);
        Assert (vx, "can't find ValueIdx in scope?");
        // TODO: there may be a better way to check for new channels..
        // Using first 3 chars for now
        if (strncmp(channame, "_ch", 3) == 0) {
          list_append(_decomp_vx, vx);
        }
      }
      
    }
    pp_forced (_pp, 0);
  }

  void runSynth (ActPass *ap, Process *p) {

    if (p->getlang() && p->getlang()->getchp()) {
      fprintf (_pp->fp, "chp {top_decomp:(\n");
      chp_pretty_print (_pp->fp, p->getlang()->getchp()->c);
      fprintf (_pp->fp, "\n)}\n");
    }

    fprintf (_pp->fp, "\n\n");
    fprintf (_pp->fp, "/* end decomp */\n");
  }
};

ActSynthesize *gen_decomp_engine (const char *prefix,
				char *infile,
				char *outfile,
				char *exprfile)

{
  return new Decomp (prefix, infile, outfile, exprfile);
}

  
