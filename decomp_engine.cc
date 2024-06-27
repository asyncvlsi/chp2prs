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

  void runSynth (ActPass *ap, Process *p) {
    pp_printf (_pp, "/* decomposition output */");
    pp_forced (_pp, 0);

    pp_flush (_pp);
    fprintf (_pp->fp, "/* start decomp */\n");
    fflush (_pp->fp);

    int chpopt;
    ActDynamicPass *dp;

    dp = dynamic_cast <ActDynamicPass *> (ap);
    Assert (dp, "What?");

    chpopt = dp->getIntParam ("chp_optimize");

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

      std::vector<ActId *> newnames;
      act_chp_lang_t *l = chp_graph_to_act (g, newnames, p->CurScope());
      p->getlang()->getchp()->c = l;
    
      MultiChan *mc = new MultiChan (_pp->fp, g, p->CurScope());
      mc->process_multichans();
      auto vs = mc->get_auxiliary_procs();

      l = chp_graph_to_act (g, newnames, p->CurScope());

#if 0
      BreakPoints *bkp = new BreakPoints (_pp->fp, g, p->CurScope());
      bkp->mark_breakpoints();
      // bkp->print_decomp_info();

      ChoppingBlock *cb = new ChoppingBlock (_pp->fp, g, 
                                bkp->get_decomp_info_map(), p->CurScope());
      // cb->chop_graph();
      cb->excise_internal_loops();
      auto vs = cb->get_chopped_seqs();
#endif

      act_chp_lang_t *decomp;
      NEW (decomp, act_chp_lang_t);
      decomp->type = ACT_CHP_COMMA;
      list_t *decomp_procs = list_new();

      if (vs.empty()) {
        p->getlang()->getchp()->c = l;
      }
      else {
        list_append(decomp_procs, l);

        for (auto v : vs)
        {
          GraphWithChanNames gc;
          gc.graph.id_pool() = g.graph.id_pool();
          gc.graph.m_seq = v;
          gc.name_from_chan = g.name_from_chan;  
          // std::vector<ActId *> newnames;
          act_chp_lang_t *l1 = chp_graph_to_act (gc, newnames, p->CurScope());
          list_append(decomp_procs, l1);
          fprintf (_pp->fp, "\n\n");
        }
        decomp->u.semi_comma.cmd = decomp_procs;
        p->getlang()->getchp()->c = decomp;
      }
    }

    // print out new chp
    fprintf (_pp->fp, "chp {\n");
    chp_print (_pp->fp, p->getlang()->getchp()->c);
    fprintf (_pp->fp, "\n}\n");

    fprintf (_pp->fp, "\n\n");
    fprintf (_pp->fp, "/* end decomp */\n");
    
    pp_forced (_pp, 0);
  }
};

ActSynthesize *gen_decomp_engine (const char *prefix,
				char *infile,
				char *outfile,
				char *exprfile)

{
  return new Decomp (prefix, infile, outfile, exprfile);
}

  