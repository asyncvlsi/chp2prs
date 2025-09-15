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

#include <act/chp/breakpoint.h>
#include <act/chp/chopping_block.h>
#include <act/chp/projection.h>
#include <act/chp/pretty_print.h>
#include <act/chp/multichan.h>

#include "ring/ring_else_gen.h"

#include <act/chp/chp-opt.h>
#include <act/chp/static-tokens.h>

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

  bool overrideTypes() { return true; }

  bool skipOverride (ValueIdx *vx) {
    if (TypeFactory::isProcessType(vx->t))
      return false;
    return true;
  }

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

    int chpopt, pll, project;
    ActDynamicPass *dp;

    dp = dynamic_cast <ActDynamicPass *> (ap);
    Assert (dp, "What?");

    chpopt = dp->getIntParam ("chp_optimize");
    pll = dp->getIntParam ("parallelism");
    project = dp->getIntParam ("project");

    if (p->getlang() && p->getlang()->getchp()) {

      fill_in_else_explicit (p->getlang()->getchp()->c, p);

      auto g = ChpOptimize::chp_graph_from_act (p->getlang()->getchp()->c,
						p->CurScope (), 1);

      if (chpopt) {
        // ChpOptimize::optimize_chp_O2 (g.graph, p->getName(), false);
        ChpOptimize::optimize_chp_basic2 (g.graph, p->getName(), false);
      }
      else {
        ChpOptimize::optimize_chp_basic (g.graph, p->getName(), false);
        // ChpOptimize::eliminateDeadCode (g.graph);
      }
      uninlineBitfieldExprsHack (g.graph);

#if 0
      auto xg = ChpOptimize::chp_graph_from_act (p->getlang()->getchp()->c,
          p->CurScope (), 1);
      ChpOptimize::optimize_chp_basic (xg.graph, "brr", false);
      ChpOptimize::putIntoNewStaticTokenForm(xg.graph);
      fprintf(stdout, "\n ------- stf -------- \n");
      print_chp(std::cout, xg.graph);
      fprintf(stdout, "\n ------- stf -------- \n");
      ChpOptimize::takeOutOfNewStaticTokenForm(xg.graph);
      fprintf(stdout, "\n ------- non-stf -------- \n");
      print_chp(std::cout, xg.graph);
      fprintf(stdout, "\n ------- non-stf -------- \n");
      std::vector<ActId *> xnms;
      auto xchp = chp_graph_to_act (xg, xnms, p->CurScope());
      fprintf(stdout, "\n ------- chp -------- \n");
      chp_print(stdout, xchp);
      fprintf(stdout, "\n ------- chp -------- \n");
      exit(2);
#endif

      std::unordered_set<ActId *> newnames;
      std::vector<ActId *> tmp_names;

      act_chp_lang_t *top_chp;
      top_chp = new act_chp_lang_t;
      top_chp->label = NULL;
      top_chp->space = NULL;
      top_chp->type = ACT_CHP_COMMA;
      top_chp->u.semi_comma.cmd = list_new();

      // necessary rewrites for ring synthesis --------------------------------
      MultiChan *mc = new MultiChan (_pp->fp, g, p->CurScope());
      mc->process_multichans();
      auto vs = mc->get_auxiliary_procs();
      
      DecompAnalysis *dca = new DecompAnalysis (_pp->fp, g, p->CurScope());
      dca->analyze();
      
      ChoppingBlock *cb = new ChoppingBlock (_pp->fp, g, 
                            dca->get_decomp_info_map(), p->CurScope());
      cb->excise_internal_loops();
      auto vs1 = cb->get_chopped_seqs();
        
      for ( auto vv : {{g.graph.m_seq}, vs, vs1} ) {
        for (auto v : vv) {
          g.graph.m_seq = v;
          act_chp_lang_t *tmp = chp_graph_to_act (g, tmp_names, p->CurScope());
          for ( auto x : tmp_names ) { newnames.insert(x); }
          list_append(top_chp->u.semi_comma.cmd, tmp);
        }
      }
      // ----------------------------------------------------------------------
      
      // projection/decomposition for slack elastic programs ------------------
      std::vector<std::unordered_map<ChpOptimize::ChanId, ActId *>> nfc = {};
      if (project) {
        std::vector<Strategy> prj_steps = {};
        prj_steps = {Strategy::None, Strategy::BruteForce};
        for ( auto ss : prj_steps ) {
          fill_in_else_explicit (top_chp, p);
          auto gnew = chp_graph_from_act (top_chp, p->CurScope(), 1);
          DecompAnalysis *dca2 = new DecompAnalysis (_pp->fp, gnew, p->CurScope());
          dca2->analyze();
          Projection *pr2 = new Projection (_pp->fp, gnew, 
                          dca2->get_decomp_info_map(), p->CurScope());
          pr2->project(ss);
          auto [names2, top_chp2, nfc2] = pr2->get_result();
          for ( auto x : names2 ) { newnames.insert(x); }
          for ( auto x : nfc2 ) { nfc.push_back(x); }
          
          // list_concat(top_chp->u.semi_comma.cmd, top_chp2->u.semi_comma.cmd);
          top_chp = top_chp2;
        }
      }
      // ----------------------------------------------------------------------

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

      // to prevent internal chans from getting added twice
      std::unordered_set<std::string> chans = {};

      int ref = config_get_int("act.refine_steps");
      static char chan_prefix[20];
      snprintf (chan_prefix, 20, "_ch_%d_",ref);
      int len = strlen(chan_prefix);

      nfc.push_back(g.name_from_chan);

      for ( auto m : nfc ) {
        for ( auto id : m ) {
          const char *channame = (id.second)->getName();
          ValueIdx *vx = p->CurScope()->LookupVal (channame);
          Assert (vx, "can't find ValueIdx in scope?");
          // TODO: there may be a better way to check for new channels..
          // Using first 3 chars for now
          if (strncmp(channame, chan_prefix, len) == 0
            && !chans.count(std::string(channame))) {
            list_append(_decomp_vx, vx);
            chans.insert(std::string(channame));
          }
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

  
