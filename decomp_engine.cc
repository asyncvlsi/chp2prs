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

#include <act/chp/chopping_block.h>
#include <act/chp/projection.h>
#include <act/chp/pretty_print.h>
#include <act/chp/multichan.h>
#include <act/chp/chp_cost.h>
#include <act/chp/expr_pipe.h>

#include <act/chp/chp-opt.h>
#include <act/chp/static-tokens.h>

#include <chrono>
using namespace std::chrono;

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

    // for memories that might get instantiated
    pp_printf_raw (_pp, "import syn;\n");
    pp_printf_raw (_pp, "open syn;\n");
    pp_printf_raw (_pp, "open syn::decomp;\n");
    /* print imports */
    fprintf (_expr, "namespace syn {\n\nexport namespace expr {\n\n");
    fclose (_expr);
    _expr = NULL;
    // Header for decomp_sim.conf
    int print_rt = dp->getIntParam ("run_time");
    if (print_rt) {
      FILE *ff = fopen("decomp_sim.conf", "w");
      Assert (ff, "Could not open output file to write actsim configuration");
      fprintf(ff, "begin sim\n");
      fprintf(ff, "   begin chp\n");
      fprintf(ff, "      int debug_metrics 0 # set to 1 to see delay association in actsim\n");
      fprintf(ff, "      int detailed_delay_annotation 1\n");
      fprintf(ff, "   end\n");
      fprintf(ff, "end\n\n");
      fclose(ff);
    }
  }

  void processStruct (Data *d) {
    // some special stuff for templated pure structs
    if (TypeFactory::isPureStruct(d)) {
      int n_params = d->getRemainingParams();
      std::string dfn = d->getFullName();
      bool emit_decl = (dfn.size()>=2 && dfn.substr(dfn.size()-2,2)!="<>");
      if (emit_decl) {
        char buf[4096];
        ActNamespace::Act()->msnprintfproc (buf, 4096, d);
        pp_printf_raw (_pp, "\ndeftype %s <: %s () {}\n\n", buf, d->getFullName());
      }
    }
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

    int chpopt, project;
    double cycle_time_target;
    ActDynamicPass *dp;

    dp = dynamic_cast <ActDynamicPass *> (ap);
    Assert (dp, "What?");

    chpopt = dp->getIntParam ("chp_optimize");
    cycle_time_target = dp->getRealParam("cycle_time_target");
    project = dp->getIntParam ("project");
    const char *externopt_toolname = (char *)dp->getPtrParam("externopt_toolname");
    const char *fname = p->getFullName();
    const char *memname = config_get_string("act.decomp.mem");
    bool is_mem = !(strncmp(fname, memname, strlen(memname)));

    if (p->getlang() && p->getlang()->getchp() && !is_mem) {

      int print_rt = dp->getIntParam ("run_time");
      fprintf(stdout, "\n// %s : ",p->getName());
      fflush(stdout);

      _fill_in_else_explicit (p->getlang()->getchp()->c, p->CurScope());

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

      std::unordered_set<ActId *> newnames;
      std::vector<ActId *> tmp_names;

      if (!project) {
        // ExprPipe ep (g, p->CurScope());
        // ep.set_n_cuts(pll);
        // ep.run();
      }

      if (!ChpOptimize::isProbeFree(g.graph) && project) {
        warning ("Probes in CHP - not running projection");
        project = false;
      }
      
      auto t1 = high_resolution_clock::now();
      // necessary rewrites for ring synthesis --------------------------------
      MultiChan mc = MultiChan (g, p->CurScope());
      mc.process_multichans();
      auto vs = mc.get_auxiliary_procs();
      
      ChoppingBlock cb = ChoppingBlock (g, p->CurScope());
      cb.excise_internal_loops();
      auto vs1 = cb.get_chopped_seqs();
        
      auto btop = g.graph.newParBlock();
      for ( auto vv : {{g.graph.m_seq}, vs, vs1} ) {
        for (auto v : vv) {
          btop->u_par().branches.push_back(v);
        }
      }
      g.graph.m_seq = g.graph.newSequence({btop});
      ChpOptimize::fillInElseExplicit(g.graph);
      act_chp_lang_t *top_chp = chp_graph_to_act (g, tmp_names, p->CurScope());
      for ( auto x : tmp_names ) { newnames.insert(x); }
      // ----------------------------------------------------------------------
      auto t2 = high_resolution_clock::now();

      // projection/decomposition for slack elastic programs ------------------
      std::vector<std::unordered_map<ChpOptimize::ChanId, ActId *>> nfc = {};
      if (project) {
        Projection pr = Projection (g, p->CurScope());
        pr.project(Strategy::Timing, cycle_time_target);
        if (print_rt && ChpOptimize::isProbeFree(g.graph)) {
          pr.export_ddg_and_tg(p->getName());
        }
        auto [names2, top_chp2, nfc2] = pr.get_final_result();
        for ( auto x : names2 ) { newnames.insert(x); }
        for ( auto x : nfc2 ) { nfc.push_back(x); }
        top_chp = top_chp2;
      }
      // ----------------------------------------------------------------------

      _trim_nested_same_int (top_chp, p->CurScope());
      
      auto t3 = high_resolution_clock::now();

      auto d1 = duration_cast<microseconds>(t2 - t1);
      auto d2 = duration_cast<microseconds>(t3 - t2);
      if (print_rt) {
        fprintf(stdout, "\n// ----------- Process Runtimes ----------- ");
        fprintf(stdout, "\n// Multichan + Loop Exc. : %-8lld microseconds", d1.count());
        fprintf(stdout, "\n// Projection            : %-8lld microseconds", d2.count());
        fprintf(stdout, "\n");
      }

      if (print_rt) {
        ChpCost cc(p->CurScope(), g, std::string(externopt_toolname));
        cc.dump_actsim_conf("decomp_sim.conf", top_chp, p);
      }

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

  
