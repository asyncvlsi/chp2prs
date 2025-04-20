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
#include "decomp/projection.h"
#include "decomp/pretty_print.h"
#include "decomp/multichan.h"

#include "ring/ring_else_gen.h"

#include "opt/chp-opt.h"
#include "opt/static-tokens.h"

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

    int chpopt, pll, project;
    ActDynamicPass *dp;

    dp = dynamic_cast <ActDynamicPass *> (ap);
    Assert (dp, "What?");

    chpopt = dp->getIntParam ("chp_optimize");
    pll = dp->getIntParam ("parallelism");
    project = dp->getIntParam ("project");

    if (p->getlang() && p->getlang()->getchp()) {

      fill_in_else_explicit (p->getlang()->getchp()->c, p, 1);

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

      std::vector<ActId *> tmp_names;
      std::unordered_set<ActId *> newnames;
    
      std::vector<Sequence> vs, vs1, vsx;

#if 1
      // necessary decompositions for synthesis -------------------------------
      MultiChan *mc = new MultiChan (_pp->fp, g, p->CurScope());
      mc->process_multichans();
      vs = mc->get_auxiliary_procs();

      BreakPoints *bkp = new BreakPoints (_pp->fp, g, p->CurScope(), 0);
      ChoppingBlock *cb = new ChoppingBlock (_pp->fp, g, 
                                bkp->get_decomp_info_map(), p->CurScope());
      cb->excise_internal_loops();
      vs1 = cb->get_chopped_seqs();

      Projection *pr = new Projection (_pp->fp, g, 
                          bkp->get_decomp_info_map(), p->CurScope());
      if (project) {
        pr->project();
      }
      auto vs3 = pr->get_procs();
      // vsx = pr->get_seqs();

      Block *top = g.graph.blockAllocator().newBlock(Block::makeParBlock());
      top->u_par().branches.push_back(g.graph.m_seq);

      chp_graph_to_act (g, tmp_names, p->CurScope());
      for ( auto x : tmp_names ) { newnames.insert(x); }
      act_chp_lang_t *top_chp;
      NEW (top_chp, act_chp_lang_t);
      top_chp->label = NULL;
      top_chp->space = NULL;
      top_chp->type = ACT_CHP_COMMA;
      top_chp->u.semi_comma.cmd = list_new();

      for ( auto vv : {vs, vs1} ) 
      {
        for (auto v : vv)
        {
          g.graph.m_seq = v;
          act_chp_lang_t *tmp = chp_graph_to_act (g, tmp_names, p->CurScope());
          for ( auto x : tmp_names ) { newnames.insert(x); }
          list_append(top_chp->u.semi_comma.cmd, tmp);
        }
      }
      std::vector<std::unordered_map<ChpOptimize::ChanId, ActId *>> nfc = {};
      for ( auto v : vs3 )
      {
        auto _g = ChpOptimize::chp_graph_from_act (v, p->CurScope (), 1);
        // ChpOptimize::optimize_chp_O2 (_g.graph, p->getName(), false);
        ChpOptimize::optimize_chp_O0 (_g.graph, p->getName(), false);
        std::vector<ActId *> tmp_names2;
        v = chp_graph_to_act (_g, tmp_names2, p->CurScope());
        for ( auto x : tmp_names2 ) { newnames.insert(x); }
        // for ( auto id : _g.name_from_chan ) { g.name_from_chan.insert(id); }
        nfc.push_back(_g.name_from_chan);
        list_append(top_chp->u.semi_comma.cmd, v);
      }

      // ----------------------------------------------------------------------

      // concurrent decomposition for slack elastic programs ------------------
      std::vector<Sequence> vs2;
      Block *top2 = g.graph.blockAllocator().newBlock(Block::makeParBlock());
      
      // only decomposing main program for now
      for ( auto d_seq : top->u_par().branches )
      {
        g.graph.m_seq = d_seq;

#if 0
        BreakPoints *bkp2 = new BreakPoints (_pp->fp, g, p->CurScope(), pll);
        bkp2->mark_breakpoints();
        ChoppingBlock *cb2 = new ChoppingBlock (_pp->fp, g, 
                                  bkp2->get_decomp_info_map(), p->CurScope());
        // cb2->chop_graph();
        vs2 = cb2->get_chopped_seqs();

        for (auto v : vs2)
        {
          g.graph.m_seq = v;
          act_chp_lang_t *tmp2 = chp_graph_to_act (g, tmp_names, p->CurScope());
          list_append(top_chp->u.semi_comma.cmd, tmp2);
          for ( auto x : tmp_names ) { newnames.insert(x); }
        }
#else
        act_chp_lang_t *tmp2 = chp_graph_to_act (g, tmp_names, p->CurScope());
        if (!project) {
          list_append(top_chp->u.semi_comma.cmd, tmp2);
        }
        for ( auto x : tmp_names ) { newnames.insert(x); }
#endif
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

      for (auto id : g.name_from_chan) {
        const char *channame = (id.second)->getName();
        ValueIdx *vx = p->CurScope()->LookupVal (channame);
        Assert (vx, "can't find ValueIdx in scope?");
        // TODO: there may be a better way to check for new channels..
        // Using first 3 chars for now
        // if (strncmp(channame, "_ch", 3) == 0
        if (strncmp(channame, chan_prefix, len) == 0
        && !chans.contains(std::string(channame))) {
          list_append(_decomp_vx, vx);
          chans.insert(std::string(channame));
        }
      }

      for ( auto m : nfc ) {
        for ( auto id : m ) {
          const char *channame = (id.second)->getName();
          ValueIdx *vx = p->CurScope()->LookupVal (channame);
          Assert (vx, "can't find ValueIdx in scope?");
          // TODO: there may be a better way to check for new channels..
          // Using first 3 chars for now
          // if (strncmp(channame, "_ch", 3) == 0
          if (strncmp(channame, chan_prefix, len) == 0
            && !chans.contains(std::string(channame))) {
            list_append(_decomp_vx, vx);
            chans.insert(std::string(channame));
          }
        }
      }
#else
    ChpOptimize::putIntoNewStaticTokenForm(g.graph);
    fprintf (stdout, "\n\n");
    print_chp(std::cout,g.graph);
    fprintf (stdout, "\n\n");
    ChpOptimize::takeOutOfStaticTokenForm(g.graph);

    BreakPoints *bkp = new BreakPoints (_pp->fp, g, p->CurScope(), 0);
    Projection *pr = new Projection (_pp->fp, g, 
                          bkp->get_decomp_info_map(), p->CurScope());
    pr->project();
    pr->print_subgraphs(_pp->fp);

    p->getlang()->getchp()->c = ChpOptimize::chp_graph_to_act (g, tmp_names, p->CurScope());
#endif
      
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

  
