/*************************************************************************
 *
 *  Copyright (c) 2026 Rajit Manohar
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
#include <act/act.h>
#include <act/passes.h>
#include <act/chp/chp-opt.h>
#include <act/chp/pretty_print.h>
#include <act/chp/multichan.h>
#include <act/chp/projection.h>

extern "C" {
  void optimize_init (ActPass *ap);
  void optimize_runcmd (ActPass *ap, const char *cmd);
  void optimize_recursive (ActPass *ap, UserDef *u, int mode);
  void *optimize_proc (ActPass *ap, Process *p, int mode);
  void *optimize_data (ActPass *ap, Data *d, int mode);
  void *optimize_chan (ActPass *ap, Channel *c, int mode);
  void optimize_free (ActPass *ap, void *v);
  void optimize_done (ActPass *ap);
}

namespace {

  // what are the options?
struct OptimizeInfo {
  int chpopt;			// -O flag
  
  int project;			// enable projection-based decomp
  
  double cycle_time_target;	// timing-driven decomp, target cycle time

  char *externopt_toolname;	// name of logic synthesis tool

  int run_time;			// print runtime breakdown

  const char *mem_name;		// memory decomp name
  int mem_len;

  bool multichan_loop;		// enable multichannel and loop
				// decompositions

  bool dump_sim_conf;		// dump simulation config

  // more here as we add optimizations...

  OptimizeInfo() {
    chpopt = 0;
    project = 0;
    cycle_time_target = -1; // none
    externopt_toolname = NULL;
    run_time = 0;
    mem_name = NULL;
    multichan_loop = false;
    dump_sim_conf = false;
  }

  bool isMem (Process *p) {
    if (!config_exists ("act.decomp.mem")) {
      return false;
    }
    if (!mem_name) {
      mem_name = config_get_string ("act.decomp.mem");
      mem_len = strlen (mem_name);
    }
    char *tmp = p->getFullName();
    if (strncmp (tmp, mem_name, mem_len) == 0) {
      FREE (tmp);
      return true;
    }
    else {
      FREE (tmp);
      return false;
    }
  }
};


}


/*
 * Create and populate the Optimize structure up with parameters
 * passed to the optimize pass.
 */
void optimize_init (ActPass *ap)
{
  ActDynamicPass *dp = dynamic_cast<ActDynamicPass *> (ap);
  Assert (dp, "Hmm...");
  
  dp->setParam ("raw", (void *) NULL);
}

static void _populate_opt (ActDynamicPass *dp)
{
  if (dp->getPtrParam ("raw") != NULL) {
    return;
  }
  OptimizeInfo *o = new OptimizeInfo();
  dp->setParam ("raw", (void *) o);
  if (dp->hasParam("chp_optimize")) {
    o->chpopt = dp->getIntParam ("chp_optimize");
  }
  if (dp->hasParam("project")) {
    o->project = dp->getIntParam ("project");
  }
  if (dp->hasParam("cycle_time_target")) {
    o->cycle_time_target = dp->getRealParam ("cycle_time_target");
  }
  if (dp->hasParam ("externopt_toolname")) {
    o->externopt_toolname = (char *) dp->getPtrParam ("externopt_toolname");
  }
  if (dp->hasParam ("run_time")) {
    o->run_time = dp->getIntParam ("run_time");
  }
  if (dp->hasParam ("multichan_loop")) {
    o->multichan_loop = (dp->getIntParam("multichan_loop") != 0) ? true : false;
  }

  if (dp->hasParam ("dump_sim_conf")) {
    o->dump_sim_conf = (dp->getIntParam ("dump_sim_conf") != 0) ? true : false;
  }
  
  if (o->dump_sim_conf) {
    FILE *fp = fopen ("decomp_sim.conf", "w");
    if (!fp) {
      fatal_error ("Could not open decomp_sim.conf for writing");
    }
    fprintf (fp, R"a(
begin sim
   begin chp
     int debug_metrics 0 # set to 1 to set delay reports in actsim
     int detailed_delay_annotation 1
   end
end
)a");
    fclose (fp);
  }
}


/*------------------------------------------------------------------------
 *
 * Optimize pass commands:
 *
 *  run_passes: run standard passes: inline, memory decomposition, arb
 *              extraction
 *
 *------------------------------------------------------------------------
 */
void optimize_runcmd (ActPass *ap, const char *cmd)
{
  // now run all the passes that need to be run first: inline,
  // memdecom, ardecomp
  ActPass *gp;
  ActCHPFuncInline *ip;
  ActCHPMemory *mem;
  ActCHPArbiter *arbp;
  ActDynamicPass *dp = dynamic_cast<ActDynamicPass *>(ap);
  Assert (dp, "Hmm...");

  _populate_opt (dp);

  if (strcmp (cmd, "run_passes") == 0) {
    Process *p = (Process *) dp->getPtrParam ("toplevel");

    gp = ActNamespace::Act()->pass_find ("finline");
    if (gp) {
      ip = dynamic_cast<ActCHPFuncInline *> (gp);
    }
    else {
      ip = new ActCHPFuncInline (ActNamespace::Act());
    }
    Assert (ip, "error in inline pass");
    if (!ip->completed()) {
      ip->run (p);
    }

    gp = ActNamespace::Act()->pass_find ("chpmem");
    if (gp) {
      mem = dynamic_cast<ActCHPMemory *> (gp);
    }
    else {
      mem = new ActCHPMemory (ActNamespace::Act());
    }
    Assert (mem, "error in mem pass");
    if (!mem->completed()) {
      mem->run (p);
    }

    gp = ActNamespace::Act()->pass_find ("chparb");
    if (gp) {
      arbp = dynamic_cast<ActCHPArbiter *> (gp);
    }
    else {
      arbp = new ActCHPArbiter (ActNamespace::Act());
    }
    Assert (arbp, "error in arbiter pass");
    if (!arbp->completed()) {
      arbp->run (p);
    }
  }
  else {
    warning ("optimize pass (dynamic): ignoring command `%s'", cmd);
  }
}


/*
 * Run per-process CHP optimizations.
 *
 *  1. Decomposition into canonical form
 *  2. Optimizations
 *
 * Returns a list of new decomp vx identifiers!
 */
void *optimize_proc (ActPass *ap, Process *p, int mode)
{
  OptimizeInfo *o;
  ActDynamicPass *dp;

  
  /* return if there is no CHP here */
  if (!(p->getlang() && p->getlang()->getchp())) return NULL;

  dp = dynamic_cast<ActDynamicPass *> (ap);
  Assert (dp, "Hmm");

  _populate_opt (dp);
  
  o = (OptimizeInfo *) dp->getPtrParam ("raw");

  if (o->isMem (p)) {
    // refactored memory: we don't do anything here
    return NULL;
  }

  /* replace "else ->" with the actual guard */
  _fill_in_else_explicit (p->getlang()->getchp()->c, p->CurScope());

  /* generate the chp graph data structure */
  ChpOptimize::GraphWithChanNames g =
    ChpOptimize::chp_graph_from_act (p->getlang()->getchp()->c,
				     p->CurScope (), 1);

  if (o->chpopt) {
    // ChpOptimize::optimize_chp_O2 (g.graph, p->getName(), false);
    ChpOptimize::optimize_chp_basic2 (g.graph, p->getName(), false);
  }
  else {
    ChpOptimize::optimize_chp_basic (g.graph, p->getName(), false);
    // ChpOptimize::eliminateDeadCode (g.graph);
  }
  uninlineBitfieldExprsHack (g.graph);

  // get projection flag
  bool project = o->project;

  if (project && !ChpOptimize::isProbeFree (g.graph)) {
    warning ("%s: probes in CHP; projection request ignored",
	     p->getName());
    project = false;
  }

  auto t1 = std::chrono::high_resolution_clock::now();


  // we keep track of the new names introduced by explicit
  // decomposition here. 
  std::unordered_set<ActId *> newnames;

  act_chp_lang_t *top_chp = NULL;
  
  /*
   * Maelstrom/ring synthesis requires two decompositions:
   *    Multi-channel rewriting
   *    Internal loop excision
   */
  if (o->multichan_loop) {
    /*
      Re-write the chp graph using multichannel decomposition and
      loop excision.
    */

    // necessary decompositions for synthesis
    MultiChan mc = MultiChan (g, p->CurScope());
    mc.process_multichans();
    auto vs = mc.get_auxiliary_procs();

    // cut nested loops out
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

    /* use this to extract any newly introduced variable names */
    std::vector<ActId *> tmp_names;
    act_chp_lang_t *top_chp = chp_graph_to_act (g, tmp_names, p->CurScope());
    for (auto x : tmp_names) {
      newnames.insert (x);
    }
  }

  /* list of channels introduced */
  std::vector< std::unordered_map<ChpOptimize::ChanId, ActId *> > xnfc = {};
  
  if (project) {
    Projection pr = Projection (g, p->CurScope());

    /* timing-driven decomposition */
    pr.project (Strategy::Timing, o->cycle_time_target);

    if (top_chp) {
      /* release storage for top chp here */
    }

    auto [names, top_chp2, nfc] = pr.get_final_result();

    // update chp data and channel map structure
    xnfc = nfc;
    top_chp = top_chp2;

    // append new names introduced by projection
    for (auto x : names) {
      newnames.insert (x);
    }
  }

  // improve expr syntax
  _trim_nested_same_int (top_chp, p->CurScope ());

  // dump actsim config
  if (o->dump_sim_conf) {
    ChpCost cc(p->CurScope(), g, std::string (o->externopt_toolname));
    cc.dump_actsim_conf ("decomp_sim.conf", top_chp, p);
  }

  // replace the CHP with the decomposed version
  p->getlang()->getchp()->c = top_chp;

  list_t *decomp_vx = list_new ();

  for (auto id : newnames) {
    ValueIdx *vx = p->CurScope()->LookupVal (id->getName());
    Assert (vx, "Can't find ValueIdx in scope!");
    list_append (decomp_vx, vx);
  }

  // to prevent internal chans from getting added twice
  std::unordered_set<std::string> chans = {};
  
  int ref = config_get_int("act.refine_steps");
  static char chan_prefix[20];
  snprintf (chan_prefix, 20, "_ch_%d_",ref);
  int len = strlen(chan_prefix);

  xnfc.push_back (g.name_from_chan);
  
  for (auto m : xnfc) {
    for (auto id : m) {
      const char *channame = (id.second)->getName();
      ValueIdx *vx = p->CurScope()->LookupVal (channame);
      Assert (vx, "Can't find channel vx in scope!");
      if (strncmp (channame, chan_prefix, len) == 0 &&
	  !chans.count (std::string (channame))) {
	list_append (decomp_vx, vx);
	chans.insert (std::string (channame));
      }
    }
  }
  return decomp_vx;
}
