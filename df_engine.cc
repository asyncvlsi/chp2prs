/*************************************************************************
 *
 *  Copyright (c) 2024 Rajit Manohar
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
#include "engines.h"
#include "opt/chp-opt.h"
#include "opt/static-tokens.h"
#include "opt/sequencers.h"
#include "opt/ir-to-dataflow.h"


static int log_2_round_up(unsigned long long n) {
    int i = 0;
    n--;
    while (n > 0) {
      n >>= 1;
      ++i;
    }
    return i;
}


class DFSynth : public ActSynthesize {
 public:
  DFSynth (const char *prefix,
	   char *infile,
	   char *outfile,
	   char *exprfile)
    : ActSynthesize (prefix, infile, outfile, exprfile) { }
  
  void emitTopImports(ActPass *ap) { 
    pp_printf_raw (_pp, "import syn;\n\n");
  }

  void emitFinal () { }
    

  bool overrideTypes() { return false; }
  void processStruct(Data *d) {
    pp_printf_raw (_pp, "/* process %s */\n", d->getName());
  }

  void runSynth (ActPass *ap, Process *p) {
    pp_printf (_pp, "/* synthesis output */");
    pp_forced (_pp, 0);

    // XXX: dflowmap bug workaround
    list_t *special_vx;
    if (p) {
      special_vx =  ActNamespace::Act()->getDecomp (p);
    }
    else {
      special_vx = NULL;
    }

    if (special_vx) {
      char ramchan[32];
      int cnt = 0;
      for (listitem_t *si = list_first (special_vx); si; si = list_next (si)) {
	for (listitem_t *li = list_first ((list_t *) list_value (si)); li;
	     li = list_next (li)) {
	  ValueIdx *vx = (ValueIdx *) list_value (li);
	  if (TypeFactory::isProcessType (vx->t)) {
	    /*
	      These are specially inserted processes during process
	      decomposition, and hence they should have pre-defined
	      translations in the library
	    */
	    Process *proc = dynamic_cast <Process *> (vx->t->BaseType());
	    Assert (proc, "Why am I here?");
	    if (strstr (proc->getName(), "ram") != 0) {
	      ValueIdx *vx_n, *vx_w;
	      vx_n = proc->CurScope()->LookupVal ("N");
	      vx_w = proc->CurScope()->LookupVal ("W");
	      Assert (vx_n && vx_w, "RAM issue?");
	      int n, w;
	      n = proc->CurScope()->getPInt (vx_n->u.idx);
	      w = proc->CurScope()->getPInt (vx_w->u.idx);
	      pp_printf (_pp, "/* ram %s : N = %d, W = %d */",
			 vx->getName(), n, w);
	      pp_forced (_pp, 0);
	      pp_printf (_pp, "chan(int<2>) _ri%d;", cnt++);
	      pp_forced (_pp, 0);
	      pp_printf (_pp, "chan(int<%d>) _ri%d;",
			 log_2_round_up (n), cnt++);
	      pp_forced (_pp, 0);
	      pp_printf (_pp, "chan(int<%d>) _ri%d;", w, cnt++);
	      pp_forced (_pp, 0);
	      pp_printf (_pp, "chan(int<%d>) _ri%d;", w, cnt++);
	      pp_forced (_pp, 0);
	      pp_printf (_pp, "_ri%d=%s.rd; _ri%d=%s.addr; _ri%d=%s.din; _ri%d=%s.dout;",
			 cnt-4, vx->getName(),
			 cnt-3, vx->getName(),
			 cnt-2, vx->getName(),
			 cnt-1, vx->getName());
	      pp_forced (_pp, 0);
	    }
	  }
	}
      }
    }
    
    pp_flush (_pp);
    fprintf (_pp->fp, "/* start dflow */\n");
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
      }
      
      putIntoNewStaticTokenForm (g.graph);
      uninlineBitfieldExprsHack (g.graph);
      auto d = chp_to_dataflow(g);
      std::vector<ActId *> res;
      act_dataflow *newd = dataflow_to_act (d, g, res, p->CurScope());

      for (auto id : res) {
	InstType *it = p->CurScope()->Lookup (id->getName());
	Assert (it, "What?");
	it->Print (_pp->fp);
	fprintf (_pp->fp, " %s;\n", id->getName());
      }
      dflow_print (_pp->fp, newd);
    }
    else {
      pp_printf_raw (_pp, "bool ___dummy;\n");
    }
    fprintf (_pp->fp, "/* end dflow */\n");
    pp_forced (_pp, 0);
  }
};

ActSynthesize *gen_df_engine (const char *prefix, char *infile,
			      char *outfile, char *exprfile)
{
  return new DFSynth (prefix, infile, outfile, NULL);
}
