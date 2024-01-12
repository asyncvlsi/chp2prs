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

class DFSynth : public ActSynthesize {
 public:
  DFSynth (const char *prefix,
	   char *infile,
	   char *outfile,
	   char *exprfile)
    : ActSynthesize (prefix, infile, outfile, exprfile) { }
  
  void emitTopImports(ActPass *ap) {
    //pp_printf_raw (_pp, "import dflow::multi;\n\n");
  }

  void emitFinal () { }
    

  bool overrideTypes() { return false; }
  void processStruct(Data *d) {
    pp_printf_raw (_pp, "/* process %s */\n", d->getName());
  }

  void runSynth (ActPass *ap, Process *p) {
    pp_printf (_pp, "/* synthesis output */");
    pp_forced (_pp, 0);

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
