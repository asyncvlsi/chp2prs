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
#include "synth.h"
#include "sdt/basicsdt.h"
#include "sdt/externoptsdt.h"
#include "opt/chp-opt.h"
#include "engines.h"

/**
 * SDT Engine
 */

static BasicSDT *_pending = NULL;

static void kill_mapper_on_exit (void)
{
  if (_pending) {
    delete _pending;
  }
  _pending = NULL;
}


class SDTSynth : public ActSynthesize {
 public:
  SDTSynth (const char *prefix,
	    char *infile,
	    char *outfile,
	    char *exprfile)
    : ActSynthesize (prefix, infile, outfile, exprfile) {
    if (!exprfile) {
      fatal_error ("SDT synthesis: requires an expression file!");
    }
  }
  
  void emitTopImports(ActPass *ap) {
    ActDynamicPass *dp = dynamic_cast <ActDynamicPass *> (ap);
    Assert (dp, "Hmm");
    int bundled_data = dp->getIntParam ("bundled_dpath");

    /* print imports */
    if (bundled_data) {
      pp_printf_raw (_pp, "import \"syn/bdopt/_all_.act\";\n");
    }
    else {
      pp_printf_raw (_pp, "import \"syn/qdi/_all_.act\";\n");
    }
    pp_printf_raw (_pp, "import \"%s\";\n", _ename);
    // open the operating namespace
    pp_printf_raw (_pp, "open syn;\n");
    pp_forced (_pp, 0);
    pp_forced (_pp, 0);

    fprintf (_expr, "namespace syn {\n\nexport namespace expr {\n\n");
    fclose (_expr);
    _expr = NULL;
  }

  void typeInt (char *buf, int sz, int bitwidth) {
    snprintf (buf, sz, "syn::sdtvar<%d>", bitwidth);
  }
  void typeBool (char *buf, int sz) {
    snprintf (buf, sz, "syn::sdtboolvar");
  }
  void typeIntChan (char *buf, int sz, int bitwidth) {
    snprintf (buf, sz, "syn::sdtchan<%d>", bitwidth);
  }
  void typeBoolChan (char *buf, int sz) {
    snprintf (buf, sz, "syn::sdtboolchan");
  }

  bool chpopt_option;

  void runPreSynth (ActPass *ap, Process *p) {
    ActDynamicPass *dp = dynamic_cast <ActDynamicPass *> (ap);
    Assert (dp, "What?");
    chpopt_option = dp->getIntParam ("chp_optimize");
  }


  bool skipOverride (ValueIdx *vx) {
    if (!chpopt_option) {
      return false;
    }
    
    if (TypeFactory::isIntType (vx->t) ||
	TypeFactory::isBoolType (vx->t) ||
	TypeFactory::isStructure (vx->t)) {
      return true;
    }
    return false;
  }

  void runSynth (ActPass *ap, Process *p) {
    pp_printf (_pp, "/* synthesis output */");
    pp_forced (_pp, 0);


    int chpopt, externopt, bundled;
    BasicSDT *sdt;
    int use_yosys;
    ActDynamicPass *dp;

    dp = dynamic_cast <ActDynamicPass *> (ap);
    Assert (dp, "What?");

    chpopt = dp->getIntParam ("chp_optimize");
    externopt = dp->getIntParam ("externopt");
    bundled = dp->getIntParam ("bundled_dpath");
    use_yosys = dp->getIntParam ("use_yosys");

    // convert this to chpgraph format
    // run optimizations
    // convert back to ACT data structures
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
      for (auto id : newnames) {
	InstType *it = p->CurScope()->Lookup (id->getName());
	if (TypeFactory::isBoolType (it)) {
	  pp_printf (_pp, "syn::sdtboolvar %s;", id->getName());
	  pp_forced (_pp, 0);
	}
	else {
	  pp_printf (_pp, "syn::sdtvar<%d> %s;",
		     TypeFactory::bitWidth (it), id->getName());
	  pp_forced (_pp, 0);
	}
      }
    }
    pp_flush (_pp);
    fprintf (_pp->fp, "/* start sdt */\n");
    fflush (_pp->fp);

    if (externopt) {
      sdt = new ExternOptSDT (bundled, chpopt, _pp->fp, _ename,
                              use_yosys == 1 ? yosys :
			      (use_yosys == 0 ? genus : abc ));
      _pending = sdt;
    }
    else {
      if (bundled) {
	fatal_error ("Bundled-data not supported is Basic mode");
      }
      sdt = new BasicSDT (bundled, chpopt, _pp->fp, _ename);
      _pending = sdt;
    }

    atexit (kill_mapper_on_exit);
    sdt->run_sdt (p, 0);
    kill_mapper_on_exit ();

    fprintf (_pp->fp, "/* end sdt */\n");
    
    pp_forced (_pp, 0);
  }
};

ActSynthesize *gen_sdt_engine (const char *prefix,  char *infile,
			       char *outfile, char *exprfile)
{
  return new SDTSynth (prefix, infile, outfile, exprfile);
}
