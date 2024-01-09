/*************************************************************************
 *
 *  Copyright (c) 2023 Rajit Manohar
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
#include <stdio.h>
#include <unistd.h>
#include <string.h>
#include <act/act.h>
#include <act/iter.h>
#include <act/passes.h>
#include "config_pkg.h"
#include "synth.h"
#include "basicsdt.h"

#include "opt/chp-opt.h"
#include "opt/static-tokens.h"
#include "opt/sequencers.h"
#include "opt/ir-to-dataflow.h"

#ifdef FOUND_expropt
#include "externoptsdt.h"
#endif

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

    if (chpopt) {
      // convert this to chpgraph format
      // run optimizations
      // convert back to ACT data structures
      if (p->getlang() && p->getlang()->getchp()) {
	auto g = ChpOptimize::chp_graph_from_act (p->getlang()->getchp()->c,
						  p->CurScope ());
       	ChpOptimize::optimize_chp_O2 (g.graph, p->getName(), false);
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
    }
    pp_flush (_pp);
    fprintf (_pp->fp, "/* start sdt */\n");
    fflush (_pp->fp);

    if (externopt) {
#if defined(FOUND_expropt) && defined (FOUND_abc)
      sdt = new ExternOptSDT (bundled, chpopt, _pp->fp, _ename,
                              use_yosys == 1 ? yosys :
			      (use_yosys == 0 ? genus : abc ));
      _pending = sdt;
#else
      fatal_error ("External optimization package not installed.");
#endif
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

class DFSynth : public ActSynthesize {
 public:
  DFSynth (const char *prefix,
	   char *infile,
	   char *outfile,
	   char *exprfile)
    : ActSynthesize (prefix, infile, outfile, exprfile) { }
  
  void emitTopImports(ActPass *ap) {
    pp_printf_raw (_pp, "import dflow::multi;\n\n");
    fclose (_expr);
    _expr = NULL;
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
      
      putIntoNewStaticTokenForm (g.graph);
      uninlineBitfieldExprsHack (g.graph);
      auto d = chp_to_dataflow(g.graph);
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


ActSynthesize *_gen_engine (const char *prefix,
			    char *infile,
			    char *outfile,
			    char *exprfile)

{
  return new SDTSynth (prefix, infile, outfile, exprfile);
}

ActSynthesize *_gen_engine_dflow (const char *prefix,
				  char *infile,
				  char *outfile,
				  char *exprfile)

{
  return new DFSynth (prefix, infile, outfile, NULL);
}


static void usage(char *name)
{
  fprintf (stderr, "Usage: %s [-Obdh] [-e <file>] [-o <file>] [-E abc|yosys|genus] -p <proc> <actfile>\n", name);
  fprintf (stderr, "Options:\n");
  fprintf (stderr, " -d : generate dataflow output\n");
  fprintf (stderr, " -O : optimize CHP\n");
  fprintf (stderr, " -b : bundled-data datapath\n");
  fprintf (stderr, " -h : display this usage message\n");
  fprintf (stderr, " -e <file> : save expressions synthesized into <file> [default: expr.act]\n");
  fprintf (stderr, " -o <file> : save output to <file> [default: print to screen]\n");
  fprintf (stderr, "-E abc|yosys|genus : select external logic optimization engine for datapath generation\n");
  fprintf (stderr, "\n");
  exit(1);
}

int main(int argc, char **argv)
{
  Act *a;
  char *proc;
  bool chpopt = false;
  bool bundled = false;
  char *exprfile = NULL;
  char *syntesistool = NULL;
  int external_opt = 0;
  bool dflow = false;

  /* initialize ACT library */
  Act::Init(&argc, &argv);

  char *outfile = NULL;
  char *procname = NULL;

  int ch;
  while ((ch = getopt (argc, argv, "hdObe:E:o:p:")) != -1) {
    switch (ch) {
    case 'h':
      usage (argv[0]);
      break;

    case 'p':
      if (procname) {
	FREE (procname);
      }
      procname = Strdup (optarg);
      break;
      
    case 'd':
      dflow = true;
      break;
      
    case 'o':
      if (outfile) {
	FREE (outfile);
      }
      outfile = Strdup (optarg);
      break;

    case 'O':
      chpopt = true;
      break;
      
    case 'b':
      bundled = true;
      break;
      
    case 'e':
      if (exprfile) {
        FREE (exprfile);
      }
      exprfile = Strdup (optarg);
      break;
      
    case 'E':
      external_opt = 1;
      syntesistool = Strdup (optarg);
      break;
      
    default:
      usage (argv[0]);
      break;
    }
  }

  if (optind != argc - 1) {
    usage (argv[0]);
  }

  if (!procname) {
    fprintf (stderr, "Missing process name: use -p to specify it.\n");
    usage (argv[0]);
  }

  if (dflow) {
    if (external_opt || exprfile || bundled) {
      fprintf (stderr, "Cannot specify dataflow generation + expression optimizations\n");
      usage (argv[0]);
    }
  }
  
      
  /* read in the ACT file */
  a = new Act(argv[optind]);

  /* expand it */
  a->Expand();

  /* find the process specified on the command line */
  Process *p = a->findProcess(procname, true);

  if (!p) {
    fatal_error("Could not find process `%s' in file `%s'", procname,
		argv[optind]);
  }

  if (!p->isExpanded()) {
    p = p->Expand (ActNamespace::Global(), p->CurScope(), 0, NULL);
  }
  Assert (p, "What?");

  ActDynamicPass *c2p = new ActDynamicPass (a, "synth", "libactchp2prspass.so", "synthesis");

  if (!c2p || (c2p->loaded() == false)) {
    fatal_error ("Could not load dynamic pass!");
  }

  if (!exprfile && !dflow) {
    exprfile = Strdup ("expr.act");
  }

  if (dflow) {
    c2p->setParam ("engine", (void *) _gen_engine_dflow);
    c2p->setParam ("prefix", (void *)Strdup ("df"));
  }
  else {
    c2p->setParam ("prefix", (void *)Strdup ("sdt"));
    c2p->setParam ("engine", (void *) _gen_engine);

    /* sdt only */
    c2p->setParam ("expr", (void *) exprfile);
    c2p->setParam ("externopt", external_opt);
    c2p->setParam ("bundled_dpath", bundled);
  }
  c2p->setParam ("in", (void *) argv[optind]);
  c2p->setParam ("out", (void *) outfile);

  c2p->setParam ("chp_optimize", chpopt);
  
  if (external_opt) {
    int param = 0;
    if (strcmp (syntesistool, "genus") == 0) {
       param = 0;
    } 
    else if (strcmp (syntesistool, "yosys") == 0) {
       param = 1;
    }
    else if (strcmp (syntesistool, "abc") == 0) {
       param = 2;
    }
    else {
       fatal_error ("Unknown synthesis option %s", syntesistool);
    }
    c2p->setParam ("use_yosys", param);
  }
  c2p->run (p);

  return 0;
}
