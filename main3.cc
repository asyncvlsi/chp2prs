/*************************************************************************
 *
 *  This file is part of the ACT library
 *
 *  Copyright (c) 2023-2024 Karthi Srinivasan
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

#include "src_ring_synth/reqs.h"
#include "src_ring_synth/ring_else_gen.h"
#include "src_ring_synth/ring_forge.h"

#include "opt/chp-opt.h"
#include "opt/static-tokens.h"
#include "opt/sequencers.h"
#include "opt/ir-to-dataflow.h"

#include "synth.h"


class RingSynth : public ActSynthesize {
 public:
  RingSynth (const char *prefix,
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
    int bundled_data = dp->getIntParam ("bundled_dpath");

    /* print imports */
    if (bundled_data) {
      // pp_printf_raw (_pp, "import \"syn/bdopt/_all_.act\";\n");
      pp_printf_raw (_pp, "import \"true_pipe_c_brs_bd.act\";\n");
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
    snprintf (buf, sz, "bd_int<%d>", bitwidth);
  }
  void typeBool (char *buf, int sz) {
    fatal_error ("bools not supported, use int<1> instead");
  }
  void typeIntChan (char *buf, int sz, int bitwidth) {
    snprintf (buf, sz, "bd<%d>", bitwidth);
  }
  void typeBoolChan (char *buf, int sz) {
    fatal_error ("bool chans not supported, use chan(int<1>) instead");
  }

  void runSynth (ActPass *ap, Process *p) {
    pp_printf (_pp, "/* synthesis output */");
    pp_forced (_pp, 0);

    pp_flush (_pp);
    fprintf (_pp->fp, "/* start rsyn */\n");
    fflush (_pp->fp);

    int chpopt, bundled;
    ActDynamicPass *dp;

    dp = dynamic_cast <ActDynamicPass *> (ap);
    Assert (dp, "What?");

    chpopt = dp->getIntParam ("chp_optimize");
    bundled = dp->getIntParam ("bundled_dpath");

    if (1) { //opt
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
          // pp_printf (_pp, "syn::sdtboolvar %s;", id->getName());
          // pp_forced (_pp, 0);
          fatal_error ("no bools");
        }
        else {
          pp_printf (_pp, "bd_int<%d> %s;",
              TypeFactory::bitWidth (it), id->getName());
          pp_forced (_pp, 0);
          p->CurScope()->Add (id->getName(), it);
        }
      }
    }
    }

#if defined(FOUND_expropt) && defined (FOUND_abc)

#else
    fatal_error ("External optimization package not installed.");
#endif

    act_chp_lang_t *c = p->getlang()->getchp()->c;  
    chp_print (stdout, c);
    // core synthesis functions here
    Assert (c, "hmm c");
    mangle_init();
    fill_in_else_explicit (c, p, 1);

    ActBooleanizePass *b = (ActBooleanizePass *) dp->getPass("booleanize");
    b->run(p);

    fprintf (stdout, "\n");
    p->CurScope()->Print(stdout);
    fprintf (stdout, "\n");
    Assert (b, "hmm b");

    RingForge *rf = new RingForge (_pp->fp, p, c, b, "");

    rf->run_forge();
    
    /*
    ChpOptimize::putIntoNewStaticTokenForm (cg.graph);
    auto d = ChpOptimize::chp_to_dataflow(cg.graph);

    std::vector<ActId *> res;
    act_dataflow *newd = dataflow_to_act (d, cg, res, p->CurScope());

    fprintf (stdout, "\n");
    fprintf (stdout, "%lu", size(d));
    fprintf (stdout, "\n\n");
    dflow_print (stdout, newd);
    */
    fprintf (_pp->fp, "/* end rsyn */\n");
    
    pp_forced (_pp, 0);
  }
};

ActSynthesize *_gen_engine (const char *prefix,
			    char *infile,
			    char *outfile,
			    char *exprfile)

{
  return new RingSynth (prefix, infile, outfile, exprfile);
}


static void usage(char *name)
{
  fprintf(stderr, "Usage: %s [-hdO] [-e <file>] [-o <file>] -p <proc> <actfile>\n", name);
  fprintf (stderr, "Options:\n");
  fprintf (stderr, " -O : optimize CHP\n");
  fprintf (stderr, " -h : display this usage message\n");
  fprintf (stderr, " -e <file> : save expressions synthesized into <file> [default: expr.act]\n");
  fprintf (stderr, " -o <file> : save output to <file> [default: print to screen]\n");
  exit(1);
}

int main (int argc, char **argv)
{
    Act *a;
    Process *p;
    char *proc = NULL;
    char *outfile = NULL;
    char *exprfile = NULL;
    bool chpopt = false;
    bool debug = false;
    act_languages *lang;
    act_chp *chp;
    act_chp_lang_t *chp_lang;

    Act::Init (&argc, &argv);

    int ch;
    while ((ch = getopt (argc, argv, "hdOe:o:p:")) != -1) {
      switch (ch) {
      case 'h':
        usage (argv[0]);
        break;

      case 'p':
        if (proc) {
        FREE (proc);
        }
        proc = Strdup (optarg);
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
        
      case 'e':
        if (exprfile) {
        FREE (exprfile);
        }
        exprfile = Strdup (optarg);
        break;

      case 'd':
        debug = true;
        break;
        
      default:
        usage (argv[0]);
        break;
      }
    }

    if ( optind != argc - 1 ) {
        usage (argv[0]);
    }

    if (!proc) {
      fprintf (stderr, "Missing process name: use -p to specify it.\n");
      usage (argv[0]);
    }

    a = new Act (argv[optind]);
    a->Expand();

    p = a->findProcess (proc, true);

    if (!p) {
        fatal_error ("Process not found");
    }

    p = p->Expand(ActNamespace::Global(), p->CurScope(),0,NULL);
    Assert (p, "Process expand failed - what?");

    if (debug) 
    {
        fprintf(stdout,"\n\n------------------------------------------------------------");
        fprintf(stdout,"\n\nBegin debugging print..");
        fprintf(stdout,"\n\n------------------------------------------------------------");
        fprintf (stdout, "\n\nProcess Scope:\n");
        p->CurScope()->Print (stdout);
        fprintf (stdout, "\n\nGlobal Namespace:\n");
        ActNamespace::Global()->Print(stdout);
        fprintf(stdout,"\n\n------------------------------------------------------------");
        fprintf(stdout,"\n\nOriginal CHP: \n");
        // _chp_pretty_print(stdout,chp_lang,0);
        fprintf(stdout,"\n\n------------------------------------------------------------\n\n");
        fprintf(stdout,"\n\nEnd debugging print..");
        fprintf(stdout,"\n\n------------------------------------------------------------");
    }

    ActDynamicPass *rsyn = new ActDynamicPass (a, "synth", "libactchp2prspass.so", "synthesis");
    
    if (!rsyn || (rsyn->loaded() == false)) {
    fatal_error ("Could not load dynamic pass!");
    }
    
    if (!exprfile) {
      exprfile = Strdup ("expr.act");
    }

    rsyn->setParam ("prefix", (void *)Strdup ("ring"));
    rsyn->setParam ("expr", (void *) exprfile);
    rsyn->setParam ("out", (void *) outfile);
    rsyn->setParam ("in", (void *) argv[optind]);
    rsyn->setParam ("engine", (void *) _gen_engine);

    rsyn->setParam ("chp_optimize", chpopt);
    rsyn->setParam ("bundled_dpath", true);

    rsyn->run (p);

    return 0;
}