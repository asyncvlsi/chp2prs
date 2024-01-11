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

    int chpopt, externopt, bundled;
    int use_yosys;
    ActDynamicPass *dp;

    dp = dynamic_cast <ActDynamicPass *> (ap);
    Assert (dp, "What?");

    chpopt = dp->getIntParam ("chp_optimize");
    externopt = dp->getIntParam ("externopt");
    bundled = dp->getIntParam ("bundled_dpath");
    use_yosys = dp->getIntParam ("use_yosys");

    if (chpopt)
    {
#ifdef FOUND_chp_opt
  //     ActPass *opt_p = dp->getAct()->pass_find ("chpopt");
  //     if (opt_p && p->getlang()->getchp()) {
	// opt_p->run (p);
	// printf("> Optimized CHP:\n");
	// chp_print(stdout, p->getlang()->getchp()->c);
	// printf("\n");
  //     }
#else
    //   fatal_error ("Optimize flag is not currently enabled in the build.");
#endif
    }

    if (externopt) {
#if defined(FOUND_expropt) && defined (FOUND_abc)
    //   sdt = new ExternOptSDT (bundled, chpopt, _pp->fp, _ename,
    //                           use_yosys == 1 ? yosys :
	// 		      (use_yosys == 0 ? genus : abc ));
    //   _pending = sdt;
#else
      fatal_error ("External optimization package not installed.");
#endif
    }
    else {
      if (bundled) {
	// fatal_error ("Bundled-data not supported is Basic mode");
      }
    //   sdt = new BasicSDT (bundled, chpopt, _pp->fp, _ename);
    //   _pending = sdt;
    }

    act_chp_lang_t *c = p->getlang()->getchp()->c;  
    // core synthesis functions here
    Assert (c, "hmm c");
    mangle_init();
    fill_in_else_explicit (c, p, 1);

    ActBooleanizePass *b = (ActBooleanizePass *) dp->getPass("booleanize");
    Assert (b, "hmm b");

    RingForge *rf = new RingForge (_pp->fp, p, c, b, "");

    rf->run_forge();
    
    /*
    auto cg = ChpOptimize::chp_graph_from_act (c, p->CurScope());
    ChpOptimize::optimize_chp_O2 (cg.graph, p->getName(), false);
    ChpOptimize::putIntoNewStaticTokenForm (cg.graph);
    ChpOptimize::uninlineBitfieldExprsHack (cg.graph);
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
    pp_flush (_pp);
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
  fprintf(stderr, "Usage: %s [-d] <actfile> <process> <output file>", name);
  exit(1);
}

int main (int argc, char **argv)
{
    Act *a;
    Process *p;
    FILE *fp_out;
    int debug = 0;
    act_languages *lang;
    act_chp *chp;
    act_chp_lang_t *chp_lang;

    Act::Init (&argc, &argv);

    const char *a_name;

    int ch;

    while ((ch = getopt (argc, argv, "d")) != -1) {
    switch (ch) {
    case 'd':
        debug = 1;
        break;
    default:
        usage (argv[0]);
        break;
        }
    }

    if ( optind != argc - 3 ) {
        usage (argv[0]);
    }

    a = new Act (argv[optind]);
    a_name = argv[optind];
    a->Expand();

    p = a->findProcess (argv[optind+1]);
    fp_out = fopen (argv[optind+2], "w");

    if (!fp_out) {
        fatal_error ("Could not open %s for writing", argv[optind+2]);
    }

    if (!p) {
        fatal_error ("Process not found");
    }

    p = p->Expand(ActNamespace::Global(), p->CurScope(),0,NULL);
    Assert (p, "Process expand failed - what?");

    if (debug == 1) 
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
    char *exprfile = Strdup ("expr.act");

    rsyn->setParam ("prefix", (void *)Strdup ("ring"));
    rsyn->setParam ("expr", (void *) exprfile);
    rsyn->setParam ("out", (void *) argv[optind+2]);
    rsyn->setParam ("in", (void *) argv[optind]);
    rsyn->setParam ("engine", (void *) _gen_engine);

    rsyn->setParam ("bundled_dpath", 1);

    rsyn->run (p);

    return 0;
}