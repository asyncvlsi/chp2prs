/*************************************************************************
 *
 *  This file is part of the ACT library
 *
 *  Copyright (c) 2023 Karthi Srinivasan
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
#include "synth.h"


class RingSynth : public ActSynthesize {
 public:
  RingSynth (const char *prefix,
	    char *infile,
	    char *outfile,
	    char *exprfile = NULL)
    : ActSynthesize (prefix, infile, outfile, exprfile) { }
  
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
    snprintf (buf, sz, "bd_int<%d>", bitwidth);
  }
  void typeBool (char *buf, int sz) {
    fatal_error ("bools not supported, use int<1> instead");
  }
  void typeIntChan (char *buf, int sz, int bitwidth) {
    snprintf (buf, sz, "bd<%d>", bitwidth);
  }
  void typeBoolChan (char *buf, int sz) {
    fatal_error ("bool chans not supported, use bd<1> instead");
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
      ActPass *opt_p = dp->getAct()->pass_find ("chpopt");
      if (opt_p && p->getlang()->getchp()) {
	opt_p->run (p);
	printf("> Optimized CHP:\n");
	chp_print(stdout, p->getlang()->getchp()->c);
	printf("\n");
      }
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

    // core synthesis functions here
    act_chp_lang_t *c = p->getlang()->getchp()->c;
    Assert (c, "hmm c");
    mangle_init();
    Hashtable *hvi = construct_var_info_hashtable (p->getlang()->getchp()->c, p);
    // print_var_info_hashtable (hvi);
    print_refine_body(_pp->fp, p, c, hvi);
    pp_flush (_pp);

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

    // p->Print (stdout);
    lang = p->getlang();
    chp = lang->getchp();

    if (chp)    { chp_lang = chp->c; }
    else        { fatal_error("No chp body"); }

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

    // Optimization Step ------------------------
    // int tmp=check_if_pipeable(chp_lang, p, 1);
    // fprintf (fp_out, "%d", tmp);

    // NOTE: moved inside branched ring synthesis -----
    // generate_live_var_info (chp_lang, p, 1);
    // generate_live_var_bits (chp_lang, p, 1);
    // print_live_var_info (chp_lang, p, 1);
    // moved inside branched ring synthesis -----------

    // ActSynthesize *as = new ActSynthesize("ring_", argv[optind], argv[optind+1]);

    // as->prepSynthesis();

    fprintf (stdout, "\n gettin here");


    ActDynamicPass *rsyn = new ActDynamicPass (a, "synth", "libactrsynpass.so", "synthesis");
    
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

    // // Pre-processing 1 -------------------------

    // fill_in_else_explicit (chp_lang, p, 1);

    // // Ring Synthesis ---------------------------

    // fprintf (fp_out, "import \"%s\";\n",a_name);
    // print_headers_and_imports_expr();
    // print_headers_and_imports (fp_out, p);
    
    // mangle_init();

    // print_overrides (fp_out,p);
    // Hashtable *hvi = construct_var_info_hashtable 
    //                         (fp_out, chp_lang, p);
    // print_var_info_hashtable (hvi);
    // print_refine_body (fp_out, p, chp_lang, hvi);

    // // ------------------------------------------

    // Expr *e;
    // generate_expr_block (e,32,p,fp_out);

    // Live Variable Analysis & Ring Chopping ---
    // fprintf (fp_out, "\n\n");
    // generate_live_var_info (chp_lang, p, 1);
    // generate_live_var_bits (chp_lang, p, 1);

    // print_live_var_info (chp_lang, p, 1);

    // act_chp_lang_t *cc;
    // cc = chop_ring (chp_lang, p, 1, 1);
    // Assert ((cc), "what");

    // chp_print (stdout, cc);
    // fprintf(stdout, "\n||\n");
    // chp_print (stdout, chp_lang);
    // ------------------------------------------

    return 0;
}