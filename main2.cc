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

    pp_flush (_pp);
    fprintf (_pp->fp, "/* start sdt */\n");
    fflush (_pp->fp);

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

    if (chpopt)
    {
      
    }

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

ActSynthesize *_gen_engine (const char *prefix,
			    char *infile,
			    char *outfile,
			    char *exprfile)

{
  return new SDTSynth (prefix, infile, outfile, exprfile);
}

static void usage(char *name)
{
  fprintf(stderr, "Usage BasicSDT: %s [-Ob] [-e <exprfile>] <actfile> <process> <out>\n", name);
  fprintf(stderr, "Usage ExrpOptSDT: %s [-Ob] -o [<abc,yosys,genus>] [-e <exprfile>] <actfile> <process> <out>\n", name);
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

  /* initialize ACT library */
  Act::Init(&argc, &argv);

  int ch;
  while ((ch = getopt (argc, argv, "Obe:o:")) != -1) {
    switch (ch) {
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
    case 'o':
      external_opt = 1;
      syntesistool = Strdup (optarg);
      break;
    default:
      usage (argv[0]);
      break;
    }
  }

  if ( optind != argc - 3 ) {
    usage (argv[0]);
  }
      
  /* read in the ACT file */
  a = new Act(argv[optind]);

  /* expand it */
  a->Expand();

  /* find the process specified on the command line */
  Process *p = a->findProcess(argv[optind+1], true);

  if (!p)
  {
    fatal_error("Could not find process `%s' in file `%s'", argv[optind+1], argv[optind]);
  }

  if (!p->isExpanded())
  {
    //fatal_error("Process `%s' is not expanded.", argv[optind+1]);
    p = p->Expand (ActNamespace::Global(), p->CurScope(), 0, NULL);
  }
  Assert (p, "What?");

  ActDynamicPass *c2p = new ActDynamicPass (a, "synth", "libactchp2prspass.so", "synthesis");

  if (!c2p || (c2p->loaded() == false)) {
    fatal_error ("Could not load dynamic pass!");
  }

  if (!exprfile) {
    exprfile = Strdup ("expr.act");
  }

  c2p->setParam ("prefix", (void *)Strdup ("sdt"));
  c2p->setParam ("expr", (void *) exprfile);
  c2p->setParam ("out", (void *) argv[optind+2]);
  c2p->setParam ("in", (void *) argv[optind]);
  c2p->setParam ("engine", (void *) _gen_engine);

  /* specific parameters for SDT */
  c2p->setParam ("chp_optimize", chpopt);
  c2p->setParam ("externopt", external_opt);
  c2p->setParam ("bundled_dpath", bundled);
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
