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
#include "engines.h"

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
    c2p->setParam ("engine", (void *) gen_df_engine);
    c2p->setParam ("prefix", (void *)Strdup ("df"));
  }
  else {
    c2p->setParam ("prefix", (void *)Strdup ("sdt"));
    c2p->setParam ("engine", (void *) gen_sdt_engine);

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
