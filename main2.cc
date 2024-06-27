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
  fprintf (stderr, "Usage: %s [-OXRbh] [-d <file>] [-e <file>] [-o <file>] [-E abc|yosys|genus] -p <proc> <actfile>\n", name);
  fprintf (stderr, "Options:\n");
  fprintf (stderr, " -h : help; display this message\n");
  fprintf (stderr, " -O : optimize CHP\n");
  fprintf (stderr, " -X : decompose CHP and stop [for now]\n");
  fprintf (stderr, " -F dataflow|sdt|ring : synthesis output format\n");
  fprintf (stderr, "        * dataflow : dataflow output\n");
  fprintf (stderr, "        * sdt : syntax-directed translation prs output\n");
  fprintf (stderr, "        * ring : ring-based synthesis prs output [implies bundled data]\n");
  fprintf (stderr, "        * decomp : decomposition - CHP-to-CHP\n");
  fprintf (stderr, " -R : synthesize with ring approach [deprecated, use -F ring]\n");
  fprintf (stderr, " -b : bundled-data datapath for SDT (default QDI)\n");
  fprintf (stderr, " -d : dataflow synthesis [deprecated, use '-F dataflow']\n");
  fprintf (stderr, " -m <int> : delay bloat percentage for ring synthesis (default 100) \n");
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
  bool decompose = false;
  bool bundled = false;
  char *exprfile = NULL;
  FILE *dfile;
  char *syntesistool = NULL;
  int external_opt = 0;
  int delay_margin = 100;
  bool dflow = false;

  /* initialize ACT library */
  Act::Init(&argc, &argv);

  char *outfile = NULL;
  char *procname = NULL;

  bool use_ring = false;

  int ch;
  while ((ch = getopt (argc, argv, "RhObde:E:o:p:F:m:")) != -1) {
    switch (ch) {
    case 'F':
      if (!strcmp (optarg, "dataflow")) {
	dflow = true;
      }
      else if (!strcmp (optarg, "ring")) {
	use_ring = true;
      }
      else if (!strcmp (optarg, "sdt")) {
	use_ring = false;
	dflow = false;
      }
      else if (!strcmp (optarg, "decomp")) {
	decompose = true;
      }
      else {
	fprintf (stderr, "Unknown synthesis output format: %s\n", optarg);
	usage (argv[0]);
      }
      break;
      
    case 'R':
      use_ring = true;
      bundled = true;
      break;
      
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
      
    // case 'd':
    //   if (decompfile) {
    //     FREE (decompfile);
    //   }
    //   decompfile = Strdup (optarg);
    //   break;
      
    case 'E':
      external_opt = 1;
      syntesistool = Strdup (optarg);
      break;

    case 'm':
      delay_margin = std::atoi(Strdup (optarg));
      if (delay_margin < 100) {
        warning ("Delay multiplier < 1.0, this is not recommended!");
      }
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

  if (dflow && use_ring) {
    fprintf (stderr, "Please select either dataflow or ring output, not both!");
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
    if (use_ring) {
      c2p->setParam ("engine", (void *) gen_ring_engine);
      c2p->setParam ("prefix", (void *)Strdup ("ring"));
      c2p->setParam ("delay_margin", delay_margin);
    }
    else if (decompose) {
      c2p->setParam ("engine", (void *) gen_decomp_engine);
      c2p->setParam ("prefix", (void *)Strdup ("decomp"));
    }
    else {
      c2p->setParam ("engine", (void *) gen_sdt_engine);
      c2p->setParam ("prefix", (void *)Strdup ("sdt"));
    }

    c2p->setParam ("expr", (void *) exprfile);
    c2p->setParam ("externopt", external_opt);
    c2p->setParam ("bundled_dpath", bundled);
  }

  /* input/output options */
  c2p->setParam ("in", (void *) argv[optind]);
  c2p->setParam ("out", (void *) outfile);

  /* optimization options */
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
