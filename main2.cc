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
 *************************************************************************
 */
#include <stdio.h>
#include <unistd.h>
#include <string.h>
#include <act/act.h>
#include <act/iter.h>
#include <act/passes.h>
#include "engines.h"
#include "synth.h"
#include <chrono>
using namespace std::chrono;

static void usage(char *name)
{
  fprintf (stderr, "Usage: %s [-OXh] [-C <circuit family>] [-e <file>] [-o <file>] [-E abc|yosys|genus] -p <proc> <actfile>\n", name);
  fprintf (stderr, "Options:\n");
  fprintf (stderr, " -h : help; display this message\n");
  fprintf (stderr, " -p <proc> : name of the ACT process to be translated (the top-level process).");
  fprintf (stderr, " -O : optimize CHP\n");
  fprintf (stderr, " -F checkchp : this can be used to check CHP synthesis support\n");
  fprintf (stderr, " -F dataflow|sdt|ring|decomp : synthesis output format\n");
  fprintf (stderr, "        * dataflow : dataflow output\n");
  fprintf (stderr, "        * sdt : syntax-directed translation prs output\n");
  fprintf (stderr, "        * ring : ring-based synthesis prs output\n");
  fprintf (stderr, "        * decomp : decomposition - CHP-to-CHP\n");
  fprintf (stderr, " -C : qdi|bd|bd2|bdp|di|ditest: Circuit / Datapath family\n");
  fprintf (stderr, "        * qdi : quasi delay insensitive (default) [w.i.p. for ring] \n");
  fprintf (stderr, "        * bd : bundled data (4-phase, D flip-flops) \n");
  fprintf (stderr, "        * bd2 : bundled data (2-phase, pulsed latches) [only ring]\n");
  fprintf (stderr, "        * bdp : bundled data (4-phase, pulsed latches) [only ring]\n");
  fprintf (stderr, "        * di : delay insensitive [only ring]\n");
  fprintf (stderr, "        * ditest : delay insensitive - testing for signal forks with extra buffers - not synthesizable [only ring]\n");
  fprintf (stderr, " -d : dataflow synthesis [deprecated, use '-F dataflow']\n");
  fprintf (stderr, " -m <int> : delay bloat percentage for ring synthesis (default 100) \n");
  fprintf (stderr, " -X : Enable projection during decomposition (w.i.p.) \n");
  fprintf (stderr, " -P <double> : Set cycle time target in picoseconds for projection (w.i.p.) (default=inf)\n");
  fprintf (stderr, " -e <file> : save expressions synthesized into <file> [default: expr.act]\n");
  fprintf (stderr, " -o <file> : save output to <file> [default: print to screen]\n");
  fprintf (stderr, " -E abc|yosys|genus : select external logic optimization engine for datapath generation\n");
  fprintf (stderr, " -t : print tool runtime breakdown (for decomp: also produce delay annotation file and dot graphs of DDG and TG)\n");
  fprintf (stderr, " -cnf=<custom.conf> : load your custom config file\n");
  fprintf (stderr, " -T<tech> : load your tech config\n");
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
  bool dpath_di = false;
  bool dpath_ditest = false;
  bool dpath_bd_2phase = false;
  bool dpath_bd_pulsed = false;
  bool non_ssa = false;
  char *exprfile = NULL;
  FILE *dfile;
  char *syntesistool = NULL;
  int external_opt = 0;
  int delay_margin = 100;
  double cycle_time_target = 1000000.0;
  bool dflow = false;

  auto start = high_resolution_clock::now();
  /* initialize ACT library */
  Act::Init(&argc, &argv);

  char *outfile = NULL;
  char *procname = NULL;

  bool use_ring = false;
  bool arb = true;
  bool project = false;
  bool run_time = false;
  bool check_chp = false;

  int ch;
  while ((ch = getopt (argc, argv, "htOXde:E:o:p:F:C:m:P:")) != -1) {
    switch (ch) {
    case 'F':
      if (!strcmp (optarg, "dataflow")) {
	dflow = true;
      }
      else if (!strcmp (optarg, "ring")) {
	use_ring = true;
	arb = false;
      }
      else if (!strcmp (optarg, "sdt")) {
	use_ring = false;
	dflow = false;
      }
      else if (!strcmp (optarg, "decomp")) {
	decompose = true;
	arb = false;
      }
      else if (!strcmp (optarg, "checkchp")) {
	check_chp = true;
      }
      else {
	fprintf (stderr, "Unknown synthesis output format: %s\n", optarg);
	usage (argv[0]);
      }
      break;
    case 'C':
      if (!strcmp (optarg, "qdi")) {
  bundled = false;
      }
      else if (!strcmp (optarg, "bd")) {
	bundled = true;
      }
      else if (!strcmp (optarg, "bd2")) {
	bundled = true;
  dpath_bd_2phase = true;
      }
      else if (!strcmp (optarg, "bdp")) {
	bundled = true;
  dpath_bd_pulsed = true;
      }
      else if (!strcmp (optarg, "di")) {
	dpath_di = true;
      }
      else if (!strcmp (optarg, "ditest")) {
	dpath_ditest = true;
      }
      else {
	fprintf (stderr, "Unknown circuit/datapath family: %s\n", optarg);
	usage (argv[0]);
      }
      break;

    case 't':
      run_time = true;
      unlink("decomp_sim.conf");
      break;

    case 'X':
      project = true;
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

    case 'm':
      delay_margin = std::atoi(Strdup (optarg));
      if (delay_margin < 100) {
        warning ("Delay multiplier < 1.0, this is not recommended!");
      }
      break;

    case 'P':
      cycle_time_target = std::atof(Strdup (optarg));
      break;
    
    default:
      fprintf (stderr, "Unknown option: %c\n", ch);
      usage (argv[0]);
      break;
    }
  }

  if (optind != argc - 1) {
    fprintf (stderr, "1 positional argument (<actfile>) required found %d\n", argc - optind);
    usage (argv[0]);
  }

  if (!procname) {
    fprintf (stderr, "Missing process name: use -p to specify it.\n");
    usage (argv[0]);
  }

  if (!check_chp) {
    if (dflow && use_ring) {
      fprintf (stderr, "Please select either dataflow or ring output, not both!");
      usage (argv[0]);
    }

    if (use_ring && non_ssa && bundled && !(dpath_bd_pulsed)) {
      fprintf (stderr, "Non-SSA style bundled datapath not supported with DFFs");
      usage (argv[0]);
    }

    if (dflow) {
      if (external_opt || exprfile || bundled) {
	fprintf (stderr, "Cannot specify dataflow generation + expression optimizations\n");
	usage (argv[0]);
      }
    }
  }

  /* read in the ACT file */
  a = new Act(argv[optind]);

  /* expand it */
  a->Expand();

  /* find the process specified on the command line */
  Process *p = a->findProcess(procname, true);
  /* read synthesis configuration file */
  config_read("synth.conf");

  if (!p) {
    fatal_error("Could not find process `%s' in file `%s'", procname,
		argv[optind]);
  }

  if (!p->isExpanded()) {
    p = p->Expand (ActNamespace::Global(), p->CurScope(), 0, NULL);
  }
  Assert (p, "What?");
  auto mid = high_resolution_clock::now();

  ActDynamicPass *c2p = new ActDynamicPass (a, "synth", "libactchp2prspass.so", "synthesis");

  if (!c2p || (c2p->loaded() == false)) {
    fatal_error ("Could not load dynamic pass!");
  }

  if (!exprfile && !dflow) {
    exprfile = Strdup ("expr.act");
  }

  if (check_chp) {
    // pretend ring
    syntesistool = Strdup("abc");
    c2p->setParam ("engine", (void *) gen_checker_engine);
    c2p->setParam ("prefix", (void *) Strdup ("checker"));
    c2p->setParam ("expr", (void *) "/dev/null");

    /* input/output options */
    c2p->setParam ("in", (void *) argv[optind]);
    c2p->setParam ("out", (void *) "/dev/null");

    /* optimization options */
    c2p->setParam ("chp_optimize", false);

    /* arbiter pass switch */
    c2p->setParam ("run_arb_pass", true);
    c2p->run (p);
    return 0;
  }

  if (dflow) {
    c2p->setParam ("engine", (void *) gen_df_engine);
    c2p->setParam ("prefix", (void *)Strdup ("df"));
  }
  else {
    if (use_ring) {
      if (!syntesistool) syntesistool = Strdup("abc");
      c2p->setParam ("engine", (void *) gen_ring_engine);
      c2p->setParam ("prefix", (void *)Strdup ("ring"));
      c2p->setParam ("delay_margin", delay_margin);
      c2p->setParam ("datapath_style", non_ssa);
      c2p->setParam ("run_time", run_time);
      c2p->setParam ("externopt_toolname", (void *)Strdup (syntesistool));
    }
    else if (decompose) {
      if (!syntesistool) syntesistool = Strdup("abc");
      c2p->setParam ("engine", (void *) gen_decomp_engine);
      c2p->setParam ("prefix", (void *)Strdup ("decomp"));
      c2p->setParam ("cycle_time_target", cycle_time_target);
      c2p->setParam ("project", project);
      c2p->setParam ("run_time", run_time);
      c2p->setParam ("externopt_toolname", (void *)Strdup (syntesistool));
    }
    else {
      c2p->setParam ("engine", (void *) gen_sdt_engine);
      c2p->setParam ("prefix", (void *)Strdup ("sdt"));
    }

    c2p->setParam ("expr", (void *) exprfile);
    c2p->setParam ("externopt", external_opt);
    c2p->setParam ("bundled_dpath", bundled);
    c2p->setParam ("bundled_dpath_2phase", dpath_bd_2phase);
    c2p->setParam ("bundled_dpath_pulsed", dpath_bd_pulsed);
    c2p->setParam ("di_dpath", dpath_di);
    c2p->setParam ("ditest_dpath", dpath_ditest);
  }

  /* input/output options */
  c2p->setParam ("in", (void *) argv[optind]);
  c2p->setParam ("out", (void *) outfile);

  /* optimization options */
  c2p->setParam ("chp_optimize", chpopt);

  /* arbiter pass switch */
  c2p->setParam ("run_arb_pass", arb);
  
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
    c2p->setParam ("externopt_toolname", syntesistool);
  }
  auto mid2 = high_resolution_clock::now();
  c2p->run (p);

  auto stop = high_resolution_clock::now();
  auto duration = duration_cast<microseconds>(stop - start);
  auto duration1 = duration_cast<microseconds>(mid - start);
  auto duration2 = duration_cast<microseconds>(stop - mid2);
  if (run_time) {
    fprintf(stdout, "\n// ---------- Top-level Runtimes ---------- ");
    fprintf(stdout, "\n// Total            : %-8lld microseconds", duration.count());
    fprintf(stdout, "\n// ACT Core Library : %-8lld microseconds", duration1.count());
    fprintf(stdout, "\n// Synthesis Pass   : %-8lld microseconds", duration2.count());
    fprintf(stdout, "\n");
  }

  return 0;
}
