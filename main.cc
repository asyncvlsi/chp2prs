/*************************************************************************
 *
 *  This file is part of the ACT library
 *
 *  Copyright (c) 2018-2019 Rajit Manohar
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
  fprintf(stderr, "Usage BasicSDT: %s [-Ob] [-e <exprfile>] <actfile> <process> <out>\n", name);
  fprintf(stderr, "Usage ExrpOptSDT: %s [-Ob] -o [<abc,yosys,genus>] [-e <exprfile>] <actfile> <process> <out>\n", name);
  fprintf (stderr, "Options:\n");
  //fprintf (stderr, " -h : help; display this message\n");
  //fprintf (stderr, " -p <proc> : name of the ACT process to be translated (the top-level process).");
  fprintf (stderr, " -O : optimize CHP\n");
  fprintf (stderr, " -b : bd Circuit / Datapath family\n");
  fprintf (stderr, " -e : <exprfile>: the file to save al the optimised logic expressions in\n");
  fprintf (stderr, " -o abc|yosys|genus : select external logic optimization engine for datapath generation\n");
  fprintf (stderr, " -cnf=<custom.conf> : load your custom config file\n");
  fprintf (stderr, " -T<tech> : load your tech config\n");
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
  int emit_import = 0;
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
    fprintf (stderr, "3 positional arguments (<actfile> <process> <out>) required found %d", argc - optind);
    usage (argv[0]);
  }
      
  /* read in the ACT file */
  a = new Act(argv[optind]);

  /* expand it */
  a->Expand();

  /* find the process specified on the command line */
  Process *p = a->findProcess(argv[optind+1], true);
  config_read ("synth.conf");

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
  emit_import = 1;

  if (chpopt)
  {
    warning ("-O is ignored");
  }

  ActApplyPass *app = new ActApplyPass (a);
  
  ActCHPFuncInline *ip = new ActCHPFuncInline (a);
  ip->run (p);

  ActCHPMemory *mem = new ActCHPMemory (a);
  mem->run (p);

  ActCHPArbiter *arbp = new ActCHPArbiter (a);
  arbp->run (p);

#if 0  
  ActDynamicPass *c2p = new ActDynamicPass (a, "chp2prs", "libactchp2prspass.so", "chp2prs");
#endif
  ActDynamicPass *c2p = new ActDynamicPass (a, "synth", "libactchp2prspass.so",
					    "synthesis");
  
  if (!c2p || (c2p->loaded() == false)) {
    fatal_error ("Could not load dynamic pass!");
  }

  if (!exprfile) {
    exprfile = Strdup ("expr.act");
  }

  c2p->setParam ("engine", (void *) gen_sdt_engine);
  c2p->setParam ("prefix", (void *) Strdup ("sdt"));
  c2p->setParam ("expr", (void *)exprfile);
  c2p->setParam ("externopt", external_opt);
  c2p->setParam ("bundled_dpath", bundled);
  c2p->setParam ("in", (void *) argv[optind]);
  c2p->setParam ("out", (void *) argv[optind+2]);
  c2p->setParam ("chp_optimize", chpopt);
  if (external_opt) {
    c2p->setParam ("externopt_toolname", syntesistool);
  }
  c2p->run (p);

  return 0;
}
