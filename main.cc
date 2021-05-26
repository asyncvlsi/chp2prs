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
#include <act/passes/netlist.h>
#include "check_chp.h"
#include "cartographer.h"
#include "config_pkg.h"

#ifdef FOUND_expropt
#include "externoptsdt.h"
#endif

#ifdef FOUND_chp_opt
#include <act/chp-opt/optimize.h>
#endif
#include "basicsdt.h"


static void usage(char *name)
{
  fprintf(stderr, "Usage BasicSDT: %s [-Ob] [-e<exprfile>] <actfile> <process> <out>\n", name);
  fprintf(stderr, "Usage ExrpOptSDT: %s [-Ob] -o[<yosys,genus>] -e<exprfile> <actfile> <process> <out>\n", name);
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
    usage (argv[0]);
  }
      
  /* read in the ACT file */
  a = new Act(argv[optind]);

  /* expand it */
  a->Expand();

  /* find the process specified on the command line */
  Process *p = a->findProcess(argv[optind+1]);

  if (!p)
  {
    fatal_error("Could not find process `%s' in file `%s'", argv[optind+1], argv[optind]);
  }

  if (!p->isExpanded())
  {
    //fatal_error("Process `%s' is not expanded.", argv[optind+1]);
    p = p->Expand (ActNamespace::Global(), p->CurScope(), 0, NULL);
    emit_import = 1;
  }
  else {
    emit_import = 0;
  }
  Assert (p, "What?");

  /* extract the chp */
  if (p->getlang() == NULL || p->getlang()->getchp() == NULL)
  {
    fatal_error("Process `%s' does not have any chp.", argv[optind+1]);
  }

  if (chpopt)
  {
#ifdef FOUND_chp_opt    
    ChpOptPass *copt = new ChpOptPass (a);
    copt->run (p);
    printf("> Optimized CHP:\n");
    chp_print(stdout, p->getlang()->getchp()->c);
    printf("\n");
#else
    fatal_error ("Optimize flag is not currently enabled in the build.");
#endif
  }

  check_chp(p);
  SDTEngine *sdt;
  if (external_opt) 
  {

#ifdef FOUND_expropt
    sdt = new ExternOptSDT(bundled, chpopt,
                    emit_import ? argv[optind] : NULL,
                    argv[optind+2],
                    exprfile,
                    strcmp(syntesistool, "genus") ? yosys : genus );
#else
    fatal_error ("External optimization package not installed!");
#endif
  }
  else
  {
    if (bundled) fatal_error("the BasicSDT flow only supports QDI currently");
    sdt = new BasicSDT(bundled, chpopt,
                    emit_import ? argv[optind] : NULL,
                    argv[optind+2]);
      sdt->mkExprBlocks (exprfile);
  }

  sdt->run_sdt (p);
  return 0;
}
