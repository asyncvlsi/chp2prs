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
#include "config.h"
#include "check_chp.h"
#include "cartographer.h"
#ifdef CHP_OPTIMIZE
#include <act/chp-opt/optimize.h>
#endif

#include "basicsdt.h"


static void usage(char *name)
{
  fprintf(stderr, "Usage: %s [-Ob] [-e file] <actfile> <process> <outfile>\n", name);
  exit(1);
}

int main(int argc, char **argv)
{
  Act *a;
  char *proc;
  bool chpopt = false;
  bool bundled = false;
  char *exprfile = NULL;
  int emit_import = 0;

  /* initialize ACT library */
  Act::Init(&argc, &argv);

  int ch;
  while ((ch = getopt (argc, argv, "Obe:")) != -1) {
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
    default:
      usage (argv[0]);
      break;
    }
  }

  if (optind != argc - 3) {
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
#ifdef CHP_OPTIMIZE    
    ChpOpt::optimize(p, a->getTypeFactory());
    printf("> Optimized CHP:\n");
    chp_print(stdout, p->getlang()->getchp()->c);
    printf("\n");
#else
    fatal_error ("Optimize flag is not currently enabled in the build.");
#endif
  }

  check_chp(p);
  BasicSDT *sdt = new BasicSDT(bundled, chpopt,
			       emit_import ? argv[optind] : NULL,
			       argv[optind+2]);
  sdt->mkExprBlocks (exprfile);
  sdt->run_sdt (p);

  return 0;
}
