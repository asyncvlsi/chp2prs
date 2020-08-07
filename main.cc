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
  fprintf(stderr, "Usage: %s <actfile> <process> <outfile> [--optimize] [--bundled]\n", name);
  exit(1);
}

int main(int argc, char **argv)
{
  Act *a;
  char *proc;
  bool chpopt = false;
  bool bundled = false;

  /* initialize ACT library */
  Act::Init(&argc, &argv);

  /* some usage check */
  if (argc < 4 || argc > 6)
  {
    usage(argv[0]);
  }
    
  /* check if optimize */
  if ((argc == 5 && strcmp(argv[4], "--optimize") == 0)
       || (argc == 6 && strcmp(argv[5], "--optimize") == 0)) {
    chpopt = true;
    //printf("> Sequencer Optimization turned ON\n");
  }
  else
  {
    //printf("> Sequencer Optimization turned OFF\n");
  }
  
  /* check if bundled data */
  if ((argc == 5 && strcmp(argv[4], "--bundled") == 0)
       || (argc == 6 && strcmp(argv[5], "--bundled") == 0))
  {
    bundled = true;
    //printf("> Bundled data turned ON\n");
  }
  else
  {
    //printf("> Bundled data turned OFF\n");
  }
  
  /* read in the ACT file */
  a = new Act(argv[1]);

  /* expand it */
  a->Expand();

  /* read configuration file, if any */
  config_read("prs2net.conf");

  /* find the process specified on the command line */
  Process *p = a->findProcess(argv[2]);

  if (!p)
  {
    fatal_error("Could not find process `%s' in file `%s'", argv[2], argv[1]);
  }

  if (!p->isExpanded())
  {
    fatal_error("Process `%s' is not expanded.", argv[2]);
  }

  /* extract the chp */
  if (p->getlang() == NULL || p->getlang()->getchp() == NULL)
  {
    fatal_error("Input file `%s' does not have any chp.", argv[2]);
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
  BasicSDT *sdt = new BasicSDT(bundled, chpopt, argv[3]);
  sdt->run_sdt (p);

  return 0;
}
