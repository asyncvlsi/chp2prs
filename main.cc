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
#include "config_pkg.h"

#ifdef FOUND_chp_opt
#include <act/chp-opt/optimize.h>
#endif

static void usage(char *name)
{
  fprintf(stderr, "Usage BasicSDT: %s [-Ob] [-e <exprfile>] <actfile> <process> <out>\n", name);
  fprintf(stderr, "Usage ExrpOptSDT: %s [-Ob] -o [<yosys,genus>] [-e <exprfile>] <actfile> <process> <out>\n", name);
  exit(1);
}

static void begin_sdtout (const char *output_file,
			  const char *expr_file,
			  const char *import_file,
			  int bundled_data,
			  int expropt,
			  FILE **output_stream)
{
  /* initialize the output location */ 
  if (output_file) {
    *output_stream = fopen(output_file, "w");
    if (!*output_stream) {
      fatal_error ("Could not open file `%s' for writing", output_file);
    }
  }
  else {
    *output_stream = stdout;
  }

  if (import_file) {
    fprintf (*output_stream, "import \"%s\";\n", import_file);
  }
  
  /* print imports */
  if (bundled_data) {
    if (expropt) {
      fprintf(*output_stream, "import \"syn/bdopt/_all_.act\";\n");
    }
    else {
      fprintf (*output_stream, "import \"syn/bundled.act\";\n");
    }
  }
  else {
    if (expropt) {
      fprintf(*output_stream, "import \"syn/qdiopt/_all_.act\";\n");
    }
    else {
      fprintf(*output_stream, "import \"syn/qdibasic/_all_.act\";\n");
    }
  }

  // open the operating namespace
  fprintf(*output_stream, "open syn;\n");
  
  if (expr_file) {
    FILE *efp = fopen (expr_file, "w");
    if (!efp) {
      fatal_error ("Could not open expression file `%s' for writing",
		   expr_file);
    }
    fprintf (*output_stream, "import \"%s\";\n", expr_file);
    fprintf (efp, "namespace syn {\n\nexport namespace expr {\n\n");
    fclose (efp);
  }
  fprintf(*output_stream, "\n");
}


static void end_sdtout (FILE *fpout, const char *expr_file)
{
  if (fpout != stdout) {
    fclose (fpout);
  }
  if (expr_file) {
    FILE *efp = fopen (expr_file, "a");
    fprintf (efp, "\n}\n\n}\n");
    fclose (efp);
  }
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

  if (chpopt)
  {
#ifdef FOUND_chp_opt    
    ChpOptPass *copt = new ChpOptPass (a);
#else
    fatal_error ("Optimize flag is not currently enabled in the build.");
#endif
  }

  ActDynamicPass *c2p = new ActDynamicPass (a, "chp2prs", "libactchp2prspass.so", "chp2prs");

  if (!c2p || (c2p->loaded() == false)) {
    fatal_error ("Could not load dynamic pass!");
  }

  if (!exprfile) {
    exprfile = Strdup ("expr.act");
  }

  FILE *fpout, *efp;

  begin_sdtout (argv[optind+2], exprfile, emit_import ? argv[optind] : NULL,
		bundled, external_opt, &fpout);

  c2p->setParam ("chp_optimize", chpopt);
  c2p->setParam ("externopt", external_opt);
  c2p->setParam ("bundled_dpath", bundled);
  if (external_opt) {
    c2p->setParam ("use_yosys", strcmp (syntesistool, "genus") ? 1 : 0);
  }
  c2p->setParam ("expr_file", exprfile);
  c2p->setParam ("output_fp", fpout);

  c2p->run (p);

  end_sdtout (fpout, exprfile);
  
  return 0;
}
