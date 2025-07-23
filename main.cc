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
#include "config_pkg.h"

#ifdef FOUND_chp_opt
#include <act/chp-opt/optimize.h>
#endif


static void usage(char *name)
{
  fprintf(stderr, "Usage BasicSDT: %s [-Ob] [-e <exprfile>] <actfile> <process> <out>\n", name);
  fprintf(stderr, "Usage ExrpOptSDT: %s [-Ob] -o [<abc,yosys,genus>] [-e <exprfile>] <actfile> <process> <out>\n", name);
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
    fprintf(*output_stream, "import \"syn/bdopt/_all_.act\";\n");
  }
  else {
    fprintf(*output_stream, "import \"syn/qdi/_all_.act\";\n");
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


int emit_refinement_header (FILE *fp, UserDef *u)
{
  int has_overrides = 0;
  
  fprintf(fp, "sdt_");
  ActNamespace::Act()->mfprintfproc (fp, u);
  fprintf (fp, " <: ");
  if (u->getns() != ActNamespace::Global()) {
    char *nsname = u->getns()->Name();
    fprintf (fp, "%s::", nsname);
    FREE (nsname);
  }
  u->printActName (fp);
  fprintf (fp, " ()\n");

  int bw = 0;

#define OVERRIDE_OPEN				\
  do {						\
    if (!has_overrides) {			\
      fprintf(fp, "+{\n");			\
      has_overrides = true;			\
    }						\
  } while (0)
    
  /* iterate through Scope Hashtable to find all chp variables */
  ActInstiter iter(u->CurScope());
  for (iter = iter.begin(); iter != iter.end(); iter++) {
    ValueIdx *vx = *iter;
    /* chan variable found */
    if (TypeFactory::isChanType (vx->t)) {
      bw = TypeFactory::bitWidth(vx->t);
      OVERRIDE_OPEN;
      fprintf(fp, "  syn::sdtchan<%d> %s;\n", bw, vx->getName());
    }
    else if (TypeFactory::isIntType (vx->t)) {
      /* chp-optimize creates sel0, sel1,... & loop0, loop1, ... which do not have dualrail overrides */
      bw = TypeFactory::bitWidth(vx->t);
      OVERRIDE_OPEN;
      fprintf(fp, "  syn::sdtvar<%d> %s;\n", bw, vx->getName());
    }
    else if (TypeFactory::isBoolType (vx->t)) {
      OVERRIDE_OPEN;
      fprintf (fp, " syn::sdtboolvar %s;\n", vx->getName());
    }
    else if (TypeFactory::isProcessType (vx->t)) {
      OVERRIDE_OPEN;
      fprintf (fp, " sdt_");
      Process *proc = dynamic_cast <Process *> (vx->t->BaseType());
      Assert (proc, "Why am I here?");
      ActNamespace::Act()->mfprintfproc (fp, proc);
      fprintf (fp, " %s;\n", vx->getName());
    }
    else if (TypeFactory::isStructure (vx->t)) {
      OVERRIDE_OPEN;
      fprintf (fp, " sdt_");
      Data *d = dynamic_cast <Data *> (vx->t->BaseType());
      Assert (d, "Why am I here?");
      ActNamespace::Act()->mfprintfproc (fp, d);
      fprintf (fp, " %s;\n", vx->getName());
    }
  }
  /* end param declaration */
  if (has_overrides) {
    fprintf(fp, "}\n{\n");
  }
  else {
    fprintf(fp, "{\n");
  }
  return has_overrides;
#undef OVERRIDE_OPEN
}

static void _struct_check (void *cookie, Data *d)
{
  FILE *fp;
  if (!cookie || !d) return;

  fp = (FILE *) cookie;

  if (!TypeFactory::isStructure (d)) {
    return;
  }

  fprintf (fp, "deftype ");
  emit_refinement_header (fp, d);

  fprintf (fp, "}\n");

  if (TypeFactory::isValidChannelDataType (d)) {
    fprintf (fp, "defchan sdt_chan");
    ActNamespace::Act()->mfprintfproc (fp, d);
    fprintf (fp, " <: chan(");
    if (d->getns() != ActNamespace::Global()) {
      char *nsname = d->getns()->Name();
      fprintf (fp, "%s::", nsname);
      FREE (nsname);
    }
    d->printActName (fp);
    fprintf (fp, ") (syn::sdtchan<%d> x) { }\n", TypeFactory::totBitWidth (d));
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
  emit_import = 1;

  if (chpopt)
  {
#ifdef FOUND_chp_opt    
    ChpOptPass *copt = new ChpOptPass (a);
#else
    fatal_error ("Optimize flag is not currently enabled in the build.");
#endif
  }

  ActApplyPass *app = new ActApplyPass (a);
  
  ActCHPFuncInline *ip = new ActCHPFuncInline (a);
  ip->run (p);

  ActCHPMemory *mem = new ActCHPMemory (a);
  mem->run (p);

  ActCHPArbiter *arbp = new ActCHPArbiter (a);
  arbp->run (p);

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


  /* now find all structures and channels with user-defined structs 

     chan(struct) needs more stuff...
       
     defchan sdtchan_... <: chan(struct) (...)
   */
  app->setCookie (fpout);
  app->setDataFn (_struct_check);
  app->run_per_type (p);
  app->setCookie (NULL);
  app->setDataFn (NULL);
 

  c2p->setParam ("chp_optimize", chpopt);
  c2p->setParam ("externopt", external_opt);
  c2p->setParam ("bundled_dpath", bundled);
  if (external_opt) {
    c2p->setParam ("synthesis_engine", syntesistool);
  }
  c2p->setParam ("expr_file", exprfile);
  c2p->setParam ("output_fp", fpout);

  c2p->run (p);

  end_sdtout (fpout, exprfile);
  
  return 0;
}
