/*************************************************************************
 *
 *  This file is part of the ACT library
 *
 *  Copyright (c) 2023-2024 Karthi Srinivasan
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
  fprintf(stderr, "Usage: %s [-hdO] [-e <file>] [-o <file>] -p <proc> <actfile>\n", name);
  fprintf (stderr, "Options:\n");
  fprintf (stderr, " -O : optimize CHP\n");
  fprintf (stderr, " -h : display this usage message\n");
  fprintf (stderr, " -e <file> : save expressions synthesized into <file> [default: expr.act]\n");
  fprintf (stderr, " -o <file> : save output to <file> [default: print to screen]\n");
  exit(1);
}

int main (int argc, char **argv)
{
    Act *a;
    Process *p;
    char *proc = NULL;
    char *outfile = NULL;
    char *exprfile = NULL;
    bool chpopt = false;
    bool debug = false;
    act_languages *lang;
    act_chp *chp;
    act_chp_lang_t *chp_lang;

    Act::Init (&argc, &argv);

    int ch;
    while ((ch = getopt (argc, argv, "hdOe:o:p:")) != -1) {
      switch (ch) {
      case 'h':
        usage (argv[0]);
        break;

      case 'p':
        if (proc) {
        FREE (proc);
        }
        proc = Strdup (optarg);
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

      case 'd':
        debug = true;
        break;
        
      default:
        usage (argv[0]);
        break;
      }
    }

    if ( optind != argc - 1 ) {
        usage (argv[0]);
    }

    if (!proc) {
      fprintf (stderr, "Missing process name: use -p to specify it.\n");
      usage (argv[0]);
    }

    a = new Act (argv[optind]);
    a->Expand();

    p = a->findProcess (proc, true);

    if (!p) {
        fatal_error ("Process not found");
    }

    p = p->Expand(ActNamespace::Global(), p->CurScope(),0,NULL);
    Assert (p, "Process expand failed - what?");

    if (debug) 
    {
        fprintf(stdout,"\n\n------------------------------------------------------------");
        fprintf(stdout,"\n\nBegin debugging print..");
        fprintf(stdout,"\n\n------------------------------------------------------------");
        fprintf (stdout, "\n\nProcess Scope:\n");
        p->CurScope()->Print (stdout);
        fprintf (stdout, "\n\nGlobal Namespace:\n");
        ActNamespace::Global()->Print(stdout);
        fprintf(stdout,"\n\n------------------------------------------------------------");
        fprintf(stdout,"\n\nOriginal CHP: \n");
        // _chp_pretty_print(stdout,chp_lang,0);
        fprintf(stdout,"\n\n------------------------------------------------------------\n\n");
        fprintf(stdout,"\n\nEnd debugging print..");
        fprintf(stdout,"\n\n------------------------------------------------------------");
    }

    ActDynamicPass *rsyn = new ActDynamicPass (a, "synth", "libactchp2prspass.so", "synthesis");
    
    if (!rsyn || (rsyn->loaded() == false)) {
    fatal_error ("Could not load dynamic pass!");
    }
    
    if (!exprfile) {
      exprfile = Strdup ("expr.act");
    }

    rsyn->setParam ("prefix", (void *)Strdup ("ring"));
    rsyn->setParam ("expr", (void *) exprfile);
    rsyn->setParam ("out", (void *) outfile);
    rsyn->setParam ("in", (void *) argv[optind]);
    rsyn->setParam ("engine", (void *) gen_ring_engine);

    rsyn->setParam ("chp_optimize", chpopt);
    rsyn->setParam ("bundled_dpath", true);

    rsyn->run (p);

    return 0;
}
