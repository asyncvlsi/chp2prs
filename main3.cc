/*************************************************************************
 *
 *  This file is part of the ACT library
 *
 *  Copyright (c) 2023 Karthi Srinivasan
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

#include "src_ring_synth/reqs.h"

static void usage(char *name)
{
  fprintf(stderr, "Usage: %s [-d] <actfile> <process> <output file>", name);
  exit(1);
}

int main (int argc, char **argv)
{
    Act *a;
    Process *p;
    FILE *fp_out;
    int debug = 0;
    act_languages *lang;
    act_chp *chp;
    act_chp_lang_t *chp_lang;

    Act::Init (&argc, &argv);

    const char *a_name;

    int ch;

    while ((ch = getopt (argc, argv, "d")) != -1) {
    switch (ch) {
    case 'd':
        debug = 1;
        break;
    default:
        usage (argv[0]);
        break;
        }
    }

    if ( optind != argc - 3 ) {
        usage (argv[0]);
    }

    a = new Act (argv[optind]);
    a_name = argv[optind];
    a->Expand();

    p = a->findProcess (argv[optind+1]);
    fp_out = fopen (argv[optind+2], "w");

    if (!fp_out) {
        fatal_error ("Could not open %s for writing", argv[optind+2]);
    }

    if (!p) {
        fatal_error ("Process not found");
    }

    p = p->Expand(ActNamespace::Global(), p->CurScope(),0,NULL);
    Assert (p, "Process expand failed - what?");

    Scope *proc_scope = p->CurScope();

    // p->Print (stdout);
    lang = p->getlang();
    chp = lang->getchp();

    if (chp)    { chp_lang = chp->c; }
    else        { fatal_error("No chp body"); }

    if (debug == 1) 
    {
        fprintf(stdout,"\n\n------------------------------------------------------------");
        fprintf(stdout,"\n\nBegin debugging print..");
        fprintf(stdout,"\n\n------------------------------------------------------------");
        fprintf (stdout, "\n\nProcess Scope:\n");
        proc_scope->Print (stdout);
        fprintf (stdout, "\n\nGlobal Namespace:\n");
        ActNamespace::Global()->Print(stdout);
        fprintf(stdout,"\n\n------------------------------------------------------------");
        fprintf(stdout,"\n\nOriginal CHP: \n");
        // _chp_pretty_print(stdout,chp_lang,0);
        fprintf(stdout,"\n\n------------------------------------------------------------\n\n");
        fprintf(stdout,"\n\nEnd debugging print..");
        fprintf(stdout,"\n\n------------------------------------------------------------");
    }

    // New synthesis test -----------------------
    // p->CurScope()->Print(fp_out);
    // p->PrintHeader(fp_out, "defproc");

    // Optimization Step ------------------------

    // int tmp=check_if_pipeable(chp_lang, p, 1);
    // fprintf (fp_out, "%d", tmp);

    // Pre-processing 1 -------------------------

    fill_in_else_explicit (chp_lang, p, 1);

    // NOTE: moved inside branched ring synthesis -----
    // generate_live_var_info (chp_lang, p, 1);
    // generate_live_var_bits (chp_lang, p, 1);
    // print_live_var_info (chp_lang, p, 1);
    // moved inside branched ring synthesis -----------

    // Ring Synthesis ---------------------------

    fprintf (fp_out, "import \"%s\";\n",a_name);
    print_headers_and_imports_expr();
    print_headers_and_imports (fp_out, p);
    
    mangle_init();

    print_overrides (fp_out,p);
    Hashtable *hvi = construct_var_info_hashtable 
                            (fp_out, chp_lang, p);
    print_var_info_hashtable (hvi);
    print_refine_body (fp_out, p, chp_lang, hvi);

    // ------------------------------------------

    // Expr *e;
    // generate_expr_block (e,32,p,fp_out);

    // Live Variable Analysis & Ring Chopping ---
    // fprintf (fp_out, "\n\n");
    // generate_live_var_info (chp_lang, p, 1);
    // generate_live_var_bits (chp_lang, p, 1);

    // print_live_var_info (chp_lang, p, 1);

    // act_chp_lang_t *cc;
    // cc = chop_ring (chp_lang, p, 1, 1);
    // Assert ((cc), "what");

    // chp_print (stdout, cc);
    // fprintf(stdout, "\n||\n");
    // chp_print (stdout, chp_lang);
    // ------------------------------------------

    return 0;
}