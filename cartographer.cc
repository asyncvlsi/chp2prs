/*************************************************************************
 *
 *  This file is part of chp2prs
 *
 *  Copyright (c) 2018, 2020 Rajit Manohar
 *  Copyright (c) 2018-2019 Zeb Mehring
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
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <act/types.h>
#include "cartographer.h"
#include <act/iter.h>
#include <act/value.h>

#ifdef CHP_OPTIMIZE
#include <act/chp-opt/optimize.h>
#endif

#include "basicsdt.h"

/*
 *
 *  Core syntax-directed translation code written by Rajit Manohar
 *
 *  Extensions to full expressions, optimizations, and direct use of
 *  the ACT library  by Zeb Mehring
 *
 */


#define MAX(a, b) (((a) > (b)) ? (a) : (b))


/* Recursively called fn to handle different chp statement types */

/* Print proc definition & override CHP variables */
bool write_process_definition(FILE *fp, Process * p, const char * proc_name)
{
  bool has_overrides = 0;
  bool has_bool_overrides = 0;

  fprintf(fp, "defproc sdt_%s <: %s ()\n", proc_name, proc_name);

  int bw = 0;
  
  /* iterate through Scope Hashtable to find all chp variables */
  ActInstiter iter(p->CurScope());
  for (iter = iter.begin(); iter != iter.end(); iter++) {
    ValueIdx *vx = *iter;
    /* chan variable found */
    if (TypeFactory::isChanType (vx->t))
    {
      bw = TypeFactory::bitWidth(vx->t);
      if (!has_overrides) {
	fprintf(fp, "+{\n");
	has_overrides = true;
      }
      fprintf(fp, "  syn::sdtchan<%d> %s;\n", bw, vx->getName());
    }
    
    /* int variable found */
    if (TypeFactory::isIntType (vx->t)) {
      /* chp-optimize creates sel0, sel1,... & loop0, loop1, ... which do not have dualrail overrides */
      bw = TypeFactory::bitWidth(vx->t);
      if (!has_overrides) {
	fprintf(fp, "+{\n");
	has_overrides = true;
      }
      fprintf(fp, "  syn::sdtvar<%d> %s;\n", bw, vx->getName());
    }
    else if (TypeFactory::isBoolType (vx->t)) {
      if (!has_overrides) {
	fprintf(fp, "+{\n");
	has_overrides = true;
      }
      fprintf (fp, " syn::sdtboolvar %s;\n", vx->getName());
      has_bool_overrides = 1;
    }
  }
  /* end param declaration */
  if (has_overrides) {
    fprintf(fp, "}\n{\n");
  }
  else {
    fprintf(fp, "{\n");
  }

  if (has_bool_overrides) {
    int vconnect = 0;
    for (iter = iter.begin(); iter != iter.end(); iter++) {
      ValueIdx *vx = *iter;
      if (TypeFactory::isBoolType (vx->t)) {
	fprintf (fp, " syn::sdtvar<1> b_%s;\n", vx->getName());
	fprintf (fp, " syn::varconnect vc_%d(%s,b_%s);\n",
		 vconnect++, vx->getName(), vx->getName());
      }
    }
  }
  return has_overrides;
}

/* Initialize var_init_false vars for each CHP int */
void initialize_chp_ints(FILE *fp, Process * p, bool has_overrides)
{
  int bw = 0;

  /* iterate through Scope Hashtable to find all chp ints */
  fprintf(fp, "  /* Initialize chp vars */\n");

  ActInstiter iter(p->CurScope());
  for (iter = iter.begin(); iter != iter.end(); iter++) {
    ValueIdx *vx = *iter;
    
    /* int variable found */
    if (TypeFactory::isIntType (vx->t)) {
      bw = TypeFactory::bitWidth(vx->t);
      fprintf(fp, "  syn::var_init<%d,false> var_%s(%s);\n", bw,
	      vx->getName(), vx->getName());
    }
    else if (TypeFactory::isBoolType (vx->t)) {
      fprintf(fp, "  syn::var_init<1,false> var_%s(b_%s);\n",
	      vx->getName(), vx->getName());
    }
  }
  fprintf(fp, "\n");
}

void BasicSDT::_emit_begin ()
{
  /* initialize the output location */ 
  if (output_file) {
    output_stream = fopen(output_file, "w");
    if (!output_stream) {
      fatal_error ("Could not open file `%s' for reading", output_file);
    }
  }
  else {
    output_stream = stdout;
  }


  /* get proc_name */
  size_t pn_len = strlen(P->getName());
  char proc_name[pn_len];
  strncpy(proc_name, P->getName(), pn_len-2);
  proc_name[pn_len-2] = '\0';
  
  /* print imports */
  if (bundled_data) {
    fprintf (output_stream, "import \"syn/bundled.act\";\n");
  }
  else {
    fprintf(output_stream, "import syn;\n");
  }
  fprintf(output_stream, "\n");
  
  /* Print params for toplevel from process port list */
  int pnum = P->getNumPorts();
  bool has_overrides = false;
       
  /* Write process definition and variable declarations */
  int overrides = write_process_definition(output_stream, P, proc_name);
  initialize_chp_ints(output_stream, P, overrides);
}


void BasicSDT::_emit_end (int id)
{
  /* connect toplevel "go" signal and print wrapper process instantiation */
  
  fprintf (output_stream, "/*--- connect reset to go signal ---*/\n");

  fprintf (output_stream, "   prs { Reset => c%d.r- }\n", id);

  fprintf (output_stream, "}\n");
  
  if (output_file) fclose(output_stream);
}
  
