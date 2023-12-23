/*************************************************************************
 *
 *  Copyright (c) 2023 Rajit Manohar
 *
 *  This library is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU Lesser General Public
 *  License as published by the Free Software Foundation; either
 *  version 2.1 of the License, or (at your option) any later version.
 *
 *  This library is distributed in the hope that it will be useful,
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
#include "synth_pass.h"
#include <act/act.h>
#include <act/iter.h>
#include "synth.h"

/*
  Dynamic pass for logic synthesis.

  Load the pass, and set the following parameters:

  engine = pointer to a function that returns a new ActSynthesize
  object; takes four char * arguments corresponding to the
  constructor.

  Constructor arguments are provided with the following parameters to
  the dynamic pass:

  prefix = prefix string
  in = input file
  out = output file
  expr = expr file

  To define a new synthesis engine, change the engine setting and run
  the pass!
*/

/*
 * This should have the dynamic pass
 */
static ActSynthesize *_init (ActPass *ap)
{
  ActDynamicPass *dp;
  ActSynthesize *(*f) (char *, char *, char *, char *);
  ActSynthesize *ret;

  dp = dynamic_cast<ActDynamicPass *> (ap);
  if (!dp) {
    return NULL;
  }

  ret = (ActSynthesize *) dp->getPtrParam ("raw");
  if (ret) {
    return ret;
  }

  f = (ActSynthesize * (*) (char *, char *, char *, char *))
    dp->getPtrParam ("engine");
  if (!f) {
    return NULL;
  }
  
  char *pref = (char *)dp->getPtrParam ("prefix");
  char *ifile = (char *)dp->getPtrParam ("in");
  char *ofile = (char *)dp->getPtrParam ("out");
  char *efile = (char *)dp->getPtrParam ("expr");

  if (!ifile || !ofile || !pref) {
    return NULL;
  }

  ret = (*f) (pref, ifile, ofile, efile);
  dp->setParam ("raw", (void *) ret);

  ret->prepSynthesis (dp->getRoot());
		     
  return ret;
}


void synthesis_init (ActPass *ap)
{
  ActDynamicPass *dp = dynamic_cast<ActDynamicPass *> (ap);
  Assert (dp, "What?");
}

void synthesis_done (ActPass *ap)
{
  ActSynthesize *syn = _init (ap);
  if (syn) {
    delete syn;
    ActDynamicPass *dp = dynamic_cast<ActDynamicPass *>(ap);
    dp->setParam ("raw", (void *)NULL);
  }
}


static int emit_refinement_header (ActSynthesize *syn,
				   UserDef *u)
{
  int has_overrides = 0;
  char buf[10240];
  list_t *special_vx;
  pp_t *pp = syn->getPP ();
  const char *prefix = syn->getPrefix ();

  Process *p = dynamic_cast <Process *> (u);
  if (p) {
    special_vx = ActNamespace::Act()->getDecomp (p);
  }
  else {
    special_vx = NULL;
  }

  pp_printf (pp, "%s_", prefix);

  ActNamespace::Act()->msnprintfproc (buf, 10240, u);
  
  pp_printf (pp, " %s <: ", buf);
  pp_lazy (pp, 0);
  u->snprintActName (buf, 10240);
  pp_printf_raw (pp, " %s()\n", buf);

  int bw = 0;

#define OVERRIDE_OPEN				\
  do {						\
    if (!has_overrides) {			\
      pp_printf (pp, "+{");			\
      pp_forced (pp, 2);			\
      pp_setb (pp);				\
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
      syn->typeChan (buf, 10240, bw);
      pp_printf_raw (pp, "%s %s;\n", buf, vx->getName());
    }
    else if (TypeFactory::isIntType (vx->t)) {
      /* chp-optimize creates sel0, sel1,... & loop0, loop1, ... which do not have dualrail overrides */
      bw = TypeFactory::bitWidth(vx->t);
      OVERRIDE_OPEN;
      syn->typeInt (buf, 10240, bw);
      pp_printf_raw (pp, "%s %s;\n", buf, vx->getName());
    }
    else if (TypeFactory::isBoolType (vx->t)) {
      OVERRIDE_OPEN;
      syn->typeBool (buf, 10240);
      pp_printf_raw (pp, "%s %s;\n", buf, vx->getName());
    }
    else if (TypeFactory::isProcessType (vx->t) || TypeFactory::isStructure (vx->t)) {
      OVERRIDE_OPEN;
      pp_printf (pp, "%s_", prefix);
      UserDef *ud = dynamic_cast <UserDef *> (vx->t->BaseType());
      Assert (ud, "Why am I here?");
      ActNamespace::Act()->msnprintfproc (buf, 10240, ud);
      pp_printf_raw (pp, "%s %s;\n", buf, vx->getName());
    }
  }
  /* end param declaration */
  if (has_overrides) {
    pp_endb (pp);
    pp_printf_raw (pp, "}\n{");
  }
  else {
    pp_printf_raw (pp, "{");
  }
  pp_forced (pp, 2);
  pp_setb (pp);
  return has_overrides;
#undef OVERRIDE_OPEN
}



void *synthesis_proc (ActPass *ap, Process *p, int mode)
{
  ActSynthesize *syn = _init (ap);
  return NULL;
}

/*
 * Emit structure refinement, if needed
 */
void *synthesis_data (ActPass *ap, Data *d, int mode)
{
  ActSynthesize *syn = _init (ap);
  if (TypeFactory::isStructure (d)) {
    /* do something! */

  }
  return NULL;
}
