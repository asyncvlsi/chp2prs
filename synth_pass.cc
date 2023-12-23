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
  }
}


void *synthesis_proc (ActPass *ap, Process *p, int mode)
{
  ActSynthesize *syn = _init (ap);
  return NULL;
}

void *synthesis_data (ActPass *ap, Data *d, int mode)
{
  ActSynthesize *syn = _init (ap);
  return NULL;
}
