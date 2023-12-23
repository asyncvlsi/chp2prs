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
 * This should have the dynamic pass
 */
static void _init (ActDynamicPass *dp)
{
  if (dp->getPtrParam ("raw")) {
    return;
  }
  char *pref = (char *)dp->getPtrParam ("prefix");
  char *ifile = (char *)dp->getPtrParam ("in");
  char *ofile = (char *)dp->getPtrParam ("out");
  char *efile = (char *)dp->getPtrParam ("expr");

  if (!ifile || !ofile || !pref) return;

  dp->setParam ("raw", (void *) new ActSynthesize (pref, ifile, ofile, efile));
}

void synthesis_init (ActPass *ap)
{
  ActDynamicPass *dp = dynamic_cast<ActDynamicPass *> (ap);
  Assert (dp, "What?");
}

