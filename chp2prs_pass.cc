/*************************************************************************
 *
 *  Copyright (c) 2021 Rajit Manohar
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
#include <act/act.h>
#include "sdt/basicsdt.h"
#include "sdt/externoptsdt.h"
#include "chp2prs_pass.h"


void chp2prs_init (ActPass *dp)
{
  /* nothing has to be done */
}


static BasicSDT *_pending = NULL;

static void kill_mapper_on_exit (void)
{
  if (_pending) {
    delete _pending;
  }
  _pending = NULL;
}

void *chp2prs_proc (ActPass *ap, Process *p, int mode)
{
  ActDynamicPass *dp = dynamic_cast<ActDynamicPass *> (ap);
  Assert (dp, "What?!");

  /* -- create sdt version if a chp body exists without a prs body -- */
  int chpopt, externopt, bundled;
  const char *exprfile;
  FILE *fp;
  BasicSDT *sdt;
  int use_yosys;

  chpopt = dp->getIntParam ("chp_optimize");
  externopt = dp->getIntParam ("externopt");
  bundled = dp->getIntParam ("bundled_dpath");
  use_yosys = dp->getIntParam ("use_yosys");
  exprfile = (const char *) dp->getPtrParam ("expr_file");
  fp = (FILE *) dp->getPtrParam ("output_fp");

  if (chpopt) {
    warning ("-O flag is ignored");
  }

  if (externopt) {
    sdt = new ExternOptSDT (bundled, chpopt, fp, exprfile,
			      use_yosys == 1 ? yosys :  
                             (use_yosys == 0 ? genus : abc ));
    _pending = sdt;
  }
  else {
    if (bundled) {
      fatal_error ("Bundled-data not supported is Basic mode");
    }
    sdt = new BasicSDT (bundled, chpopt, fp, exprfile);
    _pending = sdt;
  }
  
  atexit (kill_mapper_on_exit);
  sdt->run_sdt (p);
  kill_mapper_on_exit ();

  return NULL;
}
