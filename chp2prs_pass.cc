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
#include "config_pkg.h"
#include "basicsdt.h"

#ifdef FOUND_expropt
#include "externoptsdt.h"
#endif

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
  const char *mapper;

  chpopt = dp->getIntParam ("chp_optimize");
  externopt = dp->getIntParam ("externopt");
  bundled = dp->getIntParam ("bundled_dpath");
  if (dp->hasParam ("synthesis_engine")) {
    mapper = (char *) dp->getPtrParam ("synthesis_engine");
  }
  else {
    mapper = NULL;
  }
  exprfile = (const char *) dp->getPtrParam ("expr_file");
  fp = (FILE *) dp->getPtrParam ("output_fp");

  if (chpopt)
  {
#ifdef FOUND_chp_opt
    ActPass *opt_p = dp->getAct()->pass_find ("chpopt");
    if (opt_p && p->getlang()->getchp()) {
      opt_p->run (p);
      printf("> Optimized CHP:\n");
      chp_print(stdout, p->getlang()->getchp()->c);
      printf("\n");
    }
#else
    fatal_error ("Optimize flag is not currently enabled in the build.");
#endif
  }

  if (externopt) {
#if defined(FOUND_expropt)
    sdt = new ExternOptSDT (bundled, chpopt, fp, exprfile, mapper);
    _pending = sdt;
#else
    fatal_error ("External optimization package not installed.");
#endif    
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

