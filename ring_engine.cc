/*************************************************************************
 *
 *  Copyright (c) 2024 Rajit Manohar
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
#include "synth.h"
#include "engines.h"

#include "ring/reqs.h"
#include "ring/ring_else_gen.h"
// #include "ring/ring_forge.h"
#include "ring/tiny_forge.h"

#include "opt/chp-opt.h"

class RingSynth : public ActSynthesize {
 public:
  RingSynth (const char *prefix,
	    char *infile,
	    char *outfile,
	    char *exprfile)
    : ActSynthesize (prefix, infile, outfile, exprfile) { 
    if (!exprfile) {
      fatal_error ("Ring Synthesis: requires an expression file");
    }
    }
  
  void emitTopImports(ActPass *ap) {
    ActDynamicPass *dp = dynamic_cast <ActDynamicPass *> (ap);
    Assert (dp, "Hmm");
    int bundled_data = dp->getIntParam ("bundled_dpath");

    /* print imports */
    // pp_printf_raw (_pp, "import \"syn/ring/_all_.act\";\n");
    pp_printf_raw (_pp, "import syn::ring;\n");
    pp_printf_raw (_pp, "open syn::ring;\n");
    pp_printf_raw (_pp, "open syn;\n");

    pp_printf_raw (_pp, "import \"%s\";\n", _ename);
    // open the operating namespace
    pp_printf_raw (_pp, "open syn::expr;\n");
    pp_forced (_pp, 0);
    pp_forced (_pp, 0);

    fprintf (_expr, "namespace syn {\n\nexport namespace expr {\n\n");
    fclose (_expr);
    _expr = NULL;
  }

  void typeInt (char *buf, int sz, int bitwidth) {
    snprintf (buf, sz, "bd_int<%d>", bitwidth);
  }
  void typeBool (char *buf, int sz) {
    fatal_error ("bools not supported, use int<1> instead");
  }
  void typeIntChan (char *buf, int sz, int bitwidth) {
    snprintf (buf, sz, "bd<%d>", bitwidth);
  }
  void typeBoolChan (char *buf, int sz) {
    fatal_error ("bool chans not supported, use chan(int<1>) instead");
  }

  void runSynth (ActPass *ap, Process *p) {
    pp_printf (_pp, "/* synthesis output */");
    pp_forced (_pp, 0);

    pp_flush (_pp);
    fprintf (_pp->fp, "/* start rsyn */\n");
    fflush (_pp->fp);

    int chpopt, bundled, dm;
    ActDynamicPass *dp;

    dp = dynamic_cast <ActDynamicPass *> (ap);
    Assert (dp, "What?");

    chpopt = dp->getIntParam ("chp_optimize");
    bundled = dp->getIntParam ("bundled_dpath");
    dm = dp->getIntParam ("delay_margin");

    act_chp_lang_t *c = p->getlang()->getchp()->c;

    // core synthesis functions here
    bool synthesize=true;

    if (synthesize)
    {
      Assert (c, "hmm no chp lol - something went wrong");
      mangle_init();
      fill_in_else_explicit (c, p, 1);

      ActBooleanizePass *b = (ActBooleanizePass *) dp->getPass("booleanize");
      b->run(p);
      Assert (b, "hmm b");

      RingForge *rf = new RingForge (_pp->fp, p, c, b, dm, "", _ename);
      TinyForge *tf = new TinyForge (_pp->fp, p, c, b, dm, "", _ename);

      // if (tf->check_if_pipeable(c))
      //   tf->run_forge();
      // else
      //   rf->run_forge();

      rf->run_forge();

      revert_mangle();
    }
    
    fprintf (_pp->fp, "\n\n");
    fprintf (_pp->fp, "/* end rsyn */\n");
    
    pp_forced (_pp, 0);
  }
};

ActSynthesize *gen_ring_engine (const char *prefix,
				char *infile,
				char *outfile,
				char *exprfile)

{
  return new RingSynth (prefix, infile, outfile, exprfile);
}

  