/*************************************************************************
 *
 *  Copyright (c) 2024 Rajit Manohar
 *  Copyright (c) 2025 Karthi Srinivasan
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
 *************************************************************************
 */
#include "synth.h"
#include "engines.h"

#include "ring/reqs.h"
#include "ring/ring_misc.h"
#include "ring/tiny_forge.h"
#include "ring/ring_scan.h"

#include "opt/chp-opt.h"

#include <chrono>
using namespace std::chrono;

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
      config_set_string("synth.bundled.cell_lib_namespace","std::cells");
      config_set_string("synth.bundled.cell_lib","${ACT_HOME}/act/std/cells.act");
    }
  
  void emitTopImports(ActPass *ap) {
    ActDynamicPass *dp = dynamic_cast <ActDynamicPass *> (ap);
    Assert (dp, "Hmm");
    int bundled_data = dp->getIntParam ("bundled_dpath");
    int bundled_data_2phase = dp->getIntParam ("bundled_dpath_2phase");
    int bundled_data_pulsed = dp->getIntParam ("bundled_dpath_pulsed");
    int di_dpath = dp->getIntParam ("di_dpath");
    int ditest_dpath = dp->getIntParam ("ditest_dpath");

    int dm = dp->getIntParam ("delay_margin");
    int dpath_style = dp->getIntParam ("datapath_style");
    const char *externopt_toolname = (char *)dp->getPtrParam("externopt_toolname");
    BD_MODE bdpath_mode; 
    if (bundled_data_pulsed) bdpath_mode = BD_MODE::Latch_4phase;
    else if (bundled_data_2phase) bdpath_mode = BD_MODE::Latch_2phase;
    else bdpath_mode = BD_MODE::DFF;
    tf = new TinyForge (_pp->fp, bundled_data, dm, dpath_style, bdpath_mode, 
          externopt_toolname, "", _ename);
    /* print imports */
    
    if (bundled_data) {
      pp_printf_raw (_pp, "import syn::ring;\n");
      if (bundled_data_2phase) pp_printf_raw (_pp, "open syn::ring_2phase;\n");
      else if (bundled_data_pulsed) pp_printf_raw (_pp, "open syn::ring_pulsed;\n");
      else pp_printf_raw (_pp, "open syn::ring;\n");
    }
    else if (di_dpath){
      pp_printf_raw (_pp, "import \"syn/diopt/_all_.act\";\n");
      pp_printf_raw (_pp, "import syn::ring;\n");
      pp_printf_raw (_pp, "open syn::ring_di_dpath;\n");
    }
    else if (ditest_dpath){
      pp_printf_raw (_pp, "import \"syn/ditest/_all_.act\";\n");
      pp_printf_raw (_pp, "import syn::ring;\n");
      pp_printf_raw (_pp, "open syn::ring_di_dpath;\n");
    }
    else {
      pp_printf_raw (_pp, "import \"syn/qdi/_all_.act\";\n");
      pp_printf_raw (_pp, "import syn::ring;\n");
      pp_printf_raw (_pp, "open syn::ring_di_dpath;\n");
    }
    pp_printf_raw (_pp, "open syn;\n");

    pp_printf_raw (_pp, "import \"%s\";\n", _ename);
    // open the operating namespace
    pp_printf_raw (_pp, "open syn::expr;\n");

    // Delay-line table -------------------------
    pp_printf_raw (_pp, "\n// Delay Line Parameters ----\n");
    std::string params_table = bd_mode_config_params.at(bdpath_mode);
    std::string offsets_table  = bd_mode_config_offsets.at(bdpath_mode);

    int dp_sz = config_get_table_size(params_table.c_str());
    int *dparams = config_get_table_int(params_table.c_str());
    for (int i=0;i<dp_sz;i++) {
      pp_printf_raw (_pp, "Delay_Params[%d]=%d;\n",i,dparams[i]);
    }
    dp_sz = config_get_table_size(offsets_table.c_str());
    dparams = config_get_table_int(offsets_table.c_str());
    for (int i=0;i<dp_sz;i++) {
      pp_printf_raw (_pp, "Delay_Offsets[%d]=%d;\n",i,dparams[i]);
    }
    pp_printf_raw (_pp, "// Delay Line Parameters ----\n");
    
    const char *scn = config_get_string("synth.struct_chan_name");
    pp_printf_raw (_pp, 
      "\nexport defchan ring_chan_bool <: chan(bool) (ring_chan<1> %s) { }\n",scn);

    // ------------------------------------------

    pp_forced (_pp, 0);
    pp_forced (_pp, 0);

    fprintf (_expr, "namespace syn {\n\nexport namespace expr {\n\n");
    fclose (_expr);
    _expr = NULL;
  }

  void typeInt (char *buf, int sz, int bitwidth) {
    snprintf (buf, sz, "ring_int<%d>", bitwidth);
  }
  void typeBool (char *buf, int sz) {
    snprintf (buf, sz, "ring_bool");
  }
  void typeIntChan (char *buf, int sz, int bitwidth) {
    snprintf (buf, sz, "ring_chan<%d>", bitwidth);
  }
  void typeBoolChan (char *buf, int sz) {
    snprintf (buf, sz, "ring_chan_bool");
  }

  bool skipOverride (ValueIdx *vx) {
    if (TypeFactory::isDataType(vx->t) && TypeFactory::isBoolType(vx->t))
      return true;
    return false;
  }

  void processStruct (Data *d) {
    int w = TypeFactory::totBitWidth (d);
    Assert (w>=0, "What");
    fprintf(_pp->fp, "\n// Total Bitwidth : %d\n", w);
    char name[10240];
    char mname[10240];
    std::string ns = "";
    if (d->getns() && d->getns() != ActNamespace::Global()) {
      ns = d->getns()->Name(true);
    }
    d->snprintActName(name, 10240);
    ns = ns + name;
    ActNamespace::Act()->msnprintf(mname, 10240, ns.c_str());
    const char *scn = config_get_string("synth.struct_chan_name");
    fprintf(_pp->fp, "defchan chan_%s <: chan(%s) (ring_chan<%d> %s) {}\n\n", 
                      mname, ns.c_str(), w, scn);
  }

  void typeStructChan (char *buf, int sz, InstType *t) {
    InstType *td = TypeFactory::getChanDataType(t);
    Assert (td, "What");
    char name[10240];
    td->sPrint(name, 10240);
    char mname[10240];
    std::string ns = "";
    if (td->getNamespace() && td->getNamespace() != ActNamespace::Global()) {
      ns = td->getNamespace()->Name(true);
    }
    ns = ns + name;
    ActNamespace::Act()->msnprintf(mname, 10240, ns.c_str());
    snprintf (buf, sz, "chan_%s", mname);
  }

  TinyForge *tf;

  void emitFinal() {
    _expr = fopen (_ename, "a");
    if (!_expr) {
      fatal_error ("Could not open %s for appending!", _ename);
    }
    fprintf (_expr, "}\n}\n");
    fclose (_expr);
    _expr = NULL;

    tf->~TinyForge();
  }

  void runPreSynth (ActPass *ap, Process *p) {

    if (p->getlang() && p->getlang()->getchp()) {
      ScanInsertion *si = new ScanInsertion(p, p->getlang()->getchp()->c);
      si->insert_scan_points();
      if (!_new_ports) {
        _new_ports = list_new();
      } 
      _new_ports = si->get_new_ports();
      p->getlang()->getchp()->c = si->getc();
    }
  }

  void runSynth (ActPass *ap, Process *p) {
    pp_printf (_pp, "/* synthesis output */");
    pp_forced (_pp, 0);

    pp_flush (_pp);
    fprintf (_pp->fp, "/* start rsyn */\n");
    fflush (_pp->fp);

    int chpopt, bundled, dm, dpath_style;
    ActDynamicPass *dp;

    dp = dynamic_cast <ActDynamicPass *> (ap);
    Assert (dp, "What?");

    chpopt = dp->getIntParam ("chp_optimize");
    bundled = dp->getIntParam ("bundled_dpath");
    dm = dp->getIntParam ("delay_margin");
    dpath_style = dp->getIntParam ("datapath_style");

    act_chp_lang_t *c = p->getlang()->getchp()->c;

    // core synthesis functions here
    bool synthesize=true;

    if (synthesize)
    {
      Assert (c, "hmm no chp lol - something went wrong");
      mangle_init();
      rewrite_terminating_program (c);
      place_skip_in_empty_branches (c);
      fill_in_else_explicit (c, p->CurScope());
      flatten_lists (c, p->CurScope());

      ActBooleanizePass *b = (ActBooleanizePass *) dp->getPass("booleanize");
      b->run(p);
      Assert (b, "hmm b");

#if 1
      tf->set_p(p);
      tf->set_c(c);
      tf->set_bp(b);

      fprintf(stdout, "// %s : ",p->getName());
      fflush(stdout);
      auto ss1 = high_resolution_clock::now();
      if (tf->check_if_pipeable(c))
        tf->run_tiny_forge();
      else
        tf->run_forge();
      auto st1 = high_resolution_clock::now();
      auto d2 = duration_cast<microseconds>(st1 - ss1);

      int print_rt = dp->getIntParam ("run_time");
      if (print_rt) {
        fprintf(stdout, "\n// ----------- Process Runtimes ----------- ");
        fprintf(stdout, "\n// Maelstrom        : %-8lld microseconds", d2.count());
        fprintf(stdout, "\n// ABC              : %-8lld microseconds", tf->get_runtime());
        fprintf(stdout, "\n// ABC I/O          : %-8lld microseconds", tf->get_io_runtime());
        fprintf(stdout, "\n");
      }
      fprintf(stdout, "\n");
#else

      chp_print(_pp->fp, c);
      fprintf(stdout, "\n\n");

#endif
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

  