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
#ifndef __ACT_SYNTH_H__
#define __ACT_SYNTH_H__

#include <stdio.h>
#include <common/pp.h>
#include <act/act.h>

/*
 *  Logic synthesis engine and helper methods.
 */
class ActSynthesize {
 public:
  /**
   * Constructor for logic synthesis output. This opens two output
   * files: one for the top-level output, and the other for the
   * auxillary expression file used by expression optimization
   *
   * @param prefix is the prefix pre-pended to processes/type
   * definitions that implement the original process/structures.
   * @param infile is the input file that holds the original input
   * @param outfile is the output file that holds the synthesis
   * results
   * @param exprfile is the expression support file (default is expr.act)
   */
  ActSynthesize (const char *prefix,
		 char *infile,
		 char *outfile,
		 char *exprfile = NULL);
  
  ~ActSynthesize ();

  /**
   * Override this function to include any imports that have to be
   * emitted at the top-level. These imports typically correspond to
   * any standard library that is used by the synthesis engine.
   */
  virtual void emitTopImports (ActPass *ap) {
    pp_printf_raw (_pp, "import \"%s\";\n", _ename);
  };

  /**
   * Override this function to add any final text to the outputs
   * generated 
   */
  virtual void emitFinal() {
    _expr = fopen (_ename, "a");
    if (!_expr) {
      fatal_error ("Could not open %s for appending!", _ename);
    }
    fprintf (_expr, "}\n}\n");
    fclose (_expr);
    _expr = NULL;
  }


  /**
   * Run the pre-synthesis steps needed. This calls decomposition
   * passes.
   *
   * @param ap is the pass
   * @return true on success, false if there was some error
   */
  bool prepSynthesis (ActPass *ap);

  /**
   * Run any final steps at the end of the entire synthesis process.
   *
   * @param p is the top-level process
   */
  void finalSynthesis (Process *p);

  /**
   * Print the name of the type that implements an n-bit integer to
   * the provided buffer
   */
  virtual void typeInt (char *buf, int sz, int bitwidth) { buf[0] = '\0'; }
  virtual void typeBool (char *buf, int sz) { buf[0] = '\0'; }
  virtual void typeIntChan (char *buf, int sz, int bitwidth) { buf[0] = '\0'; }
  virtual void typeBoolChan (char *buf, int sz) { buf[0] = '\0'; }

  const char *getLibNamespace () { return "syn"; }

  /**
   * Return pretty-printer
   */
  pp_t *getPP () { return _pp; }

  /**
   * Return prefix
   */
  const char *getPrefix () { return _prefix; }

  /**
   * Run logic synthesis locally on a process
   */
  virtual void runSynth (ActPass *ap, Process *p) { }
  
protected:
  FILE *_out;			///< output stream
  pp_t *_pp;			///< output pretty-printer
  FILE *_expr;			///< expr output file
  char *_ename;			///< expr file name

  const char *_prefix;		///< synthesis prefix
  
  Process *_top;

  void Close (); ///< close output files
};


#endif /* __ACT_SYNTH_H__ */
