/*************************************************************************
 *
 *  Copyright (c) 2026 Rajit Manohar
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

/**
 * Checker Engine
 */

class CheckerSynth : public ActSynthesize {
 public:
  CheckerSynth (const char *prefix,
		char *infile,
		char *outfile,
		char *exprfile)
    : ActSynthesize (prefix, NULL, Strdup ("/dev/null"), Strdup ("/dev/null")) {
  }
  
  void emitTopImports(ActPass *ap) { }

  void typeInt (char *buf, int sz, int bitwidth) { buf[0] = '\0'; }
  void typeBool (char *buf, int sz) { buf[0] = '\0'; }
  void typeIntChan (char *buf, int sz, int bitwidth) { buf[0] = '\0'; }
  void typeBoolChan (char *buf, int sz) { buf[0] = '\0'; }
  void runPreSynth (ActPass *ap, Process *p) { }
  bool skipOverride (ValueIdx *vx) { return true; }
  void processStruct (Data *d) { }
  void typeStructChan (char *buf, int sz, InstType *t) { }
  void runSynth (ActPass *ap, Process *p) { }
};

ActSynthesize *gen_checker_engine (const char *prefix,  char *infile,
				   char *outfile, char *exprfile)
{
  return new CheckerSynth (prefix, infile, outfile, exprfile);
}
