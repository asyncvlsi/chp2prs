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
#include <act/extmacro.h>
#include <act/iter.h>
#include <act/passes.h>
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

  if (!ifile || !pref) {
    return NULL;
  }

  ret = (*f) (pref, ifile, ofile, efile);
  dp->setParam ("raw", (void *) ret);

  ret->prepSynthesis (dp);
		     
  return ret;
}


void synthesis_init (ActPass *ap)
{
  ActDynamicPass *dp;
  dp = dynamic_cast <ActDynamicPass *> (ap);
  Assert (dp, "What?");

  /* add dependency to Booleanize */
  ActPass *b = dp->getAct()->pass_find ("booleanize");
  if (!b) {
     b = new ActBooleanizePass (dp->getAct());
  }
  dp->addDependency ("booleanize");
}

void synthesis_run (ActPass *ap, Process *p)
{
  ActSynthesize *syn = _init (ap);
  if (syn) {
    syn->finalSynthesis (p);
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
  list_t *special_vx, *decomp_vx;
  pp_t *pp = syn->getPP ();
  const char *prefix = syn->getPrefix ();
  Process *p = dynamic_cast <Process *> (u);
  bool is_process;

  if (p) {
    special_vx = ActNamespace::Act()->getDecomp (p);
    decomp_vx = syn->getDecompVx(); 
  }
  else {
    special_vx = NULL;
    decomp_vx = NULL;
  }

  if (TypeFactory::isProcessType (u)) {
    pp_printf (pp, "defproc ");
    is_process = true;
  }
  else {
    pp_printf (pp, "deftype ");
    is_process = false;
  }

  pp_printf (pp, "%s_", prefix);

  ActNamespace::Act()->msnprintfproc (buf, 10240, u);
  
  pp_printf (pp, "%s <: ", buf);
  pp_lazy (pp, 4);
  u->snprintActName (buf, 10240);
  pp_printf (pp, "%s()", buf);
  pp_forced (pp, 0);
  
  int bw = 0;
  bool overrideTypes = syn->overrideTypes();

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

    if (special_vx) {
      /* these are fresh instances introduced during decomposition;
	 we need to declare them, not refine them!
      */
      int sp = 0;
      for (listitem_t *si = list_first (special_vx); si; si = list_next (si)) {
	for (listitem_t *li = list_first ((list_t *) list_value (si)); li;
	     li = list_next (li)) {
	  if (vx == (ValueIdx *) list_value (li)) {
	    sp = 1;
	    break;
	  }
	}
	if (sp) {
	  break;
	}
      }
      if (sp) {
	continue;
      }
    }

    if (syn->skipOverride (vx)) {
      continue;
    }
    
    /* chan variable found */
    if (!TypeFactory::isParamType(vx->t)) {
    if (u->isPort(vx->getName())) {
    if (TypeFactory::isChanType (vx->t)) {
      if (overrideTypes) {
	bw = TypeFactory::bitWidth(vx->t);
	OVERRIDE_OPEN;
	if (TypeFactory::isBoolType (TypeFactory::getChanDataType (vx->t))) {
	  syn->typeBoolChan (buf, 10240);
	}
	else {
	  syn->typeIntChan (buf, 10240, bw);
	}
	pp_printf_raw (pp, "%s %s;\n", buf, vx->getName());
      }
    }
    else if (TypeFactory::isIntType (vx->t)) {
      /* chp-optimize creates sel0, sel1,... & loop0, loop1, ... which do not have dualrail overrides */
      if (overrideTypes) {
	bw = TypeFactory::bitWidth(vx->t);
	OVERRIDE_OPEN;
	syn->typeInt (buf, 10240, bw);
	pp_printf_raw (pp, "%s %s;\n", buf, vx->getName());
      }
    }
    else if (TypeFactory::isBoolType (vx->t)) {
      if (overrideTypes) {
	OVERRIDE_OPEN;
	syn->typeBool (buf, 10240);
	pp_printf_raw (pp, "%s %s;\n", buf, vx->getName());
      }
    }
    else if (TypeFactory::isProcessType (vx->t) || TypeFactory::isStructure (vx->t)) {
      if (overrideTypes || TypeFactory::isProcessType (vx->t)) {
         OVERRIDE_OPEN;
         pp_printf (pp, "%s_", prefix);
         UserDef *ud = dynamic_cast <UserDef *> (vx->t->BaseType());
         Assert (ud, "Why am I here?");
         ActNamespace::Act()->msnprintfproc (buf, 10240, ud);
         pp_printf_raw (pp, "%s %s;\n", buf, vx->getName());
      }
    }
    }
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

  if (decomp_vx) {
    /* these are fresh instances introduced during decomposition;
       we need to declare them, not refine them!
       note: these instances go outside the refine bodies
    */
    for (listitem_t *si = list_first (decomp_vx); si; si = list_next (si)) {
	ValueIdx *vx = (ValueIdx *) list_value (si);

	if (TypeFactory::isChanType (vx->t)) {
	    bw = TypeFactory::bitWidth(vx->t);
	    if (TypeFactory::isBoolType (TypeFactory::getChanDataType (vx->t))) {
	      syn->typeBoolChan (buf, 10240);
	    }
	    else {
	      syn->typeIntChan (buf, 10240, bw);
	    }
	    pp_printf_raw (pp, "%s %s;\n", buf, vx->getName());
	}
	else if (TypeFactory::isIntType (vx->t)) {
	    bw = TypeFactory::bitWidth(vx->t);
	    syn->typeInt (buf, 10240, bw);
	    pp_printf_raw (pp, "%s %s;\n", buf, vx->getName());
	}
	else if (TypeFactory::isBoolType (vx->t)) {
	    syn->typeBool (buf, 10240);
	    pp_printf_raw (pp, "%s %s;\n", buf, vx->getName());
	}
	else if (TypeFactory::isProcessType (vx->t)) {
	  /*
	    These are specially inserted processes during process
	    decomposition, and hence they should have pre-defined
	    translations in the library
	  */
	  Process *proc = dynamic_cast <Process *> (vx->t->BaseType());
	  Assert (proc, "Why am I here?");
	  char buf[1024];
	  int pos;
	  int found = 0;
	  ActNamespace::Act()->unmangle_string (proc->getName(), buf, 1024);
	  for (pos=0; buf[pos]; pos++) {
	    if (buf[pos] == '<') {
	      buf[pos] = '\0';
	      found = 1;
	      break;
	    }
	  }
	  pp_printf (pp, "%s::%s_builtin", syn->getLibNamespace(), buf);
	  if (found) {
	    buf[pos] = '<';
	    pp_printf (pp, "%s", buf+pos);
	  }
	  pp_printf_raw (pp, " %s;\n", vx->getName());
	}
	else if (TypeFactory::isStructure (vx->t)) {
	    pp_printf (pp, "%s_", prefix);
	    Data *d = dynamic_cast <Data *> (vx->t->BaseType());
	    Assert (d, "Why am I here?");
	    ActNamespace::Act()->msnprintfproc (buf, 10240, d);
	    pp_printf_raw (pp, "%s %s;\n", buf, vx->getName());
	}
      
    }
    pp_flush (pp);
    pp_printf (pp, "/* raw output */");
    pp_forced (pp, 0);
    for (listitem_t *si = list_first (decomp_vx); si; si = list_next (si)) {
	ValueIdx *vx = (ValueIdx *) list_value (si);
	if (vx->hasConnection()) {
	  Scope::printConnections (pp->fp, vx->connection(), true);
	}
    }
    fflush (pp->fp);
    pp_printf (pp, "/* end raw output */");
    pp_forced (pp, 0);
  }

  has_overrides = 0; // reset

  if (is_process) {

  if (config_get_int ("act.refine_steps")  > 0) {
    pp_printf (pp, "refine <%d> ", config_get_int("act.refine_steps") + 1);
  }
  else {
    pp_printf (pp, "refine ");
  }
  pp_forced (pp, 2);
  pp_setb (pp);

  // once more
  for (iter = iter.begin(); iter != iter.end(); iter++) {
    ValueIdx *vx = *iter;

    if (special_vx) {
      /* these are fresh instances introduced during decomposition;
	 we need to declare them, not refine them!
      */
      int sp = 0;
      for (listitem_t *si = list_first (special_vx); si; si = list_next (si)) {
	for (listitem_t *li = list_first ((list_t *) list_value (si)); li;
	     li = list_next (li)) {
	  if (vx == (ValueIdx *) list_value (li)) {
	    sp = 1;
	    break;
	  }
	}
	if (sp) {
	  break;
	}
      }
      if (sp) {
	continue;
      }
    }

    if (syn->skipOverride (vx)) {
      continue;
    }
    
    /* chan variable found */
    if (!TypeFactory::isParamType(vx->t)) {
    if (!u->isPort(vx->getName())) {
    if (TypeFactory::isChanType (vx->t)) {
      if (overrideTypes) {
	bw = TypeFactory::bitWidth(vx->t);
	OVERRIDE_OPEN;
	if (TypeFactory::isBoolType (TypeFactory::getChanDataType (vx->t))) {
	  syn->typeBoolChan (buf, 10240);
	}
	else {
	  syn->typeIntChan (buf, 10240, bw);
	}
	pp_printf_raw (pp, "%s %s;\n", buf, vx->getName());
      }
    }
    else if (TypeFactory::isIntType (vx->t)) {
      /* chp-optimize creates sel0, sel1,... & loop0, loop1, ... which do not have dualrail overrides */
      if (overrideTypes) {
	bw = TypeFactory::bitWidth(vx->t);
	OVERRIDE_OPEN;
	syn->typeInt (buf, 10240, bw);
	pp_printf_raw (pp, "%s %s;\n", buf, vx->getName());
      }
    }
    else if (TypeFactory::isBoolType (vx->t)) {
      if (overrideTypes) {
	OVERRIDE_OPEN;
	syn->typeBool (buf, 10240);
	pp_printf_raw (pp, "%s %s;\n", buf, vx->getName());
      }
    }
    else if (TypeFactory::isProcessType (vx->t) || TypeFactory::isStructure (vx->t)) {
      if (overrideTypes || TypeFactory::isProcessType (vx->t)) {
         OVERRIDE_OPEN;
         pp_printf (pp, "%s_", prefix);
         UserDef *ud = dynamic_cast <UserDef *> (vx->t->BaseType());
         Assert (ud, "Why am I here?");
         ActNamespace::Act()->msnprintfproc (buf, 10240, ud);
         pp_printf_raw (pp, "%s %s;\n", buf, vx->getName());
      }
    }
    }
    }
  }

  if (has_overrides) {
    pp_endb (pp);
    pp_printf_raw (pp, "}\n{");
  }
  else {
    pp_printf_raw (pp, "{");
  }
  pp_forced (pp, 2);
  pp_setb (pp);

  }

  // if (u->getlang() && u->getlang()->getchp()) {
  //   if (config_get_int ("act.refine_steps")  > 0) {
  //     pp_printf (pp, "refine <%d> {", config_get_int("act.refine_steps") + 1);
  //   }
  //   else {
  //     pp_printf (pp, "refine {");
  //   }
  //   pp_forced (pp, 2);
  //   pp_setb (pp);
  // }

  if (special_vx) {
    /* these are fresh instances introduced during decomposition;
       we need to declare them, not refine them!
    */
    for (listitem_t *si = list_first (special_vx); si; si = list_next (si)) {
      for (listitem_t *li = list_first ((list_t *) list_value (si)); li;
	   li = list_next (li)) {
	ValueIdx *vx = (ValueIdx *) list_value (li);

	if (TypeFactory::isChanType (vx->t)) {
	  if (overrideTypes) {
	    bw = TypeFactory::bitWidth(vx->t);
	    if (TypeFactory::isBoolType (TypeFactory::getChanDataType (vx->t))) {
	      syn->typeBoolChan (buf, 10240);
	    }
	    else {
	      syn->typeIntChan (buf, 10240, bw);
	    }
	    pp_printf_raw (pp, "%s %s;\n", buf, vx->getName());
	  }
	}
	else if (TypeFactory::isIntType (vx->t)) {
	  if (overrideTypes) {
	    bw = TypeFactory::bitWidth(vx->t);
	    syn->typeInt (buf, 10240, bw);
	    pp_printf_raw (pp, "%s %s;\n", buf, vx->getName());
	  }
	}
	else if (TypeFactory::isBoolType (vx->t)) {
	  if (overrideTypes) {
	    syn->typeBool (buf, 10240);
	    pp_printf_raw (pp, "%s %s;\n", buf, vx->getName());
	  }
	}
	else if (TypeFactory::isProcessType (vx->t)) {
	  /*
	    These are specially inserted processes during process
	    decomposition, and hence they should have pre-defined
	    translations in the library
	  */
	  Process *proc = dynamic_cast <Process *> (vx->t->BaseType());
	  Assert (proc, "Why am I here?");
	  char buf[1024];
	  int pos;
	  int found = 0;
	  ActNamespace::Act()->unmangle_string (proc->getName(), buf, 1024);
	  for (pos=0; buf[pos]; pos++) {
	    if (buf[pos] == '<') {
	      buf[pos] = '\0';
	      found = 1;
	      break;
	    }
	  }
	  pp_printf (pp, "%s::%s_builtin", syn->getLibNamespace(), buf);
	  if (found) {
	    buf[pos] = '<';
	    pp_printf (pp, "%s", buf+pos);
	  }
	  pp_printf_raw (pp, " %s;\n", vx->getName());
	}
	else if (TypeFactory::isStructure (vx->t)) {
	  if (overrideTypes) {
	    OVERRIDE_OPEN;
	    pp_printf (pp, "%s_", prefix);
	    Data *d = dynamic_cast <Data *> (vx->t->BaseType());
	    Assert (d, "Why am I here?");
	    ActNamespace::Act()->msnprintfproc (buf, 10240, d);
	    pp_printf_raw (pp, "%s %s;\n", buf, vx->getName());
	  }
	}
      }
    }
    pp_flush (pp);
    pp_printf (pp, "/* raw output */");
    pp_forced (pp, 0);
    for (listitem_t *si = list_first (special_vx); si; si = list_next (si)) {
      for (listitem_t *li = list_first ((list_t *) list_value (si)); li;
	   li = list_next (li)) {
	ValueIdx *vx = (ValueIdx *) list_value (li);
	if (vx->hasConnection()) {
	  Scope::printConnections (pp->fp, vx->connection(), true);
	}
      }
    }
    fflush (pp->fp);
    pp_printf (pp, "/* end raw output */");
    pp_forced (pp, 0);
  }

  if (special_vx) {
    list_free (special_vx);
  }
  
  return has_overrides;
#undef OVERRIDE_OPEN
}



void *synthesis_proc (ActPass *ap, Process *p, int mode)
{
  ActSynthesize *syn = _init (ap);
  if (!syn) return NULL;
  
  if (mode == 0) {
    pp_t *pp = syn->getPP ();
    
    if (p->getlang() && p->getlang()->getchp()) {
      /* check if we should synthesize this at all! */
      ExternMacro *macro = new ExternMacro (p);
      if (macro->isValid()) {
	/*-- we already have an external definition --*/
	delete macro;
	return NULL;
      }
      delete macro;

      if (p->getns() && p->getns() != ActNamespace::Global()) {
	if (strcmp (p->getns()->getName(), "std") == 0) {
	  list_t *l = ActNamespace::Act()->getDecompTypes ();
	  if (l) {
	    for (listitem_t *li = list_first (l); li; li = list_next (li)) {
	      if (p == (Process *) list_value (li)) {
		list_free (l);
		return NULL;
	      }
	    }
	    list_free (l);
	  }
	}
      }
    
      syn->runPreSynth (ap, p);
      int v = emit_refinement_header (syn, p);
      syn->runSynth (ap, p);
      
      pp_endb (pp);
      pp_printf (pp, "/* end refine */");
      pp_forced (pp, 0);
      pp_printf (pp, "}");
      pp_forced (pp, 0);
    }
    else {
      // syn->runPreSynth (ap, p);
      int v = emit_refinement_header (syn, p);
      // TODO: fix this hack maybe
      pp_printf (pp, "{ 42=42 : \"placeholder\" };");
      pp_forced (pp, 0);
      pp_printf (pp, "}");
      pp_forced (pp, 0);
    }
    syn->resetDecompVx();
    pp_endb (pp);
    pp_printf (pp, "/* end process */");
    pp_forced (pp, 0);
    pp_printf (pp, "}");
    pp_forced (pp, 0);
  }
  return NULL;
}

/*
 * Emit structure refinement, if needed
 */
void *synthesis_data (ActPass *ap, Data *d, int mode)
{
  ActSynthesize *syn = _init (ap);
  if (!syn) return NULL;

  if (mode == 0) {
    if (TypeFactory::isStructure (d)) {
      syn->processStruct (d);
      if (!syn->overrideTypes()) {
	return NULL;
      }

      /* do something! */
      pp_t *pp = syn->getPP ();
      int v = emit_refinement_header (syn, d);

      pp_endb (pp);
      pp_forced (pp, 0);
      pp_printf (pp, "}");
      pp_forced (pp, 0);
    }
  }
  return NULL;
}
