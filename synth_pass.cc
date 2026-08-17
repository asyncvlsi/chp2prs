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

  if (!pref) {
    return NULL;
  }

  ret = (*f) (pref, ifile, ofile, efile);
  dp->setParam ("raw", (void *) ret);

  int arb = dp->getIntParam("run_arb_pass");
  ret->prepSynthesis (dp, arb);
		     
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

static char *_extract_strict_name (UserDef *ud)
{
  char buf2[10240];
  int pos, idx;

  Assert (ud->isExpanded(), "What?");

  // walk through buf2 and trim non-strict parameters
  ActNamespace::Act()->unmangle_string (ud->getName(), buf2, 10240);
  pos = ud->getNumParams() - ud->getRemainingParams();
  // pos = # inherited

  idx = 0;
  while (buf2[idx] && buf2[idx] != '<') {
    idx++;
  }

  while (buf2[idx] && pos < ud->numStrict()) {
    bool skip_array = false;
    int skip_pstruct = 0;
    while (buf2[idx] &&
	   (skip_array || (skip_pstruct > 0) ||
	    buf2[idx] != ',' && buf2[idx] != '>')) {
      if (skip_pstruct > 0) {
	if (buf2[idx] == ')') {
	  skip_pstruct--;
	}
      }
      else {
	if (buf2[idx] == '{') {
	  skip_array = true;
	}
	else if (buf2[idx] == '}') {
	  skip_array = false;
	}
	else if (buf2[idx] == '(') {
	  skip_pstruct++;
	}
      }
      if (!skip_array && (skip_pstruct == 0)) {
	if (buf2[idx] == ',') {
	  break;
	}
      }
      idx++;
    }
    pos++;
    if (buf2[idx] == ',' || buf2[idx] == '>') {
      idx++;
    }
  }
  if (buf2[idx] == ',') {
    buf2[idx] = '>';
    buf2[idx+1] = '\0';
  }
  else if (buf2[idx] == '<') {
    buf2[idx+1] = '>';
    buf2[idx+2] = '\0';
  }
  return Strdup (buf2);
}



#define HEADER_NORMAL 0   /* normal header for refinement + overrides */

#define HEADER_DECL_VARIANT 1  /* used for variant header */

#define HEADER_DECL   2   /* declaration, used for variants */

#define HEADER_DECLII 3  /* used for refinement overrides in variants */


static int emit_refinement_header (ActSynthesize *syn,
				   UserDef *u,
				   int *braces,
				   int header_type = HEADER_NORMAL)
{
  int has_overrides = 0;
  char buf[10240];
  list_t *special_vx, *decomp_vx;
  pp_t *pp = syn->getPP ();
  const char *prefix = syn->getPrefix ();
  Process *p = dynamic_cast <Process *> (u);
  bool is_process;

  *braces = 0;

  if (p) {
    special_vx = ActNamespace::Act()->getDecomp (p);
    decomp_vx = syn->getDecompVx(); 

    if (special_vx && decomp_vx) {
      list_append (special_vx, decomp_vx);
      decomp_vx = NULL;
    }
    else if (decomp_vx) {
      special_vx = list_new();
      list_append(special_vx, decomp_vx);
      decomp_vx = NULL;
    }
  }
  else {
    special_vx = NULL;
    decomp_vx = NULL;
  }

  if (TypeFactory::isProcessType (u)) {
    is_process = true;
  }
  else {
    is_process = false;
  }

  if (header_type != HEADER_DECLII) {
    /*
      HEADER_DECLII, we've already emitted the defproc, so we just
      need to handle the overrides for the variant.
    */
    
    int count = syn->emitNamespace (u);
    if (is_process) {
      pp_printf (pp, "export defproc ");
    }
    else {
      pp_printf (pp, "export deftype ");
    }
    pp_printf (pp, "%s_", prefix);
    ActNamespace::Act()->msnprintfproc (buf, 10240, u, 1);

    char *strict_name;
    if (u->hasNonStrict()) {
      strict_name = _extract_strict_name (u);
      ActNamespace::Act()->mangle_string (strict_name, buf, 10240);
    }
    pp_printf (pp, "%s <: ", buf);
    pp_lazy (pp, 4);

    if (u->hasNonStrict()) {
      Assert (strict_name, "Hmm...");
      int len = strlen (strict_name);
      if (strict_name[len-1] == '>' && strict_name[len-2] == '<') {
	strict_name[len-2] = '\0';
      }
      snprintf (buf, 10240, "%s", strict_name);
      FREE (strict_name);
    }
    else {
      u->snprintActName (buf, 10240);
    }
    pp_printf (pp, "%s(", buf);
  
    /* add any new ports here */
    list_t *newp = syn->getNewPorts();
    if (newp && is_process) {
      pp_forced (pp, 0);
      for (listitem_t *li = list_first(newp); li; li = list_next (li)) {
	char buf1[1024];
	int pos = list_ivalue(li);
	auto it = u->getPortType(pos);
	auto nm = u->getPortName(pos);
	it->sPrint(buf1, 1024);
	fprintf(pp->fp, "%s %s", buf1, nm);
	if (list_next (li)) fprintf(pp->fp,";\n");
      }
    }
    pp_printf (pp, ")");
    pp_forced (pp, 0);
    *braces = count;
  }
  
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

  /* function to actually emit the override */
  auto emit_override = [&] (ValueIdx *vx) {
    int bw;
    if (TypeFactory::isChanType (vx->t)) {
      bw = TypeFactory::bitWidth(vx->t);
      OVERRIDE_OPEN;
      if (TypeFactory::isBoolType (TypeFactory::getChanDataType (vx->t))) {
	syn->typeBoolChan (buf, 10240);
      }
      else if (TypeFactory::isPureStruct (TypeFactory::getChanDataType (vx->t))) {
	syn->typeStructChan (buf, 10240, vx->t);
      }
      else {
	syn->typeIntChan (buf, 10240, bw);
      }
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
    else if (TypeFactory::isProcessType (vx->t)
	     || TypeFactory::isStructure (vx->t)) {
      OVERRIDE_OPEN;
      UserDef *ud = dynamic_cast <UserDef *> (vx->t->BaseType());
      Assert (ud, "Why am I here?");
      if (ud->getns() && ud->getns() != ActNamespace::Global()) {
	char *tmp = ud->getns()->Name (true);
	pp_printf (pp, "%s", tmp);
	FREE (tmp);
      }
      ActNamespace::Act()->msnprintfproc (buf, 10240, ud, 1);
      pp_printf (pp, "%s_", prefix);

      if (ud->hasNonStrict()) {
	char *news = _extract_strict_name (ud);
	ActNamespace::Act()->mangle_string (news, buf, 10240);
	FREE (news);
      }
      pp_printf_raw (pp, "%s %s;\n", buf, vx->getName());
    }
  };

  /* -- end override function -- */

  
  if (header_type != HEADER_DECLII) {
    /* 1. Override ports */
    if (header_type == HEADER_DECL) {
      if (has_overrides) {
	pp_endb (pp);
	pp_printf_raw (pp, "}");
      }
      return has_overrides;
    }

    if (overrideTypes) {
      for (int i=0; i < u->getNumPorts(); i++) {
	ValueIdx *vx = u->CurScope()->LookupVal (u->getPortName (i));
	if (!vx) continue;

	if (syn->skipOverride (vx)) {
	  continue;
	}
	emit_override (vx);
      }
    }

    if (header_type == HEADER_DECL_VARIANT) {
      if (has_overrides) {
	pp_endb (pp);
	pp_printf_raw (pp, "}");
      }
      return has_overrides;
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

    has_overrides = 0; // reset
  }

  if (is_process) {
    /* emit refinement header */
    pp_setb (pp);
    if (config_get_int ("act.refine_steps")  > 0) {
      pp_printf (pp, "refine <%d> ", config_get_int("act.refine_steps") + 1);
    }
    else {
      pp_printf (pp, "refine ");
    }

    // go through all the instances
    ActInstiter iter(u->CurScope ());
    for (iter = iter.begin(); iter != iter.end(); iter++) {
      ValueIdx *vx = *iter;
      
      if (TypeFactory::isParamType (vx->t)) continue; // meta parameters
      if (syn->skipOverride (vx)) continue; // user-defined skip
      if (u->isPort (vx->getName())) continue; // handled already

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

      if (overrideTypes || TypeFactory::isProcessType (vx->t)) {
	emit_override (vx);
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
  }

  if (special_vx) {
    /* these are fresh instances introduced during decomposition;
       we need to declare them, not refine them!
    */
    for (listitem_t *si = list_first (special_vx); si; si = list_next (si)) {
      for (listitem_t *li = list_first ((list_t *) list_value (si)); li;
	   li = list_next (li)) {
	ValueIdx *vx = (ValueIdx *) list_value (li);

	if (TypeFactory::isProcessType (vx->t)) {
	  /* special inserted processes; need to map to builtin
	     compilation */
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
	  /*
	    For std::ram generated by chpmem-pass. it is now
	    up to the synthesis to open the correct namespace.
	  */
	  pp_printf (pp, "%s_builtin", buf);
	  //pp_printf (pp, "syn::%s_builtin", buf);
	  if (found) {
	    buf[pos] = '<';
	    pp_printf (pp, "%s", buf+pos);
	  }
	  pp_printf_raw (pp, " %s;\n", vx->getName());
	  continue;
	}

	if (syn->skipOverride (vx)) {
	  /* just emit the special vx */
	  if (TypeFactory::isUserType (vx->t)) {
	    UserDef *u = dynamic_cast<UserDef *> (vx->t->BaseType());
	    if (u->getns() && u->getns() != ActNamespace::Global()) {
	      char *tmp = u->getns()->Name (true);
	      pp_printf (pp, "%s", tmp);
	      FREE (tmp);
	    }
	  }
	  vx->t->sPrint (buf, 10240);
	  pp_printf_raw (pp, "%s %s;\n", buf, vx->getName());
	}
	else {
	  /* emit the override */
	  has_overrides = 1;
	  emit_override (vx);
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
  int res;
  int braces = -1;
  if (!syn) return NULL;
  
  if (mode == 0) {
    pp_t *pp = syn->getPP ();

    res = syn->shouldSynthesize (p);

    if (res == NO_SYNTHESIS) {
      return NULL;
    }
    if (res == DUMMY_SYNTHESIS) {
      emit_refinement_header (syn, p, &braces, HEADER_NORMAL);
      syn->emitCloseNamespace (braces);
      return NULL;
    }

    if (p->hasNonStrict()) {
      char *s = _extract_strict_name (p);
      if (syn->recordProcessVariant (s, p)) {
	emit_refinement_header (syn, p, &braces, HEADER_DECL);
	pp_printf (pp, ";");
	pp_forced (pp, 0);
	syn->emitCloseNamespace (braces);
      }
      FREE (s);
      return NULL;
    }

    if (res == ACTUAL_SYNTHESIS) {
      if (!syn->checkSynth (ap, p)) {
	act_error_ctxt (stderr);
	fprintf (stderr, "Process [ %s ]: ", p->getFullName());
	syn->printSynthError (stderr);
	fprintf (stderr, "\n");
	exit (1);
      }
      syn->runPreSynth (ap, p);
      int v = emit_refinement_header (syn, p, &braces);
      syn->runSynth (ap, p);

      pp_endb (pp);
      pp_printf (pp, "/* end refine */");
      pp_forced (pp, 0);
      pp_printf (pp, "}");
      syn->emitCloseNamespace (braces);
    }
    else {
      Assert (res == TRIVIAL_SYNTHESIS, "What?");

      // syn->runPreSynth (ap, p);
      int v = emit_refinement_header (syn, p, &braces);
      // TODO: fix this hack maybe
      pp_printf (pp, "{ 42=42 : \"placeholder\" };");
      // print any of the existing language bodies that are here!
      if (p->getlang()) {
	pp_flush (pp);
	p->getlang()->Print (pp->fp);
      }
      pp_endb (pp);
      pp_forced (pp, 0);
      pp_printf (pp, "}");
      pp_forced (pp, 0);
    }
    syn->resetDecompVx();
    pp_endb (pp);
    pp_printf (pp, "/* end process */");
    pp_forced (pp, 0);
    pp_printf (pp, "}");
    syn->emitCloseNamespace (braces);
  }
  return NULL;
}

void _synth_emit_variant (ActPass *ap, const char *name, list_t *lprocs)
{
  ActSynthesize *syn = _init (ap);
  if (!syn) return;
  
  pp_t *pp = syn->getPP ();

  pp_forced (pp, 0);
  pp_printf (pp, "/* emit variant: %s */", name);
  pp_forced (pp, 0);

  int res = TRIVIAL_SYNTHESIS;

  for (listitem_t *li = list_first (lprocs); li; li = list_next (li)) {
    Process *p = (Process *) list_value (li);
    int tmp = syn->shouldSynthesize (p);
    if (tmp == ACTUAL_SYNTHESIS) {
      res = ACTUAL_SYNTHESIS;

      if (!syn->checkSynth (ap, p)) {
	act_error_ctxt (stderr);
	fprintf (stderr, "Process [ %s ]: ", p->getFullName());
	syn->printSynthError (stderr);
	fprintf (stderr, "\n");
	exit (1);
      }
      syn->runPreSynth (ap, p);
    }
    else {
      Assert (tmp == TRIVIAL_SYNTHESIS, "What?");
    }
  }
  
  if (res == ACTUAL_SYNTHESIS) {
    int braces = -1;
    Process *p = (Process *) list_value (list_first (lprocs));
    int v = emit_refinement_header (syn, p, &braces, HEADER_DECL_VARIANT);
    pp_forced (pp, 0);
    pp_printf (pp, "{ { false : \"Default override used, missing variant!\" }; }");
    pp_forced (pp, 0);
#if 0
    char *ns = NULL;
    if (p->getns() && p->getns() != ActNamespace::Global()) {
      ns = p->getns()->Name (true);
    }
#endif
    for (listitem_t *li = list_first (lprocs); li; li = list_next (li)) {
      char buf[10240];
      p = (Process *) list_value (li);
      pp_printf (pp, "| ");
      pp_setb (pp);
#if 0
      if (ns) {
	pp_printf (pp, "%s", ns);
      }
#endif
      p->snprintActName (buf, 10240);
      pp_printf (pp, "%s => {", buf);
      pp_forced (pp, 0);
      int tmp;
      emit_refinement_header (syn, p, &tmp, HEADER_DECLII);
      syn->runSynth (ap, p);
      pp_endb (pp);
      pp_printf (pp, "/* end refine */");
      pp_forced (pp, 0);
      pp_printf (pp, "}");
      pp_forced (pp, 0);

      syn->resetDecompVx();
      pp_endb (pp);
      pp_printf (pp, "/* end process variant */");
      pp_forced (pp, 0);
      pp_printf (pp, "}");
      pp_forced (pp, 0);
    }
    syn->emitCloseNamespace (braces);
  }
  else {
    Assert (res == TRIVIAL_SYNTHESIS, "What?");
    Assert (0, "FiXME!");
  }
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
      int braces = -1;
      int v = emit_refinement_header (syn, d, &braces);

      pp_endb (pp);
      pp_forced (pp, 0);
      pp_printf (pp, "}");
      syn->emitCloseNamespace (braces);
    }
  }
  return NULL;
}


void synthesis_run (ActPass *ap, Process *p)
{
  ActSynthesize *syn = _init (ap);
  if (syn) {
    if (syn->hasVariants ()) {
      syn->applyVariants (ap, _synth_emit_variant);
    }
    syn->finalSynthesis (p);
    delete syn;
    ActDynamicPass *dp = dynamic_cast<ActDynamicPass *>(ap);
    dp->setParam ("raw", (void *)NULL);
  }
}
