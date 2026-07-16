#pragma once
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

#include <act/act.h>
#include <act/expr.h>
#include "chp-graph.h"

namespace ChpOptimize {

/*
  Namespace manager
*/
struct var_to_actvar {
  const IdPool &id;
  std::unordered_map<ChanId, ActId *> name_from_chan;
  std::unordered_map<VarId, ActId *> name_from_var;
  std::unordered_map<ActId *, ActId *> structname_from_name;
  Scope *sc;
  int sc_chan, sc_var;

  std::vector<ActId *> newvars;

  var_to_actvar(Scope *s, const IdPool &id_)
    : sc{s}, sc_var{0}, id{id_}, sc_chan{0} 
  {}

  bool isBool (const VarId &v) const {
    return id.getIsBool (v);
  }
  
  bool isBool (const ChanId &ch) const {
    return id.getIsBool (ch);
  }

  bool isStruct (const ChanId &ch) const {
    return id.getIsStruct (ch);
  }

  bool ActIdIsPureStructChan (ActId *chanid) const {
    Assert (chanid, "what");
    if (!sc) return false; 
    auto cit = sc->FullLookup(chanid, NULL);
    if (!TypeFactory::isChanType(cit)) {
      return false;
    }
    auto it = TypeFactory::getChanDataType(cit);
    return TypeFactory::isPureStruct (it);
  }

  // create struct(e) from e
  Expr *wrap_in_struct (Expr *e, ChanId chan) {
    Expr *e1;
    NEW (e1, Expr);
    e1->type = E_FUNCTION;
    auto cid = chanMap(chan);
    auto cit = sc->FullLookup(cid, NULL);
    auto it = TypeFactory::getChanDataType(cit);
    auto dx = dynamic_cast<Data *>(it->BaseType());
    hassert (dx);
    char nmu[4096];
    char nm[4096];
    dx->snprintActName(nm,4096);
    dx->getUnexpanded()->snprintActName(nmu,4096);
    if (!dx->getMacro(nm)) {
      dx->getUnexpanded()->setName(nm);
      dx->synthStructMacro();
      dx->getUnexpanded()->setName(nmu);
    }
    auto um = dx->getMacro(nm);
    hassert (um);
    auto f = um->getFunction();
    hassert (f);
    e1->u.fn.s = (char *)f;
    NEW (e1->u.fn.r, Expr);
    e1->u.fn.r->type = E_LT;
    e1->u.fn.r->u.e.r = NULL;
    e1->u.fn.r->u.e.l = expr_dup(e);
    return e1;
  }

  // create int(x) from x
  Expr *wrap_in_int (ActId *chan_id) const {
    auto cit = sc->Lookup(chan_id);
    auto it = TypeFactory::getChanDataType(cit);
    auto dx = dynamic_cast<Data *>(it->BaseType());
    hassert(dx);
    auto um = dx->getMacro("int");
    if (!um) {
      um = dx->newMacro(string_cache("int"));
	    um->mkBuiltin();
	    um->setRetType(TypeFactory::Factory()->NewInt(
          sc,Type::direction::NONE,0,const_expr(32)));
	    um->getRetType()->MkCached();
	  }
    hassert(um);
    Expr *e;
    NEW (e, Expr);
    e->type = E_USERMACRO;
    e->u.fn.s = (char *)um;
    e->u.fn.r = act_expr_var(chan_id);
    return e;
  }

  ActId *intOfStructVar (const VarId &v, const ChanId &ch) {
    auto cid = chanMap(ch);
    auto cit = sc->FullLookup(cid, NULL);
    static char buf[100], buf1[100];
    const char *var_prefix = "_va";
    const char *svar_prefix = "_sva";

    if (name_from_var.count(v)) {
      if (structname_from_name.count(name_from_var[v]))
        return name_from_var[v]->Clone();
    }

    auto it = TypeFactory::Factory()->NewInt(sc, Type::NONE, 0, const_expr (id.getBitwidth(v)));
    it = it->Expand (NULL, sc);
    do {
      snprintf (buf, 100, "%s%d", var_prefix, sc_var++);
    } while (sc->Lookup (buf));
    sc->Add (buf, it);
    name_from_var[v] = new ActId (buf);
    newvars.push_back (name_from_var[v]);

    it = TypeFactory::Factory()->NewUserDef(sc, TypeFactory::getChanDataType(cit));
    it = it->Expand (NULL, sc);
    snprintf (buf1, 100, "%s%d", svar_prefix, sc_var-1);
    sc->Add (buf1, it);
    auto sid = new ActId (buf1);
    structname_from_name[name_from_var[v]] = sid;
    newvars.push_back(sid);

    return name_from_var[v];
  }

  ActId *structVar (ActId *ivar) {
    for ( auto [x,y] : structname_from_name ) {
      if (x->isEqual(ivar)) return y;
    } 
    hassert (false);
    return nullptr;
  }

  ActId *chanMap (const ChanId &ch) {
    // const char *chan_prefix = "_ch";
    int ref = config_get_int("act.refine_steps");
    static char chan_prefix[20];
    snprintf (chan_prefix, 20, "_ch_%d_",ref);
    static char buf[100];
    if (name_from_chan.count(ch)) {
      return name_from_chan[ch]->Clone();
    }
    do {
      snprintf (buf, 100, "%s%d", chan_prefix, sc_chan++);
    } while (sc->Lookup (buf));

    InstType *it;

    if (isBool (ch)) {
      it = TypeFactory::Factory()->NewBool (Type::NONE);
    }
    else {
      it = TypeFactory::Factory()->NewInt (sc, Type::NONE, 0,
					   const_expr (id.getBitwidth(ch)));
    }
    it = TypeFactory::Factory()->NewChan (sc, Type::NONE, it, NULL);
    it = it->Expand (NULL, sc);
    sc->Add (buf, it);
    name_from_chan[ch] = new ActId (buf);

    newvars.push_back (name_from_chan[ch]);
    
    return name_from_chan[ch];
  }

  ActId *varMap (const VarId &v) {
    const char *var_prefix = "_va";
    static char buf[100];
    if (name_from_var.count(v)) {
      return name_from_var[v]->Clone();
    }
    do {
      snprintf (buf, 100, "%s%d", var_prefix, sc_var++);
    } while (sc->Lookup (buf));

    InstType *it;
    if (isBool (v)) {
      it = TypeFactory::Factory()->NewBool (Type::NONE);
    }
    else {
      it = TypeFactory::Factory()->NewInt (sc, Type::NONE, 0,
					   const_expr (id.getBitwidth(v)));
    }
    it = it->Expand (NULL, sc);
    sc->Add (buf, it);
    name_from_var[v] = new ActId (buf);

    newvars.push_back (name_from_var[v]);
    
    return name_from_var[v];
  }
};

}
