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
  IdPool *id;
  std::unordered_map<ChanId, ActId *> name_from_chan;
  std::unordered_map<VarId, ActId *> name_from_var;
  Scope *sc;
  int sc_chan, sc_var;

  std::vector<ActId *> newvars;

  var_to_actvar(Scope *s, IdPool *id_)
    : sc{s}, sc_var{0}, id{id_}, sc_chan{0} 
  {}

  bool isBool (const VarId &v) {
    return id->getIsBool (v);
  }
  
  bool isBool (const ChanId &ch) {
    return id->getIsBool (ch);
  }

  ActId *chanMap (const ChanId &ch) {
    const char *chan_prefix = "_ch";
    static char buf[100];
    if (name_from_chan.contains(ch)) {
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
					   const_expr (id->getBitwidth(ch)));
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
    if (name_from_var.contains(v)) {
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
					   const_expr (id->getBitwidth(v)));
    }
    it = it->Expand (NULL, sc);
    sc->Add (buf, it);
    name_from_var[v] = new ActId (buf);

    newvars.push_back (name_from_var[v]);
    
    return name_from_var[v];
  }
};

}
