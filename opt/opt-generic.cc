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
#include "opt-generic.h"

namespace ChpOptimize {

template<class L>
Analysis<L>::Analysis(const char *name, FlowDirection f)
{
  _f = f;
  _nm = Strdup (name);
  _vals.clear();
}

template<class L>
void Analysis<L>::init (const IdPool &ids)
{
  _vals.clear();
  for (size_t i=1; i < ids.varNum(); i++) {
    _vals[VarId(i)] = L::bot();
  }
}


template<class L>
bool Analysis<L>::run(std::vector<Block *> &blks)
{
  /* Note: This only works once we are in static token form */
  
  bool changed = true;
  bool ret = false;
  while (changed) {
    changed = false;
    for (Block *_b : blks) {
      switch (_b->type()) {
      case BlockType::Basic: {
	auto &stmt = _b->u_basic().stmt;
	switch (stmt.type()) {
	case StatementType::Assign: {
	  std::unordered_map<ChpExprDag::Node *, L> _eval;
	  // evaluate the dag expression, caching the result in the
	  // _eval map and using the _vals map as the base case for
	  // any variables.
	  L::eval (_eval, _vals, stmt.u_assign().e);
	  int pos = 0;
	  // update the map for all vars on the lhs
	  for (auto &id : stmt.u_assign().ids) {
	    L result = L::merge (_vals[id],
				  _eval[stmt.u_assign().e.roots[pos]]);
	    if (result != _vals[id]) {
	      _vals[id] = result;
	      changed = true;
	    }
	    pos++;
	  }
	  break;
	}
	case StatementType::Send:
	  // nothing to do here
	  break;
	case StatementType::Receive:
	  if (stmt.u_receive().var) {
	    if (_vals[*stmt.u_receive().var] != L::top) {
	      _vals[*stmt.u_receive().var] = L::top();
	      changed = true;
	    }
	  }
	  break;
	}
	break;
      }

      case BlockType::Par:
	// we don't need to do anything here, since we are operating
	// on the flat block list
	// No need for splits and merges here in real static token form.
	hassert (_b->u_par().splits.empty());
	hassert (_b->u_par().merges.empty());
	break;

      case BlockType::Select:
	// we need to manage phi and phi_inv functions here
	for (auto &split : _b->u_select().splits) {
	  for (auto &br : split.branch_ids) {
	    if (br) {
	      if (_vals[*br] != _vals[split.pre_id]) {
		changed = true;
		_vals[*br] = _vals[split.pre_id];
	      }
	    }
	  }
	}
	for (auto &merge : _b->u_select().merges) {
	  auto res = L::bot();
	  for (auto &br : merge.branch_ids) {
	    res = L::merge (res, _vals[br]);
	  }
	  if (res != _vals[merge.post_id]) {
	    changed = true;
	    _vals[merge.post_id] = res;
	  }
	}
	break;

      case BlockType::DoLoop:
	// handle phis
	for (auto &inphi : _b->u_doloop().in_phis) {
	  if (_vals[inphi.bodyin_id] != _vals[inphi.pre_id]) {
	    changed = true;
	    _vals[inphi.bodyin_id] = _vals[inphi.pre_id];
	  }
	}
	for (auto &outphi : _b->u_doloop().out_phis) {
	  if (_vals[outphi.post_id] != _vals[outphi.bodyout_id]) {
	    changed = true;
	    _vals[outphi.post_id] = _vals[outphi.bodyout_id];
	  }
	}
	for (auto &loopphi : _b->u_doloop().loop_phis) {
	  auto res = L::merge (_vals[loopphi.pre_id],
			       _vals[loopphi.bodyout_id]);
	  if (res != _vals[loopphi.bodyin_id]) {
	    changed = true;
	    _vals[loopphi.bodyin_id] = res;
	  }
	  if (loopphi.post_id && (res != _vals[*loopphi.post_id])) {
	    changed = true;
	    _vals[*loopphi.post_id] = res;
	  }
	}
	break;
	
      case BlockType::StartSequence:
      case BlockType::EndSequence:
	hassert (false);
	break;
      }
    }
    ret = ret || changed;
  }
  return ret;
}


 MultiAnalysis::MultiAnalysis (ChpGraph &c) : _prog(c)
{
  _liveblocks_fwd.clear();
  _liveblocks_rev.clear();
}

MultiAnalysis::~MultiAnalysis()
{
  _liveblocks_fwd.clear ();
  _liveblocks_rev.clear ();
}

template<class L>
bool MultiAnalysis::runAnalysis (Analysis<L> *a, bool first)
{
  if (!a) return false;
  if (first) {
    a->init (_prog.id_pool());
  }
  if (a->getDirection() == FlowDirection::forward) {
    if (_liveblocks_fwd.empty()) {
      _liveblocks_fwd = _prog.makeReversePostorder (FlowDirection::forward);
    }
    return a->run (_liveblocks_fwd);
  }
  else {
    if (_liveblocks_rev.empty()) {
      _liveblocks_rev = _prog.makeReversePostorder (FlowDirection::backward);
    }
    return a->run (_liveblocks_rev);
  }
}

}
