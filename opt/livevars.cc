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
#include "chp-opt.h"
#include "algos.h"

namespace ChpOptimize {


static
void run_seq (Sequence seq,
  std::unordered_map<const Block *, std::unordered_set<ChpOptimize::VarId> > &livein,
  std::unordered_map<const Block *, std::unordered_set<ChpOptimize::VarId> > &liveout,
  std::unordered_map<const Block *, UsesAndDefs> &raw)
{
  bool changed;
  Block *curr = seq.endseq->parent();

  do {
    changed = false;

    while (curr->type() != BlockType::StartSequence) {
      Sequence *s;

      // 1. Set liveout as the livein for the successor, if any.
      //
      // 2. The liveout for the the last element of a sequence:
      //       - top-level sequence: this is empty!
      //       - 
      //    For a nested block, the liveout for the last value must be
      //    set as the live-in for its successor. This must be manually
      //    propagated to the last element of the "sequence" (for a
      //    non-empty sequence)

      if (!liveout.contains(curr)) {
	liveout[curr] = std::unordered_set <VarId> ();
      }
      
      auto old_liveout = liveout[curr];

      if (curr->child()->type() != BlockType::EndSequence) {
	// the liveout for thet last block in a sequence is set by its
	// enclosing block
	liveout[curr] = livein[curr->child()];
      }

      switch (curr->type()) {
      case BlockType::Basic:
	{
	// livein = liveout + reads - writes
	  UsesAndDefs &ud = raw[curr];
	  auto newset = liveout[curr];
	  newset.merge (ud.var_reads);
	  newset = Algo::set_minus (newset, ud.var_writes);
	  if (newset != livein[curr]) {
	    changed = true;
	    livein[curr] = newset;
	  }
	}
	break;

      case BlockType::Par:
	{
	  auto old = livein[curr];

	  livein[curr].clear();
	  for (auto &branch : curr->u_par().branches) {
	    if (!branch.empty()) {
	      // propagate liveout to last value in the sequence
	      liveout[branch.endseq->parent()] = liveout[curr];
	      run_seq (branch, livein, liveout, raw);
	      livein[curr].merge (livein[branch.startseq->child()]);
	    }
	  }
	  if (old != livein[curr]) {
	    changed = true;
	  }
	}
	break;
	  
      case BlockType::Select:
	hassert(curr->u_select().splits.empty());
	hassert(curr->u_select().merges.empty());
	
	{
	  auto old = livein[curr];

	  livein[curr].clear();
	
	  for (auto &branch : curr->u_select().branches) {
	    s = &branch.seq;
	    if (!s->empty()) {
	      liveout[s->endseq->parent()] = liveout[curr];
	    }
	    run_seq (*s, livein, liveout, raw);
	    
	    livein[curr].merge (livein[s->startseq->child()]);
	    if (branch.g.type() == IRGuardType::Expression) {
	      addIdsUsedByExpr(livein[curr], branch.g.u_e().e);
	    }
	  }
	  if (old != livein[curr]) {
	    changed = true;
	  }
	}
      break;

      case BlockType::DoLoop:
	hassert(curr->u_doloop().in_phis.empty());
	hassert(curr->u_doloop().out_phis.empty());
	hassert(curr->u_doloop().loop_phis.empty());
      
	s = &curr->u_doloop().branch;
	if (!s->empty()) {
	  liveout[s->endseq->parent()] = liveout[curr];
	  liveout[s->endseq->parent()].merge (livein[s->startseq->child()]);
	  addIdsUsedByExpr(liveout[s->endseq->parent()],
			   curr->u_doloop().guard);
	  run_seq (*s, livein, liveout, raw);

	  if (livein[curr] != livein[s->startseq->child()]) {
	    changed = true;
	    livein[curr] = livein[s->startseq->child()];
	  }
	}
	break;

      case BlockType::StartSequence:
      case BlockType::EndSequence:
	hassert(false);
	break;
      }

      if (old_liveout != liveout[curr]) {
	changed = true;
      }
      
      curr = curr->parent();
    }
  }  while (changed);
}
  


std::unordered_map<const Block *, std::unordered_set<VarId> >
getLiveOutVars (const ChpGraph &graph)
{
  std::unordered_map<const Block *, std::unordered_set<VarId> > ret;
  std::unordered_map<const Block *, std::unordered_set<VarId> > livein;
  
  std::unordered_map<const Block *, UsesAndDefs> raw;

  if (graph.m_seq.empty()) {
    // empty graph!
    return ret;
  }
      
  raw = getDefUsesTable (graph);

#if 0
  auto pre = [&] (std::ostream &os, const Block &b) { return; };
  auto post = [&] (std::ostream &os, const Block &b)
    {
     if (raw.contains (&b)) {
       bool first;
       os << " {rd=";
       first = true;
       for (auto &v : raw[&b].var_reads) {
	 if (!first) {
	   os << ",";
	  }
	 os << "v" << v.m_id;
	 first = false;
       }
       first = true;
       os << " | wr=";
       for (auto &v : raw[&b].var_writes) {
	 if (!first) {
	   os << ",";
	 }
	 os << "v" << v.m_id;
	 first = false;
       }
       os << "}";
     }
    };
  print_chp (std::cout, graph, pre, post);
#endif  
  
  // initialize final liveout to an empty set
  ret[graph.m_seq.endseq->parent()] = std::unordered_set<VarId> ();

  // XXX: here
  run_seq (graph.m_seq, livein, ret, raw);

  return ret;
}

}
