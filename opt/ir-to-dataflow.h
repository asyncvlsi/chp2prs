#pragma once
/*************************************************************************
 *
 *  This file is part of the ACT library
 *
 *  Copyright (c) 2021-2022 Henry Heffan
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

#include "chp-graph.h"
#include "algos.h"
#include <act/lang.h>

namespace ChpOptimize {

using DExpr = IRExpr<ChpTag, ChanId, ManageMemory::no>;
using DExprDag = IRExprDag<ChpTag, ChanId>;
using DExprSingleRootDag = IRExprSingleRootDag<ChpTag, ChanId>;

enum class DataflowKind { Func, Split, MergeMix, Arbiter, Sink, Init,
			  Instance // not really dataflow!
			 //, Cluster?
			 };

void printDataflowExpr (std::ostream &os, const DExpr &d);

struct Dataflow {
public:
  struct Func {
    std::vector<ChanId> ids;
    DExprDag e;

    Func(std::vector<ChanId> id_, DExprDag e_) :
      ids{std::move(id_)}, e{std::move(e_)} {}
    Func() = default;
    ~Func() = default;
    Func(Func &&o) = default;
    Func &operator=(Func &&o) = default;
    Func (Func &f_) : ids{f_.ids}, e{DExprDag::deep_copy(f_.e)} {}
    Func &operator=(Func &f_) {
      ids = f_.ids;
      e = DExprDag::deep_copy(f_.e);
      return *this;
    }
  };

  struct Init {
    BigInt v;
    int v_width;
    ChanId lhs, rhs;
  };

  struct Split {
    ChanId cond_id;
    ChanId in_id;
    std::vector<OptionalChanId> out_ids;
  };

  struct MergeMix {
    OptionalChanId cond_id;
    std::vector<ChanId> in_ids;
    ChanId out_id;
  };

  struct Arbiter {
    std::vector<ChanId> in_ids;
    ChanId out_id;
    OptionalChanId sel_id;
  };

  struct Sink {
    ChanId in_id;
  };

  struct Instance {
    // seq merging
    //   . takes N control channels (in)
    //   . takes 1 control channel (out)
    //   . generates one control (0, 1, ..., N-1) used for channel
    //     split/merge

    // selection merging
    //   . takes N control channels as input
    //   . takes guard value as input (0..,N-1)
    //   . generates one control output
    //   . generates guard value for split/merge of data

    // loop merging
    //   . takes 1 control channel and 1 guard channel
    //   . generates control output
    int type;			// 0 = seq, 1 = sel, 2 = loop, > 2 = RR

    std::vector<ChanId> ctrl;	// control channels
    OptionalChanId ctrl_out;	// output control (optional)
    
    OptionalChanId guard;	// guard id, if any [select and loop]
    OptionalChanId sm_sel;	// split/merge select
  };
    

private:
  using Variant_t =
    TypedVariant7<DataflowKind,
		  Func, DataflowKind::Func,
		  Init, DataflowKind::Init,
		  Split, DataflowKind::Split,
		  MergeMix, DataflowKind::MergeMix,
		  Arbiter, DataflowKind::Arbiter,
		  Sink, DataflowKind::Sink,
		  Instance, DataflowKind::Instance>;

  explicit Dataflow (Variant_t u_) : u{std::move(u_)} {}

public:
  Variant_t u;

  [[nodiscard]] Func &u_func() { return u.u_v0(); }
  [[nodiscard]] Init &u_init() { return u.u_v1(); }
  [[nodiscard]] Split &u_split() { return u.u_v2(); }
  [[nodiscard]] MergeMix &u_mergemix() { return u.u_v3(); }
  [[nodiscard]] Arbiter &u_arbiter() { return u.u_v4(); }
  [[nodiscard]] Sink &u_sink() { return u.u_v5(); }
  [[nodiscard]] Instance &u_inst() { return u.u_v6(); }

  Dataflow() = default;
  ~Dataflow() = default;
  Dataflow(Dataflow &&d) noexcept = default;
  Dataflow &operator=(Dataflow &&) noexcept = default;
  Dataflow(Dataflow &d) = delete;
  Dataflow &operator=(Dataflow &) = delete;

  static Dataflow mkFunc (std::vector<ChanId> id_, DExprDag e_) {
    return Dataflow{Variant_t{Func{id_, std::move(e_)}}};
  }

  static Dataflow mkInit (ChanId inp, ChanId outp, BigInt val, int width) {
    return Dataflow{Variant_t{Init{val, width, inp,outp}}};
  }

  static Dataflow mkSplit(ChanId cond, ChanId inp, std::vector<OptionalChanId> out) {
    return Dataflow{Variant_t{Split{cond,inp,out}}};
  }

  static Dataflow mkMergeMix (OptionalChanId cond, std::vector<ChanId> inp, ChanId out) {
    return Dataflow{Variant_t{MergeMix{cond,inp,out}}};
  }

  static Dataflow mkArbiter (std::vector<ChanId> inp, ChanId out,
			     OptionalChanId which) {
    return Dataflow{Variant_t{Arbiter{inp,out,which}}};
  }

  static Dataflow mkSink (ChanId ch) {
    return Dataflow{Variant_t{Sink{ch}}};
  }

  static Dataflow mkInstSeq (std::vector<ChanId> cin,
			     OptionalChanId cout,
			     ChanId sel)
  {
    return Dataflow{Variant_t(Instance{0, cin, cout,
				       OptionalChanId::null_id(),
				       sel})};
  }

  static Dataflow mkInstSeqRR (int N,
			       OptionalChanId cout,
			       ChanId sel)
  {
    std::vector<ChanId> ctmp;
    ctmp.push_back (sel);
    return Dataflow{Variant_t(Instance{N+2, ctmp, cout,
				       OptionalChanId::null_id(),
				       sel})};
  }

  static Dataflow mkInstSel (std::vector<ChanId> cin,
			     ChanId cout,
			     ChanId guard,
			     OptionalChanId sel)
  {
    return Dataflow{Variant_t(Instance{1, cin, cout,
				       guard,
				       sel})};
  }
  
  static Dataflow mkInstDoLoop (ChanId cin,
				ChanId cout,
				ChanId guard)
  {
    std::vector<ChanId> ch;
    ch.push_back (cin);
    return Dataflow{Variant_t(Instance{2, ch, cout,
				       guard,
				       OptionalChanId::null_id()})};
  }

  void Print (std::ostream &os) {
    switch (u.type()) {
    case DataflowKind::Func:
      {
	if (u_func().ids.size() > 1) {
	  os << "dataflow_cluster {" << std::endl;
	}
	for (int i=0; i < u_func().ids.size(); i++) {
	  printDataflowExpr (os, *u_func().e.roots[i]);
	  os << " -> C";
	  os << u_func().ids[i].m_id << std::endl;
	}
	if (u_func().ids.size() > 1) {
	  os << "}" << std::endl;
	}
      }
      break;

    case DataflowKind::Init:
      os << "C" << u_init().lhs.m_id << " -> ["
	 << u_init().v.to_hex_string().c_str() << "]"
	 << "C" << u_init().rhs.m_id
	 << std::endl;
      break;

    case DataflowKind::Split:
      os << "{C" << u_split().cond_id.m_id << "} "
	 << "C" << u_split().in_id.m_id << " -> "
	 << Algo::join_str_mapped (u_split().out_ids,
				   [&] (const OptionalChanId &x) {
				     return (x) ?
				       string_format ("C%d", (*x).m_id) :
				       "*";
				   }, ",")
	 << std::endl;
      break;

    case DataflowKind::MergeMix:
      os << "{" <<
	(u_mergemix().cond_id ?
	 string_format ("C%d", (*u_mergemix().cond_id).m_id) : "*") << "} "
	 << Algo::join_str_mapped (u_mergemix().in_ids,
				   [&] (const ChanId &x) {
				     return string_format ("C%d", x.m_id);
				   }, ",")
	 << " -> C" << u_mergemix().out_id.m_id
	 << std::endl;
      break;

    case DataflowKind::Arbiter:
      os << "{|} " 
	 << Algo::join_str_mapped (u_arbiter().in_ids,
				   [&] (const ChanId &x) {
				     return string_format ("C%d", x.m_id);
				   }, ",")
	 << " -> C" << u_arbiter().out_id.m_id;
      if (u_arbiter().sel_id) {
	os << ", C" << (*u_arbiter().sel_id).m_id;
      }
      os << std::endl;
      break;

    case DataflowKind::Sink:
      os << "C" << u_sink().in_id.m_id << " -> *"
	 << std::endl;
      break;

    case DataflowKind::Instance:
      if (u_inst().type > 2) {
	os << "inst_RR" << (u_inst().ctrl_out ? "" : "_noctrl")
	   << "<" << u_inst().type - 2 << ">"
	   << "(";
	if (u_inst().ctrl_out) {
	  os << "C" << (*u_inst().ctrl_out).m_id << ", ";
	}
	os << "C" << (*u_inst().sm_sel).m_id << ")";
      }
      else {
	os << "inst_" << (u_inst().type == 0 ? "seq" :
			  u_inst().type == 1 ? "sel" : "loop");
	if (!(u_inst().ctrl_out)) {
	  os << "_noctrl";
	}
	if (!(u_inst().sm_sel)) {
	  os << "_nosel";
	}
	os << (u_inst().type == 2 ? "" :
	       string_format("<%d>", u_inst().ctrl.size()))
	   << "(";
	if (u_inst().type == 2) {
	  os << "C" << u_inst().ctrl[0].m_id << ", "
	     << "C" << (*u_inst().guard).m_id << ", "
	     << "C" << (*u_inst().ctrl_out).m_id << ")";
	}
	else {
	  bool first = true;
	  os << "{";
	  for (auto ch : u_inst().ctrl) {
	    if (!first) {
	      os << ",";
	    }
	    os << "C" << ch.m_id;
	    first = false;
	  }
	  os << "}";
	  if (u_inst().ctrl_out) {
	    os << ", C" << (*u_inst().ctrl_out).m_id;
	  }
	  if (u_inst().guard) {
	    os << ", C" << (*u_inst().guard).m_id;
	  }
	  if (u_inst().sm_sel) {
	    os << ", C" << (*u_inst().sm_sel).m_id;
	  }
	  os << ")";
	}
      }
      os << std::endl;
      break;
    }
  }
};


// This function takes in a chp-graph in "static token form" and creates a
// dataflow graph. The entire chp program (including functions in other
// processes) is required to have "slack elasticity", meaning that one could add
// an arbitrary number of buffers on any channel without changing the
// computation. No probes and no shared variables is one way to guarantee that.
// We use an improved algorithm to handle multiple channel access.
std::vector<Dataflow> chp_to_dataflow(ChpGraph &chp);
act_dataflow *dataflow_to_act (std::vector<Dataflow> &d,
			       GraphWithChanNames &gr,
			       std::vector<ActId *> &newnames,
			       Scope *s);
  
} // namespace ChpOptimize
