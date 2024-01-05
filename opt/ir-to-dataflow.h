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

namespace ChpOptimize {

using DExpr = IRExpr<ChpTag, ChanId, ManageMemory::yes>;
using DExprDag = IRExprDag<ChpTag, ChanId>;
using DExprSingleRootDag = IRExprSingleRootDag<ChpTag, ChanId>;


enum class DataflowKind { Func, Split, MergeMix, Arbiter, Sink, Init
			 //, Cluster?
			 };

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

private:
  using Variant_t =
    TypedVariant6<DataflowKind,
		  Func, DataflowKind::Func,
		  Init, DataflowKind::Init,
		  Split, DataflowKind::Split,
		  MergeMix, DataflowKind::MergeMix,
		  Arbiter, DataflowKind::Arbiter,
		  Sink, DataflowKind::Sink>;

  Variant_t u;

  explicit Dataflow (Variant_t u_) : u{std::move(u_)} {}

public:
  [[nodiscard]] Func &u_func() { return u.u_v0(); }
  [[nodiscard]] Init &u_init() { return u.u_v1(); }
  [[nodiscard]] Split &u_split() { return u.u_v2(); }
  [[nodiscard]] MergeMix &u_mergemix() { return u.u_v3(); }
  [[nodiscard]] Arbiter &u_arbiter() { return u.u_v4(); }
  [[nodiscard]] Sink &u_sink() { return u.u_v5(); }

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
  
  
};


// This function takes in a chp-graph in "static token form" and creates a
// dataflow graph. The entire chp program (including functions in other
// processes) is required to have "slack elasticity", meaning that one could add
// an arbitrary number of buffers on any channel without changing the
// computation. No probes and no shared variables is one way to guarantee that.
// We use an improved algorithm to handle multiple channel access.
std::vector<Dataflow> chp_to_dataflow(const ChpGraph &chp);
} // namespace ChpOptimize
