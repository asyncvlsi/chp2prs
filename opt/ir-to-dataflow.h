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
#include "utils.h"
#include <act/lang.h>

namespace ChpOptimize {

using DExpr = IRExpr<ChpTag, ChanId, ChanId, ManageMemory::no>;
using DExprDag = IRExprDag<ChpTag, ChanId, ChanId>;
using DExprSingleRootDag = IRExprSingleRootDag<ChpTag, ChanId, ChanId>;

enum class DataflowKind { Func, Split, MergeMix, Arbiter, Sink, Init,
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

  bool keep;
  bool dead;
  const char *msg;

private:
  using Variant_t =
    TypedVariant6<DataflowKind,
		  Func, DataflowKind::Func,
		  Init, DataflowKind::Init,
		  Split, DataflowKind::Split,
		  MergeMix, DataflowKind::MergeMix,
		  Arbiter, DataflowKind::Arbiter,
		  Sink, DataflowKind::Sink>;

  explicit Dataflow (Variant_t u_) : u{std::move(u_)} { keep = false; dead = false; msg = NULL; }

public:
  Variant_t u;

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

  static Dataflow mkSrc (ChanId id, BigInt val, int width) {
    std::vector<ChanId> ids;
    ids.push_back (id);
    DExprDag dg;
    DExprDag::Node *n = dg.newNode (DExprDag::Node::makeConstant (val, width));
    dg.roots.push_back (n);
    return Dataflow{Variant_t{Func{ids, std::move(dg)}}};
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

  static Dataflow mkBuf (ChanId ch_in, ChanId ch_out, int bw) {
    DExprDag cp;
    DExprDag::Node *n =
      cp.newNode (DExprDag::Node::makeVariableAccess (ch_in, bw));
    cp.roots.push_back (n);
    std::vector<ChanId> inlist;
    inlist.clear ();
    inlist.push_back (ch_out);
    Dataflow dtmp = Dataflow::mkFunc (inlist, std::move (cp));
    dtmp.keep = true;
    return dtmp;
  }

  static DExprDag::Node *helper_or (DExprDag &d,
				    DExprDag::Node *n1,
				    DExprDag::Node *n2)
  {
    return
      d.newNode (DExprDag::Node::makeBinaryOp (IRBinaryOpType::Or, n1, n2));
  }
					       
  static DExprDag::Node *helper_and (DExprDag &d,
				    DExprDag::Node *n1,
				    DExprDag::Node *n2)
  {
    return
      d.newNode (DExprDag::Node::makeBinaryOp (IRBinaryOpType::And, n1, n2));
  }
  
  /* return ch = val */
  static DExprDag::Node *helper_eq (DExprDag &d,
			     ChanId ch,
			     int bw,
			     int val)
  {
    DExprDag::Node *n =
      d.newNode (DExprDag::Node::makeBinaryOp (IRBinaryOpType::EQ,
					       d.newNode (DExprDag::Node::makeVariableAccess (ch, bw)),
					       d.newNode (DExprDag::Node::makeConstant (BigInt (val), bw))));
    return n;
  }

  /* return ch != val */
  static DExprDag::Node *helper_ne (DExprDag &d,
			     ChanId ch,
			     int bw,
			     int val)
  {
    DExprDag::Node *n =
      d.newNode (DExprDag::Node::makeBinaryOp (IRBinaryOpType::NE,
					       d.newNode (DExprDag::Node::makeVariableAccess (ch, bw)),
					       d.newNode (DExprDag::Node::makeConstant (BigInt (val), bw))));
    return n;
  }

  static DExprDag::Node *helper_const (DExprDag &d,
				       int val,
				       int bw)
  {
    return d.newNode (DExprDag::Node::makeConstant (BigInt (val), bw));
  }

  static DExprDag::Node *helper_var (DExprDag &d,
				     ChanId var,
				     int bw)
  {
    return d.newNode (DExprDag::Node::makeVariableAccess (var, bw));
  }
  
  static DExprDag::Node *helper_query (DExprDag &d,
				       DExprDag::Node *n1,
				       DExprDag::Node *n2,
				       DExprDag::Node *n3)
  {
    return d.newNode (DExprDag::Node::makeQuery (n1, n2, n3));
  }

  /* return x + 1 mod N */
  static DExprDag::Node *helper_inc (DExprDag &d,
			      ChanId ch,
			      int bw,
			      int N)
  {
    DExprDag::Node *n =
      d.newNode (DExprDag::Node::makeQuery
		 (d.newNode (DExprDag::Node::makeBinaryOp (IRBinaryOpType::EQ,
							   d.newNode (DExprDag::Node::makeVariableAccess (ch, bw)),
							   d.newNode (DExprDag::Node::makeConstant (BigInt (N-1), bw)))),
		  d.newNode (DExprDag::Node::makeConstant (BigInt (0), bw)),
		  d.newNode (DExprDag::Node::makeResize (
		  d.newNode (DExprDag::Node::makeBinaryOp (IRBinaryOpType::Plus,
							   d.newNode (DExprDag::Node::makeVariableAccess (ch, bw)),
							   d.newNode (DExprDag::Node::makeConstant (BigInt (1), bw)))),
		  bw))));
    return n;
  }
    


  static std::list<Dataflow> mkInstSeqRR (int N,
					  OptionalChanId cout,
					  ChanId sel,
					  std::function<ChanId (ChanId &, int)> fresh)
  {
    std::list<Dataflow> ret;

    /*
       f -> [0] sel
       sel = N-1 ? 0 : sel + 1 -> f

       if there is a cout:

       g -> [0] gint
       gint = N ? 0 : gint + 1 -> g
       g = N ? 0 : 1 -> cout
    */

    int bw = log_2_round_up (N);

    // f -> [0] sel
    ChanId f = fresh(sel, 0);
    ret.push_back (Dataflow::mkInit(f, sel, BigInt(0), bw));

    // sel = N-1 ? 0 : sel + 1 -> f
    {
      DExprDag dg;
      DExprDag::Node *n = helper_inc (dg, sel, bw, N);
      dg.roots.push_back (n);
      
      std::vector<ChanId> ids;
      ids.push_back(f);
      ret.push_back (Dataflow::mkFunc (ids, std::move (dg)));
    }

    if (cout) {
      ChanId g, gint;

      bw = log_2_round_up (N+1);
      
      g = fresh (sel, bw);
      gint = fresh (g,0);

      // g -> [0] gint
      ret.push_back (Dataflow::mkInit (g, gint, BigInt (0), bw));
      
      // gint = N ? 0 : gint + 1 -> g
      DExprDag dg;
      DExprDag::Node *n = helper_inc (dg, gint, bw, N+1);
      dg.roots.push_back (n);
      std::vector<ChanId> ids;
      ids.push_back (g);
      ret.push_back (Dataflow::mkFunc (ids, std::move (dg)));

      //   gint = N ? 0 : 1 -> cout
      DExprDag dg2;
      n = helper_query (dg2, helper_eq (dg2, gint, bw, N),
			helper_const (dg2, 0, 1),
			helper_const (dg2, 1, 1));
      dg2.roots.push_back(n);
      ids.clear ();
      ids.push_back(*cout);
      ret.push_back (Dataflow::mkFunc (ids, std::move (dg2)));
    }
    
    return ret;
  }

  static std::list<Dataflow>
  mkInstSeq (std::vector<ChanId> cin,
	     OptionalChanId cout,
	     ChanId sel,
	     std::function<ChanId (const OptionalChanId &)> fresh)
  {
    /*
      1. {s_top} B0, ..., BN-1 -> Bin

      2. {Bin} s_top -> *, sel   // if Bin = 1 then send out s

      3. Bin = 0 & s_top = N-1 ? 0 : (s_top + (Bin ? 0 : 1)) -> news

      4. news -> [0] s_top

      5. news = 0 | Bin = 1 -> bcond
      6. {bcond} Bin -> *, Bout
    */

    std::list<Dataflow> ret;

    int bw = log_2_round_up (cin.size());

    ChanId s_top = fresh (sel);
    ChanId bin = fresh (cin[0]);

    // 1.
    ret.push_back (mkMergeMix (s_top, cin, bin));

    // 2.
    {
      std::vector<OptionalChanId> out;
      out.push_back (OptionalChanId::null_id());
      out.push_back (sel);
      ret.push_back (mkSplit (bin, s_top, out));
    }

    ChanId news = fresh (sel);

    // 3.
    {
      DExprDag dg;
      DExprDag::Node *n =
	helper_query (dg,
		      helper_and (dg,
				  helper_eq (dg, bin, 1, 0),
				  helper_eq (dg, s_top, bw,
					     cin.size()-1)),
		      helper_const (dg, 0, bw),
		      dg.newNode
		      (DExprDag::Node::makeResize (
		     dg.newNode(
			DExprDag::Node::makeBinaryOp (IRBinaryOpType::Plus,
				  helper_var (dg, s_top, bw),
				  helper_query (dg,
						helper_var (dg, bin, 1),
						helper_const (dg, 0, 1),
						helper_const (dg, 1, 1))
						      )),
		      bw)));
      dg.roots.push_back (n);
      std::vector<ChanId> id;
      id.push_back (news);
      ret.push_back (mkFunc (id, std::move (dg)));
    }

    // 4.
    ret.push_back (mkInit (news, s_top, BigInt (0), bw));

    if (cout) {
      ChanId bcond = fresh (OptionalChanId::null_id());
      /*
	5. news = 0 | Bin = 1 -> bcond
      */
      {
	DExprDag dg;
	DExprDag::Node *n =
	  helper_or (dg, helper_eq (dg, news, bw, 0),
		     helper_eq (dg, bin, 1, 1));
	dg.roots.push_back (n);
	std::vector<ChanId> id;
	id.push_back (bcond);
	ret.push_back (mkFunc (id, std::move (dg)));
      }
      /*
	6. {bcond} Bin -> *, Bout
      */
      {
	std::vector<OptionalChanId> out;
	out.push_back (OptionalChanId::null_id());
	out.push_back (cout);
	ret.push_back (mkSplit (bcond, bin, out));
      }
    }

    return ret;
  }



	 
  static std::list<Dataflow> mkInstSel (std::vector<ChanId> cin,
			     ChanId cout,
			     ChanId guard,
			     OptionalChanId sel,
			     int selw,
			     std::function<ChanId (const OptionalChanId &)> fresh, bool swap)
  {
    /*
      1.  bintVal -> [0] i_bint
      2. {i_bint} guard, LoopGuard -> gval

      3. {gval} B1, ..., BN -> cout

      4.  cout = 1 -> bintVal
   
      5. {bintVal} gval -> *, Selout
      6. {bintVal} gval -> *, LoopGuard
    */
    std::list<Dataflow> ret;

    ChanId bintV = fresh (OptionalChanId::null_id());
    ChanId i_bint = fresh (OptionalChanId{bintV});

    if (swap) {
      ChanId c0 = cin[0];
      ChanId c1 = cin[1];
      cin[0] = c1;
      cin[1] = c0;
    }

    /* 1.  bintVal -> [0] i_bint */
    ret.push_back (Dataflow::mkInit (bintV, i_bint, BigInt(0), 1));

    /* 2. {i_bint} guard, LoopGuard -> gval */
    ChanId loopguard;
    if (sel && selw == log_2_round_up(cin.size())) {
      loopguard = *sel;
    }
    else {
      loopguard = fresh (guard);
    }
    ChanId gval = fresh (guard);
    {
      ChanId buf_loopg = fresh (guard);
      std::vector<ChanId> in;
      in.push_back (guard);
      in.push_back (buf_loopg);
      ret.push_back (Dataflow::mkMergeMix (i_bint, in, gval));

      ret.push_back (Dataflow::mkBuf (loopguard, buf_loopg,
				      log_2_round_up (cin.size())));
    }

    /* 3. {gval} B1, ..., BN -> cout */
    ret.push_back (Dataflow::mkMergeMix (gval, cin, cout));


    /* 4.  cout = 1  -> bintVal */
    {
      DExprDag dg;
      DExprDag::Node *n = helper_eq (dg, cout, 1, 1);
      dg.roots.push_back (n);
      std::vector<ChanId> id;
      id.push_back (bintV);
      ret.push_back (Dataflow::mkFunc (id, std::move (dg)));
    }

    /*
      5+6. {bintVal} gval -> *, LoopGuard
    */
    {
      std::vector<OptionalChanId> out;
      out.push_back (OptionalChanId::null_id());
      out.push_back (loopguard);
      Dataflow xd = mkSplit (bintV, gval, out);
      ret.push_back (std::move (xd));
    }
    if (sel && selw != log_2_round_up(cin.size())) {
      Assert (selw + 1 == log_2_round_up (cin.size()), "What?");
      DExprDag dg;
      DExprDag::Node *n =
	dg.newNode (DExprDag::Node::makeResize(
					       helper_var (dg, loopguard,
							   selw+1),
					       selw));
      dg.roots.push_back (n);
      std::vector<ChanId> id;
      id.push_back (*sel);
      ret.push_back (mkFunc (id, std::move (dg)));
      /*
	{bintVal} gval -> *, sel 
      */
    }
    
    return ret;
  }
  
  static std::list<Dataflow> mkInstDoLoop (ChanId cin,
				ChanId cout,
				ChanId guard,
				std::function<ChanId (const OptionalChanId &)> fresh)

  {
    std::list<Dataflow> ret;

    /*
      0 -> Bloop
      {in_loop} Bloop, Bi -> bint
      0 -> Gloop
      {in_loop} Guard, Gloop -> gval

      (in_loop & ~b | ~in_loop & gval = 0) ? 1 : ~in_loop -> newl
      newl -> [1] in_loop

      in_loop & bint | ~in_loop & gval = 0 -> bcond
      {bcond} bint -> *, Bout

    */
    
    /* 
       0 -> Bloop
       {in_loop} Bloop, Bi -> bint
    */
    ChanId Bloop = fresh (cin);
    ChanId bint = fresh (cin);
    ChanId in_loop = fresh (OptionalChanId::null_id());
    {
      ret.push_back (mkSrc (Bloop, BigInt(0), 1));
      std::vector<ChanId> in;
      in.push_back (Bloop);
      in.push_back (cin);
      ret.push_back (mkMergeMix (in_loop, in, bint));
    }

    /*
      0 -> Gloop
      {in_loop} guard, Gloop -> gval
    */
    ChanId Gloop = fresh (guard);
    ChanId gval = fresh (guard);
    {
      ret.push_back(mkSrc (Gloop, BigInt (0), 1));

      std::vector<ChanId> in;
      in.push_back (guard);
      in.push_back (Gloop);

      Dataflow xd = mkMergeMix (in_loop, in, gval);
      xd.msg = "loop-gval";
      ret.push_back(std::move (xd));
    }

    
    /* 
      (in_loop & (bint=1) | ~in_loop -> newl
      newl -> [1] in_loop 
    */
    {
      ChanId newl = fresh (in_loop);
      DExprDag dg;
      DExprDag::Node *n =
	helper_or (dg,
		   helper_and (dg,
			       helper_var (dg, in_loop, 1),
			       helper_eq (dg, bint, 1, 1)),
		   helper_eq (dg, in_loop, 1, 0));
      dg.roots.push_back (n);

      std::vector<ChanId> id;
      
      id.push_back (newl);
      ret.push_back (mkFunc (id, std::move (dg)));

      Dataflow xd = mkInit (newl, in_loop, BigInt(1), 1);
      xd.msg = "loop-in_loop";
      
      ret.push_back (std::move (xd));
    }

    /*
      in_loop & bint | ~in_loop & gval = 0 -> bcond
      {bcond} bint -> *, Bout
    */
    {
      ChanId bcond = fresh (OptionalChanId::null_id());
      DExprDag dg;
      DExprDag::Node *n =
	helper_or (dg,
		   helper_and (dg,
			       helper_var (dg, in_loop, 1),
			       helper_var (dg, bint, 1)
			       ),
		   helper_and (dg,
			       helper_eq (dg, in_loop, 1, 0),
			       helper_eq (dg, gval, 1, 0))
		   );
      dg.roots.push_back (n);
      std::vector<ChanId> id;
      id.push_back (bcond);
      ret.push_back (mkFunc (id, std::move (dg)));

      std::vector<OptionalChanId> out;
      out.push_back (OptionalChanId::null_id());
      out.push_back (cout);
      ret.push_back (mkSplit (bcond, bint, out));
    }

    return ret;
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
	  os << u_func().ids[i].m_id;
	  if (u_func().ids.size() > 1) {
	    os << std::endl;
	  }
	}
	if (u_func().ids.size() > 1) {
	  os << "}" << std::endl;
	}
      }
      break;

    case DataflowKind::Init:
      os << "C" << u_init().lhs.m_id << " -> ["
	 << u_init().v.to_hex_string().c_str() << "]"
	 << "C" << u_init().rhs.m_id;
      break;

    case DataflowKind::Split:
      os << "{C" << u_split().cond_id.m_id << "} "
	 << "C" << u_split().in_id.m_id << " -> "
	 << Algo::join_str_mapped (u_split().out_ids,
				   [&] (const OptionalChanId &x) {
				     return (x) ?
				       string_format ("C%d", (*x).m_id) :
				       "*";
				   }, ",");
      break;

    case DataflowKind::MergeMix:
      os << "{" <<
	(u_mergemix().cond_id ?
	 string_format ("C%d", (*u_mergemix().cond_id).m_id) : "*") << "} "
	 << Algo::join_str_mapped (u_mergemix().in_ids,
				   [&] (const ChanId &x) {
				     return string_format ("C%d", x.m_id);
				   }, ",")
	 << " -> C" << u_mergemix().out_id.m_id;
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
      break;

    case DataflowKind::Sink:
      os << "C" << u_sink().in_id.m_id << " -> *";
      break;
    }
    if (msg) {
      os << " // " << msg;
    }
    os << std::endl;
  }
};


// This function takes in a chp-graph in "static token form" and creates a
// dataflow graph. The entire chp program (including functions in other
// processes) is required to have "slack elasticity", meaning that one could add
// an arbitrary number of buffers on any channel without changing the
// computation. No probes and no shared variables is one way to guarantee that.
// We use an improved algorithm to handle multiple channel access.
std::vector<Dataflow> chp_to_dataflow(GraphWithChanNames &gr);
  
act_dataflow *dataflow_to_act (std::vector<Dataflow> &d,
			       GraphWithChanNames &gr,
			       std::vector<ActId *> &newnames,
			       Scope *s);

// @return true if there are no probes in guards/assignments
bool isProbeFree (const ChpGraph &g);
  
} // namespace ChpOptimize
