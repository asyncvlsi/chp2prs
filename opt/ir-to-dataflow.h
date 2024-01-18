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

using DExpr = IRExpr<ChpTag, ChanId, ManageMemory::no>;
using DExprDag = IRExprDag<ChpTag, ChanId>;
using DExprSingleRootDag = IRExprSingleRootDag<ChpTag, ChanId>;

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

private:
  using Variant_t =
    TypedVariant6<DataflowKind,
		  Func, DataflowKind::Func,
		  Init, DataflowKind::Init,
		  Split, DataflowKind::Split,
		  MergeMix, DataflowKind::MergeMix,
		  Arbiter, DataflowKind::Arbiter,
		  Sink, DataflowKind::Sink>;

  explicit Dataflow (Variant_t u_) : u{std::move(u_)} { keep = false; }

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
					  std::function<ChanId (ChanId &)> fresh)
  {
    ChanId f = fresh(sel);
    std::list<Dataflow> ret;

    /*
       f -> [0] sel
       sel = N-1 ? 0 : sel + 1 -> f

       if there is a cout:

       sel = N - 1 ? 2 : 1 -> cout
    */

    int bw = log_2_round_up (N);

    // f -> [0] sel
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
      // sel = N - 1 ? 2 : 1 -> cout
      DExprDag dg;
      DExprDag::Node *n =
      dg.newNode (
	 DExprDag::Node::makeQuery (
	    helper_eq (dg, sel, bw, N-1),
	    helper_const (dg, 2, 2),
	    helper_const (dg, 1, 2)));
      dg.roots.push_back (n);
				     
      std::vector<ChanId> ids;
      ids.push_back(*cout);
      ret.push_back (Dataflow::mkFunc (ids, std::move (dg)));
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
      1. {s_top} B0, ..., BN -> b

      2. {b!=0} s_top -> *, sel

      3. (b != 1 ? s_top + 1 : s_top) -> s

      4. s -> [0] s_top

      5. (b = 1 | s = 0 | b = 2 & pending) -> bcond
      6. (b = 1 | s != 0 & b = 2 & pending) ? 1 : (pending ? 2 : 0)  -> bval
      7. {bcond} bval -> *, cout

      8. (b = 1 ? pending : (s = 0 ? 0 : (s != 0 & b = 2 ? 1 : pending))) -> newpend
      9. newpend -> [0] pending
    */
    std::list<Dataflow> ret;

    int bw = log_2_round_up (cin.size());

    /* 1. {s_top} B0, ..., BN-1 -> b */
    ChanId b = fresh (cin[0]);
    ChanId s_top;
    s_top = fresh (sel);
    ret.push_back (mkMergeMix (s_top, cin, b));

    /* 2.  b != 0 -> bzero 
           {bzero} s_top -> *, sel
    */
    {
      // b != 0 -> bzero
      ChanId bzero = fresh (OptionalChanId::null_id());
      DExprDag dg;
      DExprDag::Node *n = helper_ne (dg, b, 2, 0);
      dg.roots.push_back(n);
      std::vector<ChanId> id;
      id.push_back (bzero);
      ret.push_back (mkFunc (id, std::move (dg)));

      // {bzero} s_top -> *, sel

      std::vector<OptionalChanId> out;
      out.push_back (OptionalChanId::null_id());
      out.push_back (sel);
      ret.push_back (mkSplit (bzero, s_top, out));
    }

    /* 3.  (b != 1 ? s_top + 1 : s_top) -> s  */
    ChanId s = fresh (sel);
    {
      DExprDag dg;
      DExprDag::Node *n =
	dg.newNode (DExprDag::Node::makeQuery (
           helper_ne (dg, b, 2, 1), // b != 1
	   helper_inc (dg, s_top, bw, cin.size()),
	   dg.newNode (DExprDag::Node::makeVariableAccess (s_top, bw))));
      dg.roots.push_back (n);
      std::vector<ChanId> ids;
      ids.push_back (s);
      ret.push_back (mkFunc (ids, std::move(dg)));
    }
											 
    /* 4. s -> [0] s_top */
    ret.push_back (mkInit (s, s_top, BigInt (0), bw));

    if (cout) {
      /* 5. (b = 1 | s = 0 | b = 2 & pending) -> bcond */
      ChanId pending = fresh (OptionalChanId::null_id());
      ChanId bcond = fresh (OptionalChanId::null_id());

      DExprDag dg;
      DExprDag::Node *n =
	helper_or (dg, helper_eq (dg, b, 2, 1),
		   helper_or (dg, helper_eq (dg, s, bw, 0),
			      helper_and (dg, helper_eq (dg, b, 2, 2),
					  helper_eq (dg, pending, 1, 1))));
      dg.roots.push_back (n);

      std::vector<ChanId> id;
      id.push_back (bcond);
      ret.push_back (mkFunc (id, std::move (dg)));

      /* 6. (b = 1 | s != 0 & b = 2 & pending) ? 1 : (pending ? 2 : 0) -> bval */
      ChanId bval = fresh (*cout);
      DExprDag dg2;
      DExprDag::Node *n2 =
	dg2.newNode (DExprDag::Node::makeQuery (
		helper_or (dg2, helper_eq (dg2, b, 2, 1),
			   helper_and (dg2, helper_ne (dg2, s, bw, 0),
				       helper_and (dg2,
						   helper_eq (dg2, b, 2, 2),
						   helper_eq (dg2, pending, 1, 1)))),
		helper_const (dg2, 1, 2),
	  dg2.newNode
	(DExprDag::Node::makeQuery (helper_eq (dg2, pending, 1, 1),
				    helper_const (dg2, 2, 2),
				    helper_const (dg2, 0, 2)))));
      dg2.roots.push_back (n2);
      id.clear();
      id.push_back (bval);
      ret.push_back (mkFunc (id, std::move (dg2)));

      /* 7. {bcond} bval -> *, cout */
      std::vector<OptionalChanId> outid;

      outid.push_back (OptionalChanId::null_id());
      outid.push_back (*cout);
      
      ret.push_back (mkSplit (bcond, bval, outid));

      /*  8. (b = 1 ? pending : (s = 0 ? 0 : (s != 0 & b = 2 ? 1 : pending))) -> newpend */
      ChanId newpend = fresh (OptionalChanId::null_id());
      {
	DExprDag dg;
      DExprDag::Node *n =
	dg.newNode (DExprDag::Node::makeQuery (
	       helper_eq (dg, b, 2, 1),
	       dg.newNode (DExprDag::Node::makeVariableAccess (pending, 1)),
	       dg.newNode
	       (DExprDag::Node::makeQuery
		(helper_eq (dg, s, bw, 0),
		 helper_const (dg, 0, 1),
		 dg.newNode (DExprDag::Node::makeQuery
			     (helper_eq (dg, b, 2, 2),
			      helper_const (dg, 1, 1),
			      dg.newNode (DExprDag::Node::makeVariableAccess (pending, 1))))))));
      dg.roots.push_back (n);
      std::vector<ChanId> id;
      id.push_back (newpend);
      
      ret.push_back (mkFunc (id, std::move (dg)));
      }

      /* 9. newpend -> [0] pending */
      ret.push_back (mkInit (newpend, pending, BigInt(0), 1));
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

    3. {gval} B1, ..., BN -> bint

    4. bint -> cout

    5.  bint != 0 -> g2

    6.  bint = 1 -> bintVal

    7. {g2} gval -> *, sel

    8.  {bintVal} gval -> *, LoopGuard
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
    ret.push_back (Dataflow::mkInit (bintV, i_bint, BigInt(0), 2));

    /* 2. {i_bint} guard, LoopGuard -> gval */
    ChanId loopguard = fresh (guard);
    ChanId buf_loopg = fresh (guard);
    ChanId gval = fresh (guard);
    std::vector<ChanId> inlist;
    inlist.push_back (guard);
    inlist.push_back (buf_loopg);
    ret.push_back (Dataflow::mkMergeMix (i_bint, inlist, gval));

    DExprDag cp;
    DExprDag::Node *n =
      cp.newNode (DExprDag::Node::makeVariableAccess (loopguard,
						      log_2_round_up (cin.size())));
    cp.roots.push_back (n);
    inlist.clear ();
    inlist.push_back (buf_loopg);
    Dataflow dtmp = Dataflow::mkFunc (inlist, std::move (cp));
    dtmp.keep = true;
    inlist.clear ();
    ret.push_back (std::move (dtmp));

    /* 3. {gval} B1, ..., BN -> bint */
    ChanId bint = fresh (cin[0]);
    ret.push_back (Dataflow::mkMergeMix (gval, cin, bint));

    /* 4. bint -> cout */
    DExprDag dg;
    n = dg.newNode (DExprDag::Node::makeVariableAccess (bint, 2));
    dg.roots.push_back (n);
    inlist.clear();
    inlist.push_back (cout);
    ret.push_back (Dataflow::mkFunc (inlist, std::move (dg)));

    /* 5.  bint != 0 -> g2 */

    ChanId g2;
    if (sel) {
      g2 = fresh (OptionalChanId::null_id());

      DExprDag dg2;
      n = dg2.newNode (DExprDag::Node::makeBinaryOp (IRBinaryOpType::NE,
	   dg2.newNode (DExprDag::Node::makeVariableAccess (bint, 2)),
           dg2.newNode (DExprDag::Node::makeConstant (BigInt(0), 2))));
      dg2.roots.push_back(n);
      inlist.clear();
      inlist.push_back (g2);
      ret.push_back (Dataflow::mkFunc (inlist, std::move (dg2)));
    }

    /* 6. bint = 1 -> bintVal */
    DExprDag dg3;
    n = dg3.newNode (DExprDag::Node::makeBinaryOp (IRBinaryOpType::EQ,
	   dg3.newNode (DExprDag::Node::makeVariableAccess (bint, 2)),
           dg3.newNode (DExprDag::Node::makeConstant (BigInt(1), 2))));
    dg3.roots.push_back(n);
    inlist.clear();
    inlist.push_back (bintV);
    ret.push_back (Dataflow::mkFunc (inlist, std::move (dg3)));
    
    
    /* 7. {g2} gval -> *, sel */
    std::vector<OptionalChanId> outlist;
    if (sel) {
      if (selw != log_2_round_up (cin.size())) {
	// there's an extra channel for the "no B" case, and adding it
	// bumps up the bit-width; we need to convert the gval to the
	// smaller bitwidth
	Assert (selw + 1 == log_2_round_up (cin.size()), "What?");

	// add int(gval,selw) -> gval2
	DExprDag dg4;
	n = dg4.newNode (DExprDag::Node::makeResize (
						     dg4.newNode (DExprDag::Node::makeVariableAccess (gval, selw+1)), selw));
	dg4.roots.push_back (n);
	inlist.clear ();
	ChanId gvaltrunc = fresh (sel);
	inlist.push_back (gvaltrunc);
	ret.push_back (Dataflow::mkFunc (inlist, std::move (dg4)));
	inlist.clear ();

	outlist.push_back (OptionalChanId::null_id());
	outlist.push_back (sel);
	ret.push_back (Dataflow::mkSplit (g2, gvaltrunc, outlist));
      }
      else {
	outlist.push_back (OptionalChanId::null_id());
	outlist.push_back (sel);
	ret.push_back (Dataflow::mkSplit (g2, gval, outlist));
      }
    }

    /* 8.  {bintVal} gval -> *, LoopGuard */
    outlist.clear();
    outlist.push_back (OptionalChanId::null_id());
    outlist.push_back (loopguard);
    ret.push_back (Dataflow::mkSplit (bintV, gval, outlist));

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
	{in_loop} Bloop, Bi -> b

	in_loop & (b != 1) ? 0 : 1 -> [1] in_loop

	0 -> Gloop
	{in_loop} guard, Gloop -> gval


	in_loop ? ((b = 2 & pending = 0) ? 1 : pending) : (gval = 0 ? 0 : pending)  -> [0] pending

	in_loop ? 1 : (pending ? 2 : 0) -> bval
	in_loop & (b = 1 | b = 2 & pending) | ~in_loop & g = 0 -> bcond
	{bcond} bval -> *, cout    
    */


    /* 0 -> Bloop */
    ChanId Bloop = fresh (cin);
    {
      DExprDag dg;
      DExprDag::Node *n = helper_const (dg, 0, 2);
      dg.roots.push_back (n);
      std::vector<ChanId> id;
      id.push_back (Bloop);
      ret.push_back (mkFunc (id, std::move (dg)));
    }

    /* {in_loop} Bloop, Bi -> b */
    ChanId b = fresh (cin);
    ChanId in_loop = fresh (OptionalChanId::null_id());
    std::vector<ChanId> idlist;
    idlist.push_back (Bloop);
    idlist.push_back (cin);
    ret.push_back (mkMergeMix (in_loop, idlist, b));
    idlist.clear();

    /* 
       in_loop & (b != 1) ? 0 : 1 -> newl 
       newl -> [1] in_loop 
    */
    ChanId newl;
    {
      DExprDag dg;
      DExprDag::Node *n =
	helper_query (dg,
		      helper_and (dg, dg.newNode (DExprDag::Node::makeVariableAccess (in_loop, 1)),
				  helper_ne (dg, b, 2, 1)),
		      helper_const (dg, 0, 1),
		      helper_const (dg, 1, 1)
		      );
      dg.roots.push_back (n);
      newl = fresh (in_loop);
      idlist.push_back (newl);
      ret.push_back (mkFunc (idlist, std::move (dg)));
      idlist.clear ();

      ret.push_back (mkInit (newl, in_loop, BigInt(1), 1));
    }

    /*
      {in_loop} guard, Gloop -> gval
    */
    ChanId Gloop = fresh (guard);
    ChanId gval = fresh (guard);

    ret.push_back(mkSrc (Gloop, BigInt (0), 1));

    idlist.push_back (guard);
    idlist.push_back (Gloop);
    ret.push_back(mkMergeMix (in_loop, idlist, gval));
    idlist.clear ();
    
    std::vector<OptionalChanId> slist;
    
    /* in_loop ? ((b = 2 & pending = 0) ? 1 : pending) : (gval = 0 ?
       0 : pending)  -> [0] pending */
    ChanId pending = fresh (OptionalChanId::null_id());
    {
      DExprDag dg;
      DExprDag::Node *n =
	helper_query (dg,
		      dg.newNode (DExprDag::Node::makeVariableAccess (in_loop, 1)),
		      helper_query (dg,
				    helper_and (dg,
						helper_eq (dg, b, 2, 2),
						helper_eq (dg, pending, 1, 0)
						),
				    helper_const (dg, 1, 1),
				    dg.newNode (DExprDag::Node::makeVariableAccess (pending, 1))
				    ),
		      helper_query (dg,
				    helper_eq (dg, gval, 1, 0),
				    helper_const (dg, 0, 1),
				    dg.newNode (DExprDag::Node::makeVariableAccess (pending, 1))    
				    )
		      );
      dg.roots.push_back (n);
      ChanId newpend = fresh (pending);
      idlist.push_back (newpend);
      ret.push_back (mkFunc (idlist, std::move (dg)));
      idlist.clear ();
      
      ret.push_back (mkInit (newpend, pending, BigInt (0), 1));
    }
      
    /* in_loop ? 1 : (pending ? 2 : 0) -> bval */
    ChanId bval = fresh (cin);
    {
      DExprDag dg;
      DExprDag::Node *n =
	helper_query (dg,
		      dg.newNode (DExprDag::Node::makeVariableAccess (in_loop, 1)),
		      helper_const (dg, 1, 2),
		      helper_query (dg,
				    dg.newNode (DExprDag::Node::makeVariableAccess (pending, 1)),
				    helper_const (dg, 2, 2),
				    helper_const (dg, 0, 2)));
      dg.roots.push_back (n);
      idlist.push_back (bval);
      ret.push_back (mkFunc (idlist, std::move (dg)));
      idlist.clear ();
    }

    /* in_loop & (b = 1 | b = 2 & pending) | ~in_loop & g = 0 -> bcond */
    ChanId bcond = fresh (OptionalChanId::null_id());
    {
      DExprDag dg;
      DExprDag::Node *n =
	helper_or (dg,
		   helper_and (dg,
			       dg.newNode (DExprDag::Node::makeVariableAccess (in_loop, 1)),
			       helper_or (dg,
					  helper_eq (dg, b, 2, 1),
					  helper_and (dg,
						      helper_eq (dg, b, 2, 2),
						      dg.newNode (DExprDag::Node::makeVariableAccess (pending, 1)))
					  )
			       ),
		   helper_and (dg,
			       helper_eq (dg, gval, 1, 0),
			       dg.newNode (DExprDag::Node::makeUnaryOp (IRUnaryOpType::Not,
									dg.newNode (DExprDag::Node::makeVariableAccess (in_loop, 1)))))
		   );
      dg.roots.push_back (n);
      idlist.push_back (bcond);
      ret.push_back (mkFunc (idlist, std::move (dg)));
      idlist.clear ();
    }
					  
    /* {bcond} bval -> *, cout */
    slist.clear();
    slist.push_back (OptionalChanId::null_id());
    slist.push_back (cout);
    ret.push_back (mkSplit (bcond, bval, slist));
    
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
    }
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
  
} // namespace ChpOptimize
