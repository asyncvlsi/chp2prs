/*************************************************************************
 *
 *  This file is part of the ACT library
 *
 *  Copyright (c) 2021-2022 Henry Heffan
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

#include "ir-to-dataflow.h"
#include "chp-print.h"
#include "algos.h"
#include "utils.h"
#include <functional>
#include <iostream>

namespace ChpOptimize {
namespace {

  
/*
 * This holds the mapping from variables to channels, and from
 * channels to channels for multiple channel access.
 */
struct MultiChannelState {
  /*
    For multi-channel sends and receives, this maps the original
    channel to the fresh channel in the block.
  */
  std::unordered_map<ChanId, ChanId> datamap;

  /*
    If the channel output is variable, we need the control channel
    mapping. The argument to this map is the *current name* of the
    channel, and the map returns the variable control token sequence
    for the channel (the 0/1/2 sequence).
  */
  std::unordered_map<ChanId, ChanId> ctrlmap;
};
    
struct DataflowChannelManager {
  /*
    This holds the map from the optimized variables to channels
  */
  std::unordered_map<VarId,ChanId> varmap;
  IdPool *id_pool;

  /*
    Outermost block for a channel
  */
  std::unordered_map<ChanId, Block *> chanmap;
};


ChanId dflow_map(VarId v, DataflowChannelManager &maps)
{
  if (!maps.varmap.contains(v)) {
    maps.varmap[v] =
      maps.id_pool->makeUniqueChan (maps.id_pool->getBitwidth (v));
  }
  return maps.varmap[v];
}

void dflow_forcemap (VarId v, ChanId c, DataflowChannelManager &maps)
{
  maps.varmap[v] = c;
}

int select_guard_width(const Block::Variant_Select &select) {
    return log_2_round_up(select.branches.size());
}

ChanId dflow_freshchan (DataflowChannelManager &maps,
			const Block::Variant_Select &select) 
{
  return maps.id_pool->makeUniqueChan (select_guard_width (select));
}

 ChanId dflow_freshchan (DataflowChannelManager &maps, int width)
{
  return maps.id_pool->makeUniqueChan (width);
}
						  
/* 
   Take a ChpExprDag that uses variables and convert it to a 
   dataflow dag, remaing variables to channels.
*/
DExprDag of_chp_dag(const ChpExprDag &dag, DataflowChannelManager &maps)
{
    DExprDag ddag;
    std::unordered_map<const ChpExprDag::Node *, DExprDag::Node *> mp;
    ChpExprDag::iterNodes(dag, [&](const ChpExprDag::Node &n) {
        switch (n.type()) {
        case IRExprTypeKind::BinaryOp:
            mp[&n] = ddag.newNode(DExprDag::Node::makeBinaryOp(
                n.u_e2().op_type, mp.at(n.u_e2().l), mp.at(n.u_e2().r)));
            break;
        case IRExprTypeKind::UnaryOp:
            mp[&n] = ddag.newNode(DExprDag::Node::makeUnaryOp(
                n.u_e1().op_type, mp.at(n.u_e1().l)));
            break;
        case IRExprTypeKind::Query:
            mp[&n] = ddag.newNode(DExprDag::Node::makeQuery(
                mp.at(n.u_query().selector), mp.at(n.u_query().l),
                mp.at(n.u_query().r)));
            break;
        case IRExprTypeKind::Const:
            mp[&n] = ddag.newNode(
                DExprDag::Node::makeConstant(n.u_cons().v, n.u_cons().v_width));
            break;
        case IRExprTypeKind::Var:
	    mp[&n] =
	      ddag.newNode(DExprDag::Node::makeVariableAccess(
		      dflow_map (n.u_var().id, maps), n.width));;
            break;
        case IRExprTypeKind::Bitfield:
            mp[&n] = ddag.newNode(DExprDag::Node::makeBitfield(
                mp.at(n.u_bitfield().e), n.u_bitfield().hi(),
                n.u_bitfield().lo()));
            break;
        }
    });
    for (const auto &root : dag.roots)
        ddag.roots.push_back(mp.at(root));
    return ddag;
}

/* 
   Take a ChpExprDag that uses variables and convert it to a 
   dataflow dag, remaing variables to channels.
*/
DExprSingleRootDag of_chp_dag(const ChpExprSingleRootDag &dag, DataflowChannelManager &maps)
{
    DExprSingleRootDag ddag;
    std::unordered_map<const ChpExprSingleRootDag::Node *,
                              DExprSingleRootDag::Node *> mp;
    ChpExprSingleRootDag::iterNodes(dag, [&](const ChpExprSingleRootDag::Node &n) {
        switch (n.type()) {
        case IRExprTypeKind::BinaryOp:
            mp[&n] = ddag.m_dag.newNode(DExprSingleRootDag::Node::makeBinaryOp(
                n.u_e2().op_type, mp.at(n.u_e2().l), mp.at(n.u_e2().r)));
            break;
        case IRExprTypeKind::UnaryOp:
            mp[&n] = ddag.m_dag.newNode(DExprSingleRootDag::Node::makeUnaryOp(
                n.u_e1().op_type, mp.at(n.u_e1().l)));
            break;
        case IRExprTypeKind::Query:
            mp[&n] = ddag.m_dag.newNode(DExprSingleRootDag::Node::makeQuery(
                mp.at(n.u_query().selector), mp.at(n.u_query().l),
                mp.at(n.u_query().r)));
            break;
        case IRExprTypeKind::Const:
            mp[&n] = ddag.m_dag.newNode(
                DExprDag::Node::makeConstant(n.u_cons().v, n.u_cons().v_width));
            break;
        case IRExprTypeKind::Var:
	    mp[&n] =
	      ddag.m_dag.newNode(DExprSingleRootDag::Node::makeVariableAccess(
		      dflow_map (n.u_var().id, maps), n.width));;
            break;
        case IRExprTypeKind::Bitfield:
            mp[&n] = ddag.m_dag.newNode(DExprSingleRootDag::Node::makeBitfield(
                mp.at(n.u_bitfield().e), n.u_bitfield().hi(),
                n.u_bitfield().lo()));
            break;
        }
    });
    ddag.m_dag.roots.push_back(mp.at(dag.root()));
    return ddag;
}
 
/*
  How many bits do we need for the guard?
*/



std::string unary_op_marker(IRUnaryOpType op) {
    switch (op) {
    case IRUnaryOpType::Not:
        return " ~";
    case IRUnaryOpType::UnaryMinus:
        return " -";
    }
    hassert(false);
    return "";
}
std::string binary_op_marker(IRBinaryOpType op) {
    switch (op) {
    case IRBinaryOpType::And:
        return " & ";
    case IRBinaryOpType::Or:
        return " | ";
    case IRBinaryOpType::Xor:
        return " ^ ";
    case IRBinaryOpType::Plus:
        return " + ";
    case IRBinaryOpType::Minus:
        return " - ";
    case IRBinaryOpType::Mult:
        return " * ";
    case IRBinaryOpType::Div:
        return " / ";
    case IRBinaryOpType::Mod:
        return " % ";
    case IRBinaryOpType::LeftShift:
        return " << ";
    case IRBinaryOpType::RightShift:
        return " >> ";
    case IRBinaryOpType::ArithmeticRightShift:
        return " >>> ";
    case IRBinaryOpType::LT:
        return " < ";
    case IRBinaryOpType::GT:
        return " > ";
    case IRBinaryOpType::LE:
        return " <= ";
    case IRBinaryOpType::GE:
        return " >= ";
    case IRBinaryOpType::EQ:
        return " = ";
    case IRBinaryOpType::NE:
        return " != ";
    case IRBinaryOpType::Concat:
        hassert(false);
    }
    hassert(false);
    return "";
}

void print_dexpr(std::ostream &o, const DExpr &e) {
    switch (e.type()) {
    case IRExprTypeKind::Const:
        o << string_format("int(%s, %d)", e.u_cons().v.to_hex_string().c_str(),
                           e.u_cons().v_width);
        break;
    case IRExprTypeKind::Var:
        o << string_format("C%d", e.u_var().id.m_id);
        break;
    case IRExprTypeKind::BinaryOp:
        if (e.u_e2().op_type == IRBinaryOpType::Concat) {
            o << "{";
            print_dexpr(o, *e.u_e2().l);
            o << ", ";
            print_dexpr(o, *e.u_e2().r);
            o << "}";
        } else {
            o << "(";
            print_dexpr(o, *e.u_e2().l);
            o << binary_op_marker(e.u_e2().op_type);
            print_dexpr(o, *e.u_e2().r);
            o << ")";
        }
        break;
    case IRExprTypeKind::UnaryOp:
        o << "(";
        o << unary_op_marker(e.u_e1().op_type);
        print_dexpr(o, *e.u_e1().l);
        o << ")";
        break;
    case IRExprTypeKind::Query:
        o << "(";
        print_dexpr(o, *e.u_query().selector);
        o << " ? ";
        print_dexpr(o, *e.u_query().l);
        o << " : ";
        print_dexpr(o, *e.u_query().r);
        o << ")";
        break;
    case IRExprTypeKind::Bitfield:
        // bitfields not supported in dataflow
        o << "int((";
        print_dexpr(o, *e.u_bitfield().e);
        o << string_format(") >> %d, %d)", e.u_bitfield().lo(),
                           e.u_bitfield().ct());
        break;
    }
}

std::string string_of_dexpr(const DExpr &e) {
    std::stringstream ss;
    print_dexpr(ss, e);
    return ss.str();
}


void computeOutermostBlock (Sequence seq, DataflowChannelManager &dm,
			     Block *outer = NULL)
{
  Block *curr = seq.startseq->child();
  while (curr->type() != BlockType::EndSequence) {
    switch (curr->type()) {
    case BlockType::Basic: {
      switch (curr->u_basic().stmt.type()) {
      case StatementType::Assign: {
	break;
      }
      case StatementType::Send: {
	if (dm.chanmap.contains (curr->u_basic().stmt.u_send().chan)) {
	  // we already have a map for this channel, so we need to
	  // move it to the current outer scope
	  dm.chanmap[curr->u_basic().stmt.u_send().chan] =
	    (outer ? outer : seq.startseq);
	}
	else {
	  dm.chanmap[curr->u_basic().stmt.u_send().chan] =
	    (outer ? outer : curr);
	}
	break;
      }
      case StatementType::Receive: {
	if (dm.chanmap.contains (curr->u_basic().stmt.u_receive().chan)) {
	  // we already have a map for this channel, so we need to
	  // move it to the current outer scope
	  dm.chanmap[curr->u_basic().stmt.u_receive().chan] =
	    (outer ? outer : seq.startseq);
	}
	else {
	  dm.chanmap[curr->u_basic().stmt.u_receive().chan] =
	    (outer ? outer : curr);
	}
	break;
      }
      }
      break;
    }
    case BlockType::Par: {
      // A well-formed program cannot have channel conflicts in
      // parallel branches
      for (auto &branch : curr->u_par().branches) {
	computeOutermostBlock (branch, dm, outer);
      }
      break;
    }
    case BlockType::Select: {
      for (auto &branch : curr->u_select().branches) {
	computeOutermostBlock (branch.seq, dm, outer ? outer : seq.startseq);
      }
      break;
    }
    case BlockType::DoLoop: {
      computeOutermostBlock (curr->u_doloop().branch, dm, 
			     outer ? outer : seq.startseq);
      break;
    }
    case BlockType::StartSequence:
    case BlockType::EndSequence:
      hassert(false);
      break;
    }
    curr = curr->child();
  }
}

void printOutermostBlock (DataflowChannelManager &dm)
{
  for (auto &[chan, block] : dm.chanmap) {
    printf ("ch %d -> ", (int)chan.m_id);
    if (!block) {
      printf ("null\n");
    }
    else {
      switch (block->type()) {
      case BlockType::Basic: {
	switch (block->u_basic().stmt.type()) {
	case StatementType::Assign:
	  printf ("assign!\n");
	break;
	case StatementType::Send:
	  printf ("send\n");
	break;
	case StatementType::Receive:
	  printf ("recv!\n");
	break;
	}
	break;
      }
      case BlockType::StartSequence:
	printf ("seq\n");
	break;
	
      default:
	hassert(false);
	break;
      }
    }
  }
}


/*
  swap is true in the special case where the guard is already a
  one-bit variable
*/
ChanId nodes_add_guard(const Block::Variant_Select &select,
		       bool &swap,
		       std::vector<Dataflow> &d,
		       DataflowChannelManager &dm)
{
  hassert(Algo::count_if(select.branches, [&](const SelectBranch &branch) {
	return branch.g.type() == IRGuardType::Else;
      }) <= 1);

  swap = false;
  
  int width = select_guard_width(select);

  if (width == 1) {
    /* check if we have a special case */    
    if (select.branches.front().g.type() == IRGuardType::Expression &&
	select.branches.front().g.u_e().e.m_dag.roots[0]->type() ==
	IRExprTypeKind::Var) {
      ChanId guard =
	dflow_map (select.branches.front().g.u_e().e.m_dag.roots[0]->u_var().id, dm);
      swap = true;
      return guard;
    }
    else if (select.branches.back().g.type() == IRGuardType::Expression &&
	     select.branches.back().g.u_e().e.m_dag.roots[0]->type() ==
	     IRExprTypeKind::Var) {
      ChanId guard =
	dflow_map (select.branches.back().g.u_e().e.m_dag.roots[0]->u_var().id, dm);
      swap = false;
      return guard;
    }
  }

  // first find the else branch (if there is one). Otherwise, since there are
  // no probes, promote the first branch to be the "else" case
  auto else_branch =
    Algo::find_if(select.branches, [&](const SelectBranch &branch) {
	return branch.g.type() == IRGuardType::Else;
      });
  if (else_branch == select.branches.end())
    else_branch = select.branches.begin();

  int else_branch_idx =
    (int)std::distance(select.branches.begin(), else_branch);

  DExprDag guard_dag;
  DExprDag::Node *root = NULL;

  int idx = -1;
  for (const auto &branch : select.branches) {
    idx++;
    if (&branch == &*else_branch)
      continue;
    hassert(branch.g.type() != IRGuardType::Else);
    hassert(branch.g.type() == IRGuardType::Expression);
    DExprDag::Node *dnode =
      guard_dag.addSubdag (of_chp_dag (branch.g.u_e().e, dm));
    if (root) {
      root = guard_dag.newNode(
	DExprDag::Node::makeQuery (
	  dnode,
	  guard_dag.newNode (DExprDag::Node::makeConstant (BigInt(idx), width)),
	  root));
    }
    else {
      root = guard_dag.newNode (
	DExprDag::Node::makeQuery (
 	  dnode,
	  guard_dag.newNode (DExprDag::Node::makeConstant (BigInt(idx), width)),
	  guard_dag.newNode (DExprDag::Node::makeConstant (BigInt(else_branch_idx), width))));
    }
  }

  guard_dag.roots.push_back (root);

  // add dataflow expression!
  ChanId guard_id = dflow_freshchan (dm, select);
  std::vector<ChanId> ids;
  ids.push_back (guard_id);
  d.push_back (Dataflow::mkFunc (ids, std::move(guard_dag)));
  return guard_id;
}

// returns two channels: the second one has the initial token on it
std::pair<ChanId,ChanId>
  nodes_add_loopguard(const Block::Variant_DoLoop &doloop,
			   std::vector<Dataflow> &d,
			   DataflowChannelManager &dm)
{
  DExprDag guard_dag;
  DExprDag::Node *root = guard_dag.addSubdag(of_chp_dag (doloop.guard, dm));
  guard_dag.roots.push_back (root);
  ChanId guard_id = dflow_freshchan (dm, 1);
  std::vector<ChanId> ids;
  ids.push_back (guard_id);
  d.push_back (Dataflow::mkFunc (ids, std::move(guard_dag)));

  ChanId init_guard_id = dflow_freshchan (dm, 1);
  d.push_back (Dataflow::mkInit (guard_id, init_guard_id,
				 BigInt(0), 1));

  return std::pair<ChanId,ChanId>{guard_id, init_guard_id};
}

MultiChannelState createDataflow (Sequence seq, DataflowChannelManager &dm,
				  std::vector<Dataflow> &d)
{
  MultiChannelState ret, empty;
  
  Block *curr = seq.startseq->child();
  std::vector<MultiChannelState> seqs;
  while (curr->type() != BlockType::EndSequence) {
    switch (curr->type()) {
    case BlockType::Basic: {
      switch (curr->u_basic().stmt.type()) {
      case StatementType::Assign:
	// This is straightforward: each variable is mapped to its
	// current channel mapping, and this is turned into a function
	// block.
	d.push_back (
	     Dataflow::mkFunc(
   	       Algo::map1<ChanId> (curr->u_basic().stmt.u_assign().ids,
			  [&] (const VarId v) {
			    return dflow_map (v, dm);
			  }),
	       of_chp_dag (curr->u_basic().stmt.u_assign().e, dm))
	);
	seqs.push_back (empty);
	break;
      case StatementType::Send:
	{ std::vector<ChanId> ids;
	  ChanId sc = curr->u_basic().stmt.u_send().chan;

	  if (dm.chanmap[sc] == curr) {
	    // No multi-channel access issues for this statement, so
	    // there's a simple function translation.
	    
	    // empty multichannel state
	    seqs.push_back (empty);
	  }
	  else {
	    // Multi-channel access for this send. We need a fresh
	    // channel to replace the send, and record this mapping in
	    // the multi-channel state.
	    
	    // record multichannel information
	    MultiChannelState ms;
	    ChanId fresh =  dflow_freshchan (dm, dm.id_pool->getBitwidth (sc));
	    ms.datamap[sc] = fresh;
	    seqs.push_back (ms);

	    // replace send channel with the fresh name.
	    sc = fresh;
	  }

	  // create the send operation
	  ids.push_back (sc);
	  d.push_back (
	     Dataflow::mkFunc (
		ids,
		of_chp_dag (curr->u_basic().stmt.u_send().e.m_dag, dm)));
	}
	break;
      case StatementType::Receive:
	{
	  ChanId rc = curr->u_basic().stmt.u_receive().chan;

	  if (dm.chanmap[rc] == curr) {
	    // this is the only place for this receive!
	    if (curr->u_basic().stmt.u_receive().var) {
	      dflow_forcemap (*curr->u_basic().stmt.u_receive().var, rc, dm);
	    }
	    else {
	      d.push_back (Dataflow::mkSink (rc));
	    }
	    seqs.push_back (empty);
	  }
	  else {
	    // we need a fresh channel here
	    ChanId fresh = dflow_freshchan (dm, dm.id_pool->getBitwidth (rc));
	    if (curr->u_basic().stmt.u_receive().var) {
	      dflow_forcemap (*curr->u_basic().stmt.u_receive().var,
			      fresh, dm);
	    }
	    else {
	      d.push_back (Dataflow::mkSink (fresh));
	    }
	    // record multichannel information for this channel
	    MultiChannelState ms;
	    ms.datamap[rc] = fresh;
	    seqs.push_back (ms);
	  }
	}
	break;
      }
      break;
    }
    case BlockType::Par: {
      // A well-formed program cannot have channel conflicts in
      // parallel branches, so any multi-channel access is the union
      // across all parallel branches.
      MultiChannelState ms, acc;
      for (auto &branch : curr->u_par().branches) {
	ms = createDataflow (branch, dm, d);
	acc.datamap = Algo::set_union (acc.datamap, ms.datamap);
	acc.ctrlmap = Algo::set_union (acc.ctrlmap, ms.ctrlmap);
      }
      seqs.push_back (acc);
      break;
    }
    case BlockType::Select:
      {
	// deal with guards, phiinv, and phi
	ChanId guard;
	bool swap = false;
	if (!curr->u_select().splits.empty()
	    || !curr->u_select().merges.empty()) {
	  // we need a guard!
	  guard = nodes_add_guard (curr->u_select(), swap, d, dm);
	}

	// phiinv
	for (auto &split : curr->u_select().splits) {
	  std::vector<OptionalChanId> out;
	  out = Algo::map1<OptionalChanId> (split.branch_ids,
			    [&] (OptionalVarId v) {
			      if (v) {
				return OptionalChanId{dflow_map ((*v), dm)};
			      }
			      else {
				return OptionalChanId::null_id();
			      }
			    });

	  if (swap) {
	    OptionalChanId c0 = out[0];
	    OptionalChanId c1 = out[1];
	    out[1] = c0;
	    out[0] = c1;
	  }
	  
	  d.push_back (
	       Dataflow::mkSplit(guard,
				 dflow_map (split.pre_id, dm),
				 out));
	}

	std::vector<MultiChannelState> msv;
	for (auto &branch : curr->u_select().branches) {
	  msv.push_back (createDataflow (branch.seq, dm, d));
	}
	
	for (auto &merge : curr->u_select().merges) {
	  std::vector<ChanId> inp;
	  inp = Algo::map1<ChanId> (merge.branch_ids,
			    [&] (VarId v) {
			      return dflow_map (v, dm);
			    });

	  if (swap) {
	    ChanId c0 = inp[0];
	    ChanId c1 = inp[1];
	    inp[1] = c0;
	    inp[0] = c1;
	  }
	  
	  d.push_back (Dataflow::mkMergeMix (OptionalChanId{guard}, inp,
					     dflow_map (merge.post_id, dm)));
	}

	// XXX: reconcile multi-channel access
	
	

      }
      break;
    case BlockType::DoLoop: {
      MultiChannelState ms = createDataflow (curr->u_doloop().branch, dm, d);

      // deal with loopphi, phiinv, loopphinv
      auto guards = nodes_add_loopguard (curr->u_doloop(), d, dm);

      // handle loop-phis first
      for (auto &loopphi : curr->u_doloop().loop_phis) {
	std::vector<OptionalChanId> outp;

	ChanId feedback =
	  dflow_freshchan (dm, dm.id_pool->getBitwidth (loopphi.bodyout_id));

	if (loopphi.post_id) {
	  outp.push_back (dflow_map (*loopphi.post_id, dm));
	}
	else {
	  outp.push_back (OptionalChanId::null_id());
	}
	outp.push_back (feedback);

	d.push_back (Dataflow::mkSplit
		     (guards.first, dflow_map (loopphi.bodyout_id, dm),
		      outp));


	std::vector<ChanId> inp;
	inp.push_back (dflow_map (loopphi.pre_id, dm));
	inp.push_back (feedback);
	d.push_back (Dataflow::mkMergeMix
		     (guards.second, inp, dflow_map (loopphi.bodyin_id, dm)));
      }

      for (auto &inphi : curr->u_doloop().in_phis) {
	std::vector<ChanId> inp;
	ChanId feedback =
	  dflow_freshchan (dm, dm.id_pool->getBitwidth (inphi.pre_id));
	inp.push_back (dflow_map (inphi.pre_id, dm));
	inp.push_back (feedback);
	d.push_back (Dataflow::mkMergeMix
		     (OptionalChanId{guards.second}, inp,
		      dflow_map (inphi.bodyin_id, dm)));

	std::vector<OptionalChanId> outp;
	outp.push_back (OptionalChanId::null_id());
	outp.push_back (feedback);
	d.push_back (Dataflow::mkSplit
		     (guards.first, dflow_map(inphi.bodyin_id, dm),
		      outp));
      }

      for (auto &outphi : curr->u_doloop().out_phis) {
	std::vector<OptionalChanId> outp;
	outp.push_back (OptionalChanId::null_id());
	outp.push_back (dflow_map (outphi.post_id, dm));
	d.push_back (Dataflow::mkSplit
		     (guards.first, dflow_map(outphi.bodyout_id, dm),
		      outp));
      }

      // XXX reconcile multi-channel access

      break;
    }
    case BlockType::StartSequence:
    case BlockType::EndSequence:
      hassert(false);
      break;
    }
    curr = curr->child();
  }

  // XXX reconcile sequencing multi-channel access
  
  return ret;
}
 
} // namespace

void printDataflowExpr (std::ostream &os, const DExpr &d)
{
  print_dexpr (os, d);
}


std::vector<Dataflow> chp_to_dataflow(ChpGraph &chp)
{
  std::vector<Dataflow> d;
  DataflowChannelManager m;

  hassert (chp.is_static_token_form);
  
  m.id_pool = &chp.id_pool();

  // recursively translate, while doing multichannel stuff
  // simultaneously
  computeOutermostBlock (chp.m_seq, m);

  // now create dataflow blocks!
  MultiChannelState ret = createDataflow (chp.m_seq, m, d);
  hassert (ret.datamap.empty() && ret.ctrlmap.empty());

  return d;
}

} // namespace ChpOptimize
