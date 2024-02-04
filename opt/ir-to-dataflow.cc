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

#include "ir-to-dataflow.h"
#include "chp-print.h"
#include "algos.h"
#include "act-names.h"
#include "ir-expr-act-conversion.h"
#include "utils.h"
#include <functional>
#include <iostream>
#include <common/agraph.h>

template<> struct std::hash<std::pair<int,int>> {
  size_t
    operator () (const std::pair<int,int> c) const {
    size_t seed = 0;
    hash_combine (seed, c.first);
    hash_combine (seed, c.second);
    return seed;
  }
};

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
  std::unordered_map<ChanId, ChanId> ctrlmapnz;

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

  /*
    If we generate round robin sequeners, this saves them for re-use
    if needed
  */
  std::unordered_map<int, ChanId> rr_noctrl;

  // pair: first one is the select output, second one is the control channel
  std::unordered_map<int, std::pair <ChanId, ChanId> > rr_ctrl;

  bool isOutermostBlock (ChanId ch, const Block *b) {
    if (chanmap[ch] == b) {
      return true;
    }
    else {
      return false;
    }
  }

  int bitWidth (ChanId ch) {
    return id_pool->getBitwidth (ch);
  }

  // return a fresh channel with the specified bitwidth
  ChanId fresh (int width) {
    return id_pool->makeUniqueChan (width);
  }

  // return a fresh version of the channel ch, replicating its
  // bitwidth and direction flag
  ChanId fresh (const ChanId &ch) {
    ChanId ret;
    ret = id_pool->makeUniqueChan (id_pool->getBitwidth (ch));
    id_pool->setChanDir (ret, id_pool->isChanInput (ch));
    return ret;
  }

  // return a fresh channel with the same bitwidth as the varaible
  ChanId fresh (const VarId &var) {
    return id_pool->makeUniqueChan (id_pool->getBitwidth (var));
  }

  // return current channel mapping for variable v; if there isn't a
  // mapping, create one and return it.
  ChanId mapvar (const VarId &v) {
    if (!varmap.contains(v)) {
      varmap[v] = id_pool->makeUniqueChan (id_pool->getBitwidth (v));
    }
    return varmap[v];
  }

  // map the variable v to the channel c
  void setmap (const VarId &v, const ChanId &c) {
    varmap[v] = c;
  }


  /* source that generates a 2 */
  std::pair<ChanId,ChanId> generateMultiBaseCase (std::vector<Dataflow> &d) {
    ChanId fv = fresh (2);
    {
      DExprDag e;
      DExprDag::Node *n =
	e.newNode (DExprDag::Node::makeConstant (BigInt (2), 2));
      e.roots.push_back (n);
      std::vector<ChanId> ids;
      ids.push_back (fv);
      d.push_back (Dataflow::mkFunc (ids, std::move (e)));
    }

    ChanId fv2 = fresh (1);
    {
      DExprDag e;
      DExprDag::Node *n =
	e.newNode (DExprDag::Node::makeConstant (BigInt (1), 1));
      e.roots.push_back (n);
      std::vector<ChanId> ids;
      ids.push_back (fv2);
      d.push_back (Dataflow::mkFunc (ids, std::move (e)));
    }
    
    return std::pair<ChanId,ChanId>(fv,fv2);
  }

  void generateCopy (ChanId from, ChanId to, std::vector<Dataflow> &d) {
    DExprDag e;
    DExprDag::Node *n =
      e.newNode (DExprDag::Node::makeVariableAccess (from,
						     id_pool->getBitwidth (from)));
    e.roots.push_back (n);
    std::vector<ChanId> ids;
    ids.push_back (to);
    d.push_back (Dataflow::mkFunc (ids, std::move (e)));
  }
};


 int guard_width (int n) {
   return log_2_round_up (n);
 }
 
int select_guard_width(const Block::Variant_Select &select) {
    return log_2_round_up(select.branches.size());
}

ChanId dflow_freshchan (DataflowChannelManager &maps,
			const Block::Variant_Select &select) 
{
  return maps.fresh (select_guard_width (select));
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
		      maps.mapvar (n.u_var().id), n.width));;
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
		      maps.mapvar (n.u_var().id), n.width));;
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


void computeOutermostBlock (Sequence seq, DataflowChannelManager &dm)
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
	  dm.chanmap[curr->u_basic().stmt.u_send().chan] = seq.startseq;
	}
	else {
	  dm.chanmap[curr->u_basic().stmt.u_send().chan] = curr;
	}
	break;
      }
      case StatementType::Receive: {
	if (dm.chanmap.contains (curr->u_basic().stmt.u_receive().chan)) {
	  // we already have a map for this channel, so we need to
	  // move it to the current outer scope
	  dm.chanmap[curr->u_basic().stmt.u_receive().chan] = seq.startseq;
	}
	else {
	  dm.chanmap[curr->u_basic().stmt.u_receive().chan] = curr;
	}
	break;
      }
      }
      break;
    }
    case BlockType::Par: {
      // A well-formed program cannot have channel conflicts in
      // parallel branches
      std::vector < std::unordered_map<ChanId, Block *> > maps;
      maps.push_back (dm.chanmap);
      for (auto &branch : curr->u_par().branches) {
        dm.chanmap.clear();
	computeOutermostBlock (branch, dm);
        maps.push_back(dm.chanmap);
      }
      dm.chanmap.clear();
      // construct merged map
      for (auto &map : maps) {
         for (auto &[ch, blk] : map) {
             if (dm.chanmap.contains (ch)) { 
                  dm.chanmap[ch] = seq.startseq;
             } 
             else {
                  dm.chanmap[ch] = blk;
             }
        }
      }
      break;
    }
    case BlockType::Select: {
      std::vector < std::unordered_map<ChanId, Block *> > maps;
      auto curmap = dm.chanmap;
      for (auto &branch : curr->u_select().branches) {
        dm.chanmap.clear();
	computeOutermostBlock (branch.seq, dm);
        maps.push_back (dm.chanmap);
      }
      dm.chanmap.clear();
      // construct merged map
      for (auto &map : maps) {
         for (auto &[ch, blk] : map) {
             if (dm.chanmap.contains (ch)) { 
                 dm.chanmap[ch] = curr;
             } 
             else {
                  dm.chanmap[ch] = blk;
             }
        }
      }
      for (auto &[ch, blk] : curmap) {
        if (dm.chanmap.contains (ch)) {
           dm.chanmap[ch] = seq.startseq;
        }
        else {
          dm.chanmap[ch] = blk;
        }
      }
      break;
    }
    case BlockType::DoLoop: {
      auto curmap = dm.chanmap;
      dm.chanmap.clear();
      computeOutermostBlock (curr->u_doloop().branch, dm);
      for (auto &[ch, blk] : curmap) {
        if (dm.chanmap.contains (ch)) {
           dm.chanmap[ch] = seq.startseq;
        }
        else {
          dm.chanmap[ch] = blk;
        }
      }
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

      case BlockType::Select:
	printf ("sel\n");
	break;
	
      case BlockType::DoLoop:
	printf ("loop\n");
	break;
	
      default:
	hassert(false);
	break;
      }
    }
  }
}

// this checks if the guard is in fact
//   v = expect
// if it is, it returns v
// otherwise it returns a null variable
template<typename T, typename U, typename V>
V check_guard_eq (const typename IRExprDag<T,U>::Node *n, int expect)
{
  if (n->type() != IRExprTypeKind::BinaryOp) {
    return V::null_id();					
  }
  if (n->u_e2().op_type != IRBinaryOpType::EQ) {
    return V::null_id();					
  }
  if (n->u_e2().l->type() != IRExprTypeKind::Var) {
    return V::null_id();					
  }
  if (n->u_e2().r->type() != IRExprTypeKind::Const) {
    return V::null_id();					
  }
  if (n->u_e2().r->u_cons().v.getVal(0) != expect) {
    return V::null_id();					
  }
  return n->u_e2().l->u_var().id;
};

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
    /* check if we have a special case of a single Boolean variable */
    if (select.branches.front().g.type() == IRGuardType::Expression &&
	select.branches.front().g.u_e().e.m_dag.roots[0]->type() ==
	IRExprTypeKind::Var) {
      ChanId guard =
       dm.mapvar (select.branches.front().g.u_e().e.m_dag.roots[0]->u_var().id);
      swap = true;
      return guard;
    }
    else if (select.branches.back().g.type() == IRGuardType::Expression &&
	     select.branches.back().g.u_e().e.m_dag.roots[0]->type() ==
	     IRExprTypeKind::Var) {
      ChanId guard =
	dm.mapvar (select.branches.back().g.u_e().e.m_dag.roots[0]->u_var().id);
      swap = false;
      return guard;
    }
  }
  else if (width > 1) {
    /* Check if we have a special case of a single variable, and where
       the guard for the ith branch is (v = i) for the same variable
       v, and where v has the correct bit-width.

       It is too late for this check, because by this time the CHP
       optimization engine has converted this into [ v1 -> .. [] v2
       ... ]

       We need to track which Boolean variables correspond to "g = i"
       comparisons!
    */

    VarId guardvar;
    bool noguard = true;
    int gval = 0;
    bool found_special_case = true;
    bool mustbe_last = false;
    OptionalVarId gret;
    bool found_else = false;

    for (auto &branch : select.branches) {
      if (branch.g.type() == IRGuardType::Expression) {
	if (mustbe_last) {
	  found_special_case = false;
	  break;
	}
	gret = check_guard_eq<ChpTag,VarId,OptionalVarId> (branch.g.u_e().e.m_dag.roots[0], gval);
	if (gret) {
	  if (noguard) {
	    noguard = false;
	    guardvar = *gret;
	  }
	  else {
	    if (guardvar != *gret) {
	      found_special_case = false;
	      break;
	    }
	  }
	}
	else {
	  found_special_case = false;
	  break;
	}
      }
      else {
	mustbe_last = true;
	found_else = true;
      }
      gval++;
    }

    if (found_special_case) {
      if (dm.id_pool->getBitwidth (guardvar) == width) {
	if (!found_else || ((1UL << width) == select.branches.size())) {
	  return dm.mapvar (guardvar);
	}
      }
    }

    // each guard could be a single Boolean
    // if the Boolenans are the same as (v = i) for the same variable
    // v, then we can return "v" as the guard
    std::vector<ChanId> bool_glist;
    for (auto &branch : select.branches) {
      if (branch.g.type() == IRGuardType::Expression) {
	auto n = branch.g.u_e().e.m_dag.roots[0];
	if (n->type() == IRExprTypeKind::Var) {
	  bool_glist.push_back (dm.mapvar (n->u_var().id));
	}
      }
    }
    if (bool_glist.size() + (found_else ? 1 : 0) == select.branches.size()) {
      // this could be possible!
      bool noguard = true;
      ChanId guardvar;
      OptionalChanId gret;
      bool found_special_case = true;
      for (auto &xd : d) {
	if (xd.u.type() == DataflowKind::Func) {
	  int pos = 0;
	  for (auto &id : xd.u_func().ids) {
	    size_t lpos;
	    for (lpos = 0; lpos < bool_glist.size(); lpos++) {
	      if (bool_glist[lpos].m_id == id.m_id) {
		break;
	      }
	    }
	    if (lpos < bool_glist.size()) {
	      // check that the expression is guard = lpos
	      gret = check_guard_eq<ChpTag,ChanId,OptionalChanId> (xd.u_func().e.roots[pos], (int)lpos);
	      if (gret) {
		if (noguard) {
		  noguard = false;
		  guardvar = *gret;
		}
		else if (guardvar != *gret) {
		  found_special_case = false;
		  break;
		}
	      }
	      else {
		found_special_case = false;
		break;
	      }
	    }
	    pos++;
	  }

	  if (found_special_case) {
	    if (dm.id_pool->getBitwidth (guardvar) == width) {
	      if (!found_else || ((1UL << width) == select.branches.size())) {
		return guardvar;
	      }
	    }
	  }
	}
      }
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
  ChanId guard_id = dm.fresh (1);
  std::vector<ChanId> ids;
  ids.push_back (guard_id);
  d.push_back (Dataflow::mkFunc (ids, std::move(guard_dag)));

  ChanId init_guard_id = dm.fresh (1);
  d.push_back (Dataflow::mkInit (guard_id, init_guard_id,
				 BigInt(0), 1));

  return std::pair<ChanId,ChanId>{guard_id, init_guard_id};
}

void genOrigToNewGuard (std::vector<int> &idx,
			ChanId inguard,
			ChanId out_guard,
			DataflowChannelManager &dm,
			std::vector<Dataflow> &d,
			bool swap)
{
  // compute guard as follows:
  //  g = (idx[0] ? 0 : idx[1] ? 1 : .. idx[N] ? n : n + 1)
  if (idx.size() == 0) return;
  
  DExprDag newguard;
  DExprDag::Node *root = NULL;
  size_t pos = idx.size()-1;

  int ibw, obw;

  ibw = dm.id_pool->getBitwidth (inguard);
  obw = dm.id_pool->getBitwidth (out_guard);
  
  if (swap) {
    root = Dataflow::helper_query (newguard,
				   Dataflow::helper_eq (newguard, inguard, ibw, idx[pos]),
				   Dataflow::helper_const (newguard, pos+1, obw),
				   Dataflow::helper_const (newguard, pos, obw));
  }
  else {
    root = Dataflow::helper_query
      (newguard,
       Dataflow::helper_eq (newguard, inguard, ibw, idx[pos]),
       Dataflow::helper_const (newguard, pos, obw),
       Dataflow::helper_const (newguard, pos+1, obw)
       );
  
    while (pos > 0) {
      pos--;
      root = Dataflow::helper_query
	(newguard,
	 Dataflow::helper_eq (newguard, inguard, ibw, idx[pos]),
	 Dataflow::helper_const (newguard, pos, obw),
	 root);
    }
  }
  newguard.roots.push_back (root);

  std::vector<ChanId> ids;
  ids.push_back (out_guard);
  d.push_back (Dataflow::mkFunc(ids, std::move(newguard)));
}

static
void msg (int depth, const char *msg, std::vector<Dataflow> &d)
{
#if 0
  for (int i=0; i < depth; i++) {
    printf (".");
  }
  printf ("%s: %d\n", msg, (int) d.size());
#else
  return;
#endif  
}
  
MultiChannelState reconcileMultiSel (Block *curr,
				     ChanId guard,
				     std::vector<MultiChannelState> &msv,
				     DataflowChannelManager &dm,
				     std::vector<Dataflow> &d,
				     bool swap)
{
  // collect channel requirements
  std::unordered_map<ChanId,std::vector<int>>  chan_idx;
  std::unordered_set<ChanId> variable;
  MultiChannelState ret;

  for (int idx = 0; idx < msv.size(); idx++) {
    for (auto &[ch, _] : msv[idx].datamap) {
      chan_idx[ch].push_back(idx);

      // if the channel is variable token at this point, then it is
      // variable token for the entire block
      if (msv[idx].ctrlmap.contains(ch)) {
	variable.insert (ch);
      }
    }
  }

  // for each channel, we have the index values that correspond to
  // multi-channel access

  for (auto &[ch, idxvec] : chan_idx) {
    std::vector<ChanId> chlist;
    ChanId fresh;
    for (int i=0; i < idxvec.size(); i++) {
      chlist.push_back(msv[idxvec[i]].datamap[ch]);
    }
    if (dm.isOutermostBlock (ch, curr)) {
      fresh = ch;
    }
    else {
      fresh = dm.fresh (ch);
    }
    msg (0, ">> sel-ctrl-start", d);

    // this is the selection control
    OptionalChanId ctrlguard;

    if (idxvec.size() != msv.size() || variable.contains(ch)) {
      std::vector<ChanId> ctrl_chans;
      std::vector<ChanId> ctrl_chansnz;
      for (auto idx : idxvec) {
	if (!msv[idx].ctrlmap.contains (ch)) {
	  auto [ch1, ch2] =  dm.generateMultiBaseCase (d);
	  msv[idx].ctrlmap[ch] = ch1;
	  msv[idx].ctrlmapnz[ch] = ch2;
	}
	ctrl_chans.push_back(msv[idx].ctrlmap[ch]);
	ctrl_chansnz.push_back (msv[idx].ctrlmapnz[ch]);
      }
      // all msv[ ] that contain the channels also include
      // the variable control token sequence

      // we need to convert the guard to a new value based on the
      // index set.
      ChanId ch_guard;

      if (idxvec.size() != msv.size()) {
	// generate local guard
	ch_guard = dm.fresh (guard_width (idxvec.size()+1));
	// one more to indicate "no value"

	genOrigToNewGuard (idxvec, guard, ch_guard, dm, d, swap);

	// note that if swap is true, the guard is backward

	// we also need to generate a dummy extra ctrl channel for the
	// "no value"; this is always "B!0"
	ChanId xtractrl = dm.fresh (2);
	ChanId xtranz = dm.fresh (1);
	d.push_back(Dataflow::mkSrc (xtractrl, BigInt(0), 2));
	ctrl_chans.push_back (xtractrl);
	d.push_back(Dataflow::mkSrc (xtranz, BigInt(0), 1));
	ctrl_chansnz.push_back (xtranz);
      }
      else {
	ch_guard = guard;
      }

      // this is the guard used for the data merge/split
      int selw = 0;
      if (idxvec.size() > 1) {
	ctrlguard = dm.fresh (guard_width (idxvec.size()));
	selw = guard_width (idxvec.size());
      }
      else {
	// no sel out needed for channel
	ctrlguard = OptionalChanId::null_id();
      }

      // B and Bnz output from selection merging
      ChanId cfresh = dm.fresh (2);
      ChanId cfreshnz = dm.fresh (1);

      auto freshalloc = [&] (const OptionalChanId &ch) -> ChanId {
								  if (ch) {
									   return dm.fresh (*ch); } else { return dm.fresh (1); } };

      // XXX: DOES THIS NEED "swap"? I think so, because the guard is backward
      std::list<Dataflow> tmp = Dataflow::mkInstSel (ctrl_chans,
						     ctrl_chansnz,
						     cfresh,
						     cfreshnz,
						     ch_guard,
						     ctrlguard,
						     selw,
						     freshalloc,
						     swap);

      for (auto &xd : tmp) {
	d.push_back (std::move (xd));
      }
      if (ch != fresh) {
	ret.ctrlmap[ch] = cfresh;
	ret.ctrlmapnz[ch] = cfreshnz;
      }
    }
    else {
      // special case, directly use the guard without having to do
      // any merging of control tokens
      ctrlguard = guard;
    }
    if (ctrlguard) {
      if (swap) {
	ChanId c0 = chlist[0];
	ChanId c1 = chlist[1];
	chlist[1] = c0;
	chlist[0] = c1;
      }
      if (!dm.id_pool->isChanInput (ch)) {
	d.push_back(Dataflow::mkMergeMix(*ctrlguard, chlist, fresh));
      }
      else {
	d.push_back(Dataflow::mkSplit(*ctrlguard, fresh,
				      Algo::map1<OptionalChanId> (chlist,
					  [&] (const ChanId &ch) {
					    return OptionalChanId{ch};
								})));
      }
      if (ch != fresh) {
	// only propagate up if we haven't fully reconciled this
	// channel.
	ret.datamap[ch] = fresh;
      }
    }
    else {
      ret.datamap[ch] = chlist[0];
    }
    msg (0, ">> sel-ctrl-end", d);
  }
  return ret;
}

MultiChannelState reconcileMultiSeq (Block *curr,
				     std::vector<MultiChannelState> &msv,
				     DataflowChannelManager &dm,
				     std::vector<Dataflow> &d)
{
  if (msv.size() == 1) {
    return msv[0];
  }

  std::unordered_map<ChanId,std::vector<int>>  chan_idx;
  std::unordered_set<ChanId> variable;
  MultiChannelState ret;

  for (int idx = 0; idx < msv.size(); idx++) {
    for (auto &[ch, rhs] : msv[idx].datamap) {
      chan_idx[ch].push_back(idx);
      // if the channel is variable token at this point, then it is
      // variable token for the entire block
      if (msv[idx].ctrlmap.contains(ch)) {
	variable.insert (ch);
      }
    }
  }

  for (auto &[ch, idxvec] : chan_idx) {
    // don't share these for now
    dm.rr_noctrl.clear();
    dm.rr_ctrl.clear();
    
    if (variable.contains (ch) || idxvec.size() > 1) {
      if (idxvec.size() == 1) {
	// just propagate the single variable channel up
	ret.datamap[ch] = msv[idxvec[0]].datamap[ch];
	ret.ctrlmap[ch] = msv[idxvec[0]].ctrlmap[ch];
	ret.ctrlmapnz[ch] = msv[idxvec[0]].ctrlmapnz[ch];
      }
      else {
	//printf ("/* ch %d, sz %d */\n", ch.m_id, idxvec.size());
	msg (0, ">> seq-ctrl-start", d);
	
	ChanId selout = dm.fresh (guard_width (idxvec.size()));
	bool special_case = true;
	for (auto idx : idxvec) {
	  if (msv[idx].ctrlmap.contains (ch)) {
	    special_case = false;
	  }
	}
	special_case = false;

	std::vector<ChanId> chlist;
	std::vector<ChanId> chlistnz;
	if (!special_case) {
	  for (auto idx : idxvec) {
	    if (!msv[idx].ctrlmap.contains (ch)) {
	      auto [ch1, ch2] = dm.generateMultiBaseCase (d);
	      msv[idx].ctrlmap[ch] = ch1;
	      msv[idx].ctrlmapnz[ch] = ch2;
	    }
	    chlist.push_back(msv[idx].ctrlmap[ch]);
	    chlistnz.push_back(msv[idx].ctrlmapnz[ch]);
	  }
	}

	OptionalChanId cfresh, cfreshnz;

	if (!dm.isOutermostBlock (ch, curr)) {
	  cfresh = dm.fresh (2);
	  cfreshnz = dm.fresh (1);
	}
	else {
	  cfresh = OptionalChanId::null_id();
	  cfreshnz = OptionalChanId::null_id();
	}


	if (special_case) {
	  // control channel is simply 0, 1, 2, 3 (repeat)
	  // variable sequence is      1, 1, 1, 2 (repeat)

	  auto freshalloc = [&] (ChanId &ch) -> ChanId { return dm.fresh (ch); };
	  

	  if (dm.rr_ctrl.contains(idxvec.size())) {
	    auto &res = dm.rr_ctrl[idxvec.size()];
	    dm.generateCopy (res.first, selout, d);
	    if (cfresh) {
	      dm.generateCopy (res.second, *cfresh, d);
	      // constant 1 is the output
	      d.push_back (Dataflow::mkSrc(*cfreshnz, BigInt(1), 1));
	    }
	  }
	  else if (cfresh) {
	    std::list<Dataflow> tmp =
	      Dataflow::mkInstSeqRR(idxvec.size(), cfresh, cfreshnz,
				    selout, freshalloc);
	    for (auto &xd : tmp) {
	      d.push_back (std::move(xd));
	    }
	    
	    dm.rr_ctrl[idxvec.size()] = std::pair<ChanId,ChanId>(selout,*cfresh);
	  }
	  else if (dm.rr_noctrl.contains (idxvec.size())) {
	    ChanId res = dm.rr_noctrl[idxvec.size()];
	    dm.generateCopy (res, selout, d);
	  }
	  else {
	    std::list<Dataflow> tmp =
	      Dataflow::mkInstSeqRR(idxvec.size(), cfresh, cfreshnz, selout, freshalloc);
	    for (auto &xd : tmp) {
	      d.push_back (std::move(xd));
	    }
	    
	    dm.rr_noctrl[idxvec.size()] = selout;
	  }
	  //printf (" ** spec-seq-variable-merge: %d **\n", ch.m_id);
	}
	else {
	  auto freshalloc = [&] (const OptionalChanId &ch) -> ChanId { if (ch) return dm.fresh (*ch); else return dm.fresh(1); };

	  std::list<Dataflow> tmp = Dataflow::mkInstSeq (chlist,
							 chlistnz,
							 cfresh,
							 cfreshnz,
							 selout,
							 freshalloc);

	  for (auto &xd : tmp) {
	    d.push_back (std::move (xd));
	  }
	}
	
	chlist.clear();
	for (auto idx : idxvec) {
	  chlist.push_back(msv[idx].datamap[ch]);
	}
	ChanId fresh;

	if (!dm.isOutermostBlock (ch, curr)) {
	  fresh = dm.fresh (ch);
	}
	else {
	  fresh = ch;
	}
	if (!dm.id_pool->isChanInput (ch)) {
	  ChanId fresh2 = dm.fresh (fresh);
	  d.push_back (Dataflow::mkBuf (fresh2, fresh, dm.bitWidth (fresh)));
	  d.push_back(Dataflow::mkMergeMix (selout, chlist, fresh2));
	}
	else {
	  d.push_back(Dataflow::mkSplit (selout, fresh,
					 Algo::map1<OptionalChanId> (chlist,
								     [&] (const ChanId &ch) {
								       return OptionalChanId{ch}; })));
	  if (!dm.isOutermostBlock (ch, curr)) {
	    ChanId fresh2 = dm.fresh (fresh);
	    d.push_back (Dataflow::mkBuf (fresh2, fresh, dm.bitWidth (fresh)));
	    fresh = fresh2;
	  }
	}
	if (!dm.isOutermostBlock (ch, curr)) {
	  ret.datamap[ch] = fresh;
	  ret.ctrlmap[ch] = (*cfresh);
	  ret.ctrlmapnz[ch] = (*cfreshnz);
	}
	msg (0, ">> seq-ctrl-end", d);
      }
    }
    else {
      // nothing to do, just propagate this up!
      if (!dm.isOutermostBlock (ch, curr)) {
	ret.datamap[ch] = msv[idxvec[0]].datamap[ch];
      }
    }
  }
  
  return ret;
}


bool useGuardMultiLoop (Block *curr,
			const MultiChannelState &msv,
			DataflowChannelManager &dm)
{
  for (auto &[ch, _] : msv.datamap) {
    if (!dm.isOutermostBlock (ch, curr)) {
      return true;
    }
  }
  return false;
}

MultiChannelState reconcileMultiLoop (Block *curr,
				      ChanId guard,
				      MultiChannelState &msv,
				      DataflowChannelManager &dm,
				      std::vector<Dataflow> &d)
{
  MultiChannelState ret;

  auto freshalloc = [&] (const OptionalChanId &ch) -> ChanId
    {
     if (ch) {
	      return dm.fresh (*ch);
     } else {
	     return dm.fresh (1);
     }
    };      
  
  for (auto &[ch, rhs] : msv.datamap) {
    if (dm.isOutermostBlock (ch, curr)) {
      // channel has been fully reconciled!
    }
    else {
      if (!msv.ctrlmap.contains (ch)) {
	auto [ch1, ch2] = dm.generateMultiBaseCase (d);
	msv.ctrlmap[ch] = ch1;
	msv.ctrlmapnz[ch] = ch2;
      }


      ChanId cfresh = dm.fresh (msv.ctrlmap[ch]);
      ChanId cfreshnz = dm.fresh (msv.ctrlmapnz[ch]);
      std::list<Dataflow> tmp = Dataflow::mkInstDoLoop (msv.ctrlmap[ch],
							msv.ctrlmapnz[ch],
							cfresh, cfreshnz,
							guard, freshalloc);

      
      msg (0, ">> do-ctrl-start", d);
      for (auto &x : tmp) {
	d.push_back (std::move (x));
      }
      msg (0, ">> do-ctrl-end", d);
      
      ret.datamap[ch] = rhs;
      ret.ctrlmap[ch] = cfresh;
      ret.ctrlmapnz[ch] = cfreshnz;
    }
  }
  return ret;
}


MultiChannelState createDataflow (Sequence seq, DataflowChannelManager &dm,
				  std::vector<Dataflow> &d)
{
  MultiChannelState ret, empty;

  static int depth = 0;

  msg (depth, "BLOCK-start", d);
  depth++;
  
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
	msg (depth, "assign", d);
	d.push_back (
	     Dataflow::mkFunc(
   	       Algo::map1<ChanId> (curr->u_basic().stmt.u_assign().ids,
			  [&] (const VarId v) { return dm.mapvar (v); }),
	       of_chp_dag (curr->u_basic().stmt.u_assign().e, dm))
	);
	seqs.push_back (empty);
	break;
	
      case StatementType::Send:
	msg (depth, "send", d);
#if 0
	print_chp_block (std::cout, curr, depth);
#endif	
	{ std::vector<ChanId> ids;
	  ChanId sc = curr->u_basic().stmt.u_send().chan;
	  dm.id_pool->setChanDir (sc, false);
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
	    ChanId fresh =  dm.fresh (sc);
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
	msg (depth, "recv", d);
	{
	  ChanId rc = curr->u_basic().stmt.u_receive().chan;
	  dm.id_pool->setChanDir (rc, true);
	  
	  if (dm.chanmap[rc] == curr) {
	    // this is the only place for this receive!
	    if (curr->u_basic().stmt.u_receive().var) {
	      dm.setmap (*curr->u_basic().stmt.u_receive().var, rc);
	    }
	    else {
	      d.push_back (Dataflow::mkSink (rc));
	    }
	    seqs.push_back (empty);
	  }
	  else {
	    // we need a fresh channel here
	    ChanId fresh = dm.fresh (rc);
	    if (curr->u_basic().stmt.u_receive().var) {
	      dm.setmap (*curr->u_basic().stmt.u_receive().var, fresh);
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
      msg (depth, "par", d);
      // A well-formed program cannot have channel conflicts in
      // parallel branches, so any multi-channel access is the union
      // across all parallel branches.
      MultiChannelState ms, acc;
      for (auto &branch : curr->u_par().branches) {
	ms = createDataflow (branch, dm, d);
	acc.datamap = Algo::set_union (acc.datamap, ms.datamap);
	acc.ctrlmap = Algo::set_union (acc.ctrlmap, ms.ctrlmap);
	acc.ctrlmapnz = Algo::set_union (acc.ctrlmapnz, ms.ctrlmapnz);
      }
      seqs.push_back (acc);
      break;
    }
      
    case BlockType::Select:
      msg (depth, "sel", d);
      {
	// deal with guards, phiinv, and phi
	ChanId guard;
	bool swap = false;
#if 0	
	if (!curr->u_select().splits.empty()
	    || !curr->u_select().merges.empty()) {
	  // we need a guard!
	}
#endif	
	guard = nodes_add_guard (curr->u_select(), swap, d, dm);

	// phiinv
	for (auto &split : curr->u_select().splits) {
	  std::vector<OptionalChanId> out;
	  out = Algo::map1<OptionalChanId> (split.branch_ids,
			    [&] (OptionalVarId v) {
			      if (v) {
				return OptionalChanId{dm.mapvar ((*v))};
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
	  d.push_back (Dataflow::mkSplit(guard,dm.mapvar (split.pre_id), out));
	}

	std::vector<MultiChannelState> msv;
	for (auto &branch : curr->u_select().branches) {
	  msv.push_back (createDataflow (branch.seq, dm, d));
	}

	for (auto &merge : curr->u_select().merges) {
	  std::vector<ChanId> inp;
	  inp = Algo::map1<ChanId> (merge.branch_ids,
				    [&] (VarId v) { return dm.mapvar (v); });

	  if (swap) {
	    ChanId c0 = inp[0];
	    ChanId c1 = inp[1];
	    inp[1] = c0;
	    inp[0] = c1;
	  }
	  
	  d.push_back (Dataflow::mkMergeMix (OptionalChanId{guard}, inp,
					     dm.mapvar (merge.post_id)));
	}

	// reconcile multi-channel access with a guard channel
	seqs.push_back (reconcileMultiSel (curr, guard, msv, dm, d, swap));
      }
      break;
      
    case BlockType::DoLoop: {
      msg (depth, "loop", d);
      MultiChannelState ms = createDataflow (curr->u_doloop().branch, dm, d);

      // deal with loopphi, phiinv, loopphinv
      std::pair<ChanId, ChanId> guards;

      if (curr->u_doloop().loop_phis.size() > 0
	  || curr->u_doloop().in_phis.size() > 0
	  || curr->u_doloop().out_phis.size() > 0
	  || useGuardMultiLoop (curr, ms, dm)) {
	guards = nodes_add_loopguard (curr->u_doloop(), d, dm);
      }

      // handle loop-phis first
      for (auto &loopphi : curr->u_doloop().loop_phis) {
	std::vector<OptionalChanId> outp;

	ChanId feedback = dm.fresh (loopphi.bodyout_id);

	if (loopphi.post_id) {
	  outp.push_back (dm.mapvar (*loopphi.post_id));
	}
	else {
	  outp.push_back (OptionalChanId::null_id());
	}
	outp.push_back (feedback);

	// printf (">> loop: %d .. ", (int)d.size());
	
	d.push_back (Dataflow::mkSplit
		     (guards.first, dm.mapvar (loopphi.bodyout_id),
		      outp));

	ChanId f2 = dm.fresh (feedback);
	d.push_back (Dataflow::mkBuf (feedback, f2, dm.bitWidth (feedback)));
	feedback = f2;

	std::vector<ChanId> inp;
	inp.push_back (dm.mapvar (loopphi.pre_id));
	inp.push_back (feedback);
	d.push_back (Dataflow::mkMergeMix
		     (guards.second, inp, dm.mapvar (loopphi.bodyin_id)));
	
	//	printf ("%d <<\n", (int)d.size());
	
      }

      for (auto &inphi : curr->u_doloop().in_phis) {
	std::vector<ChanId> inp;
	ChanId feedback = dm.fresh (inphi.pre_id);
	inp.push_back (dm.mapvar (inphi.pre_id));
	inp.push_back (feedback);

	//	printf (">> inphi: %d .. ", (int)d.size());
	
	d.push_back (Dataflow::mkMergeMix
		     (OptionalChanId{guards.second}, inp,
		      dm.mapvar (inphi.bodyin_id)));

	ChanId f2 = dm.fresh (feedback);
	d.push_back (Dataflow::mkBuf (f2, feedback, dm.bitWidth (feedback)));
	feedback = f2;
	
	std::vector<OptionalChanId> outp;
	outp.push_back (OptionalChanId::null_id());
	outp.push_back (feedback);
	d.push_back (Dataflow::mkSplit
		     (guards.first, dm.mapvar (inphi.bodyin_id),
		      outp));


	//	printf ("%d <<\n", (int)d.size());
	
      }

      for (auto &outphi : curr->u_doloop().out_phis) {
	std::vector<OptionalChanId> outp;
	outp.push_back (dm.mapvar (outphi.post_id));
	outp.push_back (OptionalChanId::null_id());
	d.push_back (Dataflow::mkSplit
		     (guards.first, dm.mapvar (outphi.bodyout_id),
		      outp));
      }

      // reconcile multi-channel access
      seqs.push_back (reconcileMultiLoop (curr, guards.first, ms, dm, d));
      break;
    }
      
    case BlockType::StartSequence:
    case BlockType::EndSequence:
      hassert(false);
      break;
    }
    curr = curr->child();
  }

  // reconcile sequencing multi-channel access
  ret = reconcileMultiSeq (seq.startseq, seqs, dm, d);

  depth--;
  msg (depth, "BLOCK-end", d);
  
  return ret;
}


//
// single use: it has a single use, plus the use is only on the LHS of
// a function. In that case, replace the DEF with the RHS of the function.
// 
void computeUses (int idx, Dataflow &d,
		  std::unordered_map<ChanId, std::list<int>> &uses,
		  std::unordered_map<ChanId, int> &defs)
{
  auto update = [&] (const ChanId &ch)
    {
     uses[ch].push_back (idx);
    };

  auto updatedefs = [&] (const ChanId &ch)
    {
     hassert (!defs.contains(ch));
     defs[ch] = idx;
    };
  
  switch (d.u.type()) {
  case DataflowKind::Func:
    DExprDag::iterNodes (d.u_func().e, [&] (const DExprDag::Node &n) {
	switch (n.type()) {
	case IRExprTypeKind::Var:
	  update(n.u_var().id);
	  break;
	default:
	  break;
	}
      });
    for (auto &id : d.u_func().ids) {
      updatedefs (id);
    }
    break;

  case DataflowKind::Init:
    update(d.u_init().lhs);
    updatedefs(d.u_init().rhs);
    break;

  case DataflowKind::Split:
    update(d.u_split().cond_id);
    update(d.u_split().in_id);
    for (auto &id : d.u_split().out_ids) {
      if (id) {
	updatedefs (*id);
      }
    }
    break;

  case DataflowKind::MergeMix:
    if (d.u_mergemix().cond_id) {
      update (*d.u_mergemix().cond_id);
    }
    for (auto x : d.u_mergemix().in_ids) {
      update (x);
    }
    updatedefs (d.u_mergemix().out_id);
    break;

  case DataflowKind::Arbiter:
    // no arbiters!
    hassert (false);
    break;

  case DataflowKind::Sink:
    break;
  }
}


void replaceChan (Dataflow &d, ChanId src, ChanId dst)
{
  switch (d.u.type()) {
  case DataflowKind::Func:
    for (auto &id : d.u_func().ids) {
      if (id == src) {
	id = dst;
      }
    }
    break;

  case DataflowKind::Init:
    hassert (src == d.u_init().rhs);
    d.u_init().rhs = dst;
    break;

  case DataflowKind::Split:
    for (auto &id : d.u_split().out_ids) {
      if (id && (*id == src)) {
	id = OptionalChanId{dst};
      }
    }
    break;

  case DataflowKind::MergeMix:
    hassert (d.u_mergemix().out_id == src);
    d.u_mergemix().out_id = dst;
    break;

  case DataflowKind::Arbiter:
    // no arbiters!
    hassert (false);
    break;

  case DataflowKind::Sink:
    break;
  }
}

void replaceChanUses (Dataflow &d, ChanId src, ChanId dst)
{
  switch (d.u.type()) {
  case DataflowKind::Func:
    DExprDag::mapNodes (d.u_func().e, [&] (DExprDag::Node &n) {
	switch (n.type()) {
	case IRExprTypeKind::Var:
	  if (n.u_var().id == src) {
	    n.u_var().id = dst;
	  }
	  break;
	default:
	  break;
	}
      });
    break;

  case DataflowKind::Init:
    hassert (src == d.u_init().lhs);
    d.u_init().lhs = dst;
    break;

  case DataflowKind::Split:
    if (d.u_split().cond_id == src) {
      d.u_split().cond_id = dst;
    }
    if (d.u_split().in_id == src) {
      d.u_split().in_id = dst;
    }
    break;

  case DataflowKind::MergeMix:
    if (d.u_mergemix().cond_id &&
	*d.u_mergemix().cond_id == src) {
      d.u_mergemix().cond_id = OptionalChanId{dst};
    }
    for (auto &x : d.u_mergemix().in_ids) {
      if (x == src) {
	x = dst;
      }
    }
    break;

  case DataflowKind::Arbiter:
    // no arbiters!
    hassert (false);
    break;

  case DataflowKind::Sink:
    break;
  }
}

static void mark_node (void *cookie, AGraph *g, AGvertex *v, bool onentry)
{
  if (!onentry) {
    v->isio = 1;
  }
}

static void mark_edge (void *cookie, AGraph *g, AGedge *e)
{
  if (!g->getVertex (e->dst)->visited) {
    // tree edge
  }
  else {
    if (!g->getVertex(e->dst)->isio) {
      printf ("found back edge!\n");
      // back edge
    }
    else {
      // cross edge
    }
  }
}


void elimZeroSlackCycles (std::vector<Dataflow> &d)
{
  // Form a graph for each definition in the dataflow list
  int count = 0;

  std::unordered_map<ChanId,int> chanmap;
  std::vector<ChanId> invmap;
  
  
  for (auto &di : d) {
    switch (di.u.type()) {
    case DataflowKind::Init:
      chanmap[di.u_init().rhs] = count;
      count++;
      break;

    case DataflowKind::Split:
      for (auto &rhs : di.u_split().out_ids) {
	if (rhs) {
	  chanmap[*rhs] = count;
	  invmap.push_back(*rhs);
	  count++;
	}
      }
      break;
      
    case DataflowKind::MergeMix:
      chanmap[di.u_mergemix().out_id] = count;
      invmap.push_back(di.u_mergemix().out_id);
      count++;
      break;

    case DataflowKind::Func:
      for (auto &ch : di.u_func().ids) {
	chanmap[ch] = count;
	count++;
      }
      break;

    case DataflowKind::Sink:
      break;

    case DataflowKind::Arbiter:
      hassert (false);
      break;
    }
  }
  if (count < 2) return;

  AGraph *a = new AGraph();

  for (int i=0; i < count; i++) {
    a->addVertex();
  }
  for (auto &di : d) {
    switch (di.u.type()) {
    case DataflowKind::Init:
      if (chanmap.contains (di.u_init().lhs)) {
	a->addEdge (chanmap[di.u_init().lhs], chanmap[di.u_init().rhs]);
      }
      break;

    case DataflowKind::Split:
      for (auto &rhs : di.u_split().out_ids) {
	if (rhs) {
	  if (chanmap.contains (di.u_split().in_id)) {
	    a->addEdge (chanmap[di.u_split().in_id], chanmap[*rhs]);
	  }
	  if (chanmap.contains (di.u_split().cond_id)) {
	    a->addEdge (chanmap[di.u_split().cond_id], chanmap[*rhs]);
	  }
	}
      }
      break;
      
    case DataflowKind::MergeMix:
      for (auto &lhs : di.u_mergemix().in_ids) {
	if (chanmap.contains (lhs)) {
	  a->addEdge (chanmap[lhs], chanmap[di.u_mergemix().out_id]);
	}
      }
      if (di.u_mergemix().cond_id) {
	if (chanmap.contains (*di.u_mergemix().cond_id)) {
	  a->addEdge (chanmap[*di.u_mergemix().cond_id], chanmap[di.u_mergemix().out_id]);
	}
      }
      break;

    case DataflowKind::Func:
      DExprDag::iterNodes (di.u_func().e,
			   [&] (const DExprDag::Node &n) {
			     if (n.type() == IRExprTypeKind::Var) {
			       if (chanmap.contains (n.u_var().id)) {
				 for (auto &ids : di.u_func().ids) {
				   a->addEdge (chanmap[n.u_var().id], chanmap[ids]);
				 }
			       }
			     }
			   });
      break;

    case DataflowKind::Sink:
      break;

    case DataflowKind::Arbiter:
      hassert (false);
      break;
    }
  }

  FILE *fp = fopen ("test.dot", "w");
  a->printDot (fp, "Test");
  fclose (fp);

  a->runDFS (NULL, mark_node, mark_edge);

  delete a;
}

 

} // namespace

void printDataflowExpr (std::ostream &os, const DExpr &d)
{
  print_dexpr (os, d);
}

std::vector<Dataflow> chp_to_dataflow(GraphWithChanNames &gr)
{
  std::vector<Dataflow> d;
  DataflowChannelManager m;

  hassert (gr.graph.is_static_token_form);
  
#if 0
  printf ("/*#############################\n");
  print_chp(std::cout, gr.graph);
  printf ("\n#############################*/\n\n");
#endif

  m.id_pool = &gr.graph.id_pool();

  // recursively translate, while doing multichannel stuff
  // simultaneously
  computeOutermostBlock (gr.graph.m_seq, m);

  // now create dataflow blocks!
  MultiChannelState ret = createDataflow (gr.graph.m_seq, m, d);
  hassert (ret.datamap.empty() && ret.ctrlmap.empty());

#if 0
  printf ("/*---\n");
  int pos = 0;
  for (auto &elem : d) {
    printf ("I0 %3d :: ", pos);
    elem.Print (std::cout);
    pos++;
  }
  printf ("---*/\n");
#endif  

  
  std::unordered_map<ChanId, std::list<int>> dfuses;
  std::unordered_map<ChanId, int> dfdefs;
  
  int idx = 0;
  for (auto &x : d) {
    computeUses (idx, x, dfuses, dfdefs);
    idx++;
  }

  // sink UNUSED channels that are part of the varmap!
  for (auto &[var, ch] : m.varmap) {
    if (!dfuses.contains (ch)) {
      d.push_back (Dataflow::mkSink (ch));
    }
  }

#if 0
  printf ("/*---\n");
  int pos = 0;
  for (auto &elem : d) {
    printf ("I %3d :: ", pos);
    elem.Print (std::cout);
    pos++;
  }
  printf ("---*/\n");
#endif  

  // strip out single fanout buffers

  // check that any used variable is defined or an input
  // and any defined variable is used or an output
  for (auto &[x,l] : dfuses) {
    if (!(dfdefs.contains(x) || gr.name_from_chan.contains (x))) {
      warning ("I-Internal channel C%d is used but not defined?",
	       (int)x.m_id);
    }
  }

#if 0
  // This can now happen, because we *always* generate guard channels
  // because we don't know until we process the bodies if we need the
  // guard channels or not.
  for (auto &[x,l] : dfdefs) {
    if (!(dfuses.contains(x) || gr.name_from_chan.contains (x))) {
      warning ("I-Internal channel C%d is defined, but not used?",
	       (int)x.m_id);
    }
  }
#endif  

  std::unordered_set<std::pair<int,int>> delidx;
  
  // This can happen because a guard for a split/merge is unused!
  std::unordered_set<ChanId> dead_chans;

  std::unordered_set<ChanId> used_dead;

  std::unordered_set<ChanId> processed;

  while (1) {
    dead_chans.clear();

    for (auto &[x,l] : dfdefs) {
      if (processed.contains (x)) continue;
      if (!(dfuses.contains(x) || gr.name_from_chan.contains (x))) {
	dead_chans.insert (x);
      }
    }
    
    if (dead_chans.empty()) {
      break;
    }

    for (auto &ch : dead_chans) {
      processed.insert (ch);
      auto dead_idx = dfdefs[ch];
      used_dead.clear();
      switch (d[dead_idx].u.type()) {
      case DataflowKind::Func:
	// for each LHS variable, remove its use from the use-list
	{
	  std::vector<DExprDag::Node *> newroots;
	  std::vector<ChanId> newids;
	  DExprDag::Node *dead_root = NULL;
	  std::unordered_set<ChanId> used_rest;
	  for (int ii = 0; ii < d[dead_idx].u_func().ids.size(); ii++) {
	    if (d[dead_idx].u_func().ids[ii] != ch) {
	      newids.push_back (d[dead_idx].u_func().ids[ii]);
	      newroots.push_back (d[dead_idx].u_func().e.roots[ii]);
	      // get used vars in the live expressions
	      DExprDag::iterNodesBelow(d[dead_idx].u_func().e.roots[ii],
				       [&](const DExprDag::Node &n){
		      if (n.type() == IRExprTypeKind::Var) {
			used_rest.insert(n.u_var().id);
		      } });
	    }
	    else {
	      dead_root = d[dead_idx].u_func().e.roots[ii];
	      // get used vars in the dead expression
	      DExprDag::iterNodesBelow(dead_root,
				       [&](const DExprDag::Node &n){
		      if (n.type() == IRExprTypeKind::Var) {
			used_dead.insert(n.u_var().id);
		      } });
	    }
	  }
	  Assert (dead_root != NULL, "What?");
	  if (newids.size() == 0) {
	    delidx.insert (std::pair(dead_idx, -1));
	  }
	  else {
	    d[dead_idx].u_func().ids = newids;
	    d[dead_idx].u_func().e.roots = newroots;
	  }
	  // actual dead nodes are those that were used in the dead
	  // subgraph MINUS those that are in the live one
	  used_dead = Algo::set_minus (used_dead, used_rest);

	}
	break;

      case DataflowKind::Split:
	// if the split is *,*,* then remove the lhs variables
	{
	  int total, opt;
	  total = 0;
	  opt = 0;
	  for (auto &rhs : d[dead_idx].u_split().out_ids) {
	    total++;
	    if (rhs) {
	      if ((*rhs) == ch) {
		rhs = OptionalChanId::null_id();
		opt++;
	      }
	    }
	    else {
	      opt++;
	    }
	  }
	  if (opt == total) {
	    used_dead.insert (d[dead_idx].u_split().in_id);
	    used_dead.insert (d[dead_idx].u_split().cond_id);
	    delidx.insert (std::pair(dead_idx, -1));
	  }
	}
	break;

      case DataflowKind::MergeMix:
	// remove the LHS variables from the use-list
	if (d[dead_idx].u_mergemix().cond_id) {
	  used_dead.insert (*(d[dead_idx].u_mergemix().cond_id));
	}
	for (auto &dch : d[dead_idx].u_mergemix().in_ids) {
	  used_dead.insert (dch);
	}
	delidx.insert (std::pair(dead_idx, -1));
	break;

      case DataflowKind::Init:
	// remove the lhs variable from the use list
	used_dead.insert (d[dead_idx].u_init().lhs);
	delidx.insert (std::pair(dead_idx, -1));
	break;

      case DataflowKind::Sink:
	break;
	
      case DataflowKind::Arbiter:
	fatal_error ("What?");
	break;
      }

      // now for each dead channel, delete the dead_idx from the
      // used list
      for (auto &ud : used_dead) {
	dfuses[ud].remove (dead_idx);
	if (dfuses[ud].empty()) {
	  dfuses.erase (ud);
	  if (gr.name_from_chan.contains (ud)) {
	    d.push_back (Dataflow::mkSink (ud));
	  }
	}
      }
    }
  }
  
  // delete dead code.
  // code is dead if:
  //   the def has no uses
  //   the channel defined is not in the original chp
  for (auto &[ch, idx] : dfdefs) {
    if (!dfuses.contains (ch) && !gr.name_from_chan.contains (ch)) {
      // check this
      if (d[idx].u.type() == DataflowKind::Func) {
	int subidx = 0;
	for (auto &xid : d[idx].u_func().ids) {
	  if (xid == ch) {
	    delidx.insert (std::pair(idx,subidx));
	  }
	  subidx++;
	}
      }
    }
  }

#if 0
  idx = 0;
  for (auto &x : d) {
    printf ("F %3d :: ", idx);
    x.Print (std::cout);
    idx++;
  }
  printf ("----\n");
#endif  

  idx = 0;
  for (auto &x : d) {
    // look for x -> y
    if (x.keep) {
      idx++;
      continue;
    }
    if (x.u.type() == DataflowKind::Func && x.u_func().ids.size() == 1) {
      // single RHS
      int count = 0;
      OptionalChanId lhs = OptionalChanId::null_id();
      DExprDag::iterNodes(x.u_func().e,[&](const DExprDag::Node &n){
	  count++;
	  if (n.type() == IRExprTypeKind::Var) {
	    lhs = OptionalChanId{n.u_var().id};
	  }
	});
      if (count == 1 && lhs) {
	// single LHS, variable
	if (dfuses.contains(x.u_func().ids[0])) {
	  // there are uses for the rhs, so use simple forward
	  // substitution, and mark this index deleted
	  for (auto uses : dfuses[x.u_func().ids[0]]) {
	    replaceChanUses (d[uses], x.u_func().ids[0], *lhs);
	    // now every location that used to use the old id now uses
	    // the new id
	    dfuses[*lhs].push_back (uses);
	  }
	  // this is going to be an internal channel, but check just
	  // in case...
	  if (!gr.name_from_chan.contains (x.u_func().ids[0])) {
	    delidx.insert (std::pair(idx,-1));
	  }
	}
	else {
	  // no uses for the RHS, now we see if the LHS has additional
	  // uses.
	  if (dfuses[*lhs].front() == dfuses[*lhs].back()) {
	    // no other uses of the lhs, replace the def
	    if (dfdefs.contains (*lhs)) {
	      replaceChan (d[dfdefs[*lhs]], *lhs, x.u_func().ids[0]);

	      // and now replace all uses of RHS with *lhs
	      if (dfuses.contains (x.u_func().ids[0])) {
		Assert (0, "This should not happen!");
		for (auto uses : dfuses[x.u_func().ids[0]]) {
		  replaceChanUses (d[uses], x.u_func().ids[0], *lhs);
		}
	      }
	      delidx.insert (std::pair(idx,-1));
	    }
	    else {
	      // no defs for the lhs; must be a primary input!
	      Assert (gr.name_from_chan.contains(*lhs), "Not sure how this happeed!");
	    }
	  }
	}
      }
    }
    idx++;
  }
#if 0  
  for (auto &[x,y] : delidx) {
    printf ("  --> (%d,%d)\n", x, y);
  }
  printf ("----\n");
#endif  
  

  std::vector<Dataflow> dfinal;
  idx = 0;
  for (auto &elem : d) {
    if (!delidx.contains (std::pair(idx,-1))) {
      // check if there are sub-indices to be deleted
      if (elem.u.type() == DataflowKind::Func) {
	std::vector<DExprDag::Node *> newroots;
	std::vector<ChanId> newids;
	for (int ii = 0; ii < elem.u_func().ids.size(); ii++) {
	  if (!delidx.contains(std::pair(idx,ii))) {
	    newids.push_back (elem.u_func().ids[ii]);
	    newroots.push_back (elem.u_func().e.roots[ii]);
	  }
	}
	if (newroots.size() != 0) {
	  elem.u_func().e.roots = newroots;
	  elem.u_func().ids = newids;
	  dfinal.push_back (std::move (elem));
	}
      }
      else {
	// keep it!
	dfinal.push_back (std::move (elem));
      }
    }
    idx++;
  }

  dfuses.clear();
  dfdefs.clear();
  idx = 0;
  //  printf ("/*\n");
  for (auto &x : dfinal) {
#if 0
    printf ("F %3d :: ", idx);
    x.Print (std::cout);
#endif
    computeUses (idx, x, dfuses, dfdefs);
    idx++;
  }
  // printf ("*/\n");

  // check that any used varaible is defined or an input
  // and any defined variable is used or an output
  for (auto &[x,l] : dfuses) {
    if (!(dfdefs.contains(x) || gr.name_from_chan.contains (x))) {
      warning ("Internal channel C%d is used but not defined?",
	       (int)x.m_id);
    }
  }

  for (auto &[x,l] : dfdefs) {
    if (!(dfuses.contains(x) || gr.name_from_chan.contains (x))) {
      warning ("Internal channel C%d is defined, but not used?",
	       (int)x.m_id);
    }
  }

  // now we analyze the graph to see if we have zero slack cycles; if
  // we do, we add explicit buffers!
  //elimZeroSlackCycles (dfinal);
  
  return dfinal;
}

namespace {

void toAct (list_t *l, Dataflow &d, var_to_actvar &map)
{
  act_dataflow_element *e;
  ActExprIntType t;
  Assert (l, "What?");

  auto varToId = [&] (const ChanId &v) { return map.chanMap (v); };
  
  switch (d.u.type()) {
  case DataflowKind::Func:
    for (int i=0; i < d.u_func().ids.size(); i++) {
      NEW (e, act_dataflow_element);
      e->t = ACT_DFLOW_FUNC;
      e->u.func.nbufs = NULL;
      e->u.func.init = NULL;
      e->u.func.istransparent = 0;
      e->u.func.rhs = map.chanMap (d.u_func().ids[i]);
      if (map.isBool (d.u_func().ids[i])) {
	t = ActExprIntType::Bool;
      }
      else {
	t = ActExprIntType::Int;
      }
      e->u.func.lhs =
	template_func_new_expr_from_irexpr (*d.u_func().e.roots[i], t, varToId);

      if (d.keep) {
	e->u.func.nbufs = const_expr (2);
      }
      
      list_append (l, e);
    }
    break;

  case DataflowKind::Init:
    NEW (e, act_dataflow_element);
    e->t = ACT_DFLOW_FUNC;
    e->u.func.nbufs = NULL;
    e->u.func.rhs = map.chanMap (d.u_init().rhs);
    NEW (e->u.func.lhs, Expr);
    e->u.func.lhs->type = E_VAR;
    e->u.func.lhs->u.e.l = (Expr *) map.chanMap (d.u_init().lhs);
    e->u.func.lhs->u.e.r = NULL;
    if (map.isBool (d.u_init().lhs)) {
      e->u.func.init = const_expr_bool (d.u_init().v.getVal (0));
    }
    else {
      e->u.func.init = const_expr (d.u_init().v.getVal (0));
    }
    e->u.func.nbufs = const_expr (1);
    list_append (l, e);
    break;

  case DataflowKind::Split:
    NEW (e, act_dataflow_element);
    e->t = ACT_DFLOW_SPLIT;
    e->u.splitmerge.guard = map.chanMap (d.u_split().cond_id);
    MALLOC (e->u.splitmerge.multi, ActId *, d.u_split().out_ids.size());
    for (size_t i=0; i < d.u_split().out_ids.size(); i++) {
      if (d.u_split().out_ids[i]) {
	e->u.splitmerge.multi[i] = map.chanMap (*d.u_split().out_ids[i]);
      }
      else {
	e->u.splitmerge.multi[i] = NULL;
      }
    }
    e->u.splitmerge.nmulti = d.u_split().out_ids.size();
    e->u.splitmerge.single = map.chanMap (d.u_split().in_id);
    e->u.splitmerge.nondetctrl = NULL;
    list_append (l, e);
    break;

  case DataflowKind::MergeMix:
    NEW (e, act_dataflow_element);
    if (d.u_mergemix().cond_id) {
      e->t = ACT_DFLOW_MERGE;
      e->u.splitmerge.guard = map.chanMap (*d.u_mergemix().cond_id);
    }
    else {
      e->t = ACT_DFLOW_MIXER;
      e->u.splitmerge.guard = NULL;
    }
    MALLOC (e->u.splitmerge.multi, ActId *, d.u_mergemix().in_ids.size());
    for (size_t i=0; i < d.u_mergemix().in_ids.size(); i++) {
      e->u.splitmerge.multi[i] = map.chanMap (d.u_mergemix().in_ids[i]);
    }
    e->u.splitmerge.nmulti = d.u_mergemix().in_ids.size();
    e->u.splitmerge.single = map.chanMap (d.u_mergemix().out_id);
    e->u.splitmerge.nondetctrl = NULL;
    list_append (l, e);
    break;

  case DataflowKind::Arbiter:
    // no arbiters!
    hassert (false);
    break;

  case DataflowKind::Sink:
    NEW (e, act_dataflow_element);
    e->t = ACT_DFLOW_SINK;
    e->u.sink.chan = map.chanMap (d.u_sink().in_id);
    list_append (l, e);
    break;
  }
}

}

act_dataflow *dataflow_to_act (std::vector<Dataflow> &d,
			       GraphWithChanNames &gr,
			       std::vector<ActId *> &newnames,
			       Scope *s) 
{
  var_to_actvar table(s, &gr.graph.id_pool());
  act_dataflow *ret;

  for (auto &[x, v] : gr.name_from_chan) {
    table.name_from_chan[x] = v;
  }
#if 0  
  for (auto &[x, v] : gr.name_from_var) {
    table.name_from_var[x] = v;
  }
#endif  

  NEW (ret, act_dataflow);

  ret->dflow = list_new ();
  ret->isexpanded = 1;

  for (auto &x : d) {
    toAct (ret->dflow, x, table);
  }
  ret->order = NULL;

  newnames = std::move(table.newvars);
  
  return ret;
}

} // namespace ChpOptimize
