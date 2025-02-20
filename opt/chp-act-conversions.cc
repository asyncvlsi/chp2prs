/*************************************************************************
 *
 *  This file is part of the ACT library
 *
 *  Copyright (c) 2018-2020 Lincoln Berkley
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

#include "chp-graph.h"
#include "ir-expr-act-conversion.h"
#include "act-names.h"
#include <act/act.h>
#include <act/expr.h>

namespace ChpOptimize {
namespace {
std::unique_ptr<ChpExpr> new_chpexpr_from_expr(NameParsingIdPool &id_pool,
                                               const ActExprStruct *o) {
    return template_func_new_irexpr_from_expr<ChpTag, VarId, ChanId, ManageMemory::yes>(
        o, [&](ActId *act_id) -> std::pair<VarId, int> {
            OptionalVarId id = id_pool.varIdFromActId(act_id);
            hassert(id);
            return {*id, id_pool.getBitwidth(*id)};
        },
	[&](ActId *act_id) -> std::pair<ChanId, int> {
	  OptionalChanId id = id_pool.chanIdFromActId(act_id);
	  hassert (id);
	  return {*id, id_pool.getBitwidth(*id)};											});
}
// struct expr *new_expr_from_chpexpr(IdPool &id_pool, const ChpExpr &o,
// ActExprIntType expectedType) {
//    return template_func_new_expr_from_irexpr<ChpTag, VarId,
//    ManageMemory::yes>(
//        o, expectedType, [&](VarId var_id) -> ActId * { return
//        id_pool.toActId(var_id); });
//}
// struct expr *new_expr_from_chpexprdag_node(IdPool &id_pool, const
// ChpExprDag::Node &o, ActExprIntType expectedType) {
//    return template_func_new_expr_from_irexpr<ChpTag, VarId,
//    ManageMemory::no>(
//        o, expectedType, [&](VarId var_id) -> ActId * { return
//        id_pool.toActId(var_id); });
//}

IRGuard new_irguard_from_expr(NameParsingIdPool &id_pool,
                              const ActExprStruct *o) {
    if (!o)
        return IRGuard::makeElse();
    if (o->type == E_PROBE) {
        hassert(false); // TODO figure out how to extract var id
    }
    return IRGuard::makeExpression(
        ChpExprSingleRootDag ::of_expr(new_chpexpr_from_expr(id_pool, o)));
}

// TODO figure out how when to turn int constants of 1 and 0 back into E_TRUE
// and E_FALSE struct expr *new_expr_from_irguard(IdPool &id_pool, const IRGuard
// &o) {
//    switch (o.type()) {
//    case IRGuardType::Expression: {
//        return new_expr_from_chpexprdag_node(id_pool, *o.u_e().e.root(),
//        ActExprIntType::Bool);
//    }
//    case IRGuardType::Else:
//        return nullptr;
//    }
//    hassert(false);
//    return nullptr;
//}
//
// act_chp_lang chp_struct_from_statement(IdPool &id_pool, const Statement &o) {
//    act_chp_lang r;
//    r.label = nullptr;
//    switch (o.type()) {
//    case StatementType::Assign:
//        hassert(o.u_assign().ids.size() == o.u_assign().e.roots.size());
//        // Make sure to run the pass that enforces this!
//        hassert(o.u_assign().ids.size() == 1);
//
//        r.type = ACT_CHP_ASSIGN;
//        r.u.assign.id = id_pool.toActId(o.u_assign().ids[0]);
//        // TODO add support for bool channel outputs (i.e. turn into into
//        bool, or add a temporary?) r.u.assign.e =
//        new_expr_from_chpexprdag_node(id_pool, *o.u_assign().e.roots.at(0),
//                                                     ActExprIntType::Int); //
//                                                     TODO should I malloc
//                                                     these instead?
//        break;
//    case StatementType::Receive:
//        r.type = ACT_CHP_RECV;
//        r.u.comm.chan = id_pool.toActId(o.u_receive().chan);
//        r.u.comm.var = id_pool.toActId(o.u_receive().var);
//        // 0    : no conversion needed for the variable specified
//        // 1    : bool( var )
//        // 2    : int ( var )
//        r.u.comm.convert = id_pool.getIsBool(o.u_receive().chan) ? 1 : 0;
//        r.u.comm.e = nullptr;
//        r.u.comm.flavor = 0;
//        break;
//    case StatementType::Send: {
//        r.type = ACT_CHP_SEND;
//        r.u.comm.chan = id_pool.toActId(o.u_send().chan);
//
//        // TODO should I malloc these instead?
//        if (id_pool.getIsBool(o.u_send().chan)) {
//            r.u.comm.e = new_expr_from_chpexprdag_node(id_pool,
//            *o.u_send().e.root(), ActExprIntType::Bool);
//        } else {
//            r.u.comm.e = new_expr_from_chpexprdag_node(id_pool,
//            *o.u_send().e.root(), ActExprIntType::Int);
//        }
//        r.u.comm.var = nullptr;
//        r.u.comm.flavor = 0;
//        break;
//    }
//    }
//
//    return r;
//}

Sequence parse_into_ir(const act_chp_lang *c, BlockAllocator &blockAllocator,
                       NameParsingIdPool &id_pool) {
    switch (c->type) {
    case ACT_CHP_SEMI: {
        Block *entry = nullptr, *exit_b = nullptr;
        for (listitem_t *li = list_first(c->u.semi_comma.cmd); li;
             li = list_next(li)) {
            Sequence subseq = parse_into_ir(
                (const act_chp_lang *)list_value(li), blockAllocator, id_pool);
            if (!subseq.empty()) {
                Block *sub_start = subseq.startseq->child();
                Block *sub_end = subseq.endseq->parent();
                Block::disconnect(subseq.startseq, sub_start);
                Block::disconnect(sub_end, subseq.endseq);

                if (!entry) {
                    entry = sub_start;
                } else {
                    Block::connect(exit_b, sub_start);
                }
                exit_b = sub_end;
            }
        }
        if (!entry) {
            hassert(!exit_b);
            return blockAllocator.newSequence({});
        } else {
            hassert(exit_b);
            return blockAllocator.newSequence(entry, exit_b);
        }
    }

    case ACT_CHP_COMMA: {
        Block *parallel = blockAllocator.newBlock(Block::makeParBlock());
        for (listitem_t *li = list_first(c->u.semi_comma.cmd); li;
             li = list_next(li)) {
            Sequence subseq = parse_into_ir(
                (const act_chp_lang *)list_value(li), blockAllocator, id_pool);
            parallel->u_par().branches.push_back(subseq);
        }
        return blockAllocator.newSequence({parallel});
    }

    case ACT_CHP_SELECT: {
        Block *select = blockAllocator.newBlock(Block::makeSelectBlock());
        for (act_chp_gc_t *gc = c->u.gc; gc; gc = gc->next) {
            Sequence subseq = parse_into_ir(gc->s, blockAllocator, id_pool);
            select->u_select().branches.emplace_back(
                subseq, new_irguard_from_expr(id_pool, gc->g));
        }
        return blockAllocator.newSequence({select});
    }

    case ACT_CHP_LOOP: {
        // the IR only has do-loops
        // Therefore we translate `*[ A -> XA [] B -> XB ]` into
        // `do_loop_guard:=false; DO[ [A -> XA [] B -> XB [] else
        // -> do_loop_guard:=false ] ]WHILE[do_loop_guard] If the loop already
        // has an else branch we don't generate the final `exec:=false` branch
        Block *doloop = blockAllocator.newBlock(Block::makeDoLoopBlock());
        Block *select = blockAllocator.newBlock(Block::makeSelectBlock());

        VarId doloop_guard = id_pool.makeTmpVar(1);
        Block *assign = blockAllocator.newBlock(
            Block::makeBasicBlock(Statement::makeAssignment(
                doloop_guard, ChpExprSingleRootDag ::of_expr(
                                  ChpExpr::makeConstant(BigInt{1}, 1)))));

        doloop->u_doloop().branch =
            blockAllocator.newSequence({assign, select});
        doloop->u_doloop().guard = ChpExprSingleRootDag::of_expr(
            ChpExpr::makeVariableAccess(doloop_guard, 1));

        bool loop_already_has_else_branch = false;
        for (act_chp_gc_t *gc = c->u.gc; gc; gc = gc->next) {
            Sequence subseq = parse_into_ir(gc->s, blockAllocator, id_pool);
            IRGuard g = new_irguard_from_expr(id_pool, gc->g);
            if (g.type() == IRGuardType::Else)
                loop_already_has_else_branch = true;
            select->u_select().branches.emplace_back(subseq, std::move(g));
        }

        if (!loop_already_has_else_branch) {
            Block *doloop_guard_assign = blockAllocator.newBlock(
                Block::makeBasicBlock(Statement::makeAssignment(
                    doloop_guard, ChpExprSingleRootDag::of_expr(
                                      ChpExpr::makeConstant(BigInt{0}, 1)))));
            select->u_select().branches.emplace_back(
                blockAllocator.newSequence({doloop_guard_assign}),
                IRGuard::makeElse());
        }

        return blockAllocator.newSequence({doloop});
    }
    case ACT_CHP_DOLOOP: {
        hassert(c->u.gc);                  // check there is at least 1 child
        hassert(c->u.gc->next == nullptr); // check there is only 1 child
        act_chp_gc_t *gc = c->u.gc;
        Block *doloop = blockAllocator.newBlock(Block::makeDoLoopBlock());
        Sequence subseq = parse_into_ir(gc->s, blockAllocator, id_pool);
        doloop->u_doloop().branch = subseq;
        auto irguard = new_irguard_from_expr(id_pool, gc->g);
        hassert(irguard.type() ==
                IRGuardType::Expression); // We currently don't support anything
                                          // else on a DoLoop
        doloop->u_doloop().guard = std::move(irguard.u_e().e);
        return blockAllocator.newSequence({doloop});
    }
    case ACT_CHP_SKIP: {
        return blockAllocator.newSequence({});
    }
    case ACT_CHP_ASSIGN: {
        VarId var_id = *id_pool.varIdFromActId(c->u.assign.id);
        int width = id_pool.getBitwidth(var_id);
        auto expr = ChpExprSingleRootDag::of_expr(
            new_chpexpr_from_expr(id_pool, c->u.assign.e));
        if (width != expr.width()) {
            expr = ChpExprSingleRootDag::makeResize(
                std::make_unique<ChpExprSingleRootDag>(std::move(expr)), width);
        }
        Block *assign = blockAllocator.newBlock(Block::makeBasicBlock(
            Statement::makeAssignment(var_id, std::move(expr))));
        return blockAllocator.newSequence({assign});
    }
    case ACT_CHP_SEND: {
        hassert(!c->u.comm.flavor);
        hassert(!c->u.comm.var);
        ChanId chan_id = *id_pool.chanIdFromActId(c->u.comm.chan);
        int width = id_pool.getBitwidth(chan_id);
        auto expr_ptr = new_chpexpr_from_expr(id_pool, c->u.comm.e);
        if (!expr_ptr)
            expr_ptr =
                std::make_unique<ChpExpr>(ChpExpr::makeConstant(BigInt{0}, 1));
        auto expr = ChpExprSingleRootDag::of_expr(std::move(expr_ptr));
        if (width != expr.width()) {
            expr = ChpExprSingleRootDag::makeResize(
                std::make_unique<ChpExprSingleRootDag>(std::move(expr)), width);
        }

        if (expr.root()->type() == IRExprTypeKind::Var) {
            Block *send = blockAllocator.newBlock(Block::makeBasicBlock(
                Statement::makeSend(chan_id, std::move(expr))));
            return blockAllocator.newSequence({send});
        }
        // otherwise, assign into a temporary of the right width, and send the
        // temporary.
        VarId tmp_id = id_pool.makeTmpVar(width);
        Block *assign = blockAllocator.newBlock(Block::makeBasicBlock(
            Statement::makeAssignment(tmp_id, std::move(expr))));
        Block *send =
            blockAllocator.newBlock(Block::makeBasicBlock(Statement::makeSend(
                chan_id,
                ChpExprSingleRootDag::makeVariableAccess(tmp_id, width))));
        return blockAllocator.newSequence({assign, send});
    }
    case ACT_CHP_RECV: {
        hassert(!c->u.comm.e);
        hassert(!c->u.comm.flavor);
        ChanId chan_id = *id_pool.chanIdFromActId(c->u.comm.chan);
        int chan_width = id_pool.getBitwidth(chan_id);
        OptionalVarId optional_var_id = id_pool.varIdFromActId(c->u.comm.var);
        if (!optional_var_id) {
            Block *receive = blockAllocator.newBlock(Block::makeBasicBlock(
                Statement::makeReceive(chan_id, OptionalVarId::null_id())));
            return blockAllocator.newSequence({receive});
        }

        VarId var_id = *optional_var_id;
        int var_width = id_pool.getBitwidth(var_id);

        if (chan_width == var_width) {
            Block *receive = blockAllocator.newBlock(
                Block::makeBasicBlock(Statement::makeReceive(chan_id, var_id)));
            return blockAllocator.newSequence({receive});
        }
        // otherwise, receive into a temporary of the right width, and then
        // assign to the right value
        VarId tmp_id = id_pool.makeTmpVar(chan_width);
        Block *receive = blockAllocator.newBlock(
            Block::makeBasicBlock(Statement::makeReceive(chan_id, tmp_id)));
        Block *assign = blockAllocator.newBlock(
            Block::makeBasicBlock(Statement::makeAssignment(
                var_id, ChpExprSingleRootDag::makeResize(
                            std::make_unique<ChpExprSingleRootDag>(
                                ChpExprSingleRootDag::makeVariableAccess(
                                    tmp_id, chan_width)),
                            var_width))));
        return blockAllocator.newSequence({receive, assign});
    }
    case ACT_CHP_FUNC: {
      return blockAllocator.newSequence({});
      break;
    }
    default:
        fatal_error("CHP optimize: Unknown type");
        break;
    }
    hassert(false);
    return Sequence{};
}
} // namespace

std::pair<Sequence, NameParsingIdPool>
parse_ir_from_act(act_chp_lang *c, BlockAllocator &blockAllocator,
                  Scope *scope) {
    NameParsingIdPool id_pool{scope};
    std::unordered_map<ChanId, std::string> name_from_chan;
    auto graph = parse_into_ir(c, blockAllocator, id_pool);
    return {graph, id_pool};
}

GraphWithChanNames chp_graph_from_act(act_chp_lang *lang, Scope *s) {
    GraphWithChanNames graph_with_names;
    auto [seq, id_pool] =
        parse_ir_from_act(lang, graph_with_names.graph.blockAllocator(), s);

    graph_with_names.graph.m_seq = seq;
    graph_with_names.graph.id_pool() = std::move(id_pool.id_pool());
    graph_with_names.graph.validateGraphInvariants();

    graph_with_names.name_from_chan = id_pool.name_from_chan_map();
    return graph_with_names;
}


namespace {


act_chp_lang_t *seq_to_act (const Sequence &seq, var_to_actvar &map)
{
  act_chp_lang_t *ret;

  NEW (ret, act_chp_lang_t);
  ret->type = ACT_CHP_SEMI;
  ret->u.semi_comma.cmd = list_new ();
  ret->label = NULL;
  ret->space = NULL;

  auto varToId = [&] (const VarId &v) { return map.varMap (v); };
  auto chanToId = [&] (const ChanId &v) { return map.chanMap (v); };
  
  Block *curr = seq.startseq->child();
  if (curr && (curr->type() == BlockType::EndSequence))
  {
    act_chp_lang_t *skip;
    NEW (skip, act_chp_lang_t);
    skip->type = ACT_CHP_SKIP;
    skip->space = NULL;
    skip->label = NULL;
    list_append (ret->u.semi_comma.cmd, skip);
  }
  while (curr->type() != BlockType::EndSequence) {
    act_chp_lang_t *item;
    int idx;
    ActExprIntType t;
    act_chp_gc_t *gc;
    
    switch (curr->type()) {
    case BlockType::Basic:
      switch (curr->u_basic().stmt.type()) {
      case StatementType::Assign:
	idx = 0;
	for (auto &id : curr->u_basic().stmt.u_assign().ids) {
	  NEW (item, act_chp_lang_t);
	  item->label = NULL;
	  item->space = NULL;
	  item->type = ACT_CHP_ASSIGN;
	  item->u.assign.id = map.varMap (id);
	  if (map.isBool (id)) {
	    t = ActExprIntType::Bool;
	  }
	  else {
	    t = ActExprIntType::Int;
	  }
	  item->u.assign.e =
	    template_func_new_expr_from_irexpr
	    (*curr->u_basic().stmt.u_assign().e.roots[idx], t, varToId, chanToId);
	  list_append (ret->u.semi_comma.cmd, item);
	  idx++;
	}
	break;

      case StatementType::Send:
	NEW (item, act_chp_lang_t);
	item->label = NULL;
	item->space = NULL;
	item->type = ACT_CHP_SEND;
	item->u.comm.chan = map.chanMap (curr->u_basic().stmt.u_send().chan);
	if (map.isBool (curr->u_basic().stmt.u_send().chan)) {
	  t = ActExprIntType::Bool;
	}
	else {
	  t = ActExprIntType::Int;
	}
	item->u.comm.flavor = 0;
	item->u.comm.convert = 0;
	item->u.comm.e =
	  template_func_new_expr_from_irexpr
	  (*curr->u_basic().stmt.u_send().e.m_dag.roots[0], t, varToId, chanToId);
	list_append (ret->u.semi_comma.cmd, item);
	item->u.comm.var = NULL;
	break;

      case StatementType::Receive:
	NEW (item, act_chp_lang_t);
	item->label = NULL;
	item->space = NULL;
	item->type = ACT_CHP_RECV;
	item->u.comm.chan = map.chanMap (curr->u_basic().stmt.u_receive().chan);
	item->u.comm.flavor = 0;
	item->u.comm.convert = 0;
	item->u.comm.e = NULL;
	if (curr->u_basic().stmt.u_receive().var) {
	  item->u.comm.var = map.varMap (*curr->u_basic().stmt.u_receive().var);
	}
	else {
	  item->u.comm.var = NULL;
	}
	list_append (ret->u.semi_comma.cmd, item);
	break;
      }
      break;
      
    case BlockType::Par:
      NEW (item, act_chp_lang_t);
      item->label = NULL;
      item->space = NULL;
      item->type = ACT_CHP_COMMA;
      item->u.semi_comma.cmd = list_new ();
      list_append (ret->u.semi_comma.cmd, item);
      for (auto &x : curr->u_par().branches) {
	list_append (item->u.semi_comma.cmd, seq_to_act (x, map));
      }
      break;
	  
    case BlockType::Select:
      hassert(curr->u_select().splits.empty());
      hassert(curr->u_select().merges.empty());
      NEW (item, act_chp_lang_t);
      item->label = NULL;
      item->space = NULL;
      item->type = ACT_CHP_SELECT;
      item->u.gc = NULL;
      gc = NULL;
      list_append (ret->u.semi_comma.cmd, item);
      for (auto &branch : curr->u_select().branches) {
	if (gc) {
	  NEW (gc->next, act_chp_gc_t);
	  gc = gc->next;
	}
	else {
	  NEW (gc, act_chp_gc_t);
	  item->u.gc = gc;
	}
	gc->id = NULL;
	gc->lo = NULL;
	gc->hi = NULL;
	gc->next = NULL;
	switch (branch.g.type()) {
	case IRGuardType::Expression:
	  gc->g =
	    template_func_new_expr_from_irexpr (*branch.g.u_e().e.m_dag.roots[0],
						ActExprIntType::Bool,
						varToId, chanToId);
	  break;

	case IRGuardType::Else:
	  gc->g = NULL;
	  break;
	}
	gc->s = seq_to_act (branch.seq, map);
      }
      break;
      
    case BlockType::DoLoop:
      hassert(curr->u_doloop().in_phis.empty());
      hassert(curr->u_doloop().out_phis.empty());
      hassert(curr->u_doloop().loop_phis.empty());
      NEW (item, act_chp_lang_t);
      item->type = ACT_CHP_DOLOOP;
      item->label = NULL;
      item->space = NULL;
      NEW (item->u.gc, act_chp_gc_t);
      item->u.gc->next = NULL;
      item->u.gc->id = NULL;
      item->u.gc->lo = NULL;
      item->u.gc->hi = NULL;
      item->u.gc->g = 
	template_func_new_expr_from_irexpr (*curr->u_doloop().guard.m_dag.roots[0],
					    ActExprIntType::Bool,
					    varToId, chanToId);
      item->u.gc->s = seq_to_act (curr->u_doloop().branch, map);
      list_append (ret->u.semi_comma.cmd, item);
      break;

    case BlockType::StartSequence:
    case BlockType::EndSequence:
      hassert(false);
      break;
    }
    curr = curr->child();
  }
  return ret;
}


}

act_chp_lang *chp_graph_to_act(GraphWithChanNames &gr,
			       std::vector<ActId *> &newnames,
			       Scope *s) {
  var_to_actvar table(s, &gr.graph.id_pool());

  for (auto &[x, v] : gr.name_from_chan) {
    table.name_from_chan[x] = v;
  }
#if 0  
  for (auto &[x, v] : gr.name_from_var) {
    table.name_from_var[x] = v.
  }
#endif  

  act_chp_lang *l = seq_to_act (gr.graph.m_seq, table);

  newnames = std::move (table.newvars);
  
  return l;
}

static std::unordered_map<ChanId, ChanId> cc;
static std::unordered_map<VarId, VarId> vv;

ChanId get_chan_id (const ChanId &ci, ChpGraph &g_new, const ChpGraph &g_old)
{
  if (cc.contains(ci))
    return cc[ci];
  ChanId co = g_new.id_pool().makeUniqueChan(
                      g_old.id_pool().getBitwidth(ci), 
                      g_old.id_pool().getIsBool(ci));
  cc[ci] = co;
  return co;
}

VarId get_var_id (const VarId &vi, ChpGraph &g_new, const ChpGraph &g_old)
{
  if (vv.contains(vi))
    return vv[vi];
  VarId vo = g_new.id_pool().makeUniqueVar(
                      g_old.id_pool().getBitwidth(vi), 
                      g_old.id_pool().getIsBool(vi));
  vv[vi] = vo;
  return vo;
}

template<typename ExprType> 
ExprType deep_copy_expr (const ExprType &e, ChpGraph &g_new, const ChpGraph &g_old)
{
  auto e1 = ExprType::deep_copy(e);
  auto used_vars = getIdsUsedByExpr(e1);
  std::unordered_map<VarId, VarId> new_vars_map = {}; 
  for ( auto var : used_vars ) {
    new_vars_map.insert({var,get_var_id(var, g_new, g_old)});
  }
  ExprType::remapVars(e1, new_vars_map);
  return e1;
}

Sequence deep_copy_seq (const Sequence &seq, ChpGraph &g_new, const ChpGraph &g_old)
{
  std::vector<Block *> blks = {};
  Block *curr = seq.startseq->child();

  auto remap_chan = [&](const ChanId &id) -> ChanId {
    return get_chan_id(id, g_new, g_old);
  };

  auto remap_var = [&](const VarId &id) -> VarId {
    return get_var_id(id, g_new, g_old);
  };

  auto remap_var_opt = [&](const OptionalVarId &id) -> OptionalVarId {
    return (id) ? get_var_id(*(id), g_new, g_old) : OptionalVarId();
  };

  auto remap_vec = [&](const std::vector<VarId> &ids) -> std::vector<VarId> {
    std::vector<VarId> ret = {};
    for ( const auto &id : ids ) {
      ret.push_back(remap_var(id));
    }
    return ret;
  };

  auto remap_vec_opt = [&](const std::vector<OptionalVarId> &ids) -> std::vector<OptionalVarId> {
    std::vector<OptionalVarId> ret = {};
    for ( const auto &id : ids ) {
      ret.push_back(remap_var_opt(id));
    }
    return ret;
  };

  while (curr->type() != BlockType::EndSequence) {
    switch (curr->type()) {
    case BlockType::Basic: {
        switch (curr->u_basic().stmt.type()) {
        case StatementType::Receive: {
          const auto &recv = curr->u_basic().stmt;
          ChanId ci = remap_chan(recv.u_receive().chan);
          OptionalVarId vi = remap_var_opt(recv.u_receive().var);
          auto _b = g_new.newBasicBlock(Statement::makeReceive(ci,vi));
          blks.push_back(_b);
        }
        break;
        case StatementType::Send: {
          const auto &send = curr->u_basic().stmt;
          ChanId ci = remap_chan(send.u_send().chan);
          auto _b = g_new.newBasicBlock(Statement::makeSend(
                    ci,deep_copy_expr<ChpExprSingleRootDag>(send.u_send().e, g_new, g_old)));
          blks.push_back(_b);
        }
        break;
        case StatementType::Assign: {
          const auto &assns = curr->u_basic().stmt;
          std::vector<VarId> new_assn_vars = {};
          for (const auto &var : assns.u_assign().ids ) {
            new_assn_vars.push_back(remap_var(var));
          }
          auto _b = g_new.newBasicBlock(Statement::makeAssignment(
                    new_assn_vars,deep_copy_expr<ChpExprDag>(assns.u_assign().e, g_new, g_old)));
          blks.push_back(_b);
        }
        break;
        }
    }
    break;
    case BlockType::Par: {
        auto _b = g_new.newParBlock();
        for (const auto &branch : curr->u_par().branches) {
          _b->u_par().branches.push_back(deep_copy_seq(branch, g_new, g_old));
        }
        for (const auto &split : curr->u_par().splits) {
          Block::Variant_Par::PhiSplit newsplit;
          newsplit.pre_id = remap_var(split.pre_id);
          newsplit.branch_ids = remap_vec_opt(split.branch_ids);
          _b->u_par().splits.push_back(newsplit);
        }
        for (const auto &merge : curr->u_par().merges) {
          Block::Variant_Par::PhiMerge newmerge;
          newmerge.branch_ids = remap_vec_opt(merge.branch_ids);
          newmerge.post_id = remap_var(merge.post_id);
          _b->u_par().merges.push_back(newmerge);
        }
        blks.push_back(_b);
    }
    break;
    case BlockType::Select: {
        auto _b = g_new.newSelectBlock();
        for (const auto &branch : curr->u_select().branches) {
          _b->u_select().branches.push_back({deep_copy_seq(branch.seq, g_new, g_old),
                    (branch.g.type()==IRGuardType::Else) ? IRGuard::makeElse() :
                    IRGuard::makeExpression(deep_copy_expr<ChpExprSingleRootDag>(branch.g.u_e().e, g_new, g_old))});
        }
        for (const auto &split : curr->u_select().splits) {
          Block::Variant_Select::PhiSplit newsplit;
          newsplit.pre_id = remap_var(split.pre_id);
          newsplit.branch_ids = remap_vec_opt(split.branch_ids);
          _b->u_select().splits.push_back(newsplit);
        }
        for (const auto &merge : curr->u_select().merges) {
          Block::Variant_Select::PhiMerge newmerge;
          newmerge.branch_ids = remap_vec(merge.branch_ids);
          newmerge.post_id = remap_var(merge.post_id);
          _b->u_select().merges.push_back(newmerge);
        }
        blks.push_back(_b);
    }
    break;
    case BlockType::DoLoop: {
      auto _b = g_new.newDoLoopBlock();
      _b->u_doloop().branch = deep_copy_seq(curr->u_doloop().branch, g_new, g_old);
      _b->u_doloop().guard = deep_copy_expr<ChpExprSingleRootDag>(curr->u_doloop().guard, g_new, g_old);
      for (const auto &iphi : curr->u_doloop().in_phis) {
        Block::Variant_DoLoop::InPhi newiphi;
        newiphi.bodyin_id = remap_var(iphi.bodyin_id);
        newiphi.pre_id = remap_var(iphi.pre_id);
        _b->u_doloop().in_phis.push_back(newiphi);
      }
      for (const auto &ophi : curr->u_doloop().out_phis) {
        Block::Variant_DoLoop::OutPhi newophi;
        newophi.bodyout_id = remap_var(ophi.bodyout_id);
        newophi.post_id = remap_var(ophi.post_id);
        _b->u_doloop().out_phis.push_back(newophi);
      }
      for (const auto &lphi : curr->u_doloop().loop_phis) {
        Block::Variant_DoLoop::LoopPhi newlphi;
        newlphi.bodyin_id = remap_var(lphi.bodyin_id);
        newlphi.bodyout_id = remap_var(lphi.bodyout_id);
        newlphi.pre_id = remap_var(lphi.pre_id);
        newlphi.post_id = remap_var_opt(lphi.post_id);
        _b->u_doloop().loop_phis.push_back(newlphi);
      }
      blks.push_back(_b);
    }
    break;
    case BlockType::StartSequence:
    case BlockType::EndSequence:
        hassert(false);
        break;
    }
    curr = curr->child();
  }
  return g_new.newSequence(blks);
}

GraphWithChanNames deep_copy_graph (
      const GraphWithChanNames &g, 
std::unordered_map<ChanId, ChanId> &cc_in,
std::unordered_map<VarId, VarId> &vv_in
)
{
  GraphWithChanNames ret;
  cc.clear();
  vv.clear();
  ret.graph.m_seq = deep_copy_seq (g.graph.m_seq, ret.graph, g.graph);
  for ( const auto &x : g.name_from_chan ) {
    ret.name_from_chan.insert({get_chan_id(x.first, ret.graph, g.graph),x.second});
  }
  ret.graph.is_static_token_form = g.graph.is_static_token_form;
  cc_in = std::move(cc);
  vv_in = std::move(vv);
  return ret;
}

} // namespace ChpOptimize
