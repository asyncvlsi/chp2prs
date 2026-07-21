/*************************************************************************
 *
 *  Copyright (c) 2025 Karthi Srinivasan
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

#include "expr_pipe.h"
#include <filesystem>
#include <cmath>
namespace fs = std::filesystem;

void ExprPipe::run()
{
    // std::cout << "\n---- initial ----\n";
    // print_chp(std::cout, g->graph);
    // std::cout << "\n---- initial ----\n";
    run_seq(g->graph.m_seq);
    // std::cout << "\n---- final ----\n";
    // print_chp(std::cout, g->graph);
    // std::cout << "\n---- final ----\n";
    g->graph.validateGraphInvariants();
}

void ExprPipe::run_seq(Sequence &seq) 
{
    var_to_actvar table(s, g->graph.id_pool());
    _run_seq(seq, table);
}

void ExprPipe::set_n_cuts(int n) 
{
  Assert (n>=0, "what");
  n_cuts = n;
}

void ExprPipe::set_delay_threshold(double x) 
{
  Assert (x>=0.0, "what");
  delay_threshold = x;
}

void ExprPipe::_run_seq(Sequence &seq, var_to_actvar &table)
{
    Block *curr = seq.startseq->child();

    while (curr->type() != BlockType::EndSequence) {
    switch (curr->type()) {
    case BlockType::Basic: {
        switch (curr->u_basic().stmt.type()) {
        case StatementType::Receive:
        break;
        case StatementType::Send: {
          auto used = getIdsUsedByExpr(curr->u_basic().stmt.u_send().e);
          auto width = g->graph.id_pool().getBitwidth(curr->u_basic().stmt.u_send().chan);
          if (!used.empty() && width>0) {
            _run_expr(curr, table, width);
          }
        }
        break;
        case StatementType::Assign: {
          auto ids = curr->u_basic().stmt.u_assign().ids;
          Assert (ids.size()==1, "assignments unsplit");
          auto used = getIdsUsedByExpr(curr->u_basic().stmt.u_assign().e);
          auto width = g->graph.id_pool().getBitwidth(curr->u_basic().stmt.u_assign().ids[0]);
          if (!used.empty() && width>0) {
            _run_expr(curr, table, width);
          }
        }
        break;
        }
    }
    break;
    case BlockType::Par: {
        for (auto &branch : curr->u_par().branches) {
            _run_seq (branch, table);
        }
    }
    break;
    case BlockType::Select: {
        for ( auto &branch : curr->u_select().branches ) {
            _run_seq (branch.seq, table);
        }
    }
    break;
    case BlockType::DoLoop: {
        _run_seq (curr->u_doloop().branch, table);
    }
    break;
    case BlockType::StartSequence:
    case BlockType::EndSequence:
        hassert(false);
        break;
    }
    curr = curr->child();
    }
}

void ExprPipe::print_cexpr (const ChpExpr &e, VarId v) {
    auto b = g->graph.blockAllocator().newBlock(Block::makeBasicBlock(
            Statement::makeAssignment(v, 
            ChpExprSingleRootDag::of_expr(ChpExpr::deep_copy(e)))));
    std::cout << "-----------" << std::endl;
    print_chp_block(std::cout, b);
    std::cout << std::endl << "-----------" << std::endl;
}

void ExprPipe::reset_state() 
{
  nm.clear();
  stmts.clear();
  in_out_map.clear();
  rhss.clear();
  lhss.clear();
  _m_expr_id = 0;
}

void ExprPipe::_run_expr (Block *b, var_to_actvar &table, int width)
{
  reset_state();
  Assert (b->type()==BlockType::Basic, "what");
  Assert (!(b->u_basic().stmt.type()==StatementType::Receive), "what");
  bool send = (b->u_basic().stmt.type()==StatementType::Send);
  int was_retimed;
  if (send) {
    was_retimed = _run_expr_helper (b->u_basic().stmt.u_send().e, table, width);
  }
  else {
    auto expr = ChpExprDag::deep_copy(b->u_basic().stmt.u_assign().e);
    auto e2 = ChpExpr::deep_copy(ChpExprDag::to_expr(*expr.roots[0]));
    auto ce = ChpExprSingleRootDag::of_expr(e2);
    was_retimed = _run_expr_helper (ce, table, width);
  }
  if (!was_retimed) {
    return;
  }

  Assert ((rhss.size()-1==lhss.size()), "subexprs and output widths mismatch");
  std::vector<Block *> vb;
  // gotta process in reverse
  std::reverse(rhss.begin(),rhss.end());
  std::reverse(lhss.begin(),lhss.end());
  
  for (int i=0; i<lhss.size(); i++) {
    auto bb = g->graph.blockAllocator().newBlock(
      Block::makeBasicBlock(Statement::makeAssignment(lhss.at(i), 
      ChpExprSingleRootDag::of_expr(rhss.at(i))))
    );
    vb.push_back(bb);
  }
  auto seq = g->graph.newSequence(vb);
  g->graph.spliceInSequenceBefore(b, seq);
  if (send) {
    b->u_basic().stmt.u_send().e = ChpExprSingleRootDag::of_expr(rhss.back());
  }
  else {
    b->u_basic().stmt.u_assign().e = 
    std::move(ChpExprSingleRootDag::of_expr(rhss.back()).m_dag);
  }
}

/*
  Return val: whether retiming was performed
*/
int ExprPipe::_run_expr_helper (ChpExprSingleRootDag &e, var_to_actvar &table, int width)
{
  std::string eqn_file = "out.eqn";
  constexpr int verbose = 0;

  auto varToId = [&] (const VarId &v) { return table.varMap (v); };
  auto chanToId = [&] (const ChanId &v) { return table.chanMap (v); };

  Expr *ae = ChpOptimize::template_func_new_expr_from_irexpr(*(e.m_dag.roots[0]), 
              ActExprIntType::Int, varToId, chanToId, table);

  ChpCost ct(s, *g);
  auto e_del = ct.expr_delay(ae, width);
  if(e_del < delay_threshold) {
    return 0; // skipped this expr coz small already
  }
  set_n_cuts(std::ceil(e_del/delay_threshold)+1);

  Bimap<ActId*, int> actid_to_in_idx;
  if (verbose>0) { fprintf(stderr,"\n RT: synthesizing expr \n"); }
  auto mapped_verilog = _expr_to_verilog (ae, width, actid_to_in_idx);
  if (verbose>0) { 
    fprintf(stderr,"\n------ done ------\n"); 
    fprintf(stderr,"\n RT: generating eqn file \n"); 
  }
  _verilog_to_eqn (mapped_verilog, eqn_file);
  cleanup_tmp_files();
  if (verbose>0) { 
    fprintf(stderr,"\n------ done ------\n");
    fprintf(stderr,"\n RT: parsing eqn file \n"); 
  }
  EqnParser p(eqn_file, g->graph.id_pool());
  p.parseFile();
  if (verbose==0) {
    fs::remove(eqn_file);
  }
  if (verbose>0) { 
    fprintf(stderr,"\n------ done ------\n");
    fprintf(stderr,"\n RT: constructing CHP exprs \n"); 
  }

  nm = p.get_name_map();
  if (verbose>1) {
    fprintf(stderr, "\n--- name map --- ");
    for ( auto &[x,y] : nm ) { fprintf(stderr, "\nv%llu : %s", y.m_id, x.c_str()); }
    fprintf(stderr, "\n--- name map --- \n");
  }

  stmts = p.get_stmts();
  if (verbose>2) {
    fprintf(stderr, "\n--- parsed exprs ---");
    for ( const auto &assn : stmts ) { print_cexpr(assn.second, assn.first); }
    fprintf(stderr, "\n--- parsed exprs --- \n");
  }

  std::vector<VarId> outs{};
  for ( int i=0; i<width; i++ ) {
    std::string out_i = "out["+std::to_string(i)+"]";
    if (!nm.count(out_i)) { fprintf(stderr, "\nout_i : %s\n", out_i.c_str()); }
    Assert (nm.count(out_i), "couldn't find output bool?!");
    outs.push_back(nm.at(out_i));
  }
  
  _build_in_out_map();
  if (verbose>1) {
    fprintf(stderr, "\n--- in-out map ---");
    for ( auto [vi,vo] : in_out_map ) { fprintf(stderr, "\n%llu : %llu", vi.m_id, vo.m_id); }
    fprintf(stderr, "\n--- in-out map --- \n");
  }

  Assert (n_cuts>0, "what");
  for (int i=0; i<=n_cuts; i++) {

    if (verbose>1) {
      fprintf(stderr, "\n\n------ iter : %d ------\n", i);
      for ( auto v : outs ) { fprintf(stderr, "out: v%llu\n", v.m_id); }
    }
    // coz primary inputs are special - they don't have an image
    bool last_iter = (i==n_cuts); 
    _construct_int_expr(outs);

    if (last_iter) {
      auto vvpos = _build_primary_input_map(actid_to_in_idx, table);
      _apply_bitmap_primary_input(rhss.back(), vvpos);
    }
    else {
      auto used = _get_used(outs);

      Bimap<VarId, int> vpos;
      for ( int j=0; j<used.size(); j++) { 
        vpos.insert(used.at(j),j);
      }
    
      auto vconcat = g->graph.id_pool().makeUniqueVar(used.size());
      lhss.push_back(vconcat);
      _apply_bitmap(rhss.back(), vpos, vconcat);
      
      std::vector<VarId> next_outs_ordered;
      for ( int j=0; j<used.size(); j++) { 
        next_outs_ordered.push_back(_get_io_image({vpos.at(j)})[0]);
      }
      outs = next_outs_ordered;
    }
  }
  if (verbose>0) { fprintf(stderr, "\n------ done ------\n"); }
  return 1;
}

void ExprPipe::_apply_bitmap (ChpExpr &e, const Bimap<VarId,int> &vpos, VarId v)
{
  switch (e.type()) {
  case IRExprTypeKind::BinaryOp: {
    _apply_bitmap (*(e.u_e2().l), vpos, v);
    _apply_bitmap (*(e.u_e2().r), vpos, v);
  }
  break;
  case IRExprTypeKind::UnaryOp: {
    _apply_bitmap (*(e.u_e1().l), vpos, v);
  }
  break;
  case IRExprTypeKind::Var: {
    auto v1 = e.u_var().id;
    Assert (vpos.count(v1), "what");
    e = ChpExpr::makeBitfield(std::make_unique<ChpExpr>(
      ChpExpr::makeVariableAccess(v, g->graph.id_pool())),
      vpos.at(v1),vpos.at(v1));
  }
  break;
  case IRExprTypeKind::Const: {
  }
  break;
  default:
  Assert (false, "unexpected type");
  break;
  }
}

std::unordered_map<VarId,std::pair<VarId, int>> 
ExprPipe::_build_primary_input_map (Bimap<ActId *, int> &m, var_to_actvar &table)
{
  auto get_bit = [](std::string name) {
    std::smatch m;
    if (std::regex_search(name, m, std::regex("\\[(\\d+)\\]"))) {
      return std::stoi(m[1].str());
    }
    return -1;
  };
  auto name_prefix = [](const std::string& s) {
    size_t pos = s.find('[');
    return s.substr(0, pos == std::string::npos ? s.size() : pos);
  };

  auto get_in_idx = [](const std::string& in_i) {
    auto i = in_i.substr(3,in_i.size()-3);
    return std::stoi(i);
  };

  auto get_width = [&](std::string pfx) {
    int w=0;
    for ( auto [name, vi] : nm ) {
      if (name_prefix(name)==pfx) {
        w = std::max(w, get_bit(name));
      }
    }
    return w+1;
  };

  std::unordered_map<act_connection *, VarId> var_from_name;
  for ( const auto &[vid, aid] : table.name_from_var ) { var_from_name.insert({aid->Canonical(s),vid}); }

  auto get_varconc = [&](std::string pfx) {
    auto i = get_in_idx(pfx); 
    ActId *id = m.at(i);
    OptionalVarId ret = var_from_name.at(id->Canonical(s));
    Assert ((ret), "not found");
    return *ret;
  };

  std::unordered_map<VarId,std::pair<VarId, int>> ret;
  for ( auto [name, vi] : nm ) {
    if (name.size() >= 3 && name.compare(0, 3, "in_") == 0) {
      auto bit = get_bit(name);
      auto pfx = name_prefix(name);
      ret.insert({vi, {get_varconc(pfx),bit}});
    }
  }
  return ret;
}

/*
  vpos : var-in-expr -> {input-int-var-bw-greater-than-one, bitpos}
  essentially to replace the one-bit vars in the first expr with 
  bits of int vars that are the primary inputs
*/
void ExprPipe::_apply_bitmap_primary_input (ChpExpr &e, std::unordered_map<VarId,std::pair<VarId, int>> vpos)
{
  switch (e.type()) {
  case IRExprTypeKind::BinaryOp: {
    _apply_bitmap_primary_input (*(e.u_e2().l), vpos);
    _apply_bitmap_primary_input (*(e.u_e2().r), vpos);
  }
  break;
  case IRExprTypeKind::UnaryOp: {
    _apply_bitmap_primary_input (*(e.u_e1().l), vpos);
  }
  break;
  case IRExprTypeKind::Var: {
    auto v1 = e.u_var().id;
    Assert (vpos.count(v1), "what");
    e = ChpExpr::makeBitfield(std::make_unique<ChpExpr>(
      ChpExpr::makeVariableAccess(vpos.at(v1).first, g->graph.id_pool())),
        vpos.at(v1).second,vpos.at(v1).second);
  }
  break;
  case IRExprTypeKind::Const: {
  }
  break;
  default:
  Assert (false, "unexpected type");
  break;
  }
}

// get next level of output nodes
std::vector<VarId> ExprPipe::_get_used (std::vector<VarId> vs)
{
  std::unordered_set<VarId> used = {};
  auto union_ = [](std::unordered_set<VarId> s1, std::unordered_set<VarId> s2) {
    for ( auto x1 : s1 ) { s2.insert(x1); }
    return s2;
  };
  for ( auto v : vs ) {
    used = union_(used, getIdsUsedByExpr(&stmts.at(v)));
  }
  return std::vector(used.begin(), used.end());
}

std::vector<VarId> ExprPipe::_get_io_image (std::vector<VarId> used) 
{
  std::vector<VarId> ret = {};
  for ( auto v : used ) {
    Assert (in_out_map.count(v), "io-image var not found");
    ret.push_back(in_out_map.at(v));
  }
  return ret;
}

/*
  assumes input vs is sorted correctly - LSB first.
  construct an expr that is essentially the concatenation
  of the bool exprs for each bit of an int.
*/
void ExprPipe::_construct_int_expr (std::vector<VarId> vs)
{
  std::unordered_map<VarId, int> bitmap;

  ChpExpr conc_vars;
  for ( auto v : vs )
  {
    if (v == *vs.begin()) {
      conc_vars = ChpExpr::deep_copy(stmts.at(v));
    }
    else {
      auto v1 = ChpExpr::makeVariableAccess(v, 1);
      conc_vars = ChpExpr::makeBinaryOp(IRBinaryOpType::Concat,
                  std::make_unique<ChpExpr>(ChpExpr::deep_copy(stmts.at(v))),
                  std::make_unique<ChpExpr>(std::move(conc_vars))
                );
    }
  }
  rhss.push_back(std::move(conc_vars));
}

/*
  ABC Assumption:
  ABC names the latch input-output bool pairs as one of these:
  1. `xyz_in` , `xyz_out`
  2. `xyz_li` , `xyz_lo`
  Note that `xyz_in` and `xyz_li` are actually outputs from 
  the perspective of the combinational logic as they are 
  inputs of latches, and vice versa for `xyz_out` and `xyz_lo`.
  Example:
  =(in_0)=> (CL1) =(xyz_in)=> (LATCH) =(xyz_out)=> (CL2) =(out)=>
*/
void ExprPipe::_build_in_out_map ()
{
  for ( auto [name,vi] : nm ) {
    if (name.size() >= 4 && 
      name.compare(name.size() - 4, 4, "_out") == 0) {
      auto pfx = name.substr(0, name.size() - 4);
      pfx += "_in";
      Assert (nm.count(pfx), "no matching output for intermediate input");
      auto vo = nm.at(pfx);
      in_out_map.insert({vi,vo});
    }
    if (name.size() >= 3 && 
      name.compare(name.size() - 3, 3, "_lo") == 0) {
      auto pfx = name.substr(0, name.size() - 3);
      pfx += "_li";
      Assert (nm.count(pfx), "no matching output for intermediate input");
      auto vo = nm.at(pfx);
      in_out_map.insert({vi,vo});
    }
  }
}

/*
  ABC Assumption:
  `pipe -L n` places n stages of latches
  TODO replace this with ABC api call
*/
void ExprPipe::_verilog_to_eqn (std::string mapped_verilog, std::string eqn_file)
{
    // trim file coz abc is weird
    std::string trimmed_vlog = "trimmed.v";
    {
      std::ifstream in(mapped_verilog);
      std::ofstream out(trimmed_vlog);
      if (!in || !out) { fatal_error("File open failed\n"); }
      std::string line;
      bool done = false;
      while (std::getline(in, line)) {
        out << line << "\n";
        if (line.find("endmodule") != std::string::npos) {
          done = true; break;
        }
      }
      if (!done) { warning("no 'endmodule' found in file\n"); }
    }

    std::string cmd = "read_lib " + std::string(config_get_string("synth.liberty.typical"));
    cmd += ("; read_verilog -m "+trimmed_vlog+"; pipe -L "+std::to_string(n_cuts)+"; \
        balance; rewrite; refactor; balance; rewrite -z; balance; \
        rewrite -z; balance; strash; ifraig; scorr; dc2; \
        retime -o; strash; write_eqn "+eqn_file);

    auto abc_run = "abc -q \"" + cmd + "\" > /dev/null";
    // auto abc_run = "abc -c \"" + cmd + "\" ";
    system(abc_run.c_str());
    fs::remove(trimmed_vlog);
}   

std::string ExprPipe::_expr_to_verilog (Expr *e, int width, Bimap<ActId *, int> &m)
{
    std::string name = "test";

    _inexprmap = ihash_new (4);
    _inwidthmap = ihash_new (4);

    e = expr_dag(e);

    // also does a primitive dag-ing in thread mode
    _expr_collect_vars (e, m);

    // collect input vars in list
    list_t *all_leaves = list_new();
    {
        ihash_iter_t iter;
        ihash_bucket_t *ib;
        ihash_iter_init (_inexprmap, &iter);
        while ((ib = ihash_iter_next (_inexprmap, &iter))) {
        Expr *e1 = (Expr *)ib->key;
        list_append (all_leaves, e1);
        }
    }

    config_set_int("synth.expropt.verbose", 0);
    // run abc, then v2act to create the combinational-logic-for-math process
    auto ebi = run_external_opt(name, width, e, all_leaves, _inexprmap, _inwidthmap, false);
    auto ret = ebi->getMappedFile();
    ebi->~ExprBlockInfo();
    ebi = NULL;

    // free all temporary data structures 
    ihash_free (_inexprmap);
    _inexprmap = NULL;
    ihash_free (_inwidthmap);
    _inwidthmap = NULL;
    list_free (all_leaves);
    return ret;
}

void ExprPipe::_expr_collect_vars (Expr *&e, Bimap<ActId *, int> &m)
{
  Assert (e, "Hmm");

#define BINARY_OP					\
  do {							\
    _expr_collect_vars (e->u.e.l, m);	\
    _expr_collect_vars (e->u.e.r, m);	\
  } while (0)

#define UNARY_OP					\
  do {							\
    _expr_collect_vars (e->u.e.l, m);	\
  } while (0)
  
  switch (e->type) {
    /* binary */
  case E_AND:
  case E_OR:
  case E_XOR:
  case E_PLUS:
  case E_MINUS:
  case E_LT:
  case E_GT:
  case E_LE:
  case E_GE:
  case E_EQ:
  case E_NE:
  case E_MULT:
  case E_DIV:
  case E_MOD:
  case E_LSL:
  case E_LSR:
  case E_ASR:
    BINARY_OP;
    break;
    
  case E_UMINUS:
  case E_NOT:
  case E_COMPLEMENT:
  case E_BUILTIN_INT:
  case E_BUILTIN_BOOL:
    UNARY_OP;
    break;
    
  case E_BITFIELD:
    UNARY_OP;

  case E_QUERY:
    _expr_collect_vars (e->u.e.l, m);
    _expr_collect_vars (e->u.e.r->u.e.l, m);
    _expr_collect_vars (e->u.e.r->u.e.r, m);
    break;

  case E_COLON:
  case E_COMMA:
    fatal_error ("Should have been handled elsewhere");
    break;

    /* XXX: here */
  case E_CONCAT:
    {
      Expr *tmp = e;
      while (tmp) {
	_expr_collect_vars (tmp->u.e.l, m);
	tmp = tmp->u.e.r;
      }
    }
    break;

  case E_REAL:
    fatal_error ("No real expressions please.");
    break;

  case E_TRUE:
    break;
    
  case E_FALSE:
    break;
    
  case E_INT:
    break;

  case E_VAR: {
    ActId *var = (ActId *)e->u.e.l;
    int bw = bitwidth(var);
    if (bw==0) {
      e = const_expr(0);
      break;
    }
    ihash_bucket_t *ib;
    ihash_bucket_t *b_width;
    if (!ihash_lookup (_inexprmap, (long)e)) 
    {
        ib = ihash_add (_inexprmap, (long)e);
        ib->i = _gen_expr_id();
        m.insert(var, ib->i);
        b_width = ihash_add (_inwidthmap, (long) e);
        b_width->i = bw;
    }

    }
    break;

  case E_PROBE: {
      Assert (false, "No probes in decomp lmao");
    }
    break;
    
  case E_FUNCTION:
    fatal_error ("function!");
  case E_SELF:
  default:
    fatal_error ("Unknown expression type %d\n", e->type);
    break;
  }
  return;
#undef BINARY_OP
#undef UNARY_OP
}

int ExprPipe::_gen_expr_id()
{
    return _m_expr_id++;
}

int ExprPipe::bitwidth (ActId *id)
{
  if (!id) {
    return -1;
  }
  Assert (s, "Shouldn't have happened");
  InstType *it = s->FullLookup (id, NULL);
  if (!it) {
    return -1;
  }
  return TypeFactory::bitWidth (it);
}
