/*************************************************************************
 *
 *  Copyright (c) 2024 Karthi Srinivasan
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

#include "chopping_block.h"

#define LIVE_IN 0
#define LIVE_OUT 1

#define END_INC 0
#define END_EXC 1


void ChoppingBlock::_print_seq (Sequence seq)
{
    fprintf (stdout, "\n");
    Block *curr = seq.startseq->child(); 
    while (curr->type() != BlockType::EndSequence) {
    switch (curr->type()) {
    case BlockType::Basic: {
        switch (curr->u_basic().stmt.type()) {
        case StatementType::Send:
        fprintf (stdout, "\nsend\n");
            break;
        case StatementType::Assign:
        fprintf (stdout, "\nassign\n");
            break;
        case StatementType::Receive:
        fprintf (stdout, "\nrecv\n");
            break;
        }
    }
    break;
      
    case BlockType::Par: {
        fprintf (stdout, "\npar start\n");
        for (auto &branch : curr->u_par().branches) {
            _print_seq (branch);
        }
        fprintf (stdout, "\npar end\n");
    }
    break;
      
    case BlockType::Select:
        fprintf (stdout, "\nsel start\n");
        for (auto &branch : curr->u_select().branches) {
            _print_seq (branch.seq);
        }
        fprintf (stdout, "\nsel end\n");
    break;
      
    case BlockType::DoLoop:
        fprintf (stdout, "\ndoloop start\n");
        _print_seq (curr->u_doloop().branch);
        // fprintf (stdout, "%d", curr->u_doloop().guard.root().);
        print_chp (std::cout, g->graph);
        fprintf (stdout, "\ndoloop end\n");
        break;
    
    case BlockType::StartSequence:
    case BlockType::EndSequence:
        hassert(false); 
        break;
    }
    curr = curr->child();
    }
}

void ChoppingBlock::print_chopped_seqs()
{
    for (auto seq : v_seqs)
    {
        fprintf(stdout, "\nseq start ------\n");
        _print_seq (seq);
        fprintf(stdout, "\nseq end ------\n");
    }
}

std::vector<Sequence> ChoppingBlock::get_chopped_seqs()
{
    return v_seqs;
}

Sequence ChoppingBlock::_wrap_in_do_loop (Sequence seq)
{
    Block *doloop = g->graph.blockAllocator().newBlock(Block::makeDoLoopBlock());
    doloop->u_doloop().branch = seq;

    doloop->u_doloop().guard = (ChpExprSingleRootDag::of_expr(
                                    ChpExpr::makeConstant(BigInt{1}, 1)));
    
    return g->graph.blockAllocator().newSequence({doloop});
}

Block *ChoppingBlock::_splice_out_block(Block *bb) 
{
    Block *before = bb->parent();
    Block *after = bb->child();

    Block::disconnect(before, bb);
    Block::disconnect(bb, after);
    Block::connect(before, after);

    return after;
}

Block *ChoppingBlock::_splice_in_block_between (Block *before, Block *after, Block *bb)
{
    Block::disconnect(before, after);
    Block::connect(before, bb);
    Block::connect(bb, after);
    return after;
}

Block *ChoppingBlock::_generate_send_to_be_recvd_by(Block *bb)
{
    if (bb->type() == BlockType::StartSequence)
        return NULL;
    if (bb->type() == BlockType::EndSequence)
        return NULL;

    hassert (vmap.count(bb));
    decomp_info_t di = (vmap.find(bb))->second;
    di.live_in_vec = std::vector<VarId> {di.live_in_vars.begin(), di.live_in_vars.end()};

    if (di.total_bitwidth_in == 0)
    {
        return NULL;
    }

    ChanId chan_id = g->graph.id_pool().makeUniqueChan(di.total_bitwidth_in, false);
    var_to_actvar vtoa(s, g->graph.id_pool());
    ActId *id = vtoa.chanMap(chan_id);
    g->name_from_chan.insert({chan_id, id});

    if (di.live_in_vars.size() == 1)
    {
        VarId var_id = *di.live_in_vars.begin();
        Block *send =
            g->graph.blockAllocator().newBlock(Block::makeBasicBlock(Statement::makeSend(
                chan_id, ChpExprSingleRootDag::makeVariableAccess(var_id, di.total_bitwidth_in))));
        

        decomp_info_t di_new = _deepcopy_decomp_info(di);
        di_new.break_after = false;
        di_new.break_before = false;
        vmap.insert({send, di_new});
        
        return send;
    }

    ChpExprSingleRootDag conc_vars;

    for ( auto var : di.live_in_vars )
    {
        if (var == *di.live_in_vars.begin())
        {
            conc_vars = ChpExprSingleRootDag::makeVariableAccess(var, g->graph.id_pool().getBitwidth(var));
        }
        else
        {
            auto v1 = ChpExprSingleRootDag::makeVariableAccess(var, g->graph.id_pool().getBitwidth(var));
            conc_vars = ChpExprSingleRootDag::makeBinaryOp(IRBinaryOpType::Concat,
                        std::make_unique<ChpExprSingleRootDag>(std::move(conc_vars)),
                        std::make_unique<ChpExprSingleRootDag>(std::move(v1)));
        }
    }

    Block *send =
            g->graph.blockAllocator().newBlock(Block::makeBasicBlock(Statement::makeSend(
                chan_id, std::move(conc_vars))));

    decomp_info_t di_new = _deepcopy_decomp_info(di);
    di_new.break_after = false;
    di_new.break_before = false;
    vmap.insert({send, di_new});
    return send;
}

Block *ChoppingBlock::_generate_send_to_be_sent_from(Block *bb)
{
    if (bb->type() == BlockType::StartSequence) {
        fatal_error ("shouldn't have been used like this");
        return NULL;
    }
    if (bb->type() == BlockType::EndSequence) {
        fprintf (stdout, "\ntype end\n");
        return NULL;
    }

    hassert (vmap.count(bb));
    decomp_info_t di = (vmap.find(bb))->second;
    di.live_out_vec = std::vector<VarId> {di.live_out_vars.begin(), di.live_out_vars.end()};

    if (di.total_bitwidth_out == 0)
    {
        return NULL;
    }

    ChanId chan_id = g->graph.id_pool().makeUniqueChan(di.total_bitwidth_out, false);
    var_to_actvar vtoa(s, g->graph.id_pool());
    ActId *id = vtoa.chanMap(chan_id);
    g->name_from_chan.insert({chan_id, id});

    if (di.live_out_vars.size() == 1)
    {
        VarId var_id = *di.live_out_vars.begin();
        Block *send =
            g->graph.blockAllocator().newBlock(Block::makeBasicBlock(Statement::makeSend(
                chan_id, ChpExprSingleRootDag::makeVariableAccess(var_id, di.total_bitwidth_out))));

        decomp_info_t di_new = _deepcopy_decomp_info(di);
        di_new.break_after = false;
        di_new.break_before = false;
        vmap.insert({send, di_new});
        
        return send;
    }

    ChpExprSingleRootDag conc_vars;

    for ( auto var : di.live_out_vars )
    {
        if (var == *di.live_out_vars.begin())
        {
            conc_vars = ChpExprSingleRootDag::makeVariableAccess(var, g->graph.id_pool().getBitwidth(var));
        }
        else
        {
            auto v1 = ChpExprSingleRootDag::makeVariableAccess(var, g->graph.id_pool().getBitwidth(var));
            conc_vars = ChpExprSingleRootDag::makeBinaryOp(IRBinaryOpType::Concat,
                        std::make_unique<ChpExprSingleRootDag>(std::move(conc_vars)),
                        std::make_unique<ChpExprSingleRootDag>(std::move(v1)));
        }
    }

    Block *send =
            g->graph.blockAllocator().newBlock(Block::makeBasicBlock(Statement::makeSend(
                chan_id, std::move(conc_vars))));

    decomp_info_t di_new = _deepcopy_decomp_info(di);
    di_new.break_after = false;
    di_new.break_before = false;
    vmap.insert({send, di_new});

    // fprintf (stdout, "\nsize n\n");
    return send;
}

void ChoppingBlock::excise_internal_loops()
{
    _excise_internal_loops(g->graph.m_seq, 1);
}

void ChoppingBlock::_excise_internal_loops(Sequence seq, int root)
{
    Block *curr = seq.startseq->child();
    Block *tmp;

    while (curr->type() != BlockType::EndSequence) {
    switch (curr->type()) {
    case BlockType::Basic:
    break;
      
    case BlockType::Par: {
        for ( auto &branch : curr->u_par().branches )
            _excise_internal_loops (branch, root);
    }
    break;
      
    case BlockType::Select: {
        for ( auto &branch : curr->u_select().branches )
            _excise_internal_loops (branch.seq, root);
    }
    break;
      
    case BlockType::DoLoop:
        if (root == 0) 
        {
            _excise_internal_loops (curr->u_doloop().branch, root);
            // fprintf(stdout, "\nexcising internal loop \n");
            tmp = curr->child();
            _excise_loop(curr);
            // fprintf(stdout, "\nfinished excising internal loop \n");
            tmp = tmp->parent();
        }
        else {
            _excise_internal_loops (curr->u_doloop().branch, 0);
            return;
        }
        break;
    
    case BlockType::StartSequence:
    case BlockType::EndSequence:
        hassert(false);
        break;
    }
    curr = curr->child();
    }   
    return;
}

void ChoppingBlock::_excise_loop (Block *curr)
{
    Block *prev = curr->parent();
    Block *next = curr->child();

    Block *send_to_loop = NULL;
    Block *send_from_loop = NULL;
    std::vector<Block *> v_block_ptrs;
    std::vector<Block *> v_for_sm;

    std::pair<int, Sequence> recv_in_loop = {0, {}};
    std::pair<int, Sequence> recv_from_loop = {0, {}};

    send_to_loop = _generate_send_to_be_recvd_by (curr);
    if (send_to_loop) {
        v_block_ptrs.push_back(send_to_loop);
        recv_in_loop = _generate_recv_and_maybe_assigns (send_to_loop, LIVE_IN);
        switch(recv_in_loop.first)
        {
            case 0:
                break;
            case 1:
                v_for_sm.push_back(recv_in_loop.second.startseq->child());
                _splice_out_block(recv_in_loop.second.startseq->child());
                break;
            case 2:
                v_for_sm.push_back(recv_in_loop.second.startseq->child());
                _splice_out_block(recv_in_loop.second.startseq->child());
                v_for_sm.push_back(recv_in_loop.second.startseq->child());
                _splice_out_block(recv_in_loop.second.startseq->child());
                break;
        }
    }

    send_from_loop = _generate_send_to_be_sent_from (curr);
    if (send_from_loop) {
        recv_from_loop = _generate_recv_and_maybe_assigns (send_from_loop, LIVE_OUT);
        switch(recv_from_loop.first)
        {
            case 0:
                break;
            case 1:
                v_block_ptrs.push_back(recv_from_loop.second.startseq->child());
                _splice_out_block(recv_from_loop.second.startseq->child());
                break;
            case 2:
                v_block_ptrs.push_back(recv_from_loop.second.startseq->child());
                _splice_out_block(recv_from_loop.second.startseq->child());
                v_block_ptrs.push_back(recv_from_loop.second.startseq->child());
                _splice_out_block(recv_from_loop.second.startseq->child());
                break;
        }
    }

    Sequence loop_call;
    loop_call = g->graph.blockAllocator().newSequence(v_block_ptrs);

    // place the loop function call+return in the right spot
    g->graph.spliceInSequenceBefore(curr, loop_call);
    
    // tear out the loop
    _splice_out_block (curr);

    // wrap the loop in the state machine to act as function
    Sequence sm = _construct_sm_loop (curr, v_for_sm, send_from_loop);

    Sequence _ = g->graph.blockAllocator().newSequence({curr});

    v_seqs.push_back(sm);
}

Sequence ChoppingBlock::_construct_sm_loop (Block *curr, std::vector<Block *> recv, Block *send)
{

    hassert (curr->type() == BlockType::DoLoop);

    VarId c = g->graph.id_pool().makeUniqueVar(1, false);
    // assign: c := 0
    Block *init_c = g->graph.blockAllocator().newBlock(
            Block::makeBasicBlock(Statement::makeAssignment(c,ChpExprSingleRootDag::makeConstant(BigInt(0),1))));

    // assign: c := 1
    Block *c_1 = g->graph.blockAllocator().newBlock(
            Block::makeBasicBlock(Statement::makeAssignment(c,ChpExprSingleRootDag::makeConstant(BigInt(1),1))));

    // assign: c := 0 (another for later)
    Block *c_0 = g->graph.blockAllocator().newBlock(
            Block::makeBasicBlock(Statement::makeAssignment(c,ChpExprSingleRootDag::makeConstant(BigInt(0),1))));
    
    // expr: c=0 
    auto c_access_0 = ChpExprSingleRootDag::makeVariableAccess(c, 1);
    IRGuard c_eqs_0 = IRGuard::makeExpression (
                            ChpExprSingleRootDag::makeBinaryOp(IRBinaryOpType::EQ,
                            std::make_unique<ChpExprSingleRootDag>(std::move(c_access_0)),
                            std::make_unique<ChpExprSingleRootDag>(
                                ChpExprSingleRootDag::makeConstant(BigInt(0),1)
                            )));

    // expr: c=1 
    auto c_access_1 = ChpExprSingleRootDag::makeVariableAccess(c, 1);
    IRGuard c_eqs_1 = IRGuard::makeExpression (
                            ChpExprSingleRootDag::makeBinaryOp(IRBinaryOpType::EQ,
                            std::make_unique<ChpExprSingleRootDag>(std::move(c_access_1)),
                            std::make_unique<ChpExprSingleRootDag>(
                                ChpExprSingleRootDag::makeConstant(BigInt(1),1)
                            )));

    Block *select_1 = g->graph.blockAllocator().newBlock(Block::makeSelectBlock());
    Block *select_2 = g->graph.blockAllocator().newBlock(Block::makeSelectBlock());

    recv.push_back(c_1);
    Sequence line_1 = g->graph.blockAllocator().newSequence(recv);
    Sequence line_2 = g->graph.blockAllocator().newSequence({});

    select_1->u_select().branches.push_back({line_1,std::move(c_eqs_0)});
    select_1->u_select().branches.push_back({line_2,std::move(c_eqs_1)});

    ChpExprSingleRootDag e;
    e = ChpExprSingleRootDag::deep_copy(curr->u_doloop().guard);

    Block *predoloop;
    Block *postdoloop;
    std::vector<Block *> v_blks, tmp;

    predoloop = curr->u_doloop().branch.startseq->child();

    SelectBranch *sbt = NULL;

    while (predoloop->type()!=BlockType::Select && predoloop->type()!=BlockType::EndSequence) {
        v_blks.push_back(predoloop);
        predoloop = predoloop->child();
    }
    // simple do-loop
    if (predoloop->type() != BlockType::Select) {
        for (auto b : v_blks) { _splice_out_block(b); }
        select_2->u_select().branches.push_back({
            g->graph.blockAllocator().newSequence(v_blks), 
            IRGuard::makeExpression(ChpExprSingleRootDag::deep_copy(e))});
    }
    else {
        postdoloop = predoloop->child();

        for ( auto &branch : predoloop->u_select().branches )
        {
            if (!(branch.g.type() == IRGuardType::Else))
            {   
                select_2->u_select().branches.push_back({branch.seq,
                                            IRGuard::deep_copy(branch.g)});
            }
            else {
                sbt = &branch;
            }
        }
    }

    std::vector<Block *> v_blks_1 = {};
    if (sbt) {
        for ( auto bb = (*sbt).seq.startseq->child(); bb!=(*sbt).seq.endseq; bb=bb->child() ) {
            v_blks_1.push_back(bb);
        }
        for (auto bb : v_blks_1) {
            _splice_out_block(bb);
        }
    }

    Sequence line_3;
    if (send) {
        v_blks_1.push_back(c_0);
        v_blks_1.push_back(send);
    }
    else {
        v_blks_1.push_back(c_0);
    }
    line_3 = g->graph.blockAllocator().newSequence(v_blks_1);

    select_2->u_select().branches.push_back({line_3, IRGuard::makeElse()});
    
    for (auto b : v_blks)
    {
        _splice_out_block(b);
    }
    v_blks.insert(v_blks.begin(), select_1);
    v_blks.push_back(select_2);

    while (postdoloop->type() != BlockType::EndSequence) {
        tmp.push_back(postdoloop);
        v_blks.push_back(postdoloop);
        postdoloop = postdoloop->child();
    }
    
    for (auto b : tmp)
    {
        _splice_out_block(b);
    }

    // select_1, pre, select_2, post
    Sequence func = _wrap_in_do_loop(g->graph.blockAllocator().newSequence(v_blks));

    auto v_inits = _initialize_ics (curr);
    for ( auto b : v_inits )
    {
        _splice_in_block_between (func.startseq, func.startseq->child(), b);
    }
    // Sequence func = _wrap_in_do_loop(g->graph.blockAllocator().newSequence({select_1,select_2}));
    _splice_in_block_between (func.startseq, func.startseq->child(), init_c);

    _rename_all_vars(func);
    return func;
}

void ChoppingBlock::_rename_all_vars (Sequence &seq) 
{
    std::unordered_map<VarId, VarId> vm{};
    _rename_all_vars_helper(seq, vm);
}

void ChoppingBlock::_rename_all_vars_helper (Sequence &seq, std::unordered_map<VarId, VarId> &vm) 
{
    auto rename = [&](VarId &v) {
        if (!vm.count(v)) {
            int bw = g->graph.id_pool().getBitwidth(v);
            vm.insert({v,g->graph.id_pool().makeUniqueVar(bw, false)}); 
        }
        v = vm.at(v);
    };
    auto rename_vec = [&](std::vector<VarId> &vv) {
        for ( auto &v : vv ) {
            rename(v);
        }
    };
    auto rename_opt = [&](OptionalVarId &ov) {
        if (ov) {
            VarId v = *ov;
            rename(v);
            ov = OptionalVarId(v);
        }
    };
    auto rename_expr = [&](ChpExprDag &dag) {
        ChpExprDag::mapNodes(dag, [&](ChpExprDag::Node &n) {
            if (n.type() == IRExprTypeKind::Var)
                rename(n.u_var().id);
        });
    };

    Block *curr = seq.startseq->child();
    while (curr->type() != BlockType::EndSequence) {
    switch (curr->type()) {
    case BlockType::Basic: {
        switch (curr->u_basic().stmt.type()) {
        case StatementType::Send: {
            rename_expr(curr->u_basic().stmt.u_send().e.m_dag);
        }
        break;
        case StatementType::Assign: {
            rename_expr(curr->u_basic().stmt.u_assign().e);
            rename_vec(curr->u_basic().stmt.u_assign().ids);
        }
        break;
        case StatementType::Receive: {
            rename_opt(curr->u_basic().stmt.u_receive().var);
        }
        break;
        }
    }
    break;
    case BlockType::Par: {
        for (auto &branch : curr->u_par().branches) {
            _rename_all_vars_helper(branch, vm);
        }
    }
    break;
    case BlockType::Select: {
        for (auto &branch : curr->u_select().branches) {
            if (branch.g.type()==IRGuardType::Expression)
                rename_expr(branch.g.u_e().e.m_dag);
        }
        for (auto &branch : curr->u_select().branches) {
            _rename_all_vars_helper(branch.seq, vm);
        }
    }
    break;
    case BlockType::DoLoop: {
        rename_expr(curr->u_doloop().guard.m_dag);
        _rename_all_vars_helper(curr->u_doloop().branch, vm);
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

std::vector<Block *> ChoppingBlock::_initialize_ics(Block *curr)
{
    std::vector<Block *> v_inits;
    hassert (vmap.count(curr));
    hassert (curr->type() == BlockType::DoLoop);
    decomp_info_t di = (vmap.find(curr))->second;
    
    if (di.live_in_vars.size() == 0)
    {
        return {};
    }

    for ( auto var : di.live_in_vars )
    {
        Block *init_v = g->graph.blockAllocator().newBlock(
        Block::makeBasicBlock(Statement::makeAssignment(var,
            ChpExprSingleRootDag::makeConstant(BigInt(0),g->graph.id_pool().getBitwidth(var)))));
        v_inits.push_back(init_v);
    }

    return v_inits;
}

int ChoppingBlock::_splice_in_recv_before(Block *bb, Block *send, int type)
{
    if (bb->type() == BlockType::StartSequence)
        return 0;

    // hmm... test
    // if (bb->type() == BlockType::EndSequence)
    //     return 0;

    hassert (send);

    std::pair<int, Sequence> recv_plus_maybe_assigns = _generate_recv_and_maybe_assigns (send, type);
    if (!recv_plus_maybe_assigns.second.empty())
    {
        g->graph.spliceInSequenceBefore(bb, recv_plus_maybe_assigns.second);
    }
    return recv_plus_maybe_assigns.first;
}

std::pair<int, Sequence> ChoppingBlock::_generate_recv_and_maybe_assigns (Block *send, int type)
{
    hassert (vmap.count(send));
    decomp_info_t di = (vmap.find(send))->second;

    // std::unordered_set<VarId> vars;
    std::vector<VarId> vars;
    vars.clear();
    int bitwidth;
    if (type==LIVE_IN) {
        vars = di.live_in_vec;
        bitwidth = di.total_bitwidth_in;
    }
    else {
        vars = di.live_out_vec;
        bitwidth = di.total_bitwidth_out;
    }

    if (vars.size() == 0)
    {
        return std::pair(0,g->graph.blockAllocator().newSequence({}));
    }

    ChanId chan_id = send->u_basic().stmt.u_send().chan;

    if (vars.size() == 1)
    {
        VarId var_id = *vars.begin();
        OptionalVarId ovid(var_id);

        Block *receive = g->graph.blockAllocator().newBlock(
                Block::makeBasicBlock(Statement::makeReceive(chan_id, ovid)));
        // test
        decomp_info_t di_new = _deepcopy_decomp_info(di);
        di_new.break_after = false;
        di_new.break_before = false;
        vmap.insert({receive, di_new});
        // test

        return std::pair(1,g->graph.blockAllocator().newSequence({receive}));
    }

    VarId var_concat = g->graph.id_pool().makeUniqueVar(bitwidth, false);
    OptionalVarId ovid(var_concat);
    // C?x
    Block *receive = g->graph.blockAllocator().newBlock(
            Block::makeBasicBlock(Statement::makeReceive(chan_id, ovid)));
    
    // test
    decomp_info_t di_new = _deepcopy_decomp_info(di);
    di_new.break_after = false;
    di_new.break_before = false;
    vmap.insert({receive, di_new});
    // test

    //  x1 = x{0..i} , x2 = x{i+1..j} ...
    // make the assign blocks, parallel them all

    int range_ctr = bitwidth;
    Block *parallel = g->graph.blockAllocator().newBlock(Block::makeParBlock());
    for ( auto var : vars )
    {
        hassert (range_ctr>=0);
        VarId vi = var;
        int width = g->graph.id_pool().getBitwidth(vi);
        // vi := v_concat{i+w..i}
        hassert (width>=0);
        if (width>0) {
        Block *assign = g->graph.blockAllocator().newBlock(Block::makeBasicBlock(
                    Statement::makeAssignment(vi, 
                        ChpExprSingleRootDag::makeBitfield(
                            std::make_unique<ChpExprSingleRootDag>(
                                ChpExprSingleRootDag::makeVariableAccess(
                                    var_concat, bitwidth)),
                            range_ctr-1, range_ctr-width)
                                            )));

        parallel->u_par().branches.push_back(
                g->graph.blockAllocator().newSequence({assign}));
        }
        range_ctr -= (width);
    }

    vmap.insert({parallel, di_new});

    return std::pair(2,g->graph.blockAllocator().newSequence({receive, parallel}));
}
