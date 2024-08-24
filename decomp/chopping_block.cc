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
#include "../opt/utils.h"

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

    hassert (vmap.contains(bb));
    decomp_info_t *di = (vmap.find(bb))->second;

    if (di->total_bitwidth_in == 0)
    {
        return NULL;
    }

    ChanId chan_id = g->graph.id_pool().makeUniqueChan(di->total_bitwidth_in, false);
    var_to_actvar vtoa(s, &g->graph.id_pool());
    ActId *id = vtoa.chanMap(chan_id);
    g->name_from_chan.insert({chan_id, id});

    if (di->live_in_vars.size() == 1)
    {
        VarId var_id = *di->live_in_vars.begin();
        Block *send =
            g->graph.blockAllocator().newBlock(Block::makeBasicBlock(Statement::makeSend(
                chan_id, ChpExprSingleRootDag::makeVariableAccess(var_id, di->total_bitwidth_in))));
        

        decomp_info_t *di_new = _deepcopy_decomp_info(di);
        di_new->break_after = false;
        di_new->break_before = false;
        vmap.insert({send, di_new});
        
        return send;
    }

    ChpExprSingleRootDag conc_vars;

    for ( auto var : di->live_in_vars )
    {
        if (var == *di->live_in_vars.begin())
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

    decomp_info_t *di_new = _deepcopy_decomp_info(di);
    di_new->break_after = false;
    di_new->break_before = false;
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

    hassert (vmap.contains(bb));
    decomp_info_t *di = (vmap.find(bb))->second;

    if (di->total_bitwidth_out == 0)
    {
        return NULL;
    }

    ChanId chan_id = g->graph.id_pool().makeUniqueChan(di->total_bitwidth_out, false);
    var_to_actvar vtoa(s, &g->graph.id_pool());
    ActId *id = vtoa.chanMap(chan_id);
    g->name_from_chan.insert({chan_id, id});

    if (di->live_out_vars.size() == 1)
    {
        VarId var_id = *di->live_out_vars.begin();
        Block *send =
            g->graph.blockAllocator().newBlock(Block::makeBasicBlock(Statement::makeSend(
                chan_id, ChpExprSingleRootDag::makeVariableAccess(var_id, di->total_bitwidth_out))));

        decomp_info_t *di_new = _deepcopy_decomp_info(di);
        di_new->break_after = false;
        di_new->break_before = false;
        vmap.insert({send, di_new});
        
        return send;
    }

    ChpExprSingleRootDag conc_vars;

    for ( auto var : di->live_out_vars )
    {
        if (var == *di->live_out_vars.begin())
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

    decomp_info_t *di_new = _deepcopy_decomp_info(di);
    di_new->break_after = false;
    di_new->break_before = false;
    vmap.insert({send, di_new});

    // fprintf (stdout, "\nsize n\n");
    return send;
}

void ChoppingBlock::chop_graph()
{
    _handle_ic_lcd(g->graph.m_seq);
    _chop_graph(g->graph.m_seq, 2);
}

void ChoppingBlock::excise_internal_loops()
{
    _excise_internal_loops(g->graph.m_seq, 1);
}

Block *ChoppingBlock::_find_next_break_after (Block *b)
{
    if (b->type() == BlockType::StartSequence || b->type() == BlockType::EndSequence)
        return NULL;

    Block *itr = b;
    while (itr->type() != BlockType::EndSequence)
    {
        hassert (vmap.contains(itr));
        decomp_info_t *di = (vmap.find(itr))->second;
        if (di->break_after)
            return itr;
        itr = itr->child();
    }
    return NULL;
}

Block *ChoppingBlock::_find_next_break_before (Block *b)
{
    if (b->type() == BlockType::StartSequence || b->type() == BlockType::EndSequence)
        return NULL;

    Block *itr = b;
    while (itr->type() != BlockType::EndSequence)
    {
        hassert (vmap.contains(itr));
        decomp_info_t *di = (vmap.find(itr))->second;
        if (di->break_before)
            return itr;
        itr = itr->child();
    }
    return NULL;
}

/*
 * Tear out from b_start to b_end.
 * b_start included, b_end EXCLUDED.
 * TODO: check if both are part of same Sequence, 
 * assuming that's true for now.
*/
std::vector<Block *> ChoppingBlock::_split_sequence_from_to(Block *b_start, Block *b_end)
{
    hassert (b_start->type() != BlockType::StartSequence);

    std::vector<Block *> v_block_ptrs = {};

    Block *itr = b_start;

    do {
        v_block_ptrs.push_back (itr);
        itr = _splice_out_block (itr);
    }
    while (itr != b_end);

    return v_block_ptrs;
}

std::vector<Block *> ChoppingBlock::_split_sequence_from_to_new(Block *b_start, Block *b)
{
    hassert (b_start->type() == BlockType::StartSequence);

    std::vector<Block *> v_block_ptrs = {};

    Block *end = b->child();
    hassert (end);

    Block *itr = b_start->child();
    hassert (itr);

    do {
        v_block_ptrs.push_back (itr);
        itr = _splice_out_block (itr);
    }
    while (itr != end);

    return v_block_ptrs;
}

Block *ChoppingBlock::_build_sequence (Block *b_start, Block *b_end_plus_1, int type)
{
    if (type == END_EXC) hassert (b_start != b_end_plus_1);

    std::vector<Block *> v_block_ptrs;
    if (type == END_EXC) {
        v_block_ptrs = _split_sequence_from_to (b_start, b_end_plus_1);
    }
    else {
        v_block_ptrs = _split_sequence_from_to_new (b_start, b_end_plus_1);
    }

    Block *send = NULL;
    if (type == END_EXC) {
        send = _generate_send_to_be_recvd_by (b_end_plus_1);
    }   
    else {
        send = _generate_send_to_be_sent_from (b_end_plus_1);
    } 
    if (send)
        v_block_ptrs.push_back(send);

    Sequence seq_out;
    seq_out = g->graph.blockAllocator().newSequence(v_block_ptrs);

    v_seqs.push_back(_wrap_in_do_loop(seq_out));
    return send;
}

void ChoppingBlock::_handle_ic_lcd (Sequence seq)
{
    Block *curr = seq.startseq->child();
    if (curr->type() == BlockType::DoLoop) 
    {
        return;
    }
    while (curr->type() != BlockType::DoLoop && curr->type() != BlockType::EndSequence)
    {
        curr = curr->child();
    }
    hassert (curr->type() == BlockType::DoLoop);
    _handle_ic_lcd_helper (curr);
}

void ChoppingBlock::_handle_ic_lcd_helper (Block *doloop)
{
    Block *send_to_main   = _generate_send_to_be_recvd_by(doloop);
    Block *send_from_main = _generate_send_to_be_recvd_by(doloop);
    Block *recv_from_main = NULL;
    Block *new_main_blk = g->graph.blockAllocator().newBlock(Block::makeDoLoopBlock());

    Sequence seq = doloop->u_doloop().branch;
    Sequence lcd = g->graph.newSequence({});

    if (send_to_main) 
    {
        // insert comms in main
        _splice_in_recv_before (seq.startseq->child(), send_to_main, LIVE_IN);
        _splice_in_block_between (seq.endseq->parent(), seq.endseq, send_from_main);

        // insert comms in new process
        _splice_in_block_between (lcd.startseq,lcd.startseq->child(), send_to_main);
        _splice_in_recv_before (lcd.endseq, send_from_main, LIVE_IN);

        // build lcd handler and add to vec
        doloop->u_doloop().branch = lcd;
        doloop->u_doloop().guard = (ChpExprSingleRootDag::of_expr(
                                        ChpExpr::makeConstant(BigInt{1}, 1)));
        v_seqs.push_back(g->graph.m_seq);

        // replace main with new one
        new_main_blk->u_doloop().branch = seq;
        new_main_blk->u_doloop().guard = (ChpExprSingleRootDag::of_expr(
                                        ChpExpr::makeConstant(BigInt{1}, 1)));
        Sequence new_main = g->graph.newSequence({new_main_blk});
        g->graph.m_seq = new_main;
    }
}

void ChoppingBlock::_chop_graph(Sequence seq, int root)
{
    Block *curr = seq.startseq->child();
    decomp_info *di, *di_par;
    Block *end_block;
    Block *tmp;
    int n = 0;

    while (curr->type() != BlockType::EndSequence) {
    switch (curr->type()) {
    case BlockType::Basic: {
        hassert (vmap.contains(curr));
        di = (vmap.find(curr))->second;
        if (di->break_before && (curr != seq.startseq->child()))
        {   
            Block *send = _build_sequence (seq.startseq->child(), curr, END_EXC);
            if (send) {
                _splice_in_recv_before (curr, send, LIVE_IN);
            }
        }
    }
    break;
      
    case BlockType::Par: {

        hassert (vmap.contains(curr));
        di = (vmap.find(curr))->second;
        if (di->break_before && di->break_after)
        {
            Block *send = _build_sequence (seq.startseq->child(), curr, END_EXC);
            if (send) {
                n = _splice_in_recv_before (curr, send, LIVE_IN);
            }

            Block *pll_merge_send = _process_parallel (curr, n);
            for (auto &branch : curr->u_par().branches) {
                _chop_graph (branch, 0);
            }
            // tear out the old pll
            curr = curr->child();
            _splice_out_block (curr->parent());
            // insert the recv for the merge_send just after the old pll location
            if (pll_merge_send) {
                int n1 = _splice_in_recv_before (curr, pll_merge_send, LIVE_IN);
            }
            continue;
        }
    }
    break;
      
    case BlockType::Select: {
        hassert (vmap.contains(curr));
        di = (vmap.find(curr))->second;
        if (di->break_before && di->break_after)
        {
            Block *send = _build_sequence (seq.startseq->child(), curr, END_EXC);
            // Block *send = _build_sequence (seq.startseq, curr, END_EXC);
            // impossible for non-trivial selections to have no live-in vars.
            // at least guard vars must exist...
            hassert (send);
            if (send) {
                n = _splice_in_recv_before (curr, send, LIVE_IN);
            }
            // need to tear out the newly generated recv also (done)
            // fprintf (stdout, "\ngot here sel\n");
            // n==0 => selection has variable-less guards only i.e. constant true/false.
            // in which case, go rewrite your program, will deal with this later..
            hassert (n!=0);
            Block *merge_send = _process_selection (curr, n);
            for (auto &branch : curr->u_select().branches) {
                _chop_graph (branch.seq, 0);
            }
            // tear out the old selection
            curr = curr->child();
            _splice_out_block (curr->parent());
            // insert the recv for the merge_send just after the old selection location
            if (merge_send) {
                int n1 = _splice_in_recv_before (curr, merge_send, LIVE_IN);
            }
            continue;
        }
    }
    break;
      
    case BlockType::DoLoop:
        if (root != 2) 
        {
            fatal_error ("excise internal loops please");
        }
        else {
            _chop_graph (curr->u_doloop().branch, 1);
        }
        // don't do tail processing for doloop
        break;
    
    case BlockType::StartSequence:
    case BlockType::EndSequence:
        hassert(false);
        break;
    }
    curr = curr->child();
    }
    // tear out tail
    if (!seq.empty() && (root == 0))
    {   
        // incoming and outgoing dependencies handled in _process_selection
        // _build_sequence (seq.startseq, seq.endseq->parent(), END_INC);
        _build_sequence (seq.startseq->child(), seq.endseq, END_EXC);
    }
    if (!seq.empty() && (root == 2))
    {   
        v_seqs.push_back(seq);
    }
    return;
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
            _excise_internal_loops (branch, 0);
    }
    break;
      
    case BlockType::Select: {
        for ( auto &branch : curr->u_select().branches )
            _excise_internal_loops (branch.seq, 0);
    }
    break;
      
    case BlockType::DoLoop:
        if (root != 1) 
        {
            _excise_internal_loops (curr->u_doloop().branch, 0);
            // fprintf(stdout, "\nexcising internal loop \n");
            tmp = curr->child();
            _excise_loop(curr);
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

Block *ChoppingBlock::_excise_loop (Block *curr)
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
    return NULL;
}

/*
    construct this:
    x:=0;
    init_vars:=0;
    *[  [ c=0 -> Ls?{vars};{assign all from concat};c:=1 (line 1)
        []c=1 -> skip (line 2)
        ];
        [ L
        []else -> Lf!{vars};c:=0 (line 3)
        ]  ]
    
    where L is the original loop (turned into selection) and an else branch is added
*/
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

    Sequence line_3;
    if (send) {
        line_3 = g->graph.blockAllocator().newSequence({c_0,send});
    }
    else {
        line_3 = g->graph.blockAllocator().newSequence({c_0});
    }

    ChpExprSingleRootDag e;
    e = ChpExprSingleRootDag::deep_copy(curr->u_doloop().guard);

    Block *predoloop;
    Block *postdoloop;
    std::vector<Block *> v_blks, tmp;

    predoloop = curr->u_doloop().branch.startseq->child();
    while (predoloop->type() != BlockType::Select) {
        v_blks.push_back(predoloop);
        predoloop = predoloop->child();
    }
    hassert (predoloop->type() == BlockType::Select);
    postdoloop = predoloop->child();

    for ( auto &branch : predoloop->u_select().branches )
    {
        if (!(branch.g.type() == IRGuardType::Else))
        {   
            select_2->u_select().branches.push_back({branch.seq,
                                        IRGuard::deep_copy(branch.g)});
        }
    }
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

    return func;
}

std::vector<Block *> ChoppingBlock::_initialize_ics(Block *curr)
{
    std::vector<Block *> v_inits;
    hassert (vmap.contains(curr));
    decomp_info_t *di = (vmap.find(curr))->second;
    
    if (di->live_in_vars.size() == 0)
    {
        return {};
    }

    for ( auto var : di->live_in_vars )
    {
        Block *init_v = g->graph.blockAllocator().newBlock(
        Block::makeBasicBlock(Statement::makeAssignment(var,ChpExprSingleRootDag::makeConstant(BigInt(0),1))));
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
    hassert (vmap.contains(send));
    decomp_info_t *di = (vmap.find(send))->second;

    std::unordered_set<VarId> vars;
    vars.clear();
    int bitwidth;
    if (type==LIVE_IN) {
        vars = di->live_in_vars;
        bitwidth = di->total_bitwidth_in;
    }
    else {
        vars = di->live_out_vars;
        bitwidth = di->total_bitwidth_out;
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
        decomp_info_t *di_new = _deepcopy_decomp_info(di);
        di_new->break_after = false;
        di_new->break_before = false;
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
    decomp_info_t *di_new = _deepcopy_decomp_info(di);
    di_new->break_after = false;
    di_new->break_before = false;
    vmap.insert({receive, di_new});
    // test

    //  x1 = x{0..i} , x2 = x{i+1..j} ...
    // make the assign blocks, parallel them all

    int range_ctr = bitwidth;
    Block *parallel = g->graph.blockAllocator().newBlock(Block::makeParBlock());
    for ( auto var : vars )
    {
        VarId vi = var;
        int width = g->graph.id_pool().getBitwidth(vi);
        // vi := v_concat{i+w..i}
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
        range_ctr -= (width);
    }

    vmap.insert({parallel, di_new});

    return std::pair(2,g->graph.blockAllocator().newSequence({receive, parallel}));
}

Block *ChoppingBlock::_process_parallel (Block *pll, int n)
{
    hassert (n<3);

    hassert (pll->type() == BlockType::Par);
    hassert (vmap.contains(pll));
    decomp_info_t *di = (vmap.find(pll))->second;

    Block *recv;
    Block *pll_assigns;
    Sequence seq_pll;

    auto [sends_to_pll, sends_out_of_pll] = _generate_pll_send_recv_and_seed_branches (pll);

    if (n==0) {
        seq_pll = g->graph.blockAllocator().newSequence({sends_to_pll});
    }
    else if (n==1) {
        recv = pll->parent();
        _splice_out_block (recv);
        seq_pll = g->graph.blockAllocator().newSequence({recv,sends_to_pll});
    }
    else if (n==2) {
        recv = pll->parent()->parent();
        pll_assigns = pll->parent();
        _splice_out_block (recv);
        _splice_out_block (pll_assigns);
        seq_pll = g->graph.blockAllocator().newSequence({recv, pll_assigns, sends_to_pll});
    }
    else {
        hassert (false);
    }

    v_seqs.push_back(_wrap_in_do_loop(seq_pll));

    if (sends_out_of_pll) {
        return sends_out_of_pll;
    }
    return NULL;
}

Block *ChoppingBlock::_process_selection (Block *sel, int n)
{
    hassert (n<3);
    // hassert (n>0);
    hassert (sel->type() == BlockType::Select);
    hassert (vmap.contains(sel));
    decomp_info_t *di = (vmap.find(sel))->second;

    Block *recv;
    Block *pll_assigns;
    Sequence seq_split;

    // generate split and merge ------------------------
    auto [split, merge_send] = _generate_split_merge_and_seed_branches (sel);
    // also sends live_vars to head of branches,
    // places receives at the headto get them,
    // places sends at the tails of branches
    // (done)

    if (n==1) {
        recv = sel->parent();
        _splice_out_block (recv);
        // seq = g->graph.blockAllocator().newSequence({recv});
        seq_split = g->graph.blockAllocator().newSequence({recv,split});
    }
    else if (n==2) {
        recv = sel->parent()->parent();
        pll_assigns = sel->parent();
        _splice_out_block (recv);
        _splice_out_block (pll_assigns);
        // seq = g->graph.blockAllocator().newSequence({recv, pll_assigns});
        seq_split = g->graph.blockAllocator().newSequence({recv, pll_assigns, split});
    }
    else {
        hassert (false);
    }
    v_seqs.push_back(_wrap_in_do_loop(seq_split));
    // generate split and merge ------------------------

    // TODO TMRW
    // generate control-signal fifo
    if (merge_send) {
        return merge_send;
    }
    return NULL;
}

std::pair<Block *, Block *> ChoppingBlock::_generate_pll_send_recv_and_seed_branches (Block *pll)
{
    hassert (vmap.contains(pll));
    decomp_info_t *di_pll = (vmap.find(pll))->second;

    bool merge_needed = true;
    if (di_pll->total_bitwidth_out == 0) merge_needed = false;

    VarId merge_var;
    if (merge_needed) {
        merge_var = g->graph.id_pool().makeUniqueVar(di_pll->total_bitwidth_out, false);
    }

    Block *pll_sends = g->graph.blockAllocator().newBlock(Block::makeParBlock());
    Block *pll_recvs = g->graph.blockAllocator().newBlock(Block::makeParBlock());

    for (auto &branch : pll->u_par().branches) {

        hassert (!(branch.empty()));

        Block *send_live_vars_to_branch = NULL;
        Block *send_live_vars_from_branch = NULL;

        // send live-vars to the branch head
        send_live_vars_to_branch = 
            _generate_send_to_be_recvd_by (branch.startseq->child());

        if (send_live_vars_to_branch) {
            pll_sends->u_par().branches.push_back(
                g->graph.blockAllocator().newSequence({send_live_vars_to_branch}));

            _splice_in_recv_before (branch.startseq->child(), send_live_vars_to_branch, LIVE_IN);
        }

        send_live_vars_from_branch = 
            _generate_send_to_be_sent_from (branch.endseq->parent());
    
        if (send_live_vars_from_branch) {
            _splice_in_block_between (branch.endseq->parent(),branch.endseq, 
                                        send_live_vars_from_branch);

            if (merge_needed) {
                ChanId recv_chan = send_live_vars_from_branch->u_basic().stmt.u_send().chan;
                Block *rx_live = g->graph.blockAllocator().newBlock(
                        Block::makeBasicBlock(Statement::makeReceive(recv_chan, merge_var)));
                
                pll_recvs->u_par().branches.emplace_back(
                    g->graph.blockAllocator().newSequence({rx_live}));      
            }
        }
    } //end loop over branches

    Block *pll_send_data = NULL;

    if (merge_needed) {

        ChanId send_chan = g->graph.id_pool().makeUniqueChan(di_pll->total_bitwidth_out, false);
        var_to_actvar vtoa(s, &g->graph.id_pool());
        ActId *id = vtoa.chanMap(send_chan);
        g->name_from_chan.insert({send_chan, id});

        pll_send_data = 
            g->graph.blockAllocator().newBlock(Block::makeBasicBlock(Statement::makeSend(
                send_chan, ChpExprSingleRootDag::makeVariableAccess(merge_var, di_pll->total_bitwidth_out))));

        decomp_info_t *di_pll_new = _deepcopy_decomp_info(di_pll);
        di_pll_new->live_in_vars = di_pll->live_out_vars;
        di_pll_new->total_bitwidth_in = di_pll->total_bitwidth_out;
        di_pll_new->live_out_vars = {};
        di_pll_new->total_bitwidth_out = 0;
        di_pll_new->break_after = false;
        di_pll_new->break_before = false;
        vmap.insert({pll_send_data, di_pll_new});

        Sequence merge_pll_proc = g->graph.blockAllocator().newSequence(
                                {pll_recvs, pll_send_data});
        v_seqs.push_back(_wrap_in_do_loop(merge_pll_proc));
    }

    return {pll_sends, pll_send_data};

}

/*
    *[C?live_in; { live_1 := live_in{0..i}, live_2 := live_in{i+1..j} ... }
        [ G1 -> Ctrl!1 , Co1!live_out_1
        []G2 -> Ctrl!2 , Co2!live_out_2
        ..
        []Gn -> Ctrl!n , Con!live_out_n
        ]
     ]
*/
std::pair<Block *, Block *> ChoppingBlock::_generate_split_merge_and_seed_branches (Block *sel)
{
    Block *split = g->graph.blockAllocator().newBlock(Block::makeSelectBlock());

    int ctrl_bw = log_2_round_up (sel->u_select().branches.size());
    if (ctrl_bw == 0) ctrl_bw = 1;

    ChanId ctrl_chan_id = g->graph.id_pool().makeUniqueChan(ctrl_bw, false);
    var_to_actvar vtoa(s, &g->graph.id_pool());
    ActId *id = vtoa.chanMap(ctrl_chan_id);
    g->name_from_chan.insert({ctrl_chan_id, id});

    Block *merge = g->graph.blockAllocator().newBlock(Block::makeSelectBlock());

    // receive in the merge
    VarId ctrl_id = g->graph.id_pool().makeUniqueVar(ctrl_bw, false);
    Block *merge_ctrl_recv = g->graph.blockAllocator().newBlock(
            Block::makeBasicBlock(Statement::makeReceive(ctrl_chan_id, ctrl_id)));
    hassert (vmap.contains(sel));
    decomp_info_t *di_sel = (vmap.find(sel))->second;

    bool merge_needed = true;
    if (di_sel->total_bitwidth_out == 0) merge_needed = false;

    VarId merge_var;
    if (merge_needed) {
        merge_var = g->graph.id_pool().makeUniqueVar(di_sel->total_bitwidth_out, false);
    }

    int branch_itr = 0;
    for (auto &branch : sel->u_select().branches) {

        // send control value on split<->merge channel 
        Block *send_ctrl = 
            g->graph.blockAllocator().newBlock(Block::makeBasicBlock(Statement::makeSend(
                ctrl_chan_id, ChpExprSingleRootDag::makeConstant(BigInt(branch_itr) ,ctrl_bw))));
        
        // merge branch guard
        auto ctrl_id_access = ChpExprSingleRootDag::makeVariableAccess(ctrl_id, g->graph.id_pool().getBitwidth(ctrl_id));
        IRGuard ctrl_eqs_i = IRGuard::makeExpression (
                                ChpExprSingleRootDag::makeBinaryOp(IRBinaryOpType::EQ,
                                std::make_unique<ChpExprSingleRootDag>(std::move(ctrl_id_access)),
                                std::make_unique<ChpExprSingleRootDag>(
                                    ChpExprSingleRootDag::makeConstant(BigInt(branch_itr) ,ctrl_bw)
                                )));

        Block *pll_sends = g->graph.blockAllocator().newBlock(Block::makeParBlock());

        if (merge_needed) {
            pll_sends->u_par().branches.push_back(
                g->graph.blockAllocator().newSequence({send_ctrl}));
        }

        Block *send_live_vars_to_branch = NULL;
        Block *send_live_vars_from_branch = NULL;

        // branch has stuff, so send in live_vars that it needs --------------
        if (!branch.seq.empty()) {
            // send live-vars to the branch head
            send_live_vars_to_branch = 
                _generate_send_to_be_recvd_by (branch.seq.startseq->child());
        }
        else {
            // send live-vars out of merge
            send_live_vars_to_branch = 
                _generate_send_to_be_sent_from (sel);
        }
        if (send_live_vars_to_branch) {
            pll_sends->u_par().branches.push_back(
                g->graph.blockAllocator().newSequence({send_live_vars_to_branch}));
            // insert receive blocks at the branch head to get the live_vars
            _splice_in_recv_before (branch.seq.startseq->child(), send_live_vars_to_branch, LIVE_IN);
        }
        // branch has stuff, so send in live_vars that it needs --------------


        split->u_select().branches.emplace_back(
            g->graph.blockAllocator().newSequence({pll_sends}), std::move(branch.g));


        // insert send blocks at the end of the branch -----------------------
        if (!branch.seq.empty()) {
            send_live_vars_from_branch = 
                _generate_send_to_be_sent_from (branch.seq.endseq->parent());
        }
        else {
        // this is unchaged wrt send_live_vars_to_branch since branch is empty
            send_live_vars_from_branch = 
                _generate_send_to_be_sent_from (sel);
        }
        if (send_live_vars_from_branch) {
            _splice_in_block_between (branch.seq.endseq->parent(),branch.seq.endseq, 
                                        send_live_vars_from_branch);

            if (merge_needed) {
                ChanId recv_chan = send_live_vars_from_branch->u_basic().stmt.u_send().chan;
                Block *rx_merge = g->graph.blockAllocator().newBlock(
                        Block::makeBasicBlock(Statement::makeReceive(recv_chan, merge_var)));
                
                merge->u_select().branches.emplace_back(
                    g->graph.blockAllocator().newSequence({rx_merge}), std::move(ctrl_eqs_i));
            }
        }
        else {
            if (merge_needed) {
                merge->u_select().branches.emplace_back(
                    g->graph.blockAllocator().newSequence({}), std::move(ctrl_eqs_i));
            }
        }
        // insert send blocks at the end of the branch -----------------------

        branch_itr++;
    } //end loop over branches

    Block *merge_send_data = NULL;
    // generate the send out of the merge
    if (merge_needed) {

        ChanId merge_send_chan = g->graph.id_pool().makeUniqueChan(di_sel->total_bitwidth_out, false);
        var_to_actvar vtoa(s, &g->graph.id_pool());
        ActId *id = vtoa.chanMap(merge_send_chan);
        g->name_from_chan.insert({merge_send_chan, id});

        merge_send_data = 
            g->graph.blockAllocator().newBlock(Block::makeBasicBlock(Statement::makeSend(
                merge_send_chan, ChpExprSingleRootDag::makeVariableAccess(merge_var, di_sel->total_bitwidth_out))));

        decomp_info_t *di_sel_new = _deepcopy_decomp_info(di_sel);
        di_sel_new->live_in_vars = di_sel->live_out_vars;
        di_sel_new->total_bitwidth_in = di_sel->total_bitwidth_out;
        di_sel_new->live_out_vars = {};
        di_sel_new->total_bitwidth_out = 0;
        di_sel_new->break_after = false;
        di_sel_new->break_before = false;
        vmap.insert({merge_send_data, di_sel_new});

        Sequence merge_proc = g->graph.blockAllocator().newSequence(
                                {merge_ctrl_recv, merge, merge_send_data});
        v_seqs.push_back(_wrap_in_do_loop(merge_proc));
    }
    
    return {split,merge_send_data};
}
