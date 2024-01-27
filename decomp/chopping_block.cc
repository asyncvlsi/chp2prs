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

    if (di->live_in_vars.size() == 0)
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

    if (di->live_out_vars.size() == 0)
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

    fprintf (stdout, "\nsize n\n");
    return send;
}

// new
// break from the first break_before to the closest break_after

void ChoppingBlock::chop_graph()
{
    _chop_graph(g->graph.m_seq, 1);
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

    std::vector<Block *> v_block_ptrs;

    Block *itr = b_start;
    
    do {
        v_block_ptrs.push_back (itr);
        itr = _splice_out_block (itr);
    }
    while (itr != b_end);

    return v_block_ptrs;
}

Block *ChoppingBlock::_build_sequence (Block *b_start, Block *b_end_plus_1)
{
    hassert (b_start != b_end_plus_1);

    std::vector<Block *> v_block_ptrs = _split_sequence_from_to (b_start, b_end_plus_1);
    
    Block *send = _generate_send_to_be_recvd_by (b_end_plus_1);
    if (send)
        v_block_ptrs.push_back(send);

    Sequence seq_out;
    seq_out = g->graph.blockAllocator().newSequence(v_block_ptrs);

    v_seqs.push_back(_wrap_in_do_loop(seq_out));
    return send;
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
        // switch (curr->u_basic().stmt.type()) {
        //     case StatementType::Send:
        //         break;
        //     case StatementType::Assign:
        //         break;
        //     case StatementType::Receive:
        //         break;
        // }
        if (di->break_before)
        {
            Block *send = _build_sequence (seq.startseq->child(), curr);
            if (send)
                _splice_in_recv_before (curr, send);
        }
    }
    break;
      
    case BlockType::Par: {
        fatal_error ("working on par");
        for (auto &branch : curr->u_par().branches) {
            _chop_graph (branch, 0);
        }
    }
    break;
      
    case BlockType::Select: {
        hassert (vmap.contains(curr));
        di = (vmap.find(curr))->second;
        if (di->break_before)
        {
            Block *send = _build_sequence (seq.startseq->child(), curr);
            // impossible for non-trivial selections to have no live-in vars.
            // at least guard vars must exist...
            hassert (send);
            if (send)
                n = _splice_in_recv_before (curr, send);

            if (di->break_after)
            {   
                // need to tear out the newly generated recv also
                fprintf (stdout, "\ngot here\n");
                // n==0 => selection has variable-less guards i.e. true, false etc.
                // in which case, go rewrite, will deal with this later
                hassert (n!=0);
                _process_selection (curr, n);
                for (auto &branch : curr->u_select().branches) {
                    _chop_graph (branch.seq, 0);
                    // v_seqs.push_back(branch.seq);
                }
                curr = curr->child();
                _splice_out_block (curr->parent());
                continue;
            }
        }
    }
    break;
      
    case BlockType::DoLoop:
        if (root != 1)
            fatal_error ("excise internal loops please");
        _chop_graph (curr->u_doloop().branch, 1);
        // don't do tail processing for doloop
        return;
        break;
    
    case BlockType::StartSequence:
    case BlockType::EndSequence:
        hassert(false);
        break;
    }
    curr = curr->child();
    }
    // tear out tail
    if (!seq.empty())
    {   
        // TODO: need to add incoming and outgoing dependencies here too
        // coz this might be in a sub-branch
        // Note: Handled in _process_selection
        // fprintf (stdout, "\ngot to tail\n");
        _build_sequence (seq.startseq->child(), seq.endseq);
    }
    return;
}

int ChoppingBlock::_splice_in_recv_before(Block *bb, Block *send)
{
    if (bb->type() == BlockType::StartSequence)
        return 0;

    // hmm... test
    // if (bb->type() == BlockType::EndSequence)
    //     return 0;

    hassert (send);

    std::pair<int, Sequence> recv_plus_maybe_assigns = _generate_recv_and_maybe_assigns (send);
    if (!recv_plus_maybe_assigns.second.empty())
    {
        g->graph.spliceInSequenceBefore(bb, recv_plus_maybe_assigns.second);
    }
    return recv_plus_maybe_assigns.first;
}

std::pair<int, Sequence> ChoppingBlock::_generate_recv_and_maybe_assigns (Block *send)
{
    hassert (vmap.contains(send));
    decomp_info_t *di = (vmap.find(send))->second;

    if (di->live_in_vars.size() == 0)
    {
        return std::pair(0,g->graph.blockAllocator().newSequence({}));
    }

    ChanId chan_id = send->u_basic().stmt.u_send().chan;

    if (di->live_in_vars.size() == 1)
    {
        VarId var_id = *di->live_in_vars.begin();
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

    VarId var_concat = g->graph.id_pool().makeUniqueVar(di->total_bitwidth_in, false);
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

    int range_ctr = di->total_bitwidth_in;
    Block *parallel = g->graph.blockAllocator().newBlock(Block::makeParBlock());
    for ( auto var : di->live_in_vars )
    {
        VarId vi = var;
        int width = g->graph.id_pool().getBitwidth(vi);
        // vi := v_concat{i+w..i}
        Block *assign = g->graph.blockAllocator().newBlock(Block::makeBasicBlock(
                    Statement::makeAssignment(vi, 
                        ChpExprSingleRootDag::makeBitfield(
                            std::make_unique<ChpExprSingleRootDag>(
                                ChpExprSingleRootDag::makeVariableAccess(
                                    var_concat, di->total_bitwidth_in)),
                            range_ctr-1, range_ctr-width)
                                            )));

        parallel->u_par().branches.push_back(
                g->graph.blockAllocator().newSequence({assign}));
        range_ctr -= (width);
    }

    return std::pair(2,g->graph.blockAllocator().newSequence({receive, parallel}));
}

void ChoppingBlock::_process_selection (Block *sel, int n)
{
    hassert (n<3);
    // hassert (n>0);
    hassert (sel->type() == BlockType::Select);
    hassert (vmap.contains(sel));
    decomp_info_t *di = (vmap.find(sel))->second;

    Block *recv;
    Block *pll_assigns;
    Sequence seq, seq_split;

    // generate split ------------------------
    Block *split = _generate_split_and_seed_branches (sel);
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
    // generate split ------------------------

    // TODO TMRW
    // generate control-signal fifo

    // generate merge


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
Block *ChoppingBlock::_generate_split_and_seed_branches (Block *sel)
{
    Block *split = g->graph.blockAllocator().newBlock(
        Block::makeSelectBlock());

    int ctrl_bw = log_2_round_up (sel->u_select().branches.size());
    ChanId ctrl_chan_id = g->graph.id_pool().makeUniqueChan(ctrl_bw, false);

    int branch_itr = 0;
    for (auto &branch : sel->u_select().branches) {

        // send control value on split<->merge channel 
        Block *send_ctrl = 
            g->graph.blockAllocator().newBlock(Block::makeBasicBlock(Statement::makeSend(
                ctrl_chan_id, ChpExprSingleRootDag::makeConstant(BigInt(branch_itr) ,ctrl_bw))));

        // branch has stuff, so send in and pull out the live vars from it
        if (!branch.seq.empty()) {
            // send live-vars to the branch head
            Block *send_live_vars_to_branch = 
                _generate_send_to_be_recvd_by (branch.seq.startseq->child());

            Block *pll_sends = g->graph.blockAllocator().newBlock(Block::makeParBlock());

            // parallel compose the control send and live_var send 
            pll_sends->u_par().branches.push_back(
                g->graph.blockAllocator().newSequence({send_ctrl}));

            // parallel compose the control send and live_var send 
            if (send_live_vars_to_branch) {
                pll_sends->u_par().branches.push_back(
                    g->graph.blockAllocator().newSequence({send_live_vars_to_branch}));
                
                // insert receive blocks at the branch head to get the live_vars
                _splice_in_recv_before (branch.seq.startseq->child(), send_live_vars_to_branch);
            }

            // insert send blocks at the end of the branch
            Block *send_live_vars_from_branch = 
                _generate_send_to_be_sent_from (branch.seq.endseq->parent());

            if (send_live_vars_from_branch) {
                _splice_in_block_between (branch.seq.endseq->parent(),branch.seq.endseq, 
                                            send_live_vars_from_branch);
            }

            split->u_select().branches.emplace_back(
                g->graph.blockAllocator().newSequence({pll_sends}), std::move(branch.g));
        }
        // branch is empty, so transmit the live-out-from-selection vars and pull them out 
        else {
            Block *send_live_vars_to_branch = 
                _generate_send_to_be_sent_from (sel);

            Block *pll_sends = g->graph.blockAllocator().newBlock(Block::makeParBlock());
            
            pll_sends->u_par().branches.push_back(
                g->graph.blockAllocator().newSequence({send_ctrl}));
            
            if (send_live_vars_to_branch) {
                pll_sends->u_par().branches.push_back(
                    g->graph.blockAllocator().newSequence({send_live_vars_to_branch}));
                
                // insert receive blocks in the branch to get the live_vars
                _splice_in_recv_before (branch.seq.startseq->child(), send_live_vars_to_branch);
            }
            
            // insert send blocks at the end of the branch 
            // this is unchaged wrt send_live_vars_to_branch since branch is empty
            Block *send_live_vars_from_branch = 
                _generate_send_to_be_sent_from (sel);

            if (send_live_vars_from_branch) {
                _splice_in_block_between (branch.seq.endseq->parent(),branch.seq.endseq, 
                                            send_live_vars_from_branch);
            }
            
            split->u_select().branches.emplace_back(
                g->graph.blockAllocator().newSequence({pll_sends}), std::move(branch.g));
        
        }
        branch_itr++;
    }

    return split;
    
}
