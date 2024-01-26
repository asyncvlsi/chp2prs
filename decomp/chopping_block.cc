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

Block *ChoppingBlock::_generate_send(Block *bb)
{
    if (bb->type() == BlockType::StartSequence)
        return NULL;
    if (bb->type() == BlockType::EndSequence)
        return NULL;

    hassert (vmap.contains(bb));
    decomp_info_t *di = (vmap.find(bb))->second;

    if (di->tx_vars.size() == 0)
    {
        return NULL;
    }

    ChanId chan_id = g->graph.id_pool().makeUniqueChan(di->total_bitwidth, false);
    var_to_actvar vtoa(s, &g->graph.id_pool());
    ActId *id = vtoa.chanMap(chan_id);
    g->name_from_chan.insert({chan_id, id});

    if (di->tx_vars.size() == 1)
    {
        VarId var_id = *di->tx_vars.begin();
        Block *send =
            g->graph.blockAllocator().newBlock(Block::makeBasicBlock(Statement::makeSend(
                chan_id, ChpExprSingleRootDag::makeVariableAccess(var_id, di->total_bitwidth))));
        vmap.insert({send, di});
        return send;
    }

    ChpExprSingleRootDag conc_vars;

    for ( auto var : di->tx_vars )
    {
        if (var == *di->tx_vars.begin())
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

    vmap.insert({send, di});
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
    std::vector<Block *> v_block_ptrs = _split_sequence_from_to (b_start, b_end_plus_1);
    
    Block *send = _generate_send (b_end_plus_1);
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

    while (curr->type() != BlockType::EndSequence) {
    switch (curr->type()) {
    case BlockType::Basic: {
        if (vmap.contains(curr))
        {
            di = (vmap.find(curr))->second;
            // fprintf (stdout, "\nbasic : ");
            switch (curr->u_basic().stmt.type()) {
                case StatementType::Send:
                    break;
                case StatementType::Assign:
                    break;
                case StatementType::Receive:
                    break;
            }
            if (di->break_before)
            {
                Block *send = _build_sequence (seq.startseq->child(), curr);
                if (send)
                    _splice_in_recv_before (curr, send);
            }
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
        fatal_error ("working on select");
        for (auto &branch : curr->u_select().branches) {
            _chop_graph (branch.seq, 0);
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
        _build_sequence (seq.startseq->child(), seq.endseq);
    }
    return;
}

void ChoppingBlock::_splice_in_recv_before(Block *bb, Block *send)
{
    if (bb->type() == BlockType::StartSequence)
        return;
    if (bb->type() == BlockType::EndSequence)
        return;

    Sequence recv_plus_maybe_assigns = _generate_recv_and_maybe_assigns (send);
    if (!recv_plus_maybe_assigns.empty())
    {
        g->graph.spliceInSequenceBefore(bb, recv_plus_maybe_assigns);
    }
    return;
}

Sequence ChoppingBlock::_generate_recv_and_maybe_assigns (Block *send)
{
    hassert (vmap.contains(send));
    decomp_info_t *di = (vmap.find(send))->second;

    if (di->tx_vars.size() == 0)
    {
        return g->graph.blockAllocator().newSequence({});
    }

    ChanId chan_id = send->u_basic().stmt.u_send().chan;

    if (di->tx_vars.size() == 1)
    {
        VarId var_id = *di->tx_vars.begin();
        OptionalVarId ovid(var_id);

        Block *receive = g->graph.blockAllocator().newBlock(
                Block::makeBasicBlock(Statement::makeReceive(chan_id, ovid)));
        // test
        vmap.insert({receive, di});
        // test

        return g->graph.blockAllocator().newSequence({receive});
    }

    VarId var_concat = g->graph.id_pool().makeUniqueVar(di->total_bitwidth, false);
    OptionalVarId ovid(var_concat);
    // C?x
    Block *receive = g->graph.blockAllocator().newBlock(
            Block::makeBasicBlock(Statement::makeReceive(chan_id, ovid)));
    
    // test
    vmap.insert({receive, di});
    // test

    //  x1 = x{0..i} , x2 = x{i+1..j} ...
    // make the assign blocks, parallel them all

    int range_ctr = di->total_bitwidth;
    Block *parallel = g->graph.blockAllocator().newBlock(Block::makeParBlock());
    for ( auto var : di->tx_vars )
    {
        VarId vi = var;
        int width = g->graph.id_pool().getBitwidth(vi);
        // vi := v_concat{i+w..i}
        Block *assign = g->graph.blockAllocator().newBlock(Block::makeBasicBlock(
                    Statement::makeAssignment(vi, 
                        ChpExprSingleRootDag::makeBitfield(
                            std::make_unique<ChpExprSingleRootDag>(
                                ChpExprSingleRootDag::makeVariableAccess(
                                    var_concat, di->total_bitwidth)),
                            range_ctr-1, range_ctr-width)
                                            )));

        parallel->u_par().branches.push_back(
                g->graph.blockAllocator().newSequence({assign}));
        range_ctr -= (width);
    }

    return g->graph.blockAllocator().newSequence({receive, parallel});
}
