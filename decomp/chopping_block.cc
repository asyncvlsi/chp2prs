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

Block *ChoppingBlock::_splice_in_recv_before(Block *bb, Block *send)
{
    if (bb->type() == BlockType::StartSequence)
        return NULL;
    if (bb->type() == BlockType::EndSequence)
        return NULL;

    decomp_info_t *di = (vmap.find(bb))->second;

    if (di->tx_vars.size() == 0)
    {
        return NULL;
    }

    ChanId chan_id = send->u_basic().stmt.u_send().chan;

    if (di->tx_vars.size() == 1)
    {
        VarId var_id = *di->tx_vars.begin();
        OptionalVarId ovid(var_id);

        Block *receive = g->graph.blockAllocator().newBlock(
                Block::makeBasicBlock(Statement::makeReceive(chan_id, ovid)));
        _splice_in_block_between (bb->parent(), bb, receive);

        // test
        vmap.insert({receive, vmap.find(bb)->second});

        return receive;
    }

    VarId var_concat = g->graph.id_pool().makeUniqueVar(di->total_bitwidth, false);
    OptionalVarId ovid(var_concat);
    // C?x
    Block *receive = g->graph.blockAllocator().newBlock(
            Block::makeBasicBlock(Statement::makeReceive(chan_id, ovid)));
    _splice_in_block_between (bb->parent(), bb, receive);
    
    // test
    vmap.insert({receive, vmap.find(bb)->second});

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
    
    _splice_in_block_between (receive, bb, parallel);
    return parallel;
}

Block *ChoppingBlock::_generate_send(Block *bb)
{
    if (bb->type() == BlockType::StartSequence)
        return NULL;
    if (bb->type() == BlockType::EndSequence)
        return NULL;

    // fprintf (stdout, "\nhere multi\n ");
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

    return send;
}

void ChoppingBlock::_split_sequence_before(Block *b, Sequence parent_seq, int root)
{
    std::vector<Block *> v_block_ptrs;

    Block *itr = parent_seq.startseq->child();
    int flag = 0;
    // need to generate the incoming vars for this block..
    // splice in the send for this block at its location
    // generate the recv
    // basically the same, again
    if (root != 1 && (itr->type() != BlockType::EndSequence))
    {
        fprintf (stdout, "\n\nhere: %d\n\n", itr->type());
        Block *sr = _generate_send (itr);
        _splice_in_block_between (itr->parent(), itr, sr);
        fprintf (stdout, "\n\nhere now: %d\n\n", itr->type());
        _splice_in_recv_before (itr, sr);
        itr = parent_seq.startseq->child();
        flag = 1;
        // itr = itr->child();
        Assert (itr, "no block");
        fprintf (stdout, "\n\nhere again: %d\n\n", itr->child()->type());
        // fprintf (stdout, "\n\n\n");
        // return;
    }
    // if (flag == 1)
    //     itr = itr->child();

    while (itr != b)
    {   
        v_block_ptrs.push_back(itr);
        itr = _splice_out_block(itr);
    }

    Block *send = _generate_send (b);
    if (send)
    {   
        v_block_ptrs.push_back(send);
        _splice_in_recv_before (b, send);

    }
    Sequence seq_out;
    seq_out = g->graph.blockAllocator().newSequence(v_block_ptrs);

    v_seqs.push_back(_wrap_in_do_loop(seq_out));

    return;
}

void ChoppingBlock::chop_graph()
{
    // _print_seq (g->graph.m_seq);
    _chop_graph(g->graph.m_seq, 1);
    // for (auto v_seq : v_seqs)
    // {
    //     print_seq (v_seq);
    //     fprintf (stdout, "\n-----------------\n");
    // }
}

void ChoppingBlock::_chop_graph(Sequence seq, int root)
{
    Block *curr = seq.startseq->child();
    decomp_info *di, *di_par;
    Block *new_block;
    Block *tmp;

    while (curr->type() != BlockType::EndSequence) {
    switch (curr->type()) {
    case BlockType::Basic: {
        if (vmap.contains(curr))
        {
        di = (vmap.find(curr))->second;
        fprintf (stdout, "\nbasic : ");
        switch (curr->u_basic().stmt.type()) {
        case StatementType::Send:
        // fprintf (stdout, "send : %d\n\n", di->is_breakpoint);
            break;
        case StatementType::Assign:
        // fprintf (stdout, "assign : %d\n\n", di->is_breakpoint);
            break;
        case StatementType::Receive:
        // fprintf (stdout, "recv : %d\n\n", di->is_breakpoint);
            break;
        }
        if (di->break_before && curr->parent()->type()!=BlockType::StartSequence)
        {
            fprintf (stdout, "\nbreaking basic\n");
            _split_sequence_before (curr, seq, root);
        }
        }
        else
        {
            fatal_error ("huh");
        }
    }
    break;
      
    case BlockType::Par: {
        // fatal_error ("working on par...");
        for (auto &branch : curr->u_par().branches) {
            _chop_graph (branch, 0);
        }
    }
    break;
      
    case BlockType::Select:
        if (vmap.contains(curr))
        {
        di = (vmap.find(curr))->second;
        fprintf (stdout, "select : %d\n\n", di->break_before);
        if (di->break_before && curr->parent()->type()!=BlockType::StartSequence)
        {
            fprintf (stdout, "\nbreaking sel\n");
            _split_sequence_before (curr, seq, root);
        }
        }
        else
        {
            fatal_error ("huh");
        }
        for (auto &branch : curr->u_select().branches) {
            _chop_graph (branch.seq, 0);
        }
    break;
      
    case BlockType::DoLoop:
        if (root != 1)
            fatal_error ("excise internal loops please");

        // fprintf (stdout, "\ndoloop start\n");
        _chop_graph (curr->u_doloop().branch, 1);
        if (curr->u_doloop().branch.empty())
            return;
        // fprintf (stdout, "\ndoloop end");
        break;
    
    case BlockType::StartSequence:
    case BlockType::EndSequence:
        hassert(false);
        break;
    }
    curr = curr->child();
    }
    //tail of seq
    if (!seq.empty() && (root==1))
    {
        fprintf (stdout, "\nbreaking tail\n");
        _split_sequence_before(curr, seq, root);
    }
    return;
}
