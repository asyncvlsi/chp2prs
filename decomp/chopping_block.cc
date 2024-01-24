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

// TODO: Comm. block insertions...
// TODO: Name and name-mapping handling...


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

Sequence ChoppingBlock::_split_sequence_before(Block *b, Sequence parent_seq)
{
    std::vector<Block *> v_block_ptrs;

    Block *itr = parent_seq.startseq->child();
    Block *blk;
    while (itr != b)
    {   
        v_block_ptrs.push_back(itr);
        itr = _splice_out_block(itr);
    }

    Sequence seq_out;
    seq_out = g->graph.blockAllocator().newSequence(v_block_ptrs);

    // fprintf (stdout, "\nexcised subseq start----");
    // print_seq (seq_out);
    // fprintf (stdout, "\nexcised subseq end----\n");

    // v_seqs.push_back(seq_out);
    v_seqs.push_back(_wrap_in_do_loop(seq_out));

    return seq_out;
}

void ChoppingBlock::chop_graph()
{
    // _print_seq (g->graph.m_seq);
    _chop_graph(g->graph.m_seq, 0);
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
        // fprintf (stdout, "\nbasic : ");
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
        if (di->is_breakpoint && curr->parent()->type()!=BlockType::StartSequence)
        {
            // fprintf (stdout, "\nbreaking \n");
            // di_par = (vmap.find(curr->parent()))->second;
            // ChanId chan_id = idpool.makeUniqueChan(di_par->total_bitwidth);
            // auto sendvarexpr = ChpExprSingleRootDag::makeVariableAccess(
            //                 *(di_par->tx_vars.begin()), 
            //                 idpool.getBitwidth(chan_id));
            // new_block = g->graph.blockAllocator().newBlock(Block::makeBasicBlock(
            //                 Statement::makeSend(chan_id, std::move(sendvarexpr))
            //                 ));
            _split_sequence_before (curr, seq);
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
        // fprintf (stdout, "select : %d\n\n", di->is_breakpoint);
        if (di->is_breakpoint && curr->parent()->type()!=BlockType::StartSequence)
        {
            // fprintf (stdout, "\nbreaking \n");
            _split_sequence_before (curr, seq);
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
        // fprintf (stdout, "\ndoloop start\n");
        _chop_graph (curr->u_doloop().branch, 0);
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
    if (!seq.empty())
        _split_sequence_before(curr, seq);
        
    return;
}
