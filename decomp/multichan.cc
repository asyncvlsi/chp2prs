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

#include "multichan.h"
#include "../opt/utils.h"

 std::vector<Sequence> MultiChan::get_auxiliary_procs()
{
    return v_aux;
}

void MultiChan::process_multichans()
{
    _build_multichan_info (g->graph.m_seq);
    _delete_singles ();
    _print_multichan_info ();

    for ( auto cbp : mc_info )
    {
        _update_with_aliases (g->graph.m_seq, cbp.first);
        // TODO: insert_guard_comms for branched programs
        // if (_contains_chan_access(g->graph.m_seq, cbp.first))
        //     fprintf (fp, "found access\n");
        Assert (_contains_chan_access(g->graph.m_seq, cbp.first), "huh?");
        auto aux = _build_aux_process (g->graph.m_seq, cbp.first);
        v_aux.push_back(aux);
    }
    _print_multichan_info ();
}

void MultiChan::_build_multichan_info (Sequence seq)
{
    Block *curr = seq.startseq->child();

    while (curr->type() != BlockType::EndSequence) {
    switch (curr->type()) {
    case BlockType::Basic: {
        switch (curr->u_basic().stmt.type()) {
        case StatementType::Assign:
            break;
        case StatementType::Send:
            _add_chan_blk_pair (curr, curr->u_basic().stmt.u_send().chan);
            break;
        case StatementType::Receive:
            _add_chan_blk_pair (curr, curr->u_basic().stmt.u_receive().chan);
            break;
      }
    }
    break;
      
    case BlockType::Par: {
        for (auto &branch : curr->u_par().branches) {
            _build_multichan_info (branch);
        }
    }
    break;
      
    case BlockType::Select:
        for (auto &branch : curr->u_select().branches) {
            _build_multichan_info (branch.seq);
        }
    break;
      
    case BlockType::DoLoop:
        _build_multichan_info (curr->u_doloop().branch);
        break;
    
    case BlockType::StartSequence:
    case BlockType::EndSequence:
        hassert(false);
        break;
    }
    curr = curr->child();
    }
}

void MultiChan::_add_chan_blk_pair (Block *b, ChanId id)
{
    chan_blk_pair tmp;
    if (!mc_info.contains(id))
    {
        tmp.clear();
        // first alias number is 1, 0 is reserved for special purpose
        tmp.insert({b,{id,1}});
        mc_info.insert({id, tmp});
    }
    else 
    {
        unsigned int curr_no_of_aliases = (mc_info.find(id)->second).size();
        mc_info.find(id)->second.insert({b,{id, curr_no_of_aliases+1}});
    }
}

void MultiChan::_print_multichan_info ()
{
    fprintf (fp, "\n\n----------------\n\n");
    for ( auto itr : mc_info )
    {
        Assert (g->name_from_chan.contains(itr.first), "channel name not found?");
        auto chmap = (g->name_from_chan.find(itr.first))->second;
        char chname[1024];
        chmap->sPrint(chname,1024,NULL,1);
        fprintf (fp, "accesses of chan : %s : ", chname);

        for ( auto itr2 : itr.second )
        {
            Assert (itr2.first->type() == BlockType::Basic, "non-basic block..?");
            auto chmap2 = (g->name_from_chan.find(itr2.second.first))->second;
            char chname2[1024];
            chmap2->sPrint(chname2,1024,NULL,1);
            switch (itr2.first->u_basic().stmt.type()) 
            {
                case StatementType::Receive:
                fprintf (fp, "%s?(varid:%d)  ", chname2, itr2.first->u_basic().stmt.u_receive().var);
                break;

                case StatementType::Send:
                fprintf (fp, "%s!(expr)  ", chname2);
                break;

                default:
                Assert (false, "wut");
                break;
            }
        }
        fprintf (fp,"\n");
    }
    fprintf (fp, "\n----------------\n\n");
}

void MultiChan::_delete_singles ()
{
    multichan_alias_struct mc_info_new;
    for ( auto itr : mc_info )
    {
        auto vec = itr.second;
        if (vec.size() > 1)
        {
            mc_info_new.insert(itr);
        }
    }
    mc_info.clear();
    mc_info = mc_info_new;
}

void MultiChan::_update_with_aliases (Sequence seq, ChanId id)
{
    Block *curr = seq.startseq->child();

    while (curr->type() != BlockType::EndSequence) {
    switch (curr->type()) {
    case BlockType::Basic: {
        switch (curr->u_basic().stmt.type()) {
        case StatementType::Assign:
            break;
        case StatementType::Send:
            if (curr->u_basic().stmt.u_send().chan == id)
            {
                ChanId alias_chan = g->graph.id_pool().makeUniqueChan(g->graph.id_pool().getBitwidth(id));
                var_to_actvar vtoa(s, &g->graph.id_pool());
                ActId *aid = vtoa.chanMap(alias_chan);
                g->name_from_chan.insert({alias_chan, aid});

                (mc_info.find(id)->second).find(curr)->second.first = alias_chan;
            }
            break;
        case StatementType::Receive:
            if (curr->u_basic().stmt.u_receive().chan == id)
            {
                ChanId alias_chan = g->graph.id_pool().makeUniqueChan(g->graph.id_pool().getBitwidth(id));
                var_to_actvar vtoa(s, &g->graph.id_pool());
                ActId *aid = vtoa.chanMap(alias_chan);
                g->name_from_chan.insert({alias_chan, aid});
                
                (mc_info.find(id)->second).find(curr)->second.first = alias_chan;
            }
            break;
      }
    }
    break;
      
    case BlockType::Par: {
        for (auto &branch : curr->u_par().branches) {
            _update_with_aliases (branch, id);
        }
    }
    break;
      
    case BlockType::Select:
        for (auto &branch : curr->u_select().branches) {
            _update_with_aliases (branch.seq, id);
        }
    break;
      
    case BlockType::DoLoop:
        _update_with_aliases (curr->u_doloop().branch, id);
        break;
    
    case BlockType::StartSequence:
    case BlockType::EndSequence:
        hassert(false);
        break;
    }
    curr = curr->child();
    }
}

void MultiChan::_replace_with_alias (Block *b, ChanId alias_chan)
{
    fprintf (fp, "\nreplacing with alias..\n");
    Assert (b->type() == BlockType::Basic, "non-basic block");
    Assert ((b->u_basic().stmt.type() == StatementType::Receive) 
        || (b->u_basic().stmt.type() == StatementType::Send), "non-comm. action");

    // ChanId chan, alias_chan;
    switch (b->u_basic().stmt.type()) {
    case StatementType::Send:
        // chan = b->u_basic().stmt.u_send().chan;
        // alias_chan = g->graph.id_pool().makeUniqueChan(g->graph.id_pool().getBitwidth(chan));
        b->u_basic().stmt.u_send().chan = alias_chan;
        break;
    case StatementType::Receive:
        // chan = b->u_basic().stmt.u_receive().chan;
        // alias_chan = g->graph.id_pool().makeUniqueChan(g->graph.id_pool().getBitwidth(chan));
        b->u_basic().stmt.u_receive().chan = alias_chan;
        break;
    default:
        Assert (false, "wut");
        break;
    }
}

Block* MultiChan::_compute_first_alias_block (Sequence seq, ChanId id, int root)
{
    Block *ret = NULL;

    Block *curr = seq.startseq->child();

    while (curr->type() != BlockType::EndSequence) {
    switch (curr->type()) {
    case BlockType::Basic: {
        switch (curr->u_basic().stmt.type()) {
        case StatementType::Assign:
            break;
        case StatementType::Send:
            if (curr->u_basic().stmt.u_send().chan == id)
            {
                return (mc_info.find(id)->second).find(curr)->first;
            }
            break;
        case StatementType::Receive:
            if (curr->u_basic().stmt.u_receive().chan == id)
            {
                return (mc_info.find(id)->second).find(curr)->first;
            }
            break;
      }
    }
    break;
      
    case BlockType::Par: {
        fatal_error ("not working yet..");
    }
    break;
      
    case BlockType::Select:
        fatal_error ("not working yet..");
    break;
      
    case BlockType::DoLoop:
        ret = _compute_first_alias_block (curr->u_doloop().branch, id, 0);
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

// NOTE : Will probably have to directly build the aux. process as
// i walk through the program tree, to prevent this AST rebuilding ...

// For handling selections etc.
// Gotta implement proper next_alias searching...
#if 0
std::pair<Block *, std::pair<ChanId, unsigned int>> _compute_next_alias (Sequence seq, OptionalChanId id)
{

}
#endif

// Build up the auxiliary multichan handler process
// Currently works only for when all accesses are in the same branch
// W.I.P...
Sequence MultiChan::_build_aux_process (Sequence seq, ChanId id)
{
    int alias_tracker = 1;

    auto alias_var_bw = log_2_round_up((mc_info.find(id)->second).size()+1);
    auto alias_var = g->graph.id_pool().makeUniqueVar(alias_var_bw);
    // auto it = TypeFactory::Factory()->NewInt (s, Type::NONE, 0, const_expr (alias_var_bw));
    // static char buf[100];
    // snprintf (buf, 100, "alias");
    // s->Add(buf, it);
    auto next_alias_var = g->graph.id_pool().makeUniqueVar(alias_var_bw);
    auto data_var = g->graph.id_pool().makeUniqueVar( g->graph.id_pool().getBitwidth(id) );

    // make init. cond. alias := 0 assignment
    Block *init_alias = g->graph.blockAllocator().newBlock(
        Block::makeBasicBlock(Statement::makeAssignment(alias_var,ChpExprSingleRootDag::makeConstant(BigInt(0),alias_var_bw))));
    Assert (init_alias, "huh");

    Block *sm_sel = g->graph.blockAllocator().newBlock(Block::makeSelectBlock());
    
    // first branch - initialize vars -------------------------------
    // will have to update this when selections are supported
    // [ alias=0 -> next_alias := 1 
    // [] ...
    auto alias_access_0 = ChpExprSingleRootDag::makeVariableAccess(alias_var, alias_var_bw);
    IRGuard alias_eq_0 = IRGuard::makeExpression (
                        ChpExprSingleRootDag::makeBinaryOp(IRBinaryOpType::EQ,
                        std::make_unique<ChpExprSingleRootDag>(std::move(alias_access_0)),
                        std::make_unique<ChpExprSingleRootDag>(
                            ChpExprSingleRootDag::makeConstant(BigInt(0),alias_var_bw)
                        )));

    auto first_alias_blk = _compute_first_alias_block (seq, id, 1);

    Block *next_alias_eq_1 = g->graph.blockAllocator().newBlock(
                            Block::makeBasicBlock(Statement::makeAssignment(
                            next_alias_var,ChpExprSingleRootDag::makeConstant(BigInt(alias_tracker),alias_var_bw))));
    
    sm_sel->u_select().branches.emplace_back(g->graph.blockAllocator().newSequence({next_alias_eq_1}), std::move(alias_eq_0));
    // first branch - initialize vars -------------------------------


    chan_blk_pair cbp = mc_info.find(id)->second;

    Assert (first_alias_blk, "unsupported rn");
    // iterate and build state machine ------------------------------
    // [] alias=a_i -> chan_i?x ; next_alias := a_j 
    Block *curr = NULL;
    while ((curr = _compute_first_alias_block(seq, id, 1)))
    {
        auto next_alias_chan = cbp.find(curr)->second.first;
        _replace_with_alias (curr, next_alias_chan);

        auto alias_access_i = ChpExprSingleRootDag::makeVariableAccess(alias_var, alias_var_bw);
        IRGuard alias_eq_i = IRGuard::makeExpression (
                            ChpExprSingleRootDag::makeBinaryOp(IRBinaryOpType::EQ,
                            std::make_unique<ChpExprSingleRootDag>(std::move(alias_access_i)),
                            std::make_unique<ChpExprSingleRootDag>(
                                ChpExprSingleRootDag::makeConstant(BigInt(alias_tracker),alias_var_bw)
                            )));

        Block *recv_first_alias = g->graph.blockAllocator().newBlock(
                                Block::makeBasicBlock(Statement::makeReceive(next_alias_chan, data_var)));

        Sequence br1;
        Block *update_next_alias_var = NULL;
        if (_compute_first_alias_block(seq, id, 1)) {
            update_next_alias_var = g->graph.blockAllocator().newBlock(
                                    Block::makeBasicBlock(Statement::makeAssignment(
                                    next_alias_var,ChpExprSingleRootDag::makeConstant(BigInt(alias_tracker+1),1))));
        }
        // got all aliases, reset to zero and start again
        else {
            update_next_alias_var = g->graph.blockAllocator().newBlock(
                                    Block::makeBasicBlock(Statement::makeAssignment(
                                    next_alias_var,ChpExprSingleRootDag::makeConstant(BigInt(0),1))));
        }

        br1 = g->graph.blockAllocator().newSequence({recv_first_alias, update_next_alias_var});
        sm_sel->u_select().branches.emplace_back(br1, std::move(alias_eq_i));
        alias_tracker++;
    }
    // iterate and build state machine ------------------------------

    // [ alias != 0 -> Orig!x [] else -> skip ] ---------------------
    auto alias_access_n = ChpExprSingleRootDag::makeVariableAccess(alias_var, alias_var_bw);
    IRGuard alias_neq_0 = IRGuard::makeExpression (
                            ChpExprSingleRootDag::makeBinaryOp(IRBinaryOpType::NE,
                            std::make_unique<ChpExprSingleRootDag>(std::move(alias_access_n)),
                            std::make_unique<ChpExprSingleRootDag>(
                                ChpExprSingleRootDag::makeConstant(BigInt(0),alias_var_bw)
                            )));

    Block *send = g->graph.blockAllocator().newBlock(Block::makeBasicBlock(Statement::makeSend(
                    id, ChpExprSingleRootDag::makeVariableAccess(data_var, g->graph.id_pool().getBitwidth(id)) )));

    Block *conditional_send = g->graph.blockAllocator().newBlock(Block::makeSelectBlock());
    conditional_send->u_select().branches.emplace_back(g->graph.blockAllocator().newSequence({send}), std::move(alias_neq_0));
    conditional_send->u_select().branches.emplace_back(g->graph.blockAllocator().newSequence({}), IRGuard::makeElse());
    // [ alias != 0 -> Orig!x [] else -> skip ] ---------------------

    // alias := next_alias ------------------------------------------
    Block *alias_update = g->graph.blockAllocator().newBlock(
                        Block::makeBasicBlock(Statement::makeAssignment(alias_var,
                        ChpExprSingleRootDag::makeVariableAccess(next_alias_var, alias_var_bw))));
    // alias := next_alias ------------------------------------------

    Sequence aux_core = g->graph.newSequence({sm_sel, conditional_send, alias_update});
    Sequence aux = g->graph.newSequence({init_alias, _wrap_in_do_loop (aux_core)});
    return aux;
}

bool MultiChan::_contains_chan_access (Sequence seq, ChanId id)
{
    bool ret = false;
    Block *curr = seq.startseq->child();

    while (curr->type() != BlockType::EndSequence) {
    switch (curr->type()) {
    case BlockType::Basic: {
        switch (curr->u_basic().stmt.type()) {
        case StatementType::Assign:
            break;
        case StatementType::Send:
            if (curr->u_basic().stmt.u_send().chan == id)
                return true;
            break;
        case StatementType::Receive:
            if (curr->u_basic().stmt.u_receive().chan == id)
                return true;
            break;
      }
    }
    break;
      
    case BlockType::Par: {
        for (auto &branch : curr->u_par().branches) {
            ret = ret || _contains_chan_access (branch, id);
        }
    }
    break;
      
    case BlockType::Select:
        for (auto &branch : curr->u_select().branches) {
            ret = ret || _contains_chan_access (branch.seq, id);
        }
    break;
      
    case BlockType::DoLoop:
        ret = ret || _contains_chan_access (curr->u_doloop().branch, id);
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

Block *MultiChan::_wrap_in_do_loop (Sequence seq)
{
    Block *doloop = g->graph.blockAllocator().newBlock(Block::makeDoLoopBlock());
    doloop->u_doloop().branch = seq;

    doloop->u_doloop().guard = (ChpExprSingleRootDag::of_expr(
                                    ChpExpr::makeConstant(BigInt{1}, 1)));
    
    return doloop;
}

// TODO...
// for branched programs
void MultiChan::_insert_guard_comm (Block *b)
{
    Assert ((b->type() == BlockType::Select) || (b->type() == BlockType::DoLoop), "not select or loop?");
    switch (b->type()) {

    case BlockType::Select:
        break;

    case BlockType::DoLoop:
        break;

    default:
        Assert (false, "wut");
        break;
    }
}