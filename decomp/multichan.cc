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
        alias_number = 0;
        _update_with_aliases (g->graph.m_seq, cbp.first);
        // TODO: insert_guard_comms for branched programs
        Assert (_contains_chan_access(g->graph.m_seq, cbp.first), "huh?");

        // alias_number = cbp.second.size();
        Assert(alias_number == cbp.second.size(), "hmm");
        auto final_state = _build_state_table (g->graph.m_seq, cbp.first, 0);
        _st.push_back (StateRow(final_state, 0));
        _print_state_table (_st);

        auto aux = _build_aux_process_new (_st, cbp.first);
        v_aux.push_back(aux);
        _st.clear();
    }
    _print_multichan_info ();
}

int MultiChan::_gen_alias_number ()
{
    alias_number++;
    return alias_number;
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
                (mc_info.find(id)->second).find(curr)->second.second = _gen_alias_number();
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
                (mc_info.find(id)->second).find(curr)->second.second = _gen_alias_number();
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

Block* MultiChan::_compute_next_alias_block (Sequence seq, ChanId id, int root)
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
        for (auto &branch : curr->u_par().branches) {
            ret = _compute_next_alias_block (branch, id, 0);
            if (ret) return ret;
        }
    }
    break;
      
    case BlockType::Select:
        fatal_error("wip");
        // for (auto &branch : curr->u_select().branches) {
        //     ret = _compute_next_alias_block (branch.seq, id, 0);
        //     if (ret) return ret;
        // }
    break;
      
    case BlockType::DoLoop:
        ret = _compute_next_alias_block (curr->u_doloop().branch, id, 0);
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

bool MultiChan::_seq_contains_block (Block *b, Sequence seq)
{
    Block *curr = seq.startseq->child();
    while (curr->type() != BlockType::EndSequence) {
        if (curr == b) return true;
        curr = curr->child();
    }
    return false;
}

int MultiChan::_build_state_table (Sequence seq, ChanId id, int curr_state)
{
    Block *curr = seq.startseq->child();
    bool alias_hit = false;
    std::vector<int> ns_i, ns_o; 

    while (curr->type() != BlockType::EndSequence) {
    switch (curr->type()) {
    case BlockType::Basic: {
        switch (curr->u_basic().stmt.type()) {
        case StatementType::Assign:
            break;
        case StatementType::Send:
            if (curr->u_basic().stmt.u_send().chan == id)
            {
                auto next_state = (mc_info.find(id)->second).find(curr)->second.second;
                _st.push_back(StateRow(curr_state, next_state));
                curr_state = next_state;
                alias_hit = true;
            }
            break;
        case StatementType::Receive:
            if (curr->u_basic().stmt.u_receive().chan == id)
            {   
                auto next_state = (mc_info.find(id)->second).find(curr)->second.second;
                _st.push_back(StateRow(curr_state, next_state));
                curr_state = next_state;
                alias_hit = true;
            }
            break;
      }
    }
    break;
      
    case BlockType::Par: {
        fprintf (fp, "\n\nSTILL WORKING ON THIS\n\n");
        // fatal_error ("wip");
        // for (auto &branch : curr->u_par().branches) {
        //     _build_state_table (branch, id, curr_state);
        // }
    }
    break;
      
    case BlockType::Select: {
        // check if this select needs to be processed at all
        // i.e. if there's a chan access somewhere in it
        bool process = false;
        for (auto &branch : curr->u_select().branches) {
            process |= _contains_chan_access(branch.seq, id);
        }

        if (process) {
            ns_i.clear();
            ns_o.clear();
            auto merge_state = _gen_alias_number();
            fprintf (fp, "\n merge state : %d \n", merge_state);
            for (auto &branch : curr->u_select().branches) {
                auto next_state = _gen_alias_number();
                ns_i.push_back(next_state);
                auto out_state = _build_state_table (branch.seq, id, next_state);
                _st.push_back(StateRow(out_state, merge_state));
            }
            // fprintf (fp, "\n got here 1 dawg \n");
            _st.push_back(StateRow(curr_state, ns_i, curr));
            // fprintf (fp, "\n got here 2 dawg \n");
            curr_state = merge_state;
        }
    }
    break;
      
    case BlockType::DoLoop:
        curr_state = _build_state_table (curr->u_doloop().branch, id, curr_state);
        break;
    
    case BlockType::StartSequence:
    case BlockType::EndSequence:
        hassert(false);
        break;
    }
    curr = curr->child();
    }
    // if (!alias_hit) {
    //     auto next_state = _gen_alias_number();
    //     _st.push_back(StateRow(curr_state, next_state));
    //     curr_state = next_state;
    // }
    return curr_state;
}

void MultiChan::_print_state_table (StateTable st)
{   
    fprintf (fp, "\n-------------------- \n");
    fprintf (fp, "\n--- state table ---- \n");
    for (auto sr : st)
    {
        fprintf (fp, "\n-------------------- \n");
        switch (sr.c) {
            case Cond::True:
                fprintf(fp, "%d : true : %d", sr.curr, sr.nexts.front());
            break;
            case Cond::False:
                fatal_error ("brr");
            break;
            case Cond::Guard:
                fprintf(fp, "%d : guard : (\n", sr.curr);
                print_chp_block (std::cout, sr.sel, 0);
                fprintf(fp, " \n) : ");
                for ( auto x : sr.nexts )
                {
                    fprintf(fp, "%d, ", x);
                }
            break;
        }
        fprintf (fp, "\n");
    }
    fprintf (fp, "\n-------------------- \n");
}

// NOTE: From one block, compute all next reachable aliases
// and build an expr for the next_alias assignment... 
// next_alias := (g1=0)? a1 : (g1=1)? a2 : ...
// send out a ray/walker from the current block and find all terminal points
// if you hit end of program on some branch, set next_alias to 0

// ^ implementing this is a nightmare, gonna do state transition table

// Build up the auxiliary multichan handler process
// W.I.P...
Sequence MultiChan::_build_aux_process_new (StateTable s, ChanId id)
{
    auto alias_var_bw = log_2_round_up(s.size());
    auto alias_var = g->graph.id_pool().makeUniqueVar(alias_var_bw);

    auto next_alias_var = g->graph.id_pool().makeUniqueVar(alias_var_bw);
    auto data_var = g->graph.id_pool().makeUniqueVar( g->graph.id_pool().getBitwidth(id) );

    // make init. cond. alias := 0 assignment
    Block *init_alias = g->graph.blockAllocator().newBlock(
        Block::makeBasicBlock(Statement::makeAssignment(alias_var,ChpExprSingleRootDag::makeConstant(BigInt(0),alias_var_bw))));
    Assert (init_alias, "huh");

    Block *sm_sel = g->graph.blockAllocator().newBlock(Block::makeSelectBlock());

    // iterate and build state machine ------------------------------
    // [] alias=a_i -> chan_i?x ; next_alias := a_j 
    Block *curr = NULL;
    for ( auto sr : s )
    {
        bool valid_alias = false;
        if (sr.curr > 0 && sr.curr <= alias_number) // if valid receiving alias
        { valid_alias = true; } 

        Assert (sr.c == Cond::True, "working on sels");

        auto alias_access_i = ChpExprSingleRootDag::makeVariableAccess(alias_var, alias_var_bw);
        IRGuard alias_eq_i = IRGuard::makeExpression (
                            ChpExprSingleRootDag::makeBinaryOp(IRBinaryOpType::EQ,
                            std::make_unique<ChpExprSingleRootDag>(std::move(alias_access_i)),
                            std::make_unique<ChpExprSingleRootDag>(
                                ChpExprSingleRootDag::makeConstant(BigInt(sr.curr),alias_var_bw)
                            )));

        Block *blk = NULL;
        Block *recv_alias = NULL;
        if (valid_alias) // if valid receiving alias
        {
            blk = _find_alias_block (id, sr.curr);
            auto alias_chan = (mc_info.find(id)->second).find(blk)->second.first;
            _replace_with_alias (blk, alias_chan);
            recv_alias = g->graph.blockAllocator().newBlock(
                                Block::makeBasicBlock(Statement::makeReceive(alias_chan, data_var)));
        }

        Sequence br1;
        Block *update_next_alias_var = g->graph.blockAllocator().newBlock(
                                    Block::makeBasicBlock(Statement::makeAssignment(
                                    next_alias_var,ChpExprSingleRootDag::makeConstant(BigInt(sr.nexts.front()),1))));

        if (valid_alias) {
            br1 = g->graph.blockAllocator().newSequence({recv_alias, update_next_alias_var});
        }
        else {
            br1 = g->graph.blockAllocator().newSequence({update_next_alias_var});
        }
        sm_sel->u_select().branches.emplace_back(br1, std::move(alias_eq_i));
    }
    // iterate and build state machine ------------------------------

    // [ OR(alias==receiving_state) -> Orig!x [] else -> skip ] -----
    auto send_guard = _build_send_guard (alias_var, alias_var_bw, alias_number);

    Block *send = g->graph.blockAllocator().newBlock(Block::makeBasicBlock(Statement::makeSend(
                    id, ChpExprSingleRootDag::makeVariableAccess(data_var, g->graph.id_pool().getBitwidth(id)) )));

    Block *conditional_send = g->graph.blockAllocator().newBlock(Block::makeSelectBlock());
    conditional_send->u_select().branches.emplace_back(g->graph.blockAllocator().newSequence({send}), std::move(send_guard));
    conditional_send->u_select().branches.emplace_back(g->graph.blockAllocator().newSequence({}), IRGuard::makeElse());
    // [ OR(alias==receiving_state) -> Orig!x [] else -> skip ] -----

    // alias := next_alias ------------------------------------------
    Block *alias_update = g->graph.blockAllocator().newBlock(
                        Block::makeBasicBlock(Statement::makeAssignment(alias_var,
                        ChpExprSingleRootDag::makeVariableAccess(next_alias_var, alias_var_bw))));
    // alias := next_alias ------------------------------------------

    Sequence aux_core = g->graph.newSequence({sm_sel, conditional_send, alias_update});
    Sequence aux = g->graph.newSequence({init_alias, _wrap_in_do_loop (aux_core)});
    return aux;
}

IRGuard MultiChan::_build_send_guard (VarId v, int bw, int n)
{
    auto ret = ChpExprSingleRootDag::makeConstant(BigInt{0}, 1);
    for (int i = 1 ; i<=n ; i++)
    {
        auto alias_access = ChpExprSingleRootDag::makeVariableAccess(v, bw);
        auto alias_eq_i = ChpExprSingleRootDag::makeBinaryOp(IRBinaryOpType::EQ,
                            std::make_unique<ChpExprSingleRootDag>(std::move(alias_access)),
                            std::make_unique<ChpExprSingleRootDag>(
                                ChpExprSingleRootDag::makeConstant(BigInt(i),bw)
                            ));
        ret =   ChpExprSingleRootDag::makeBinaryOp(IRBinaryOpType::Or,
                std::make_unique<ChpExprSingleRootDag>(std::move(ret)),
                std::make_unique<ChpExprSingleRootDag>(std::move(alias_eq_i))
                );
    }
    return IRGuard::makeExpression(std::move(ret));
}

// this is a bad way of doing it - gotta fix this later
Block *MultiChan::_find_alias_block (ChanId id, unsigned int alias_i)
{
    auto cbp = mc_info.find(id)->second;
    for ( auto x : cbp )
    {
        if (x.second.second == alias_i)
            return x.first;
    }
    Assert (false, "shouldn't have gotten here");
    return NULL;
}

bool MultiChan::_contains_chan_access_shallow (Sequence seq, ChanId id)
{
    Block *curr = seq.startseq->child();
    while (curr->type() != BlockType::EndSequence) {
        if (curr->type() == BlockType::Basic)
        {
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
        curr = curr->child();
    }
    return false;
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