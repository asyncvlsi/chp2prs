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
        nas.clear();
        _compute_next_aliases (g->graph.m_seq, cbp.first);
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
        tmp.insert({b,id});
        mc_info.insert({id, tmp});
    }
    else 
    {
        mc_info.find(id)->second.insert({b,id});
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
            auto chmap2 = (g->name_from_chan.find(itr2.second))->second;
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

                (mc_info.find(id)->second).find(curr)->second = alias_chan;
            }
            break;
        case StatementType::Receive:
            if (curr->u_basic().stmt.u_receive().chan == id)
            {
                ChanId alias_chan = g->graph.id_pool().makeUniqueChan(g->graph.id_pool().getBitwidth(id));
                var_to_actvar vtoa(s, &g->graph.id_pool());
                ActId *aid = vtoa.chanMap(alias_chan);
                g->name_from_chan.insert({alias_chan, aid});
                
                (mc_info.find(id)->second).find(curr)->second = alias_chan;
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

void MultiChan::_replace_with_alias (Block *b)
{
    fprintf (fp, "\nreplacing with alias..\n");
    Assert (b->type() == BlockType::Basic, "non-basic block");
    Assert ((b->u_basic().stmt.type() == StatementType::Receive) 
        || (b->u_basic().stmt.type() == StatementType::Send), "non-comm. action");

    ChanId chan, alias_chan;

    switch (b->u_basic().stmt.type()) {
    case StatementType::Send:
        chan = b->u_basic().stmt.u_send().chan;
        alias_chan = g->graph.id_pool().makeUniqueChan(g->graph.id_pool().getBitwidth(chan));
        b->u_basic().stmt.u_send().chan = alias_chan;
        break;
    case StatementType::Receive:
        chan = b->u_basic().stmt.u_receive().chan;
        alias_chan = g->graph.id_pool().makeUniqueChan(g->graph.id_pool().getBitwidth(chan));
        b->u_basic().stmt.u_receive().chan = alias_chan;
        break;
    default:
        Assert (false, "wut");
        break;
    }
}

void MultiChan::_compute_next_aliases (Sequence seq, ChanId id)
{
    // TODO...
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

// TODO...
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