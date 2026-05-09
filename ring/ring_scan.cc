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
 *************************************************************************
 */

#include "ring_scan.h"

void ScanInsertion::insert_scan_points()
{   
    // fprintf(stdout, "hi from scan inserter");
    _insert_scan_points(_c);
    // fprintf(stdout, "\n inside SI\n");
    // chp_print(stdout, _c);
    // fprintf(stdout, "\n inside SI\n");
}

int ScanInsertion::_gen_scan_port_idx ()
{
    return scan_port_ctr++;
}

void ScanInsertion::_insert_scan_point (act_chp_lang_t *&c)
{
    // fprintf(stdout, "\n inside SI fn\n");
    // chp_print(stdout, _c);
    // fprintf(stdout, "\n inside SI fn\n");
    if (static_breakpt_markers.count(c->label)) {
        c->label = NULL;
        _insert_static_breakpoint(c);
    }
    else if (dynamic_breakpt_markers.count(c->label)) {
        c->label = NULL;
        _insert_dynamic_breakpoint(c);
    }
    else if (static_setpt_markers.count(c->label)) {
        c->label = NULL;
        _insert_static_setpoint(c);
    }
    else if (dynamic_setpt_markers.count(c->label)) {
        c->label = NULL;
        _insert_dynamic_setpoint(c);
    }
    else if (static_getpt_markers.count(c->label)) {
        c->label = NULL;
        _insert_static_getpoint(c);
    }
    else if (dynamic_getpt_markers.count(c->label)) {
        c->label = NULL;
        _insert_dynamic_getpoint(c);
    }
}

void ScanInsertion::_insert_scan_points (act_chp_lang_t *&c)
{
    if (!c) return;

    if (c->label) {
        _insert_scan_point (c);
    }

    switch (c->type) {
    case ACT_CHP_COMMALOOP:
    case ACT_CHP_SEMILOOP:
        fatal_error ("Replication loops should've been removed..");
        break;
        
    case ACT_CHP_COMMA:
    case ACT_CHP_SEMI:
        for (listitem_t *li = list_first (c->u.semi_comma.cmd); li; li = list_next (li)) 
        {
            act_chp_lang_t *stmt = (act_chp_lang_t *)(list_value(li));
            _insert_scan_points (stmt);
        }
        break;

    case ACT_CHP_LOOP:
    case ACT_CHP_DOLOOP:
    {
        act_chp_gc_t *gc = c->u.gc;
        _insert_scan_points (gc->s);
        break;
    }
        
    case ACT_CHP_SELECT:
    case ACT_CHP_SELECT_NONDET:
        for (act_chp_gc_t *gc = c->u.gc ; gc ; gc = gc->next)
        {
            _insert_scan_points (gc->s);
        }
        break;
        
    case ACT_CHP_SKIP:
    case ACT_CHP_ASSIGN:
    case ACT_CHP_ASSIGNSELF:
    case ACT_CHP_RECV:
    case ACT_CHP_SEND:
        if (c->label) {
            _insert_scan_point (c);
        }
        break;
        
    case ACT_CHP_FUNC:
    case ACT_CHP_HOLE: /* to support verification */
    case ACT_CHP_MACRO:
    case ACT_HSE_FRAGMENTS:
        break;

    default:
        fatal_error ("Unknown type");
        break;
    }
    return;
}

void ScanInsertion::print_new_ports(FILE *fp)
{
    fprintf(fp, "// No. of New Ports : %d \n", list_length(new_ports));
    for (listitem_t *li = list_first(new_ports); li ; li = li->next)
    {
        int pos = list_ivalue(li);
        auto it = _p->getPortType(pos);
        auto nm = _p->getPortName(pos);
        fprintf(fp, "\n// new port : ");
        it->Print(fp);
        fprintf(fp, " %s", nm);
    }
}

list_t *ScanInsertion::get_new_ports()
{
    return new_ports;
}

void ScanInsertion::_insert_dynamic_setpoint(act_chp_lang_t *&c)
{
    Assert (c->type==ACT_CHP_ASSIGN, "Set points only at assignments!");
    ActId *var = c->u.assign.id;
    InstType *it = _p->CurScope()->FullLookup(var, NULL);
    int bw = TypeFactory::totBitWidth(it);

    std::string new_chan_name = _add_chan_port(bw);
    ActId *chan_id = new ActId (new_chan_name.c_str());
    std::string new_bool_name = _add_bool_port();
    ActId *bool_id = new ActId (new_bool_name.c_str());

    act_chp_lang_t *recv = _make_recv(chan_id, var);

    act_chp_lang_t *sel = _make_nds_template(bool_id);
    sel->u.gc->s = chp_expand(c, ActNamespace::Global(), _p->CurScope());
    sel->u.gc->next->s = recv;

    c->type = ACT_CHP_SELECT_NONDET;
    c->u.gc = sel->u.gc;
}

void ScanInsertion::_insert_static_setpoint(act_chp_lang_t *&c)
{
    Assert (c->type==ACT_CHP_ASSIGN, "Set points only at assignments!");
    ActId *var = c->u.assign.id;
    InstType *it = _p->CurScope()->FullLookup(var, NULL);
    int bw = TypeFactory::totBitWidth(it);

    std::string new_chan_name = _add_chan_port(bw);
    ActId *chan_id = new ActId (new_chan_name.c_str());
    std::string new_bool_name = _add_bool_port();
    ActId *bool_id = new ActId (new_bool_name.c_str());

    act_chp_lang_t *recv = _make_recv(chan_id, var);
    act_chp_lang_t *c_dup = chp_expand(c, ActNamespace::Global(), _p->CurScope());

    list_t *ll = list_new();
    list_append(ll, c_dup);
    list_append(ll, recv);

    c->type = ACT_CHP_SEMI;
    c->u.semi_comma.cmd = ll;
}

void ScanInsertion::_insert_dynamic_getpoint(act_chp_lang_t *&c)
{
    Assert (c->type==ACT_CHP_ASSIGN, "Set points only at assignments!");
    ActId *var = c->u.assign.id;
    InstType *it = _p->CurScope()->FullLookup(var, NULL);
    int bw = TypeFactory::totBitWidth(it);

    std::string new_chan_name = _add_chan_port(bw);
    ActId *chan_id = new ActId (new_chan_name.c_str());
    std::string new_bool_name = _add_bool_port();
    ActId *bool_id = new ActId (new_bool_name.c_str());

    act_chp_lang_t *send = _make_send(chan_id, var);
    act_chp_lang_t *skip = _make_skip();

    act_chp_lang_t *sel = _make_nds_template(bool_id);
    sel->u.gc->s = skip;
    sel->u.gc->next->s = send;

    act_chp_lang_t *c_dup = chp_expand(c, ActNamespace::Global(), _p->CurScope());

    list_t *ll = list_new();
    list_append(ll, c_dup);
    list_append(ll, sel);

    c->type = ACT_CHP_SEMI;
    c->u.semi_comma.cmd = ll;
}

void ScanInsertion::_insert_static_getpoint(act_chp_lang_t *&c)
{
    Assert (c->type==ACT_CHP_ASSIGN, "Set points only at assignments!");
    ActId *var = c->u.assign.id;
    InstType *it = _p->CurScope()->FullLookup(var, NULL);
    int bw = TypeFactory::totBitWidth(it);

    std::string new_chan_name = _add_chan_port(bw);
    ActId *chan_id = new ActId (new_chan_name.c_str());
    std::string new_bool_name = _add_bool_port();
    ActId *bool_id = new ActId (new_bool_name.c_str());
    
    act_chp_lang_t *send = _make_send(chan_id, var);
    act_chp_lang_t *c_dup = chp_expand(c, ActNamespace::Global(), _p->CurScope());

    list_t *ll = list_new();
    list_append(ll, c_dup);
    list_append(ll, send);

    c->type = ACT_CHP_SEMI;
    c->u.semi_comma.cmd = ll;
}

void ScanInsertion::_insert_dynamic_breakpoint(act_chp_lang_t *&c)
{
    std::string new_chan_name = _add_chan_port(1);
    ActId *chan_id = new ActId (new_chan_name.c_str());
    std::string new_bool_name = _add_bool_port();
    ActId *bool_id = new ActId (new_bool_name.c_str());

    act_chp_lang_t *send = _make_send (chan_id, NULL);
    act_chp_lang_t *skip = _make_skip();

    act_chp_lang_t *sel = _make_nds_template(bool_id);
    sel->u.gc->s = skip;
    sel->u.gc->next->s = send;

    act_chp_lang_t *c_dup = chp_expand(c, ActNamespace::Global(), _p->CurScope());

    list_t *ll = list_new();
    list_append(ll, sel);
    list_append(ll, c_dup);

    c->type = ACT_CHP_SEMI;
    c->u.semi_comma.cmd = ll;
}

void ScanInsertion::_insert_static_breakpoint(act_chp_lang_t *&c)
{
    std::string new_chan_name = _add_chan_port(1);
    ActId *chan_id = new ActId (new_chan_name.c_str());
    
    act_chp_lang_t *send = _make_send(chan_id, NULL);
    act_chp_lang_t *c_dup = chp_expand(c, ActNamespace::Global(), _p->CurScope());

    list_t *ll = list_new();
    list_append(ll, send);
    list_append(ll, c_dup);

    c->type = ACT_CHP_SEMI;
    c->u.semi_comma.cmd = ll;
}

act_chp_lang_t *ScanInsertion::_make_send (ActId *chan, ActId *var)
{
    act_chp_lang_t *send = new act_chp_lang_t;
    send->label = NULL;
    send->space = NULL;
    send->type = ACT_CHP_SEND;
    send->u.comm.chan = chan;
    if (var) {
        send->u.comm.e = new Expr;
        send->u.comm.e->type = E_VAR;
        send->u.comm.e->u.e.l = (Expr *)(var);
    }
    else {
        send->u.comm.e = NULL;
    }
    return send;
}

act_chp_lang_t *ScanInsertion::_make_skip()
{
    act_chp_lang_t *skip = new act_chp_lang_t;
    skip->label = NULL;
    skip->space = NULL;
    skip->type = ACT_CHP_SKIP;
    return skip;
}

act_chp_lang_t *ScanInsertion::_make_recv (ActId *chan, ActId *var)
{
    act_chp_lang_t *recv = new act_chp_lang_t;
    recv->label = NULL;
    recv->space = NULL;
    recv->type = ACT_CHP_RECV;
    recv->u.comm.chan = chan;
    recv->u.comm.var = var;
    return recv;
}

// [| ~b -> NULL [] b -> NULL |]
act_chp_lang_t *ScanInsertion::_make_nds_template (ActId *bool_id)
{
    Expr *b = new Expr;
    b->type = E_VAR;
    b->u.e.l = (Expr *)(bool_id);

    Expr *nb = new Expr;
    nb->type = E_NOT;
    nb->u.e.l = b;

    act_chp_gc_t *gc = new act_chp_gc_t;
    gc->id = NULL;
    gc->lo = NULL;
    gc->hi = NULL;
    gc->g = nb;
    gc->s = NULL;

    gc->next = new act_chp_gc_t;
    gc->next->id = NULL;
    gc->next->lo = NULL;
    gc->next->hi = NULL;
    gc->next->g = b;
    gc->next->s = NULL;
    gc->next->next = NULL;

    act_chp_lang_t *sel = new act_chp_lang_t;
    sel->label = NULL;
    sel->space = NULL;
    sel->type = ACT_CHP_SELECT_NONDET;
    sel->u.gc = gc;

    return sel;
}

std::string ScanInsertion::_add_bool_port()
{
    std::string new_bool_name = scan_bool_pfx;
    new_bool_name.append(std::to_string(_gen_scan_port_idx()));
    InstType *it = TypeFactory::Factory()->NewBool(Type::NONE);
    it = it->Expand (ActNamespace::Global(), _p->CurScope());

    list_iappend (new_ports, _p->getNumPorts());
    int ret = _p->AddPort(it, strdup(new_bool_name.c_str()));
    Assert (ret==1, "could not add port?");
    Assert (_p->isPort(new_bool_name.c_str()), "not port?");

    return new_bool_name;
}

std::string ScanInsertion::_add_chan_port(int bw)
{
    std::string new_chan_name = scan_chan_pfx;
    new_chan_name.append(std::to_string(_gen_scan_port_idx()));

    InstType *it = TypeFactory::Factory()->NewInt (_p->CurScope(), Type::NONE, 0, const_expr (bw));
    it = TypeFactory::Factory()->NewChan (_p->CurScope(), Type::NONE, it, NULL);
    it = it->Expand (ActNamespace::Global(), _p->CurScope());

    list_iappend (new_ports, _p->getNumPorts());
    int ret = _p->AddPort(it, strdup(new_chan_name.c_str()));
    Assert (ret==1, "could not add port?");
    Assert (_p->isPort(new_chan_name.c_str()), "not port?");

    return new_chan_name;
}