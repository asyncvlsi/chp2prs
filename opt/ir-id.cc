/*************************************************************************
 *
 * This file is part of the ACT library
 *
 * Copyright (c) 2021-2022 Henry Heffan
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License
 * as published by the Free Software Foundation; either version 2
 * of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor,
 * Boston, MA 02110-1301, USA.
 *
 **************************************************************************
 */

#include "ir-id.h"
#include "algos.h"
#include "hassert.h"
#include <act/act.h>

namespace ChpOptimize {

OptionalChanId NameParsingIdPool::chanIdFromActId(ActId *id) {
    if (!id) {
        return OptionalChanId::null_id();
    }

    ActIdDataPtr canonicalId = id->Canonical(m_scope);
    auto irid_it = m_actid_to_chanid.find(canonicalId);
    if (irid_it != m_actid_to_chanid.end()) {
        return irid_it->second;
    }

    // look up the channel's info
    InstType *varType = m_scope->FullLookup(id, nullptr);
    hassert(varType);
    Chan *varChannel = dynamic_cast<Chan *>(varType->BaseType());
    hassert(varChannel);
    InstType *chanType = varChannel->datatype();
    Int *varInt = dynamic_cast<Int *>(chanType->BaseType());
    bool is_bool = dynamic_cast<Bool *>(chanType->BaseType());
    bool is_enum = TypeFactory::isUserEnum (chanType);

    /* XXX: will fail for data types and structures */
    hassert(varInt || is_bool || is_enum);
    
    int bitwidth = varInt ? TypeFactory::bitWidth(varInt) :
      (is_enum ? TypeFactory::bitWidth (chanType) : 1);

    /* XXX: this will fail if the channel is a pure synchronization
       channel 
    */
    hassert(bitwidth > 0);

    // then create a variable to hold it
    auto new_id = m_id_pool.makeUniqueChan(bitwidth, is_bool);

    hassert(m_chanid_to_actid.find(new_id) == m_chanid_to_actid.end());
    m_chanid_to_actid[new_id] = id;
    m_actid_to_chanid[canonicalId] = new_id;

    return new_id;
}

OptionalVarId NameParsingIdPool::varIdFromActId(ActId *id) {
    if (!id) {
        return OptionalVarId::null_id();
    }

    ActIdDataPtr canonicalId = id->Canonical(m_scope);
    auto irid_it = m_actid_to_varid.find(canonicalId);
    if (irid_it != m_actid_to_varid.end()) {
        // delete canonicalId; I dont think we can delete this :(
        return irid_it->second;
    }

    // look up the variable's info
    hassert(id);
    InstType *varType = m_scope->FullLookup(id, nullptr);
    hassert(varType);
    Int *varInt = dynamic_cast<Int *>(varType->BaseType());
    bool is_bool = TypeFactory::isBoolType(varType);
    bool is_enum = TypeFactory::isUserEnum (varType);

    /* XXX: will fail for data types and structures */
    hassert(varInt || is_bool || is_enum);
    
    int bitwidth = varInt ? TypeFactory::bitWidth(varInt) :
      (is_enum ? TypeFactory::bitWidth (varType) : 1);
    
    hassert(bitwidth > 0);

    // then create a variable to hold it
    auto new_id = m_id_pool.makeUniqueVar(bitwidth, is_bool);

    hassert(m_varid_to_actid.find(new_id) == m_varid_to_actid.end());
    m_varid_to_actid[new_id] = id;
    m_actid_to_varid[canonicalId] = new_id;

    return new_id;
}

const char *NameParsingIdPool::getName(const VarId &id) {
    return m_varid_to_actid.at(id)->getName();
}
const char *NameParsingIdPool::getName(const ChanId &id) {
    return m_chanid_to_actid.at(id)->getName();
}

[[nodiscard]] std::unordered_map<ChanId, ActId *>
NameParsingIdPool::name_from_chan_map() const {
  return m_chanid_to_actid;
}

[[nodiscard]] std::unordered_map<VarId, ActId *>
NameParsingIdPool::name_from_var_map() const {
  return m_varid_to_actid;
}

/*
 * Straightforward accessor functions for the ID pool
 */
bool IdPool::getIsBool(const ChanId &id) const {
    return m_chanid_infos[id.m_id].is_bool;
}

bool IdPool::getIsBool(const VarId &id) const {
    return m_varid_infos[id.m_id].is_bool;
}

int IdPool::getBitwidth(const VarId &var_id) const {
    hassert(var_id.m_id < m_varid_infos.size());
    return m_varid_infos[var_id.m_id].bitwidth;
}

int IdPool::getBitwidth(const ChanId &chan_id) const {
    hassert(chan_id.m_id < m_chanid_infos.size());
    return m_chanid_infos[chan_id.m_id].bitwidth;
}

VarId IdPool::makeUniqueVar(int bitwidth, bool is_bool) {
    VarId new_id = m_next_varid;
    hassert(new_id.m_id == m_varid_infos.size());
    m_next_varid = m_next_varid.next_id();
    hassert(bitwidth > 0);
    m_varid_infos.push_back(VarIdInfo{bitwidth, is_bool});
    return new_id;
}

ChanId IdPool::makeUniqueChan(int bitwidth, bool is_bool) {
    ChanId new_id = m_next_chanid;
    hassert(new_id.m_id == m_chanid_infos.size());
    m_next_chanid = m_next_chanid.next_id();
    hassert(bitwidth > 0);
    m_chanid_infos.push_back(ChanIdInfo{bitwidth, is_bool});
    return new_id;
}

void IdPool::setChanDir (const ChanId &ch, bool is_inp) {
  hassert (ch.m_id < m_chanid_infos.size());
  m_chanid_infos[ch.m_id].is_inp = is_inp;
}

bool IdPool::isChanInput (const ChanId &ch) {
  hassert (ch.m_id < m_chanid_infos.size());
  return m_chanid_infos[ch.m_id].is_inp;
}

} // namespace ChpOptimize
