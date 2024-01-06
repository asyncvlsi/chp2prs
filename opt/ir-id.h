#pragma once
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

#include "hassert.h"
#include <compare>
#include <functional>
#include <unordered_map>
#include <unordered_set>
#include <vector>

class ActNamespace;
class TypeFactory;
class Scope;
class act_connection;
class ActId;

namespace ChpOptimize {


/**
 * This is simply an integer, but it is wrapped in a class for type
 * safety. This ensures that an integer representation for a variable
 * cannot be mistaken for an integer representation for something
 * else. Ids will have integer values that are 1 or larger.
 */
class VarId {
  public:
    uint64_t m_id;

    VarId()
        : m_id{1} {};
    explicit VarId(uint64_t id)
        : m_id{id} {
        hassert(m_id != 0);
    }

    /**
     * Returns the first valid variable ID. Used to initialize a
     * variable
     */
    static VarId first_id() { return VarId{1}; }

    /**
     * Given a variable Id, return its successor
     */
    [[nodiscard]] VarId next_id() const {
        hassert(m_id + 1 != 0);
        return VarId{m_id + 1};
    }

    auto operator<=>(const VarId &) const = default;
};


/**
 * This is an optional integer, but it is wrapped in a class for type
 * safety. This ensures that an integer representation for a variable
 * cannot be mistaken for an integer representation for something
 * else. The value 0 is used to indicate that there is no integer.
 */
class OptionalVarId {
    uint64_t m_idd;

  public:
    OptionalVarId()
        : m_idd{0} {}
    OptionalVarId(VarId c) // NOLINT
        : m_idd{c.m_id} {}

    /**
     * Returns an empty ID
     */
    static OptionalVarId null_id() { return OptionalVarId{}; }

    /**
     * The () operator returns true if a valid ID exists, false
     * otherwise.
     */
    operator bool() const { return m_idd != 0; } // NOLINT

    /**
     * The dereference operator returns the VarId coresponding to the
     * ID held. If there is no ID held, there will be an assertion
     * failure. 
     */
    VarId operator*() const {
        hassert(*this != null_id());
        return VarId{m_idd};
    }

   /**
    * DONT USE. This is for internal use only.
    */
    uint64_t _getId() const { return m_idd; }

    auto operator<=>(const OptionalVarId &) const = default;
};


/**
 * ChanId and OptionalChanId are the analogs for VarId and OptionalVarId
 */
class ChanId {
  public:
    uint64_t m_id;

    ChanId()
        : m_id{1} {};
    explicit ChanId(uint64_t id)
        : m_id{id} {
        hassert(m_id != 0);
    }

    static ChanId first_id() { return ChanId{1}; }
    [[nodiscard]] ChanId next_id() const {
        hassert(m_id + 1 != 0);
        return ChanId{m_id + 1};
    }

    auto operator<=>(const ChanId &) const = default;
};

class OptionalChanId {
    uint64_t m_idd;

  public:
    OptionalChanId()
        : m_idd{0} {}
    OptionalChanId(ChanId c) // NOLINT
        : m_idd{c.m_id} {}

    static OptionalChanId null_id() { return OptionalChanId{}; }
    operator bool() const { return m_idd != 0; } // NOLINT
    ChanId operator*() const {
        hassert(*this != null_id());
        return ChanId{m_idd};
    }
    uint64_t _getId() const { return m_idd; }

    auto operator<=>(const OptionalChanId &) const = default;
};

} // namespace ChpOptimize




/*
 * Support functions so that we can compute hashes for the ChanId,
 * OptionalChanId, VarId, and OptionalVarId classes. This enables them
 * to be used in maps, etc. from the C++ standard library.
 */
template <> struct ::std::hash<::ChpOptimize::ChanId> {
    size_t operator()(const ::ChpOptimize::ChanId &obj) const {
        return hash<int>()(obj.m_id);
    }
};

template <> struct ::std::hash<::ChpOptimize::OptionalChanId> {
    size_t operator()(const ::ChpOptimize::OptionalChanId &obj) const {
        return hash<int>()(obj._getId());
    }
};

template <> struct ::std::hash<::ChpOptimize::VarId> {
    size_t operator()(const ::ChpOptimize::VarId &obj) const {
        return hash<int>()(obj.m_id);
    }
};

template <> struct ::std::hash<::ChpOptimize::OptionalVarId> {
    size_t operator()(const ::ChpOptimize::OptionalVarId &obj) const {
        return hash<int>()(obj._getId());
    }
};


namespace ChpOptimize {


/**
 * This holds a pool of variables and channels, along with their type
 * information (bit-widths, are they Booleans).
 *
 * XXX: add support for exchange channels.
 */
class IdPool {
    struct VarIdInfo {
        int bitwidth;
        bool is_bool;
    };
    VarId m_next_varid = VarId::first_id();

    /**
     * Vector of variable IDs that are part of the current
     * pool. Variable ID #0 is a dummy. Note that these vectors rely
     * on the fact that the ith element of the vector is the ID that
     * is numbered i.
     */
    std::vector<VarIdInfo> m_varid_infos = {
        VarIdInfo{0, false}}; // 1 element of padding for the "null" id

    struct ChanIdInfo {
        int bitwidth;
        bool is_bool;
        bool is_inp;
    };
    ChanId m_next_chanid = ChanId::first_id();
    std::vector<ChanIdInfo> m_chanid_infos = {
        ChanIdInfo{0, false}}; // 1 element of padding for the "null" id

  public:
    IdPool() = default;

    [[nodiscard]] int getBitwidth(const VarId &id) const;
    [[nodiscard]] int getBitwidth(
        const ChanId &id) const; // returns 0 if the channel carries no data
    [[nodiscard]] bool getIsBool(const VarId &id) const;
    [[nodiscard]] bool getIsBool(const ChanId &id) const;

    [[nodiscard]] VarId makeUniqueVar(int bitwidth, bool is_bool = false);
    [[nodiscard]] ChanId makeUniqueChan(int bitwidth, bool is_bool = false);
    void setChanDir (const ChanId &id, bool is_inp);
    bool isChanInput (const ChanId &id);
    [[nodiscard]] VarId cloneVar (const VarId &id) {
      hassert(id.m_id < m_varid_infos.size());
      VarIdInfo idi = m_varid_infos[id.m_id];
      return makeUniqueVar (idi.bitwidth, idi.is_bool);
    }
};


  
/**
 * Maintains correspondence between the integer (VarId/ChanId) mapping
 * for actual ActIds from a scope.
 *
 * Note: all functions *assume* that the user is only calling these
 * functions with a properly type-checked program. In other words, if
 * a chanId is requested, then the ActId passed into the function is
 * in fact a legitimate channel in the scope.
 *
 * XXX: need to support structures
 */
class NameParsingIdPool {
// It would be much better if we stoped caring about act info in the middle
// TODO this class is NOT thread-safe, which violates const correctness. There
// should be a lock on the "actIdFromChanId" function


    
    Scope *m_scope; ///< The scope of the process of interest

    using ActIdDataPtr = act_connection *;

    std::unordered_map<VarId, ActId *> m_varid_to_actid{};
    std::unordered_map<ActIdDataPtr, VarId> m_actid_to_varid{};

    std::unordered_map<ChanId, ActId *> m_chanid_to_actid{};
    std::unordered_map<ActIdDataPtr, ChanId> m_actid_to_chanid{};

    IdPool m_id_pool;  ///< The Id pool used for the scope

  public:
    NameParsingIdPool()
        : m_scope{nullptr} {}
    explicit NameParsingIdPool(Scope *scope)
        : m_scope{scope} {}

    // the returned pointer should be deleted by the caller. Cant use a
    // std::unique_ptr because it might not have been created with [new]
    [[nodiscard]] OptionalChanId chanIdFromActId(ActId *id);
    [[nodiscard]] OptionalVarId varIdFromActId(ActId *id);

    [[nodiscard]] const IdPool &id_pool() const { return m_id_pool; }
    [[nodiscard]] IdPool &id_pool() { return m_id_pool; }


    /* XXX: fix this. this doesn't work for dotted identifiers,
       e.g. structures */
    [[nodiscard]] const char *getName(const VarId &id);
    [[nodiscard]] const char *getName(const ChanId &id);

    [[nodiscard]] int getBitwidth(const VarId &id) const {
        return m_id_pool.getBitwidth(id);
    }
    [[nodiscard]] int getBitwidth(const ChanId &id) const {
        return m_id_pool.getBitwidth(id);
    }
    [[nodiscard]] bool getIsBool(const VarId &id) const {
        return m_id_pool.getIsBool(id);
    }
    [[nodiscard]] bool getIsBool(const ChanId &id) const {
        return m_id_pool.getIsBool(id);
    }

    [[nodiscard]] VarId makeTmpVar(int bitwidth, bool is_bool = false) {
        return m_id_pool.makeUniqueVar(bitwidth, is_bool);
    }

    [[nodiscard]] std::unordered_map<ChanId, std::string>
    name_from_chan_map() const;
    [[nodiscard]] std::unordered_map<VarId, std::string>
    name_from_var_map() const;
};

} // namespace ChpOptimize
