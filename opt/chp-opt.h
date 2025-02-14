#pragma once
/*************************************************************************
 *
 *  This file is part of the ACT library
 *
 *  Copyright (c) 2018-2020 Lincoln Berkley
 *  Copyright (c) 2021-2022 Henry Heffan
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

#include "chp-cost-model.h"
#include "chp-graph.h"
#include "chp-print.h"

namespace ChpOptimize {

void optimize_chp_O2(ChpGraph &g, const std::string &proc_name, bool verbose);
void optimize_chp_O0(ChpGraph &g, const std::string &proc_name, bool verbose);
void optimize_chp_basic(ChpGraph &g, const std::string &proc_name, bool verbose);

/* optimizations */
bool propagateConstants(ChpGraph &graph);
bool eliminateCopies(ChpGraph &graph);
bool eliminateCopiesNew(ChpGraph &graph);
bool eliminateDeadCode(ChpGraph &graph);

void uninlineBitfieldExprsHack(ChpGraph &graph);

struct UsesAndDefs {
    bool can_hang = false;
    bool uses_chan = false;
    std::unordered_set<VarId> var_reads;
    std::unordered_set<VarId> var_writes;

    friend UsesAndDefs &operator|=(UsesAndDefs &a, const UsesAndDefs &b) {
        a.can_hang |= b.can_hang;
        a.uses_chan |= b.uses_chan;
        for (const auto id : b.var_reads)
            a.var_reads.insert(id);
        for (const auto id : b.var_writes)
            a.var_writes.insert(id);
        return a;
    }
};

std::unordered_map<const Block *, UsesAndDefs>
getDefUsesTable(const ChpGraph &graph);

std::pair<
  std::unordered_map<const Block *, std::unordered_set<VarId> >,
  std::unordered_map<const Block *, std::unordered_set<VarId> > >
getLiveVars (const ChpGraph &graph);
  
  
} // namespace ChpOptimize
