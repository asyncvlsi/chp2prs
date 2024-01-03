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

#include "chp-graph.h"

namespace ChpOptimize {
bool transposeAssignmentAndChanOps(ChpGraph &graph);
bool fuseAssignments(ChpGraph &graph);
void packAssignmentOneVar(ChpGraph &graph);

void inlineOneWriteExprs(ChpGraph &graph);

void uninlineBitfieldExprsHack(ChpGraph &graph);
void unpackLargeConstantHack(ChpGraph &graph);
void parallelizeGraph(ChpGraph &graph);

bool liftGuards(ChpGraph &graph);
bool uniqueifyInnerPaths(ChpGraph &graph);

/// This is not a smart method of removing self-assignment. It is meant to be
/// fast (for the -O0) option as this is a required invariant for chp2prs
void removeSelfAssignments(ChpGraph &graph);
void removeElseOnlySelect(ChpGraph &graph);
void hassertNoSelfAssignments(const ChpGraph &graph);

} // namespace ChpOptimize
