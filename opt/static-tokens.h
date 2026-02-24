#pragma once
/*************************************************************************
 *
 *  This file is part of the ACT library
 *
 *  Copyright (c) 2018-2020 Lincoln Berkley
 *  Copyright (c) 2021-2022 Henry Heffan
 *  Copyright (c) 2024 Rajit Manohar
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
//
#include "chp-graph.h"

namespace ChpOptimize {

void putIntoStaticTokenForm(ChpGraph &graph);
void takeOutOfStaticTokenForm(ChpGraph &graph);

/* 
 * The fix_singletons flag is used to fix singleton selections. When the flag
 * is turned on, [ G -> S ] is replaced with S prior to static token form
 * conversion.
 *
 * This is used by the dataflow synthesis pass, because dataflow generation
 * assumes deadlock-freedom and no probes, which means that a singleton
 * selection must have its guard evaluate to true.
 *
 * For other uses of static token form (e.g. standard compiler optimizations),
 * this flag is set to false.
 */
void putIntoNewStaticTokenForm(ChpGraph &graph, bool fix_singletons = false);

void takeOutOfNewStaticTokenForm(ChpGraph &graph);



} // namespace ChpOptimize
