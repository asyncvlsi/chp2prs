#pragma once
/*************************************************************************
 *
 *  This file is part of the ACT library
 *
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

struct SdtProcOutput {
    std::string main_file, aux_file;
};
// This function takes in a chp-graph in "static token form" and creates a
// dataflow graph, as described in stf_to_dataflow.ps.gz. Note that the static
// token form of this project is slightly different that the static token form
// used in the paper. The entire chp program (including functions in other
// processes) is required to have "slack elasticity", meaning that one could add
// an arbitrary number of buffers on any channel without changing the
// computation. No probes and no shared variables is one way to guarantee that.
// To deal with channels with multiple reads/writes, we use the algorithm
// described in multiple_channel_accesses.pdf
SdtProcOutput
run_chp_to_dataflow_wrapper_proc(const std::string &wrapper_proc_name,
                                 const std::vector<ChanId> &chans,
                                 const ChpGraph &chp);
} // namespace ChpOptimize
