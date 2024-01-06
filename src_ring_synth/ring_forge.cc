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

#include "ring_forge.h"

RingForge::RingForge ( FILE *fp, Process *p, act_chp_lang_t *c,
            const char *circuit_library,
            const char *exprfile = "expr.act" )
    : RingEngine ( fp, p, c, circuit_library, exprfile )
{
    ring_block_prefix = "block_";
    conn_block_prefix = "conn_z_";

    // Datapath name prefixes
    capture_block_prefix = "latch_";
    expr_block_prefix = "blk_";
    expr_block_instance_prefix = "inst_";
    expr_block_input_prefix = "in_";

    // Channel name prefixes
    sync_chan_name_prefix = "sync_";
    parallel_chan_name_prefix = "sync_";
    init_cond_chan_prefix = "C_init_";

    // Bundled datapath parameters
    invx1_delay_ps = 21;
    capture_delay = 5;
    pulse_width = 6;

    // Instance counters
    block_count = 0;
    itb_wrapper_count = 0;
    bd_chan_count = 0;
    sync_chan_count = 0;
    expr_id = 0;
    expr_block_id = 0;
    mux_block_id = 0;
    branch_id = 0;
}


