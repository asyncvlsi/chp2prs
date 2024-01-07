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

#include "reqs.h"

/*
 * Ring synthesizer class
 */
class RingForge : public RingEngine {
    public: 

    RingForge ( FILE *fp, Process *p, act_chp_lang_t *c,
            const char *circuit_library,
            const char *exprfile = "expr.act" );

    protected:

        void _run_forge_helper ();

        // Main synthesis functions
        void generate_pipe(act_chp_lang_t *, FILE *, int, Process *);
        int generate_one_ring(act_chp_lang_t *, FILE *, int, int, Process *);
        int generate_branched_ring(act_chp_lang_t *, FILE *, int, int, Process *, int);

        // Pipeline block generation functions
        int _generate_itb();
        int _generate_pipe_element(act_chp_lang_t *, int);
        int _generate_gp_connect();
        int _generate_parallel_split(int);
        int _generate_parallel_merge(int);
        int _generate_selection_split(int);
        int _generate_selection_merge(int);
        int _generate_init_cond_itb(int, int, int, int);
        int _generate_pipe_element_custom(int, int, int, ActId *);

        // Datapath generation functions
        int _generate_single_latch (var_info *, int);
        int _generate_expr_block(Expr *, int);
        int _generate_expr_block_for_sel(Expr *, int);

        void _expr_collect_vars (Expr *, int);
        void _instantiate_expr_block (int, list_t *);

        // Channel generation functions
        int _generate_bd_chan(int);
        int _generate_sync_chan();

        // Pipeline block connection functions
        int _connect_pipe_elements (int, int);
        int _connect_pll_split_outputs_to_pipe (int, int, int);
        int _connect_pipe_to_pll_merge_inputs (int, int, int);
        int _connect_sel_split_outputs_to_pipe (int, int, int);
        int _connect_guards_to_sel_split_input (int, int, int);
        int _connect_pipe_to_sel_merge_inputs (int, int, int);

        // Pipeline block name prefixes
        const char *ring_block_prefix;
        const char *conn_block_prefix;

        // Datapath name prefixes
        const char *capture_block_prefix;
        const char *expr_block_prefix;
        const char *expr_block_instance_prefix;
        const char *expr_block_input_prefix;

        // Channel name prefixes
        const char *sync_chan_name_prefix; 
        const char *parallel_chan_name_prefix;
        const char *init_cond_chan_prefix;

        static unsigned int invx1_delay_ps;

        // Capture delay of a latch in multiples of 2*(INVX1 delay)
        static unsigned int capture_delay;

        // Pulse-width for the pulse-generator for the latch as fn. (2n+1)*d of invx1_delay_ps
        static unsigned int pulse_width;

};