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

        // Data collection / query functions
        int _is_elementary_action (act_chp_lang_t *);
        int _chp_has_branches (act_chp_lang_t *, int);
        int _length_of_guard_set (act_chp_lang_t *);
        int _expr_is_pure_variable(Expr *, Process *);
        int _get_expr_width (Expr *, Process *);

        // Pipeline block generation functions
        int _generate_itb(FILE *);
        int _generate_pipe_element(act_chp_lang_t *, FILE *, Process *, int);
        int _generate_gp_connect(FILE *);
        int _generate_parallel_split(int, FILE *);
        int _generate_parallel_merge(int, FILE *);
        int _generate_selection_split(int, FILE *);
        int _generate_selection_merge(int, FILE *);
        int _generate_init_cond_itb(FILE *, int, int, int, int);
        int _generate_pipe_element_custom(int, int, int, ActId *, FILE *, Process *);

        // Expropt functions
        int _generate_expr_block(Expr *, int, Process *, FILE *);
        int _generate_expr_block_for_sel(Expr *, Process *, FILE *, int);
        void _expr_collect_vars (Expr *, int , Process *);
        void _instantiate_expr_block (FILE *, int, list_t *, Process *);

        // Channel generation functions
        int generate_bd_chan(int, FILE *);
        int generate_sync_chan(FILE *);

        // Pipeline block connection functions
        int connect_pipe_elements (FILE *, int, int, int);
        int connect_pll_split_outputs_to_pipe (FILE *, int, int, int);
        int connect_pipe_to_pll_merge_inputs (FILE *, int, int, int);
        int connect_sel_split_outputs_to_pipe (FILE *, int, int, int);
        int connect_guards_to_sel_split_input (FILE *, int, int, int);
        int connect_pipe_to_sel_merge_inputs (FILE *, int, int, int);

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

        // Instance counters
        unsigned int block_count;
        unsigned int itb_wrapper_count;
        unsigned int bd_chan_count;
        unsigned int sync_chan_count;
        unsigned int expr_id;
        unsigned int expr_block_id;
        unsigned int mux_block_id;
        unsigned int branch_id;

};