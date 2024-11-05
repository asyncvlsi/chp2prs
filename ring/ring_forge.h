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

#include "ring_live_vars.h"
#include "ring.h"

/*
 * Ring synthesizer class
 */
class RingForge : public RingEngine {
    public: 

    RingForge ( FILE *fp, Process *p, act_chp_lang_t *c,
            ActBooleanizePass *bp, int bdpath,
            int delay_margin, int dp_style,
            const char *circuit_library,
            const char *exprfile);

        void run_forge ();

    protected:

        void _run_forge_helper (act_chp_lang_t *);

        // Main synthesis functions
        void generate_pipe(act_chp_lang_t *, int);
        int generate_one_ring(act_chp_lang_t *, int, int);
        int generate_branched_ring(act_chp_lang_t *, int, int, int);
        int generate_branched_ring_non_ssa(act_chp_lang_t *, int, int, int);

        // Pipeline block generation functions
        int _generate_itb();
        int _generate_pipe_element(act_chp_lang_t *, int);
        int _generate_gp_connect();
        int _generate_parallel_split(int);
        int _generate_parallel_merge(int);
        int _generate_selection_split(int);
        int _generate_nds_split(int);
        int _generate_selection_merge(int);
        int _generate_init_cond_itb(int, int, int, int);
        int _generate_pipe_element_custom(int, int, int, ActId *);
        int _generate_pipe_element_lcd(int, ActId *);
        int _generate_pipe_element_lcd(int, const char *);
        int _generate_pause_element();
        int _generate_loop_wrapper();
        int _generate_doloop_wrapper();

        // Datapath generation functions
        int _generate_single_latch (var_info *, latch_info *, long long);
        int _generate_single_latch_non_ssa (var_info *, long long);
        int _generate_expr_block(Expr *, int);
        int _generate_expr_block_for_sel(Expr *, int);
        int _compute_merge_mux_info(latch_info_t *, int);
        float _lookup_mux_delays (int, int);
        std::pair<int,int> _get_pre_sel_latch_and_size (std::vector<int>);

        list_t *_create_channel_accesses (list_t *ics);

        // expr block functions
        void _expr_collect_vars (Expr *, int);
        void _instantiate_expr_block (int, list_t *);
        void _print_list_of_vars (FILE *fp, list_t *);

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
        const char *var_access_prefix;
        const char *expr_block_output_prefix;

        // Channel name prefixes
        const char *sync_chan_name_prefix; 
        const char *init_cond_chan_prefix;

        static const unsigned int invx1_delay_ps = 21;

        // Capture delay of a latch in multiples of 2*(INVX1 delay)
        static const unsigned int capture_delay = 5;

        // Pulse-width for the pulse-generator for the latch as fn. (2n+1)*d of invx1_delay_ps
        static const unsigned int pulse_width = 6;

        int _delay_margin;
        float delay_multiplier;
        int bundled;
        int datapath_style;

        // Temp: Lookup table for mux delays
        static const int max_mux_size = 4;
        static const int max_or_size = 4;
        float mux_delays[max_mux_size][max_or_size] = {
                        {0     , 0     , 0     , 0     }, // 1-input muxes => no mux 
                        {96.43 , 96.43 , 102.36, 106.81}, // 2-input muxes
                        {102.36, 102.36, 106.81, 124.33}, // 3-input muxes
                        {106.81, 106.81, 124.33, 135.41}  // 4-input muxes
        };             // 0-OR    1-OR    2-OR    3-OR 

};