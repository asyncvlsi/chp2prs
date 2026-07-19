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
 *************************************************************************
 */

#ifndef __ACT_RING_FORGE_H__
#define __ACT_RING_FORGE_H__

#include "ring_vars.h"
#include "ring.h"

// NOTE
// Using cache will turn off multi-output expr block support 
// (for now)
#define USE_CACHE 1

enum class BD_MODE { Latch_4phase, Latch_2phase, DFF };

static const std::unordered_map<BD_MODE, std::string>
bd_mode_config_params = {
    {BD_MODE::Latch_4phase, "synth.ring.bundled.delay_params_L"},
    {BD_MODE::Latch_2phase, "synth.ring.bundled.delay_params_L2P"},
    {BD_MODE::DFF,          "synth.ring.bundled.delay_params_DFF"}
};

static const std::unordered_map<BD_MODE, std::string>
bd_mode_config_offsets = {
    {BD_MODE::Latch_4phase, "synth.ring.bundled.delay_offsets_L"},
    {BD_MODE::Latch_2phase, "synth.ring.bundled.delay_offsets_L2P"},
    {BD_MODE::DFF,          "synth.ring.bundled.delay_offsets_DFF"}
};

static const std::unordered_map<BD_MODE, std::string>
bd_mode_config_vals = {
    {BD_MODE::Latch_4phase, "synth.ring.bundled.delay_vals_L"},
    {BD_MODE::Latch_2phase, "synth.ring.bundled.delay_vals_L2P"},
    {BD_MODE::DFF,          "synth.ring.bundled.delay_vals_DFF"}
};

/*
 * Ring synthesizer class
 */
class RingForge : public RingEngine {
    public: 

    RingForge ( FILE *fp, int bdpath,
                int delay_margin, int dp_style,
                BD_MODE bdpath_mode, 
                const char *externopt_toolname, 
                const char *circuit_library,
                const char *exprfile);

        void run_forge ();
        long long get_runtime();
        long long get_io_runtime();

    ~RingForge () {
        if (eeo) { 
            eeo->~ExprCache(); 
            eeo=NULL; 
        }
    }

    protected:

#if USE_CACHE
        ExprCache *eeo;
#else
        ExternalExprOpt *eeo;
#endif

        void _run_forge_helper (act_chp_lang_t *);
        bool _structure_check_q (act_chp_lang_t *);
        bool _structure_check_p (act_chp_lang_t *);
        bool _internal_loop_check (act_chp_lang_t *);
        bool _fill_in_ics (act_chp_lang_t *&);

        // Main synthesis functions
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
        int _generate_nds_split_stable(int);
        int _generate_probed_ds_split(int);
        int _generate_selection_merge(int);
        int _generate_pipe_element_lcd(int, ActId *, int, int, int);
        int _generate_pipe_element_lcd(int);
        int _generate_pause_element();
        int _generate_probe_access(ActId *);
        int _generate_probe_access_neg(ActId *);

        int handle_struct_recv (ActId *, ActId *, latch_info_t *, int);
        int struct_bw (ActId *);
        bool need_nesting (ActId *);
        Expr *struct_to_int_concat(Expr *);

        // Datapath generation functions
        int _generate_single_latch (var_info *, latch_info *, long long);
        int _generate_single_latch_non_ssa (var_info *, long long);
        int _generate_expr_block(Expr *, int, bool);
        int _generate_expr_block_for_sel(Expr *, int, bool);
        int _generate_expr_block_for_sel_all(act_chp_gc_t *, int, bool);
        std::pair<int,int> _compute_merge_mux_info(latch_info_t *, int, std::vector<ActId *>&);
        int _generate_probe_clause(list_t *, list_t *);
        int _generate_probe_circuit(Expr *, int);
        double _lookup_mux_delays (int, int);
        std::pair<int,int> _get_pre_sel_latch_and_size (std::vector<int>);

        int delay_table_sz;
        int mux_table_sz;
        int or_table_sz;
        int _compute_delay_line_param(double);

        std::vector<act_connection *> _create_channel_accesses (std::vector<act_connection *> ics);

        // expr block functions
        void _expr_collect_vars (Expr *&, int);
        void _instantiate_expr_block (std::string, int, list_t *, bool);
        void _print_list_of_vars (FILE *fp, std::vector<act_connection *>);
        int _bitWidth (ActId *);

        // Channel generation functions
        int _generate_bd_chan(int);
        int _generate_sync_chan();

        // Pipeline block connection functions
        int _connect_pipe_elements (int, int);
        int _connect_pll_split_outputs_to_pipe (int, int, int);
        int _connect_pipe_to_pll_merge_inputs (int, int, int);
        int _connect_sel_split_outputs_to_pipe (int, int, int);
        int _connect_guards_to_sel_split_input (int, int, int);
        int _connect_guards_to_sel_split_input_multi (int, int, int);
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

        // Name of int port chan in impl. of struct chan
        const char *struct_chan_name;

        BD_MODE _bdpath_mode;

        // Capture delay of a latch (ps)
        double capture_delay;

        // Pulse-width for the pulse-generator for the latch (ps)
        double pulse_width;

        int _delay_margin;
        
        float delay_multiplier;
        int bundled;
        int datapath_style;
        int verbose;

        long long runtime1;
        long long runtime2;

};

#endif