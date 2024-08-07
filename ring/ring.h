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
 * Base class for ring synthesis 
 */
class RingEngine {
    public:
        RingEngine ( FILE *fp, Process *p, act_chp_lang_t *c,
            ActBooleanizePass *bp, 
            const char *circuit_library,
            const char *exprfile);
    
        // void run_forge ();

    protected:
        
        FILE *_fp;

        Process *_p;

        act_chp_lang_t *_c;

        ActBooleanizePass *_bp; 

        // virtual void _run_forge_helper ();

        // Info collection
        void construct_var_infos (act_chp_lang_t *c);
        void print_var_infos (FILE *fp);
        int length_of_guard_set (act_chp_lang_t *c);
        bool is_elementary_action(act_chp_lang_t *c);
        bool chp_has_branches (act_chp_lang_t *c, int root);
        int get_expr_width(Expr *ex);

        // Internal functions
        void _construct_var_info (act_chp_lang_t *c, ActId *id, var_info *v);
        void _print_var_info (FILE *fp, var_info *v);
        bool _var_appears_in_expr (Expr *e, ActId *id);
        int _var_in_list (const char *name, list_t *l);

        // Save and restore state of var_infos 
        void save_var_infos ();
        void restore_var_infos ();

        // Merge mux info builder functions ---------
        void _construct_merge_latch_info (act_chp_lang_t *, int);
        bool _var_assigned_in_subtree (act_chp_lang_t *, const char *);

        void compute_mergemux_info (act_chp_lang_t *c);
        int _compute_mergemux_info (act_chp_lang_t *, var_info *, int);
        int _get_latest_assign_in_branch (act_chp_lang_t *, var_info *, int);
        
        void print_merge_mux_infos (FILE *fp, act_chp_lang_t *);
        void _print_latch_info_struct (FILE *fp, latch_info_t *);

        void flow_assignments (act_chp_lang_t *c);
        int _flow_assignments (act_chp_lang_t *, var_info *, int);

        bool _check_all_muxes_mapped (act_chp_lang_t *, bool);
        // Merge mux info builder functions ---------

        // Internal helper functions
        void _save_read_ids ();
        void _restore_read_ids ();
        var_info *_deepcopy_var_info (var_info *v, int only_read_id);
        Hashtable *_deepcopy_var_info_hashtable (Hashtable *h_in, int only_read_id);

        Hashtable *var_infos;
        Hashtable *var_infos_copy;
        Hashtable *var_infos_read_ids;

        // Expression handling for Expropt
        iHashtable *_inexprmap;
        iHashtable *_inwidthmap;

        char *_exprfile;
        char *_circuit_library;

        // Integer counters for instance IDs 
        unsigned int _block_id;
        unsigned int _itb_wrapper_id;
        unsigned int _bd_chan_id;
        unsigned int _sync_chan_id;
        unsigned int _expr_id;
        static unsigned int _expr_block_id;
        unsigned int _mux_block_id;

        int _gen_block_id ();
        int _gen_itb_wrapper_id ();
        int _gen_bd_chan_id ();
        int _gen_sync_chan_id ();
        int _gen_expr_id ();
        int _gen_expr_block_id ();
        int _gen_mux_block_id ();

        unsigned int _branch_id;
};