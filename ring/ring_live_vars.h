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
 *  Compute live variables at each point in a given CHP tree and 
 *  tag the actions (using the space pointer). 
 *  For actions, tagged variables are live-in. 
 *  For selections, tagged variables are live-out and are used
 *  to compute the merging muxes.
*/
class LiveVarAnalysis {
    public:
        /**
         * Live Variable Analysis 
         * Pre-processing for ring synthesis
         *
         * @param fp_out is the output file pointer
         *
         * @param p_in is the input Process for analysis
         *
         * @param c_in is the input chp tree for analysis
         *
         */
        LiveVarAnalysis (FILE *fp_out, Process *p_in, act_chp_lang_t *c_in)
        { 
            fp = fp_out; 
            p = p_in; 
            c = c_in; 
            H_live = hash_new(0);
            H_lcd = hash_new(0);
        }

        /*
        * Run the live-variable analysis algorithm
        */
        void generate_live_var_info ();

        /*
        * Print the generated information. For actions,
        * the variable are the ones that are live-in.
        * For selections, variables are the ones that
        * are live-out (of the merge).
        */
        void print_live_var_info ();

    private:

        // Output file 
        FILE *fp;

        // Process for analysis
        Process *p;

        // Top-level CHP tree for analysis
        act_chp_lang_t *c;

        // Internal running tracker of live variables
        Hashtable *H_live;

        // Internal tracker of loop carried dependencies
        Hashtable *H_lcd;

        void _generate_live_var_info (act_chp_lang_t *c_t, int root);
        void _print_live_var_info (act_chp_lang_t *c_t, int root);   
        void _print_var_list (list_t *var_list);

        void _add_to_live_vars (ActId *id);
        void _add_to_live_vars (Expr *e);
        void _remove_from_live_vars (ActId *id);

        void _tag_action_with_reqd_vars (act_chp_lang_t *action);
        
        void _add_to_live_vars_lcd (ActId *id);
        void _tag_action_with_reqd_vars_union_lcd (act_chp_lang_t *action);

        // Not used, this processing might be done in opt
        // void _generate_live_var_bits (act_chp_lang_t *c, int root);
        // int  _compute_total_bits (list_t *var_list);
        // void _update_tx_bits(int bits);
        // void _update_tx_bits(ActId *id, int mode);
        
};

