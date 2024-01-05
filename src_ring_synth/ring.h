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
            const char *circuit_library,
            const char *exprfile = "expr.act")
            {
                _fp = fp;
                _p = p;
                _c = c;
                _circuit_library = Strdup(circuit_library);
                _exprfile = Strdup(exprfile);
            }; 
    
        void run_forge ();

    protected:

        virtual void _run_forge_helper ();

        void _construct_var_infos (ActBooleanizePass *bp);
        void _print_var_infos (FILE *fp);
        
        void _construct_var_info (act_chp_lang_t *c, ActId *id, var_info *v);
        void _print_var_info (FILE *fp, var_info *v);
        bool _var_appears_in_expr (Expr *e, ActId *id);

        Hashtable *var_infos;
        
        char *_exprfile;
        char *_circuit_library;
        
        FILE *_fp;

        Process *_p;
        act_chp_lang_t *_c;
};