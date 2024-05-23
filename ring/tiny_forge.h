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

class TinyForge : public RingForge {
    public: 

    TinyForge ( FILE *fp, Process *p, act_chp_lang_t *c,
            ActBooleanizePass *bp, 
            int delay_margin,
            const char *circuit_library,
            const char *exprfile = "expr.act" );

        void run_forge ();
        bool check_if_pipeable (act_chp_lang_t *, int);

    protected:

        unsigned int term_inst_id;

        int _gen_term_inst_id();

        const char *term_inst_prefix;

        int _action_type(act_chp_lang_t *);

        int _terminate_port (int, int, int);

        void _generate_pipe (act_chp_lang_t *, int);

};