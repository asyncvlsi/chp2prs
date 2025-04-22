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


enum class Action { None, Receive, Send, Assign };

enum class Port { M1, Zero, P1 };

enum class Term { Sink, Source };

static const std::set<std::vector<Action>> valid_signatures = 
{   
    // {Action::Send},
    // {Action::Receive},
    {Action::Receive, Action::Send} // gotta figure out if need half/full buffer..
};

class TinyForge : public RingForge {
    public: 

    TinyForge ( FILE *fp, 
            // Process *p, act_chp_lang_t *c,
            // ActBooleanizePass *bp, 
            int bdpath,
            int delay_margin, int dp_style, 
            const char *circuit_library,
            const char *exprfile = "expr.act" );

        void run_tiny_forge ();
        bool check_if_pipeable (act_chp_lang_t *);

    ~TinyForge () {
        if (eeo) { eeo->~ExprCache(); eeo=NULL; }
    }

    protected:

        unsigned int term_inst_id;

        std::vector<Action> prog_signature;

        int _gen_term_inst_id();

        const char *term_inst_prefix;

        bool _build_prog_signature (act_chp_lang_t *, int);

        void _terminate_port (int, Port, Term);

        void _run_forge (act_chp_lang_t *, int);
        void _run_forge_new (act_chp_lang_t *, std::vector<Action>);

};