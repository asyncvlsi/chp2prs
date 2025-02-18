/*************************************************************************
 *
 *  Copyright (c) 2025 Karthi Srinivasan
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

#ifndef __CHP_COST_H__
#define __CHP_COST_H__


#include "chopping_block.h"
#include <act/expropt.h>

class ChpCost {
    public:

        ChpCost (Scope *s)
        {
            _s = s;
            procs = {};
            _expr_id = 0;
            send_delay = config_get_real("synth.ring.bundled.send_delay");
            recv_delay = config_get_real("synth.ring.bundled.recv_delay");
            assn_delay = config_get_real("synth.ring.bundled.assn_delay");
            capture_delay = config_get_real("synth.ring.bundled.capture_delay");
        }

        void add_procs (std::vector<act_chp_lang_t *>);

        double latency_cost (act_chp_lang_t *);
        double _latency_cost (act_chp_lang_t *);

        double expr_delay (Expr *, int);
        void _expr_collect_vars (Expr *);
        int _gen_expr_id ();
        int bitwidth (ActId *);

        // Expression handling for Expropt
        iHashtable *_inexprmap;
        iHashtable *_inwidthmap;

        Scope *_s;

        int _expr_id;
        
        std::vector<act_chp_lang_t *> procs;

        double send_delay;
        double recv_delay;
        double assn_delay;
        double capture_delay;



};

#endif