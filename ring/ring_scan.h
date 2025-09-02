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
 *************************************************************************
 */

#ifndef __ACT_RING_SCAN_H__
#define __ACT_RING_SCAN_H__

#include "reqs.h"

class ScanInsertion {
    public:
        ScanInsertion (Process *p_in, act_chp_lang_t *c_in)
        { 
            _p = p_in; 
            _c = c_in; 

            static_breakpt_markers = 
            {"break_static","break_st","bs"};
            dynamic_breakpt_markers = 
            {"break_dynamic","break_dyn","bd"};

            static_setpt_markers = 
            {"set_static","set_st","ss"};
            dynamic_setpt_markers = 
            {"set_dynamic","set_dyn","sd"};

            static_getpt_markers = 
            {"get_static","get_st","gs"};
            dynamic_getpt_markers = 
            {"get_dynamic","get_dyn","gd"};

            char buf[10240];
            ActNamespace::Act()->msnprintfproc (buf, 10240, _p);
            scan_bool_pfx = buf;
            scan_bool_pfx.append("_bp_b_");
            scan_chan_pfx = buf;
            scan_chan_pfx.append("_bp_C_");

            scan_port_ctr = 0;
            new_ports = list_new();
        }

        void insert_scan_points ();
        
        list_t *get_new_ports ();
        
        void print_new_ports (FILE *);

        act_chp_lang_t *getc () { return _c; }

    protected:
        
        void _insert_scan_points (act_chp_lang_t *&);

        void _insert_scan_point (act_chp_lang_t *&);

        /*
            Semantics for get/set/watch insertions
            C_bp : New channel port to the process
            b_bp : New bool port to the process
            breakpoint : can be anywhere
            setpoint : only at assignments
            getpoint : only at assignments
        */

        /*
            ps:A  -->  A; C_bp!
        */
        void _insert_static_breakpoint (act_chp_lang_t *&);

        /*
            pd:A  -->  A; [| ~b_bp->skip [] b_bp->C_bp! |]
        */
        void _insert_dynamic_breakpoint (act_chp_lang_t *&);

        /*
            gs:x:=e  -->  x:=e; C_bp!x
        */
        void _insert_static_getpoint (act_chp_lang_t *&);

        /*
            gd:x:=e -->  x:=e; [| ~b_bp->skip [] b_bp->C_bp!x |]
        */
        void _insert_dynamic_getpoint (act_chp_lang_t *&);

        /*
            ss:x:=e  -->  x:=e; C_bp?x
        */
        void _insert_static_setpoint (act_chp_lang_t *&);

        /*
            sd:x:=e -->  [| ~b_bp->x:=e [] b_bp->C_bp?x |]
        */
        void _insert_dynamic_setpoint (act_chp_lang_t *&);

        std::string _add_chan_port (int);
        std::string _add_bool_port ();

        int _gen_scan_port_idx ();

        act_chp_lang_t *_make_send (ActId *, ActId *);
        act_chp_lang_t *_make_nds_template (ActId *);
        act_chp_lang_t *_make_recv (ActId *, ActId *);
        act_chp_lang_t *_make_skip ();

        // Process for analysis
        Process *_p;

        // Top-level CHP tree for analysis
        act_chp_lang_t *_c;

        list_t *new_ports;

        std::unordered_set<std::string> static_breakpt_markers;
        std::unordered_set<std::string> dynamic_breakpt_markers;
        std::unordered_set<std::string> dynamic_setpt_markers;
        std::unordered_set<std::string> static_setpt_markers;
        std::unordered_set<std::string> dynamic_getpt_markers;
        std::unordered_set<std::string> static_getpt_markers;

        std::string scan_bool_pfx;
        std::string scan_chan_pfx;

        unsigned int scan_port_ctr;

  
};

#endif