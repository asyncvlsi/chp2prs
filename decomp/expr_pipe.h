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

#ifndef __OP_PIPE_H__
#define __OP_PIPE_H__

#include <act/chp/chopping_block.h>
#include <act/expr_cache.h>
#include <act/chp/ir-expr-act-conversion.h>
#include <memory>
#include <regex>
#include <act/chp/eqn_parser.h>

/*
    Expression/Operator Pipelining:
    Essentially we want to break up an expression into N sub-expressions,
    that when composed in order produce the original expression.
    i.e. given f(x), find f1, f2, ..., fn such that
    f1( f2( ... ( fn(x) ) ... ) ) = f(x)

    Consider the simple case of n=2.
    We do this by taking the original expression f, synthesizing it via
    ABC and retiming it, which cuts and places latches at some point in the 
    logic graph. We then read back in the left and right halves spit out
    in .eqn format and reconstruct the two sub-exprs f1 and f2.

    This allows us to write z:=f(x) as (y:=f2(x); z:=f1(y)) 
    which then allows a copy-insertion on y, which effectively pipelines
    the computation of the expensive function f.
    Note that the bitwidth of y may be anything and is an optimization metric
    to be passed to ABC to minimize/constrain.

    99% of the work here is really just tracking mappings between names 
    of variables/signals across different levels of representation. 
*/ 
class ExprPipe : public ExprCache {
    public:

        ExprPipe (GraphWithChanNames &g_in, Scope *s_in)
        : ExprCache ("abc", bd, false, ""),
            s (s_in), g(&g_in), _m_expr_id(0), 
            nm(), stmts(), rhss(), in_out_map(),
            n_cuts(0)
        {
            config_set_int("synth.expropt.vectorize_all_ports", 1);
        }

        ~ExprPipe () {
            if (_syn_dlib) {
                dlclose (_syn_dlib);
                _syn_dlib = NULL;
            }
        }

        void run ();
        
    protected:

        void _run_seq (Sequence, var_to_actvar &);

        void _run_expr (Block *, var_to_actvar &, int);
        void _run_expr_helper (ChpExprSingleRootDag &, var_to_actvar &, int);

        void _construct_int_expr (std::vector<VarId>);
        void _build_in_out_map ();

        void _apply_bitmap (ChpExpr &, const Bimap<VarId,int> &, VarId);
        void _apply_bitmap_primary_input (ChpExpr &, std::unordered_map<VarId,std::pair<VarId, int>>);

        std::unordered_map<VarId,std::pair<VarId, int>> 
            _build_primary_input_map (Bimap<ActId *, int> &, var_to_actvar &);

        std::vector<VarId> _get_used (std::vector<VarId>);
        std::vector<VarId> _get_io_image (std::vector<VarId>);

        void _expr_collect_vars (Expr *, Bimap<ActId *, int> &); 
        int _gen_expr_id();
        int bitwidth(ActId *);

        void print_cexpr (const ChpExpr &, VarId);

        std::string _expr_to_verilog (Expr *, int, Bimap<ActId *, int> &);

        void _verilog_to_eqn (std::string, std::string);

        void _parse_eqn ();

        void reset_state ();

        Scope *s;
        GraphWithChanNames *g;

        Bimap<std::string, VarId> nm;
        std::unordered_map<VarId, ChpExpr> stmts;

        // map from in-var of expr to out-var of prev expr
        // essentially the INORDER-OUTORDER map
        std::unordered_map<VarId, VarId> in_out_map;

        std::vector<ChpExpr> rhss;
        std::vector<VarId> lhss;

        int _m_expr_id;
        int n_cuts;

        // Expression handling for Expropt
        iHashtable *_inexprmap;
        iHashtable *_inwidthmap;

};

#endif