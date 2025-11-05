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
#include <act/abc_api.h>
#include <act/chp/ir-expr-act-conversion.h>
#include <memory>

/*
    Expression/Operator Pipelining:
    Essentially we want to break up an expression into N sub-expressions,
    that when composed in order produce the original expression.
    i.e. given f(x), find f1, f2, ..., fn such that
    f1( f2( ... ( fn(x) ) ) ) = f(x)

    Consider the simple case of n=2.
    We do this by taking the original expression f, synthesizing it via
    ABC and retiming it, which cuts and places latches at some point in the 
    logic graph. We then read back in the left and right halves spit out
    in .eqn format and reconstruct the two sub-exprs f1 and f2.

    This allows us to write z:=f(x) as (y:=f1(x); z:=f2(y)) 
    which then allows a copy-insertion on y, which effectively pipelines
    the computation of the expensive function f.
*/ 

/*
    Initialize syn+retiming commands
    Take in an expr and synth it 
*/

class ExprPipe : public ExprCache {
    public:

        ExprPipe (GraphWithChanNames &g_in, Scope *s_in)
        : ExprCache ("abc", bd, false, ""),
            s (s_in), g(&g_in), _m_expr_id(0)
        {
            if (!_abc_api) {
                _abc_api = new AbcApi();
            }
        }

        void run ();
        
        protected:

        void _run_seq (Sequence, var_to_actvar &);

        void _run_expr (Expr *, int);
        void _expr_collect_vars (Expr *); 
        int _gen_expr_id();
        int bitwidth(ActId *);

        std::string _expr_to_verilog (Expr *, int);

        void _verilog_to_eqn (std::string);

        void _parse_eqn ();

        std::string v2eqn_cmd;

        Scope *s;
        GraphWithChanNames *g;

        int _m_expr_id;

        // Expression handling for Expropt
        iHashtable *_inexprmap;
        iHashtable *_inwidthmap;

};

#endif