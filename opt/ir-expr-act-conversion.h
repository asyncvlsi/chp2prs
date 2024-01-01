#pragma once
/*************************************************************************
 *
 * This file is part of the ACT library
 *
 * Copyright (c) 2021-2022 Henry Heffan
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License
 * as published by the Free Software Foundation; either version 2
 * of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor,
 * Boston, MA 02110-1301, USA.
 *
 **************************************************************************
 */

#include "ir-expr.h"
#include <act/act.h>
#include <common/int.h>


namespace ChpOptimize {
using ActBigInt = ::BigInt;
using ActExprStruct = struct ::expr;

namespace detail {

/**  
 * For E_INT, this returns a pair that corresponds to the newly
 * defined ChpOptimize::BigInt (big-int.h) and the bit-width of the
 * integer according to Act rules.
 */
inline std::pair<BigInt, int> bigint_from_int_expr(const ActExprStruct *o) {
    hassert(o->type == E_INT);
    if (!o->u.ival.v_extra) {
        /* we use the default bit-width for integers */
        BigInt result = BigInt(o->u.ival.v);
        return {result, BigInt::bitwidth(result)};
    } else {
        /* otherwise we take the original Act BigInt and convert it to ours */
        ActBigInt *bi = (ActBigInt *)o->u.ival.v_extra; // this should be const

        BigInt res = BigInt{0};
        for (ssize_t i = 0; i < (ssize_t)bi->getLen(); ++i) {
            res += (BigInt{bi->getVal(i)} << (64 * i));
        }
        return {res, bi->getWidth()};
    }
}
  
} // namespace detail


/*
 * This returns an IRExpr from an Act Expr *
 * The first three template arguments are those required for the
 * IRExpr, while the last is a function that maps ActId * to a pair
 * corresponding to a varId and bitwidth.
 */
template <typename Tag, typename VarIdType, ManageMemory manageMemory,
          typename VarIdFromActIdFn>
std::unique_ptr<IRExpr<Tag, VarIdType, manageMemory>>
template_func_new_irexpr_from_expr(const ActExprStruct *o,
                                   const VarIdFromActIdFn &varid_from_actid) {
    static_assert(
        std::is_same_v<std::invoke_result_t<VarIdFromActIdFn, ActId *>,
                       std::pair<VarIdType, int>>);

    // short-cut for the recursive call
    auto new_irexpr_from_expr = [&](const ActExprStruct *xo)
        -> std::unique_ptr<IRExpr<Tag, VarIdType, manageMemory>> {
        return template_func_new_irexpr_from_expr<Tag, VarIdType, manageMemory>(
            xo, varid_from_actid);
    };

    using IRExpr_t = IRExpr<Tag, VarIdType, manageMemory>;
    if (!o)
        return nullptr;

    switch (o->type) {
    case E_LPAR:
    case E_RPAR:
    case E_REAL:
    case E_END:
        hassert(false);
        break; // these should already be stripped out

    case E_CONCAT:
      /* Convert the standard list of concatenations into a simple
       * tree with a binary concat operator (IRExpr v/s Expr
       * representation).
       */
        hassert(o->u.e.r);
        hassert(o->u.e.r->type == E_CONCAT);
        if (o->u.e.r->u.e.r == nullptr) {
            return std::make_unique<IRExpr_t>(IRExpr_t::makeBinaryOp(
                IRBinaryOpType::Concat, new_irexpr_from_expr(o->u.e.l),
                new_irexpr_from_expr(o->u.e.r->u.e.l)));
        } else {
            return std::make_unique<IRExpr_t>(IRExpr_t::makeBinaryOp(
                IRBinaryOpType::Concat, new_irexpr_from_expr(o->u.e.l),
                new_irexpr_from_expr(o->u.e.r)));
        }

	/* This next group are "pass-thru" conversions */
    case E_AND:
        return std::make_unique<IRExpr_t>(IRExpr_t::makeBinaryOp(
            IRBinaryOpType::And, new_irexpr_from_expr(o->u.e.l),
            new_irexpr_from_expr(o->u.e.r)));
    case E_OR:
        return std::make_unique<IRExpr_t>(IRExpr_t::makeBinaryOp(
            IRBinaryOpType::Or, new_irexpr_from_expr(o->u.e.l),
            new_irexpr_from_expr(o->u.e.r)));
    case E_XOR:
        return std::make_unique<IRExpr_t>(IRExpr_t::makeBinaryOp(
            IRBinaryOpType::Xor, new_irexpr_from_expr(o->u.e.l),
            new_irexpr_from_expr(o->u.e.r)));
    case E_PLUS:
        return std::make_unique<IRExpr_t>(IRExpr_t::makeBinaryOp(
            IRBinaryOpType::Plus, new_irexpr_from_expr(o->u.e.l),
            new_irexpr_from_expr(o->u.e.r)));
    case E_MINUS:
        return std::make_unique<IRExpr_t>(IRExpr_t::makeBinaryOp(
            IRBinaryOpType::Minus, new_irexpr_from_expr(o->u.e.l),
            new_irexpr_from_expr(o->u.e.r)));
    case E_MULT:
        return std::make_unique<IRExpr_t>(IRExpr_t::makeBinaryOp(
            IRBinaryOpType::Mult, new_irexpr_from_expr(o->u.e.l),
            new_irexpr_from_expr(o->u.e.r)));
    case E_DIV:
        return std::make_unique<IRExpr_t>(IRExpr_t::makeBinaryOp(
            IRBinaryOpType::Div, new_irexpr_from_expr(o->u.e.l),
            new_irexpr_from_expr(o->u.e.r)));
    case E_MOD:
        return std::make_unique<IRExpr_t>(IRExpr_t::makeBinaryOp(
            IRBinaryOpType::Mod, new_irexpr_from_expr(o->u.e.l),
            new_irexpr_from_expr(o->u.e.r)));
    case E_LSL:
        return std::make_unique<IRExpr_t>(IRExpr_t::makeBinaryOp(
            IRBinaryOpType::LeftShift, new_irexpr_from_expr(o->u.e.l),
            new_irexpr_from_expr(o->u.e.r)));
    case E_LSR:
        return std::make_unique<IRExpr_t>(IRExpr_t::makeBinaryOp(
            IRBinaryOpType::RightShift, new_irexpr_from_expr(o->u.e.l),
            new_irexpr_from_expr(o->u.e.r)));
    case E_ASR:
        return std::make_unique<IRExpr_t>(IRExpr_t::makeBinaryOp(
            IRBinaryOpType::ArithmeticRightShift,
            new_irexpr_from_expr(o->u.e.l), new_irexpr_from_expr(o->u.e.r)));

    case E_LT:
        return std::make_unique<IRExpr_t>(IRExpr_t::makeBinaryOp(
            IRBinaryOpType::LT, new_irexpr_from_expr(o->u.e.l),
            new_irexpr_from_expr(o->u.e.r)));
    case E_GT:
        return std::make_unique<IRExpr_t>(IRExpr_t::makeBinaryOp(
            IRBinaryOpType::GT, new_irexpr_from_expr(o->u.e.l),
            new_irexpr_from_expr(o->u.e.r)));
    case E_LE:
        return std::make_unique<IRExpr_t>(IRExpr_t::makeBinaryOp(
            IRBinaryOpType::LE, new_irexpr_from_expr(o->u.e.l),
            new_irexpr_from_expr(o->u.e.r)));
    case E_GE:
        return std::make_unique<IRExpr_t>(IRExpr_t::makeBinaryOp(
            IRBinaryOpType::GE, new_irexpr_from_expr(o->u.e.l),
            new_irexpr_from_expr(o->u.e.r)));
    case E_EQ:
        return std::make_unique<IRExpr_t>(IRExpr_t::makeBinaryOp(
            IRBinaryOpType::EQ, new_irexpr_from_expr(o->u.e.l),
            new_irexpr_from_expr(o->u.e.r)));
    case E_NE:
        return std::make_unique<IRExpr_t>(IRExpr_t::makeBinaryOp(
            IRBinaryOpType::NE, new_irexpr_from_expr(o->u.e.l),
            new_irexpr_from_expr(o->u.e.r)));

    /* remove builtin-int */
    case E_BUILTIN_INT:
        if (o->u.e.r) {
            hassert(o->u.e.r->type == E_INT);
            return std::make_unique<IRExpr_t>(IRExpr_t::makeBitfield(
                new_irexpr_from_expr(o->u.e.l),
                detail::bigint_from_int_expr(o->u.e.r).first.getI32() - 1, 0));
        } else {
            // this is a conversion from a bool -> int, so the bitwidth is 1, so
            // it is a nop
            auto e = new_irexpr_from_expr(o->u.e.l);
            hassert(e->width == 1);
            return e;
        }

    /* remove builtin bool */
    case E_BUILTIN_BOOL: {
        auto e = new_irexpr_from_expr(o->u.e.l);
        if (e->width == 1)
            return e;
        return std::make_unique<IRExpr_t>(IRExpr_t::makeBinaryOp(
            IRBinaryOpType::NE, std::move(e),
            std::make_unique<IRExpr_t>(IRExpr_t::makeConstant(BigInt{0}, 1))));
    }

    /* ensure both l and r fields have the same bit-width */
    case E_QUERY: {
        hassert(o->u.e.r);
        hassert(o->u.e.r->type == E_COLON);
        auto lhs = new_irexpr_from_expr(o->u.e.r->u.e.l);
        auto rhs = new_irexpr_from_expr(o->u.e.r->u.e.r);
        int width = std::max(lhs->width, rhs->width);
        if (lhs->width != width)
            lhs = std::make_unique<IRExpr_t>(
                IRExpr_t::makeResize(std::move(lhs), width));
        if (rhs->width != width)
            rhs = std::make_unique<IRExpr_t>(
                IRExpr_t::makeResize(std::move(rhs), width));
        return std::make_unique<IRExpr_t>(IRExpr_t::makeQuery(
            new_irexpr_from_expr(o->u.e.l), std::move(lhs), std::move(rhs)));
    }

    case E_COLON:
        hassert(false); // should have skiped this in the Query case
        break;

    case E_COMPLEMENT: // how is this different than E_NOT?
    case E_NOT:
        return std::make_unique<IRExpr_t>(IRExpr_t::makeUnaryOp(
            IRUnaryOpType::Not, new_irexpr_from_expr(o->u.e.l)));

    case E_UMINUS:
        return std::make_unique<IRExpr_t>(IRExpr_t::makeUnaryOp(
            IRUnaryOpType::UnaryMinus, new_irexpr_from_expr(o->u.e.l)));

    case E_INT: {
        auto [val, bit_width] = detail::bigint_from_int_expr(o);
        return std::make_unique<IRExpr_t>(
            IRExpr_t::makeConstant(val, bit_width));
    }
    case E_TRUE:
        return std::make_unique<IRExpr_t>(IRExpr_t::makeConstant(BigInt{1}, 1));
    case E_FALSE:
        return std::make_unique<IRExpr_t>(IRExpr_t::makeConstant(BigInt{0}, 1));
    case E_VAR: {
        auto [id, bit_width] = varid_from_actid((ActId *)o->u.e.l);
        return std::make_unique<IRExpr_t>(
            IRExpr_t::makeVariableAccess(id, bit_width));
    }
      
    case E_PROBE: {
        hassert(false); // TODO figure this out when we need it
        break;
    }
      
    case E_BITFIELD: {
        hassert(o->u.e.l);
        hassert(o->u.e.r);
        hassert(o->u.e.r->type == E_BITFIELD);
        hassert(o->u.e.r->u.e.l);
        hassert(o->u.e.r->u.e.l->type == E_INT);
        hassert(o->u.e.r->u.e.r);
        hassert(o->u.e.r->u.e.r->type == E_INT);

        auto [id, bit_width] = varid_from_actid((ActId *)o->u.e.l);

        return std::make_unique<IRExpr_t>(IRExpr_t::makeBitfield(
            std::make_unique<IRExpr_t>(
                IRExpr_t::makeVariableAccess(id, bit_width)),
            detail::bigint_from_int_expr(o->u.e.r->u.e.r).first.getI32(),
            detail::bigint_from_int_expr(o->u.e.r->u.e.l).first.getI32()));
    }
    case E_RAWFREE:
    case E_NUMBER: {
        hassert(false); // I dont know what this is
        break;
    }
    default:
        fprintf(stderr, "%d\n", (o->type));
        hassert(false); // I dont think we can reach this?
        break;
    }
    hassert(false);
    return nullptr;
}

/*
  This converts an IRExpr to an Act Expr *
*/
  
// `expectedType` must be Int if the width of `o` is not 1. If it is 1, the
// returned expr struct will be properly typed.
template <typename Tag, typename VarIdType, ManageMemory manageMemory,
          typename ActIdFromVarIdFn>
ActExprStruct *template_func_new_expr_from_irexpr(
    const IRExpr<Tag, VarIdType, manageMemory> &e, ActExprIntType expectedType,
    const ActIdFromVarIdFn &actid_from_varid) {
    static_assert(
        std::is_same_v<std::invoke_result_t<ActIdFromVarIdFn, VarIdType>,
                       ActId *>);
    hassert(e.width > 0);
    if (e.width > 1)
        hassert(expectedType == ActExprIntType::Int);

    using IRExpr_t = IRExpr<Tag, VarIdType, manageMemory>;

    auto new_expr_from_irexpr =
        [&](const IRExpr_t &xo,
            ActExprIntType expectedType) -> ActExprStruct * {
        return template_func_new_expr_from_irexpr<Tag, VarIdType, manageMemory>(
            xo, expectedType, actid_from_varid);
    };

    //    ActExprStruct *r;
    //    NEW(r, ActExprStruct);

    auto newExprStruct = [&]() {
        ActExprStruct *r;
        NEW(r, ActExprStruct);
        return r;
    };
    
    auto newUnaryOp = [&](int type, const IRExpr_t &l) {
        Expr *result = newExprStruct();
        result->type = type;
        result->u.e.l = new_expr_from_irexpr(l, ActExprIntType::Int);
        result->u.e.r = nullptr;
        return result;
    };
    
    auto newBinaryOp = [&](int type, const IRExpr_t &l, const IRExpr_t &r) {
        Expr *result = newExprStruct();
        result->type = type;
        result->u.e.l = new_expr_from_irexpr(l, ActExprIntType::Int);
        result->u.e.r = new_expr_from_irexpr(r, ActExprIntType::Int);
        return result;
    };
    
    auto typedFromInt = [&](ActExprStruct *int_expr,
                            ActExprIntType desiredType) {
        if (desiredType == ActExprIntType::Int)
            return int_expr;
        Expr *result = newExprStruct();
        result->type = E_BUILTIN_BOOL;
        result->u.e.l = int_expr;
        result->u.e.r = nullptr;
        return result;
    };
    
    auto typedFromBool = [&](ActExprStruct *bool_expr,
                             ActExprIntType desiredType) {
        if (desiredType == ActExprIntType::Bool)
            return bool_expr;
        Expr *result = newExprStruct();
        result->type = E_BUILTIN_INT;
        result->u.e.l = bool_expr;
        result->u.e.r = nullptr;
        return result;
    };

    switch (e.type()) {
    case IRExprTypeKind::BinaryOp: {
        switch (e.u_e2().op_type) {
        case IRBinaryOpType::Concat: {
            if (e.u_e2().r->type() == IRExprTypeKind::BinaryOp &&
                e.u_e2().r->u_e2().op_type == IRBinaryOpType::Concat)
                return typedFromInt(
                    newBinaryOp(E_CONCAT, *e.u_e2().l, *e.u_e2().r),
                    expectedType);

            Expr *result = newExprStruct();
            result->type = E_CONCAT;
            result->u.e.l =
                new_expr_from_irexpr(*e.u_e2().l, ActExprIntType::Int);
            NEW(result->u.e.r, ActExprStruct);
            result->u.e.r->type = E_CONCAT;
            result->u.e.r->u.e.l =
                new_expr_from_irexpr(*e.u_e2().r, ActExprIntType::Int);
            result->u.e.r->u.e.r = nullptr;
            return result;
        }

        case IRBinaryOpType::And:
            return typedFromInt(newBinaryOp(E_AND, *e.u_e2().l, *e.u_e2().r),
                                expectedType);
        case IRBinaryOpType::Or:
            return typedFromInt(newBinaryOp(E_OR, *e.u_e2().l, *e.u_e2().r),
                                expectedType);
        case IRBinaryOpType::Xor:
            return typedFromInt(newBinaryOp(E_XOR, *e.u_e2().l, *e.u_e2().r),
                                expectedType);
        case IRBinaryOpType::Plus:
            return typedFromInt(newBinaryOp(E_PLUS, *e.u_e2().l, *e.u_e2().r),
                                expectedType);
        case IRBinaryOpType::Minus:
            return typedFromInt(newBinaryOp(E_MINUS, *e.u_e2().l, *e.u_e2().r),
                                expectedType);
        case IRBinaryOpType::Mult:
            return typedFromInt(newBinaryOp(E_MULT, *e.u_e2().l, *e.u_e2().r),
                                expectedType);
        case IRBinaryOpType::Div:
            return typedFromInt(newBinaryOp(E_DIV, *e.u_e2().l, *e.u_e2().r),
                                expectedType);
        case IRBinaryOpType::Mod:
            return typedFromInt(newBinaryOp(E_MOD, *e.u_e2().l, *e.u_e2().r),
                                expectedType);
        case IRBinaryOpType::LeftShift:
            return typedFromInt(newBinaryOp(E_LSL, *e.u_e2().l, *e.u_e2().r),
                                expectedType);
        case IRBinaryOpType::RightShift:
            return typedFromInt(newBinaryOp(E_LSR, *e.u_e2().l, *e.u_e2().r),
                                expectedType);
        case IRBinaryOpType::ArithmeticRightShift:
            return typedFromInt(newBinaryOp(E_ASR, *e.u_e2().l, *e.u_e2().r),
                                expectedType);

        case IRBinaryOpType::LT:
            return typedFromBool(newBinaryOp(E_LT, *e.u_e2().l, *e.u_e2().r),
                                 expectedType);
        case IRBinaryOpType::GT:
            return typedFromBool(newBinaryOp(E_GT, *e.u_e2().l, *e.u_e2().r),
                                 expectedType);
        case IRBinaryOpType::LE:
            return typedFromBool(newBinaryOp(E_LE, *e.u_e2().l, *e.u_e2().r),
                                 expectedType);
        case IRBinaryOpType::GE:
            return typedFromBool(newBinaryOp(E_GE, *e.u_e2().l, *e.u_e2().r),
                                 expectedType);
        case IRBinaryOpType::EQ:
            return typedFromBool(newBinaryOp(E_EQ, *e.u_e2().l, *e.u_e2().r),
                                 expectedType);
        case IRBinaryOpType::NE:
            return typedFromBool(newBinaryOp(E_NE, *e.u_e2().l, *e.u_e2().r),
                                 expectedType);
        }
        hassert(false);
        break;
    }
    case IRExprTypeKind::UnaryOp: {
        switch (e.u_e1().op_type) {
        case IRUnaryOpType::Not:
            return typedFromInt(newUnaryOp(E_NOT, *e.u_e1().l), expectedType);
        case IRUnaryOpType::UnaryMinus:
            return typedFromInt(newUnaryOp(E_UMINUS, *e.u_e1().l),
                                expectedType);
        }
        hassert(false);
        break;
    }
    case IRExprTypeKind::Query: {
        Expr *result = newExprStruct();
        result->type = E_QUERY;
        result->u.e.l =
            new_expr_from_irexpr(*e.u_query().selector, ActExprIntType::Bool);
        result->u.e.r = newBinaryOp(E_COLON, *e.u_query().l, *e.u_query().r);
        return typedFromInt(result, expectedType);
    }

    case IRExprTypeKind::Const: {
        // TODO turn back to E_TRUE or E_FALSE if needed
        // TODO this is incorrect!. This should return a constant wrapped in a
        // lengthen/shorten, right? if (e.u.cons.v_width != 32) {
        Expr *result = newExprStruct();
        result->type = E_BUILTIN_INT;
        NEW(result->u.e.l, ActExprStruct);
        result->u.e.l->type = E_INT;
        hassert(e.u_cons().v <= BigInt{0xffffffffffffffff});
        result->u.e.l->u.ival.v =
            e.u_cons().v.getUI64(); // TODO implement e.u.v_extra
        result->u.e.l->u.ival.v_extra = nullptr;
        NEW(result->u.e.r, ActExprStruct);
        result->u.e.r->type = E_INT;
        result->u.e.r->u.ival.v = e.u_cons().v_width;
        result->u.e.r->u.ival.v_extra = nullptr;
        return typedFromInt(result, expectedType);
    }
    case IRExprTypeKind::Var: {
        Expr *result = newExprStruct();
        result->type = E_VAR;
        // TODO normalize this into a VarId{} here
        ActId *id = actid_from_varid(e.u_var().id);
        result->u.e.l = (Expr *)(id); // this is really an (ActId*)
        return typedFromInt(result,
                            expectedType); // we only support "int" variables
    }
    case IRExprTypeKind::Bitfield: {
        if (e.u_bitfield().slice.lo() == 0) {
            Expr *result = newExprStruct();
            result->type = E_BUILTIN_INT;
            result->u.e.l =
                new_expr_from_irexpr(*e.u_bitfield().e, ActExprIntType::Int);
            NEW(result->u.e.r, ActExprStruct);
            result->u.e.r->type = E_INT;
            result->u.e.r->u.ival.v = e.u_bitfield().ct();
            result->u.e.r->u.ival.v_extra = nullptr;
            return typedFromInt(result, expectedType);
        } else {
            Expr *result = newExprStruct();
            result->type = E_BITFIELD;
            // can only decode an ir tree if bitfields are only appled
            // to variable expressions. Run the final pass first!
            hassert(e.u_bitfield().e->type() == IRExprTypeKind::Var);
            ActId *id = actid_from_varid(e.u_bitfield().e->u_var().id);
            result->u.e.l = (Expr *)id;
            NEW(result->u.e.r, ActExprStruct);
            result->u.e.r->type = E_BITFIELD;
            NEW(result->u.e.r->u.e.l, ActExprStruct);
            result->u.e.r->u.e.l->type = E_INT;
            result->u.e.r->u.e.l->u.ival.v = e.u_bitfield().lo();
            result->u.e.r->u.e.l->u.ival.v_extra = nullptr;
            NEW(result->u.e.r->u.e.r, ActExprStruct);
            result->u.e.r->u.e.r->type = E_INT;
            result->u.e.r->u.e.r->u.ival.v = e.u_bitfield().hi();
            result->u.e.r->u.e.r->u.ival.v_extra = nullptr;
            return typedFromInt(result, expectedType);
        }
        hassert(false);
    }
    }
    hassert(false);
    return nullptr;
}

} // namespace ChpOptimize
