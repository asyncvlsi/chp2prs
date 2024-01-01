/*************************************************************************
 *
 *  This file is part of the ACT library
 *
 *  Copyright (c) 2018-2020 Lincoln Berkley
 *  Copyright (c) 2021-2022 Henry Heffan
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

#include "chp-opt.h"
#include "chp-print.h"
#include "int-math.h"
#include "algos.h"
#include "hassert.h"
#include <map>

namespace ChpOptimize {

namespace {

struct IntLatticeWithCt {
    IntLattice lat;
    int ct = 0;
};

using VarValueTable = std::unordered_map<VarId, IntLatticeWithCt>;

// Given the current set of possible states going into an expression and a scope
// to look up variable names in, compute the possible values of the expression
// [e]. If we request a rewrite, then replace subexpressions with simpler
// constants if possible. Return the evaluation lattice
std::unordered_map<const ChpExprDag::Node *, IntLattice>
evaluate(const VarValueTable &table, const ChpExprDag &dag) {
    std::unordered_map<const ChpExprDag::Node *, IntLattice> evaluations;
    ChpExprDag::iterNodes(dag, [&](const ChpExprDag::Node &orig) {
        switch (orig.type()) {
        case IRExprTypeKind::Var: {
            const IntLattice value = table.at(orig.u_var().id).lat;
            hassert(value.bitwidth() == orig.width);
            evaluations[&orig] = value;
            return;
        }
        case IRExprTypeKind::Const: {
            auto lattice =
                IntLattice::of_constant(orig.u_cons().v, orig.u_cons().v_width);
            hassert(lattice.bitwidth() == orig.width);
            evaluations[&orig] = lattice;
            return;
        }
        case IRExprTypeKind::Query: {
            hassert(orig.type() == IRExprTypeKind::Query);
            const auto &selector_lattice =
                evaluations.at(orig.u_query().selector);
            const auto &lhs_lattice = evaluations.at(orig.u_query().l);
            const auto &rhs_lattice = evaluations.at(orig.u_query().r);
            hassert(selector_lattice.bitwidth() == 1);
            int evaluated_width = ChpExpr::widthAfterQuery(
                lhs_lattice.bitwidth(), rhs_lattice.bitwidth());

            IntLattice lattice;
            hassert(lhs_lattice.bitwidth() == rhs_lattice.bitwidth());
            if (selector_lattice.isConstant() &&
                selector_lattice.constantValue() == BigInt{1}) {
                lattice = lhs_lattice.withWidth(evaluated_width);
            } else if (selector_lattice.isConstant() &&
                       selector_lattice.constantValue() == BigInt{0}) {
                lattice = rhs_lattice.withWidth(evaluated_width);
            } else {
                hassert(lhs_lattice.bitwidth() == rhs_lattice.bitwidth());
                lattice = IntLattice::union_(lhs_lattice, rhs_lattice);
            }
            hassert(lattice.bitwidth() == orig.width);
            evaluations[&orig] = lattice;
            return;
        }
        case IRExprTypeKind::BinaryOp: {
            const auto &lhs_lattice = evaluations.at(orig.u_e2().l);
            const auto &rhs_lattice = evaluations.at(orig.u_e2().r);
            auto lattice =
                applyBinaryOp(orig.u_e2().op_type, lhs_lattice, rhs_lattice);
            hassert(lattice.bitwidth() == orig.width);
            evaluations[&orig] = lattice;
            return;
        }
        case IRExprTypeKind::Bitfield: {
            const auto &lhs_lattice = evaluations.at(orig.u_bitfield().e);
            auto lattice = lhs_lattice.withBitfield(orig.u_bitfield().slice);
            hassert(lattice.bitwidth() == orig.width);
            evaluations[&orig] = lattice;
            return;
        }
        case IRExprTypeKind::UnaryOp: {
            const auto &lhs_lattice = evaluations.at(orig.u_e1().l);
            auto lattice = applyUnaryOp(orig.u_e1().op_type, lhs_lattice);
            hassert(lattice.bitwidth() == orig.width);
            evaluations[&orig] = lattice;
            return;
        }
        }
        hassert(false);
    });
    return evaluations;
}

[[maybe_unused]] bool rewrite_bitfield(
    ChpExprDag::Node &orig, const IntLattice &e_lat,
    const std::unordered_map<const ChpExprDag::Node *, int> &use_cts,
    ChpExprDag &dag) {
    using Node = ChpExprDag::Node;
    if (e_lat.bitwidth() == orig.u_bitfield().ct() &&
        orig.u_bitfield().lo() == 0) {
        orig = Node::shallow_copy(*orig.u_bitfield().e);
        return true;
    } else if (e_lat.bitwidth() <= orig.u_bitfield().slice.lo() ||
               0 > orig.u_bitfield().slice.hi()) {
        orig = Node::makeConstant(BigInt(0), orig.u_bitfield().ct());
        return true;
    } else if (e_lat.bitwidth() <= orig.u_bitfield().slice.hi()) {
        int left_pad_amt = orig.u_bitfield().slice.hi() + 1 - e_lat.bitwidth();
        orig = Node::makeBinaryOp(
            IRBinaryOpType::Concat,
            dag.newNode(Node::makeConstant(BigInt(0), left_pad_amt)),
            dag.newNode(Node::makeBitfield(orig.u_bitfield().e,
                                           e_lat.bitwidth() - 1,
                                           orig.u_bitfield().lo()))

        );
        return true;
    } else if (0 > orig.u_bitfield().slice.lo()) {
        int right_pad_amt = -orig.u_bitfield().slice.lo();
        orig = Node::makeBinaryOp(
            IRBinaryOpType::Concat,
            dag.newNode(Node::makeBitfield(orig.u_bitfield().e,
                                           orig.u_bitfield().hi(), 0)),
            dag.newNode(Node::makeConstant(BigInt(0), right_pad_amt)));
        return true;
    }

    switch (orig.u_bitfield().e->type()) {
    case IRExprTypeKind::BinaryOp: {
        // Otherwise, we can end up with exponential blowup on the sha256
        // example
        if (use_cts.at(orig.u_bitfield().e) > 1)
            return false;

        // We only want to apply yhtis optimization to nodes that are read only
        // once
        switch (orig.u_bitfield().e->u_e2().op_type) {
        case IRBinaryOpType::And:
        case IRBinaryOpType::Or:
        case IRBinaryOpType::Xor: {
            // push the bitfield through the operation
            BitSlice slice = orig.u_bitfield().slice;
            Node *l_sliced = dag.newNode(Node::makeBitfield(
                orig.u_bitfield().e->u_e2().l, slice.hi(), slice.lo()));
            Node *r_sliced = dag.newNode(Node::makeBitfield(
                orig.u_bitfield().e->u_e2().r, slice.hi(), slice.lo()));
            orig = Node::makeBinaryOp(orig.u_bitfield().e->u_e2().op_type,
                                      l_sliced, r_sliced);
            break;
        }
        case IRBinaryOpType::LeftShift:
        case IRBinaryOpType::RightShift:
            // Left and right shifts by a constant get converted to bitfields in
            // the binary_op_rewriting
            break;
        case IRBinaryOpType::Plus: {
        case IRBinaryOpType::Mult:
            // if we have the expression `(a+b){hi..lo}`, we only need to
            // compute `a+b` up to bit `hi`. To do this, we only need to compute
            // `(a{hi..0} + b{hi..0}){hi..lo}. Therefore, if either `a.width >
            // (hi + 1)` or `b.width > (hi + 1)`, we should add a slice onto a
            // and b. Likewise for `(a*b){hi..lo}
            BitSlice slice = orig.u_bitfield().slice;
            Node *l = orig.u_bitfield().e->u_e2().l;
            Node *r = orig.u_bitfield().e->u_e2().r;
            if (l->width > slice.hi() + 1 || r->width > slice.hi() + 1) {
                Node *l_sliced =
                    l->width > slice.hi() + 1
                        ? dag.newNode(Node::makeBitfield(l, slice.hi(), 0))
                        : l;
                Node *r_sliced =
                    r->width > slice.hi() + 1
                        ? dag.newNode(Node::makeBitfield(r, slice.hi(), 0))
                        : r;
                orig.u_bitfield().e = dag.newNode(Node::makeBinaryOp(
                    orig.u_bitfield().e->u_e2().op_type, l_sliced, r_sliced));
                return true;
            }
            break;
        }
        case IRBinaryOpType::ArithmeticRightShift:
        case IRBinaryOpType::Minus:
        case IRBinaryOpType::Div:
        case IRBinaryOpType::Mod:
            // TODO implement these
            break;
        case IRBinaryOpType::LT:
        case IRBinaryOpType::GT:
        case IRBinaryOpType::LE:
        case IRBinaryOpType::GE:
        case IRBinaryOpType::EQ:
        case IRBinaryOpType::NE:
            // As these produce outputs with width 1, any bitfield should be
            // cleaned up by the cases at the top of the function. Therefore, we
            // should never reach this point. If we do, there is a bug at the
            // top of the function.
            hassert(false);
            break;
        case IRBinaryOpType::Concat: {
            // these really only need to calculate bitwidth?
            auto high_expr = orig.u_bitfield().e->u_e2().l;
            auto low_expr = orig.u_bitfield().e->u_e2().r;
            auto low_expr_width = low_expr->width;

            hassert(e_lat.bitwidth() == high_expr->width + low_expr->width);

            if (low_expr_width > orig.u_bitfield().slice.hi()) {
                // only need low bits
                orig =
                    Node::makeBitfield(low_expr, orig.u_bitfield().slice.hi(),
                                       orig.u_bitfield().slice.lo());
            } else if (low_expr_width <= orig.u_bitfield().slice.lo()) {
                // only need high bits
                orig = Node::makeBitfield(
                    high_expr, orig.u_bitfield().slice.hi() - low_expr_width,
                    orig.u_bitfield().slice.lo() - low_expr_width);
            } else {
                // need both
                orig = Node::makeBinaryOp(
                    IRBinaryOpType::Concat,
                    dag.newNode(Node::makeBitfield(
                        high_expr,
                        orig.u_bitfield().slice.hi() - low_expr_width, 0)),
                    dag.newNode(
                        Node::makeBitfield(low_expr, low_expr_width - 1,
                                           orig.u_bitfield().slice.lo())));
            }
            return true;
        }
        }
        return false;
    }
    case IRExprTypeKind::UnaryOp: {

        switch (orig.u_bitfield().e->u_e1().op_type) {
        case IRUnaryOpType::Not: {
            // push the bitfield through the operation
            BitSlice slice = orig.u_bitfield().slice;
            Node *l_sliced = dag.newNode(Node::makeBitfield(
                orig.u_bitfield().e->u_e1().l, slice.hi(), slice.lo()));
            orig = Node::makeUnaryOp(orig.u_bitfield().e->u_e1().op_type,
                                     l_sliced);
            break;
        }
        case IRUnaryOpType::UnaryMinus: {
            break;
        }
        }
        return false;
    }
    case IRExprTypeKind::Query: {
        BitSlice slice = orig.u_bitfield().slice;
        Node *l_sliced = dag.newNode(Node::makeBitfield(
            orig.u_bitfield().e->u_query().l, slice.hi(), slice.lo()));
        Node *r_sliced = dag.newNode(Node::makeBitfield(
            orig.u_bitfield().e->u_query().r, slice.hi(), slice.lo()));
        orig = Node::makeQuery(orig.u_bitfield().e->u_query().selector,
                               l_sliced, r_sliced);
        return false;
    }
    case IRExprTypeKind::Const:
        hassert(e_lat.isConstant());
        // Then this will get cleaned up at the end of the prune function
        return false;
    case IRExprTypeKind::Var:
        return false;
    case IRExprTypeKind::Bitfield: {
        if (orig.u_bitfield().e->u_bitfield().slice.ct() - 1 >=
            orig.u_bitfield().slice.hi()) {
            hassert(orig.type() == IRExprTypeKind::Bitfield);
            // slice{hi1..lo1}{hi2..lo2}   is the same as
            // slice{lo1+hi2..lo1+lo2}
            // TODO we can also rewrite int(int(e, w1), w2) if width(e) <= w1 <=
            // w2
            orig = Node::makeBitfield(
                orig.u_bitfield().e->u_bitfield().e,
                orig.u_bitfield().e->u_bitfield().slice.lo() +
                    orig.u_bitfield().slice.hi(),
                orig.u_bitfield().e->u_bitfield().slice.lo() +
                    orig.u_bitfield().slice.lo());
            hassert(orig.width == orig.u_bitfield().slice.ct());
            hassert(orig.width == orig.u_bitfield().slice.ct());
            return true;
        }
        return false;
    }
    }
    hassert(false);
    return false;
}

enum class BitwiseBinOpBitKind { Zero, One, Lhs, Rhs, Both, InvLhs, InvRhs };
bool rewrite_bitwise_binary_op(
    ChpExprDag::Node &n_orig, const IntLattice &lhs_lattice,
    const IntLattice &rhs_lattice, ChpExprDag &dag,
    const std::function<BitwiseBinOpBitKind(BitState, BitState)>
        &kind_from_bits) {
    // iterate along the sequence of bits. Each bit is either drawn from the
    // "left" array or has value "0" There are 7 possible values for a given
    // bit. It is one of {0, 1, lhs, rhs, ~lhs, ~rhs, both}

    using Node = ChpExprDag::Node;

    const auto &lhs_states = lhs_lattice.bits();
    const auto &rhs_states = rhs_lattice.bits();

    std::vector<Node *> segments;

    auto extending_at = [](const BitStateArr &v, int i) {
        return i >= v.ssize() ? BitState::off : v.at(i);
    };
    BitwiseBinOpBitKind kind0 =
        kind_from_bits(lhs_states.at(0), rhs_states.at(0));
    {

        BitwiseBinOpBitKind kind = kind0;
        int last_idx = 0;
        for (int idx = 1; idx < n_orig.width + 1; ++idx) {
            // we add one extra iteration, and process a segment when we hit the
            // end of the loop
            auto bkind = kind_from_bits(extending_at(lhs_states, idx),
                                        extending_at(rhs_states, idx));
            if (idx == n_orig.width || kind != bkind) {
                // then then set of bits {idx-1..last_idx} forms a range
                int seg_width = idx - last_idx;
                switch (kind) {
                case BitwiseBinOpBitKind::Zero:
                    segments.push_back(
                        dag.newNode(Node::makeConstant(BigInt{0}, seg_width)));
                    break;
                case BitwiseBinOpBitKind::One:
                    segments.push_back(dag.newNode(Node::makeConstant(
                        BigInt::pow_2_minus_1(seg_width), seg_width)));
                    break;
                case BitwiseBinOpBitKind::Lhs:
                    segments.push_back(dag.newNode(Node::makeBitfield(
                        n_orig.u_e2().l, idx - 1, last_idx)));
                    break;
                case BitwiseBinOpBitKind::Rhs:
                    segments.push_back(dag.newNode(Node::makeBitfield(
                        n_orig.u_e2().r, idx - 1, last_idx)));
                    break;
                case BitwiseBinOpBitKind::InvLhs:
                    segments.push_back(dag.newNode(Node::makeBitfield(
                        dag.newNode(Node::makeUnaryOp(IRUnaryOpType::Not,
                                                      n_orig.u_e2().l)),
                        idx - 1, last_idx)));
                    break;
                case BitwiseBinOpBitKind::InvRhs:
                    segments.push_back(dag.newNode(Node::makeBitfield(
                        dag.newNode(Node::makeUnaryOp(IRUnaryOpType::Not,
                                                      n_orig.u_e2().r)),
                        idx - 1, last_idx)));
                    break;
                case BitwiseBinOpBitKind::Both:
                    segments.push_back(dag.newNode(Node::makeBinaryOp(
                        n_orig.u_e2().op_type,
                        dag.newNode(Node::makeBitfield(n_orig.u_e2().l, idx - 1,
                                                       last_idx)),
                        dag.newNode(Node::makeBitfield(n_orig.u_e2().r, idx - 1,
                                                       last_idx)))));
                    break;
                }
                kind = bkind;
                last_idx = idx;
            }
        }
    }

    if (segments.size() > 1 || kind0 != BitwiseBinOpBitKind::Both) {
        Node *n = segments.front();
        for (int i = 1; i < (int)segments.size(); ++i)
            n = dag.newNode(
                Node::makeBinaryOp(IRBinaryOpType::Concat, segments[i], n));
        hassert(n);
        n_orig = Node::shallow_copy(*n);
        return true;
    }
    return false;
}

[[maybe_unused]] bool rewrite_binary_op(ChpExprDag::Node &n_orig,
                                        const IntLattice &lhs_lattice,
                                        const IntLattice &rhs_lattice,
                                        ChpExprDag &dag) {
    using Node = ChpExprDag::Node;

    int orig_width = n_orig.width;
    auto lhs_expr = n_orig.u_e2().l;
    auto rhs_expr = n_orig.u_e2().r;
    const auto &orig = n_orig;

    if (lhs_lattice.isConstant() && !rhs_lattice.isConstant() &&
        isCommutative(orig.u_e2().op_type)) {
        n_orig = Node::makeBinaryOp(orig.u_e2().op_type, rhs_expr, lhs_expr);
        return true;
    }

    // TODO add rewrites that push bit-math operations throguh concats (e.g. {a,
    // b} | c   becomes    {a | c{}, b | c{}} simple binary rewrites
    switch (orig.u_e2().op_type) {
    case IRBinaryOpType::And: {
        return rewrite_bitwise_binary_op(
            n_orig, lhs_lattice, rhs_lattice, dag, [](BitState l, BitState r) {
                if (l == BitState::off || r == BitState::off)
                    return BitwiseBinOpBitKind::Zero;
                else if (l == BitState::on && r == BitState::on)
                    return BitwiseBinOpBitKind::One;
                else if (l == BitState::on && r == BitState::unknown)
                    return BitwiseBinOpBitKind::Rhs;
                else if (l == BitState::unknown && r == BitState::on)
                    return BitwiseBinOpBitKind::Lhs;
                else {
                    hassert(l == BitState::unknown && r == BitState::unknown);
                    return BitwiseBinOpBitKind::Both;
                }
            });
    }
    case IRBinaryOpType::Or: {
        return rewrite_bitwise_binary_op(
            n_orig, lhs_lattice, rhs_lattice, dag, [](BitState l, BitState r) {
                if (l == BitState::on || r == BitState::on)
                    return BitwiseBinOpBitKind::One;
                else if (l == BitState::off && r == BitState::off)
                    return BitwiseBinOpBitKind::Zero;
                else if (l == BitState::off && r == BitState::unknown)
                    return BitwiseBinOpBitKind::Rhs;
                else if (l == BitState::unknown && r == BitState::off)
                    return BitwiseBinOpBitKind::Lhs;
                else {
                    hassert(l == BitState::unknown && r == BitState::unknown);
                    return BitwiseBinOpBitKind::Both;
                }
            });
    }
    case IRBinaryOpType::Xor: {
        return rewrite_bitwise_binary_op(
            n_orig, lhs_lattice, rhs_lattice, dag, [](BitState l, BitState r) {
                if ((l == BitState::off && r == BitState::off) ||
                    (l == BitState::on && r == BitState::on))
                    return BitwiseBinOpBitKind::Zero;
                else if ((l == BitState::off && r == BitState::on) ||
                         (l == BitState::on && r == BitState::off))
                    return BitwiseBinOpBitKind::One;
                else if (l == BitState::off && r == BitState::unknown)
                    return BitwiseBinOpBitKind::Rhs;
                else if (l == BitState::on && r == BitState::unknown)
                    return BitwiseBinOpBitKind::InvRhs;
                else if (l == BitState::unknown && r == BitState::off)
                    return BitwiseBinOpBitKind::Lhs;
                else if (l == BitState::unknown && r == BitState::on)
                    return BitwiseBinOpBitKind::InvLhs;
                else {
                    hassert(l == BitState::unknown && r == BitState::unknown);
                    return BitwiseBinOpBitKind::Both;
                }
            });
    }
    case IRBinaryOpType::Mult: {
        if (rhs_lattice.isConstant() &&
            rhs_lattice.constantValue() == BigInt{1}) {
            n_orig = Node::makeResize(lhs_expr, orig_width);
            return true;
        } else if (rhs_lattice.isConstant() &&
                   BigInt::is_pow_2(rhs_lattice.constantValue())) {
            int shift_amnt = static_cast<int>(
                BigInt::log_2_of_pow_2(rhs_lattice.constantValue()));

            auto lower_padding =
                dag.newNode(Node::makeConstant(BigInt{0}, shift_amnt));

            // under bitwidth rules we need to pad with 1 extra zero at the
            // front
            auto upper_padding = dag.newNode(Node::makeConstant(BigInt{0}, 1));
            auto lower_bits = dag.newNode(Node::makeBinaryOp(
                IRBinaryOpType::Concat, lhs_expr, lower_padding));
            n_orig = Node::makeBinaryOp(IRBinaryOpType::Concat, upper_padding,
                                        lower_bits);
            return true;
        }
        break;
    }
    case IRBinaryOpType::Plus:
    case IRBinaryOpType::Minus: {
        if (rhs_lattice.isConstant() &&
            rhs_lattice.constantValue() == BigInt{0}) {
            n_orig = Node::makeResize(lhs_expr, orig_width);
            return true;
        }
        break;
    }
    case IRBinaryOpType::Div: {
        if (rhs_lattice.isConstant() &&
            rhs_lattice.constantValue() == BigInt{1}) {
            n_orig = Node::makeResize(lhs_expr, orig_width);
            return true;
        } else if (rhs_lattice.isConstant() &&
                   BigInt::is_pow_2(rhs_lattice.constantValue())) {
            auto shift_amnt = dag.newNode(Node::makeConstant(
                BigInt{BigInt::log_2_of_pow_2(rhs_lattice.constantValue())},
                32));
            auto shifted = dag.newNode(Node::makeBinaryOp(
                IRBinaryOpType::RightShift, lhs_expr, shift_amnt));
            n_orig = Node::makeResize(shifted, orig_width);
            return true;
        }
        break;
    }
    case IRBinaryOpType::RightShift: {
        if (rhs_lattice.isConstant()) {
            n_orig = Node::makeBitfield(
                lhs_expr, rhs_lattice.constantValue().getI32() + orig_width - 1,
                rhs_lattice.constantValue().getI32());
            return true;
        }
        break;
    }
    case IRBinaryOpType::LeftShift: {
        if (rhs_lattice.isConstant()) {
            n_orig = Node::makeBitfield(
                lhs_expr,
                -rhs_lattice.constantValue().getI32() + orig_width - 1,
                -(rhs_lattice.constantValue().getI32()));
            return true;
        }
        break;
    }
    case IRBinaryOpType::ArithmeticRightShift:
    case IRBinaryOpType::Mod:
    case IRBinaryOpType::LT:
    case IRBinaryOpType::GT:
    case IRBinaryOpType::LE:
    case IRBinaryOpType::GE:
    case IRBinaryOpType::EQ:
    case IRBinaryOpType::NE:
    case IRBinaryOpType::Concat:
        break;
    }
    // This one optimization is harder to do with a DAG
    //    else if (orig->u_e2().op_type == IRBinaryOpType::Concat) {
    //        if (lhs_expr->type() == IRExprTypeKind::BinaryOp &&
    //        lhs_expr->u_e2().op_type == IRBinaryOpType::Concat) {
    //            // The right-rotate the tree
    //            auto high = lhs_expr->u_e2().l.take();
    //            auto mid = lhs_expr->u_e2().r.take();
    //            auto low = std::move(rhs_expr);
    //
    //            auto concat = std::move(lhs_expr);
    //            *concat = ChpExpr::makeBinaryOp(IRBinaryOpType::Concat,
    //            std::move(mid), std::move(low)); *orig =
    //            ChpExpr::makeBinaryOp(IRBinaryOpType::Concat, std::move(high),
    //            std::move(concat)); changed = true; return std::move(orig);
    //        }
    //    }

    // TODO we can replace right_shift(e, const) with bitfield(e)
    // TODO we can replace left_shift(e, const) with bitfield(e)

    // TODO a special case for strenth reduction of e.g. mult to bit shifts?
    // TODO rewrite bitwise AND / OR as bit concatenation if e.g. we have no
    // overlap in unknown bits?
    //      this is good because it can be treated as a nop in the later
    //      parallelization adding code

    return false;
}

void root_concat_bit_state_helper(ChpExprDag::Node *n, BitStateArr &arr,
                                  int offset) {
    if (n->type() == IRExprTypeKind::Const) {
        for (int i = 0; i < n->width; ++i)
            arr.set(offset + i,
                    n->u_cons().v.bit_on_at(i) ? BitState::on : BitState::off);
    } else if (n->type() == IRExprTypeKind::BinaryOp &&
               n->u_e2().op_type == IRBinaryOpType::Concat) {
        root_concat_bit_state_helper(n->u_e2().l, arr,
                                     offset + n->u_e2().r->width);
        root_concat_bit_state_helper(n->u_e2().r, arr, offset);
    } else {
        for (int i = 0; i < n->width; ++i)
            arr.set(offset + i, BitState::unknown);
    }
}
BitStateArr root_concat_bit_state(ChpExprDag::Node *root) {
    BitStateArr arr(root->width);
    root_concat_bit_state_helper(root, arr, 0);
    return arr;
}

// Given the current set of possible states going into an expression and a scope
// to look up variable names in, compute the possible values of the expression
// [e]. If we request a rewrite, then replace subexpressions with simpler
// constants if possible. Return the evaluation lattice, and also return the
// rewriten expression. If we are not rewriting, the rewritten expression should
// be nullptr This requires that the table values are "stabilized". In other
// words, it assumes the only possible values the variables can ever take on are
// those in the tables. This should be called after flowing is finished. This
// rewrites the expression node "orig" in place. No node may have its underlying
// evaluation changed in the rewriting!
void prune_(
    ChpExprDag::Node &orig,
    std::unordered_set<const ChpExprDag::Node *> &done_nodes,
    const std::unordered_map<const ChpExprDag::Node *, IntLattice> &evaluations,
    const std::unordered_map<const ChpExprDag::Node *, int> &use_cts,
    ChpExprDag &dag, bool &changed) {
    if (done_nodes.count(&orig))
        return;
    done_nodes.insert(&orig);
    using Node = ChpExprDag::Node;
    int orig_width = orig.width;

    const auto &evalLattice = evaluations.at(&orig);

    if (evalLattice.isConstant() && orig.type() != IRExprTypeKind::Const) {
        changed = true;
        orig = Node::makeConstant(evalLattice.constantValue(),
                                  evalLattice.bitwidth());
        return;
    }

    // only do "full constant application on bitfields and roots. TODO add
    // roots!
    //     (orig.type() == IRExprTypeKind::Bitfield &&
    // orig.u_bitfield().e->type() != IRExprTypeKind::Bitfield &&
    // !(orig.u_bitfield().e->type() == IRExprTypeKind::BinaryOp &&
    // orig.u_bitfield().e->u_e2().op_type == IRBinaryOpType::Concat))
    // ||
    if (Algo::contains(dag.roots, &orig) &&
        orig.type() != IRExprTypeKind::Const &&
        root_concat_bit_state(&orig) != evalLattice.bits()) {

        hassert(BitStateArr::any_bit_constant(evalLattice.bits()));
        // then there are some constant bits, so rewrite it to be constant in
        // those bits
        int orig_lo = orig.type() == IRExprTypeKind::Bitfield
                          ? orig.u_bitfield().lo()
                          : 0;
        Node *orig_data = orig.type() == IRExprTypeKind::Bitfield
                              ? orig.u_bitfield().e
                              : dag.newNode(Node::shallow_copy(orig));
        const auto &bit_states = evalLattice.bits();
        auto bit_state_at = [&](int i) {
            return i < bit_states.ssize() ? bit_states.at(i) : BitState::off;
        };
        Node *node = nullptr;
        auto add_slice = [&](int lo, int hi) {
            hassert(lo <= hi);
            Node *nn =
                bit_state_at(hi) == BitState::unknown
                    ? dag.newNode(Node::makeBitfield(orig_data, hi + orig_lo,
                                                     lo + orig_lo))
                    : dag.newNode(Node::makeConstant(
                          BigInt::from_bits(hi - lo + 1,
                                            [&](const int j) {
                                                hassert(bit_state_at(j + lo) !=
                                                        BitState::unknown);
                                                return bit_state_at(j + lo) ==
                                                       BitState::on;
                                            }),
                          hi - lo + 1));
            node = node ? dag.newNode(Node::makeBinaryOp(IRBinaryOpType::Concat,
                                                         nn, node))
                        : nn;
        };

        int last = 0;
        for (int i = 1; i < evalLattice.bitwidth(); ++i) {
            if ((bit_state_at(i - 1) == BitState::unknown) !=
                (bit_state_at(i) == BitState::unknown)) {
                add_slice(last, i - 1);
                last = i;
            }
        }
        hassert(node);
        add_slice(last, evalLattice.bitwidth() - 1);
        hassert(node);
        changed = true;
        orig = Node::shallow_copy(*node);
        return;
    }

    switch (orig.type()) {
    case IRExprTypeKind::Const:
    case IRExprTypeKind::Var:
        return;
    case IRExprTypeKind::Query: {
        prune_(*orig.u_query().selector, done_nodes, evaluations, use_cts, dag,
               changed);
        prune_(*orig.u_query().l, done_nodes, evaluations, use_cts, dag,
               changed);
        prune_(*orig.u_query().r, done_nodes, evaluations, use_cts, dag,
               changed);
        auto selector_lattice = evaluations.at(orig.u_query().selector);
        hassert(selector_lattice.bitwidth() == 1);
        auto lhs_lattice = evaluations.at(orig.u_query().l);
        auto rhs_lattice = evaluations.at(orig.u_query().r);
        int evaled_width = ChpExpr::widthAfterQuery(lhs_lattice.bitwidth(),
                                                    rhs_lattice.bitwidth());
        hassert(evaled_width == evalLattice.bitwidth());

        if (selector_lattice.isConstant() &&
            selector_lattice.constantValue() == BigInt{1}) {
            orig = Node::makeResize(orig.u_query().l, evaled_width);
            changed = true;
        } else if (selector_lattice.isConstant() &&
                   selector_lattice.constantValue() == BigInt{0}) {
            orig = Node::makeResize(orig.u_query().r, evaled_width);
            changed = true;
        }
        return;
    }
    case IRExprTypeKind::BinaryOp: {
        prune_(*orig.u_e2().l, done_nodes, evaluations, use_cts, dag, changed);
        prune_(*orig.u_e2().r, done_nodes, evaluations, use_cts, dag, changed);
        auto lhs_lattice = evaluations.at(orig.u_e2().l);
        auto rhs_lattice = evaluations.at(orig.u_e2().r);

        changed |= rewrite_binary_op(orig, lhs_lattice, rhs_lattice, dag);
        hassert(orig.width == orig_width);
        break;
    }
    case IRExprTypeKind::UnaryOp: {
        prune_(*orig.u_e1().l, done_nodes, evaluations, use_cts, dag, changed);
        auto lhs_lattice = evaluations.at(orig.u_e1().l);

        break;
    }
    case IRExprTypeKind::Bitfield: {
        prune_(*orig.u_bitfield().e, done_nodes, evaluations, use_cts, dag,
               changed);
        auto lhs_lattice = evaluations.at(orig.u_bitfield().e);
        changed |= rewrite_bitfield(orig, lhs_lattice, use_cts, dag);
        hassert(orig.width == orig_width);
        break;
    }
    }

    hassert(orig.width == orig_width);
}

// Given the stabilized set of possible states going into an expression and a
// scope to look up variable names in, propagate constant values within the
// expression. It takes ownership of the passed in ChpExpr * (so the caller no
// longer can access/free that memory), and will return a Expr* that the caller
// takes ownership of. It is possilbe (and indeed likely) that the returned tree
// will share much of the memory of the original tree.
void prune(ChpExprDag &dag, const VarValueTable &table, bool &changed) {
    bool my_changed;
    do {
        my_changed = false;

        const auto &evaluations = evaluate(table, dag);
        std::unordered_set<const ChpExprDag::Node *> done_nodes;

        std::unordered_map<const ChpExprDag::Node *, int> use_cts;
        ChpExprDag ::iterNodes(dag, [&use_cts](const ChpExprDag::Node &n) {
            switch (n.type()) {
            case IRExprTypeKind::BinaryOp:
                use_cts[n.u_e2().l] += 1;
                use_cts[n.u_e2().r] += 1;
                break;
            case IRExprTypeKind::UnaryOp:
                use_cts[n.u_e1().l] += 1;
                break;
            case IRExprTypeKind::Query:
                use_cts[n.u_query().selector] += 1;
                use_cts[n.u_query().l] += 1;
                use_cts[n.u_query().r] += 1;
                break;
            case IRExprTypeKind::Bitfield:
                use_cts[n.u_bitfield().e] += 1;
                break;
            case IRExprTypeKind::Const:
            case IRExprTypeKind::Var:
                break;
            }
        });

        for (auto root : dag.roots) {
            // TODO this should be a smart graph algorithm, as right now this
            // can be exponential
            int orig_width = root->width;
            prune_(*root, done_nodes, evaluations, use_cts, dag, changed);
            hassert(root->width == orig_width);
        }

        ChpExprDag::deduplicate(dag);

        changed |= my_changed;
    } while (my_changed);
}
void prune(ChpExprSingleRootDag &e, const VarValueTable &table, bool &changed) {
    prune(e.m_dag, table, changed);
}

bool substitute(Block *b, const VarValueTable &table) {
    bool changed = false;
    hassert(b);
    switch (b->type()) {
    case BlockType::StartSequence:
    case BlockType::EndSequence:
        hassert(false);
        break;
    case BlockType::Basic:
        if (b->u_basic().stmt.type() == StatementType::Assign) {
            prune(b->u_basic().stmt.u_assign().e, table, changed);
            // fprintf(stderr, "%lu\n",
            // ChpExprDag::deep_copy(b->u_basic().stmt.u_assign().e).raw_nodes_ct());
        } else if (b->u_basic().stmt.type() == StatementType::Send) {
            prune(b->u_basic().stmt.u_send().e, table, changed);
        }
        break;
    case BlockType::Select: {
        for (SelectBranch &gs : b->u_select().branches) {
            if (gs.g.type() != IRGuardType::Else) {
                hassert(gs.g.type() == IRGuardType::Expression);
                prune(gs.g.u_e().e, table, changed);
            }
        }
        break;
    }
    case BlockType::DoLoop: {
        // In the DoLoop for forward flow, the endseq holds the set of possible
        // states after some iteration of the algorithm
        prune(b->u_doloop().guard, table, changed);
        break;
    }
    case BlockType::Par:
        break;
    }
    return changed;
}

void flow(VarValueTable &table, const Sequence &seq, const IdPool &id_pool,
          std::set<Block *> &run_once) {
    // Limit the number of times constant propagation will re-evaluate on each
    // block, to avoid infinite loops.
    constexpr int MAX_REP_CT = 20;

    auto update_cell = [&](VarId id, const IntLattice &val) -> bool {
        auto it = table.find(id);
        if (it == table.end()) {
            table[id] = {val, 1};
            return true;
        }

        IntLattice nval = it->second.ct < MAX_REP_CT
                              ? val
                              : IntLattice::of_bitwidth(val.bitwidth());
        if (it->second.lat != nval) {
            // hassert(val.isSupersetOf(it->second.lat));
            it->second.lat = std::move(nval);
            it->second.ct++;
            return true;
        }
        return false;
    };
    auto update_cell_from = [&](VarId dst, VarId src) -> bool {
        return update_cell(dst, table.at(src).lat);
    };

    auto update_cell_merge_from = [&](VarId dst, VarId src) -> bool {
        auto dst_it = table.find(dst);
        auto src_it = table.find(src);
        hassert(src_it != table.end() || dst_it != table.end());
        if (src_it == table.end())
            return false;
        if (dst_it == table.end())
            return update_cell_from(dst, src);

        return update_cell(
            dst, IntLattice::union_(dst_it->second.lat, src_it->second.lat));
    };

    for (Block *curr = seq.startseq->child(); curr != seq.endseq;
         curr = curr->child()) {
        switch (curr->type()) {
        case BlockType::Basic:
            switch (curr->u_basic().stmt.type()) {
            case StatementType::Assign: {
                const auto &assign = curr->u_basic().stmt.u_assign();
                hassert(assign.ids.size() == assign.e.roots.size());

                auto value_map = evaluate(table, assign.e);
                for (ssize_t i = 0;
                     i < static_cast<ssize_t>(assign.e.roots.size()); ++i) {
                    hassert(id_pool.getBitwidth(assign.ids[i]) ==
                            assign.e.roots[i]->width);
                    update_cell(assign.ids[i], value_map.at(assign.e.roots[i]));
                }
                break;
            }
            case StatementType::Receive: {
                const auto &receive = curr->u_basic().stmt.u_receive();

                if (receive.var) {
                    hassert(id_pool.getBitwidth(*receive.var) ==
                            id_pool.getBitwidth(receive.chan));
                    update_cell(*receive.var,
                                IntLattice::of_bitwidth(
                                    id_pool.getBitwidth(*receive.var)));
                }
                break;
            }
            case StatementType::Send:
                break;
            }
            break;
        case BlockType::Par: {
            bool changed = false;
            // first apply the split
            for (const auto &split : curr->u_par().splits) {
                for (const auto &branch_id : split.branch_ids) {
                    if (branch_id)
                        changed |= update_cell_from(*branch_id, split.pre_id);
                }
            }

            if (changed || !run_once.contains(curr)) {
                // then do all the flows
                run_once.insert(curr);

                for (const auto &branch : curr->u_par().branches)
                    flow(table, branch, id_pool, run_once);

                // Each "merge" only has one input, so really its just a copy
                for (const auto &merge : curr->u_par().merges) {
                    hassert(Algo::count_if(merge.branch_ids,
                                           [](const OptionalVarId &id) {
                                               return static_cast<bool>(id);
                                           }));
                    for (const auto &branch_id : merge.branch_ids) {
                        if (branch_id)
                            update_cell_from(merge.post_id, *branch_id);
                    }
                }
            }
            break;
        }
        case BlockType::Select: {
            bool changed = false;
            // first apply the split
            for (const auto &split : curr->u_select().splits) {
                for (const auto &branch_id : split.branch_ids) {
                    if (branch_id)
                        changed |= update_cell_from(*branch_id, split.pre_id);
                }
            }

            if (changed || !run_once.contains(curr)) {
                // then do all the flows
                run_once.insert(curr);

                // then do all the flows
                for (const auto &branch : curr->u_select().branches)
                    flow(table, branch.seq, id_pool, run_once);

                // Each "merge" only has one input, so really its just a copy
                for (const auto &merge : curr->u_select().merges) {
                    auto lats = Algo::map1<const IntLattice *>(
                        merge.branch_ids, [&](const VarId branch_id) {
                            return &table.at(branch_id).lat;
                        });
                    IntLattice out_lat = IntLattice::union_(lats);
                    update_cell(merge.post_id, out_lat);
                }
            }
            break;
        }
        case BlockType::DoLoop: {
            bool changed = false;

            // first apply the split
            for (const auto &phi : curr->u_doloop().in_phis)
                changed |= update_cell_from(phi.bodyin_id, phi.pre_id);
            for (const auto &phi : curr->u_doloop().loop_phis)
                changed |= update_cell_merge_from(phi.bodyin_id, phi.pre_id);

            if (changed || !run_once.contains(curr)) {
                // then do all the flows
                run_once.insert(curr);

                do {
                    flow(table, curr->u_doloop().branch, id_pool, run_once);

                    // and keep doing it until it stabilizes
                    changed = false;
                    for (const auto &phi : curr->u_doloop().out_phis)
                        changed |=
                            update_cell_from(phi.post_id, phi.bodyout_id);
                    for (const auto &phi : curr->u_doloop().loop_phis) {
                        changed |= update_cell_merge_from(phi.bodyin_id,
                                                          phi.bodyout_id);
                        if (phi.post_id)
                            changed |=
                                update_cell_from(*phi.post_id, phi.bodyout_id);
                    }
                } while (changed);
            }
            break;
        }
        case BlockType::StartSequence:
        case BlockType::EndSequence:
            hassert(false);
            break;
        }
    }
}

VarValueTable flow(const ChpGraph &graph) {
    VarValueTable table;
    std::set<Block *> run_once;
    flow(table, graph.m_seq, graph.id_pool(), run_once);
    return table;
}

} // namespace
bool propagateConstants(ChpGraph &graph) {
    VarValueTable table = flow(graph);

    // do rewriting until it stabilizes. Sometimes a local rewrite enables
    // another local rewrite
    bool ever_changed = false;
    for (Block *b : graph.getLiveBlocks()) {
        bool changed;
        do {
            // Im fairly sure this should be based on the predicessor?
            // Im cahgning it to make it what I think is correct, but its hard
            // to be sure... it used to be `changed = substituteFlow(b,
            // flow_table[b].m_table_stack);`
            changed = substitute(b, table);
            ever_changed |= changed;
        } while (changed);
    }

    graph.validateGraphInvariants();

    return ever_changed;
}
} // namespace ChpOptimize
