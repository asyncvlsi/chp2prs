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

#include "big-int.h"
#include "ir-id.h"
#include "hassert.h"
#include "typed-variants.h"
#include <cstring>
#include <deque>
#include <list>
#include <memory>
#include <optional>
#include <string>
#include <type_traits>


/*
 * This is a cleaned up and (more) type-safe version of the standard
 * expression tree "Expr *". This assumes that functions have been
 * expanded by a prior ACT pass, and various re-write rules have been
 * applied to eliminate a number of different expression types.
 */

namespace ChpOptimize {

enum class ActExprIntType { Int, Bool };

enum class IRBinaryOpType {
    And,
    Or,
    Xor,
    Plus,
    Minus,
    Mult,
    Div,
    Mod,
    LeftShift,
    RightShift,
    ArithmeticRightShift,
    LT,
    GT,
    LE,
    GE,
    EQ,
    NE,
    Concat
};

[[nodiscard]] inline bool isAssociative(IRBinaryOpType op) {
    return op == IRBinaryOpType::And || op == IRBinaryOpType::Or ||
           op == IRBinaryOpType::Xor || op == IRBinaryOpType::Plus ||
           op == IRBinaryOpType::Mult || op == IRBinaryOpType::Concat;
}
[[nodiscard]] inline bool isCommutative(IRBinaryOpType op) {
    return op == IRBinaryOpType::And || op == IRBinaryOpType::Or ||
           op == IRBinaryOpType::Xor || op == IRBinaryOpType::Plus ||
           op == IRBinaryOpType::Mult;
}

enum class IRUnaryOpType {
    Not,
    UnaryMinus,
};

enum class IRExprTypeKind { BinaryOp, UnaryOp, Query, Const, Var, Bitfield,
			    ChanVar, ChanProbe };

class BitSlice {
    int m_hi, m_lo; //  a lo value that is negative means to pad the lower bits
                    //  with zeros

  public:
    BitSlice()
        : m_hi{0}
        , m_lo{0} {}
    BitSlice(int hi, int lo)
        : m_hi{hi}
        , m_lo{lo} {
        hassert(hi >= lo);
    }
    [[nodiscard]] int hi() const { return m_hi; }
    [[nodiscard]] int lo() const { return m_lo; }
    [[nodiscard]] int ct() const { return hi() - lo() + 1; }
};

enum class ManageMemory { no, yes };

/**
 * Tag is used in case you want to have different categories of
 * IRExpr's.
 *
 * VarIdType is used to parameterize this by the variable ID
 * ChanIdType is used to parameterize this by the channel ID
 *
 * If manageMemory is set to true, then the IRExpr wraps all of its
 * internal pointers using std::unique_ptr<>.
 */
  template <typename Tag, typename VarIdType, typename ChanIdType,
	    ManageMemory manageMemory>
struct IRExpr {
    using PtrType = std::conditional_t<manageMemory == ManageMemory::yes,
                                       std::unique_ptr<IRExpr>, IRExpr *>;
  
    static PtrType ptr_copy(const PtrType &p) {
        if constexpr (manageMemory == ManageMemory::yes) {
            return deep_copy_ptr(p.get());
        } else {
            return p;
        }
    }

  public:

    /* An expression has a few variants:
     *   - query, with three sub-pointers
     *   - binary, with two sub-pointers
     *   - unary, with one sub-pointer
     *   - constant
     *   - variable
     *   - bitfield
     *
     *  All the variants have copy and assignment support. Note that
     *  the behavior of these will change depending on whether or not
     *  we manage memory internally or not.
     */

    struct Variant_Query {
        PtrType selector, l, r;

        Variant_Query(PtrType selector, PtrType l, PtrType r)
            : selector{std::move(selector)}
            , l{std::move(l)}
            , r{std::move(r)} {}

        Variant_Query() = default;
        ~Variant_Query() = default;
        Variant_Query(Variant_Query &&) noexcept = default;
        Variant_Query &operator=(Variant_Query &&) noexcept = default;
        Variant_Query(const Variant_Query &o)
            : selector{ptr_copy(o.selector)}
            , l{ptr_copy(o.l)}
            , r{ptr_copy(o.r)} {}
        Variant_Query &operator=(const Variant_Query &o) {
            selector = ptr_copy(o.selector);
            l = ptr_copy(o.l);
            r = ptr_copy(o.r);
            return *this;
        }
    };
  
    struct Variant_Expression2 {
        IRBinaryOpType op_type;
        PtrType l, r;

        Variant_Expression2(IRBinaryOpType op_type, PtrType l, PtrType r)
            : op_type{op_type}
            , l{std::move(l)}
            , r{std::move(r)} {}

        Variant_Expression2()
            : op_type{IRBinaryOpType::And}
            , l{}
            , r{} {}
        ~Variant_Expression2() = default;
        Variant_Expression2(Variant_Expression2 &&) noexcept = default;
        Variant_Expression2 &
        operator=(Variant_Expression2 &&) noexcept = default;
        Variant_Expression2(const Variant_Expression2 &o)
            : op_type{o.op_type}
            , l{ptr_copy(o.l)}
            , r{ptr_copy(o.r)} {}
        Variant_Expression2 &operator=(const Variant_Expression2 &o) {
            op_type = o.op_type;
            l = ptr_copy(o.l);
            r = ptr_copy(o.r);
            return *this;
        }
    };
  
    struct Variant_Expression1 {
        IRUnaryOpType op_type;
        PtrType l;

        Variant_Expression1(IRUnaryOpType op_type, PtrType l)
            : op_type{op_type}
            , l{std::move(l)} {}

        Variant_Expression1()
            : op_type{IRUnaryOpType::Not}
            , l{} {}
        ~Variant_Expression1() = default;
        Variant_Expression1(Variant_Expression1 &&) noexcept = default;
        Variant_Expression1 &
        operator=(Variant_Expression1 &&) noexcept = default;
        Variant_Expression1(const Variant_Expression1 &o)
            : op_type{o.op_type}
            , l{ptr_copy(o.l)} {}
        Variant_Expression1 &operator=(const Variant_Expression1 &o) {
            op_type = o.op_type;
            l = ptr_copy(o.l);
            return *this;
        }
    };
  
    struct Variant_Variable {
        VarIdType id;

        explicit Variant_Variable(VarIdType id)
            : id{id} {}

        Variant_Variable() = default;
        ~Variant_Variable() = default;
        Variant_Variable(Variant_Variable &&) noexcept = default;
        Variant_Variable &operator=(Variant_Variable &&) noexcept = default;
        Variant_Variable(const Variant_Variable &) = default;
        Variant_Variable &operator=(const Variant_Variable &) = default;
    };
  
    struct Variant_Constant {
        BigInt v;
        int v_width;

        Variant_Constant(BigInt v, int v_width)
            : v{std::move(v)}
            , v_width{v_width} {}
        Variant_Constant()
            : v{}
            , v_width{0} {}
        ~Variant_Constant() = default;
        Variant_Constant(Variant_Constant &&) noexcept = default;
        Variant_Constant &operator=(Variant_Constant &&) noexcept = default;
        Variant_Constant(const Variant_Constant &) = default;
        Variant_Constant &operator=(const Variant_Constant &) = default;
    };
  
    struct Variant_Bitfield {
        PtrType e;
        BitSlice
            slice; // take bits {hi, hi-1, ..., lo+1, lo} inclusive on both ends
        Variant_Bitfield(PtrType e, BitSlice slice)
            : e{std::move(e)}
            , slice{slice} {}

        Variant_Bitfield() = default;
        ~Variant_Bitfield() = default;
        Variant_Bitfield(Variant_Bitfield &&) noexcept = default;
        Variant_Bitfield &operator=(Variant_Bitfield &&) noexcept = default;
        Variant_Bitfield(const Variant_Bitfield &o)
            : e{ptr_copy(o.e)}
            , slice{o.slice} {}
        Variant_Bitfield &operator=(const Variant_Bitfield &o) {
            e = ptr_copy(o.e);
            slice = o.slice;
            return *this;
        }

        [[nodiscard]] int hi() const { return slice.hi(); }
        [[nodiscard]] int lo() const { return slice.lo(); }
        [[nodiscard]] int ct() const { return slice.ct(); }
    };

    struct Variant_ChanVariable {
        ChanIdType id;

        explicit Variant_ChanVariable(ChanIdType id)
            : id{id} {}

        Variant_ChanVariable() = default;
        ~Variant_ChanVariable() = default;
        Variant_ChanVariable(Variant_ChanVariable &&) noexcept = default;
        Variant_ChanVariable &operator=(Variant_ChanVariable &&) noexcept = default;
        Variant_ChanVariable(const Variant_ChanVariable &) = default;
        Variant_ChanVariable &operator=(const Variant_ChanVariable &) = default;
    };

    struct Variant_ChanProbe {
        ChanIdType id;

        explicit Variant_ChanProbe(ChanIdType id)
            : id{id} {}

        Variant_ChanProbe() = default;
        ~Variant_ChanProbe() = default;
        Variant_ChanProbe(Variant_ChanProbe &&) noexcept = default;
        Variant_ChanProbe &operator=(Variant_ChanProbe &&) noexcept = default;
        Variant_ChanProbe(const Variant_ChanProbe &) = default;
        Variant_ChanProbe &operator=(const Variant_ChanProbe &) = default;
    };
  

  private:
    /* Use the 8 variants type-safe union */
    using Variant_t = TypedVariant8<
        IRExprTypeKind,
        Variant_Constant, IRExprTypeKind::Const,
        Variant_Query, IRExprTypeKind::Query,
        Variant_Expression2, IRExprTypeKind::BinaryOp,
        Variant_Expression1, IRExprTypeKind::UnaryOp,
        Variant_Variable, IRExprTypeKind::Var,
        Variant_Bitfield, IRExprTypeKind::Bitfield,
        Variant_ChanVariable, IRExprTypeKind::ChanVar,
        Variant_ChanProbe, IRExprTypeKind::ChanProbe
    >;

    Variant_t u;

    IRExpr(Variant_t u_, int width)
        : u{std::move(u_)}
        , width{width} {}

  public:
    int width;			// static bit-width for the expression

    [[nodiscard]] Variant_Constant &u_cons() { return u.u_v0(); }
    [[nodiscard]] const Variant_Constant &u_cons() const { return u.u_v0(); }
  
    [[nodiscard]] Variant_Query &u_query() { return u.u_v1(); }
    [[nodiscard]] const Variant_Query &u_query() const { return u.u_v1(); }
  
    [[nodiscard]] Variant_Expression2 &u_e2() { return u.u_v2(); }
    [[nodiscard]] const Variant_Expression2 &u_e2() const { return u.u_v2(); }
  
    [[nodiscard]] Variant_Expression1 &u_e1() { return u.u_v3(); }
    [[nodiscard]] const Variant_Expression1 &u_e1() const { return u.u_v3(); }
  
    [[nodiscard]] Variant_Variable &u_var() { return u.u_v4(); }
    [[nodiscard]] const Variant_Variable &u_var() const { return u.u_v4(); }
  
    [[nodiscard]] Variant_Bitfield &u_bitfield() { return u.u_v5(); }
    [[nodiscard]] const Variant_Bitfield &u_bitfield() const { return u.u_v5();}

    [[nodiscard]] Variant_ChanVariable &u_chvar() { return u.u_v6(); }
    [[nodiscard]] const Variant_ChanVariable &u_chvar() const { return u.u_v6(); }
  
    [[nodiscard]] Variant_ChanProbe &u_probe() { return u.u_v7(); }
    [[nodiscard]] const Variant_ChanProbe &u_probe() const { return u.u_v7(); }
  
    

    [[nodiscard]] IRExprTypeKind type() const { return u.type(); }

    IRExpr()
        : u{Variant_Constant()}
        , width{0} {}
    ~IRExpr() = default;
  
    IRExpr(IRExpr &&o) noexcept = default;
    IRExpr &operator=(IRExpr &&o) noexcept = default;
    IRExpr(const IRExpr &) = delete;
    IRExpr &operator=(const IRExpr &) = delete;

    /* deep copy should only be used if the memory is internally
       managed */
    static IRExpr deep_copy(const IRExpr &o) {
        static_assert(manageMemory == ManageMemory::yes);
        IRExpr result;
        result.u = o.u;
        result.width = o.width;
        return result;
    }
    static PtrType deep_copy_ptr(const IRExpr *o) {
        static_assert(manageMemory == ManageMemory::yes);
        return o ? std::make_unique<IRExpr>(deep_copy(*o)) : nullptr;
    }

    static IRExpr shallow_copy(const IRExpr &o) {
        static_assert(manageMemory == ManageMemory::no);
        IRExpr result;
        result.u = o.u;
        result.width = o.width;
        return result;
    }

    [[nodiscard]] static IRExpr makeQuery(PtrType selector, PtrType l,
                                          PtrType r) {
        hassert(selector);
        hassert(l);
        hassert(r);
        hassert(selector->width == 1);
        hassert(l->width == r->width);
        int result_width = l->width;
        return IRExpr{Variant_t{Variant_Query{std::move(selector), std::move(l),
                                              std::move(r)}},
                      result_width};
    }

    [[nodiscard]] static IRExpr makeBinaryOp(IRBinaryOpType op, PtrType l,
                                             PtrType r) {
        hassert(l);
        hassert(r);
        int result_width = IRExpr::widthAfterBinaryOp(op, l->width, r->width);
        return IRExpr{
            Variant_t{Variant_Expression2{op, std::move(l), std::move(r)}},
            result_width};
    }

    [[nodiscard]] static IRExpr makeUnaryOp(IRUnaryOpType op, PtrType l) {
        hassert(l);
        int result_width = IRExpr::widthAfterUnaryOp(op, l->width);
        return IRExpr{Variant_t{Variant_Expression1{op, std::move(l)}},
                      result_width};
    }

    [[nodiscard]] static IRExpr makeConstant(BigInt val, int bit_width) {
        return IRExpr{Variant_t{Variant_Constant{std::move(val), bit_width}},
                      bit_width};
    }

    [[nodiscard]] static IRExpr makeVariableAccess(VarIdType id,
                                                   const IdPool &id_pool) {
        int bit_width = id_pool.getBitwidth(id);
        return makeVariableAccess(id, bit_width);
    }
    [[nodiscard]] static IRExpr makeVariableAccess(VarIdType id,
                                                   int bit_width) {
        return IRExpr{Variant_t{Variant_Variable{id}}, bit_width};
    }

    [[nodiscard]] static IRExpr makeChanProbe(ChanIdType id) {
      return IRExpr{Variant_t{Variant_ChanProbe{id}}, 1};
    }

    [[nodiscard]] static IRExpr makeChanVariable(ChanIdType id, int bit_width) {
      return IRExpr{Variant_t{Variant_ChanVariable{id}}, bit_width};
    }

    [[nodiscard]] static IRExpr makeResize(PtrType e, int width) {
        hassert(width >= 1);
        return IRExpr::makeBitfield(std::move(e), width - 1, 0);
    }

    [[nodiscard]] static IRExpr makeBitfield(PtrType e, int hi, int lo) {
        hassert(e);
        hassert(hi >= lo);
        auto slice = BitSlice(hi, lo);
        return IRExpr{Variant_t{Variant_Bitfield{std::move(e), slice}},
                      slice.ct()};
    }

    static int widthAfterQuery(int left_width, int right_width) {
        return std::max(left_width, right_width);
    }

    static int widthAfterBinaryOp(IRBinaryOpType expr_type, int lhs_width,
                                  int rhs_width) {
        switch (expr_type) {
        case IRBinaryOpType::And:
            return ActInt::widthAfterOp(ActIntBinaryOpType::And, lhs_width,
                                        rhs_width);
        case IRBinaryOpType::Xor:
            return ActInt::widthAfterOp(ActIntBinaryOpType::Xor, lhs_width,
                                        rhs_width);
        case IRBinaryOpType::Or:
            return ActInt::widthAfterOp(ActIntBinaryOpType::Or, lhs_width,
                                        rhs_width);
        case IRBinaryOpType::Plus:
            return ActInt::widthAfterOp(ActIntBinaryOpType::Plus, lhs_width,
                                        rhs_width);
        case IRBinaryOpType::Minus:
            return ActInt::widthAfterOp(ActIntBinaryOpType::Minus, lhs_width,
                                        rhs_width);
        case IRBinaryOpType::Mult:
            return ActInt::widthAfterOp(ActIntBinaryOpType::Mult, lhs_width,
                                        rhs_width);
        case IRBinaryOpType::Div:
            return ActInt::widthAfterOp(ActIntBinaryOpType::Div, lhs_width,
                                        rhs_width);
        case IRBinaryOpType::Mod:
            return ActInt::widthAfterOp(ActIntBinaryOpType::Mod, lhs_width,
                                        rhs_width);
        case IRBinaryOpType::LT:
        case IRBinaryOpType::GT:
        case IRBinaryOpType::LE:
        case IRBinaryOpType::GE:
        case IRBinaryOpType::NE:
        case IRBinaryOpType::EQ:
            return 1;
        case IRBinaryOpType::LeftShift:
            hassert(rhs_width < 30);
            return ActInt::widthAfterOp(ActIntBinaryOpType::LeftShift,
                                        lhs_width, rhs_width);
        case IRBinaryOpType::ArithmeticRightShift:
        case IRBinaryOpType::RightShift:
            return ActInt::widthAfterOp(ActIntBinaryOpType::RightShift,
                                        lhs_width, rhs_width);
        case IRBinaryOpType::Concat:
            return lhs_width + rhs_width;
        }
        hassert(false);
        return 0;
    }

    static int widthAfterUnaryOp(IRUnaryOpType expr_type, int lhs_width) {
        switch (expr_type) {
        case IRUnaryOpType::Not:
            return ActInt::widthAfterOp(ActIntUnaryOpType::Not, lhs_width);
        case IRUnaryOpType::UnaryMinus:
            return ActInt::widthAfterOp(ActIntUnaryOpType::UnaryMinus,
                                        lhs_width);
        }
        hassert(false);
        return 0;
    }
};

template <typename Tag, typename VarIdType, typename ChanIdType,
	    ManageMemory manageMemory>
void addIdsUsedByExpr(std::unordered_set<VarIdType> &usages,
                      const IRExpr<Tag, VarIdType, ChanIdType, manageMemory> *e) {
    if (!e)
        return;

    auto get = [](const auto &p) {
        if constexpr (manageMemory == ManageMemory::yes) {
            return p.get();
        } else {
            return p;
        }
    };

    switch (e->type()) {
    case IRExprTypeKind::UnaryOp:
        addIdsUsedByExpr(usages, get(e->u_e1().l));
        break;
    case IRExprTypeKind::BinaryOp:
        addIdsUsedByExpr(usages, get(e->u_e2().l));
        addIdsUsedByExpr(usages, get(e->u_e2().r));
        break;
    case IRExprTypeKind::Var:
        usages.insert(e->u_var().id);
        break;
    case IRExprTypeKind::Bitfield:
        addIdsUsedByExpr(usages, get(e->u_bitfield().e));
        break;
    case IRExprTypeKind::Query:
        addIdsUsedByExpr(usages, get(e->u_query().selector));
        addIdsUsedByExpr(usages, get(e->u_query().l));
        addIdsUsedByExpr(usages, get(e->u_query().r));
        break;
    case IRExprTypeKind::Const:
    case IRExprTypeKind::ChanVar: // this function only handles variables
    case IRExprTypeKind::ChanProbe:  // and not channels that may be used
        break;
    }
}

template <typename Tag, typename VarIdType, typename ChanIdType, ManageMemory manageMemory>
std::unordered_set<VarIdType>
getIdsUsedByExpr(const IRExpr<Tag, VarIdType, ChanIdType, manageMemory> *e) {
    std::unordered_set<VarIdType> usages;
    addIdsUsedByExpr(usages, e);
    return usages;
}

} // namespace ChpOptimize
