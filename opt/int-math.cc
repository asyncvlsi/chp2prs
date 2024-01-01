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

#include "int-math.h"
#include "chp-graph.h"

namespace ChpOptimize {

namespace {

BitStateArr addBitStateVectors(const BitStateArr &vec_a,
                               const BitStateArr &vec_b, int num_bits) {
    // do bit long addition
    auto add3 = [](BitState a, BitState b, BitState c) {
        int num_can_be_on =
            (a != BitState::off) + (b != BitState::off) + (c != BitState::off);
        int num_can_be_off =
            (a != BitState::on) + (b != BitState::on) + (c != BitState::on);
        int num_must_be_on = 3 - num_can_be_off;
        bool can_be_on = num_can_be_on == 1 || num_can_be_on == 3 ||
                         (num_can_be_on == 2 && num_must_be_on <= 1);
        bool must_be_on =
            (num_must_be_on == 1 && num_can_be_on == 1) || num_must_be_on == 3;
        return std::pair<BitState, BitState>{
            can_be_on ? (must_be_on ? BitState::on : BitState::unknown)
                      : BitState::off,
            (num_can_be_on >= 2)
                ? ((num_must_be_on >= 2) ? BitState::on : BitState::unknown)
                : BitState::off};
    };

    BitStateArr result = BitStateArr(num_bits);
    BitState carry = BitState::off;
    for (int i = 0; i < num_bits; ++i) {
        BitState a = i < vec_a.ssize() ? vec_a.at(i) : BitState::off;
        BitState b = i < vec_b.ssize() ? vec_b.at(i) : BitState::off;
        auto [bit, carry_] = add3(a, b, carry);
        result.set(i, bit);
        carry = carry_;
    }

    return result;
}
BitStateArr invertBits(const BitStateArr &arr) {
    BitStateArr bit_states(arr.size());
    for (int i = 0; i < arr.ssize(); ++i) {
        BitState a = arr.at(i);
        bit_states.set(
            i, a == BitState::on
                   ? BitState::off
                   : (a == BitState::off ? BitState::on : BitState::unknown));
    }
    return bit_states;
}
BitStateArr unaryMinus(const BitStateArr &arr) {
    return addBitStateVectors(invertBits(arr), BitStateArr{{BitState::on}},
                              (int)arr.ssize());
}

BitStateArr multiplyBitStateVectors(const BitStateArr &vec_a,
                                    const BitStateArr &vec_b, int num_bits) {
    auto offset = [](const BitStateArr &arr, int i) {
        auto r = BitStateArr(arr.size() + i);
        for (int j = 0; j < arr.ssize(); ++j)
            r.set(j + i, arr.at(j));
        return r;
    };
    auto offset_on_to_unknown = [](const BitStateArr &arr, int i) {
        auto r = BitStateArr(arr.size() + i);
        for (int j = 0; j < arr.ssize(); ++j)
            r.set(j + i, arr.off_at(j) ? BitState::off : BitState::unknown);
        return r;
    };

    // TODO this can be done better
    auto acc = BitStateArr(num_bits);
    for (int i = 0; i < num_bits; ++i) {
        BitState a = i < vec_a.ssize() ? vec_a.at(i) : BitState::off;
        switch (a) {
        case BitState::off:
            break;
        case BitState::on:
            acc = addBitStateVectors(acc, offset(vec_b, i), num_bits);
            break;
        case BitState::unknown:
            acc = addBitStateVectors(acc, offset_on_to_unknown(vec_b, i),
                                     num_bits);
            break;
        }
    }

    return acc;
}
} // namespace

IntLattice IntLattice::withBitfield(const BitSlice &slice) const {
    // Compute the bit states
    BitStateArr bit_states(slice.ct());
    for (ssize_t i = 0; i < slice.ct(); ++i) {
        ssize_t j = i + slice.lo();
        bit_states.set(i, (0 <= j && j < m_bit_states.ssize())
                              ? m_bit_states.at(j)
                              : BitState::off);
    }

    auto bit_lattice = IntLattice::of_bits(bit_states);

    // TODO generalize this
    if ((slice.lo() == 0) &&
        ((slice.ct() >= m_bitwidth) || m_max < BigInt::pow_2(slice.ct()))) {
        return bit_lattice & IntLattice::of_minmax(m_min, m_max, slice.ct());
    }
    if ((slice.lo() == 0) && ((m_min >> slice.ct()) == (m_max >> slice.ct()))) {
        return bit_lattice &
               IntLattice::of_minmax(m_min & BigInt::pow_2_minus_1(slice.ct()),
                                     m_max & BigInt::pow_2_minus_1(slice.ct()),
                                     slice.ct());
    }

    return bit_lattice;
}

/*static*/ IntLattice IntLattice::of_bitwidth(int width) {
    BitStateArr bit_states = BitStateArr(width, BitState::unknown);
    return IntLattice(BigInt{0}, BigInt::pow_2_minus_1(width), width,
                      bit_states);
}
/*static*/ IntLattice IntLattice::of_constant(const BigInt &val, int width) {
    BitStateArr bit_states = BitStateArr(width, BitState::off);
    for (int i = 0; i < width; ++i) {
        bit_states.set(i, val.bit_on_at(i) ? BitState::on : BitState::off);
    }
    return IntLattice(val, val, width, bit_states);
}
/*static*/ IntLattice IntLattice::of_minmax(const BigInt &min,
                                            const BigInt &max, int width) {
    hassert(min <= max);
    hassert(max <= BigInt::pow_2_minus_1(width));
    BitStateArr bit_states = BitStateArr(width);
    for (int i = 0; i < width; ++i)
        bit_states.set(i,
                       ((min >> i) == (max >> i))
                           ? (min.bit_on_at(i) ? BitState::on : BitState::off)
                           : BitState::unknown);
    return IntLattice(min, max, width, bit_states);
}
/*static*/ IntLattice IntLattice::of_bits(BitStateArr bit_states) {
    BigInt min = BigInt::from_bits((int)bit_states.size(), [&](const int idx) {
        return bit_states.on_at(idx);
    });
    BigInt max = BigInt::from_bits((int)bit_states.size(), [&](const int idx) {
        return bit_states.maybe_on_at(idx);
    });
    return IntLattice(min, max, (int)bit_states.size(), bit_states);
}

/*static*/ IntLattice IntLattice::union_(const IntLattice &a,
                                         const IntLattice &b) {
    hassert(a.bitwidth() == b.bitwidth());

    auto bits = BitStateArr(a.bitwidth());
    for (ssize_t i = 0; i < bits.ssize(); ++i)
        bits.set(i, a.bits().at(i) == b.bits().at(i) ? a.bits().at(i)
                                                     : BitState::unknown);

    auto new_min = std::min(a.m_min, b.m_min);
    auto new_max = std::max(a.m_max, b.m_max);
    return IntLattice::of_minmax(new_min, new_max, a.bitwidth()) &
           IntLattice::of_bits(bits);
}

/*static*/ IntLattice IntLattice::intersection(const IntLattice &a,
                                               const IntLattice &b) {
    hassert(a.bitwidth() == b.bitwidth());

    auto bits = BitStateArr(a.bitwidth());
    for (ssize_t i = 0; i < bits.ssize(); ++i) {
        if (a.bits().at(i) != BitState::unknown)
            hassert(b.bits().at(i) == BitState::unknown ||
                    a.bits().at(i) == b.bits().at(i));
        bits.set(i, a.bits().at(i) == BitState::unknown ? b.bits().at(i)
                                                        : a.bits().at(i));
    }

    auto new_min = std::max(a.m_min, b.m_min);
    auto new_max = std::min(a.m_max, b.m_max);
    return IntLattice(new_min, new_max, a.bitwidth(), bits);
}

IntLattice applyBinaryOp(IRBinaryOpType op, const IntLattice &left,
                         const IntLattice &right) {
    switch (op) {
    case IRBinaryOpType::Plus: {
        int num_bits = std::max(left.bitwidth(), right.bitwidth()) + 1;
        return IntLattice::of_minmax(left.min() + right.min(),
                                     left.max() + right.max(), num_bits) &
               IntLattice::of_bits(
                   addBitStateVectors(left.bits(), right.bits(), num_bits));
    }
    case IRBinaryOpType::Minus: {
        int num_bits = std::max(left.bitwidth(), right.bitwidth()) + 1;
        auto bits = addBitStateVectors(
            left.bits(), unaryMinus(right.bits().withWidth(num_bits)),
            num_bits);

        if (left.min() >= right.max()) {
            auto new_min =
                BigInt::subtract_assert_no_underflow(left.min(), right.max());
            auto new_max =
                BigInt::subtract_assert_no_underflow(left.max(), right.min());
            // We know it cannot underflow
            return IntLattice::of_minmax(new_min, new_max, num_bits) &
                   IntLattice::of_bits(bits);
        } else if (left.max() < right.min()) {
            auto new_min =
                BigInt::subtract_assert_no_underflow(right.min(), left.max())
                    .get_twos_complement_for_word_size(
                        detail::div_round_up(num_bits, BigInt::bitsPerWord))
                    .get_with_cleared_bits_geq_bitnum(num_bits);
            auto new_max =
                BigInt::subtract_assert_no_underflow(right.max(), left.min())
                    .get_twos_complement_for_word_size(
                        detail::div_round_up(num_bits, BigInt::bitsPerWord))
                    .get_with_cleared_bits_geq_bitnum(num_bits);
            // We know it must underflow
            return IntLattice::of_minmax(new_min, new_max, num_bits) &
                   IntLattice::of_bits(bits);
        }

        return IntLattice::of_bits(bits);
    }
    case IRBinaryOpType::Mult: {
        auto num_bits = left.bitwidth() + right.bitwidth();
        return IntLattice::of_minmax(left.min() * right.min(),
                                     left.max() * right.max(), num_bits) &
               IntLattice::of_bits(multiplyBitStateVectors(
                   left.bits(), right.bits(), num_bits));
    }
    case IRBinaryOpType::Div:
        // we limit the range to be != 0. It would be undefined behavior to ever
        // divide by 0 in the CHP code
        return IntLattice::of_minmax(
            left.min() / std::max(BigInt(1), right.max()),
            left.max() / std::max(BigInt(1), right.min()), left.bitwidth());

    case IRBinaryOpType::LT:
        if (left.max() < right.min())
            return IntLattice::of_constant(BigInt(true), 1);
        if (left.min() >= right.max())
            return IntLattice::of_constant(BigInt(false), 1);
        return IntLattice::of_bitwidth(1);
    case IRBinaryOpType::GT:
        if (left.min() > right.max())
            return IntLattice::of_constant(BigInt(true), 1);
        if (left.max() <= right.min())
            return IntLattice::of_constant(BigInt(false), 1);
        return IntLattice::of_bitwidth(1);
    case IRBinaryOpType::LE:
        if (left.max() <= right.min())
            return IntLattice::of_constant(BigInt(true), 1);
        if (left.min() > right.max())
            return IntLattice::of_constant(BigInt(false), 1);
        return IntLattice::of_bitwidth(1);
    case IRBinaryOpType::GE:
        if (left.min() >= right.max())
            return IntLattice::of_constant(BigInt(true), 1);
        if (left.max() < right.min())
            return IntLattice::of_constant(BigInt(false), 1);
        return IntLattice::of_bitwidth(1);
    case IRBinaryOpType::NE:
    case IRBinaryOpType::EQ: {
        bool same_val = IRBinaryOpType(op) == IRBinaryOpType::EQ;

        if (left.max() <= right.min() && left.min() >= right.max())
            return IntLattice::of_constant(BigInt(same_val), 1);
        if (left.min() > right.max() || left.max() < right.min())
            return IntLattice::of_constant(BigInt(!same_val), 1);
        for (ssize_t i = 0; i < std::max(left.bitwidth(), right.bitwidth());
             ++i) {
            BitState a =
                i < left.bitwidth() ? left.bits().at(i) : BitState::off;
            BitState b =
                i < right.bitwidth() ? right.bits().at(i) : BitState::off;
            if ((a == BitState::on && b == BitState::off) ||
                (a == BitState::off && b == BitState::on))
                return IntLattice::of_constant(BigInt(!same_val), 1);
        }
        return IntLattice::of_bitwidth(1);
    }

    case IRBinaryOpType::Or: {
        int num_bits = std::max(left.bitwidth(), right.bitwidth());
        BitStateArr bit_states = BitStateArr(num_bits);
        for (int i = 0; i < num_bits; ++i) {
            BitState a =
                i < left.bitwidth() ? left.bits().at(i) : BitState::off;
            BitState b =
                i < right.bitwidth() ? right.bits().at(i) : BitState::off;
            BitState state =
                a == BitState::on || b == BitState::on     ? BitState::on
                : a == BitState::off && b == BitState::off ? BitState::off
                                                           : BitState::unknown;
            bit_states.set(i, state);
        }
        auto new_min = std::max(left.min(), right.min());
        auto new_max =
            std::min(left.max() + right.max(), BigInt::pow_2_minus_1(num_bits));
        return IntLattice::of_bits(bit_states) &
               IntLattice::of_minmax(new_min, new_max, num_bits);
    }
    case IRBinaryOpType::Xor: {
        int num_bits = std::max(left.bitwidth(), right.bitwidth());
        BitStateArr bit_states = BitStateArr(num_bits);
        for (int i = 0; i < num_bits; ++i) {
            BitState a =
                i < left.bitwidth() ? left.bits().at(i) : BitState::off;
            BitState b =
                i < right.bitwidth() ? right.bits().at(i) : BitState::off;
            BitState state = (a == BitState::on && b == BitState::off) ||
                                     (a == BitState::off && b == BitState::on)
                                 ? BitState::on
                             : (a == BitState::on && b == BitState::on) ||
                                     (a == BitState::off && b == BitState::off)
                                 ? BitState::off
                                 : BitState::unknown;
            bit_states.set(i, state);
        }
        return IntLattice::of_bits(bit_states);
    }
    case IRBinaryOpType::And: {
        int num_bits = std::max(left.bitwidth(), right.bitwidth());
        BitStateArr bit_states = BitStateArr(num_bits, BitState::off);
        for (int i = 0; i < std::min(left.bitwidth(), right.bitwidth()); ++i) {
            BitState a = left.bits().at(i);
            BitState b = right.bits().at(i);
            BitState state =
                a == BitState::on && b == BitState::on     ? BitState::on
                : a == BitState::off || b == BitState::off ? BitState::off
                                                           : BitState::unknown;
            bit_states.set(i, state);
        }
        auto new_min = BigInt{0} /*TODO*/;
        auto new_max = std::min(left.max(), right.max());
        return IntLattice::of_bits(bit_states) &
               IntLattice::of_minmax(new_min, new_max, num_bits);
    }
    case IRBinaryOpType::LeftShift: {
        int num_bits = ChpExpr::widthAfterBinaryOp(
            IRBinaryOpType::LeftShift, left.bitwidth(), right.bitwidth());
        BitStateArr bit_states = BitStateArr(num_bits, BitState::off);

        int rmin = right.min().getI32(), rmax = right.max().getI32();
        for (int i = 0; i < num_bits; ++i) {
            // TODO deal with really huge shifts?
            // TODO use better algorithm
            BitState bit = left.bits().atq(i - rmin);
            for (int shift = rmin + 1; shift <= rmax; ++shift) {
                bit = bit == BitState::unknown
                          ? BitState::unknown
                          : (bit == left.bits().atq(i - shift)
                                 ? bit
                                 : BitState::unknown);
            }
            bit_states.set(i, bit);
        }
        return IntLattice::of_bits(bit_states) &
               IntLattice::of_minmax(
                   left.min() << right.min().getI32(),
                   std::min(left.max() << right.max().getI32(),
                            BigInt ::pow_2_minus_1(num_bits)),
                   num_bits);
    }
    case IRBinaryOpType::RightShift: {
        int num_bits = ChpExpr::widthAfterBinaryOp(
            IRBinaryOpType::RightShift, left.bitwidth(), right.bitwidth());
        BitStateArr bit_states = BitStateArr(num_bits, BitState::off);

        int rmin = right.min().getI32(), rmax = right.max().getI32();
        for (int i = 0; i < num_bits; ++i) {
            // TODO deal with really huge shifts?
            // TODO use better algorithm
            BitState bit = left.bits().atq(i + rmin);
            for (int shift = rmin + 1; shift <= rmax; ++shift) {
                bit = bit == BitState::unknown
                          ? BitState::unknown
                          : (bit == left.bits().atq(i - shift)
                                 ? bit
                                 : BitState::unknown);
            }
            bit_states.set(i, bit);
        }
        return IntLattice::of_bits(bit_states) &
               IntLattice::of_minmax(left.min() >> right.max().getI32(),
                                     left.max() >> right.min().getI32(),
                                     num_bits);
    }
    case IRBinaryOpType::ArithmeticRightShift: {
        // TODO
        int result_width =
            ChpExpr::widthAfterBinaryOp(IRBinaryOpType::ArithmeticRightShift,
                                        left.bitwidth(), right.bitwidth());
        return IntLattice::of_bitwidth(result_width);
    }
    case IRBinaryOpType::Concat: {
        const auto &rbs = right.bits();
        const auto &lbs = left.bits();
        BitStateArr bit_states = BitStateArr(rbs.size() + lbs.size());
        for (int i = 0; i < rbs.ssize(); ++i) {
            bit_states.set(i, rbs.at(i));
        }
        for (int i = 0; i < lbs.ssize(); ++i) {
            bit_states.set(i + rbs.ssize(), lbs.at(i));
        }
        return IntLattice::of_bits(bit_states);
    }
    case IRBinaryOpType::Mod:
        // TODO this could be much better
        auto num_bits = ChpExpr ::widthAfterBinaryOp(
            IRBinaryOpType::Mod, left.bitwidth(), right.bitwidth());
        return IntLattice::of_bitwidth(num_bits) &
               IntLattice::of_minmax(
                   BigInt{0}, std::min(left.max(), right.max()), num_bits);
    }
    hassert(0);
    return IntLattice(); // This shouldnt be reachable
}

IntLattice applyUnaryOp(IRUnaryOpType op, const IntLattice &left) {
    switch (op) {
    case IRUnaryOpType::Not:
        return IntLattice::of_bits(invertBits(left.bits()));
    case IRUnaryOpType::UnaryMinus: {
        auto bits = unaryMinus(left.bits());
        // negating 0 brings it back to itself, so the normal method of
        // "flipping" the range does not work. If max()
        // !=0, then the range includes 0 and -1, and so is the entire range (at
        // least until we support multi-ranges)
        if (left.min() == BigInt{0}) {
            if (left.max() == BigInt{0})
                return left;
            return IntLattice::of_bits(bits);
        }

        // first negate the bits, then add 1
        BigInt tot = BigInt::pow_2(left.bitwidth());
        BigInt new_min = BigInt::subtract_assert_no_underflow(tot, left.max());
        BigInt new_max = BigInt::subtract_assert_no_underflow(tot, left.min());
        return IntLattice::of_minmax(new_min, new_max, left.bitwidth()) &
               IntLattice::of_bits(bits);
    }
    }
    hassert(false);
    return {}; // This shouldnt be reachable
}

/*static*/ IntLattice
IntLattice::union_(const std::vector<const IntLattice *> &ranges) {
    hassert(!ranges.empty());
    IntLattice result = *ranges[0];
    for (size_t i = 1; i < ranges.size(); ++i)
        result = union_(result, *ranges[i]);
    return result;
}

/*static*/ IntLattice
IntLattice::intersection(const std::vector<const IntLattice *> &ranges) {
    hassert(!ranges.empty());
    IntLattice result = *ranges[0];
    for (size_t i = 1; i < ranges.size(); ++i)
        result = intersection(result, *ranges[i]);
    return result;
}

} // namespace ChpOptimize
