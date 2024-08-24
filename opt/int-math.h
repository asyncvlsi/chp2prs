#pragma once
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

#include "hassert.h"
#include <cmath>
#include <utility>
#include <vector>

#include "big-int.h"
#include "ir-expr.h"

/**
 * this file implements math in line with the CHP rules for bitwidth extention.
 * These are the rules for CHP
 *
 * 1) A int constant has bitwidth of 32  (?? Is this right ??)
 * 2) When evaluating an expression, the bitwidth of the result depends both on
 * the bitwidth of the operands and the operation. See the function
 * applyBinaryOp(op, left, right) and applyUnaryOp(op, left, right) for the
 * rules 3) The bitwidth of a variable is the bit-width of the underlying
 * variable 4) When assigning an expression to a variable, the expression is
 * truncated or zero-extened to fit in the variable
 */

namespace ChpOptimize {

enum class BitState { unknown = 0, off = 1, on = 2 };

class BitStateArr {

    Buff<3, uint64_t> m_data; // each value is stored in 2 bits
    size_t m_size;

  public:
    BitStateArr()
        : BitStateArr(0) {}
    explicit BitStateArr(std::vector<BitState> states)
        : BitStateArr{states.size()} {
        for (ssize_t i = 0; i < (ssize_t)states.size(); ++i) {
            set(i, states[i]);
        }
    }
    explicit BitStateArr(size_t size)
        : BitStateArr{size, BitState::off} {}
    BitStateArr(size_t size, BitState value)
        : m_data{}
        , m_size{size} {
        m_data.ensure_size(static_cast<int>(size) / 32 + 1);
        for (ssize_t i = 0; i < (ssize_t)size / 32 + 1; ++i)
            m_data.buf()[i] = 0;
        for (ssize_t i = 0; i < ssize(); ++i)
            set(i, value);
    }

    friend bool operator==(const BitStateArr &x, const BitStateArr &y) {
        return x.m_size == y.m_size &&
               std::equal(x.m_data.buf(), x.m_data.buf() + x.m_size / 32 + 1,
                          y.m_data.buf());
    }
    friend bool operator!=(const BitStateArr &x, const BitStateArr &y) {
        return !(x == y);
    }

    [[nodiscard]] BitState at(size_t i) const {
        hassert(i < m_size);
        return static_cast<BitState>((m_data.buf()[i / 32] >> ((i % 32) * 2)) &
                                     0x3);
    }
    [[nodiscard]] BitState atq(size_t i) const {
        return i < m_size ? at(i) : BitState::off;
    }
    void set(size_t i, BitState s) {
        hassert(i < m_size);
        m_data.buf()[i / 32] &= ~(0x3ULL << ((i % 32) * 2));
        m_data.buf()[i / 32] |= static_cast<uint64_t>(s) << ((i % 32) * 2);
    }
    [[nodiscard]] bool on_at(size_t i) const {
        hassert(i < m_size);
        return at(i) == BitState::on;
    }
    [[nodiscard]] bool off_at(size_t i) const {
        hassert(i < m_size);
        return at(i) == BitState::off;
    }
    [[nodiscard]] bool maybe_on_at(size_t i) const {
        hassert(i < m_size);
        return at(i) != BitState::off;
    }
    [[nodiscard]] bool maybe_off_at(size_t i) const {
        hassert(i < m_size);
        return at(i) != BitState::on;
    }

    [[nodiscard]] static bool any_bit_constant(const BitStateArr &arr) {
        for (int i = 0; i != arr.ssize(); ++i) {
            if (arr.at(i) != BitState::unknown)
                return true;
        }
        return false;
    }

    [[nodiscard]] size_t size() const { return m_size; }
    [[nodiscard]] ssize_t ssize() const { return static_cast<ssize_t>(m_size); }

    [[nodiscard]] BitStateArr
    withWidth(size_t size, BitState default_ = BitState::off) const {
        BitStateArr result{size};
        for (ssize_t i = 0; i < (ssize_t)std::min(size, m_size); ++i)
            result.set(i, at(i));
        for (ssize_t i = (ssize_t)std::min(size, m_size); i < (ssize_t)size;
             ++i)
            result.set(i, default_);
        return result;
    }

  void print(FILE *fp) const {
    for (size_t i = 0; i < size(); i++) {
      switch (at(i)) {
      case BitState::on:
	fprintf (fp, "1");
	break;
      case BitState::off:
	fprintf (fp, "0");
	break;
      case BitState::unknown:
	fprintf (fp, "?");
	break;
      }
    }
  }
};

class IntLattice {
    BigInt m_min, m_max;
    int m_bitwidth = 0;
    // track the set of states for each bit. TODO track the set of states for
    // each block of bits
    BitStateArr m_bit_states;

  public:
    IntLattice() = default;
    IntLattice(BigInt min, BigInt max, int bitwidth, BitStateArr bit_states)
        : m_min{std::move(min)}
        , m_max{std::move(max)}
        , m_bitwidth{bitwidth}
        , m_bit_states{std::move(bit_states)} {
        hassert(m_min <= m_max);
        hassert((ssize_t)m_bit_states.size() == bitwidth);
    }
    [[nodiscard]] static IntLattice of_bitwidth(int bits);
    [[nodiscard]] static IntLattice of_constant(const BigInt &val, int bits);
    [[nodiscard]] static IntLattice of_minmax(const BigInt &min,
                                              const BigInt &max, int bits);
    [[nodiscard]] static IntLattice of_bits(BitStateArr bit_states);

    [[nodiscard]] static IntLattice union_(const IntLattice &a,
                                           const IntLattice &b);
    [[nodiscard]] static IntLattice intersection(const IntLattice &a,
                                                 const IntLattice &b);
    [[nodiscard]] static IntLattice
    union_(const std::vector<const IntLattice *> &ranges);
    [[nodiscard]] static IntLattice
    intersection(const std::vector<const IntLattice *> &ranges);

    [[nodiscard]] friend IntLattice operator|(const IntLattice &a,
                                              const IntLattice &b) {
        return union_(a, b);
    }
    [[nodiscard]] friend IntLattice operator&(const IntLattice &a,
                                              const IntLattice &b) {
        return intersection(a, b);
    }

    friend bool operator==(const IntLattice &a, const IntLattice &b) = default;
    friend bool operator!=(const IntLattice &a, const IntLattice &b) = default;

    [[nodiscard]] BigInt max() const { return m_max; }
    [[nodiscard]] BigInt min() const { return m_min; }
    [[nodiscard]] int bitwidth() const { return m_bitwidth; }
    [[nodiscard]] const BitStateArr &bits() const { return m_bit_states; }

    [[nodiscard]] bool isConstant() const { return min() == max(); }
    [[nodiscard]] BigInt constantValue() const {
        hassert(isConstant());
        return min();
    }

    [[nodiscard]] IntLattice withWidth(int width) const {
        return withBitfield(BitSlice{width - 1, 0});
    }
    [[nodiscard]] IntLattice withBitfield(const BitSlice &slice) const;
};

IntLattice applyBinaryOp(IRBinaryOpType op, const IntLattice &left,
                         const IntLattice &right);
IntLattice applyUnaryOp(IRUnaryOpType op, const IntLattice &left);

} // namespace ChpOptimize
