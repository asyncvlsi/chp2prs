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


#define INT_LATTICE_MAX_BITS 1024

class IntLattice {
    BigInt m_min, m_max;
    int m_bitwidth = 0;
    bool max_is_inf = false;
    // track the set of states for each bit. TODO track the set of states for
    // each block of bits
    BitStateArr m_bit_states;

  public:
    IntLattice() = default;

  /*
   * Create an integer lattice element. The lattice point is
   * represented as an interval [min, max], as well as any information
   * that is known about individual bits. The max value is saturating,
   * and if it is larger than 2^(INT_LATTICE_MAX_BITS)-1, it is set to
   * all 1's and the upper bound is marked as infinity.
   *
   * @param min is the minimum integer value
   * @param max is the maximum integer value
   * @param bitwidth is the bitwidth
   * @param bit_states is a vector about what is known about each bit
   * position
   *
   */ 
   IntLattice(BigInt min, BigInt max, int bitwidth, BitStateArr bit_states)
        : m_min{std::move(min)}
        , m_max{std::move(max)}
        , m_bitwidth{bitwidth}
        , m_bit_states{std::move(bit_states)} {
        if (bitwidth > INT_LATTICE_MAX_BITS) {
	  bool is_inf = false;
	  for (int i=INT_LATTICE_MAX_BITS; i < bitwidth; i++) {
	    if (bit_states.at(i) != BitState::off) {
	      is_inf = true;
	    }
	  }
	  if (is_inf) {
	    // set the max val to all 1's, and mark this as infinity
	    m_max = BigInt::pow_2_minus_1 (INT_LATTICE_MAX_BITS);
	    max_is_inf = true;

	    // only track a subset of bits
	    bitwidth = INT_LATTICE_MAX_BITS;
	    bit_states = std::move(m_bit_states);
	    m_bit_states = BitStateArr (bitwidth);
	    for (int i=0; i < bitwidth; i++) {
	      m_bit_states.set(i, bit_states.at(i));
	    }

	    if (m_min > m_max) {
	      // we may have caused a problem by reducing the value of
	      // max due to it now representing infinity.
	      m_min = m_max;
	    }
	  }
        }
        hassert(m_min <= m_max);
        hassert((ssize_t)m_bit_states.size() == bitwidth);
    }

    /*
     * Creates an integer lattice of the specified bit-width, with
     * nothiing known about all the bits.
     */
    [[nodiscard]] static IntLattice of_bitwidth(int bits);

    /*
     * Creates an integer lattice point corresponding to the specified
     * constant value
     */
    [[nodiscard]] static IntLattice of_constant(const BigInt &val, int bits);
  
    /*
     * Creates an integer lattice point corresponding to the specified
     * [min,max] range. Information about bits is computed from the
     * interval.
     */
    [[nodiscard]] static IntLattice of_minmax(const BigInt &min,
                                              const BigInt &max, int bits);

    /*
     * Creates an integer lattice point corresponding to the specified
     * information about individual bits. The min value and max value
     * are computed from the known information about each bit.
     */
    [[nodiscard]] static IntLattice of_bits(BitStateArr bit_states);

    /*
     * Take the union of the information for the two lattice points
     */
    [[nodiscard]] static IntLattice union_(const IntLattice &a,
                                           const IntLattice &b);
  
    /*
     * Take the intersection of the information for the two lattice points
     */
    [[nodiscard]] static IntLattice intersection(const IntLattice &a,
                                                 const IntLattice &b);
    [[nodiscard]] static IntLattice
    union_(const std::vector<const IntLattice *> &ranges);
  
    [[nodiscard]] static IntLattice
    intersection(const std::vector<const IntLattice *> &ranges);

    /* Operator syntax for union */
    [[nodiscard]] friend IntLattice operator|(const IntLattice &a,
                                              const IntLattice &b) {
        return union_(a, b);
    }
  
    /* Operator syntax for intersection */
    [[nodiscard]] friend IntLattice operator&(const IntLattice &a,
                                              const IntLattice &b) {
        return intersection(a, b);
    }

    friend bool operator==(const IntLattice &a, const IntLattice &b) = default;
    friend bool operator!=(const IntLattice &a, const IntLattice &b) = default;

     /* get ranges */
    [[nodiscard]] BigInt max() const { return m_max; }
    [[nodiscard]] BigInt min() const { return m_min; }
    [[nodiscard]] bool is_inf() const { return max_is_inf; }

    /* get information about individual bits */
    [[nodiscard]] int bitwidth() const { return m_bitwidth; }
    [[nodiscard]] const BitStateArr &bits() const { return m_bit_states; }

    /* if the interval is a single point, then it corresponds to a
       constant */
    [[nodiscard]] bool isConstant() const { return min() == max() && (max_is_inf == false); }

    /* for constant intervals, this provides the value of the constant
     * @return the constant corresponding to the interval
     */
    [[nodiscard]] BigInt constantValue() const {
        hassert(isConstant());
        return min();
    }

    /*
     * @param width is the bit-width of the integer to extract
     * @return the lattice point corresponding to selecting bits
     * 0...width-1
     */
    [[nodiscard]] IntLattice withWidth(int width) const {
        return withBitfield(BitSlice{width - 1, 0});
    }

    /*
     * @param slice is the bit slice to extract {m..n}
     * @return the lattice point corresponding to the specified
     * bitslice
     */
    [[nodiscard]] IntLattice withBitfield(const BitSlice &slice) const;

    /*
     * Make the "max" value of the interval infinite. Note that this
     * does not impact the state corresponding to the bits themselves.
     */
    void mk_max_inf() {
      max_is_inf = true;
      hassert (m_bitwidth == INT_LATTICE_MAX_BITS);
      m_max = BigInt::pow_2_minus_1 (INT_LATTICE_MAX_BITS);
    }
      
};

IntLattice applyBinaryOp(IRBinaryOpType op, const IntLattice &left,
                         const IntLattice &right);
IntLattice applyUnaryOp(IRUnaryOpType op, const IntLattice &left);

} // namespace ChpOptimize
