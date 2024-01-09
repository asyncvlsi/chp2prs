#pragma once
/*************************************************************************
 *
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
#include "string_format.h"
#include <cstdint>
#include <cstring>
#include <limits>
#include <memory>
#include <sstream>
#include <type_traits>

namespace ChpOptimize {
namespace detail {

inline int div_round_up(int i, int m) {
    hassert(i >= 0);
    hassert(m > 0);
    return (i + (m - 1)) / m;
}
inline int round_up_to_mult(int i, int m) {
    hassert(i >= 0);
    hassert(m > 0);
    return div_round_up(i, m) * m;
}

} // namespace detail

// A buffer with small-size optimization. Probably could replace this with some
// boost container if we wanted
template <int32_t imm_ct, typename V> class Buff {
    // TODO experiment with making the immediate thing be of length 2
    union {
        V *buf;
        V v[imm_ct];
    } u;
    int32_t m_len;

  public:
    Buff()
        : m_len{imm_ct} {
        for (int i = 0; i < imm_ct; ++i)
            u.v[i] = V();
    }
    Buff(const Buff &o)
        : m_len{o.m_len} {
        if (m_len == imm_ct) {
            memcpy(u.v, o.u.v, imm_ct * sizeof(V));
        } else {
            u.buf = (V *)malloc(m_len * sizeof(V));
            memcpy(u.buf, o.u.buf, m_len * sizeof(V));
        }
    }
    Buff(Buff &&o)
        : m_len{o.m_len} {
        if (m_len == imm_ct) {
            memcpy(u.v, o.u.v, imm_ct * sizeof(V));
        } else {
            u.buf = o.u.buf;
            o.u.buf = nullptr;
            o.m_len = imm_ct;
        }
    }
    Buff &operator=(const Buff &o) {
        if (m_len > imm_ct)
            free(u.buf);
        m_len = o.m_len;
        if (m_len == imm_ct) {
            memcpy(u.v, o.u.v, imm_ct * sizeof(V));
        } else {
            u.buf = (V *)malloc(m_len * sizeof(V));
            memcpy(u.buf, o.u.buf, m_len * sizeof(V));
        }
        return *this;
    }
    Buff &operator=(Buff &&o) {
        if (m_len > imm_ct)
            free(u.buf);
        m_len = o.m_len;
        if (m_len == imm_ct) {
            memcpy(u.v, o.u.v, imm_ct * sizeof(V));
        } else {
            u.buf = o.u.buf;
            o.u.buf = nullptr;
            o.m_len = imm_ct;
        }
        return *this;
    }
    ~Buff() {
        if (m_len > imm_ct)
            free(u.buf);
    }

    void ensure_size(int len) {
        if (len <= m_len) {
            return;
        }
        if (m_len == imm_ct) {
            V *buf = (V *)malloc(len * sizeof(V));
            memcpy(buf, u.v, imm_ct * sizeof(V));
            u.buf = buf;
        } else {
            u.buf = (V *)realloc(u.buf, len * sizeof(V));
        }
        m_len = len;
    }

    V *buf() { return m_len > imm_ct ? u.buf : u.v; }
    const V *buf() const { return m_len > imm_ct ? u.buf : u.v; }
};

template <typename W> class BigInt_t {
    static_assert(std::is_integral<W>::value && !std::is_signed<W>::value);

    // bool uses_buf;
    uint16_t m_wds; // UNIT_TYPE amount

    Buff<2, W> d;
    // rep. The number is sign-extended to the maximum width of the rep

  public:
    static constexpr int bitsPerWord = sizeof(W) * 8;

    BigInt_t()
        : m_wds{1}
        , d{} {}
    explicit BigInt_t(W val)
        : m_wds{1}
        , d{} {
        d.buf()[0] = val;
    }

    uint16_t getWdCt() const { return m_wds; }

    // if the leading words are zero, cut them off and lower the number of words
    void pack() {
        while (m_wds >= 2 && d.buf()[m_wds - 1] == 0)
            m_wds--;
    }

    template <typename Fn>
    static BigInt_t from_bits(int bit_ct, const Fn &bit_on_at_idx) {
        hassert(bit_ct >= 0);
        BigInt_t result;
        int wd_ct = bit_ct / bitsPerWord + 1;
        result.d.ensure_size(wd_ct);
        result.m_wds = wd_ct;
        for (int idx = 0; idx < wd_ct - 1; ++idx) {
            W it = 1, acc = 0;
            for (int j = bitsPerWord * idx; j < bitsPerWord * (idx + 1);
                 ++j, it <<= 1) {
                if (bit_on_at_idx(j))
                    acc |= it;
            }
            result.d.buf()[idx] = acc;
        }
        {
            W it = 1, acc = 0;
            for (int j = bitsPerWord * (wd_ct - 1); j < bit_ct; ++j, it <<= 1) {
                if (bit_on_at_idx(j))
                    acc |= it;
            }
            result.d.buf()[wd_ct - 1] = acc;
        }
        return result;
    }

    static BigInt_t from_hex(const std::string &inp) {
        //  (must start with 0x)
        if (inp.substr(0, 2) != "0x") {
            hassert(false);
            throw std::invalid_argument{"unknown input format"};
        }

        constexpr int hex_base = 16;
        constexpr int charsPerWord = bitsPerWord / 4;
        char tmp[charsPerWord + 1];

        auto chars_to_uint64_t = [&](const char *s, int len) {
            std::memcpy(tmp, s, len);
            tmp[len] = 0;
            auto res = strtoull(tmp, nullptr, hex_base);
            // this hassertion protects all 3 returns from strtoull
            static_assert(std::is_integral<decltype(res)>::value &&
                          !std::is_signed<decltype(res)>::value &&
                          sizeof(decltype(res)) >= sizeof(W));
            return static_cast<W>(res);
        };

        int char_ct = inp.size() - 2;
        int word_ct = detail::div_round_up(char_ct, charsPerWord);

        BigInt_t result;
        const char *inp_str = inp.c_str() + 2;
        const char *inp_str_end = inp.c_str() + inp.size();
        static_assert(sizeof(W) <= sizeof(uint64_t));

        if (word_ct >= 2) {
            result.m_wds = word_ct;
            result.d.ensure_size(word_ct);

            for (int i = 0; i < word_ct - 1; ++i) {
                result.d.buf()[i] = chars_to_uint64_t(
                    inp_str_end - ((i + 1) * charsPerWord), charsPerWord);
            }
            int chars_final_word = char_ct - (charsPerWord * (word_ct - 1));
            result.d.buf()[word_ct - 1] =
                chars_to_uint64_t(inp_str, chars_final_word);
            result.pack();
        } else {
            hassert(word_ct == 1);
            result.m_wds = 1;
            result.d.buf()[0] = chars_to_uint64_t(inp_str, char_ct);
        }
        return result;
    }
    [[nodiscard]] std::string to_hex_string() const {
        int first_nonzero_idx = [&]() {
            int i = m_wds - 1;
            while (i > 0 && d.buf()[i] == 0)
                i--;
            return i;
        }();

        std::stringstream s;
        s << "0x";
        s << string_format("%lx", d.buf()[first_nonzero_idx]);
        for (int i = first_nonzero_idx - 1; i >= 0; i--) {
            s << string_format("%016lx", d.buf()[i]);
        }
        return s.str();
    }

    [[nodiscard]] W getVal(int n) const {
        hassert(n < m_wds);
        return d.buf()[n];
    }

  private:
    static W add_inc_carry(W a, W b, int &carry) {
        W result = a + b;
        carry += (result < a);
        return result;
    }

  public:
    // TODO seperate the cases out into seperate loops. Also, use intrinsics to
    // remove a bunch of the manual carry stuff and speed this up a lot
    friend BigInt_t &operator+=(BigInt_t &a, const BigInt_t &b) {
        // small int optimization
        if (a.m_wds == 1 && b.m_wds == 1) {
            int carry = 0;

            W sm = add_inc_carry(a.d.buf()[0], b.d.buf()[0], carry);
            a.d.buf()[0] = sm;
            if (carry) {
                hassert(carry == 1);
                a.d.ensure_size(2);
                a.m_wds = 2;
                a.d.buf()[1] = 1;
            }
            return a;
        }

        // otherwise, do a normal multi-word addition

        // make a heap allocated, and extend a to be a buffer of length
        // `max(m_wds, b.m_wds) + 1`
        int max_len = std::max(a.m_wds, b.m_wds);

        // we want to reserve one extra word in case we have a carry bit on the
        // last addition
        a.d.ensure_size(max_len + 1);
        // pad `a` out with zeros
        for (int i = a.m_wds; i < max_len + 1; ++i)
            a.d.buf()[i] = 0;

        // add the two numbers
        int carry = 0;
        for (int i = 0; i < max_len; ++i) {
            // we already padded `a` with zeros so we dont need to do a bounds
            // check
            W a_word = a.d.buf()[i];
            W b_word = i < b.m_wds ? b.d.buf()[i] : 0;

            // TODO use intrinsics for this?
            int carry_out = 0;
            W sm = add_inc_carry(a_word, b_word, carry_out);
            sm = add_inc_carry(sm, carry, carry_out);
            hassert(carry <= 1);

            // save the carry bit, and write the word into the correct spot in
            // `a`
            a.d.buf()[i] = sm;
            hassert(carry_out == 0 || carry_out == 1);
            carry = carry_out;
        }
        // set the length of `a` to be the new number
        a.m_wds = max_len;

        // we carried out of the last addition. We need to add an additional
        // word
        if (carry) {
            a.d.buf()[max_len] = 1;
            a.m_wds += 1;
        }
        a.pack(); // TODO is this ever needed?
        return a;
    }

    friend BigInt_t operator+(const BigInt_t &a, const BigInt_t &b) {
        BigInt_t result = a;
        result += b;
        return result;
    }

    // pre-increment
    friend BigInt_t &operator++(BigInt_t &a) {
        // TODO optimize
        a += BigInt_t{1};
        return a;
    }

    // post-increment
    friend BigInt_t operator++(BigInt_t &a, int) {
        // TODO optimize
        BigInt_t result = a;
        a += BigInt_t{1};
        return result;
    }

  private:
    // dest length must be >= na + nb, we will write to length na + nb
    // This multiplies `a` times `b` and puts the result in `dest`
    static void multiply_bufs(int na, const W *a, int nb, const W *b, W *dest) {
        // we do multiplication by breaking each word into 2 half-words. Each
        // pair of half words can be multiplied without any risk of overflow

        constexpr W lowerMask = ~W(0) >> (bitsPerWord / 2);

        for (int i = 0; i < na + nb; ++i)
            dest[i] = 0;

        for (int i = 0; i < na; ++i) {
            for (int j = 0; j < nb; ++j) {
                // to multiply 2 words, we break them each into two 32-bit
                // integers (the lower bits and the upper bits), multiply the 32
                // bit integers, and then add the result to a accumulator.
                W av = a[i];
                W bv = b[j];
                W al = av & lowerMask, ah = av >> (bitsPerWord / 2);
                W bl = bv & lowerMask, bh = bv >> (bitsPerWord / 2);

                // none of these should be able to overflow. The true product is
                // "(hh << bitsPerWord) + ((lh + hl) << (bitsPerWord / 2)) + ll"
                W ll = al * bl;
                W hl = ah * bl;
                W lh = al * bh;
                W hh = ah * bh;

                // TODO maybe replace with 128 bit multiply?
                int low_carry_out = 0;
                W low =
                    add_inc_carry(ll, lh << (bitsPerWord / 2), low_carry_out);
                low =
                    add_inc_carry(low, hl << (bitsPerWord / 2), low_carry_out);
                W high = hh + (lh >> (bitsPerWord / 2)) +
                         (hl >> (bitsPerWord / 2)) +
                         low_carry_out; // this should not be able to overflow

                // add the low word into the right location
                int carry_low = 0;
                dest[i + j] = add_inc_carry(dest[i + j], low, carry_low);

                // add the high word and the carry from the first operation into
                // the next location
                int carry = 0;
                dest[i + j + 1] =
                    add_inc_carry(dest[i + j + 1], carry_low, carry);
                dest[i + j + 1] = add_inc_carry(dest[i + j + 1], high, carry);

                // then propagate the carrys as far as they need to go
                int k = i + j + 2;
                while (carry != 0) {
                    hassert(k < na + nb);
                    int new_carry = 0;
                    dest[k] = add_inc_carry(dest[k], carry, new_carry);
                    carry = new_carry;
                }
            }
        }
    }

  public:
    friend BigInt_t operator*(const BigInt_t &a, const BigInt_t &b) {
        BigInt_t result;
        if (a.m_wds == 1 && b.m_wds == 1) {
            W tmp[2];
            multiply_bufs(1, a.d.buf(), 1, b.d.buf(), tmp);
            if (tmp[1] == 0) {
                // the result still fits in 1 word
                result.d.buf()[0] = tmp[0];
                result.m_wds = 1;
            } else {
                result.d.ensure_size(2);
                result.d.buf()[0] = tmp[0];
                result.d.buf()[1] = tmp[1];
                result.m_wds = 2;
            }
            return result;
        }

        result.d.ensure_size(a.m_wds + b.m_wds);
        result.m_wds = a.m_wds + b.m_wds;
        multiply_bufs(a.m_wds, a.d.buf(), b.m_wds, b.d.buf(), result.d.buf());
        result.pack();
        return result;
    }

    friend BigInt_t &operator*=(BigInt_t &a, const BigInt_t &b) {
        a = a * b;
        return a;
    }

    friend bool operator==(const BigInt_t &a, const BigInt_t &b) {
        if (a.m_wds == 1 && b.m_wds == 1) {
            return a.d.buf()[0] == b.d.buf()[0];
        }
        // if a.m_wds > b.m_wds, check all the upper words of `a` are 0
        for (int i = b.m_wds; i < a.m_wds; ++i) {
            if (a.d.buf()[i] != 0)
                return false;
        }
        // if b.m_wds > a.m_wds, check all the upper words of `b` are 0
        for (int i = a.m_wds; i < b.m_wds; ++i) {
            if (b.d.buf()[i] != 0)
                return false;
        }
        // check the equality of all the remaining words
        for (int i = 0; i < std::min(a.m_wds, b.m_wds); ++i) {
            if (a.d.buf()[i] != b.d.buf()[i])
                return false;
        }
        return true;
    }

    friend bool operator!=(const BigInt_t &a, const BigInt_t &b) {
        return !(a == b);
    }

    friend bool operator<(const BigInt_t &a, const BigInt_t &b) {
        if (a.m_wds == 1 && b.m_wds == 1) {
            return a.d.buf()[0] < b.d.buf()[0];
        }
        // if a.m_wds > b.m_wds, check all the upper words of `a` are 0
        for (int i = b.m_wds; i < a.m_wds; ++i) {
            if (a.d.buf()[i] != 0)
                return false;
        }
        // if b.m_wds > a.m_wds, check all the upper words of `b` are 0
        for (int i = a.m_wds; i < b.m_wds; ++i) {
            if (b.d.buf()[i] != 0)
                return true;
        }
        // check the equality of all the remaining words
        for (int i = std::min(a.m_wds, b.m_wds) - 1; i >= 0; i--) {
            if (a.d.buf()[i] > b.d.buf()[i])
                return false;
            if (a.d.buf()[i] < b.d.buf()[i])
                return true;
        }
        return false;
    }

    friend bool operator<=(const BigInt_t &a, const BigInt_t &b) {
        return (a < b) || (a == b);
    }
    friend bool operator>(const BigInt_t &a, const BigInt_t &b) {
        return b < a;
    }
    friend bool operator>=(const BigInt_t &a, const BigInt_t &b) {
        return b <= a;
    }

    friend BigInt_t &operator<<=(BigInt_t &a, int amt) {
        hassert(amt >= 0);
        if (amt == 0)
            return a;

        // TODO add a special case for when a.m_wds == 1

        const int offset = amt / bitsPerWord;
        const int shift = amt % bitsPerWord;

        // if there is no shift, then we are just copying the values down the
        // buffer!
        if (shift == 0) {
            a.d.ensure_size(a.m_wds + offset);
            for (int i = a.m_wds - 1; i >= 0; i--) {
                a.d.buf()[i + offset] = a.d.buf()[i];
            }
            for (int i = offset - 1; i >= 0; i--) {
                a.d.buf()[i] = 0;
            }
            a.m_wds = a.m_wds + offset;
            return a;
        }

        // otherwise, we need to shift the words and or together the bits. Note
        // the special cases for the first/last word. The order (high order bits
        // to low order bits) matters so we dont overwrite stuff we still need
        a.d.ensure_size(a.m_wds + offset + 1);
        a.d.buf()[a.m_wds - 1 + offset + 1] =
            a.d.buf()[a.m_wds - 1] >> (bitsPerWord - shift);
        for (int i = a.m_wds - 1; i >= 1; i--) {
            a.d.buf()[i + offset] = (a.d.buf()[i] << shift) |
                                    (a.d.buf()[i - 1] >> (bitsPerWord - shift));
        }
        a.d.buf()[offset] = (a.d.buf()[0] << shift);
        for (int i = offset - 1; i >= 0; i--) {
            a.d.buf()[i] = 0;
        }
        a.m_wds = a.m_wds + offset + 1;
        return a;
    }

    friend BigInt_t operator<<(const BigInt_t &a, int amt) {
        BigInt_t result = a;
        result <<= amt;
        return result;
    }

    friend BigInt_t &operator>>=(BigInt_t &a, int amt) {
        hassert(amt >= 0);
        if (amt == 0)
            return a;

        // TODO add a special case for when a.m_wds == 1

        const int offset = amt / bitsPerWord;
        const int shift = amt % bitsPerWord;

        // if we shift the number so it is 0, then skip all the copying
        if (a.m_wds - offset <= 0) {
            a.d.buf()[0] = 0;
            a.m_wds = 1;
            return a;
        }

        // if there is no shift, then we are just copying the values down the
        // buffer!
        if (shift == 0) {
            for (int i = 0; i < a.m_wds - offset; i++) {
                a.d.buf()[i] = a.d.buf()[i + offset];
            }
            a.m_wds = a.m_wds - offset;
            return a;
        }

        // otherwise, we need to shift the words and or together the bits. Note
        // the special cases for the first/last word. The order (high order bits
        // to low order bits) matters so we dont overwrite stuff we still need
        for (int i = 0; i < a.m_wds - offset - 1; i++) {
            a.d.buf()[i] = (a.d.buf()[i + offset] >> shift) |
                           (a.d.buf()[i + offset + 1] << (bitsPerWord - shift));
        }
        hassert(a.m_wds - offset - 1 >= 0);
        a.d.buf()[a.m_wds - offset - 1] = (a.d.buf()[a.m_wds - 1] >> shift);
        a.m_wds = a.m_wds - offset;
        return a;
    }

    friend BigInt_t operator>>(const BigInt_t &a, int amt) {
        BigInt_t result = a;
        result >>= amt;
        return result;
    }

    void inplace_clear_bits_geq_bitnum(int bitnum) {
        hassert(bitnum >= 0);
        if (bitnum == 0) {
            d.buf()[0] = 0;
            m_wds = 1;
            return;
        }
        int final_wds = detail::div_round_up(bitnum, bitsPerWord);
        if (final_wds > m_wds)
            return;

        m_wds = final_wds;

        // This if is needed to stop `bits_to_clear == bitsPerWord`
        if (bitnum % bitsPerWord == 0)
            return;
        int bits_to_clear = bitsPerWord - (bitnum % bitsPerWord);
        hassert(bits_to_clear < bitsPerWord && bits_to_clear != 0);

        d.buf()[m_wds - 1] &= (~W(0) >> bits_to_clear);
    }

    BigInt_t get_with_cleared_bits_geq_bitnum(int bitnum) const {
        BigInt_t result = *this;
        result.inplace_clear_bits_geq_bitnum(bitnum);
        return result;
    }

    void inplace_invert_bits_for_size(int word_ct) {
        hassert(m_wds <= word_ct);
        // recall that if `B + A = (1 << (word_ct * bitsPerWord))`, then B=~A +
        // 1

        // first we extend the number with zeros
        d.ensure_size(word_ct + 1); // have a plus 1 here so we dont allocate a
                                    // second time in the addition
        for (int i = m_wds; i < word_ct; ++i) {
            d.buf()[i] = 0;
        }
        m_wds = word_ct;
        // then we invert all the bits
        for (int i = 0; i < word_ct; ++i) {
            d.buf()[i] = ~d.buf()[i];
        }
    }

    BigInt_t get_inverted_bits_for_size(int word_ct) const {
        BigInt_t result = *this;
        result.inplace_invert_bits_for_size(word_ct);
        return result;
    }

    // this number must have m_wds <= word_ct
    // If the current value of this number is B, after this operation it will
    // have value A such that `B + A = (1 << (word_ct * bitsPerWord))`
    void inplace_twos_complement_for_size(int word_ct) {
        hassert(m_wds <= word_ct);
        // recall that if `B + A = (1 << (word_ct * bitsPerWord))`, then B=~A +
        // 1

        inplace_invert_bits_for_size(word_ct);

        // then we add 1
        ++(*this);

        // and finally we truncate the number again (in case addition
        // overflowed, which only happens if the number was originally 0)
        m_wds = word_ct;
    }

    BigInt_t get_twos_complement_for_word_size(int word_ct) const {
        BigInt_t result = *this;
        result.inplace_twos_complement_for_size(word_ct);
        return result;
    }

    static void subtract_inplace_assert_no_underflow(BigInt_t &a,
                                                     const BigInt_t &b) {
        // the idea will be to invert b w.r.t to an integer of size a, and then
        // add b to a
        hassert(a >= b);
        int a_wds = a.m_wds;
        a += b.get_twos_complement_for_word_size(
            a.m_wds);    // this (probably will) change a.m_wds
        a.m_wds = a_wds; // truncate a.m_wds
    }

    static BigInt_t subtract_assert_no_underflow(const BigInt_t &a,
                                                 const BigInt_t &b) {
        BigInt_t result = a;
        subtract_inplace_assert_no_underflow(result, b);
        return result;
    }

    struct DivMod {
        BigInt_t div, mod;
    };
    static DivMod divmod(const BigInt_t &a, const BigInt_t &b) {
        if (a.m_wds == 1 && b.m_wds == 1) {
            W va = a.d.buf()[0], vb = b.d.buf()[0];
            return DivMod{BigInt_t(va / vb), BigInt_t(va % vb)};
        }

        BigInt_t tmp_q, tmp_r;
        tmp_q.d.ensure_size(a.m_wds);
        tmp_r.d.ensure_size(a.m_wds);
        for (int i = 0; i < a.m_wds; i++) {
            tmp_q.d.buf()[i] = 0;
            tmp_r.d.buf()[i] = 0;
        }

        for (int k = a.m_wds * bitsPerWord - 1; k >= 0; k--) {
            tmp_q <<= 1;
            tmp_r <<= 1;

            if (((a.d.buf()[k / bitsPerWord] >> (k % bitsPerWord)) & 0x1) ==
                1) {
                tmp_r += BigInt_t{1};
            }
            if (tmp_r >= b) {
                BigInt_t::subtract_inplace_assert_no_underflow(tmp_r, b);
                tmp_q += BigInt_t{1};
            }
        }

        tmp_q.pack();
        tmp_r.pack();
        return DivMod{tmp_q, tmp_r};
    }

  public:
    friend BigInt_t operator/(const BigInt_t &a, const BigInt_t &b) {
        return divmod(a, b).div;
    }
    friend BigInt_t &operator/=(BigInt_t &a, const BigInt_t &b) {
        return (a = a / b);
    }
    friend BigInt_t operator%(const BigInt_t &a, const BigInt_t &b) {
        return divmod(a, b).mod;
    }
    friend BigInt_t &operator%=(BigInt_t &a, const BigInt_t &b) {
        return (a = a % b);
    }

    friend BigInt_t &operator|=(BigInt_t &a, const BigInt_t &b) {
        for (int i = 0; i < std::min(a.m_wds, b.m_wds); ++i) {
            a.d.buf()[i] |= b.d.buf()[i];
        }
        if (a.m_wds < b.m_wds) {
            // extend the number just copying bits from b
            a.d.ensure_size(b.m_wds);
            for (int i = a.m_wds; i < b.m_wds; ++i) {
                a.d.buf()[i] = b.d.buf()[i];
            }
        }
        a.m_wds = std::max(a.m_wds, b.m_wds);
        return a;
    }
    friend BigInt_t &operator&=(BigInt_t &a, const BigInt_t &b) {
        for (int i = 0; i < std::min(a.m_wds, b.m_wds); ++i) {
            a.d.buf()[i] &= b.d.buf()[i];
        }
        a.m_wds = std::min(a.m_wds, b.m_wds);
        a.pack();
        return a;
    }
    friend BigInt_t &operator^=(BigInt_t &a, const BigInt_t &b) {
        for (int i = 0; i < std::min(a.m_wds, b.m_wds); ++i) {
            a.d.buf()[i] ^= b.d.buf()[i];
        }
        if (a.m_wds < b.m_wds) {
            // extend the number just copying bits from b
            a.d.ensure_size(b.m_wds);
            for (int i = a.m_wds; i < b.m_wds; ++i) {
                a.d.buf()[i] = b.d.buf()[i];
            }
        }
        a.m_wds = std::max(a.m_wds, b.m_wds);
        a.pack();
        return a;
    }

    friend BigInt_t operator|(const BigInt_t &a, const BigInt_t &b) {
        BigInt_t result = a;
        result |= b;
        return result;
    }

    friend BigInt_t operator&(const BigInt_t &a, const BigInt_t &b) {
        BigInt_t result = a;
        result &= b;
        return result;
    }

    friend BigInt_t operator^(const BigInt_t &a, const BigInt_t &b) {
        BigInt_t result = a;
        result ^= b;
        return result;
    }

    // TODO there are _much_ more efficient way to do these!
    static bool is_pow_2(const BigInt_t &n) {
        return (n != BigInt_t{0}) &&
               (n & (subtract_assert_no_underflow(n, BigInt_t{1}))) ==
                   BigInt_t{0};
    }
    static bool is_pow_2_minus_1(const BigInt_t &n) {
        return is_pow_2(n + BigInt_t{1});
    }
    static unsigned long long bitwidth(const BigInt_t &n) {
        int i = n.m_wds - 1;
        while (i >= 1 && n.d.buf()[i] == 0)
            i--;
        if (i == 0 && n.d.buf()[i] == 0)
            return 1;
        hassert(n.d.buf()[i] != 0);
        auto bitwidth_of_ull = [](W a) {
            hassert(a != 0);
            return sizeof(W) * 8 - __builtin_clzll(a);
        };
        return sizeof(W) * i + bitwidth_of_ull(n.d.buf()[i]);
    }
    static unsigned long long log_2_of_pow_2(const BigInt_t &n) {
        hassert(is_pow_2(n));
        return bitwidth(n) - 1;
    }
    static unsigned long long
    log_2_roundup_of_pow_2_minus_1(const BigInt_t &n) {
        hassert(is_pow_2_minus_1(n));
        return bitwidth(n);
    }

    [[nodiscard]] bool bit_on_at(int i) const {
        if (i < 0 || i >= bitsPerWord * m_wds)
            return false;
        return (d.buf()[i / bitsPerWord] >> (i % bitsPerWord)) & 1ULL;
    }
    [[nodiscard]] bool bit_off_at(int i) const { return !bit_on_at(i); }
    static BigInt_t pow_2(unsigned long long n) {
        BigInt_t result;
        const int offset = n / bitsPerWord;
        const int shift = n % bitsPerWord;
        result.d.ensure_size(offset + 1);
        for (int i = 0; i < offset; ++i) {
            result.d.buf()[i] = 0;
        }
        result.d.buf()[offset] = (W(1) << shift);
        result.m_wds = offset + 1;
        return result;
    }
    static BigInt_t pow_2_minus_1(unsigned long long n) {
        if (n == 0)
            return BigInt_t{0};
        BigInt_t result;
        const int offset = n / bitsPerWord;
        const int shift = n % bitsPerWord;
        result.d.ensure_size(offset + 1);
        for (int i = 0; i < offset; ++i) {
            result.d.buf()[i] = ~W(0);
        }
        if (shift == 0) {
            result.m_wds = offset;
        } else {
            result.d.buf()[offset] = (~W(0)) >> (64 - shift);
            result.m_wds = offset + 1;
        }
        return result;
    }

    int32_t getI32() const {
        hassert(*this <= BigInt_t{0xffffffff});
        // TODO deal with the small word case
        static_assert(sizeof(W) >= 4);
        return static_cast<int32_t>(d.buf()[0]);
    }
    int64_t getI64() const {
        hassert(*this <= BigInt_t{0x7fffffffffffffff});
        // TODO deal with the small word case
        static_assert(sizeof(W) >= 8);
        return static_cast<int64_t>(d.buf()[0]);
    }
    uint64_t getUI64() const {
        hassert(*this <= BigInt_t{0xffffffffffffffff});
        // TODO deal with the small word case
        static_assert(sizeof(W) >= 8);
        return static_cast<uint64_t>(d.buf()[0]);
    }
};

} // namespace ChpOptimize

template <typename W> struct std::hash <ChpOptimize::BigInt_t<W>> {
    size_t operator()(const ::ChpOptimize::BigInt_t<W> &obj) const {
        size_t seed = 0;
        hash_combine(seed, obj.getWdCt());
        for (int i = 0; i < obj.getWdCt(); i++) {
            hash_combine(seed, obj.getVal(i));
        }
        return seed;
    }
};

namespace ChpOptimize {

using BigInt = BigInt_t<uint64_t>;

enum class ActIntBinaryOpType {
    // binary operators
    And = 0,
    Or = 1,
    Xor = 2,
    Plus = 3,
    Minus = 4,
    Mult = 5,
    Div = 6,
    Mod = 7,
    LeftShift = 8,
    RightShift = 9
};

enum class ActIntUnaryOpType {
    Not = 18 /*bitwise*/,
    UnaryMinus = 19,
};

// An implementation of ActInt math
class ActInt {
  public:
    static int widthAfterOp(ActIntBinaryOpType op, int lhs_width,
                            int rhs_width) {
        switch (op) {
        case ActIntBinaryOpType::And:
        case ActIntBinaryOpType::Or:
        case ActIntBinaryOpType::Xor:
            return std::max(lhs_width, rhs_width);
        case ActIntBinaryOpType::Plus:
        case ActIntBinaryOpType::Minus:
            return std::max(lhs_width, rhs_width) + 1;
        case ActIntBinaryOpType::Mult:
            return lhs_width + rhs_width;
        case ActIntBinaryOpType::Div:
            return lhs_width;
        case ActIntBinaryOpType::Mod:
            return rhs_width;
        case ActIntBinaryOpType::LeftShift:
            hassert(rhs_width < 30);
            return lhs_width + (1 << rhs_width) - 1;
        case ActIntBinaryOpType::RightShift:
            return lhs_width;
        }
        hassert(false);
        return 0;
    }

    static int widthAfterOp(ActIntUnaryOpType op, int inp_width) {
        switch (op) {
        case ActIntUnaryOpType::Not:
            return inp_width;
        case ActIntUnaryOpType::UnaryMinus:
            return inp_width;
        }
        hassert(false);
        return 0;
    }

  private:
    BigInt m_value;
    int m_bitwidth;

    static ActInt make_with_clipped_high_bits(BigInt val, int bitwidth) {
        hassert(bitwidth >= 1);
        return ActInt(val.get_with_cleared_bits_geq_bitnum(bitwidth), bitwidth);
    }

  public:
    ActInt(uint64_t value, int bitwidth)
        : m_value{value}
        , m_bitwidth{bitwidth} {
        hassert(bitwidth >= 1);
        hassert(bitwidth >= 64 || (value >> bitwidth) == 0);
    }
    ActInt(BigInt value, int bitwidth)
        : m_value{std::move(value)}
        , m_bitwidth{bitwidth} {
        hassert(bitwidth >= 1);
        hassert((m_value >> bitwidth) == BigInt{0});
    }

    friend ActInt operator+(const ActInt &a, const ActInt &b) {
        return ActInt(
            a.m_value + b.m_value,
            widthAfterOp(ActIntBinaryOpType::Plus, a.m_bitwidth, b.m_bitwidth));
    }

    friend ActInt operator-(const ActInt &a) {
        int final_bitwidth =
            widthAfterOp(ActIntUnaryOpType::UnaryMinus, a.m_bitwidth);
        int final_word_ct =
            detail::div_round_up(final_bitwidth, BigInt::bitsPerWord);
        auto val = a.m_value.get_twos_complement_for_word_size(final_word_ct);
        return ActInt::make_with_clipped_high_bits(val, final_bitwidth);
    }
    friend ActInt operator-=(const ActInt &a, const ActInt &b) {
        int final_bitwidth =
            widthAfterOp(ActIntBinaryOpType::Minus, a.m_bitwidth, b.m_bitwidth);
        int final_word_ct =
            detail::div_round_up(final_bitwidth, BigInt::bitsPerWord);
        auto b_inv = b.m_value.get_twos_complement_for_word_size(final_word_ct);
        return ActInt::make_with_clipped_high_bits(a.m_value + b_inv,
                                                   final_bitwidth);
    }

    friend ActInt operator*(const ActInt &a, const ActInt &b) {
        return ActInt(
            a.m_value * b.m_value,
            widthAfterOp(ActIntBinaryOpType::Mult, a.m_bitwidth, b.m_bitwidth));
    }
    friend ActInt operator/(const ActInt &a, const ActInt &b) {
        return ActInt(
            a.m_value / b.m_value,
            widthAfterOp(ActIntBinaryOpType::Div, a.m_bitwidth, b.m_bitwidth));
    }
    friend ActInt operator%(const ActInt &a, const ActInt &b) {
        return ActInt(
            a.m_value % b.m_value,
            widthAfterOp(ActIntBinaryOpType::Mod, a.m_bitwidth, b.m_bitwidth));
    }

    friend bool operator==(const ActInt &a, const ActInt &b) {
        return a.m_value == b.m_value;
    }
    friend bool operator!=(const ActInt &a, const ActInt &b) {
        return a.m_value != b.m_value;
    }
    friend bool operator<=(const ActInt &a, const ActInt &b) {
        return a.m_value <= b.m_value;
    }
    friend bool operator>=(const ActInt &a, const ActInt &b) {
        return a.m_value >= b.m_value;
    }
    friend bool operator<(const ActInt &a, const ActInt &b) {
        return a.m_value < b.m_value;
    }
    friend bool operator>(const ActInt &a, const ActInt &b) {
        return a.m_value > b.m_value;
    }

    friend ActInt operator<<(const ActInt &a, const ActInt &b) {
        int final_width = widthAfterOp(ActIntBinaryOpType::LeftShift,
                                       a.m_bitwidth, b.m_bitwidth);
        // This special case is needed to avoid allocating a huge array if a
        // really large shift happens It is also needed because BigInt takes in
        // an int as the argument to its shift
        hassert(final_width >= 1);
        if (b.m_value.getWdCt() != 1 ||
            b.m_value.getVal(0) >= ((uint64_t)final_width))
            return ActInt(0, a.m_bitwidth);
        hassert(b.m_value.getVal(0) <= std::numeric_limits<int>::max());
        return ActInt::make_with_clipped_high_bits(
            a.m_value << (int)(b.m_value.getVal(0)), final_width);
    }

    friend ActInt operator>>(const ActInt &a, const ActInt &b) {
        int final_width = widthAfterOp(ActIntBinaryOpType::RightShift,
                                       a.m_bitwidth, b.m_bitwidth);
        // This is needed because BigInt takes in an int as the argument to its
        // shift
        hassert(final_width >= 1);
        if (b.m_value.getWdCt() != 1 ||
            b.m_value.getVal(0) >= ((uint64_t)final_width))
            return ActInt(0, a.m_bitwidth);
        hassert(b.m_value.getVal(0) <= std::numeric_limits<int>::max());
        return ActInt::make_with_clipped_high_bits(
            a.m_value << (int)(b.m_value.getVal(0)), final_width);
    }

    friend ActInt operator~(const ActInt &a) {
        int final_bitwidth = widthAfterOp(ActIntUnaryOpType::Not, a.m_bitwidth);
        int final_word_ct =
            detail::div_round_up(final_bitwidth, BigInt::bitsPerWord);
        auto val = a.m_value.get_inverted_bits_for_size(final_word_ct);
        return ActInt::make_with_clipped_high_bits(val, final_bitwidth);
    }

    friend ActInt operator|(const ActInt &a, const ActInt &b) {
        return ActInt(
            a.m_value | b.m_value,
            widthAfterOp(ActIntBinaryOpType::Or, a.m_bitwidth, b.m_bitwidth));
    }
    friend ActInt operator&(const ActInt &a, const ActInt &b) {
        return ActInt(
            a.m_value & b.m_value,
            widthAfterOp(ActIntBinaryOpType::And, a.m_bitwidth, b.m_bitwidth));
    }
    friend ActInt operator^(const ActInt &a, const ActInt &b) {
        return ActInt(
            a.m_value ^ b.m_value,
            widthAfterOp(ActIntBinaryOpType::Xor, a.m_bitwidth, b.m_bitwidth));
    }
};

} // namespace ChpOptimize
