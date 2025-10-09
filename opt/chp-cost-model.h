#pragma once
/*************************************************************************
 *
 *  This file is part of the ACT library
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

#include <cstdint>
#include <iostream>

#include "hassert.h"

namespace ChpOptimize {

using ChpArea = int64_t;
struct ChpCosts {
    ChpArea area = 0;
    // int64_t delay;

    int64_t ct_one_bit_var = 0;
    int64_t ct_one_bit_read = 0;
    int64_t ct_one_bit_write = 0;
    int64_t ct_two_input_celem = 0;
    int64_t ct_two_input_or = 0;
    int64_t ct_one_bit_receive = 0;
    int64_t ct_one_bit_in_port = 0;
    int64_t ct_one_bit_out_port = 0;
    int64_t ct_sequencer = 0;
    int64_t ct_not_or = 0;
};

inline ChpCosts &operator+=(ChpCosts &a, const ChpCosts &b) {
    a.area += b.area;
    // int64_t delay;

    a.ct_one_bit_var += b.ct_one_bit_var;
    a.ct_one_bit_read += b.ct_one_bit_read;
    a.ct_one_bit_write += b.ct_one_bit_write;
    a.ct_two_input_celem += b.ct_two_input_celem;
    a.ct_two_input_or += b.ct_two_input_or;
    a.ct_one_bit_receive += b.ct_one_bit_receive;
    a.ct_one_bit_in_port += b.ct_one_bit_in_port;
    a.ct_one_bit_out_port += b.ct_one_bit_out_port;
    a.ct_sequencer += b.ct_sequencer;
    a.ct_not_or = b.ct_not_or;
    return a;
}

inline ChpCosts operator+(const ChpCosts &a, const ChpCosts &b) {
    ChpCosts result = a;
    result += b;
    return result;
}

// https://docs.google.com/document/d/1fZcc-E7sCVBx3BA3o_PGvtvs-uX-gDVko37UiUxC2ws/edit
struct CostModel {
  private:
    ChpArea area_one_bit_var = 0;
    ChpArea area_one_bit_read = 0;
    ChpArea area_one_bit_write = 0;
    ChpArea area_two_input_celem = 0;
    ChpArea area_two_input_or = 0;
    ChpArea area_one_bit_receive = 0;
    ChpArea area_one_bit_in_port = 0;
    ChpArea area_one_bit_out_port = 0;
    ChpArea area_sequencer = 0;
    ChpArea area_not_or = 0;

  public:
    static CostModel default_model() {
        CostModel model;
        model.area_one_bit_var = 2;
        model.area_one_bit_read = 1;
        model.area_one_bit_write = 3;
        model.area_two_input_celem = 1;
        model.area_two_input_or = 1;
        model.area_one_bit_receive = 3;
        model.area_one_bit_in_port = 3;
        model.area_one_bit_out_port = 5;
        model.area_sequencer = 4;
        model.area_not_or = 1;
        return model;
    }

    [[nodiscard]] ChpCosts cost_in_port(int width) const {
        ChpCosts result;
        result.ct_one_bit_in_port = width;
        result.area = width * area_one_bit_in_port;
        return result;
    }
    [[nodiscard]] ChpCosts cost_out_port(int width) const {
        ChpCosts result;
        result.ct_one_bit_out_port = width;
        result.area = width * area_one_bit_out_port;
        return result;
    }
    [[nodiscard]] ChpCosts cost_var(int width) const {
        ChpCosts result;
        result.ct_one_bit_var = width;
        result.area = width * area_one_bit_var;
        return result;
    }
    [[nodiscard]] ChpCosts cost_read(int width) const {
        ChpCosts result;
        result.ct_one_bit_read = width;
        result.area = width * area_one_bit_read;
        return result;
    }
    [[nodiscard]] ChpCosts cost_write(int width) const {
        ChpCosts result;
        result.ct_one_bit_write = width;
        result.area = width * area_one_bit_write;
        return result;
    }
    [[nodiscard]] ChpCosts cost_receive(int width) const {
        ChpCosts result;
        result.ct_one_bit_receive = width;
        result.area = width * area_one_bit_receive;
        return result;
    }
    [[nodiscard]] ChpCosts cost_or(int n) const {
        ChpCosts result;
        result.ct_two_input_or = (n - 1);
        result.area = (n - 1) * area_two_input_or;
        return result;
    }
    [[nodiscard]] ChpCosts cost_celem(int n) const {
        ChpCosts result;
        result.ct_two_input_celem = (n - 1);
        result.area = (n - 1) * area_two_input_celem;
        return result;
    }
    [[nodiscard]] ChpCosts cost_sequencer() const {
        ChpCosts result;
        result.area = area_sequencer;
        result.ct_sequencer = 1;
        return result;
    }
    [[nodiscard]] ChpCosts cost_not_or() const {
        ChpCosts result;
        result.area = area_not_or;
        result.ct_not_or = 1;
        return result;
    }
    [[nodiscard]] ChpCosts cost_transfer(int /*to*/, int /*from*/) const {
        return ChpCosts{0};
    }
};
// we will weight things by constant integer multipliers.

class ChpGraph;
ChpCosts cost_of_chp(const ChpGraph &g, const CostModel &model);

std::ostream &operator<<(std::ostream &, const ChpCosts &costs);

} // namespace ChpOptimize
