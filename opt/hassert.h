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

#include <cassert>
#include <functional>
#include <vector>
#include <common/misc.h>

template <typename T> inline void localhassert(const T &x, const char *str, const char *file, int line) {
#ifdef NDEBUG
    if (!(x))
        __builtin_unreachable();
#else
    if (!(x)) {
       fprintf (stderr, "Assertion failed, file %s, line %d\n", file, line);
       fprintf (stderr, "Assertion: %s\n", str);
       exit (4);
    }
#endif
}
#define hassert(x)   localhassert(x, #x, __FILE__, __LINE__)

template <class T> inline void hash_combine(std::size_t &seed, const T &v) {
    std::hash<T> hasher;
    seed ^= hasher(v) + 0x9e3779b97f4a7c15ULL + (seed << 6) + (seed >> 2);
}

template <class T> struct std::hash<std::vector<T>> {
    size_t operator()(const std::vector<T> &obj) const {
        size_t seed = 0;
        hash_combine(seed, obj.size());
        for (size_t i = 0; i < obj.size(); i++) {
            hash_combine(seed, obj[i]);
        }
        return seed;
    }
};
