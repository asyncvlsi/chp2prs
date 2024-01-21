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

#include "hassert.h"
#include <algorithm>
#include <concepts>
#include <functional>
#include <list>
#include <set>
#include <sstream>
#include <unordered_map>
#include <unordered_set>
#include <vector>

namespace ChpOptimize::Algo {


/**
 * Move all items from the "items" vector passed in to the end of 
 * "out"
 */
template <typename T>
void move_append(std::vector<T> &&items, std::vector<T> &out) {
    std::move(std::begin(items), std::end(items), std::back_inserter(out));
}

// find if wrappers

/**
 * Walk through the iteratable items "items" and return the first
 * match according to function "Fn"; it returns the iterator at the
 * position of the first match.
 *
 * There are two versions: once for const items, the other for
 * non-const items.
 */
template <typename Iterable, typename Fn>
typename Iterable::const_iterator find_if(const Iterable &items, const Fn &fn) {
    return std::find_if(items.cbegin(), items.cend(), fn);
}

template <typename Iterable, typename Fn>
typename Iterable::iterator find_if(Iterable &items, const Fn &fn) {
    return std::find_if(items.begin(), items.end(), fn);
}


/**
 * Similar to the two find_ifs above, except they assert that a value is
 * found before returning the iterator.
 */
template <typename Iterable, typename Fn>
typename Iterable::const_iterator find_assert_if(const Iterable &items,
                                                 const Fn &fn) {
    auto it = find_if(items, fn);
    hassert(it != items.end());
    return it;
}

template <typename Iterable, typename Fn>
typename Iterable::iterator find_assert_if(Iterable &items, const Fn &fn) {
    auto it = find_if(items, fn);
    hassert(it != items.end());
    return it;
}

/**
 * This returns a Boolean that is true if any of the "items" satisfy
 * the predicate "fn"
 */
template <typename Iterable, typename Fn>
bool any_of(const Iterable &items, const Fn &fn) {
    return find_if(items, fn) != items.end();
}

/**
 * This returns a Boolean that is true if none of the "items" satisfy
 * the predicate "fn".
 */
template <typename Iterable, typename Fn>
bool none_of(const Iterable &items, const Fn &fn) {
    return !any_of(items, fn);
}

/**
 * This checks that all of the items satisfy the preducate fn
 */
template <typename U, typename Fn> bool all_of(const U &u, const Fn &fn) {
    return std::all_of(std::begin(u), std::end(u), fn);
}


/**
 * Wrappers for std::find; calls std::find by passing it the begin() and
 * end() iterators for the "items" passed in. 
 */

// find wrappers
template <typename Iterable, typename T>
typename Iterable::const_iterator find(const Iterable &items, const T &target) {
    return std::find(items.cbegin(), items.cend(), target);
}

template <typename Iterable, typename T>
typename Iterable::iterator find(Iterable &items, const T &target) {
    return std::find(items.begin(), items.end(), target);
}

template <typename Iterable, typename T>
typename Iterable::const_iterator find_assert(const Iterable &items,
                                              const T &target) {
    auto it = find(items, target);
    hassert(it != items.end());
    return it;
}

template <typename Iterable, typename T>
typename Iterable::iterator find_assert(Iterable &items, const T &target) {
    auto it = find(items, target);
    hassert(it != items.end());
    return it;
}

  
/**
 *  Check if the "target" is contained within "items"
 */
template <typename Iterable, typename T>
bool contains(const Iterable &items, const T &target) {
    return find(items, target) != items.end();
}

/**
 *  Count the number of times "target" is contained within "items"
 */
template <typename Iterable, typename Fn>
size_t count_if(Iterable &items, const Fn &fn) {
    return std::count_if(items.begin(), items.end(), fn);
}

/**
 * Converts vectors into a set
 */
template <typename T> std::set<T> set_of_vector(const std::vector<T> &items) {
    return std::set<T>(items.begin(), items.end());
}

/**  
 * Check if a vector has repeated items
 */
template <typename T> bool vector_has_no_repeates(const std::vector<T> &items) {
    return set_of_vector(items).size() == items.size();
}

/**
 * A wrapper for std::transform, that passes in a map that takes two
 * arguments.
 */
template <typename T, typename U, typename V, typename Fn>
typename std::vector<T> map2(const std::vector<U> &u, const std::vector<V> &v,
                             const Fn &fn) {
    std::vector<T> t;
    hassert(u.size() == v.size());
    t.reserve(u.size());
    std::transform(u.begin(), u.end(), v.begin(), std::back_inserter(t), fn);
    return t;
}

/**  
 * Check if the two-argument function "fn" is satisfied by any of the
 * pairs. The argument pairs are iterated through in both the "u" and "v"
 * containers.
 */
template <typename U, typename V, typename Fn>
bool any_of2(const U &u, const V &v, const Fn &fn) {
    hassert(u.size() == v.size());
    auto it_u = std::begin(u);
    auto it_v = std::begin(v);
    for (; it_u != std::end(u); ++it_u, ++it_v) {
        hassert(it_v != std::end(v));
        if (fn(*it_u, *it_v))
            return true;
    }
    hassert(it_v == std::end(v));
    return false;
}

/**  
 * Check if the two-argument function "fn" is satisfied by all of the
 * pairs. The argument pairs are iterated through in both the "u" and "v"
 * containers.
 */
template <typename U, typename V, typename Fn>
bool all_of2(const U &u, const V &v, const Fn &fn) {
    hassert(u.size() == v.size());
    auto it_u = std::begin(u);
    auto it_v = std::begin(v);
    for (; it_u != std::end(u); ++it_u, ++it_v) {
        hassert(it_v != std::end(v));
        if (!fn(*it_u, *it_v))
            return false;
    }
    hassert(it_v == std::end(v));
    return true;
}

/**  
 * Check if the values contained in the two containers are equal.
 */
template <typename U, typename V> bool all_equal(const U &u, const V &v) {
    hassert(u.size() == v.size());
    return all_of2(u, v, [](const auto &a, const auto &b) { return a == b; });
}

template <typename T, typename U, typename Fn>
T sum(const std::vector<U> &us, const Fn &fn, T init = T{}) {
    for (const auto &u : us) {
        init += fn(u);
    }
    return init;
}

template <typename T, typename U, typename Fn>
typename std::vector<T> map1(const U &us, const Fn &fn) {
    std::vector<T> t;
    t.reserve(us.size());
    std::transform(us.begin(), us.end(), std::back_inserter(t), fn);
    return t;
}
template <typename T, typename U, typename Fn>
typename std::vector<T> filter1(const U &us, const Fn &fn) {
    std::vector<T> t;
    t.reserve(us.size());
    std::copy_if(us.begin(), us.end(), std::back_inserter(t), fn);
    return t;
}
template <typename T, typename U, typename Fn>
typename std::unordered_set<T> filter_set(const U &us, const Fn &fn) {
    std::unordered_set<T> t;
    t.reserve(us.size());
    for (const auto &u : us) {
        if (fn(u)) {
            t.insert(u);
        }
    }
    return t;
}

// remove elements from both vector if the function is true. This takes time
// O(n) to doa removal pass over the entire vectors
template <typename U, typename V, typename Fn>
void remove_filter_2_if(std::vector<U> &us, std::vector<V> &vs, const Fn &fn) {
    hassert(us.size() == vs.size());
    const auto ct = static_cast<ssize_t>(us.size());
    ssize_t out_idx = 0;
    for (ssize_t in_idx = 0; in_idx != ct; ++in_idx) {
        bool should_remove = fn(us[in_idx], vs[in_idx]);
        if (should_remove) {
            // do nothing
        } else if (out_idx == in_idx) {
            out_idx++;
        } else {
            us[out_idx] = std::move(us[in_idx]);
            vs[out_idx] = std::move(vs[in_idx]);
            out_idx++;
        }
    }
    us.resize(out_idx);
    vs.resize(out_idx);
}

template <typename U, typename Fn>
void remove_filter_if(std::list<U> &us, const Fn &fn) {
    for (auto it = us.begin(); it != us.end();) {
        if (fn(*it)) {
            it = us.erase(it);
        } else {
            ++it;
        }
    }
}

template <typename U, typename Fn>
void remove_filter_if(std::vector<U> &us, const Fn &fn) {
    us.erase(std::remove_if(us.begin(), us.end(), fn), us.end());
}

template <typename T, typename U>
typename std::vector<T> as_sorted_vector(const U &us) {
    std::vector<T> t{us.begin(), us.end()};
    std::sort(t.begin(), t.end());
    return t;
}
template <typename T, typename U>
typename std::vector<T> as_vector(const U &us) {
    std::vector<T> t{us.begin(), us.end()};
    return t;
}

template <typename K, typename V>
std::vector<K> get_sorted_keys(const std::unordered_map<K, V> &mp) {
    std::vector<K> keys;
    for (const auto &[key, val] : mp)
        keys.push_back(key);
    std::sort(keys.begin(), keys.end());
    return keys;
}

template <typename K, typename U1, typename U2>
std::set<K> union_to_ordered_set(const U1 &u1, const U2 &u2) {
    std::set<K> s;
    for (auto k : u1)
        s.insert(k);
    for (auto k : u2)
        s.insert(k);
    return s;
}

template <typename M, typename K, typename V>
V value_default(const M &mp, const K &k, const V &default_) {
    if (auto it = mp.find(k); it != mp.end()) {
        return it->second;
    } else {
        return default_;
    }
}
template <typename M, typename K, typename Fn>
std::invoke_result_t<Fn> value_default_func(const M &mp, const K &k,
                                            const Fn &fn) {
    if (auto it = mp.find(k); it != mp.end()) {
        static_assert(
            std::is_same_v<std::invoke_result_t<Fn>, decltype(it->second)>);
        return it->second;
    } else {
        return fn();
    }
}

template <typename U, typename Fn1, typename Fn2>
void foreach_interspaced(const U &u, const Fn1 &fn1, const Fn2 &fn2) {
    auto it = u.begin();
    if (it == u.end())
        return;
    fn1(*it);
    ++it;
    for (; it != u.end(); ++it) {
        fn2();
        fn1(*it);
    }
}

template <typename U, typename Fn>
std::string join_str_mapped(const U &u, const Fn &fn,
                            const std::string &seperator) {
    std::stringstream ss;
    Algo::foreach_interspaced(
        u, [&](const auto &x) { ss << fn(x); }, [&]() { ss << seperator; });
    return ss.str();
}
template <typename U1, typename U2, typename Fn>
std::string join_str_concat_mapped(const U1 &u1, const U2 &u2, const Fn &fn,
                                   const std::string &seperator) {
    std::stringstream ss;
    if (u2.empty())
        return join_str_mapped(u1, fn, seperator);
    for (const auto &x : u1)
        ss << fn(x) << seperator;
    Algo::foreach_interspaced(
        u2, [&](const auto &x) { ss << fn(x); }, [&]() { ss << seperator; });
    return ss.str();
}
template <typename U1, typename U2, typename Fn1, typename Fn2>
std::string join_str_concat_mapped(const U1 &u1, const U2 &u2, const Fn1 &fn1,
                                   const Fn2 &fn2,
                                   const std::string &seperator) {
    std::stringstream ss;
    if (u2.empty())
        return join_str_mapped(u1, fn1, seperator);
    for (const auto &x : u1)
        ss << fn1(x) << seperator;
    Algo::foreach_interspaced(
        u2, [&](const auto &x) { ss << fn2(x); }, [&]() { ss << seperator; });
    return ss.str();
}

template <typename K, typename V>
std::unordered_map<K, V> set_union(const std::unordered_map<K, V> &a,
				   const std::unordered_map<K, V> &b) {
  std::unordered_map<K,V> result = a;
  for (const auto &[x,v] : b) {
    result[x] = v;
  }
  return result;
}

/* compute a - b, where b is a subset of a */
template <typename K>
std::unordered_set<K> set_minus_sub(const std::unordered_set<K> &a,
				   const std::unordered_set<K> &b) {
    for (const auto &x : b) {
        hassert(a.count(x));
    }
    std::unordered_set<K> result;
    for (const auto &x : a) {
        if (!b.count(x))
            result.insert(x);
    }
    return result;
}

template <typename K>
std::unordered_set<K> set_minus(const std::unordered_set<K> &a,
				   const std::unordered_set<K> &b) {
    std::unordered_set<K> result;
    for (const auto &x : a) {
        if (!b.count(x))
            result.insert(x);
    }
    return result;
}
  

} // namespace ChpOptimize::Algo
