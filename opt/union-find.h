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

#include <unordered_map>
#include <unordered_set>

namespace ChpOptimize {

template <class T> class UnionFind {
  private:
    class Node {
      public:
        Node *up;
        explicit Node(Node *t_up)
            : up{t_up} {}
    };

    std::unordered_map<T, Node *> mapin;

  public:
    typedef Node *id;

    UnionFind() = default;

    id find(const T &);
    void union_(id, id);
    void union_(const T &, const T &);
};

template <class T> typename UnionFind<T>::id UnionFind<T>::find(const T &t) {
    Node *&start_node = mapin[t];
    if (!start_node)
        start_node = new Node(nullptr);

    Node *search;
    for (search = start_node; search->up; search = search->up)
        ;
    Node *top = search;

    for (Node *search_ = start_node; search_ != top;) {
        Node *next = search_->up;
        search_->up = top;
        search_ = next;
    }

    return top;
}

template <class T>
void UnionFind<T>::union_(UnionFind<T>::id a, UnionFind<T>::id b) {
    if (a != b)
        b->up = a;
}

template <class T> void UnionFind<T>::union_(const T &a, const T &b) {
    union_(find(a), find(b));
}

} // namespace ChpOptimize
