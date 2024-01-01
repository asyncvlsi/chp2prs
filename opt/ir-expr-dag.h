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

#include "ir-expr.h"

/**
 * This is used to represent expressions as DAGs. The DAG can have
 * multiple roots. We can't use unique pointers any more within the IR
 * Expression, so we always set the managed memory flag to no.
 */
namespace ChpOptimize {
namespace detail {

  /** 
   * CanonicalFlatNode holds a Node *, where a Node * is an IRExpr
   * without memory management.
   */
template <typename Tag, typename VarIdType> struct CanonicalFlatNode {
    using Node = IRExpr<Tag, VarIdType, ManageMemory::no>;
    Node *node;

    /*
     * The equality test checks depth-1 equality of the Node (IRExpr)
     * pointer held by this structure.
     */
    friend bool operator==(const CanonicalFlatNode &a,
                           const CanonicalFlatNode &b) {
        hassert(a.node);
        Node &n = *a.node;
        hassert(b.node);
        Node &m = *b.node;
        if (n.type() != m.type())
            return false;

        switch (n.type()) {
        case IRExprTypeKind::Var:
            return n.u_var().id == m.u_var().id;
        case IRExprTypeKind::Const:
            return n.u_cons().v == m.u_cons().v &&
                   n.u_cons().v_width == m.u_cons().v_width;
        case IRExprTypeKind::Query:
            return n.u_query().selector == m.u_query().selector &&
                   n.u_query().l == m.u_query().l &&
                   n.u_query().r == m.u_query().r;
        case IRExprTypeKind::Bitfield:
            return n.u_bitfield().e == m.u_bitfield().e &&
                   n.u_bitfield().hi() == m.u_bitfield().hi() &&
                   n.u_bitfield().lo() == m.u_bitfield().lo();
        case IRExprTypeKind::BinaryOp:
            return n.u_e2().op_type == m.u_e2().op_type &&
                   n.u_e2().l == m.u_e2().l && n.u_e2().r == m.u_e2().r;
        case IRExprTypeKind::UnaryOp:
            return n.u_e1().op_type == m.u_e1().op_type &&
                   n.u_e1().l == m.u_e1().l;
        }
        hassert(false);
        return false;
    }
};
} // namespace detail

} // namespace ChpOptimize


/*
 * Hashing support
 */
template <typename Tag, typename VarIdType>
struct ::std::hash<::ChpOptimize::detail::CanonicalFlatNode<Tag, VarIdType>> {
    using Node =
        ::ChpOptimize::IRExpr<Tag, VarIdType, ChpOptimize::ManageMemory::no>;
    size_t
    operator()(const ::ChpOptimize::detail::CanonicalFlatNode<Tag, VarIdType> c)
        const {
        hassert(c.node);
        Node &n = *c.node;
        size_t seed = 0;
        switch (n.type()) {
        case ChpOptimize::IRExprTypeKind::Var:
            hash_combine(seed, n.type());
            hash_combine(seed, n.u_var().id);
            return seed;

        case ChpOptimize::IRExprTypeKind::Const:
            hash_combine(seed, n.type());
            hash_combine(seed, n.u_cons().v);
            hash_combine(seed, n.u_cons().v_width);
            return seed;

        case ChpOptimize::IRExprTypeKind::Query:
            hash_combine(seed, n.type());
            hash_combine(seed, (void *)n.u_query().selector);
            hash_combine(seed, (void *)n.u_query().l);
            hash_combine(seed, (void *)n.u_query().r);
            return seed;

        case ChpOptimize::IRExprTypeKind::Bitfield:
            hash_combine(seed, n.type());
            hash_combine(seed, (void *)n.u_bitfield().e);
            hash_combine(seed, n.u_bitfield().hi());
            hash_combine(seed, n.u_bitfield().lo());
            return seed;

        case ChpOptimize::IRExprTypeKind::BinaryOp:
            hash_combine(seed, n.type());
            hash_combine(seed, n.u_e2().op_type);
            hash_combine(seed, (void *)n.u_e2().l);
            hash_combine(seed, (void *)n.u_e2().r);
            return seed;

        case ChpOptimize::IRExprTypeKind::UnaryOp:
            hash_combine(seed, n.type());
            hash_combine(seed, n.u_e1().op_type);
            hash_combine(seed, (void *)n.u_e1().l);
            return seed;
        }
        hassert(false);
        return seed;
    }
};

namespace ChpOptimize {

template <typename Tag, typename VarIdType> struct IRExprSingleRootDag;

template <typename Tag, typename VarIdType> struct IRExprDag {
  public:
    using IRExpr_t = IRExpr<Tag, VarIdType, ManageMemory::yes>;
    using Node = IRExpr<Tag, VarIdType, ManageMemory::no>;
    using CanonicalFlatNode_t = detail::CanonicalFlatNode<Tag, VarIdType>;
    using IRExprSingleRootDag_t = IRExprSingleRootDag<Tag, VarIdType>;
    std::deque<Node> nodes;
    // the order of the list of roots matters. Each root corresponds to a value
    // this dag computes and exposes to the enviroment
    std::vector<Node *> roots;

  private:
    // maps "fn" over all nodes, iterating in a post-ordering of the DAG
    static void
    mapNodes_helper(IRExprDag::Node &n, std::unordered_set<const Node *> &done,
                    const std::function<void(IRExprDag::Node &)> &fn) {
        if (done.count(&n))
            return;
        done.insert(&n);
        switch (n.type()) {
        case IRExprTypeKind::Var:
        case IRExprTypeKind::Const:
            break;
        case IRExprTypeKind::Query: {
            mapNodes_helper(*n.u_query().selector, done, fn);
            mapNodes_helper(*n.u_query().l, done, fn);
            mapNodes_helper(*n.u_query().r, done, fn);
            break;
        }
        case IRExprTypeKind::BinaryOp: {
            mapNodes_helper(*n.u_e2().l, done, fn);
            mapNodes_helper(*n.u_e2().r, done, fn);
            break;
        }
        case IRExprTypeKind::Bitfield: {
            mapNodes_helper(*n.u_bitfield().e, done, fn);
            break;
        }
        case IRExprTypeKind::UnaryOp: {
            mapNodes_helper(*n.u_e1().l, done, fn);
            break;
        }
        }
        fn(n);
    }

    /* Similar to map, except it maps each node to an integer. If a
       node has sub-expressions, the node value is the sum of the
       values of each sub-expression, plus the function applied to the
       local node. For example, this could be used to count the number
       of nodes in the dag.
    */
    static BigInt mapReduceSumNodes_helper(
        const IRExprDag::Node &n,
        std::unordered_map<const Node *, BigInt> &vals,
        const std::function<BigInt(const IRExprDag::Node &)> &fn) {
        if (auto it = vals.find(&n); it != vals.end())
            return it->second;
        BigInt result = BigInt{0};
        switch (n.type()) {
        case IRExprTypeKind::Var:
        case IRExprTypeKind::Const:
            break;
        case IRExprTypeKind::Query: {
            result += mapReduceSumNodes_helper(*n.u_query().selector, vals, fn);
            result += mapReduceSumNodes_helper(*n.u_query().l, vals, fn);
            result += mapReduceSumNodes_helper(*n.u_query().r, vals, fn);
            break;
        }
        case IRExprTypeKind::BinaryOp: {
            result += mapReduceSumNodes_helper(*n.u_e2().l, vals, fn);
            result += mapReduceSumNodes_helper(*n.u_e2().r, vals, fn);
            break;
        }
        case IRExprTypeKind::Bitfield: {
            result += mapReduceSumNodes_helper(*n.u_bitfield().e, vals, fn);
            break;
        }
        case IRExprTypeKind::UnaryOp: {
            result += mapReduceSumNodes_helper(*n.u_e1().l, vals, fn);
            break;
        }
        }
        result += fn(n);
        vals[&n] = result;
        return result;
    }

  public:
    [[nodiscard]] size_t raw_nodes_ct() const { return nodes.size(); }

    /* returns the number of actual nodes in the dag */
    [[nodiscard]] size_t live_node_ct() const {
        size_t i = 0;
        IRExprDag::iterNodes(*this, [&](const auto &) { i++; });
        return i;
    }

    [[nodiscard]] static std::unordered_map<const Node *, BigInt>
    mapReduceSum(const IRExprDag &dag,
                 const std::function<BigInt(const IRExprDag::Node &)> &fn) {
        std::unordered_map<const Node *, BigInt> results;
        for (Node *root : dag.roots)
            mapReduceSumNodes_helper(*root, results, fn);
        return results;
    }

   /* apply the map starting from each root of the dag */
    static void mapNodes(IRExprDag &dag,
                         const std::function<void(IRExprDag::Node &)> &fn) {
        std::unordered_set<const Node *> done;
        for (Node *root : dag.roots)
            mapNodes_helper(*root, done, fn);
    }

    /* the "iter" variants are those that do not modify the dag */
    static void
    iterNodes(const IRExprDag &dag,
              const std::function<void(const IRExprDag::Node &)> &fn) {
        mapNodes(const_cast<IRExprDag &>(dag), [&](IRExprDag::Node &node) {
            return fn(const_cast<const IRExprDag::Node &>(node));
        });
    }

    /* 
       like mapNodes, but starts at the specified node rather than a
       root. Could also just call mapNodes_helper().
    */
    static void
    mapNodesBelow(Node *node,
                  const std::function<void(IRExprDag::Node &)> &fn) {
        std::unordered_set<const Node *> done;
        mapNodes_helper(*node, done, fn);
    }
  
    static void
    mapNodesBelow(std::vector<Node *> nodes,
                  const std::function<void(IRExprDag::Node &)> &fn) {
        std::unordered_set<const Node *> done;
        for (Node *node : nodes)
            mapNodes_helper(*node, done, fn);
    }
  
    static void
    iterNodesBelow(const Node *node,
                   const std::function<void(const IRExprDag::Node &)> &fn) {
        mapNodesBelow(const_cast<Node *>(node), [&](IRExprDag::Node &node) {
            return fn(const_cast<const IRExprDag::Node &>(node));
        });
    }
  
    static void
    iterNodesBelow(const std::vector<const Node *> &nodes,
                   const std::function<void(const IRExprDag::Node &)> &fn) {
        std::unordered_set<const Node *> done;
        for (const Node *node : nodes)
            mapNodes_helper(
                *const_cast<Node *>(node), done, [&](IRExprDag::Node &node) {
                    return fn(const_cast<const IRExprDag::Node &>(node));
                });
    }


  /*
   * De-duplication. If a sub-tree is the same, it should have the
   * same Node pointer. The map is used to map a given node to its
   * canonical pointer.
   */
    static Node *deduplicate_helper(
        IRExprDag::Node &n, std::unordered_map<Node *, Node *> &node_from_node,
        std::unordered_map<CanonicalFlatNode_t, Node *> &node_from_canonical) {

        /* 
	   If this node pointer is found in the node-to-node map, then
	   return the canonical pointer from the map.
	*/
        if (auto it = node_from_node.find(&n); it != node_from_node.end()) {
            return it->second;
	}

	/* 
	   De-duplicate recursively
	*/
        switch (n.type()) {
        case IRExprTypeKind::Var:
        case IRExprTypeKind::Const:
            break;
        case IRExprTypeKind::Query: {
            n.u_query().selector = deduplicate_helper(
                *n.u_query().selector, node_from_node, node_from_canonical);
            n.u_query().l = deduplicate_helper(*n.u_query().l, node_from_node,
                                               node_from_canonical);
            n.u_query().r = deduplicate_helper(*n.u_query().r, node_from_node,
                                               node_from_canonical);
            break;
        }
        case IRExprTypeKind::BinaryOp: {
            n.u_e2().l = deduplicate_helper(*n.u_e2().l, node_from_node,
                                            node_from_canonical);
            n.u_e2().r = deduplicate_helper(*n.u_e2().r, node_from_node,
                                            node_from_canonical);
            break;
        }
        case IRExprTypeKind::Bitfield: {
            n.u_bitfield().e = deduplicate_helper(
                *n.u_bitfield().e, node_from_node, node_from_canonical);
            break;
        }
        case IRExprTypeKind::UnaryOp: {
            n.u_e1().l = deduplicate_helper(*n.u_e1().l, node_from_node,
                                            node_from_canonical);
            break;
        }
        }

	/* The canonical flat node test checks for depth-1 equality */
        CanonicalFlatNode_t k = CanonicalFlatNode_t{&n};
        if (auto it = node_from_canonical.find(k);
            it != node_from_canonical.end()) {
            node_from_node[&n] = it->second;
            return it->second;
        }
        node_from_node[&n] = &n;
        node_from_canonical[k] = &n;
        return &n;
    }

  /* This eliminates all duplicate sub-trees, sharing pointers */
    static void deduplicate(IRExprDag &dag) {
        std::unordered_map<Node *, Node *> node_from_node;
        std::unordered_map<CanonicalFlatNode_t, Node *> node_from_canonical;
        // for each node (in post-order)
        // 1) canonicalize all pointers
        // 2) add this node to the canonical node map if its not already present
        for (Node *&root : dag.roots)
            root =
                deduplicate_helper(*root, node_from_node, node_from_canonical);
    }

    static IRExprDag deep_copy(const IRExprDag &dag) {
        IRExprDag result;
        result.roots = result.addSubdag(dag);
        return result;
    }

    // returns whether any remap was applied.
    static bool remapVars(
        IRExprDag &dag,
        const std::unordered_map<VarIdType, IRExprSingleRootDag_t> &remaps) {
        bool any_changed = false;
        // this maps a var id to the root of the tree after it is added as a
        // subexpression. This saves a lot of duplication, even thouhg we can
        // repack later
        std::unordered_map<VarIdType, Node *> var_to_subexpr;
        IRExprDag::mapNodes(dag, [&](Node &n) {
            if (n.type() == IRExprTypeKind::Var) {
	        VarIdType var_id = n.u_var().id;  // was VarId 
                auto remap_it = remaps.find(var_id);
                if (remap_it != remaps.end()) {
                    auto new_id_it = var_to_subexpr.find(var_id);
                    Node *s_root = nullptr;
                    if (new_id_it == var_to_subexpr.end()) {
                        s_root = dag.addSubdag(remap_it->second);
                        var_to_subexpr[n.u_var().id] = s_root;
                    } else {
                        s_root = new_id_it->second;
                    }
                    n = Node::shallow_copy(s_root);
                    any_changed = true;
                }
            }
        });
        return any_changed;
    }

    // variable renaming
    static bool
    remapVars(IRExprDag &dag,
              const std::unordered_map<VarIdType, VarIdType> &remaps) {
        bool any_changed = false;
        IRExprDag::mapNodes(dag, [&](Node &n) {
            if (n.type() == IRExprTypeKind::Var) {
                auto it = remaps.find(n.u_var().id);
                if (it != remaps.end()) {
                    n.u_var().id = it->second;
                    any_changed = true;
                }
            }
        });
        return any_changed;
    }
  
    static std::unordered_map<const Node *, BigInt>
    getFlattenedSizesBelowNodes(const IRExprDag &dag) {
        return mapReduceSum(dag, [](const auto &) { return BigInt{1}; });
    }
  
    static BigInt getSumFlattenedSizeRoots(const IRExprDag &dag) {
        auto results = getFlattenedSizesBelowNodes(dag);
        BigInt sm = BigInt{0};
        for (const auto &root : dag.roots)
            sm += results[root];
        return sm;
    }

    Node *newNode(Node &&n) {
        nodes.push_back(std::move(n));
        return &nodes.back();
    }

    IRExprDag() = default;
    ~IRExprDag() = default;
    IRExprDag(const IRExprDag &) = delete;
    IRExprDag &operator=(const IRExprDag &) = delete;
    IRExprDag(IRExprDag &&) noexcept = default;
    IRExprDag &operator=(IRExprDag &&) noexcept = default;

  private:
    Node *newUninitializedNode() {
        nodes.push_back(Node());
        return &nodes.back();
    }

  public:
    // this returns a vector paarellel to the other.roots which is the new
    // location of those roots

    template <typename NodePtr>
    std::vector<Node *> addSubdag(const IRExprDag &other,
                                  const std::vector<NodePtr> &old_nodes) {
        static_assert(std::is_same_v<NodePtr, const Node *> ||
                      std::is_same_v<NodePtr, Node *>);
        // This could be done much better. For now, us a map that maps `old
        // pointer` to `new pointer`
        std::unordered_map<const Node *, Node *> relabel;
        iterNodes(other, [&](const IRExprDag::Node &n) {
            hassert(!relabel.count(&n));
            switch (n.type()) {
            case IRExprTypeKind::Var:
                relabel[&n] = newNode(
                    IRExprDag::Node::makeVariableAccess(n.u_var().id, n.width));
                break;
            case IRExprTypeKind::Const:
                relabel[&n] = newNode(IRExprDag::Node::makeConstant(
                    n.u_cons().v, n.u_cons().v_width));
                break;
            case IRExprTypeKind::Query:
                relabel[&n] = newNode(IRExprDag::Node::makeQuery(
                    relabel.at(n.u_query().selector), relabel.at(n.u_query().l),
                    relabel.at(n.u_query().r)));
                break;
            case IRExprTypeKind::BinaryOp:
                relabel[&n] = newNode(IRExprDag::Node::makeBinaryOp(
                    n.u_e2().op_type, relabel.at(n.u_e2().l),
                    relabel.at(n.u_e2().r)));
                break;

            case IRExprTypeKind::UnaryOp:
                relabel[&n] = newNode(IRExprDag::Node::makeUnaryOp(
                    n.u_e1().op_type, relabel.at(n.u_e1().l)));
                break;

            case IRExprTypeKind::Bitfield:
                relabel[&n] = newNode(IRExprDag::Node::makeBitfield(
                    relabel.at(n.u_bitfield().e), n.u_bitfield().hi(),
                    n.u_bitfield().lo()));
                break;
            }
        });

        std::vector<Node *> remapped_nodes;
        for (const Node *node : old_nodes)
            remapped_nodes.push_back(relabel.at(node));

        return remapped_nodes;
    }
    std::vector<Node *> addSubdag(const IRExprDag &other) {
        return addSubdag(other, other.roots);
    }

    /* convert to managed expression */
    static IRExpr_t to_expr(const Node &n) {
        // This could be done much better. For now, us a map that maps `old
        // pointer` to `new pointer`
        switch (n.type()) {
        case IRExprTypeKind::Var:
            return IRExpr_t::makeVariableAccess(n.u_var().id, n.width);
        case IRExprTypeKind::Const:
            return IRExpr_t::makeConstant(n.u_cons().v, n.u_cons().v_width);
        case IRExprTypeKind::Query:
            return IRExpr_t::makeQuery(
                std::make_unique<IRExpr_t>(to_expr(*n.u_query().selector)),
                std::make_unique<IRExpr_t>(to_expr(*n.u_query().l)),
                std::make_unique<IRExpr_t>(to_expr(*n.u_query().r)));
        case IRExprTypeKind::BinaryOp:
            return IRExpr_t::makeBinaryOp(
                n.u_e2().op_type,
                std::make_unique<IRExpr_t>(to_expr(*n.u_e2().l)),
                std::make_unique<IRExpr_t>(to_expr(*n.u_e2().r)));
        case IRExprTypeKind::UnaryOp:
            return IRExpr_t::makeUnaryOp(
                n.u_e1().op_type,
                std::make_unique<IRExpr_t>(to_expr(*n.u_e1().l)));
        case IRExprTypeKind::Bitfield:
            return IRExpr_t::makeBitfield(
                std::make_unique<IRExpr_t>(to_expr(*n.u_bitfield().e)),
                n.u_bitfield().hi(), n.u_bitfield().lo());
        }
        hassert(false);
    }

    Node *addSubdag(const IRExprSingleRootDag_t &other);

    /* convert to Node from IRExpr_t */
    Node *addSubdag(const IRExpr_t &n) {
        // This could be done much better. For now, us a map that maps `old
        // pointer` to `new pointer`
        switch (n.type()) {
        case IRExprTypeKind::Var:
            return newNode(
                IRExprDag::Node::makeVariableAccess(n.u_var().id, n.width));
        case IRExprTypeKind::Const:
            return newNode(IRExprDag::Node::makeConstant(n.u_cons().v,
                                                         n.u_cons().v_width));
        case IRExprTypeKind::Query:
            return newNode(IRExprDag::Node::makeQuery(
                addSubdag(*n.u_query().selector.get()),
                addSubdag(*n.u_query().l.get()),
                addSubdag(*n.u_query().r.get())));
        case IRExprTypeKind::BinaryOp:
            return newNode(IRExprDag::Node::makeBinaryOp(
                n.u_e2().op_type, addSubdag(*n.u_e2().l.get()),
                addSubdag(*n.u_e2().r.get())));
        case IRExprTypeKind::UnaryOp:
            return newNode(IRExprDag::Node::makeUnaryOp(
                n.u_e1().op_type, addSubdag(*n.u_e1().l.get())));

        case IRExprTypeKind::Bitfield:
            return newNode(IRExprDag::Node::makeBitfield(
                addSubdag(*n.u_bitfield().e.get()), n.u_bitfield().hi(),
                n.u_bitfield().lo()));
        }
        hassert(false);
        return nullptr;
    }
};

template <typename Tag, typename VarIdType> struct IRExprSingleRootDag {
  public:
    using IRExprDag_t = IRExprDag<Tag, VarIdType>;
    using IRExpr_t = typename IRExprDag_t::IRExpr_t;
    using Node = typename IRExprDag_t::Node;

  public:
    IRExprDag_t m_dag;

  public:
    static void mapNodes(IRExprSingleRootDag &dag,
                         const std::function<void(Node &)> &fn) {
        IRExprDag_t::mapNodes(dag.m_dag, fn);
    }
    static void iterNodes(const IRExprSingleRootDag &dag,
                          const std::function<void(const Node &)> &fn) {
        IRExprDag_t::iterNodes(dag.m_dag, fn);
    }

    static IRExprSingleRootDag of_expr(std::unique_ptr<IRExpr_t> e) {
        return of_expr(*e);
    }
    static IRExprSingleRootDag of_expr(const IRExpr_t &e) {
        IRExprSingleRootDag result;
        result.m_dag.roots.push_back(result.m_dag.addSubdag(e));
        return result;
    }
    static IRExpr_t to_expr(const IRExprSingleRootDag &dag) {
        return IRExprDag_t::to_expr(*dag.root());
    }
    static IRExprSingleRootDag deep_copy(const IRExprSingleRootDag &dag) {
        return IRExprSingleRootDag{IRExprDag_t::deep_copy(dag.m_dag)};
    }

    static IRExprSingleRootDag
    combine_reduce(IRBinaryOpType op,
                   const std::vector<IRExprSingleRootDag> &elements) {
        // technically we only need assortative, but it makes this code easier
        hassert(isAssociative(op) && isCommutative(op));
        hassert(!elements.empty());
        if (elements.size() == 1)
            return IRExprSingleRootDag::deep_copy(elements[0]);
        // TODO we should merge all them together into 1 dag first, and then do
        // the queue thing, as that would
        //  avoid copies
        IRExprDag_t dag;
        std::deque<Node *> roots;
        for (const IRExprSingleRootDag &e : elements) {
            roots.push_back(dag.addSubdag(e));
        }

        hassert(roots.size() >= 2);
        do {
            Node *r0 = roots.front();
            roots.pop_front();
            Node *r1 = roots.front();
            roots.pop_front();
            roots.push_back(dag.newNode(Node::makeBinaryOp(op, r0, r1)));
        } while (roots.size() >= 2);
        hassert(roots.size() == 1);
        Node *root = roots.front();
        dag.roots = {root};
        return IRExprSingleRootDag{std::move(dag)};
    }

    // returns whether any remap was applied.
    static bool
    remapVars(IRExprSingleRootDag &dag,
              const std::unordered_map<
                  VarIdType, std::unique_ptr<IRExprSingleRootDag>> &remaps) {
        return IRExprDag_t::remapVars(dag.m_dag, remaps);
    }
    static bool
    remapVars(IRExprSingleRootDag &dag,
              const std::unordered_map<VarIdType, VarIdType> &remaps) {
        return IRExprDag_t::remapVars(dag.m_dag, remaps);
    }

    [[nodiscard]] Node *root() { return m_dag.roots[0]; }
    [[nodiscard]] const Node *root() const { return m_dag.roots[0]; }
    [[nodiscard]] int width() const { return root()->width; }

    explicit IRExprSingleRootDag(IRExprDag_t dag)
        : m_dag{std::move(dag)} {
        hassert(m_dag.roots.size() == 1);
    };

    IRExprSingleRootDag() = default;
    ~IRExprSingleRootDag() = default;
    IRExprSingleRootDag(const IRExprSingleRootDag &) = delete;
    IRExprSingleRootDag &operator=(const IRExprSingleRootDag &) = delete;
    IRExprSingleRootDag(IRExprSingleRootDag &&) noexcept = default;
    IRExprSingleRootDag &operator=(IRExprSingleRootDag &&) noexcept = default;

    [[nodiscard]] static IRExprSingleRootDag
    makeQuery(std::unique_ptr<IRExprSingleRootDag> selector,
              std::unique_ptr<IRExprSingleRootDag> l,
              std::unique_ptr<IRExprSingleRootDag> r) {
        IRExprSingleRootDag result;
        result.m_dag.roots.push_back(result.m_dag.newNode(Node::makeQuery(
            result.m_dag.addSubdag(*selector), result.m_dag.addSubdag(*l),
            result.m_dag.addSubdag(*r))));
        return result;
    }

    [[nodiscard]] static IRExprSingleRootDag
    makeBinaryOp(IRBinaryOpType op, std::unique_ptr<IRExprSingleRootDag> l,
                 std::unique_ptr<IRExprSingleRootDag> r) {
        IRExprSingleRootDag result;
        result.m_dag.roots.push_back(result.m_dag.newNode(Node::makeBinaryOp(
            op, result.m_dag.addSubdag(*l), result.m_dag.addSubdag(*r))));
        return result;
    }

    [[nodiscard]] static IRExprSingleRootDag
    makeUnaryOp(IRUnaryOpType op, std::unique_ptr<IRExprSingleRootDag> l) {
        IRExprSingleRootDag result;
        result.m_dag.roots.push_back(result.m_dag.newNode(
            Node::makeUnaryOp(op, result.m_dag.addSubdag(*l))));
        return result;
    }

    [[nodiscard]] static IRExprSingleRootDag makeConstant(BigInt val,
                                                          int bit_width) {
        IRExprSingleRootDag result;
        result.m_dag.roots.push_back(result.m_dag.newNode(
            Node::makeConstant(std::move(val), bit_width)));
        return result;
    }

    [[nodiscard]] static IRExprSingleRootDag
    makeVariableAccess(VarIdType id, const IdPool &id_pool) {
        IRExprSingleRootDag result;
        result.m_dag.roots.push_back(
            result.m_dag.newNode(Node::makeVariableAccess(id, id_pool)));
        return result;
    }
    [[nodiscard]] static IRExprSingleRootDag makeVariableAccess(VarIdType id,
                                                                int bit_width) {
        IRExprSingleRootDag result;
        result.m_dag.roots.push_back(
            result.m_dag.newNode(Node::makeVariableAccess(id, bit_width)));
        return result;
    }

    [[nodiscard]] static IRExprSingleRootDag
    makeResize(std::unique_ptr<IRExprSingleRootDag> e, int width) {
        IRExprSingleRootDag result;
        result.m_dag.roots.push_back(result.m_dag.newNode(
            Node::makeResize(result.m_dag.addSubdag(*e), width)));
        return result;
    }

    [[nodiscard]] static IRExprSingleRootDag
    makeBitfield(std::unique_ptr<IRExprSingleRootDag> e, int hi, int lo) {
        IRExprSingleRootDag result;
        result.m_dag.roots.push_back(result.m_dag.newNode(
            Node::makeBitfield(result.m_dag.addSubdag(*e), hi, lo)));
        return result;
    }
};

template <typename Tag, typename VarIdType>

typename IRExprDag<Tag, VarIdType>::Node *IRExprDag<Tag, VarIdType>::addSubdag(
    const IRExprSingleRootDag<Tag, VarIdType> &other) {
    // This could be done much better. For now, us a map that maps `old pointer`
    // to `new pointer`
    auto o_roots = addSubdag(other.m_dag);
    hassert(o_roots.size() == 1);
    return o_roots[0];
}

template <typename Tag, typename VarIdType>
void addIdsUsedByExpr(std::unordered_set<VarIdType> &usages,
                      const IRExprDag<Tag, VarIdType> &dag) {
    IRExprDag<Tag, VarIdType>::iterNodes(dag, [&](const auto &n) {
        if (n.type() == IRExprTypeKind::Var) {
            usages.insert(n.u_var().id);
        }
    });
}

template <typename Tag, typename VarIdType>
void addIdsUsedByExpr(std::unordered_set<VarIdType> &usages,
                      const IRExprSingleRootDag<Tag, VarIdType> &dag) {
    addIdsUsedByExpr(usages, dag.m_dag);
}

template <typename Tag, typename VarIdType>
void addIdsUsedByExpr(std::unordered_set<VarIdType> &usages,
                      const IRExprDag<Tag, VarIdType> &,
                      const typename IRExprDag<Tag, VarIdType>::Node *node) {
    IRExprDag<Tag, VarIdType>::iterNodesBelow(node, [&](const auto &n) {
        if (n.type() == IRExprTypeKind::Var) {
            usages.insert(n.u_var().id);
        }
    });
}

template <typename Tag, typename VarIdType>
std::unordered_set<VarIdType>
getIdsUsedByExpr(const IRExprDag<Tag, VarIdType> &e) {
    std::unordered_set<VarIdType> usages;
    addIdsUsedByExpr(usages, e);
    return usages;
}
template <typename Tag, typename VarIdType>
std::unordered_set<VarIdType>
getIdsUsedByExpr(const IRExprSingleRootDag<Tag, VarIdType> &e) {
    return getIdsUsedByExpr(e.m_dag);
}

template <typename Tag, typename VarIdType>
std::unordered_set<VarIdType>
getIdsUsedByExpr(const IRExprDag<Tag, VarIdType> &e,
                 const typename IRExprDag<Tag, VarIdType>::Node *node) {
    std::unordered_set<VarIdType> usages;
    addIdsUsedByExpr(usages, e, node);
    return usages;
}

} // namespace ChpOptimize
