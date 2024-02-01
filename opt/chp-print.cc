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
#include "chp-print.h"
#include "hassert.h"

#include "chp-graph.h"

namespace ChpOptimize {

namespace {

std::string str_of_id(OptionalVarId id) {
    return id ? "v" + std::to_string((*id).m_id) : "vNULL";
}
std::string str_of_id(ChanId id) { return "C" + std::to_string(id.m_id); }

std::string unary_op_marker(IRUnaryOpType op) {
    switch (op) {
    case IRUnaryOpType::Not:
        return " ~";
    case IRUnaryOpType::UnaryMinus:
        return " -";
    }
    hassert(false);
    return "";
}
std::string binary_op_marker(IRBinaryOpType op) {
    switch (op) {
    case IRBinaryOpType::And:
        return " & ";
    case IRBinaryOpType::Or:
        return " | ";
    case IRBinaryOpType::Xor:
        return " ^ ";
    case IRBinaryOpType::Plus:
        return " + ";
    case IRBinaryOpType::Minus:
        return " - ";
    case IRBinaryOpType::Mult:
        return " * ";
    case IRBinaryOpType::Div:
        return " / ";
    case IRBinaryOpType::Mod:
        return " % ";
    case IRBinaryOpType::LeftShift:
        return " << ";
    case IRBinaryOpType::RightShift:
        return " >> ";
    case IRBinaryOpType::ArithmeticRightShift:
        return " >>> ";
    case IRBinaryOpType::LT:
        return " < ";
    case IRBinaryOpType::GT:
        return " > ";
    case IRBinaryOpType::LE:
        return " <= ";
    case IRBinaryOpType::GE:
        return " >= ";
    case IRBinaryOpType::EQ:
        return " = ";
    case IRBinaryOpType::NE:
        return " != ";
    case IRBinaryOpType::Concat:
        hassert(false);
    }
    hassert(false);
    return "";
}

// TODO track priority to remove parens
void print_expr_inline_(std::ostream &o, const ChpExprDag::Node *root) {
    switch (root->type()) {
    case IRExprTypeKind::Const:
        o << root->u_cons().v.to_hex_string() << "i" << root->u_cons().v_width;
        break;
    case IRExprTypeKind::Var:
        o << str_of_id(root->u_var().id);
        break;
    case IRExprTypeKind::BinaryOp:
        if (root->u_e2().op_type == IRBinaryOpType::Concat) {
            o << "{";
            print_expr_inline_(o, root->u_e2().l);
            o << ", ";
            print_expr_inline_(o, root->u_e2().r);
            o << "}";
        } else {
            o << "(";
            print_expr_inline_(o, root->u_e2().l);
            o << binary_op_marker(root->u_e2().op_type);
            print_expr_inline_(o, root->u_e2().r);
            o << ")";
        }
        break;
    case IRExprTypeKind::UnaryOp:
        o << "(";
        o << unary_op_marker(root->u_e1().op_type);
        print_expr_inline_(o, root->u_e1().l);
        o << ")";
        break;
    case IRExprTypeKind::Query:
        o << "(";
        print_expr_inline_(o, root->u_query().selector);
        o << " ? ";
        print_expr_inline_(o, root->u_query().l);
        o << " : ";
        print_expr_inline_(o, root->u_query().r);
        o << ")";
        break;
    case IRExprTypeKind::Bitfield:
        print_expr_inline_(o, root->u_bitfield().e);
        o << "{" << root->u_bitfield().hi() << ".." << root->u_bitfield().lo()
          << "}";
        break;
    }
}
// TODO track priority to remove parens
void print_expr_block(std::ostream &o, const ChpExprDag &dag, int ilevel) {
    int nl = 0;
    std::string indent = std::string(ilevel * 4, ' ');
    std::unordered_map<const ChpExprDag ::Node *, int> labels;
    o << "START BLOCK\n";
    ChpExprDag::iterNodes(dag, [&](const ChpExprDag::Node &n) {
        int l = nl++;
        labels[&n] = l;
        o << indent << "      n" << l << " := ";
        switch (n.type()) {
        case IRExprTypeKind::Const:
            o << n.u_cons().v.to_hex_string() << "i" << n.u_cons().v_width;
            break;
        case IRExprTypeKind::Var:
            o << str_of_id(n.u_var().id);
            break;
        case IRExprTypeKind::BinaryOp:

            if (n.u_e2().op_type == IRBinaryOpType::Concat) {
                o << "{n" << labels.at(n.u_e2().l) << ", "
                  << "n" << labels.at(n.u_e2().r) << "}";
            } else {
                o << "n" << labels.at(n.u_e2().l)
                  << binary_op_marker(n.u_e2().op_type) << "n"
                  << labels.at(n.u_e2().r);
            }
            break;
        case IRExprTypeKind::UnaryOp:
            o << unary_op_marker(n.u_e1().op_type) << "n"
              << labels.at(n.u_e1().l);
            break;
        case IRExprTypeKind::Query:
            o << "n" << labels.at(n.u_query().selector) << " ? n"
              << labels.at(n.u_query().l) << " : n" << labels.at(n.u_query().r);
            break;
        case IRExprTypeKind::Bitfield:
            o << "n" << labels.at(n.u_bitfield().e) << "{"
              << n.u_bitfield().hi() << ".." << n.u_bitfield().lo() << "}";
            break;
        }
        o << "\n";
    });
    o << indent << "      OUTPUT(";
    for (const auto *root : dag.roots)
        o << "n" << labels.at(root) << ", ";
    o << ")\n";
    o << indent << "END BLOCK" << std::endl;
}

void print_expr(std::ostream &o, const ChpExprDag &dag, int ilevel) {
    if (dag.roots.size() > 1 ||
        ChpExprDag::getSumFlattenedSizeRoots(dag) > BigInt{20})
        print_expr_block(o, dag, ilevel);
    else
        print_expr_inline_(o, dag.roots[0]);
}

void print_expr(std::ostream &o, const ChpExprSingleRootDag &dag, int ilevel) {
    print_expr(o, dag.m_dag, ilevel);
}


 void print_chp(std::ostream &o, Sequence sequence, int ilevel,
		std::function<void(std::ostream &os, const Block &b)> pre,
		std::function<void(std::ostream &os, const Block &b)> post
		) {
    std::string indent = std::string(ilevel * 4, ' ');
    Block *curr = sequence.startseq->child();
    bool seq_first = true;

    if (curr->type() == BlockType::EndSequence) {
        o << indent << "skip";
        return;
    }

    while (curr->type() != BlockType::EndSequence) {
        if (!seq_first)
            o << ";" << std::endl;

	pre (o,*curr);
        seq_first = false;
        switch (curr->type()) {
        case BlockType::Basic:
            switch (curr->u_basic().stmt.type()) {
            case StatementType::Assign: {
                const auto &ids = curr->u_basic().stmt.u_assign().ids;
                if (ids.size() == 1) {
                    o << indent << str_of_id(ids[0]) << " := ";
                    print_expr(o, curr->u_basic().stmt.u_assign().e, ilevel);
                } else {
                    o << indent << "(";
                    bool is_first_id = true;
                    for (const auto &id : ids) {
                        if (!is_first_id)
                            o << ", ";
                        is_first_id = false;
                        o << str_of_id(id);
                    }
                    o << ") := ";
                    print_expr(o, curr->u_basic().stmt.u_assign().e, ilevel);
                }
                break;
            }
            case StatementType::Send: {
                o << indent << str_of_id(curr->u_basic().stmt.u_send().chan)
                  << "!";
                print_expr(o, curr->u_basic().stmt.u_send().e, ilevel);
                break;
            }
            case StatementType::Receive:
                o << indent << str_of_id(curr->u_basic().stmt.u_receive().chan)
                  << "?" << str_of_id(curr->u_basic().stmt.u_receive().var);
                break;
            }
            break;
        case BlockType::Par: {
            bool first = true;
            for (const auto &branch : curr->u_par().splits) {
                o << indent << "(";
                bool is_first_id = true;
                for (const auto &id : branch.branch_ids) {
                    if (!is_first_id)
                        o << ", ";
                    is_first_id = false;
                    o << str_of_id(id);
                }
                o << ") = phi_inv(";
                o << str_of_id(branch.pre_id) << ");" << std::endl;
            }
            for (const auto &path : curr->u_par().branches) {
                if (!first)
                    o << indent << " ||" << std::endl;
                o << indent << "{" << std::endl;
                print_chp(o, path, ilevel + 1,pre,post);
                o << std::endl;
                o << indent << "}" << std::endl;
                first = false;
            }
            // print merges
            for (const auto &branch : curr->u_par().merges) {
                o << ";" << std::endl
                  << indent << str_of_id(branch.post_id) << " = phi(";
                bool is_first_id = true;
                for (const auto &id : branch.branch_ids) {
                    if (!is_first_id)
                        o << ", ";
                    is_first_id = false;
                    o << str_of_id(id);
                }
                o << ")";
            }
            break;
        }
        case BlockType::Select: {
            // print splits
            for (const auto &branch : curr->u_select().splits) {
                o << indent << "(";
                bool is_first_id = true;
                for (const auto &id : branch.branch_ids) {
                    if (!is_first_id)
                        o << ", ";
                    is_first_id = false;
                    o << str_of_id(id);
                }
                o << ") = phi_inv(";
                o << str_of_id(branch.pre_id) << ");" << std::endl;
            }

            o << indent << "[" << std::endl;
            bool first = true;
            for (const auto &branch : curr->u_select().branches) {
                o << indent << (first ? "   " : "[] ");
                switch (branch.g.type()) {
                case IRGuardType::Expression:
                    print_expr(o, branch.g.u_e().e, ilevel);
                    break;
                case IRGuardType::Else:
                    o << "else";
                    break;
                }
                o << " ->" << std::endl;
                print_chp(o, branch.seq, ilevel + 1,pre,post);
                o << std::endl;
                first = false;
            }
            o << indent << "]";

            // print merges
            for (const auto &branch : curr->u_select().merges) {
                o << ";" << std::endl
                  << indent << str_of_id(branch.post_id) << " = phi(";
                bool is_first_id = true;
                for (const auto &id : branch.branch_ids) {
                    if (!is_first_id)
                        o << ", ";
                    is_first_id = false;
                    o << str_of_id(id);
                }
                o << ")";
            }

            break;
        }
        case BlockType::DoLoop: {
            o << indent << "*[" << std::endl;
            for (const auto &phi : curr->u_doloop().in_phis) {
                o << indent << str_of_id(phi.bodyin_id) << " = phi(";
                o << str_of_id(phi.pre_id) << ", _ );" << std::endl;
            }
            for (const auto &phi : curr->u_doloop().loop_phis) {
                o << indent << str_of_id(phi.bodyin_id) << " = lphi(";
                o << str_of_id(phi.pre_id) << ", tmp_"
                  << str_of_id(phi.bodyout_id) << ");" << std::endl;
            }
            print_chp(o, curr->u_doloop().branch, ilevel + 1,pre,post);
            o << std::endl;
            for (const auto &phi : curr->u_doloop().out_phis) {
                o << indent << "(dum_" << str_of_id(phi.bodyout_id) << ",";
                o << str_of_id(phi.post_id) << ") = phi_inv("
                  << str_of_id(phi.bodyout_id) << ");" << std::endl;
            }
            for (const auto &phi : curr->u_doloop().loop_phis) {
                o << indent << "(tmp_" << str_of_id(phi.bodyout_id) << ",";
                o << str_of_id(phi.post_id) << ") = lphi_inv("
                  << str_of_id(phi.bodyout_id) << ");" << std::endl;
            }
            o << indent << "  <- ";
            print_expr(o, curr->u_doloop().guard, ilevel);
            o << std::endl;
            o << indent << "]";

            break;
        }
        case BlockType::StartSequence:
        case BlockType::EndSequence:
            hassert(false); // FALLTHROUGH
        }
	post(o,*curr);
        curr = curr->child();
    }
}

 
} // namespace

void print_chp(std::ostream &o, const ChpGraph &graph) {
  auto pre = [&] (std::ostream &o, const Block &b) { return; };
  print_chp(o, graph.m_seq, 0, pre, pre);
}

void print_chp(std::ostream &o, const ChpGraph &graph,
	       std::function<void(std::ostream &os, const Block &b)> pre,
	       std::function<void(std::ostream &os, const Block &b)> post) {
  print_chp(o, graph.m_seq, 0, pre, post);
}


void print_chp_block(std::ostream &o, Block *curr, int ilevel)
{
  auto pre = [&] (std::ostream &o, const Block &b) { return; };
  
  std::string indent = std::string(ilevel * 4, ' ');
  if (curr->type() == BlockType::EndSequence) {
    o << indent << "skip";
    return;
  }

  switch (curr->type()) {
  case BlockType::Basic:
    switch (curr->u_basic().stmt.type()) {
    case StatementType::Assign: {
      const auto &ids = curr->u_basic().stmt.u_assign().ids;
      if (ids.size() == 1) {
	o << indent << str_of_id(ids[0]) << " := ";
	print_expr(o, curr->u_basic().stmt.u_assign().e, ilevel);
      } else {
	o << indent << "(";
	bool is_first_id = true;
	for (const auto &id : ids) {
	  if (!is_first_id)
	    o << ", ";
	  is_first_id = false;
	  o << str_of_id(id);
	}
	o << ") := ";
	print_expr(o, curr->u_basic().stmt.u_assign().e, ilevel);
      }
      break;
    }
    case StatementType::Send: {
      o << indent << str_of_id(curr->u_basic().stmt.u_send().chan)
	<< "!";
      print_expr(o, curr->u_basic().stmt.u_send().e, ilevel);
      break;
    }
    case StatementType::Receive:
      o << indent << str_of_id(curr->u_basic().stmt.u_receive().chan)
	<< "?" << str_of_id(curr->u_basic().stmt.u_receive().var);
      break;
    }
    break;
  case BlockType::Par: {
    bool first = true;
    for (const auto &branch : curr->u_par().splits) {
      o << indent << "(";
      bool is_first_id = true;
      for (const auto &id : branch.branch_ids) {
	if (!is_first_id)
	  o << ", ";
	is_first_id = false;
	o << str_of_id(id);
      }
      o << ") = phi_inv(";
      o << str_of_id(branch.pre_id) << ");" << std::endl;
    }
    for (const auto &path : curr->u_par().branches) {
      if (!first)
	o << indent << " ||" << std::endl;
      o << indent << "{" << std::endl;
      print_chp(o, path, ilevel + 1,pre,pre);
      o << std::endl;
      o << indent << "}" << std::endl;
      first = false;
    }
    // print merges
    for (const auto &branch : curr->u_par().merges) {
      o << ";" << std::endl
	<< indent << str_of_id(branch.post_id) << " = phi(";
      bool is_first_id = true;
      for (const auto &id : branch.branch_ids) {
	if (!is_first_id)
	  o << ", ";
	is_first_id = false;
	o << str_of_id(id);
      }
      o << ")";
    }
    break;
  }
  case BlockType::Select: {
    // print splits
    for (const auto &branch : curr->u_select().splits) {
      o << indent << "(";
      bool is_first_id = true;
      for (const auto &id : branch.branch_ids) {
	if (!is_first_id)
	  o << ", ";
	is_first_id = false;
	o << str_of_id(id);
      }
      o << ") = phi_inv(";
      o << str_of_id(branch.pre_id) << ");" << std::endl;
    }

    o << indent << "[" << std::endl;
    bool first = true;
    for (const auto &branch : curr->u_select().branches) {
      o << indent << (first ? "   " : "[] ");
      switch (branch.g.type()) {
      case IRGuardType::Expression:
	print_expr(o, branch.g.u_e().e, ilevel);
	break;
      case IRGuardType::Else:
	o << "else";
	break;
      }
      o << " ->" << std::endl;
      print_chp(o, branch.seq, ilevel + 1,pre,pre);
      o << std::endl;
      first = false;
    }
    o << indent << "]";

    // print merges
    for (const auto &branch : curr->u_select().merges) {
      o << ";" << std::endl
	<< indent << str_of_id(branch.post_id) << " = phi(";
      bool is_first_id = true;
      for (const auto &id : branch.branch_ids) {
	if (!is_first_id)
	  o << ", ";
	is_first_id = false;
	o << str_of_id(id);
      }
      o << ")";
    }

    break;
  }
  case BlockType::DoLoop: {
    o << indent << "*[" << std::endl;
    for (const auto &phi : curr->u_doloop().in_phis) {
      o << indent << str_of_id(phi.bodyin_id) << " = phi(";
      o << str_of_id(phi.pre_id) << ", _ );" << std::endl;
    }
    for (const auto &phi : curr->u_doloop().loop_phis) {
      o << indent << str_of_id(phi.bodyin_id) << " = lphi(";
      o << str_of_id(phi.pre_id) << ", tmp_"
	<< str_of_id(phi.bodyout_id) << ");" << std::endl;
    }
    print_chp(o, curr->u_doloop().branch, ilevel + 1, pre,pre);
    o << std::endl;
    for (const auto &phi : curr->u_doloop().out_phis) {
      o << indent << "(dum_" << str_of_id(phi.bodyout_id) << ",";
      o << str_of_id(phi.post_id) << ") = phi_inv("
	<< str_of_id(phi.bodyout_id) << ");" << std::endl;
    }
    for (const auto &phi : curr->u_doloop().loop_phis) {
      o << indent << "(tmp_" << str_of_id(phi.bodyout_id) << ",";
      o << str_of_id(phi.post_id) << ") = lphi_inv("
	<< str_of_id(phi.bodyout_id) << ");" << std::endl;
    }
    o << indent << "  <- ";
    print_expr(o, curr->u_doloop().guard, ilevel);
    o << std::endl;
    o << indent << "]";

    break;
  }
  case BlockType::StartSequence:
  case BlockType::EndSequence:
    hassert(false); // FALLTHROUGH
  }
}
 
} // namespace ChpOptimize
