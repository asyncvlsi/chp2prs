/*************************************************************************
 *
 *  This file is part of the ACT library
 *
 *  Copyright (c) 2018-2020 Lincoln Berkley
 *  Copyright (c) 2021-2022 Henry Heffan
 *  Copyright (c) 2024 Rajit Manohar
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

#include "static-tokens.h"
#include "chp-opt.h"
#include "algos.h"

namespace ChpOptimize {

namespace {

using VarIdRemap = std::unordered_map<VarId, VarId>;
struct VarIdRemapPair {
    VarIdRemap inputid_from_origid;
    VarIdRemap outputid_from_origid;
};

VarIdRemapPair concat(VarIdRemapPair &&a, VarIdRemapPair &&b) {
    // if something is an output of `a` and an input of `b`, they must agree on
    // the name. if something is an input of `a` and `b` and not an output of
    // `a`, then `a` and `b` must agree on its name
    VarIdRemapPair result;
    result.inputid_from_origid = a.inputid_from_origid;
    for (const auto &[orig_id, new_id] : b.inputid_from_origid) {
        if (a.outputid_from_origid.count(orig_id)) {
            hassert(new_id == a.outputid_from_origid.at(orig_id));
            // dont add this to the input map, as it is now a read from inside
            // the sequence instead of external
        } else if (a.inputid_from_origid.count(orig_id)) {
            hassert(new_id == a.inputid_from_origid.at(orig_id));
            // as orig is not wirtten in a, they must both read into the same
            // name
        } else {
            result.inputid_from_origid[orig_id] = new_id;
        }
    }
    result.outputid_from_origid = b.outputid_from_origid;
    for (const auto &[orig_id, new_id] : a.outputid_from_origid) {
        if (!b.outputid_from_origid.count(orig_id)) {
            // if `b` writes to the same variable, then `a` shouldnt not output
            // its adress
            result.outputid_from_origid[orig_id] = new_id;
        }
    }

    return result;
}
std::unordered_map<VarId, VarId>
get_input_renames_after(const VarIdRemapPair &renames) {
    VarIdRemap combined = {};
    // first union together previous with renames.inputid_from_origid,
    // hasserting that if they share keys, they also share the values associated
    // with that key
    for (const auto &[orig_id, new_id] : renames.inputid_from_origid)
        combined[orig_id] = new_id;
    // Then replace the old value ids if we wrote to that id
    for (const auto &[orig_id, new_id] : renames.outputid_from_origid)
        combined[orig_id] = new_id;
    return combined;
}

// The converted code will have the following properties.
// - Every variable will be writen to exactly once.
// - A variable *only* lives in the scope it is declared in, not
// subscopes.
 
// - No variable will share a VarId with the "default" id assigned by the
// id_pool to the ActId.

// - If a variable has a renaming in the `old_rename_from_input`, and that
// variable is not assigned to, it will be
//   assigned according to that map
 
// - Every original variable id which is written will be in the
// `renamed_from_output` map, where the value stored at the
//   old id will be the final id used to write to it
 
// - DoLoops and Selects will have correctly filled in Phi and PhiInv
// - functions
 
// - If a variable appears in the `renamed_from_input` map, reads of that value
// will be renamed to that value.
//   Otherwise, a new temporary id will be created and used for reads, and that
//   will be added to the map. If a variable is reassigned, reads after that
//   will not be read from the same id.
 
// - Every write to a variable will appear in the final `renamed_from_output`
// map. If a variable is reassigned, only the
//   final write will appear in the map.

// The correct mental representation of this code is as follows. The original
// code `c` is a block which takes in I and outputs O. There is some map `sigma`
// from I to a subset of I' which is encoded by
// `old_rename_from_input`.
//
// We want to create a bunch of code C which
// - takes in I' and outputs O' such that union(I, O) is disjoint with union(I',
// O')
// - there is a map `sigma'` from I to I' which agrees with `sigma` on every
// shared input
// - there is a map `gamma` from O to O'
// - the behavior of C, after variable renaming on the input and output, is the
// same as the behavior of `c`

// The below function takes in `c` and `sigma` (enocded as
// old_rename_from_input), and it outputs a RenamedIRSequence, which is a triple
// of `sigma'` (m_renamed_from_input), the code of C (in m_seq), and `gamma` (in
// m_renamed_from_output)

// it is convenient to define `sigma` as mapping elements of I' to themselfs.

VarId do_expr_renaming(VarId input_id, VarIdRemapPair &renaming,
                       const std::unordered_map<VarId, VarId> &previous_renames,
                       IdPool &id_pool) {
    if (renaming.inputid_from_origid.count(input_id) &&
        previous_renames.count(input_id))
        hassert(renaming.inputid_from_origid.at(input_id) ==
                previous_renames.at(input_id));

    if (renaming.inputid_from_origid.count(input_id))
        return renaming.inputid_from_origid.at(input_id);

    if (previous_renames.count(input_id)) {
        renaming.inputid_from_origid[input_id] = previous_renames.at(input_id);
        return previous_renames.at(input_id);
    }

    VarId new_id = id_pool.cloneVar (input_id);
    renaming.inputid_from_origid[input_id] = new_id;
    return new_id;
}


/*
 * input_id is mapped to either the renamed version, or remains
 * unchanged
 */
VarId new_do_expr_renaming(VarId input_id, VarIdRemap &renaming)
 {
  if (renaming.count(input_id)) {
    return renaming.at(input_id);
  }
  return input_id;
}

/*
 * rename variables in an expression
 */ 
ChpExprDag do_expr_renaming(ChpExprDag dag, VarIdRemapPair &renaming,
                            const VarIdRemap &previous_renames,
                            IdPool &id_pool) {
    ChpExprDag::mapNodes(dag, [&](ChpExprDag::Node &n) {
        if (n.type() == IRExprTypeKind::Var) {
            n.u_var().id = do_expr_renaming(n.u_var().id, renaming,
                                            previous_renames, id_pool);
        }
    });
    return dag;
}


/*
 * rename variables in an expression, given a renaming map.
 */ 
 ChpExprDag new_do_expr_renaming(ChpExprDag dag, VarIdRemap &renaming)
{
  ChpExprDag::mapNodes(dag, [&](ChpExprDag::Node &n) {
      if (n.type() == IRExprTypeKind::Var) {
	n.u_var().id = new_do_expr_renaming(n.u_var().id, renaming);
      }
    });
  return dag;
}


 ChpExprSingleRootDag do_expr_renaming(ChpExprSingleRootDag dag,
                                      VarIdRemapPair &renaming,
                                      const VarIdRemap &previous_renames,
                                      IdPool &id_pool) {
    return ChpExprSingleRootDag(do_expr_renaming(std::move(dag.m_dag), renaming,
                                                 previous_renames, id_pool));
}

ChpExprSingleRootDag new_do_expr_renaming(ChpExprSingleRootDag dag,
					  VarIdRemap &renaming)
{
  return ChpExprSingleRootDag(new_do_expr_renaming(std::move(dag.m_dag),
						   renaming));
}

VarId do_assigning_renaming(
    VarId input_id, VarIdRemapPair &renaming,
    const std::unordered_map<VarId, VarId> & /*previous_renames*/,
    IdPool &id_pool) {
  VarId new_id = id_pool.cloneVar (input_id);
    hassert(!renaming.outputid_from_origid.count(input_id));
    renaming.outputid_from_origid[input_id] = new_id;
    return new_id;
}


VarId new_do_assigning_renaming(VarId input_id,
				VarIdRemap &curmap,
				IdPool &id_pool)
{
  /* fresh LHS */
  VarId new_id = id_pool.cloneVar (input_id);

  /* future renames in this block use this updated map */
  curmap[input_id] = new_id;
  
  return new_id;
}


template <typename K>
std::unordered_set<K> absl_intersection(const std::unordered_set<K> &a,
                                        const std::unordered_set<K> &b) {
    std::unordered_set<K> result;
    for (const auto &x : a) {
        if (b.count(x))
            result.insert(x);
    }
    return result;
}

template <typename K, typename V>
std::unordered_set<K> absl_keys(const std::unordered_map<K, V> &a) {
    std::unordered_set<K> result;
    for (const auto &[x, _] : a) {
        result.insert(x);
    }
    return result;
}

// this renames all the variables inside of seq, and returns a VarIdRemapPair
// describing all the inputs and outputs to the block. Then passed in table
// stack allows us to create fewer phi functions because to every variable is
// needs be passed out of the select/do-loops. The table stack does not change
// the correctness of the code, but allows us to generate better chp
VarIdRemapPair enforce_static_token_form(Sequence seq,
                                         BlockAllocator &blockAllocator,
                                         IdPool &id_pool) {
    // whenever inputid_from_origid shares a value with
    // previous_newid_from_origid, they must also agree on the value
    VarIdRemapPair seq_renames;

    Block *curr = seq.startseq->child();
    while (curr->type() != BlockType::EndSequence) {
        auto renames_before_curr = get_input_renames_after(seq_renames);
        VarIdRemapPair curr_renames;
        switch (curr->type()) {
        case BlockType::Basic: {
            switch (curr->u_basic().stmt.type()) {
            case StatementType::Assign: {
                curr->u_basic().stmt.u_assign().e = do_expr_renaming(
                    std::move(curr->u_basic().stmt.u_assign().e), curr_renames,
                    renames_before_curr, id_pool);
                for (auto &id : curr->u_basic().stmt.u_assign().ids)
                    id = do_assigning_renaming(id, curr_renames,
                                               renames_before_curr, id_pool);
                break;
            }
            case StatementType::Send: {
                curr->u_basic().stmt.u_send().e = do_expr_renaming(
                    std::move(curr->u_basic().stmt.u_send().e), curr_renames,
                    renames_before_curr, id_pool);

                break;
            }
            case StatementType::Receive: {
                if (curr->u_basic().stmt.u_receive().var) {
                    curr->u_basic().stmt.u_receive().var =
                        do_assigning_renaming(
                            *curr->u_basic().stmt.u_receive().var, curr_renames,
                            renames_before_curr, id_pool);
                }
            }
            }
            break;
        }
        case BlockType::Par: {
            // A well-formed program, according to professor Manohar, will
            // satisfy the following conditions. In a Par block, each variable
            // is either (1) never written OR (2) read/written on at most one
            // branch. Therefore, the input rename table to each branch should
            // include the renames by the previous branches, so if a new
            // variable is read in one branch, it has the same name in every
            // branch. The outputs should never have the same variable written
            // to.

            // conceptually, the order of pieces is `PHI_INV; BRANCHES; PHI`
            std::vector<VarIdRemapPair> branch_remapses;
            for (auto &branch : curr->u_par().branches) {
                // each branch is in its own scope. Therefore, it does not have
                // any fixed renames before it starts. It will start again from
                // scratch, and we will connect up its new names to the names in
                // this scope using phi functions
                auto branch_remaps =
                    enforce_static_token_form(branch, blockAllocator, id_pool);
                branch_remapses.push_back(branch_remaps);
            }

            // build the input phi functions. To do this, first, union together
            // the set of all input ids. Then, add a phi function for each input
            // id. Every orig_id which is read on any branch is given a phi
            // function compute UNION (keys(seq.irseq.m_renamed_from_input) for
            // seq in seqs)
            std::unordered_set<VarId> all_orig_input_ids;
            for (const auto &branch_remaps : branch_remapses) {
                for (const auto &[orig_id, _] :
                     branch_remaps.inputid_from_origid)
                    all_orig_input_ids.insert(orig_id);
            }

            std::unordered_set<VarId> all_orig_output_ids;
            for (const auto &branch_remaps : branch_remapses) {
                for (const auto &[orig_id, _] :
                     branch_remaps.outputid_from_origid)
                    all_orig_output_ids.insert(orig_id);
            }

            // build the input phi functions
            for (const VarId orig_id :
                 Algo::as_sorted_vector<VarId>(all_orig_input_ids)) {
                std::vector<OptionalVarId> ids_on_branches =
                    Algo::map1<OptionalVarId>(
                        branch_remapses,
                        [&](const VarIdRemapPair &branch_remaps) {
                            return Algo::value_default(
                                branch_remaps.inputid_from_origid, orig_id,
                                OptionalVarId::null_id());
                        });
                // then apply the renaming to the orig_id
                VarId input_id = do_expr_renaming(orig_id, curr_renames,
                                                  renames_before_curr, id_pool);
                curr->u_par().splits.push_back({input_id, ids_on_branches});
            }

            // The, build one output phi functions for each assigned variable.
            for (const VarId orig_id :
                 Algo::as_sorted_vector<VarId>(all_orig_output_ids)) {
                std::vector<OptionalVarId> ids_on_branches =
                    Algo::map1<OptionalVarId>(
                        branch_remapses,
                        [&](const VarIdRemapPair &branch_remaps) {
                            return Algo::value_default(
                                branch_remaps.outputid_from_origid, orig_id,
                                OptionalVarId::null_id());
                        });

                hassert(Algo::count_if(ids_on_branches,
                                       [&](const OptionalVarId &var_id) {
                                           return static_cast<bool>(var_id);
                                       }) == 1);

                auto output_id = do_assigning_renaming(
                    orig_id, curr_renames, renames_before_curr, id_pool);
                curr->u_par().merges.push_back({ids_on_branches, output_id});
            }

            break;
        }
	  
        case BlockType::Select: {
            hassert(curr->u_select().splits.empty());
            hassert(curr->u_select().merges.empty());
            // conceptually, the order of pieces is `GUARD; PHI_INV; BRANCHES;
            // PHI`
            for (auto &branch : curr->u_select().branches) {
                if (branch.g.type() == IRGuardType::Expression) {
                    branch.g.u_e().e = do_expr_renaming(
                        std::move(branch.g.u_e().e), curr_renames,
                        renames_before_curr, id_pool);
                }
            }

            std::vector<VarIdRemapPair> branch_remapses;
            for (auto &branch : curr->u_select().branches) {
                // each branch is in its own scope. Therefore, it does not have
                // any fixed renames before it starts. It will start again from
                // scratch, and we will connect up its new names to the names in
                // this scope using phi functions
                auto branch_remaps = enforce_static_token_form(
                    branch.seq, blockAllocator, id_pool);
                branch_remapses.push_back(branch_remaps);
            }

            // build the input phi functions. To do this, first, union together
            // the set of all input ids. Then, add a phi function for each input
            // id. Every orig_id which is read on any branch is given a phi
            // function compute UNION (keys(seq.irseq.m_renamed_from_input) for
            // seq in seqs)
            std::unordered_set<VarId> all_orig_input_ids;
            for (const auto &branch_remaps : branch_remapses) {
                for (const auto &[orig_id, _] :
                     branch_remaps.inputid_from_origid)
                    all_orig_input_ids.insert(orig_id);
            }

            std::unordered_set<VarId> all_orig_output_ids;
            for (const auto &branch_remaps : branch_remapses) {
                for (const auto &[orig_id, _] :
                     branch_remaps.outputid_from_origid)
                    all_orig_output_ids.insert(orig_id);
            }

            //            const auto &tables_after_select =
            //            tables.at(curr->child()); all_orig_output_ids =
            //            Algo::filter_set<VarId>(all_orig_output_ids, [&](const
            //            VarId var_id) {
            //                const LivenessLattice *lat =
            //                tables_after_select.lookup(var_id); return lat &&
            //                lat->is_alive == IsAlive::yes;
            //            });

            // if an output id is not assigned on every branch, then we need to
            // copy the pre-existing value. In order to copy the preexisting
            // value, we need to add a split with this id.
            for (const auto output_id : all_orig_output_ids) {
                for (VarIdRemapPair &branch_remaps : branch_remapses) {
                    if (!branch_remaps.outputid_from_origid.count(output_id)) {
                        // then add a read of the output_id on this branch, and
                        // also add this output_id to the set of
                        // not-completely-written ids
                        all_orig_input_ids.insert(output_id);
                        if (!branch_remaps.inputid_from_origid.count(
                                output_id)) {
                            branch_remaps.inputid_from_origid[output_id] =
			      id_pool.cloneVar (output_id);
                        }
                    }
                }
            }

            // Now we have the complete set of candidate reads and writes needed
            // to build the phi functions. Now we hassert that every read value
            // is alive, and we cut down out set to only include alive values.
            // Next, we build one phi_inv function for each read variable. We
            // sort these to make the code deterministic, as absl's hash tables
            // arent (which is a good design choice on their part)
            for (const VarId orig_id :
                 Algo::as_sorted_vector<VarId>(all_orig_input_ids)) {
                std::vector<OptionalVarId> ids_on_branches =
                    Algo::map1<OptionalVarId>(
                        branch_remapses,
                        [&](const VarIdRemapPair &branch_remaps) {
                            return Algo::value_default(
                                branch_remaps.inputid_from_origid, orig_id,
                                OptionalVarId::null_id());
                        });
                // then apply the renaming to the orig_id
                VarId input_id = do_expr_renaming(orig_id, curr_renames,
                                                  renames_before_curr, id_pool);
                curr->u_select().splits.push_back({input_id, ids_on_branches});
            }

            // The, build one output phi functions for each assigned variable.
            for (const VarId orig_id :
                 Algo::as_sorted_vector<VarId>(all_orig_output_ids)) {
                // const LivenessLattice *lat =
                // tables_after_select.lookup(orig_id); if(lat && lat->is_alive
                // == IsAlive::yes) {
                std::vector<VarId> ids_on_branches = Algo::map1<VarId>(
                    branch_remapses, [&](const VarIdRemapPair &branch_remaps) {
                        return Algo::value_default_func(
                            branch_remaps.outputid_from_origid, orig_id, [&]() {
                                // we should have added this up above if the
                                // variable is not assigned
                                return branch_remaps.inputid_from_origid.at(
                                    orig_id);
                            });
                    });
                auto output_id = do_assigning_renaming(
                    orig_id, curr_renames, renames_before_curr, id_pool);
                curr->u_select().merges.push_back({ids_on_branches, output_id});
            }
            break;
        }
	  
        case BlockType::DoLoop: {
            hassert(curr->u_doloop().in_phis.empty());
            hassert(curr->u_doloop().out_phis.empty());
            hassert(curr->u_doloop().loop_phis.empty());
            VarIdRemapPair branch_remaps = enforce_static_token_form(
                curr->u_doloop().branch, blockAllocator, id_pool);
            auto renames_before_guard = get_input_renames_after(branch_remaps);
            VarIdRemapPair guard_remaps;
            curr->u_doloop().guard =
                do_expr_renaming(std::move(curr->u_doloop().guard),
                                 guard_remaps, renames_before_guard, id_pool);
            branch_remaps =
                concat(std::move(branch_remaps), std::move(guard_remaps));

            // Now we need to buidl the phi functions. There are 3 cases
            const auto all_orig_input_ids =
                absl_keys(branch_remaps.inputid_from_origid);
            const auto all_orig_output_ids =
                absl_keys(branch_remaps.outputid_from_origid);
            const auto loop_ids =
                absl_intersection(all_orig_input_ids, all_orig_output_ids);

            //            const auto &tables_after_doloop =
            //            tables.at(curr->child()); auto is_alive = [](const
            //            auto &table_stack, VarId var_id) {
            //                const LivenessLattice *lat =
            //                table_stack.lookup(var_id); return lat &&
            //                lat->is_alive == IsAlive::yes;
            //            };

            // These are variables that change each iteration of the loop. We
            // apply the following transformation Before:   h := BLAH; *[ h :=
            // BLAH(h) <- g]; BLAH(h) After:   h0 := BLAH; *[ h1 := phi(h0, h3);
            // h2 := BLAH(h1); (h3, h4) := phiinv(h2) <- g]; BLAH(h4)
            for (const auto &loop_id :
                 Algo::as_sorted_vector<VarId>(loop_ids)) {
                VarId pre_id = do_expr_renaming(loop_id, curr_renames,
                                                renames_before_curr, id_pool);
                OptionalVarId post_id =
                    true // is_alive(tables_after_doloop, loop_id)
                        ? do_assigning_renaming(loop_id, curr_renames,
                                                renames_before_curr, id_pool)
                        : OptionalVarId::null_id();
                curr->u_doloop().loop_phis.push_back(
                    Block::Variant_DoLoop::LoopPhi{
                        pre_id, branch_remaps.inputid_from_origid.at(loop_id),
                        branch_remaps.outputid_from_origid.at(loop_id),
                        post_id});
            }

            // These are variables that are non-conditional reads
            // Before:   h := BLAH; *[ BLAH(h) <- g]; BLAH(h)
            // After:   h0 := BLAH; *[ h1 := phi(h0, h2); (h2, h3) := phiinv(h1)
            // <- g]; BLAH(h3)
            for (const auto &read_id : Algo::as_sorted_vector<VarId>(
			     Algo::set_minus_sub(all_orig_input_ids, loop_ids))) {
                VarId pre_id = do_expr_renaming(read_id, curr_renames,
                                                renames_before_curr, id_pool);
                curr->u_doloop().in_phis.push_back(Block::Variant_DoLoop::InPhi{
                    pre_id, branch_remaps.inputid_from_origid.at(read_id)});
            }

            // These are variables that are non-conditional writes. We dont need
            // to loop the variable because it gets written on every loop
            // Before:   *[ h := BLAH() <- g]; BLAH(h) After:   *[ h0 := BLAH;
            // (_, h1) := phiinv(h0) <- g]; BLAH(h1)
            for (auto write_id : Algo::as_sorted_vector<VarId>(
                     Algo::set_minus_sub(all_orig_output_ids, loop_ids))) {
                if (true) { // is_alive(tables_after_doloop, write_id)) {
                    VarId post_id = do_assigning_renaming(
                        write_id, curr_renames, renames_before_curr, id_pool);
                    curr->u_doloop().out_phis.push_back(
                        Block::Variant_DoLoop::OutPhi{
                            branch_remaps.outputid_from_origid.at(write_id),
                            post_id});
                }
            }

            break;
        }
        case BlockType::StartSequence:
        case BlockType::EndSequence:
            hassert(false);
            break;
        }

        seq_renames = concat(std::move(seq_renames), std::move(curr_renames));
        curr = curr->child();
    }
    return seq_renames;
}



using VarSet = std::unordered_set<VarId>;

/* 
 * This converts the CHP to static token form
 *
 * The VarIdRemapPair contains two mappings:
 *
 *    - orig input var -> new var name (used for conditional uses)
 *    - orig output var -> new var name (used for new defs)
 *
 * The "rename_input" flag is used to rename uses. This flag is set
 * whenever we need a PHI' function: in loops and selects.
 *
 * We also keep track of an auxillary map which contains the
 * correspondence between the original VarIds to the newly generated
 * ones.
 *
 */
static
void _run_seq (Sequence seq,
	       VarIdRemap &curmap,
	       IdPool &id_pool,
	       std::unordered_map<const Block *, VarSet> &livein,
	       std::unordered_map<const Block *, VarSet> &liveout,
	       std::unordered_map<const Block *, UsesAndDefs> &defuse
	       )
{
  Block *curr = seq.startseq->child();
  while (curr->type() != BlockType::EndSequence) {
    switch (curr->type()) {
    case BlockType::Basic: {
      switch (curr->u_basic().stmt.type()) {
      case StatementType::Assign:
	curr->u_basic().stmt.u_assign().e =
	  new_do_expr_renaming (std::move(curr->u_basic().stmt.u_assign().e),
				curmap);
	for (auto &id : curr->u_basic().stmt.u_assign().ids) {
	  id = new_do_assigning_renaming(id, curmap, id_pool);
	}
	break;

      case StatementType::Send:
	curr->u_basic().stmt.u_send().e =
	  new_do_expr_renaming (std::move(curr->u_basic().stmt.u_send().e),
				curmap);
	break;

      case StatementType::Receive:
	if (curr->u_basic().stmt.u_receive().var) {
	  curr->u_basic().stmt.u_receive().var =
	    new_do_assigning_renaming(*curr->u_basic().stmt.u_receive().var,
				      curmap, id_pool);
	}
      }
      break;
    }
    case BlockType::Par: {
      // A well-formed program, according to professor Manohar, will
      // satisfy the following conditions. In a Par block, each variable
      // is either (1) never written OR (2) read/written on at most one
      // branch. Therefore, the input rename table to each branch should
      // include the renames by the previous branches, so if a new
      // variable is read in one branch, it has the same name in every
      // branch. The outputs should never have the same variable written
      // to.

      // This means we can just run the STF steps sequentially in each
      // branch!
      for (auto &branch : curr->u_par().branches) {
	_run_seq (branch, curmap, id_pool, livein, liveout, defuse);
      }
      break;
    }
	  
    case BlockType::Select: {
      hassert(curr->u_select().splits.empty());
      hassert(curr->u_select().merges.empty());

      std::vector <VarIdRemap> newcurmaps;
      
      // rename guards based on the current map
      for (auto &branch : curr->u_select().branches) {
	if (branch.g.type() == IRGuardType::Expression) {
	  branch.g.u_e().e =
	    new_do_expr_renaming (std::move(branch.g.u_e().e), curmap);
	}
	VarIdRemap tmp = curmap;
	newcurmaps.push_back(tmp);
      }

      // conceptually, the order of pieces is `GUARD; PHI_INV; BRANCHES;
      // PHI`

      // 1. We know what invphi functions are needed
      // 2. Add them.
      // 3. For each branch, compute a new curmap with the renamed
      // invphi variables
      // 4. Add merges at the end using the curmap at the end of each
      // branch.


      /* 
	 Create the local phiinv functions.

	 phiinv functions are needed for all uses within the
	 selection. So the variables to be remapped must include
	 variables used in any of the branches.

	 If a variable is conditionally defined, then we need
	 to "pass through" the original value by ensuring that the
	 variable is in the phiinv block. We have this info already.
      */
      std::unordered_map<VarId, std::vector<OptionalVarId> > calcsplit;
      for (auto &var : livein[curr]) {
	// Variables that are livein to the selection are candidates
	// for a split.
	// Check if they are either dead in all branches, or are pure
	// pass-throughs
	int count_passthru = 0;
	int count_dead = 0;
	for (auto &br : curr->u_select().branches) {
	  if (br.seq.empty()) {
	    count_passthru++;
	  }
	  else if (!livein[br.seq.startseq->child()].contains(var)) {
	    count_dead++;
	  }
	  else if (!defuse[br.seq.startseq].var_reads.contains(var)) {
	    count_passthru++;
	  }
	}
	if (count_passthru == curr->u_select().branches.size()) {
	  // nothing to do here, pure passthrough
	}
	else if (count_dead == curr->u_select().branches.size()) {
	  // nothing to do here, pure dead
	}
	else {
	  // we need to add a split
	  int ii = 0;
	  for (auto &br : curr->u_select().branches) {
	    if (br.seq.empty()) {
	      // passthru
	      calcsplit[var].push_back
		(new_do_assigning_renaming (var, newcurmaps[ii], id_pool));
	    }
	    else if (!livein[br.seq.startseq->child()].contains(var)) {
	      calcsplit[var].push_back(OptionalVarId::null_id());
	    }
	    else {
	      calcsplit[var].push_back
		(new_do_assigning_renaming (var, newcurmaps[ii], id_pool));
	    }
	    ii++;
	  }
	}
      }
      
      for (auto &[var, vec] : calcsplit) {
	curr->u_select().splits.push_back(
					  { new_do_expr_renaming (var, curmap),
					      vec });
      }

      int ii = 0;
      for (auto &br : curr->u_select().branches) {
	_run_seq (br.seq, newcurmaps[ii], id_pool,
		  livein, liveout, defuse);
	ii++;
      }

      /*
	 phi functions are needed for any defs within the
	 selection. So the variables to be remapped must include any
	 variables defined.
      */
      for (auto &var : liveout[curr]) {
	if (defuse.contains(curr)) {
	  if (defuse[curr].var_writes.contains (var)) {
	    std::vector<VarId> branch_ids;
	    int ii = 0;
	    for (auto &br : curr->u_select().branches) {
	      branch_ids.push_back (new_do_expr_renaming (var,newcurmaps[ii]));
	      ii++;
	    }
	    curr->u_select().merges.push_back(
					      { branch_ids,
						  new_do_assigning_renaming (var, curmap, id_pool) } );
	  }
	  else {
	    // nothing to do! There is no write to the variable
	    // anywhere, so the normal variable is bypassed through
	  }
	}
      }
      break;
    }
    case BlockType::DoLoop: {
      hassert(curr->u_doloop().in_phis.empty());
      hassert(curr->u_doloop().out_phis.empty());
      hassert(curr->u_doloop().loop_phis.empty());

      VarIdRemap outmap = curmap;

      struct tmploop {
	VarId var;
	
	VarId preid, inid;
	OptionalVarId outid;
      };
      
      std::vector<tmploop> loopphis;

      for (auto &var : livein[curr]) {
	if (defuse[curr].var_reads.contains (var)) {
	  if (!defuse[curr].var_writes.contains (var)) {
	    // Scenario I:
	    //     h := BLAH; *[ USE(h) <- g ]; ...
	    // in_phi

	    // h is live-in to the loop, and not defined in the loop
	    //  new h0 := in_phi(out-h), and we update the "outmap" used for
	    //  the body of the loop, but *not* the curmap after processing
	    //  the loop.
	    
	    VarId preid;
	    VarId invar = new_do_assigning_renaming (var, outmap, id_pool);
	    if (curmap.contains (var)) {
	      preid = curmap[var];
	    }
	    else {
	      preid = var;
	    }
	    curr->u_doloop().in_phis.push_back (Block::Variant_DoLoop::InPhi{preid, invar});
	  }
	  else {
	    // Scenario II:
	    //     h := BLAH; *[ USE(h); DEF(h) <- g ]; ...
	    // loop_phi

	    VarId preid;
	    if (curmap.contains (var)) {
	      preid = curmap[var];
	    }
	    else {
	      preid = var;
	    }
	    
	    OptionalVarId out_id;
	    if (liveout[curr].contains (var)) {
	      out_id = OptionalVarId{new_do_assigning_renaming (var, curmap, id_pool)};
	    }
	    else {
	      out_id = OptionalVarId::null_id();
	    }

	    VarId in_id = new_do_assigning_renaming (var, outmap, id_pool);

	    // body out is the value of the variable after we do
	    // _run_seq!
	    loopphis.push_back ({var, preid, in_id, out_id});
	  }
	}
      }

      _run_seq (curr->u_doloop().branch, outmap, id_pool, livein, liveout, defuse);
      curr->u_doloop().guard =
	new_do_expr_renaming(std::move(curr->u_doloop().guard), outmap);

      // update loopphi output map
      for (auto &tmp : loopphis) {
	curr->u_doloop().loop_phis.push_back({tmp.preid, tmp.inid,
	      outmap[tmp.var], tmp.outid});
	if (tmp.outid) {
	  curmap[tmp.var] = *tmp.outid;
	}
      }

      // Scenario:
      //     h := BLAH; *[ no-use(h); DEF(h) ... <- g ]; USE(h)
      // out_phi
      for (auto &var : liveout[curr]) {
	if (!livein[curr].contains (var)) {
	  curr->u_doloop().out_phis.push_back({outmap[var],
		new_do_assigning_renaming (var, curmap, id_pool)});
	}
      }
      break;
    }
    case BlockType::StartSequence:
    case BlockType::EndSequence:
      hassert(false);
      break;
    }
    curr = curr->child();
  }
}
 

  
} // namespace

void putIntoStaticTokenForm(ChpGraph &g) {
    hassert(!g.is_static_token_form);

    auto remaps =
        enforce_static_token_form(g.m_seq, g.blockAllocator(), g.id_pool());

    // the default value of variables in 0. However, the IR does not support
    // uninitialized variables being read. Therefore, add assignments for each
    // uninitialized variable.
    for (const auto &[orig_id, new_id] : remaps.inputid_from_origid) {
        Block *assign = g.blockAllocator().newBlock(
            Block::makeBasicBlock(Statement::makeAssignment(
                new_id, ChpExprSingleRootDag::makeConstant(
                            BigInt{0}, g.id_pool().getBitwidth(new_id)))));
        Block *start = g.m_seq.startseq->child();
        Block::disconnect(g.m_seq.startseq, start);
        Block::connect(g.m_seq.startseq, assign);
        Block::connect(assign, start);
    }

    g.is_static_token_form = true;
}

static
void checkLiveness (Sequence seq, std::unordered_set<VarId> &potentially_dead)
{
  Block *curr = seq.startseq->child();
  while (curr->type() != BlockType::EndSequence) {
    switch (curr->type()) {
    case BlockType::Basic: {
      switch (curr->u_basic().stmt.type()) {
      case StatementType::Assign: {
	ChpExprDag::mapNodes(curr->u_basic().stmt.u_assign().e,
			     [&](ChpExprDag::Node &n) {
			       if (n.type() == IRExprTypeKind::Var) {
				 if (potentially_dead.count(n.u_var().id)) {
				   potentially_dead.erase(n.u_var().id);
				 }
			       }
			     });
	for (const auto &var : curr->u_basic().stmt.u_assign().ids) {
	  if (potentially_dead.count(var)) {
	    potentially_dead.erase (var);
	  }
	}
	break;
      }
      case StatementType::Send: {
	ChpExprDag::mapNodes(curr->u_basic().stmt.u_send().e.m_dag,
			     [&](ChpExprDag::Node &n) {
			       if (n.type() == IRExprTypeKind::Var) {
				 if (potentially_dead.count(n.u_var().id)) {
				   potentially_dead.erase(n.u_var().id);
				 }
			       }
			     });
	break;
      }
      case StatementType::Receive: {
	if (curr->u_basic().stmt.u_receive().var) {
	  if (potentially_dead.count(*curr->u_basic().stmt.u_receive().var)) {
	    potentially_dead.erase (*curr->u_basic().stmt.u_receive().var);
	  }
	}
	break;
      }
      }
      break;
    }
    case BlockType::Par: {
      for (auto &branch : curr->u_par().branches) {
	checkLiveness (branch, potentially_dead);
      }
      break;
    }
    case BlockType::Select: {
      for (auto &branch : curr->u_select().branches) {
	if (branch.g.type() == IRGuardType::Expression) {
	  ChpExprDag::mapNodes(branch.g.u_e().e.m_dag,
			     [&](ChpExprDag::Node &n) {
			       if (n.type() == IRExprTypeKind::Var) {
				 if (potentially_dead.count(n.u_var().id)) {
				   potentially_dead.erase(n.u_var().id);
				 }
			       }
			     });
	  
	}
	checkLiveness (branch.seq, potentially_dead);
      }
      break;
    }
    case BlockType::DoLoop: {
      checkLiveness (curr->u_doloop().branch, potentially_dead);
      ChpExprDag::mapNodes(curr->u_doloop().guard.m_dag,
			   [&](ChpExprDag::Node &n) {
			     if (n.type() == IRExprTypeKind::Var) {
			       if (potentially_dead.count(n.u_var().id)) {
				 potentially_dead.erase(n.u_var().id);
			       }
			     }
			   });
      for (auto &loop_phi : curr->u_doloop().loop_phis) {
	if (potentially_dead.count(loop_phi.bodyout_id)) {
	  potentially_dead.erase (loop_phi.bodyout_id);
	}
      }
      break;
    }
    case BlockType::StartSequence:
    case BlockType::EndSequence:
      hassert(false);
      break;
    }
    curr = curr->child();
  }
}

static
void prunePhis (Sequence seq, std::unordered_set<VarId> &deadvars,
		std::unordered_set<VarId> &new_pot_dead)
{
  Block *curr = seq.startseq->child();
  while (curr->type() != BlockType::EndSequence) {
    switch (curr->type()) {
    case BlockType::Basic: {
      break;
    }
    case BlockType::Par: {
      for (auto &branch : curr->u_par().branches) {
	prunePhis (branch, deadvars, new_pot_dead);
      }
      break;
    }
    case BlockType::Select: {
      // check and prune phis
      std::vector<Block::Variant_Select::PhiSplit> newsplits;
      for (auto &phisplit : curr->u_select().splits) {
	// remove any dead variable from the split
	for (auto &var : phisplit.branch_ids) {
	  if (var && deadvars.contains(*var)) {
	    var = OptionalVarId::null_id();
	  }
	}

	// if everything is dead, we kill the split
	if (Algo::all_of (phisplit.branch_ids,
			  [&](const OptionalVarId x) { return !(x); })) {
	  new_pot_dead.insert(phisplit.pre_id);
	  // phisplit.pre_id is potentially no longer used...
	}
	else {
	  newsplits.push_back (phisplit);
	}
      }
      curr->u_select().splits = newsplits;

      std::vector<Block::Variant_Select::PhiMerge> newmerges;
      for (auto &phimerge : curr->u_select().merges) {
	if (!deadvars.contains (phimerge.post_id)) {
	  newmerges.push_back (phimerge);
	}
	else {
	  for (auto &var : phimerge.branch_ids) {
	    new_pot_dead.insert (var);
	  }
	}
      }
      curr->u_select().merges = newmerges;
      
      for (auto &branch : curr->u_select().branches) {
	prunePhis (branch.seq, deadvars, new_pot_dead);
      }
      break;
    }
    case BlockType::DoLoop: {
      // check and prune phis
      std::vector<Block::Variant_DoLoop::OutPhi> new_outphis;
      for (auto &outphi : curr->u_doloop().out_phis) {
	if (deadvars.contains (outphi.post_id)) {
	  new_pot_dead.insert (outphi.bodyout_id);
	}
	else {
	  new_outphis.push_back (outphi);
	}
      }
      curr->u_doloop().out_phis = new_outphis;

      for (auto &loopphi : curr->u_doloop().loop_phis) {
	if (loopphi.post_id && deadvars.contains (*loopphi.post_id)) {
	  loopphi.post_id = OptionalVarId::null_id();
	}
      }
      
      prunePhis (curr->u_doloop().branch, deadvars, new_pot_dead);
      break;
    }
    case BlockType::StartSequence:
    case BlockType::EndSequence:
      hassert(false);
      break;
    }
    curr = curr->child();
  }
}

static
void mergePhis (Sequence seq)
{
  Block *curr = seq.startseq->child();
  while (curr->type() != BlockType::EndSequence) {
    switch (curr->type()) {
    case BlockType::Basic: {
      break;
    }
    case BlockType::Par: {
      for (auto &branch : curr->u_par().branches) {
	mergePhis (branch);
      }
      break;
    }
    case BlockType::Select: {
      for (auto &branch : curr->u_select().branches) {
	mergePhis (branch.seq);
      }
      break;
    }
    case BlockType::DoLoop: {
      // check for merge opportunities combining outphis and loopphis 
      std::vector<Block::Variant_DoLoop::OutPhi> new_outphis;
      for (auto &outphi : curr->u_doloop().out_phis) {
	bool moved = false;
	for (auto &loopphi : curr->u_doloop().loop_phis) {
	  if (!loopphi.post_id && loopphi.bodyout_id == outphi.bodyout_id) {
	    loopphi.post_id = outphi.post_id;
	    moved = true;
	    break;
	  }
	}
	if (!moved) {
	  new_outphis.push_back (outphi);
	}
      }
      curr->u_doloop().out_phis = new_outphis;

      mergePhis (curr->u_doloop().branch);
      break;
    }
    case BlockType::StartSequence:
    case BlockType::EndSequence:
      hassert(false);
      break;
    }
    curr = curr->child();
  }
}



void putIntoNewStaticTokenForm(ChpGraph &g) {
    hassert(!g.is_static_token_form);

    auto [livein, liveout] = getLiveVars (g);

    auto defuse = getDefUsesTable(g);

    auto printvars = [&] (std::ostream &os, std::unordered_set<VarId> &vs)
      {
       if (vs.empty()) {
	return;
       }
       bool first = true;
       for (auto &v :
	      Algo::as_sorted_vector<VarId,std::unordered_set<VarId>> (vs)) {
	 if (!first) {
	   os << ",";
	 }
	 os << "v" << v.m_id;
	 first = false;
       }
      };


    // the default value of variables in 0. However, the IR does not support
    // uninitialized variables being read. Therefore, add assignments for each
    // uninitialized variable.
    if (!g.m_seq.empty()) {
      auto uninit_vars = livein[g.m_seq.startseq->child()];
      if (!uninit_vars.empty()) {
	for (const auto &v :
	       Algo::as_sorted_vector<VarId,std::unordered_set<VarId>>(uninit_vars)) {
	  Block *assign = g.blockAllocator().newBlock(
            Block::makeBasicBlock(Statement::makeAssignment(
                v, ChpExprSingleRootDag::makeConstant(
                            BigInt{0}, g.id_pool().getBitwidth(v)))));
	  Block *start = g.m_seq.startseq->child();
	  Block::disconnect(g.m_seq.startseq, start);
	  Block::connect(g.m_seq.startseq, assign);
	  Block::connect(assign, start);
	}
      }
    }
    
#if 0
    auto pre = [&] (std::ostream &os, const Block &b)
      {
       if (livein.contains (&b)) {
  	os << " {";
	printvars (os, livein[&b]);
	os << "}";
       }
       else { return; }

       if (b.type() == BlockType::Select) {
	   os << " {{ splits: ";
	   for (auto &var : livein[&b]) {
	     os << "v" << var.m_id << "->";
	     for (auto &br : b.u_select().branches) {
	       if (br.seq.empty()) {
		 os << "p";
	       }
	       else if (!livein[br.seq.startseq->child()].contains(var)) {
		 os << "_";
	       }
	       else if (defuse[br.seq.startseq].var_reads.contains(var)) {
		 os << "|";
	       }
	       else {
		 os << "p";
	       }
	     }
	     os << " ";
	   }
	   os << "}}" << std::endl;
       }
       else if (b.type() == BlockType::DoLoop) {


       }       
      };
    auto post = [&] (std::ostream &os, const Block &b)
      {
       if (liveout.contains (&b)) {
  	os << " {";
	printvars(os, liveout[&b]);
	os << "}";
       }

       if (b.type() == BlockType::Select) {
	   os << " {{ pot-merges: ";
	   for (auto &var : liveout[&b]) {
	     if (defuse.contains (&b)) {
	       auto &du = defuse[&b];
	       if (du.var_writes.contains (var)) {
		 os << "v" << var.m_id << "<-";
		 os << " ";
	       }
	     }
	   }
	   os << "}}" << std::endl;
       }
      };

    printf ("-------\n");
    print_chp (std::cout, g, pre, post);
    printf ("-------\n\n");
#endif

    VarIdRemap curmap;
    _run_seq (g.m_seq, curmap, g.id_pool(), livein, liveout, defuse);

#if 0
    printf ("/*----\n");
    print_chp (std::cout, g);
    printf ("\n----*/\n\n");
#endif    

    // remove redundant copies introduced because of the
    // on self-assignment constraint.
    //   x := x + 1
    // would get turned into
    //   xnew := x + 1; x := xnew
    // ... and so afer STF this would become
    //   xnew := x + 1; xnew2 := xnew
    // introducing a redundant copy.
    eliminateCopies (g);

    // finally merge any Phis where you can; this is needed because
    // copy elimination can get rid of some variables, allowing phiinv
    // functions to have the same RHS
    mergePhis (g.m_seq);
    
    g.is_static_token_form = true;
}


namespace {

class RegisterAllocationGraph {
    std::set<VarId> nodes;
    std::set<std::pair<VarId, VarId>> conflicts;
    std::set<std::pair<VarId, VarId>> must_merge;
    std::map<std::pair<VarId, VarId>, int> rewards;
    void insert_conflict(const VarId &a, const VarId &b) {
        if (a.m_id < b.m_id)
            conflicts.insert({a, b});
        else
            conflicts.insert({b, a});
    }
    void add_reward(const VarId &a, const VarId &b, int reward) {
        if (a.m_id < b.m_id)
            rewards[{a, b}] += reward;
        else
            rewards[{b, a}] += reward;
    }
    int get_reward(const VarId &a, const VarId &b) {
        std::pair<VarId, VarId> k;
        if (a.m_id < b.m_id)
            k = {a, b};
        else
            k = {b, a};
        if (rewards.contains(k))
            return rewards.at(k);
        return 0;
    }
    void insert_must_merge(const VarId &a, const VarId &b) {
        if (a.m_id < b.m_id)
            must_merge.insert({a, b});
        else
            must_merge.insert({b, a});
    }

  public:
    void add_set_interferes_set(const std::set<VarId> &A,
                                const std::set<VarId> &B);
    void
    add_set_interferes_set(const std::set<VarId> &A, const std::set<VarId> &B,
                           const std::set<std::pair<VarId, VarId>> &fusions,
                           int reward);
    void add_set_interferes_clique(const std::set<VarId> &A);
    void
    add_set_interferes_clique(const std::set<VarId> &A,
                              const std::set<std::pair<VarId, VarId>> &fusions,
                              int reward);
    void add_fuse_clique_required(const std::set<VarId> &A);

    friend class ColoredRegisterAllocationGraph;
};

class ColoredRegisterAllocationGraph {
  public:
    struct ColorId {
        int id;
        auto operator<=>(const ColorId &) const = default;
    };

  private:
    RegisterAllocationGraph m_g;
    std::unordered_map<VarId, ColorId> m_coloring;

  public:
    static ColoredRegisterAllocationGraph solve(RegisterAllocationGraph &&g,
                                                const IdPool &id_pool);

    [[nodiscard]] const RegisterAllocationGraph &g() const { return m_g; }
    [[nodiscard]] const std::unordered_map<VarId, ColorId> &coloring() const {
        return m_coloring;
    }
};

} // namespace

} // namespace ChpOptimize

template <>
struct std::hash<ChpOptimize::ColoredRegisterAllocationGraph::ColorId> {
    size_t operator()(
        const ChpOptimize::ColoredRegisterAllocationGraph::ColorId &obj) const {
        return std::hash<int>()((int)obj.id);
    }
};

namespace ChpOptimize {

namespace {

struct DefUses {
    std::set<VarId> defs, uses; // within the parent scope, non-recursive
};
struct DefUsesTable {
    std::unordered_map<const Block *, DefUses> block_defuses; // not recursive
    std::unordered_map<const Block *, DefUses>
        seq_rec_defuses; // maps from the "startseq" block
};

void build_def_uses_table_helper_block(const Sequence &seq,
                                       DefUsesTable &table) {
    for (Block *curr = seq.startseq->child(); curr != seq.endseq;
         curr = curr->child()) {
        DefUses result;
        switch (curr->type()) {
        case BlockType::Basic: {
            switch (curr->u_basic().stmt.type()) {
            case StatementType::Assign: {
                for (const auto id :
                     getIdsUsedByExpr(curr->u_basic().stmt.u_assign().e))
                    result.uses.insert(id);
                for (const auto id : curr->u_basic().stmt.u_assign().ids)
                    result.defs.insert(id);
                break;
            }
            case StatementType::Send: {
                for (const auto id :
                     getIdsUsedByExpr(curr->u_basic().stmt.u_send().e))
                    result.uses.insert(id);
                break;
            }
            case StatementType::Receive: {
                if (curr->u_basic().stmt.u_receive().var)
                    result.defs.insert(*curr->u_basic().stmt.u_receive().var);
                break;
            }
            }
            break;
        }
        case BlockType::Par: {
            // viewed as a black box in the current scope, add the definitions
            // and uses
            for (const auto &split : curr->u_par().splits)
                result.uses.insert(split.pre_id);
            for (const auto &merge : curr->u_par().merges)
                result.defs.insert(merge.post_id);

            // Then recurse!
            for (const auto &branch : curr->u_par().branches)
                build_def_uses_table_helper_block(branch, table);
            break;
        }
        case BlockType::Select: {
            // viewed as a black box in the current scope, add the definitions
            // and uses
            for (const auto &branch : curr->u_select().branches) {
                if (branch.g.type() == IRGuardType::Expression) {
                    for (const auto id : getIdsUsedByExpr(branch.g.u_e().e))
                        result.uses.insert(id);
                }
            }
            for (const auto &split : curr->u_select().splits)
                result.uses.insert(split.pre_id);
            for (const auto &merge : curr->u_select().merges)
                result.defs.insert(merge.post_id);

            // Then recurse!
            for (const auto &branch : curr->u_select().branches)
                build_def_uses_table_helper_block(branch.seq, table);
            break;
        }
        case BlockType::DoLoop: {
            // viewed as a black box in the current scope, add the definitions
            // and uses
            for (const auto &phi : curr->u_doloop().in_phis)
                result.uses.insert(phi.pre_id);
            for (const auto &phi : curr->u_doloop().out_phis)
                result.defs.insert(phi.post_id);
            for (const auto &phi : curr->u_doloop().loop_phis) {
                result.uses.insert(phi.pre_id);
                if (phi.post_id)
                    result.defs.insert(*phi.post_id);
            }

            // Then recurse!
            build_def_uses_table_helper_block(curr->u_doloop().branch, table);
            break;
        }
        case BlockType::StartSequence:
        case BlockType::EndSequence:
            hassert(false);
            break;
        }
        table.block_defuses[curr] = result;
    }
}

void build_def_uses_table_helper_seq(const Sequence &seq, DefUsesTable &table) {
    auto union_in = [](auto &S, const auto &O) {
        for (auto x : O)
            S.insert(x);
    };

    DefUses result;
    for (Block *curr = seq.startseq->child(); curr != seq.endseq;
         curr = curr->child()) {
        switch (curr->type()) {
        case BlockType::Basic: {
            const auto &shallow_defuse = table.block_defuses.at(curr);
            union_in(result.defs, shallow_defuse.defs);
            union_in(result.uses, shallow_defuse.uses);
            break;
        }
        case BlockType::Par: {
            // we "use" every variable coming into and out of the block. This
            // means that a different parallel branch will add conflicts between
            // this variable and its own declarations

            // Splits are fused, so we only need to add the pre_id to "uses".
            // Similarly, "merges" are always fused, so we only need to add the
            // post_id to "uses"
            for (const auto &split : curr->u_par().splits)
                result.uses.insert(split.pre_id);
            for (const auto &merge : curr->u_par().merges)
                result.uses.insert(merge.post_id);

            // build the def_use entry for each subbranch
            for (const auto &branch : curr->u_par().branches) {
                build_def_uses_table_helper_seq(branch, table);
                const DefUses &branch_defuses =
                    table.seq_rec_defuses.at(branch.startseq);
                union_in(result.defs, branch_defuses.defs);
                union_in(result.uses, branch_defuses.uses);
            }

            // For a "par" block, every split and every merge is required to be
            // fused. Therefore, there are no "true" definitions in the phi
            // functions. Therefore, there is nothing else to do
            break;
        }
        case BlockType::Select: {
            // we "use" every variable coming into and out of the block. This
            // means that a different parallel branch will add conflicts between
            // this variable and its own declarations
            for (const auto &branch : curr->u_select().branches) {
                if (branch.g.type() == IRGuardType::Expression) {
                    for (const auto id : getIdsUsedByExpr(branch.g.u_e().e))
                        result.uses.insert(id);
                }
            }

            // Splits are fused, so we only need to add the pre_id to "uses".
            for (const auto &split : curr->u_select().splits) {
                result.uses.insert(split.pre_id);
            }
            for (const auto &merge : curr->u_select().merges) {
                for (const auto &id : merge.branch_ids)
                    result.uses.insert(id);
                result.uses.insert(merge.post_id);
            }

            // build the def_use entry for each subbranch
            for (const auto &branch : curr->u_select().branches) {
                build_def_uses_table_helper_seq(branch.seq, table);
                const DefUses &branch_defuses =
                    table.seq_rec_defuses.at(branch.seq.startseq);
                union_in(result.defs, branch_defuses.defs);
                union_in(result.uses, branch_defuses.uses);
            }

            // For a "select" block, every split is required to be fused.
            // Therefore, there are no "true" definitions in the split
            // functions. However, some merges might not be true merges.
            // Therefore, for now, we add those as true definitions.
            for (const auto &merge : curr->u_select().merges)
                result.defs.insert(merge.post_id);
            break;
        }
        case BlockType::DoLoop: {
            // viewed as a black box in the current scope, add the definitions
            // and uses
            for (const auto &phi :
                 curr->u_doloop()
                     .in_phis) // always fused, so dont need to add "bodyin_id"
                result.uses.insert(phi.pre_id);
            for (const auto &phi :
                 curr->u_doloop().out_phis) // always fused, so dont need to add
                                            // "bodyout_id"
                result.uses.insert(phi.post_id);
            for (const auto &phi : curr->u_doloop().loop_phis) {
                // "pre_id" is never fused with "bodyin_id", and "bodyout_id" is
                // always fused with "post_id" (if it has it)
                result.uses.insert(phi.pre_id);
                result.uses.insert(phi.bodyin_id);
                result.uses.insert(phi.bodyout_id);
            }

            // build the def_use entry for the subbranch
            build_def_uses_table_helper_seq(curr->u_doloop().branch, table);
            const DefUses &branch_defuses =
                table.seq_rec_defuses.at(curr->u_doloop().branch.startseq);
            union_in(result.defs, branch_defuses.defs);
            union_in(result.uses, branch_defuses.uses);

            // InPhi and OutPhi functions are always merged. The only "optional"
            // merge is the "bodyout_id" to the "bodyin_id". Therefore, add a
            // "definition" for the "body_in" id, because its possible this will
            // be needed
            for (const auto &phi : curr->u_doloop().loop_phis)
                result.defs.insert(phi.bodyin_id);
            break;
        }
        case BlockType::StartSequence:
        case BlockType::EndSequence:
            hassert(false);
            break;
        }
    }
    table.seq_rec_defuses[seq.startseq] = std::move(result);
}

DefUsesTable build_def_uses_table(const ChpGraph &g) {
    DefUsesTable table;
    build_def_uses_table_helper_block(g.m_seq, table);
    build_def_uses_table_helper_seq(g.m_seq, table);
    return table;
}

struct PackedBranch {
    const Sequence &seq;
    std::set<VarId> phi_in_ids, out_ids;
};

std::vector<PackedBranch>
get_branches_with_packed_ids(const Block::Variant_Par &par) {
    std::vector<PackedBranch> r;
    for (const Sequence &seq : par.branches)
        r.push_back({seq, {}, {}});
    for (const auto &split : par.splits) {
        for (size_t i = 0; i < split.branch_ids.size(); ++i) {
            if (split.branch_ids[i])
                r[i].phi_in_ids.insert(*split.branch_ids[i]);
        }
    }
    for (const auto &merge : par.merges) {
        for (size_t i = 0; i < merge.branch_ids.size(); ++i) {
            if (merge.branch_ids[i])
                r[i].out_ids.insert(*merge.branch_ids[i]);
        }
    }
    return r;
}
std::vector<PackedBranch>
get_branches_with_packed_ids(const Block::Variant_Select &select) {
    std::vector<PackedBranch> r;
    for (const auto &b : select.branches)
        r.push_back({b.seq, {}, {}});
    for (const auto &split : select.splits) {
        for (size_t i = 0; i < split.branch_ids.size(); ++i) {
            if (split.branch_ids[i])
                r[i].phi_in_ids.insert(*split.branch_ids[i]);
        }
    }
    for (const auto &merge : select.merges) {
        for (size_t i = 0; i < merge.branch_ids.size(); ++i) {
            r[i].out_ids.insert(merge.branch_ids[i]);
        }
    }
    return r;
}
PackedBranch get_branch_with_packed_ids(const Block::Variant_DoLoop &doloop) {
    PackedBranch r = {doloop.branch, {}, {}};
    for (const auto &phi : doloop.in_phis)
        r.phi_in_ids.insert(phi.bodyin_id);
    for (const auto &phi : doloop.out_phis)
        r.out_ids.insert(phi.bodyout_id);
    for (const auto &phi : doloop.loop_phis) {
        r.phi_in_ids.insert(phi.bodyin_id);
        r.out_ids.insert(phi.bodyout_id);
    }
    for (auto id : getIdsUsedByExpr(doloop.guard))
        r.out_ids.insert(id);
    return r;
}

struct Renaming {
    VarId assigned_id, read_id;
};

// this renames all the variables inside of seq, and returns a VarIdRemapPair
// describing all the inputs and outputs to the block
void build_register_graph(RegisterAllocationGraph &rag, const Sequence &seq,
                          const std::set<VarId> &parent_live_ids,
                          const std::set<VarId> &phi_in_ids,
                          const std::set<VarId> &use_out_ids,
                          const std::vector<Renaming> &final_renamings,
                          const DefUsesTable &table) {

    // TODO note this map IS NOT RECURSIVE, so maybe we can do this faster
    // somehow (e.g. just store it in a vector) Any variable which is never used
    // is stored as a "final" use on the block it is created in. If a variable
    // in the "phi-in" list is not used, it is put as a "final use" of the
    // startseq block. If you are running this on e.g. a do loop sequence,
    // "use-out" should include every bodyout_id and every id used by the guard,
    // and "phi-in" list should include all of bodyin_id
    std::unordered_map<const Block *, std::set<VarId>> final_use_map;
    {
        auto read_ids = std::set<VarId>{use_out_ids.begin(), use_out_ids.end()};

        for (Block *curr = seq.endseq->parent(); curr != seq.startseq;
             curr = curr->parent()) {
            const auto &def_uses = table.block_defuses.at(curr);
            std::set<VarId> final_uses;
            // for each id, if we have not read it after this, this is its final
            // use. Moreover, if we are creating an id, but have not read it
            // anywhere after, then set the block that creates it to be its own
            // final use
            for (auto id : def_uses.uses) {
                if (!read_ids.contains(id)) {
                    final_uses.insert(id);
                    read_ids.insert(id);
                }
            }
            for (auto id : def_uses.defs) {
                if (!read_ids.contains(id))
                    final_uses.insert(id);
            }
            final_use_map[curr] = std::move(final_uses);
        }
        // finally, mark each id of phi_in_ids that has not been read as having
        // its final use in the startseq block
        {
            std::set<VarId> final_uses;
            // for each id, if we have not read it after this, this is its final
            // use. Moreover, if we are creating an id, but have not read it
            // anywhere after, then set the block that creates it to be its own
            // final use
            for (auto id : phi_in_ids) {
                if (!read_ids.contains(id))
                    final_uses.insert(id);
            }
            final_use_map[seq.startseq] = std::move(final_uses);
        }
    }

    // now scan forward, and generate all the conflicts!
    auto live_ids = std::set<VarId>{phi_in_ids.begin(), phi_in_ids.end()};
    for (auto id : final_use_map.at(seq.startseq)) {
        hassert(live_ids.contains(id));
        live_ids.erase(id);
    }

    auto make_clique_filter = [](VarId id,
                                 const auto opt_ids) -> std::set<VarId> {
        std::set<VarId> clique = {id};
        for (auto opt_id : opt_ids) {
            if (opt_id)
                clique.insert(*opt_id);
        }
        return clique;
    };
    auto make_clique = [](VarId id, const auto opt_ids) -> std::set<VarId> {
        std::set<VarId> clique = {id};
        for (auto opt_id : opt_ids) {
            clique.insert(opt_id);
        }
        return clique;
    };
    auto make_union = [](const std::set<VarId> &A,
                         const std::set<VarId> &B) -> std::set<VarId> {
        auto C = A;
        for (auto x : B)
            C.insert(x);
        return C;
    };

    auto add_defs_no_fusion = [&](const std::set<VarId> &defs) {
        rag.add_set_interferes_set(defs, parent_live_ids);
        rag.add_set_interferes_set(defs, live_ids);
        rag.add_set_interferes_clique(defs);
    };
    auto add_defs_allowing_fusions =
        [&](const std::set<VarId> &defs,
            const std::set<std::pair<VarId, VarId>> &fusions) {
            int REWARD = 1;
            rag.add_set_interferes_set(defs, parent_live_ids, fusions, REWARD);
            rag.add_set_interferes_set(defs, live_ids, fusions, REWARD);
            rag.add_set_interferes_clique(defs, fusions, REWARD);
        };

    auto add_defs_pure_rename_Ov = [&](const std::vector<OptionalVarId> &defs,
                                       const VarId &orig_id) {
        rag.add_fuse_clique_required(make_clique_filter(orig_id, defs));
    };
    auto add_defs_pure_rename_vV = [&](const VarId &def,
                                       const std::vector<VarId> &orig_ids) {
        rag.add_fuse_clique_required(make_clique(def, orig_ids));
    };
    auto add_defs_pure_rename_vO =
        [&](const VarId &defs, const std::vector<OptionalVarId> &orig_ids) {
            rag.add_fuse_clique_required(make_clique_filter(defs, orig_ids));
        };

    auto update_live_varids = [&](const Block *curr) {
        const auto &def_uses = table.block_defuses.at(curr);
        for (auto id : def_uses.defs) {
            hassert(!live_ids.contains(id));
            live_ids.insert(id);
        }
        for (auto id : final_use_map.at(curr)) {
            hassert(live_ids.contains(id));
            live_ids.erase(id);
        }
    };

    // every id created within this scope interferes with every parent_live_id
    for (Block *curr = seq.startseq->child(); curr != seq.endseq;
         curr = curr->child()) {
        //        add_defs(def_uses.defs);

        switch (curr->type()) {
        case BlockType::Basic:
            switch (curr->u_basic().stmt.type()) {
            case StatementType::Assign: {
                const auto &assign = curr->u_basic().stmt.u_assign();
                std::set<VarId> defs{assign.ids.begin(), assign.ids.end()};
                std::set<std::pair<VarId, VarId>> fusions = {}; // TODO
                for (size_t i = 0; i < assign.ids.size(); ++i) {
                    if (assign.e.roots[i]->type() == IRExprTypeKind::Var)
                        fusions.insert(
                            {assign.ids[i], assign.e.roots[i]->u_var().id});
                }

                add_defs_allowing_fusions(defs, fusions);
                break;
            }
            case StatementType::Send:
                break;
            case StatementType::Receive:
                if (curr->u_basic().stmt.u_receive().var)
                    add_defs_no_fusion({*curr->u_basic().stmt.u_receive().var});
                break;
            }
            update_live_varids(curr);
            break; // TODO support copy elision
        case BlockType::Par: {
            // add forced fusion on splits
            for (const auto &split : curr->u_par().splits)
                add_defs_pure_rename_Ov(split.branch_ids, split.pre_id);

            // Add conflicts within each branch.
            const auto &branches = get_branches_with_packed_ids(curr->u_par());
            for (const auto &branch : branches)
                build_register_graph(
                    rag, branch.seq, make_union(parent_live_ids, live_ids),
                    branch.phi_in_ids, branch.out_ids, {}, table);

            // Add in conflicts between branches based on parallelism
            std::vector<DefUses> def_uses_branches;
            for (const auto &path : curr->u_par().branches)
                def_uses_branches.push_back(
                    table.seq_rec_defuses.at(path.startseq));

            for (size_t i = 0; i < def_uses_branches.size(); ++i) {
                for (size_t j = i + 1; j < def_uses_branches.size(); ++j) {
                    rag.add_set_interferes_set(def_uses_branches[i].defs,
                                               def_uses_branches[j].defs);
                    rag.add_set_interferes_set(def_uses_branches[i].defs,
                                               def_uses_branches[j].uses);
                    rag.add_set_interferes_set(def_uses_branches[i].uses,
                                               def_uses_branches[j].defs);
                }
            }

            // add forced fusion on merges
            for (const auto &merges : curr->u_par().merges)
                add_defs_pure_rename_vO(merges.post_id, merges.branch_ids);

            update_live_varids(curr);
            break;
        }
        case BlockType::Select: {
            // add forced fusion on splits
            for (const auto &split : curr->u_select().splits)
                add_defs_pure_rename_Ov(split.branch_ids, split.pre_id);

            // TODO should variable read in a guard still always be alive within
            // the branches? Do the first half of the liveness update
            const auto &def_uses = table.block_defuses.at(curr);
            for (auto id : final_use_map.at(curr)) {
                if (!def_uses.defs.contains(id)) {
                    hassert(live_ids.contains(id));
                    live_ids.erase(id);
                }
            }

            // Add conflicts within each branch
            const auto &branches =
                get_branches_with_packed_ids(curr->u_select());
            for (size_t i = 0; i < curr->u_select().branches.size(); ++i) {
                const auto &branch = branches[i];

                // build the final "psudo-assign" block
                std::vector<Renaming> branch_renames;
                for (const auto &merge : curr->u_select().merges)
                    branch_renames.push_back(
                        {merge.post_id, merge.branch_ids[i]});

                build_register_graph(
                    rag, branch.seq, make_union(parent_live_ids, live_ids),
                    branch.phi_in_ids, branch.out_ids, branch_renames, table);
            }
            // TODO Is this ok? I moved the first half to right after the
            // guards? Do the second half of the liveness update
            for (auto id : def_uses.defs) {
                if (!final_use_map.at(curr).contains(id)) {
                    hassert(!live_ids.contains(id));
                    live_ids.insert(id);
                }
            }

            break;
        }
        case BlockType::DoLoop: {
            // add forced fusion on in_phis
            std::set<VarId> loop_live_ids_from_in_phis;
            for (const auto &phi : curr->u_doloop().in_phis) {
                add_defs_pure_rename_vV(phi.bodyin_id, {phi.pre_id});
                loop_live_ids_from_in_phis.insert(phi.bodyin_id);
            }

            // build the psudo-assign block for before the loop
            {
                std::set<VarId> bodyin_ids;
                std::set<std::pair<VarId, VarId>> fusions; // TODO allow fusion
                for (const auto &phi : curr->u_doloop().loop_phis) {
                    bodyin_ids.insert(phi.bodyin_id);
                    fusions.insert({phi.bodyin_id, phi.pre_id});
                }

                add_defs_allowing_fusions(bodyin_ids, fusions);
            }

            // Do the first half of the liveness update
            const auto &def_uses = table.block_defuses.at(curr);
            for (auto id : final_use_map.at(curr)) {
                if (!def_uses.defs.contains(id)) {
                    hassert(live_ids.contains(id));
                    live_ids.erase(id);
                }
            }

            // Add conflicts within each branch
            const auto &branch = get_branch_with_packed_ids(curr->u_doloop());
            {
                // build the end-of-loop "psudo-assign" block
                std::vector<Renaming> branch_renames;
                for (const auto &phi : curr->u_doloop().loop_phis)
                    branch_renames.push_back({phi.bodyin_id, phi.bodyout_id});

                build_register_graph(
                    rag, branch.seq,
                    make_union(make_union(parent_live_ids, live_ids),
                               loop_live_ids_from_in_phis),
                    branch.phi_in_ids, branch.out_ids, branch_renames, table);
            }

            // add forced fusion on out_phis and loop_phis outputs
            for (const auto &phi : curr->u_doloop().out_phis)
                add_defs_pure_rename_vV(phi.post_id, {phi.bodyout_id});
            for (const auto &phi : curr->u_doloop().loop_phis) {
                if (phi.post_id)
                    add_defs_pure_rename_vV(*phi.post_id, {phi.bodyout_id});
            }

            // Do the second half of the liveness update
            for (auto id : def_uses.defs) {
                if (!final_use_map.at(curr).contains(id)) {
                    hassert(!live_ids.contains(id));
                    live_ids.insert(id);
                }
            }
            break;
        }
        case BlockType::StartSequence:
        case BlockType::EndSequence:
            hassert(false);
            break;
        }
    }

    // finally, apply the psudo-assignment block implied by the final_renaming
    // parameter
    {
        std::set<VarId> defs;
        std::set<std::pair<VarId, VarId>> fusions;
        for (const auto &[assigned_id, read_id] : final_renamings) {
            defs.insert(assigned_id);
            fusions.insert({assigned_id, read_id});
        }
        add_defs_allowing_fusions(defs, fusions);
    }

    hassert(final_renamings.size() < 1000);
}

void RegisterAllocationGraph::add_set_interferes_set(const std::set<VarId> &A,
                                                     const std::set<VarId> &B) {
    for (auto a : A)
        nodes.insert(a);
    for (auto b : B)
        nodes.insert(b);
    for (auto a : A) {
        for (auto b : B) {
            insert_conflict(a, b);
        }
    }
}
void RegisterAllocationGraph::add_set_interferes_set(
    const std::set<VarId> &A, const std::set<VarId> &B,
    const std::set<std::pair<VarId, VarId>> &fusions, int reward) {
    for (auto p : fusions)
        add_reward(p.first, p.second, reward);
    for (auto a : A)
        nodes.insert(a);
    for (auto b : B)
        nodes.insert(b);
    for (auto a : A) {
        for (auto b : B) {
            if (fusions.contains({a, b}) || fusions.contains({b, a}))
                continue;
            insert_conflict(a, b);
        }
    }
}
void RegisterAllocationGraph::add_set_interferes_clique(
    const std::set<VarId> &A) {
    for (auto a : A)
        nodes.insert(a);
    for (auto a : A) {
        for (auto b : A) {
            if (a != b)
                insert_conflict(a, b);
        }
    }
}
void RegisterAllocationGraph::add_set_interferes_clique(
    const std::set<VarId> &A, const std::set<std::pair<VarId, VarId>> &fusions,
    int reward) {
    for (auto p : fusions)
        add_reward(p.first, p.second, reward);
    for (auto a : A)
        nodes.insert(a);
    for (auto a : A) {
        for (auto b : A) {
            if (a == b)
                continue;
            if (fusions.contains({a, b}) || fusions.contains({b, a}))
                continue;
            insert_conflict(a, b);
        }
    }
}
void RegisterAllocationGraph::add_fuse_clique_required(
    const std::set<VarId> &A) {
    for (auto a : A)
        nodes.insert(a);
    for (auto a : A) {
        for (auto b : A) {
            if (a != b)
                insert_must_merge(a, b);
        }
    }
}

template <typename K> class Indexer {
    std::unordered_map<K, int> mp;
    int i = 0;

  public:
    int get(const K &k) {
        auto it = mp.find(k);
        if (it != mp.end())
            return it->second;
        int v = i++;
        mp[k] = v;
        return v;
    }
};

/*static*/ ColoredRegisterAllocationGraph
ColoredRegisterAllocationGraph::solve(RegisterAllocationGraph &&g_,
                                      const IdPool &id_pool) {
    ColoredRegisterAllocationGraph r;
    r.m_g = std::move(g_);

    // This should either return a valid coloring, or raise an exception if it
    // is not possible to color the graph. The algorithm used here should be
    // improved

    // We do 2 things. This is _not_ efficient. First, we fuse every node which
    // must be fused. If we introduce any loops (an edge from a node to itself),
    // the graph is not colorable. Second, we greedily fuse 1 edge at a time
    // that may be fused until there are none left. Finally, greedily color the
    // nodes until all have a color.

    // TODO find good ordering heuristics
    // TODO add good unit tests for this!
    using ClusterId = int;
    std::unordered_map<VarId, int> widths;
    std::unordered_map<VarId, ClusterId> mapping;

    ClusterId i = 0;
    for (auto v : r.m_g.nodes) {
        mapping[v] = i++;
        widths[v] = id_pool.getBitwidth(v);
    }

    // first do all the merges we have to do
    for (auto [v1, v2] :
         Algo::as_sorted_vector<std::pair<VarId, VarId>>(r.m_g.must_merge)) {
        ClusterId new_id = mapping.at(v1), old_id = mapping.at(v2);
        for (auto &[v, id] : mapping) {
            if (id == old_id)
                id = new_id;
        }
    }

    // Then check that it is still colorable
    for (const auto &[a, b] : r.m_g.conflicts) {
        if (mapping.at(a) == mapping.at(b)) {
            struct NotColorableException : std::exception {};
            throw NotColorableException{};
        }
    }

    // then go one edge at a time and check if we can merge it. The thing is we
    // can merge _any_ pair without an edge. For now just ignore the fact some
    // get rewards
    // TODO this is super inefficient

    std::vector<std::tuple<VarId, VarId, int>> pairs;
    for (auto v1 : r.m_g.nodes) {
        for (auto v2 : r.m_g.nodes) {
            pairs.emplace_back(v1, v2, r.m_g.get_reward(v1, v2) + 1);
        }
    }
    std::sort(pairs.begin(), pairs.end(), [](const auto &a, const auto &b) {
        const auto &[a0, a1, a2] = a;
        const auto &[b0, b1, b2] = b;
        return std::tuple{a2, a0, a1} < std::tuple{b2, b0, b1};
    });
    for (auto [v1, v2, _] : pairs) {
        auto c1 = mapping.at(v1), c2 = mapping.at(v2);
        // try to merge v1 and v2
        if (c1 == c2)
            continue; // already merged

        // Heuristic #1: dont merge things of different width. This is _not_
        // required for correctness
        if (widths.at(v1) != widths.at(v2))
            continue;

        // ensure that we dont create a conflict with this merge
        if (Algo::any_of(r.m_g.conflicts, [&](const auto &p) {
                const auto &[n1, n2] = p;
                auto nc1 = mapping.at(n1), nc2 = mapping.at(n2);
                return (c1 == nc1 && c2 == nc2) || (c1 == nc2 && c2 == nc1);
            })) {
            continue; // there is a conflict
        }

        // otherwise, merge the two nodes!
        ClusterId new_id = c1, old_id = c2;
        for (auto &[v, id] : mapping) {
            if (id == old_id)
                id = new_id;
        }

        // Then validate, just to be sure
        for (const auto &[a, b] : r.m_g.conflicts) {
            hassert(mapping.at(a) != mapping.at(b));
        }
    }

    // finally, set the colors
    Indexer<ClusterId> final_coloring;
    for (auto v : Algo::get_sorted_keys(mapping))
        r.m_coloring[v] = ColorId{final_coloring.get(mapping.at(v))};

    // one final double check
    for (const auto &[a, b] : r.m_g.conflicts)
        hassert(r.m_coloring.at(a) != r.m_coloring.at(b));
    for (const auto &[a, b] : r.m_g.must_merge)
        hassert(r.m_coloring.at(a) == r.m_coloring.at(b));

    return r;
}

RegisterAllocationGraph build_register_graph(ChpGraph &g) {
    RegisterAllocationGraph rag;
    DefUsesTable table = build_def_uses_table(g);
    build_register_graph(rag, g.m_seq, {}, {}, {}, {}, table);
    return rag;
}

// this renames all the variables inside of seq, and returns a VarIdRemapPair
// describing all the inputs and outputs to the block
void apply_crag(
    Sequence &seq, BlockAllocator &blockAllocator,
    const ColoredRegisterAllocationGraph &crag,
    const std::unordered_map<ColoredRegisterAllocationGraph::ColorId, VarId>
        &new_var_ids,
    IdPool &id_pool) {
    auto remap_id = [&](VarId &id) {
        VarId new_id = new_var_ids.at(crag.coloring().at(id));
        hassert(id_pool.getBitwidth(new_id) == id_pool.getBitwidth(id));
        id = new_id;
    };

    for (Block *curr = seq.startseq->child(); curr != seq.endseq;
         curr = curr->child()) {
        switch (curr->type()) {
        case BlockType::StartSequence:
        case BlockType::EndSequence:
            hassert(false);
            break;
        case BlockType::Basic:
            switch (curr->u_basic().stmt.type()) {
            case StatementType::Assign: {
                auto &assign = curr->u_basic().stmt.u_assign();

                ChpExprDag::mapNodes(assign.e, [&](ChpExprDag::Node &n) {
                    if (n.type() == IRExprTypeKind::Var)
                        remap_id(n.u_var().id);
                });

                for (auto &id : assign.ids)
                    remap_id(id);

                // validate that the coloring doesnt produce any self-assigns
                Algo::remove_filter_2_if(
                    assign.ids, assign.e.roots,
                    [](const VarId &id, const ChpExprDag::Node *root) {
                        return root->type() == IRExprTypeKind::Var &&
                               root->u_var().id == id;
                    });

                hassert(assign.ids.size() == assign.e.roots.size());
                if (assign.ids.empty()) {
                    curr = curr->parent();
                    ChpGraph::spliceOutBasicBlock(curr->child(), MarkDead::yes);
                    continue;
                }

                hassert(!assign.ids.empty());

                auto read_ids = getIdsUsedByExpr(assign.e);
                for (const auto &id : assign.ids) {
                    hassert(!read_ids.contains(id));
                }

                break;
            }
            case StatementType::Send: {
                auto &send = curr->u_basic().stmt.u_send();

                ChpExprDag::mapNodes(send.e.m_dag, [&](ChpExprDag::Node &n) {
                    if (n.type() == IRExprTypeKind::Var)
                        remap_id(n.u_var().id);
                });

                break;
            }
            case StatementType::Receive: {
                auto &receive = curr->u_basic().stmt.u_receive();
                if (receive.var) {
                    VarId id = *receive.var;
                    remap_id(id);
                    receive.var = id;
                }

                break;
            }
            }
            break;
        case BlockType::Par: {
            // check that all the phi functions (both splits and merges) are all
            // the same color. If not, that means the code building the register
            // allocation graph has changed, so this would need to change as
            // well.
            for (const auto &split : curr->u_par().splits) {
                for (const auto &id : split.branch_ids) {
                    if (id)
                        hassert(crag.coloring().at(split.pre_id) ==
                                crag.coloring().at(*id));
                }
            }
            for (const auto &merge : curr->u_par().merges) {
                for (const auto &id : merge.branch_ids) {
                    if (id)
                        hassert(crag.coloring().at(merge.post_id) ==
                                crag.coloring().at(*id));
                }
            }

            // remap all the child branches
            for (auto &branch : curr->u_par().branches)
                apply_crag(branch, blockAllocator, crag, new_var_ids, id_pool);

            // finally, delete the phi functions
            curr->u_par().splits.clear();
            curr->u_par().merges.clear();
            break;
        }
        case BlockType::Select: {
            // check that all the splits functions are all the same color. If
            // not, that means the code building the register allocation graph
            // has changed, so this would need to change as well.
            for (const auto &split : curr->u_select().splits) {
                for (const auto &id : split.branch_ids) {
                    if (id)
                        hassert(crag.coloring().at(split.pre_id) ==
                                crag.coloring().at(*id));
                }
            }

            // remap all the child branches
            int branch_idx = 0;
            for (auto &branch : curr->u_select().branches) {
                if (branch.g.type() == IRGuardType::Expression) {
                    ChpExprDag::mapNodes(
                        branch.g.u_e().e.m_dag, [&](ChpExprDag::Node &n) {
                            if (n.type() == IRExprTypeKind::Var)
                                remap_id(n.u_var().id);
                        });
                }

                apply_crag(branch.seq, blockAllocator, crag, new_var_ids,
                           id_pool);

                // all the nodes in a merge need not be the same color. Insert
                // assignment statements as needed.
                // TODO place this assignment inside an existing assign block if
                // possible
                std::vector<std::pair<VarId, VarId>> assign_from_var_pairs;
                for (const auto &merge : curr->u_select().merges) {
                    const auto &branch_id = merge.branch_ids[branch_idx];
                    if (crag.coloring().at(merge.post_id) !=
                        crag.coloring().at(branch_id)) {
                        assign_from_var_pairs.emplace_back(
                            new_var_ids.at(crag.coloring().at(merge.post_id)),
                            new_var_ids.at(crag.coloring().at(branch_id)));
                    }
                }
                if (!assign_from_var_pairs.empty()) {
                    // TODO fuse this into existing assignment block
                    std::vector<VarId> assigned_ids;
                    ChpExprDag dag;
                    for (auto [assigned_id, read_id] : assign_from_var_pairs) {
                        dag.roots.push_back(dag.addSubdag(
                            ChpExprSingleRootDag::makeVariableAccess(read_id,
                                                                     id_pool)));
                        assigned_ids.push_back(assigned_id);
                    }
                    ChpGraph::spliceInSequenceBefore(
                        branch.seq.endseq,
                        {blockAllocator.newBlock(
                            Block::makeBasicBlock(Statement::makeAssignment(
                                assigned_ids, std::move(dag))))});
                }

                branch_idx++;
            }

            // finally, delete the phi functions
            curr->u_select().splits.clear();
            curr->u_select().merges.clear();
            break;
        }
        case BlockType::DoLoop: {
            // check that all the phi_in and phi_out functions are all the same
            // color. If not, that means the code building the register
            // allocation graph has changed, so this would need to change as
            // well.
            for (const auto &phi : curr->u_doloop().in_phis)
                hassert(crag.coloring().at(phi.pre_id) ==
                        crag.coloring().at(phi.bodyin_id));
            for (const auto &phi : curr->u_doloop().out_phis)
                hassert(crag.coloring().at(phi.post_id) ==
                        crag.coloring().at(phi.bodyout_id));
            for (const auto &phi : curr->u_doloop().loop_phis) {
                if (phi.post_id)
                    hassert(crag.coloring().at(*phi.post_id) ==
                            crag.coloring().at(phi.bodyout_id));
            }

            // remap all the child branches
            ChpExprDag::mapNodes(curr->u_doloop().guard.m_dag,
                                 [&](ChpExprDag::Node &n) {
                                     if (n.type() == IRExprTypeKind::Var)
                                         remap_id(n.u_var().id);
                                 });

            apply_crag(curr->u_doloop().branch, blockAllocator, crag,
                       new_var_ids, id_pool);

            // The bodyin_id and bodyout_id the nodes in a loop_phi need not be
            // the same color. Insert assignment statements as needed.
            // TODO place this assignment inside an existing assign block if
            // possible
            std::vector<std::pair<VarId, VarId>> assign_from_var_setups,
                assign_from_var_loop_ends;
            for (const auto &phi : curr->u_doloop().loop_phis) {
                if (crag.coloring().at(phi.pre_id) !=
                    crag.coloring().at(phi.bodyin_id)) {

                    assign_from_var_setups.emplace_back(
                        new_var_ids.at(crag.coloring().at(phi.bodyin_id)),
                        new_var_ids.at(crag.coloring().at(phi.pre_id)));
                }

                if (crag.coloring().at(phi.bodyin_id) !=
                    crag.coloring().at(phi.bodyout_id)) {
                    assign_from_var_loop_ends.emplace_back(
                        new_var_ids.at(crag.coloring().at(phi.bodyin_id)),
                        new_var_ids.at(crag.coloring().at(phi.bodyout_id)));
                }
            }
            if (!assign_from_var_setups.empty()) {
                // TODO fuse this into existing assignment block
                std::vector<VarId> assigned_ids;
                ChpExprDag dag;
                for (auto [assigned_id, read_id] : assign_from_var_setups) {
                    dag.roots.push_back(
                        dag.addSubdag(ChpExprSingleRootDag::makeVariableAccess(
                            read_id, id_pool)));
                    assigned_ids.push_back(assigned_id);
                }
                ChpGraph::spliceInSequenceBefore(
                    curr, {blockAllocator.newBlock(
                              Block::makeBasicBlock(Statement::makeAssignment(
                                  assigned_ids, std::move(dag))))});
            }
            if (!assign_from_var_loop_ends.empty()) {
                // TODO fuse this into existing assignment block
                std::vector<VarId> assigned_ids;
                ChpExprDag dag;
                for (auto [assigned_id, read_id] : assign_from_var_loop_ends) {
                    dag.roots.push_back(
                        dag.addSubdag(ChpExprSingleRootDag::makeVariableAccess(
                            read_id, id_pool)));
                    assigned_ids.push_back(assigned_id);
                }
                ChpGraph::spliceInSequenceBefore(
                    curr->u_doloop().branch.endseq,
                    {blockAllocator.newBlock(
                        Block::makeBasicBlock(Statement::makeAssignment(
                            assigned_ids, std::move(dag))))});
            }

            // finally, delete the phi functions
            curr->u_doloop().in_phis.clear();
            curr->u_doloop().out_phis.clear();
            curr->u_doloop().loop_phis.clear();
            break;
        }
        }
    }
}

void apply_crag(ChpGraph &g, const ColoredRegisterAllocationGraph &crag) {
    std::unordered_map<ColoredRegisterAllocationGraph::ColorId, VarId>
        new_var_ids;
    {
        std::unordered_map<ColoredRegisterAllocationGraph::ColorId, int>
            new_var_id_widths;
        for (auto [v, c] : crag.coloring())
            new_var_id_widths[c] =
                std::max(new_var_id_widths[c], g.id_pool().getBitwidth(v));
        for (auto c : Algo::get_sorted_keys(new_var_id_widths))
            new_var_ids[c] = g.id_pool().makeUniqueVar(new_var_id_widths[c]);
    }

    apply_crag(g.m_seq, g.blockAllocator(), crag, new_var_ids, g.id_pool());
}

// TODO maybe I should just directly synthesis the static token form?
void applyRegisterGraph(ChpGraph &g,
                        const ColoredRegisterAllocationGraph &crag) {
    hassert(g.is_static_token_form);
    apply_crag(g, crag);
    g.validateGraphInvariants();
    g.is_static_token_form = false;
}

} // namespace

void takeOutOfStaticTokenForm(ChpGraph &graph) {
    auto rag = build_register_graph(graph);
    auto crag =
        ColoredRegisterAllocationGraph::solve(std::move(rag), graph.id_pool());
    applyRegisterGraph(graph, crag);
}

} // namespace ChpOptimize
