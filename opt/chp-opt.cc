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
#include <chrono>

#include "chp-cost-model.h"
#include "chp-opt.h"
#include "chp-print.h"
#include "sequencers.h"
#include "static-tokens.h"

namespace ChpOptimize {

void optimize_chp_O2(ChpGraph &g, const std::string &proc_name, bool verbose) {

    auto start_t = std::chrono::steady_clock::now();
    [[maybe_unused]] auto tm = [&]() {
        return (
            long long)(std::chrono::duration_cast<std::chrono::milliseconds>(
                           std::chrono::steady_clock::now() - start_t)
                           .count());
    };
    [[maybe_unused]] auto logWithTime = [&](const std::string &name, auto fn) {
        if (verbose)
            fprintf(stderr, "%s %lld\n", name.c_str(), tm());
        return fn(g);
    };
    [[maybe_unused]] auto logWithTimeB = [&](const std::string &name, auto fn) {
        if (verbose)
            fprintf(stderr, "%s %lld\n", name.c_str(), tm());

        bool changed = fn(g);
        if (verbose)
            fprintf(stderr, "done %s (changed = %d) %lld\n", name.c_str(),
                    (int)changed, tm());

        return changed;
    };
    [[maybe_unused]] auto logq = [&](const char *s) {
        if (verbose)
            fprintf(stderr, "%s", s);
    };

    if (verbose)
        fprintf(stderr,
                "\n\n===============  optimizing %s  ================\n",
                proc_name.c_str());

    logWithTime("lifting guards", liftGuards);
    logWithTime("putIntoStaticTokenForm", putIntoStaticTokenForm);

    // Keep running elimination and propagation until reaching a fixed point
    bool changed;
    do {
        logq("\n\n");
        changed = false;

        changed |= logWithTimeB("propagateConstants", propagateConstants);
        changed |= logWithTimeB("eliminateDeadCode", eliminateDeadCode);
        changed |= logWithTimeB("transposing assignments with chan operations",
                                transposeAssignmentAndChanOps);
        changed |= logWithTimeB("fusing assignments", fuseAssignments);
        changed |= logWithTimeB("eliminateCopies", eliminateCopies);

        // We do this every loop in case we manage to fuse any loop guard
        // expressions.
        // TODO make loop branch fusion lift the guards automatically. Then we
        // can remove this pass and just do it once at the beginning
        //        changed |= logWithTimeB("lifting guards", liftGuards);
    } while (changed);
    logq("nothing changed! exited loop\n\n\n");

    //    print_chp(std::cerr, g);

    logWithTime("takeOutOfStaticTokenForm", takeOutOfStaticTokenForm);

    // double check we didnt add in any self-assignment
    hassertNoSelfAssignments(g);

    // We should remove this at some point, but there is a bug in the Act
    // library (as of 2021)
    logWithTime("unpackLargeConstantHack", unpackLargeConstantHack);
}

void optimize_chp_O0(ChpGraph &g, const std::string &, bool) {
    removeSelfAssignments(g);
    removeElseOnlySelect(g);
}

} // namespace ChpOptimize
