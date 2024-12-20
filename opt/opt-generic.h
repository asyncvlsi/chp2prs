#pragma once
/*************************************************************************
 *
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
#include "chp-graph.h"

namespace ChpOptimize {

  /*
   *
   * L is a lattice. It must have the following static member
   * functions:
   *
   *   L bot()  : empty set of values  (i.e. no information)
   *   L top()  : all possible values
   *
   *   L merge(const L& a, const L& b)
   *   bool equal(const L&a, const L& b)
   *
   *   void eval (std::unordered_map<ChpExprDag::Node *, L> &nmap,
   *              std::unordered_map<VarId, L> &vmap,
   *              const ChpExprDag &dag)
   *
   */
  template<class L>
  class Analysis {

    Analysis(const char *name, FlowDirection f);

    void init(const IdPool &idpool);
    bool run(std::vector<Block *> &blks);

    /* access fields */
    std::unordered_map<VarId, L> &getMap()  { return _vals; }
    const char *getName() { return _nm; }
    FlowDirection getDirection() { return _f; }

  private:
    // a map from variables to lattice elements
    std::unordered_map<VarId, L> _vals;
    char *_nm;
    FlowDirection _f;
  };


  class MultiAnalysis {
  public:
    MultiAnalysis (ChpGraph &c);
    ~MultiAnalysis ();

    template<class L>
    bool runAnalysis (Analysis<L> *a, bool first);
    
  private:
    std::vector<Block *> _liveblocks_fwd, _liveblocks_rev;
    ChpGraph &_prog;
  };
    

} // namespace ChpOptimize
