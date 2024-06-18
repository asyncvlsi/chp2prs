/*************************************************************************
 *
 *  Copyright (c) 2024 Karthi Srinivasan
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

#ifndef SYNTHESIS_STRUCT_H
#define SYNTHESIS_STRUCT_H

/*
    Information about each variable in a process w.r.t. its 
    use in the CHP body. Fields are quite self-explanatory.
*/
struct var_info {
  // ActId *id;
  const char *name;

  /*-- flags --*/
  unsigned int fcurexpr:1;	// found in current expression
  unsigned int fischan:1;	// channel or int?
  unsigned int fisinport:2;	// 1 if input, 0 if output, 2 if both
  unsigned int fisbool:1;	// bool variable or bool chan

  int width;			// bitwidth

  int block_in, block_out;	// for internal channels

  int latest_for_read; // correct id for reading from (used for getting data inside branches correctly)

  int nread, nwrite;		// for variables
                                //     nread  = total # of reads
                                //     nwrite = total # of writes
				// for channels
                                //     nread  = total # of receives
                                //     nwrite = total # of sends
  
  int iread, iwrite;		// running counter used for muxing ( aka choosing the latch :) )

  list_t *latest_latch_branches;

  int array_size;

};

/*
  Structure that is tagged to space pointers of chp_t's 
  that contains latch assignment info, merge_mux info
  and live_var info.
*/

enum class LatchType { Latch, Mux, ICs };

typedef struct latch_info {
  // type of the struct
  LatchType type;
  
  // ID for normal latches
  int latch_number; 

  // live vars at this point (in for actions, out for selections)
  // Length: (No. of live vars out of merge)
  list_t *live_vars;

  // Used at merge-points.
  // ID for merge muxes, one per var in live_vars.
  // -1 if mux is not needed at this merge-point for this var.
  // Dimensions: (No. of live vars out of merge)
  std::vector<int> merge_mux_latch_number; 

  // Input mapping for merge_muxes.
  // Each vector contains the latch IDs of the 
  // latch/mux that needs to be connected to 
  // the input of this mux.
  // One per merge mux that is needed.
  // -1 if mux not needed.
  // Dimensions: (No. of live vars out of merge)*(No. of branches in selection)
  std::vector<std::vector<int>> merge_mux_inputs;
  
} latch_info_t;

#endif