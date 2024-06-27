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
#include "synth.h"

/**
 * SDT engine generator
 */
ActSynthesize *gen_sdt_engine (const char *prefix,  char *infile,
			       char *outfile, char *exprfile);

/**
 * Ring synthesis engine generator
 */
ActSynthesize *gen_ring_engine (const char *prefix,  char *infile,
				char *outfile, char *exprfile);

/**
 * CHP decomposition engine generator
 */
ActSynthesize *gen_decomp_engine (const char *prefix,  char *infile,
				char *outfile, char *exprfile);

/**
 * Dataflow synthesis engine generator
 */
ActSynthesize *gen_df_engine (const char *prefix,  char *infile,
			      char *outfile, char *exprfile);
