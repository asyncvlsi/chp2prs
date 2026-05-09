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

#ifndef __PRETTY_PRINT_H__
#define __PRETTY_PRINT_H__

#include <act/act.h>
#include <act/chp/analysis.h>
#include <act/chp/utils.h>

// Prettier CHP printing method
void chp_pretty_print (FILE *, act_chp_lang_t *);

static void _chp_pretty_print (FILE *, act_chp_lang_t *, int, int);

// Replace else in selection with explicit form
void _fill_in_else_explicit (act_chp_lang_t *, Scope *);

/*
    Recursively replace:
        int(int(val,x),y) 
    with 
        int(val,x)
    IFF x==y
*/
void _trim_nested_same_int (act_chp_lang_t *&, Scope *);

void _trim_nested_same_int (Expr *&, Scope *);

void _lift_probes (GraphWithChanNames &);

void _lift_probes (GraphWithChanNames &, Sequence);

#endif