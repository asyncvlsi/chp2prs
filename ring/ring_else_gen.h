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

#include "reqs.h"

/* 
 * Recurses through the given chp tree.
 * For every selection that has an else guard,
 * replace the else guard with the correct explicit version.
 * For example,
 * 
 *  [G1->S1
 * []G2->S2
 * []G3->S3
 * []else->S4
 * ]
 * 
 * gets converted to:
 * 
 *  [G1->S1
 * []G2->S2
 * []G3->S3
 * []~(G1|G2|G3)->S4
 * ]
 * 
*/
void fill_in_else_explicit (act_chp_lang_t *c, Process *p, int root);

void expand_self_assignments (act_chp_lang_t *&c, Process *p);

void make_receives_unique (act_chp_lang_t *&c, Process *p);

bool _var_appears_in_expr (Expr *e, ActId *id);
