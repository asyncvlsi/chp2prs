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


#ifndef __ACT_RING_NAME_HANDLING_H__
#define __ACT_RING_NAME_HANDLING_H__


#include "reqs.h"

/*
    Revert the mangling settings back to 
    the main Act object's defaults.
*/
void revert_mangle ();

/*
 * Initialize the Act object that is used for 
 * string mangling functionality.
 * Needs to be called only once in the entire
 * program.
*/
void mangle_init ();

/*
 * Get the true name of a variable. This is 
 * a consistent way to get a unique string
 * name for every identifier. Indices in 
 * arrayed identifiers are mangled as required.
*/
void get_true_name (char *buf, ActId *id, Scope *s, bool mangle = true);

#endif