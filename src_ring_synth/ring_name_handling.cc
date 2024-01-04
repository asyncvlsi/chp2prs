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
#include "ring_synthesis_struct.h"

Act *a_mangle;

static const int style_global = 1;

void mangle_init ()
{ 
  char u[6];
  a_mangle = new Act;
  snprintf(u,6,"[],."); /// characters to mangle
  a_mangle->mangle(u);
}

void get_true_name (char *buf, ActId *id, Scope *s)
{
  char str[1024];
  id->sPrint(str,1024,NULL,style_global);
  a_mangle->mangle_string(str,buf,1024);
}

void generate_array_suffix(char *buf, Array *a)
{
  char s[1024];
  a->sPrint(s,1024,style_global);
  a_mangle->mangle_string(s,buf,1024);
}