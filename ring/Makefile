#-------------------------------------------------------------------------
#
#  Copyright (c) 2024 Rajit Manohar
#
#  This program is free software; you can redistribute it and/or
#  modify it under the terms of the GNU General Public License
#  as published by the Free Software Foundation; either version 2
#  of the License, or (at your option) any later version.
#
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#
#  You should have received a copy of the GNU General Public License
#  along with this program; if not, write to the Free Software
#  Foundation, Inc., 51 Franklin Street, Fifth Floor,
#  Boston, MA  02110-1301, USA.
#
#-------------------------------------------------------------------------
TARGETLIBS=libactchpring_$(EXT).so

CPPSTD=c++20

SHOBJS=ring.os \
	ring_forge.os \
	ring_else_gen.os \
	ring_name_handling.os \
	ring_live_vars.os

SRCS=$(SHOBJS:.os=.cc)

EXPRLIB=-lexpropt_sh $(ACT_HOME)/lib/libabc.so

include $(ACT_HOME)/scripts/Makefile.std

$(TARGETLIBS): $(SHOBJS)
	$(ACT_HOME)/scripts/linkso $(TARGETLIBS) $(SHOBJS) $(SHLIBACTPASS) $(EXPRLIB)

-include Makefile.deps
