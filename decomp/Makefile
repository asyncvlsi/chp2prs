#-------------------------------------------------------------------------
#
#  Copyright (c) 2024 Karthi Srinivasan
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
TARGETLIBS=libactchpdecomp_$(EXT).so

CPPSTD=c++20

SHOBJS=analysis.os \
	breakpoint.os \
	chopping_block.os \
	multichan.os \
	pretty_print.os

SRCS=$(SHOBJS:.os=.cc)

OPTLIB=-lactchpopt

EXPRLIB=-lexpropt_sh $(ACT_HOME)/lib/libabc.so

include $(ACT_HOME)/scripts/Makefile.std

$(TARGETLIBS): $(SHOBJS)
	$(ACT_HOME)/scripts/linkso $(TARGETLIBS) $(SHOBJS) $(SHLIBACTPASS) $(EXPRLIB) $(OPTLIB)

-include Makefile.deps
