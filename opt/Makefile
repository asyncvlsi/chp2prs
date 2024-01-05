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
TARGETLIBS=libactchpopt_$(EXT).so

SHOBJS=algos.os big-int.os ir-id.os \
	chp-act-conversions.os \
	chp-graph.os chp-print.os \
	static-tokens.os \
	chp-cost-model.os \
	chp-opt.os \
	int-math.os \
	constants.os \
	copy-elimination.os \
	sequencers.os \
	deadcode.os \
	ir-to-dataflow.os

CPPSTD=c++20

SRCS=$(SHOBJS:.os=.cc)

include $(ACT_HOME)/scripts/Makefile.std

ifdef expropt_INCLUDE
ifdef abc_LIBDIR
EXPRLIB=-lexpropt_sh $(ACT_HOME)/lib/libabc.so
else
EXPRLIB=
endif
endif

$(TARGETLIBS): $(SHOBJS)
	$(ACT_HOME)/scripts/linkso $(TARGETLIBS) $(SHOBJS) $(SHLIBACTPASS) $(EXPRLIB)

-include Makefile.deps
