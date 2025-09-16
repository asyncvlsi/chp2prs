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

TARGETINCS=chp-opt.h static-tokens.h ir-id.h hassert.h chp-cost-model.h act-names.h \
		   chp-graph.h union-find.h utils.h ir-expr.h ir-expr-dag.h big-int.h \
		   string_format.h typed-variants.h chp-print.h ir-expr-act-conversion.h
TARGETINCSUBDIR=act/chp

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
	livevars.os \
	ir-to-dataflow.os \
	opt-generic.os \
	stmt-parallelize.os \
	defuse.os 

CPPSTD=c++17

SRCS=$(SHOBJS:.os=.cc)

include $(ACT_HOME)/scripts/Makefile.std

EXPRLIB=-lexpropt_sh $(ACT_HOME)/lib/libabc.so

$(TARGETLIBS): $(SHOBJS)
	$(ACT_HOME)/scripts/linkso $(TARGETLIBS) $(SHOBJS) $(SHLIBACTPASS) $(EXPRLIB)

-include Makefile.deps
