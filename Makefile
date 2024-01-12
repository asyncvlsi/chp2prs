#-------------------------------------------------------------------------
#
#  Copyright (c) 2018 Rajit Manohar
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
BINARY=chp2prs_dev.$(EXT) 

TARGETS=$(BINARY) synth2.$(EXT)
TARGETLIBS=libactchp2prspass_$(EXT).so

CPPSTD=c++20

include config.mk

OBJS1=main.o

OBJS2=main2.o sdt_engine.o df_engine.o ring_engine.o

OBJS=$(OBJS1) $(OBJS2)

SHOBJS=chp2prs_pass.os synth.os synth_pass.os

SRCS=$(OBJS:.o=.cc) $(SHOBJS:.os=.cc)

SUBDIRS=lib opt sdt ring

include $(ACT_HOME)/scripts/Makefile.std

EXPRLIB=-lexpropt_sh $(ACT_HOME)/lib/libabc.so

SYNTHLIB=-lactchpopt -lactchpsdt -lactchpring

ifdef exproptcommercial_INCLUDE
EXPRLIB+=-lexproptcommercial_sh
endif

$(BINARY): $(LIB) $(OBJS1) $(ACTDEPEND)
	$(CXX) $(SH_EXE_OPTIONS) $(CFLAGS) $(OBJS1) -o $(BINARY) $(SHLIBACTPASS)

synth2.$(EXT): $(LIB) $(OBJS2) $(ACTDEPEND) $(ACT_HOME)/lib/libactchpopt.so
	$(CXX) $(SH_EXE_OPTIONS) $(CFLAGS) $(OBJS2) -o synth2.$(EXT) $(SHLIBACTPASS) $(SYNTHLIB) -lactchp2prspass $(EXPRLIB)

$(TARGETLIBS): $(SHOBJS)
	$(ACT_HOME)/scripts/linkso $(TARGETLIBS) $(SHOBJS) $(SHLIBACTPASS) $(EXPRLIB) -lactchpsdt

testreps:
	@if [ -d test -a -x test/repeat_unit.sh ]; \
	then \
		if [ -d "test/unit_tests/${unit}" ]; \
		then \
			(cd test; ./repeat_unit.sh); \
		else \
			echo "Error: make testreps unit={unit_test} [warning={0/1}]"; \
		fi \
	fi

debug: obj_main obj_cartographer obj_checkchp start_lldb

obj_chpexpr2verilog:
	@if [ -d $(EXT) -a -f $(EXT)/externoptsdt.o ] ; \
	then \
		(mv $(EXT)/externoptsdt.o externoptsdt.o); \
	fi
	@if [ -d $(EXT) -a -f $(EXT)/syntesis_helper.o ] ; \
	then \
		(mv $(EXT)/syntesis_helper.o syntesis_helper.o); \
	fi

obj_cartographer:
	@if [ -d $(EXT) -a -f $(EXT)/cartographer.o ] ; \
	then \
		(mv $(EXT)/cartographer.o cartographer.o); \
	fi
obj_main:
	@if [ -d $(EXT) -a -f $(EXT)/main.o ] ; \
	then \
		(mv $(EXT)/main.o main.o); \
	fi
obj_checkchp:
	@if [ -d $(EXT) -a -f $(EXT)/check_chp.o ] ; \
	then \
		(mv $(EXT)/check_chp.o check_chp.o); \
	fi
start_lldb:
	@if [ -x ${BINARY} ] ; \
	then \
		(lldb ./$(BINARY)); \
	fi

-include Makefile.deps
