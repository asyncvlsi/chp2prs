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
BINARY=chp2prs.$(EXT)

TARGETS=$(BINARY)
include config.mk

ifdef expropt_INCLUDE
OBJS=main.o sdt.o basicsdt.o externoptsdt.o
else
OBJS=main.o sdt.o basicsdt.o
endif

SRCS=$(OBJS:.o=.cc)

ifdef chp_opt_INCLUDE
CHPOPT=-lchpopt
else
CHPOPT=
endif

SUBDIRS=lib

include $(ACT_HOME)/scripts/Makefile.std

ifdef expropt_INCLUDE
EXPRLIB=-lexpropt
else
EXPRLIB=
endif

$(BINARY): $(LIB) $(OBJS) $(ACTDEPEND)
	$(CXX) $(CFLAGS) $(OBJS) -o $(BINARY) $(CHPOPT) $(LIBACTPASS) $(EXPRLIB)

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
