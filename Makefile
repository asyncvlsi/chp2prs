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

OBJS=main.o check_chp.o cartographer.o

SRCS=$(OBJS:.o=.cc)

include $(VLSI_TOOLS_SRC)/scripts/Makefile.std

$(BINARY): $(LIB) $(OBJS) $(ACTDEPEND)
	$(CXX) $(CFLAGS) $(OBJS) -o $(BINARY) $(LIBACTPASS)
	
update: update_channel update_globals update_syn update_bundled

update_channel:
	@if [ -d lib/ -a -f lib/channel.act ] ; \
	then \
		(cp lib/channel.act $(ACT_PATH)/); \
		(echo "Copied channel.act to ACT_PATH") \
	else \
		(echo "Error: no channel file to update") \
	fi
	
update_globals:
	@if [ -d lib/ -a -f lib/globals.act ] ; \
	then \
		(cp lib/globals.act $(ACT_PATH)/); \
		(echo "Copied globals.act to ACT_PATH") \
	else \
		(echo "Error: no globals file to update") \
	fi
	
# SYN File Copy for Syn File
update_syn:
	@if [ -d lib/ -a -f lib/syn.act ] ; \
	then \
		(cp lib/syn.act $(ACT_PATH)/); \
		(echo "Copied syn.act to ACT_PATH") \
	else \
		(echo "Error: no syn file to update") \
	fi
	
# BUNDLED File Copy for Bundled File
update_bundled:
	@if [ -d lib/ -a -f lib/bundled.act ] ; \
	then \
		(cp lib/bundled.act $(ACT_PATH)/); \
		(echo "Copied bundled.act to ACT_PATH") \
	else \
		(echo "Error: no bundled file to update") \
	fi

# # SYN File Copy for Syn Namespace
#ifdef
#update_syn:
#	@if [ -d lib/ -a -f lib/syn.act ] ; \
#	then \
#		if [ -d $(ACT_PATH)/syn ]; \
#		then \
#			cp lib/syn.act $(ACT_PATH)/syn/_all_.act; \
#			echo "Copied syn.act to ACT_PATH/act/syn/_all_.act"; \
#		else \
#			(mkdir $(ACT_PATH)/syn; cp lib/syn.act $(ACT_PATH)/syn/_all_.act); \
#			(echo "Copied syn.act to ACT_PATH/act/syn/_all_.act") \
#		fi \
#	else \
#		(echo "Error: no syn file to update") \
#	fi
	
# # BUNDLED File Copy for namespace
#update_bundled:
#	@if [ -d lib/ -a -f lib/bundled.act ] ; \
#	then \
#		if [ -d $(ACT_PATH)/bundled ]; \
#		then \
#			cp lib/bundled.act $(ACT_PATH)/bundled/_all_.act; \
#			echo "Copied bundled.act to ACT_PATH/bundled/_all_.act"; \
#		else \
#			(mkdir $(ACT_PATH)/bundled; cp lib/bundled.act $(ACT_PATH)/bundled/_all_.act); \
#			(echo "Copied bundled.act to ACT_PATH/bundled/_all_.act") \
#		fi \
#	else \
#		(echo "Error: no bundled file to update") \
#	fi

include test/Makefile

-include Makefile.deps
