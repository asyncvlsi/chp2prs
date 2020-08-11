#-------------------------------------------------------------------------
#
#  Copyright (c) 2020 Rajit Manohar
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
TARGETINSTALL=syn.act qdichan.act qdidpath.act _all_.act syngates.act 

include $(VLSI_TOOLS_SRC)/scripts/Makefile.std

all:
	@echo "Done"

install: install_act $(TARGETINSTALL)
	@if [ ! -d $(ACT_HOME)/act/syn ]; then mkdir $(ACT_HOME)/act/syn; fi
	@for i in $(TARGETINSTALL); do $(VLSI_TOOLS_SRC)/scripts/install $$i $(ACT_HOME)/act/syn/$$i; done

#	@$(VLSI_TOOLS_SRC)/scripts/install globals.act $(ACT_HOME)/act/globals.act

-include Makefile.deps
