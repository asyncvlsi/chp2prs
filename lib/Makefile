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
TARGETACT=globals.act channel.act

include $(VLSI_TOOLS_SRC)/scripts/Makefile.std

-include Makefile.deps

install: install_act
	@if [ ! -d $(ACT_HOME)/act/syn ]; then mkdir $(ACT_HOME)/act/syn; fi
	@$(VLSI_TOOLS_SRC)/scripts/install syn.act $(ACT_HOME)/act/syn/_all_.act
	@if [ ! -d $(ACT_HOME)/act/bundled ]; then mkdir $(ACT_HOME)/act/bundled; fi
	@$(VLSI_TOOLS_SRC)/scripts/install bundled.act $(ACT_HOME)/act/bundled/_all_.act
