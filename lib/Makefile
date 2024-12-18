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
TARGETINSTALL=
TARGETINSTALL_QDI=qdi/_all_.act qdi/dpath.act qdi/chan.act qdi/math.act qdi/stdcells.act qdi/var.act
TARGETINSTALL_DI=diopt/_all_.act diopt/chan.act diopt/stdcells.act diopt/var.act
TARGETINSTALL_DITEST=ditest/_all_.act ditest/chan.act ditest/stdcells.act ditest/var.act ditest/stdcells_orgfixme.act
TARGETINSTALL_RING=ring/_all_.act ring/ctrl.act ring/ctrl_pulsed.act ring/ctrl_2phase.act ring/ctrl_di_dpath.act
TARGETINSTALL_BDOPT=bdopt/_all_.act bdopt/stdcells.act bdopt/dpath.act bdopt/chan.act
TARGETINSTALL_LIBERTY=osu018_stdcells.lib LICENSE.osu018_stdcells.lib

TARGETACT=syn.act _all_.act
TARGETACTSUBDIR=syn

include $(ACT_HOME)/scripts/Makefile.std

all:
	@echo "Done"

install: install_act $(TARGETINSTALL) $(TARGETINSTALL_QDI) $(TARGETINSTALL_RING) $(TARGETINSTALL_BDOPT) $(TARGETINSTALL_DI) $(TARGETINSTALL_DITEST)
	@if [ ! -d $(ACT_HOME)/act/syn ]; then mkdir $(ACT_HOME)/act/syn; fi
	@for i in $(TARGETINSTALL); do $(ACT_HOME)/scripts/install $$i $(ACT_HOME)/act/syn/$$i; done
	@if [ ! -d $(ACT_HOME)/act/syn/qdi ]; then mkdir $(ACT_HOME)/act/syn/qdi; fi
	@for i in $(TARGETINSTALL_QDI); do $(ACT_HOME)/scripts/install $$i $(ACT_HOME)/act/syn/$$i; done
	@if [ ! -d $(ACT_HOME)/act/syn/bdopt ]; then mkdir $(ACT_HOME)/act/syn/bdopt; fi
	@for i in $(TARGETINSTALL_BDOPT); do $(ACT_HOME)/scripts/install $$i $(ACT_HOME)/act/syn/$$i; done
	@if [ ! -d $(ACT_HOME)/act/syn/ring ]; then mkdir $(ACT_HOME)/act/syn/ring; fi
	@for i in $(TARGETINSTALL_RING); do $(ACT_HOME)/scripts/install $$i $(ACT_HOME)/act/syn/$$i; done
	@if [ ! -d $(ACT_HOME)/act/syn/liberty ]; then mkdir $(ACT_HOME)/act/syn/liberty; fi
	@for i in $(TARGETINSTALL_LIBERTY); do $(ACT_HOME)/scripts/install $$i $(ACT_HOME)/act/syn/liberty/$$i; done
	@if [ ! -d $(ACT_HOME)/act/syn/diopt ]; then mkdir $(ACT_HOME)/act/syn/diopt; fi
	@for i in $(TARGETINSTALL_DI); do $(ACT_HOME)/scripts/install $$i $(ACT_HOME)/act/syn/$$i; done
	@if [ ! -d $(ACT_HOME)/act/syn/ditest ]; then mkdir $(ACT_HOME)/act/syn/ditest; fi
	@for i in $(TARGETINSTALL_DITEST); do $(ACT_HOME)/scripts/install $$i $(ACT_HOME)/act/syn/$$i; done


-include Makefile.deps
