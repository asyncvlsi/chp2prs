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
TARGETINSTALL_QDIBASIC=qdibasic/_all_.act qdibasic/dpath.act qdibasic/chan.act qdibasic/syn.act qdibasic/syngates.act 
TARGETINSTALL_QDIOPT=qdiopt/_all_.act qdiopt/stdcells.act qdiopt/dpath.act 
TARGETINSTALL_BDOPT=bdopt/_all_.act bdopt/stdcells.act bdopt/dpath.act bdopt/chan.act bdopt/syn.act bdopt/syngates.act 
TARGETINSTALL_LIBERTY=osu018_stdcells.lib LICENSE.osu018_stdcells.lib

include $(ACT_HOME)/scripts/Makefile.std

all:
	@echo "Done"

install: install_act $(TARGETINSTALL) $(TARGETINSTALL_QDIBASIC) $(TARGETINSTALL_QDIOPT) $(TARGETINSTALL_BDOPT)
	@if [ ! -d $(ACT_HOME)/act/syn ]; then mkdir $(ACT_HOME)/act/syn; fi
	@for i in $(TARGETINSTALL); do $(ACT_HOME)/scripts/install $$i $(ACT_HOME)/act/syn/$$i; done
	@if [ ! -d $(ACT_HOME)/act/syn/qdibasic ]; then mkdir $(ACT_HOME)/act/syn/qdibasic; fi
	@for i in $(TARGETINSTALL_QDIBASIC); do $(ACT_HOME)/scripts/install $$i $(ACT_HOME)/act/syn/$$i; done
	@if [ ! -d $(ACT_HOME)/act/syn/qdiopt ]; then mkdir $(ACT_HOME)/act/syn/qdiopt; fi
	@for i in $(TARGETINSTALL_QDIOPT); do $(ACT_HOME)/scripts/install $$i $(ACT_HOME)/act/syn/$$i; done
	@if [ ! -d $(ACT_HOME)/act/syn/bdopt ]; then mkdir $(ACT_HOME)/act/syn/bdopt; fi
	@for i in $(TARGETINSTALL_BDOPT); do $(ACT_HOME)/scripts/install $$i $(ACT_HOME)/act/syn/$$i; done
	@if [ ! -d $(ACT_HOME)/act/syn/liberty ]; then mkdir $(ACT_HOME)/act/syn/liberty; fi
	@for i in $(TARGETINSTALL_LIBERTY); do $(ACT_HOME)/scripts/install $$i $(ACT_HOME)/act/syn/liberty/$$i; done


-include Makefile.deps
