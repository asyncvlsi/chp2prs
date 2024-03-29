/*************************************************************************
 *
 *  Copyright (c) 2021 Rajit Manohar
 *
 *  This program is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU General Public License
 *  as published by the Free Software Foundation; either version 2
 *  of the License, or (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 51 Franklin Street, Fifth Floor,
 *  Boston, MA  02110-1301, USA.
 *
 **************************************************************************
 */
#ifndef __ACT_CHP2PRS_PASS_H__
#define __ACT_CHP2PRS_PASS_H__

#include <act/act.h>

extern "C" {

  void chp2prs_init (ActPass *ap);
  void chp2prs_run (ActPass *ap, Process *p);
  void chp2prs_recursive (ActPass *ap, UserDef *u, int mode);
  void *chp2prs_proc (ActPass *ap, Process *p, int mode);
  void *chp2prs_data (ActPass *ap, Data *d, int mode);
  void *chp2prs_chan (ActPass *ap, Channel *c, int mode);
  void chp2prs_free (ActPass *ap, void *v);
  void chp2prs_done (ActPass *ap);

}

#endif /* __ACT_CHP2PRS_PASS_H__ */
