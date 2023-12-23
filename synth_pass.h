/*************************************************************************
 *
 *  Copyright (c) 2023 Rajit Manohar
 *
 *  This library is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU Lesser General Public
 *  License as published by the Free Software Foundation; either
 *  version 2.1 of the License, or (at your option) any later version.
 *
 *  This library is distributed in the hope that it will be useful,
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
#ifndef __ACT_SYNTH_PASS_H__
#define __ACT_SYNTH_PASS_H__

#include <act/act.h>

extern "C" {

  void synthesis_init (ActPass *ap);
  void synthesis_run (ActPass *ap, Process *p);
  void synthesis_recursive (ActPass *ap, UserDef *u, int mode);
  void *synthesis_proc (ActPass *ap, Process *p, int mode);
  void *synthesis_data (ActPass *ap, Data *d, int mode);
  void *synthesis_chan (ActPass *ap, Channel *c, int mode);
  void synthesis_free (ActPass *ap, void *v);
  void synthesis_done (ActPass *ap);

}

#endif /* __ACT_SYNTH_PASS_H__ */
