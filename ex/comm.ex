import "/home/user/Documents/ADCO/act/syn.act";

defproc toplevel (a1of1 go)
{
  /* --- declaring all variables and channels --- */
  aN1of2<32> chan_a;
  syn_var_init_false var_x[32];
  syn_var_init_false var_y[32];
  /* --- end of declarations --- */

  /* semicolon */
  a1of1 c_0;

  /* assign */
  syn_var_init_false const_0;
  syn_var_init_true const_1;
  syn_expr_vararray<32> e_1;
  e_1.v[0] = const_1.v;
  e_1.v[1] = const_1.v;
  e_1.v[2] = const_1.v;
  e_1.v[3] = const_0.v;
  e_1.v[4] = const_1.v;
  e_1.v[5] = const_0.v;
  e_1.v[6] = const_1.v;
  e_1.v[7] = const_0.v;
  e_1.v[8] = const_0.v;
  e_1.v[9] = const_1.v;
  e_1.v[10] = const_0.v;
  e_1.v[11] = const_1.v;
  e_1.v[12] = const_0.v;
  e_1.v[13] = const_1.v;
  e_1.v[14] = const_0.v;
  e_1.v[15] = const_0.v;
  e_1.v[16] = const_1.v;
  e_1.v[17] = const_1.v;
  e_1.v[18] = const_1.v;
  e_1.v[19] = const_0.v;
  e_1.v[20] = const_1.v;
  e_1.v[21] = const_0.v;
  e_1.v[22] = const_1.v;
  e_1.v[23] = const_0.v;
  e_1.v[24] = const_1.v;
  e_1.v[25] = const_0.v;
  e_1.v[26] = const_0.v;
  e_1.v[27] = const_1.v;
  e_1.v[28] = const_0.v;
  e_1.v[29] = const_0.v;
  e_1.v[30] = const_0.v;
  e_1.v[31] = const_0.v;
  a1of1 c_1;
  syn_fullseq s_1;
  c_1.r = s_1.go.r;
  syn_recv rtv_1[32];
  syn_expr_vararray<32> e_2;
  syn_var_init_false tv_1[32];
  (i:32: e_2.v[i] = tv_1[i].v;)
  (i:32: e_2.v[i] = rtv_1[i].v;)
  s_1.r.r = e_1.go_r;
  (i:32: s_1.r.r = rtv_1[i].go.r;)
  syn_ctree<32> ct_1;
  (i:32: ct_1.in[i] = rtv_1[i].go.a;)
  s_1.r.a = ct_1.out;
  (i:32: e_1.out[i].t = rtv_1[i].in.t;
         e_1.out[i].f = rtv_1[i].in.f;)
  s_1.go.a = e_2.go_r;
  syn_recv s_0[32];
  (i:32: s_0[i].go.r = c_1.r;)
  (i:32: s_0[i].in.t = e_2.out[i].t;
         s_0[i].in.f = e_2.out[i].f;
         s_0[i].v = var_x[i].v;)
  syn_ctree<32> ct_0;
  (i:32: ct_0.in[i] = s_0[i].go.a;)
  ct_0.out = c_1.a;

  syn_seq s_2;
  s_2.go = c_0;
  s_2.s1 = c_1;

  /* comma */
  a1of1 c_2;

  /* send */
  syn_expr_vararray<32> e_3;
  (i:32: e_3.v[i] = var_x[i].v;)
  a1of1 c_3;
  syn_fullseq s_3;
  c_3.r = s_3.go.r;
  syn_recv rtv_3[32];
  syn_expr_vararray<32> e_4;
  syn_var_init_false tv_3[32];
  (i:32: e_4.v[i] = tv_3[i].v;)
  (i:32: e_4.v[i] = rtv_3[i].v;)
  s_3.r.r = e_3.go_r;
  (i:32: s_3.r.r = rtv_3[i].go.r;)
  syn_ctree<32> ct_3;
  (i:32: ct_3.in[i] = rtv_3[i].go.a;)
  s_3.r.a = ct_3.out;
  (i:32: e_3.out[i].t = rtv_3[i].in.t;
         e_3.out[i].f = rtv_3[i].in.f;)
  s_3.go.a = e_4.go_r;
  c_3.a = e_4.go_r;
  (i:32: chan_a.d[i] = e_4.out[i];)

  syn_par s_4;
  s_4.go = c_2;
  s_4.s1 = c_3;

  /* recv */
  a1of1 c_4;
  syn_recv s_5[32];
  (i:32: s_5[i].go.r = c_4.r;)
  (i:32: s_5[i].in.t = chan_a.d[i].t;
         s_5[i].in.f = chan_a.d[i].f;
         s_5[i].v = var_y[i].v;)
  syn_ctree<32> ct_5;
  (i:32: ct_5.in[i] = s_5[i].go.a;)
  ct_5.out = c_4.a; c_4.a = chan_a.a;

  s_4.s2 = c_4;

  s_2.s2 = c_2;

  go = c_0;
}

toplevel t;
