import "/home/user/Documents/ADCO/act/syn.act";
import "/home/user/Documents/ADCO/act/bundled.act";

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
  bundled_expr_vararray<32> be_1;
  be_1.v[0] = const_1.v;
  be_1.v[1] = const_1.v;
  be_1.v[2] = const_1.v;
  be_1.v[3] = const_0.v;
  be_1.v[4] = const_1.v;
  be_1.v[5] = const_0.v;
  be_1.v[6] = const_1.v;
  be_1.v[7] = const_0.v;
  be_1.v[8] = const_0.v;
  be_1.v[9] = const_1.v;
  be_1.v[10] = const_0.v;
  be_1.v[11] = const_1.v;
  be_1.v[12] = const_0.v;
  be_1.v[13] = const_1.v;
  be_1.v[14] = const_0.v;
  be_1.v[15] = const_0.v;
  be_1.v[16] = const_1.v;
  be_1.v[17] = const_1.v;
  be_1.v[18] = const_1.v;
  be_1.v[19] = const_0.v;
  be_1.v[20] = const_1.v;
  be_1.v[21] = const_0.v;
  be_1.v[22] = const_1.v;
  be_1.v[23] = const_0.v;
  be_1.v[24] = const_1.v;
  be_1.v[25] = const_0.v;
  be_1.v[26] = const_0.v;
  be_1.v[27] = const_1.v;
  be_1.v[28] = const_0.v;
  be_1.v[29] = const_0.v;
  be_1.v[30] = const_0.v;
  be_1.v[31] = const_0.v;
  a1of1 c_1;
  delay<64> dn_1;
  dn_1.in = c_1.r;
  bundled_vararray_to_dualrail<32> be_2;
  be_2.d = dn_1.out;
  (i:32: be_2.in[i] = be_1.out[i];)
  syn_expr_vararray<32> e_2;
  e_2.go_r = dn_1.out;
  (i:32: e_2.v[i] = be_2.out[i];)
  syn_fullseq s_1;
  e_2.go_r = s_1.go.r;
  bundled_recv<32> brtv_1;
  syn_expr_vararray<32> e_3;
  syn_var_init_false tv_1[32];
  (i:32: e_3.v[i] = tv_1[i].v;)
  (i:32: e_3.v[i] = brtv_1.v[i];)
  s_1.r.r = brtv_1.go.r;
  s_1.r.a = brtv_1.go.a;
  (i:32: e_2.out[i].t = brtv_1.in.d[i].t;
         e_2.out[i].f = brtv_1.in.d[i].f;)
  s_1.go.a = e_3.go_r;
  bundled_recv<32> s_0;
  s_0.go.r = e_3.go_r;
  s_0.go.a = c_1.a;
  (i:32: s_0.in.d[i].t = e_3.out[i].t;
         s_0.in.d[i].f = e_3.out[i].f;
         s_0.v[i] = var_x[i].v;)

  syn_seq s_2;
  s_2.go = c_0;
  s_2.s1 = c_1;

  /* comma */
  a1of1 c_2;

  /* send */
  bundled_expr_vararray<32> be_4;
  (i:32: be_4.v[i] = var_x[i].v;)
  a1of1 c_3;
  delay<64> dn_4;
  dn_4.in = c_3.r;
  bundled_vararray_to_dualrail<32> be_5;
  be_5.d = dn_4.out;
  (i:32: be_5.in = be_4.out;)
  syn_expr_vararray<32> e_5;
  e_5.go_r = dn_4.out;
  (i:32: e_5.v[i] = be_5.out[i];)
  syn_fullseq s_3;
  e_5.go_r = s_3.go.r;
  bundled_recv<32> brtv_3;
  syn_expr_vararray<32> e_6;
  syn_var_init_false tv_3[32];
  (i:32: e_6.v[i] = tv_3[i].v;)
  (i:32: e_6.v[i] = brtv_3.v[i];)
  s_3.r.r = brtv_3.go.r;
  s_3.r.a = brtv_3.go.a;
  (i:32: e_5.out[i].t = brtv_3.in.d[i].t;
         e_5.out[i].f = brtv_3.in.d[i].f;)
  s_3.go.a = e_6.go_r;
  c_3.a = e_6.go_r;
  (i:32: chan_a.d[i] = e_6.out[i];)

  syn_par s_4;
  s_4.go = c_2;
  s_4.s1 = c_3;

  /* recv */
  a1of1 c_4;
  bundled_recv<32> s_5;
  s_5.go.r = c_4.r;
  s_5.go.a = c_4.a; c_4.a = chan_a.a;
  (i:32: s_5.in.d[i].t = chan_a.d[i].t;
         s_5.in.d[i].f = chan_a.d[i].f;
         s_5.v[i] = var_y[i].v;)

  s_4.s2 = c_4;

  s_2.s2 = c_2;

  go = c_0;
}

toplevel t;
