import "/home/user/Documents/ADCO/act/syn.act";
import "/home/user/Documents/ADCO/act/bundled.act";

defproc toplevel (a1of1 go)
{
  /* --- declaring all variables and channels --- */
  syn_var_init_false var_x[32];
  syn_var_init_false var_y[32];
  syn_var_init_false var_z[32];
  /* --- end of declarations --- */

  /* semicolon */
  a1of1 c_0;

  /* assign */
  syn_var_init_false const_0;
  syn_var_init_true const_1;
  bundled_expr_vararray<32> be_1;
  be_1.v[0] = const_1.v;
  be_1.v[1] = const_0.v;
  be_1.v[2] = const_0.v;
  be_1.v[3] = const_0.v;
  be_1.v[4] = const_1.v;
  be_1.v[5] = const_1.v;
  be_1.v[6] = const_0.v;
  be_1.v[7] = const_1.v;
  be_1.v[8] = const_0.v;
  be_1.v[9] = const_0.v;
  be_1.v[10] = const_1.v;
  be_1.v[11] = const_1.v;
  be_1.v[12] = const_0.v;
  be_1.v[13] = const_1.v;
  be_1.v[14] = const_0.v;
  be_1.v[15] = const_0.v;
  be_1.v[16] = const_0.v;
  be_1.v[17] = const_0.v;
  be_1.v[18] = const_0.v;
  be_1.v[19] = const_0.v;
  be_1.v[20] = const_0.v;
  be_1.v[21] = const_0.v;
  be_1.v[22] = const_0.v;
  be_1.v[23] = const_0.v;
  be_1.v[24] = const_0.v;
  be_1.v[25] = const_0.v;
  be_1.v[26] = const_0.v;
  be_1.v[27] = const_0.v;
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
  a1of1 c_2;
  s_2.s2 = c_2;

  /* assign */
  bundled_expr_vararray<32> be_4;
  be_4.v[0] = const_1.v;
  be_4.v[1] = const_1.v;
  be_4.v[2] = const_0.v;
  be_4.v[3] = const_1.v;
  be_4.v[4] = const_0.v;
  be_4.v[5] = const_0.v;
  be_4.v[6] = const_0.v;
  be_4.v[7] = const_0.v;
  be_4.v[8] = const_0.v;
  be_4.v[9] = const_0.v;
  be_4.v[10] = const_1.v;
  be_4.v[11] = const_1.v;
  be_4.v[12] = const_0.v;
  be_4.v[13] = const_1.v;
  be_4.v[14] = const_1.v;
  be_4.v[15] = const_0.v;
  be_4.v[16] = const_0.v;
  be_4.v[17] = const_0.v;
  be_4.v[18] = const_0.v;
  be_4.v[19] = const_0.v;
  be_4.v[20] = const_0.v;
  be_4.v[21] = const_0.v;
  be_4.v[22] = const_0.v;
  be_4.v[23] = const_0.v;
  be_4.v[24] = const_0.v;
  be_4.v[25] = const_0.v;
  be_4.v[26] = const_0.v;
  be_4.v[27] = const_0.v;
  be_4.v[28] = const_0.v;
  be_4.v[29] = const_0.v;
  be_4.v[30] = const_0.v;
  be_4.v[31] = const_0.v;
  a1of1 c_3;
  delay<64> dn_4;
  dn_4.in = c_3.r;
  bundled_vararray_to_dualrail<32> be_5;
  be_5.d = dn_4.out;
  (i:32: be_5.in[i] = be_4.out[i];)
  syn_expr_vararray<32> e_5;
  e_5.go_r = dn_4.out;
  (i:32: e_5.v[i] = be_5.out[i];)
  syn_fullseq s_4;
  e_5.go_r = s_4.go.r;
  bundled_recv<32> brtv_4;
  syn_expr_vararray<32> e_6;
  syn_var_init_false tv_4[32];
  (i:32: e_6.v[i] = tv_4[i].v;)
  (i:32: e_6.v[i] = brtv_4.v[i];)
  s_4.r.r = brtv_4.go.r;
  s_4.r.a = brtv_4.go.a;
  (i:32: e_5.out[i].t = brtv_4.in.d[i].t;
         e_5.out[i].f = brtv_4.in.d[i].f;)
  s_4.go.a = e_6.go_r;
  bundled_recv<32> s_3;
  s_3.go.r = e_6.go_r;
  s_3.go.a = c_3.a;
  (i:32: s_3.in.d[i].t = e_6.out[i].t;
         s_3.in.d[i].f = e_6.out[i].f;
         s_3.v[i] = var_y[i].v;)

  syn_seq s_5;
  s_5.go = c_2;
  s_5.s1 = c_3;

  /* assign */
  bundled_expr_vararray<32> be_7;
  (i:32: be_7.v[i] = var_y[i].v;)
  bundled_expr_vararray<32> be_8;
  (i:32: be_8.v[i] = var_x[i].v;)
  bundled_mul<32> be_9;
  (i:32: be_9.in1[i] = be_7.out[i];)
  (i:32: be_9.in2[i] = be_8.out[i];)
  a1of1 c_4;
  delay<2048> de_9;
  de_9.in = c_4.r;
  delay<64> dn_9;
  dn_9.in = de_9.out;
  bundled_vararray_to_dualrail<32> be_10;
  be_10.d = dn_9.out;
  (i:32: be_10.in[i] = be_9.out[i];)
  syn_expr_vararray<32> e_10;
  e_10.go_r = dn_9.out;
  (i:32: e_10.v[i] = be_10.out[i];)
  syn_fullseq s_7;
  e_10.go_r = s_7.go.r;
  bundled_recv<32> brtv_7;
  syn_expr_vararray<32> e_11;
  syn_var_init_false tv_7[32];
  (i:32: e_11.v[i] = tv_7[i].v;)
  (i:32: e_11.v[i] = brtv_7.v[i];)
  s_7.r.r = brtv_7.go.r;
  s_7.r.a = brtv_7.go.a;
  (i:32: e_10.out[i].t = brtv_7.in.d[i].t;
         e_10.out[i].f = brtv_7.in.d[i].f;)
  s_7.go.a = e_11.go_r;
  bundled_recv<32> s_6;
  s_6.go.r = e_11.go_r;
  s_6.go.a = c_4.a;
  (i:32: s_6.in.d[i].t = e_11.out[i].t;
         s_6.in.d[i].f = e_11.out[i].f;
         s_6.v[i] = var_z[i].v;)

  s_5.s2 = c_4;

  go = c_0;
}

toplevel t;
