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
  be_1.v[4] = const_0.v;
  be_1.v[5] = const_0.v;
  be_1.v[6] = const_0.v;
  be_1.v[7] = const_0.v;
  be_1.v[8] = const_0.v;
  be_1.v[9] = const_0.v;
  be_1.v[10] = const_0.v;
  be_1.v[11] = const_0.v;
  be_1.v[12] = const_0.v;
  be_1.v[13] = const_0.v;
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
  be_4.v[0] = const_0.v;
  be_4.v[1] = const_0.v;
  be_4.v[2] = const_0.v;
  be_4.v[3] = const_0.v;
  be_4.v[4] = const_0.v;
  be_4.v[5] = const_0.v;
  be_4.v[6] = const_0.v;
  be_4.v[7] = const_0.v;
  be_4.v[8] = const_0.v;
  be_4.v[9] = const_0.v;
  be_4.v[10] = const_0.v;
  be_4.v[11] = const_0.v;
  be_4.v[12] = const_0.v;
  be_4.v[13] = const_0.v;
  be_4.v[14] = const_0.v;
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
  a1of1 c_4;
  s_5.s2 = c_4;


  /* emit individual gc (#0) [loop] */
  r1of2 gc_0;
  bundled_expr_vararray<32> be_7;
  (i:32: be_7.v[i] = var_y[i].v;)
  bundled_expr_vararray<32> be_8;
  be_8.v[0] = const_0.v;
  be_8.v[1] = const_1.v;
  be_8.v[2] = const_0.v;
  be_8.v[3] = const_1.v;
  be_8.v[4] = const_0.v;
  be_8.v[5] = const_0.v;
  be_8.v[6] = const_0.v;
  be_8.v[7] = const_0.v;
  be_8.v[8] = const_0.v;
  be_8.v[9] = const_0.v;
  be_8.v[10] = const_0.v;
  be_8.v[11] = const_0.v;
  be_8.v[12] = const_0.v;
  be_8.v[13] = const_0.v;
  be_8.v[14] = const_0.v;
  be_8.v[15] = const_0.v;
  be_8.v[16] = const_0.v;
  be_8.v[17] = const_0.v;
  be_8.v[18] = const_0.v;
  be_8.v[19] = const_0.v;
  be_8.v[20] = const_0.v;
  be_8.v[21] = const_0.v;
  be_8.v[22] = const_0.v;
  be_8.v[23] = const_0.v;
  be_8.v[24] = const_0.v;
  be_8.v[25] = const_0.v;
  be_8.v[26] = const_0.v;
  be_8.v[27] = const_0.v;
  be_8.v[28] = const_0.v;
  be_8.v[29] = const_0.v;
  be_8.v[30] = const_0.v;
  be_8.v[31] = const_0.v;
  bundled_ne<32> be_9;
  (i:32: be_9.in1[i] = be_7.out[i];)
  (i:32: be_9.in2[i] = be_8.out[i];)
  delay<128> de_9;
  de_9.in = gc_0.r;
  delay<2> dn_9;
  dn_9.in = de_9.out;
  bundled_var_to_dualrail be_10;
  be_10.d = dn_9.out;
  be_10.in = be_9.out;
  syn_expr_var e_10;
  e_10.v = be_10.out;
  syn_fullseq s_6;
  dn_9.out = s_6.go.r;
  syn_recv rtv_6;
  syn_expr_var e_11;
  syn_var_init_false tv_6;
  tv_6.v = rtv_6.v;
  e_11.v = tv_6.v;
  s_6.r.r = e_10.go_r;
  s_6.r = rtv_6.go;
  e_10.out.t = rtv_6.in.t;
  e_10.out.f = rtv_6.in.f;
  s_6.go.a = e_11.go_r;
  /* comma */
  a1of1 c_5;

  /* assign */
  bundled_expr_vararray<32> be_12;
  (i:32: be_12.v[i] = var_x[i].v;)
  bundled_expr_vararray<32> be_13;
  be_13.v[0] = const_0.v;
  be_13.v[1] = const_1.v;
  be_13.v[2] = const_0.v;
  be_13.v[3] = const_0.v;
  be_13.v[4] = const_0.v;
  be_13.v[5] = const_0.v;
  be_13.v[6] = const_0.v;
  be_13.v[7] = const_0.v;
  be_13.v[8] = const_0.v;
  be_13.v[9] = const_0.v;
  be_13.v[10] = const_0.v;
  be_13.v[11] = const_0.v;
  be_13.v[12] = const_0.v;
  be_13.v[13] = const_0.v;
  be_13.v[14] = const_0.v;
  be_13.v[15] = const_0.v;
  be_13.v[16] = const_0.v;
  be_13.v[17] = const_0.v;
  be_13.v[18] = const_0.v;
  be_13.v[19] = const_0.v;
  be_13.v[20] = const_0.v;
  be_13.v[21] = const_0.v;
  be_13.v[22] = const_0.v;
  be_13.v[23] = const_0.v;
  be_13.v[24] = const_0.v;
  be_13.v[25] = const_0.v;
  be_13.v[26] = const_0.v;
  be_13.v[27] = const_0.v;
  be_13.v[28] = const_0.v;
  be_13.v[29] = const_0.v;
  be_13.v[30] = const_0.v;
  be_13.v[31] = const_0.v;
  bundled_mul<32> be_14;
  (i:32: be_14.in1[i] = be_12.out[i];)
  (i:32: be_14.in2[i] = be_13.out[i];)
  a1of1 c_6;
  delay<2048> de_14;
  de_14.in = c_6.r;
  delay<64> dn_14;
  dn_14.in = de_14.out;
  bundled_vararray_to_dualrail<32> be_15;
  be_15.d = dn_14.out;
  (i:32: be_15.in[i] = be_14.out[i];)
  syn_expr_vararray<32> e_15;
  e_15.go_r = dn_14.out;
  (i:32: e_15.v[i] = be_15.out[i];)
  syn_fullseq s_8;
  e_15.go_r = s_8.go.r;
  bundled_recv<32> brtv_8;
  syn_expr_vararray<32> e_16;
  syn_var_init_false tv_8[32];
  (i:32: e_16.v[i] = tv_8[i].v;)
  (i:32: e_16.v[i] = brtv_8.v[i];)
  s_8.r.r = brtv_8.go.r;
  s_8.r.a = brtv_8.go.a;
  (i:32: e_15.out[i].t = brtv_8.in.d[i].t;
         e_15.out[i].f = brtv_8.in.d[i].f;)
  s_8.go.a = e_16.go_r;
  bundled_recv<32> s_7;
  s_7.go.r = e_16.go_r;
  s_7.go.a = c_6.a;
  (i:32: s_7.in.d[i].t = e_16.out[i].t;
         s_7.in.d[i].f = e_16.out[i].f;
         s_7.v[i] = var_x[i].v;)

  syn_par s_9;
  s_9.go = c_5;
  s_9.s1 = c_6;

  /* assign */
  bundled_expr_vararray<32> be_17;
  (i:32: be_17.v[i] = var_y[i].v;)
  bundled_expr_vararray<32> be_18;
  be_18.v[0] = const_1.v;
  be_18.v[1] = const_0.v;
  be_18.v[2] = const_0.v;
  be_18.v[3] = const_0.v;
  be_18.v[4] = const_0.v;
  be_18.v[5] = const_0.v;
  be_18.v[6] = const_0.v;
  be_18.v[7] = const_0.v;
  be_18.v[8] = const_0.v;
  be_18.v[9] = const_0.v;
  be_18.v[10] = const_0.v;
  be_18.v[11] = const_0.v;
  be_18.v[12] = const_0.v;
  be_18.v[13] = const_0.v;
  be_18.v[14] = const_0.v;
  be_18.v[15] = const_0.v;
  be_18.v[16] = const_0.v;
  be_18.v[17] = const_0.v;
  be_18.v[18] = const_0.v;
  be_18.v[19] = const_0.v;
  be_18.v[20] = const_0.v;
  be_18.v[21] = const_0.v;
  be_18.v[22] = const_0.v;
  be_18.v[23] = const_0.v;
  be_18.v[24] = const_0.v;
  be_18.v[25] = const_0.v;
  be_18.v[26] = const_0.v;
  be_18.v[27] = const_0.v;
  be_18.v[28] = const_0.v;
  be_18.v[29] = const_0.v;
  be_18.v[30] = const_0.v;
  be_18.v[31] = const_0.v;
  bundled_add<32> be_19;
  (i:32: be_19.in1[i] = be_17.out[i];)
  (i:32: be_19.in2[i] = be_18.out[i];)
  a1of1 c_7;
  delay<64> de_19;
  de_19.in = c_7.r;
  delay<64> dn_19;
  dn_19.in = de_19.out;
  bundled_vararray_to_dualrail<32> be_20;
  be_20.d = dn_19.out;
  (i:32: be_20.in[i] = be_19.out[i];)
  syn_expr_vararray<32> e_20;
  e_20.go_r = dn_19.out;
  (i:32: e_20.v[i] = be_20.out[i];)
  syn_fullseq s_11;
  e_20.go_r = s_11.go.r;
  bundled_recv<32> brtv_11;
  syn_expr_vararray<32> e_21;
  syn_var_init_false tv_11[32];
  (i:32: e_21.v[i] = tv_11[i].v;)
  (i:32: e_21.v[i] = brtv_11.v[i];)
  s_11.r.r = brtv_11.go.r;
  s_11.r.a = brtv_11.go.a;
  (i:32: e_20.out[i].t = brtv_11.in.d[i].t;
         e_20.out[i].f = brtv_11.in.d[i].f;)
  s_11.go.a = e_21.go_r;
  bundled_recv<32> s_10;
  s_10.go.r = e_21.go_r;
  s_10.go.a = c_7.a;
  (i:32: s_10.in.d[i].t = e_21.out[i].t;
         s_10.in.d[i].f = e_21.out[i].f;
         s_10.v[i] = var_y[i].v;)

  s_9.s2 = c_7;

  e_11.out.t = c_5.r;
  gc_0.t = c_5.a;
  gc_0.f = e_11.out.f;
  a1of1 c_8;
  /* gc cascade, start = 0, end = 0 */
  syn_bool_notand na_12;
  na_12.in1 = c_8.r;
  na_12.out = gc_0.r;
  gc_0.t = na_12.in2;
  gc_0.f = c_8.a;
  /* end of gc (#0) */

  syn_seq s_13;
  s_13.go = c_4;
  s_13.s1 = c_8;

  /* assign */
  bundled_expr_vararray<32> be_22;
  (i:32: be_22.v[i] = var_x[i].v;)
  a1of1 c_9;
  delay<64> dn_22;
  dn_22.in = c_9.r;
  bundled_vararray_to_dualrail<32> be_23;
  be_23.d = dn_22.out;
  (i:32: be_23.in[i] = be_22.out[i];)
  syn_expr_vararray<32> e_23;
  e_23.go_r = dn_22.out;
  (i:32: e_23.v[i] = be_23.out[i];)
  syn_fullseq s_15;
  e_23.go_r = s_15.go.r;
  bundled_recv<32> brtv_15;
  syn_expr_vararray<32> e_24;
  syn_var_init_false tv_15[32];
  (i:32: e_24.v[i] = tv_15[i].v;)
  (i:32: e_24.v[i] = brtv_15.v[i];)
  s_15.r.r = brtv_15.go.r;
  s_15.r.a = brtv_15.go.a;
  (i:32: e_23.out[i].t = brtv_15.in.d[i].t;
         e_23.out[i].f = brtv_15.in.d[i].f;)
  s_15.go.a = e_24.go_r;
  bundled_recv<32> s_14;
  s_14.go.r = e_24.go_r;
  s_14.go.a = c_9.a;
  (i:32: s_14.in.d[i].t = e_24.out[i].t;
         s_14.in.d[i].f = e_24.out[i].f;
         s_14.v[i] = var_z[i].v;)

  s_13.s2 = c_9;

  go = c_0;
}

toplevel t;
