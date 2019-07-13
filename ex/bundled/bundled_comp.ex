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
  be_1.v[5] = const_0.v;
  be_1.v[6] = const_0.v;
  be_1.v[7] = const_0.v;
  be_1.v[8] = const_1.v;
  be_1.v[9] = const_1.v;
  be_1.v[10] = const_0.v;
  be_1.v[11] = const_0.v;
  be_1.v[12] = const_1.v;
  be_1.v[13] = const_1.v;
  be_1.v[14] = const_1.v;
  be_1.v[15] = const_1.v;
  be_1.v[16] = const_0.v;
  be_1.v[17] = const_1.v;
  be_1.v[18] = const_1.v;
  be_1.v[19] = const_1.v;
  be_1.v[20] = const_0.v;
  be_1.v[21] = const_0.v;
  be_1.v[22] = const_1.v;
  be_1.v[23] = const_0.v;
  be_1.v[24] = const_1.v;
  be_1.v[25] = const_0.v;
  be_1.v[26] = const_1.v;
  be_1.v[27] = const_0.v;
  be_1.v[28] = const_1.v;
  be_1.v[29] = const_1.v;
  be_1.v[30] = const_0.v;
  be_1.v[31] = const_1.v;
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
  be_4.v[1] = const_1.v;
  be_4.v[2] = const_1.v;
  be_4.v[3] = const_0.v;
  be_4.v[4] = const_1.v;
  be_4.v[5] = const_1.v;
  be_4.v[6] = const_1.v;
  be_4.v[7] = const_0.v;
  be_4.v[8] = const_1.v;
  be_4.v[9] = const_1.v;
  be_4.v[10] = const_1.v;
  be_4.v[11] = const_1.v;
  be_4.v[12] = const_1.v;
  be_4.v[13] = const_0.v;
  be_4.v[14] = const_1.v;
  be_4.v[15] = const_1.v;
  be_4.v[16] = const_0.v;
  be_4.v[17] = const_0.v;
  be_4.v[18] = const_1.v;
  be_4.v[19] = const_0.v;
  be_4.v[20] = const_1.v;
  be_4.v[21] = const_1.v;
  be_4.v[22] = const_0.v;
  be_4.v[23] = const_1.v;
  be_4.v[24] = const_0.v;
  be_4.v[25] = const_0.v;
  be_4.v[26] = const_0.v;
  be_4.v[27] = const_1.v;
  be_4.v[28] = const_1.v;
  be_4.v[29] = const_0.v;
  be_4.v[30] = const_1.v;
  be_4.v[31] = const_1.v;
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


  /* emit individual gc (#0) [selection] */
  r1of2 gc_0;
  bundled_expr_vararray<32> be_7;
  (i:32: be_7.v[i] = var_x[i].v;)
  bundled_expr_vararray<32> be_8;
  (i:32: be_8.v[i] = var_y[i].v;)
  bundled_gt<32> be_9;
  (i:32: be_9.in1[i] = be_7.out[i];)
  (i:32: be_9.in2[i] = be_8.out[i];)
  delay<192> de_9;
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
  /* assign */
  bundled_expr_vararray<32> be_12;
  be_12.v[0] = const_1.v;
  be_12.v[1] = const_0.v;
  be_12.v[2] = const_0.v;
  be_12.v[3] = const_0.v;
  be_12.v[4] = const_0.v;
  be_12.v[5] = const_0.v;
  be_12.v[6] = const_0.v;
  be_12.v[7] = const_0.v;
  be_12.v[8] = const_0.v;
  be_12.v[9] = const_0.v;
  be_12.v[10] = const_0.v;
  be_12.v[11] = const_0.v;
  be_12.v[12] = const_0.v;
  be_12.v[13] = const_0.v;
  be_12.v[14] = const_0.v;
  be_12.v[15] = const_0.v;
  be_12.v[16] = const_0.v;
  be_12.v[17] = const_0.v;
  be_12.v[18] = const_0.v;
  be_12.v[19] = const_0.v;
  be_12.v[20] = const_0.v;
  be_12.v[21] = const_0.v;
  be_12.v[22] = const_0.v;
  be_12.v[23] = const_0.v;
  be_12.v[24] = const_0.v;
  be_12.v[25] = const_0.v;
  be_12.v[26] = const_0.v;
  be_12.v[27] = const_0.v;
  be_12.v[28] = const_0.v;
  be_12.v[29] = const_0.v;
  be_12.v[30] = const_0.v;
  be_12.v[31] = const_0.v;
  a1of1 c_5;
  delay<64> dn_12;
  dn_12.in = c_5.r;
  bundled_vararray_to_dualrail<32> be_13;
  be_13.d = dn_12.out;
  (i:32: be_13.in[i] = be_12.out[i];)
  syn_expr_vararray<32> e_13;
  e_13.go_r = dn_12.out;
  (i:32: e_13.v[i] = be_13.out[i];)
  syn_fullseq s_8;
  e_13.go_r = s_8.go.r;
  bundled_recv<32> brtv_8;
  syn_expr_vararray<32> e_14;
  syn_var_init_false tv_8[32];
  (i:32: e_14.v[i] = tv_8[i].v;)
  (i:32: e_14.v[i] = brtv_8.v[i];)
  s_8.r.r = brtv_8.go.r;
  s_8.r.a = brtv_8.go.a;
  (i:32: e_13.out[i].t = brtv_8.in.d[i].t;
         e_13.out[i].f = brtv_8.in.d[i].f;)
  s_8.go.a = e_14.go_r;
  bundled_recv<32> s_7;
  s_7.go.r = e_14.go_r;
  s_7.go.a = c_5.a;
  (i:32: s_7.in.d[i].t = e_14.out[i].t;
         s_7.in.d[i].f = e_14.out[i].f;
         s_7.v[i] = var_z[i].v;)

  e_11.out.t = c_5.r;
  gc_0.t = c_5.a;
  gc_0.f = e_11.out.f;
  r1of2 gc_1;
  bundled_expr_vararray<32> be_15;
  (i:32: be_15.v[i] = var_x[i].v;)
  bundled_expr_vararray<32> be_16;
  (i:32: be_16.v[i] = var_y[i].v;)
  bundled_lt<32> be_17;
  (i:32: be_17.in1[i] = be_15.out[i];)
  (i:32: be_17.in2[i] = be_16.out[i];)
  delay<128> de_17;
  de_17.in = gc_1.r;
  delay<2> dn_17;
  dn_17.in = de_17.out;
  bundled_var_to_dualrail be_18;
  be_18.d = dn_17.out;
  be_18.in = be_17.out;
  syn_expr_var e_18;
  e_18.v = be_18.out;
  syn_fullseq s_9;
  dn_17.out = s_9.go.r;
  syn_recv rtv_9;
  syn_expr_var e_19;
  syn_var_init_false tv_9;
  tv_9.v = rtv_9.v;
  e_19.v = tv_9.v;
  s_9.r.r = e_18.go_r;
  s_9.r = rtv_9.go;
  e_18.out.t = rtv_9.in.t;
  e_18.out.f = rtv_9.in.f;
  s_9.go.a = e_19.go_r;
  /* assign */
  bundled_expr_vararray<32> be_20;
  be_20.v[0] = const_0.v;
  be_20.v[1] = const_1.v;
  be_20.v[2] = const_0.v;
  be_20.v[3] = const_0.v;
  be_20.v[4] = const_0.v;
  be_20.v[5] = const_0.v;
  be_20.v[6] = const_0.v;
  be_20.v[7] = const_0.v;
  be_20.v[8] = const_0.v;
  be_20.v[9] = const_0.v;
  be_20.v[10] = const_0.v;
  be_20.v[11] = const_0.v;
  be_20.v[12] = const_0.v;
  be_20.v[13] = const_0.v;
  be_20.v[14] = const_0.v;
  be_20.v[15] = const_0.v;
  be_20.v[16] = const_0.v;
  be_20.v[17] = const_0.v;
  be_20.v[18] = const_0.v;
  be_20.v[19] = const_0.v;
  be_20.v[20] = const_0.v;
  be_20.v[21] = const_0.v;
  be_20.v[22] = const_0.v;
  be_20.v[23] = const_0.v;
  be_20.v[24] = const_0.v;
  be_20.v[25] = const_0.v;
  be_20.v[26] = const_0.v;
  be_20.v[27] = const_0.v;
  be_20.v[28] = const_0.v;
  be_20.v[29] = const_0.v;
  be_20.v[30] = const_0.v;
  be_20.v[31] = const_0.v;
  a1of1 c_6;
  delay<64> dn_20;
  dn_20.in = c_6.r;
  bundled_vararray_to_dualrail<32> be_21;
  be_21.d = dn_20.out;
  (i:32: be_21.in[i] = be_20.out[i];)
  syn_expr_vararray<32> e_21;
  e_21.go_r = dn_20.out;
  (i:32: e_21.v[i] = be_21.out[i];)
  syn_fullseq s_11;
  e_21.go_r = s_11.go.r;
  bundled_recv<32> brtv_11;
  syn_expr_vararray<32> e_22;
  syn_var_init_false tv_11[32];
  (i:32: e_22.v[i] = tv_11[i].v;)
  (i:32: e_22.v[i] = brtv_11.v[i];)
  s_11.r.r = brtv_11.go.r;
  s_11.r.a = brtv_11.go.a;
  (i:32: e_21.out[i].t = brtv_11.in.d[i].t;
         e_21.out[i].f = brtv_11.in.d[i].f;)
  s_11.go.a = e_22.go_r;
  bundled_recv<32> s_10;
  s_10.go.r = e_22.go_r;
  s_10.go.a = c_6.a;
  (i:32: s_10.in.d[i].t = e_22.out[i].t;
         s_10.in.d[i].f = e_22.out[i].f;
         s_10.v[i] = var_z[i].v;)

  e_19.out.t = c_6.r;
  gc_1.t = c_6.a;
  gc_1.f = e_19.out.f;
  r1of2 gc_2;
  bundled_expr_vararray<32> be_23;
  (i:32: be_23.v[i] = var_x[i].v;)
  bundled_expr_vararray<32> be_24;
  (i:32: be_24.v[i] = var_y[i].v;)
  bundled_eq<32> be_25;
  (i:32: be_25.in1[i] = be_23.out[i];)
  (i:32: be_25.in2[i] = be_24.out[i];)
  delay<128> de_25;
  de_25.in = gc_2.r;
  delay<2> dn_25;
  dn_25.in = de_25.out;
  bundled_var_to_dualrail be_26;
  be_26.d = dn_25.out;
  be_26.in = be_25.out;
  syn_expr_var e_26;
  e_26.v = be_26.out;
  syn_fullseq s_12;
  dn_25.out = s_12.go.r;
  syn_recv rtv_12;
  syn_expr_var e_27;
  syn_var_init_false tv_12;
  tv_12.v = rtv_12.v;
  e_27.v = tv_12.v;
  s_12.r.r = e_26.go_r;
  s_12.r = rtv_12.go;
  e_26.out.t = rtv_12.in.t;
  e_26.out.f = rtv_12.in.f;
  s_12.go.a = e_27.go_r;
  /* assign */
  bundled_expr_vararray<32> be_28;
  be_28.v[0] = const_1.v;
  be_28.v[1] = const_1.v;
  be_28.v[2] = const_0.v;
  be_28.v[3] = const_0.v;
  be_28.v[4] = const_0.v;
  be_28.v[5] = const_0.v;
  be_28.v[6] = const_0.v;
  be_28.v[7] = const_0.v;
  be_28.v[8] = const_0.v;
  be_28.v[9] = const_0.v;
  be_28.v[10] = const_0.v;
  be_28.v[11] = const_0.v;
  be_28.v[12] = const_0.v;
  be_28.v[13] = const_0.v;
  be_28.v[14] = const_0.v;
  be_28.v[15] = const_0.v;
  be_28.v[16] = const_0.v;
  be_28.v[17] = const_0.v;
  be_28.v[18] = const_0.v;
  be_28.v[19] = const_0.v;
  be_28.v[20] = const_0.v;
  be_28.v[21] = const_0.v;
  be_28.v[22] = const_0.v;
  be_28.v[23] = const_0.v;
  be_28.v[24] = const_0.v;
  be_28.v[25] = const_0.v;
  be_28.v[26] = const_0.v;
  be_28.v[27] = const_0.v;
  be_28.v[28] = const_0.v;
  be_28.v[29] = const_0.v;
  be_28.v[30] = const_0.v;
  be_28.v[31] = const_0.v;
  a1of1 c_7;
  delay<64> dn_28;
  dn_28.in = c_7.r;
  bundled_vararray_to_dualrail<32> be_29;
  be_29.d = dn_28.out;
  (i:32: be_29.in[i] = be_28.out[i];)
  syn_expr_vararray<32> e_29;
  e_29.go_r = dn_28.out;
  (i:32: e_29.v[i] = be_29.out[i];)
  syn_fullseq s_14;
  e_29.go_r = s_14.go.r;
  bundled_recv<32> brtv_14;
  syn_expr_vararray<32> e_30;
  syn_var_init_false tv_14[32];
  (i:32: e_30.v[i] = tv_14[i].v;)
  (i:32: e_30.v[i] = brtv_14.v[i];)
  s_14.r.r = brtv_14.go.r;
  s_14.r.a = brtv_14.go.a;
  (i:32: e_29.out[i].t = brtv_14.in.d[i].t;
         e_29.out[i].f = brtv_14.in.d[i].f;)
  s_14.go.a = e_30.go_r;
  bundled_recv<32> s_13;
  s_13.go.r = e_30.go_r;
  s_13.go.a = c_7.a;
  (i:32: s_13.in.d[i].t = e_30.out[i].t;
         s_13.in.d[i].f = e_30.out[i].f;
         s_13.v[i] = var_z[i].v;)

  e_27.out.t = c_7.r;
  gc_2.t = c_7.a;
  gc_2.f = e_27.out.f;
  a1of1 c_8;
  /* gc cascade, start = 0, end = 2 */
  gc_0.f = gc_1.r;
  gc_1.f = gc_2.r;
  syn_bool_notand na_15;
  na_15.in1 = c_8.r;
  na_15.out = gc_0.r;
  syn_bool_or or_16;
  or_16.in1 = gc_0.t;
  or_16.in2 = gc_1.t;
  or_16.out = c_8.a;
  gc_2.f = na_15.in2;
  /* end of gc (#0) */

  syn_seq s_17;
  s_17.go = c_4;
  s_17.s1 = c_8;
  a1of1 c_9;
  s_17.s2 = c_9;

  /* assign */
  bundled_expr_vararray<32> be_31;
  be_31.v[0] = const_1.v;
  be_31.v[1] = const_0.v;
  be_31.v[2] = const_1.v;
  be_31.v[3] = const_0.v;
  be_31.v[4] = const_1.v;
  be_31.v[5] = const_0.v;
  be_31.v[6] = const_0.v;
  be_31.v[7] = const_0.v;
  be_31.v[8] = const_1.v;
  be_31.v[9] = const_1.v;
  be_31.v[10] = const_0.v;
  be_31.v[11] = const_1.v;
  be_31.v[12] = const_1.v;
  be_31.v[13] = const_0.v;
  be_31.v[14] = const_0.v;
  be_31.v[15] = const_1.v;
  be_31.v[16] = const_0.v;
  be_31.v[17] = const_1.v;
  be_31.v[18] = const_1.v;
  be_31.v[19] = const_0.v;
  be_31.v[20] = const_0.v;
  be_31.v[21] = const_1.v;
  be_31.v[22] = const_0.v;
  be_31.v[23] = const_0.v;
  be_31.v[24] = const_0.v;
  be_31.v[25] = const_0.v;
  be_31.v[26] = const_1.v;
  be_31.v[27] = const_1.v;
  be_31.v[28] = const_0.v;
  be_31.v[29] = const_0.v;
  be_31.v[30] = const_0.v;
  be_31.v[31] = const_0.v;
  a1of1 c_10;
  delay<64> dn_31;
  dn_31.in = c_10.r;
  bundled_vararray_to_dualrail<32> be_32;
  be_32.d = dn_31.out;
  (i:32: be_32.in[i] = be_31.out[i];)
  syn_expr_vararray<32> e_32;
  e_32.go_r = dn_31.out;
  (i:32: e_32.v[i] = be_32.out[i];)
  syn_fullseq s_19;
  e_32.go_r = s_19.go.r;
  bundled_recv<32> brtv_19;
  syn_expr_vararray<32> e_33;
  syn_var_init_false tv_19[32];
  (i:32: e_33.v[i] = tv_19[i].v;)
  (i:32: e_33.v[i] = brtv_19.v[i];)
  s_19.r.r = brtv_19.go.r;
  s_19.r.a = brtv_19.go.a;
  (i:32: e_32.out[i].t = brtv_19.in.d[i].t;
         e_32.out[i].f = brtv_19.in.d[i].f;)
  s_19.go.a = e_33.go_r;
  bundled_recv<32> s_18;
  s_18.go.r = e_33.go_r;
  s_18.go.a = c_10.a;
  (i:32: s_18.in.d[i].t = e_33.out[i].t;
         s_18.in.d[i].f = e_33.out[i].f;
         s_18.v[i] = var_x[i].v;)

  syn_seq s_20;
  s_20.go = c_9;
  s_20.s1 = c_10;
  a1of1 c_11;
  s_20.s2 = c_11;


  /* emit individual gc (#1) [selection] */
  r1of2 gc_3;
  bundled_expr_vararray<32> be_34;
  (i:32: be_34.v[i] = var_x[i].v;)
  bundled_expr_vararray<32> be_35;
  (i:32: be_35.v[i] = var_y[i].v;)
  bundled_gt<32> be_36;
  (i:32: be_36.in1[i] = be_34.out[i];)
  (i:32: be_36.in2[i] = be_35.out[i];)
  delay<192> de_36;
  de_36.in = gc_3.r;
  delay<2> dn_36;
  dn_36.in = de_36.out;
  bundled_var_to_dualrail be_37;
  be_37.d = dn_36.out;
  be_37.in = be_36.out;
  syn_expr_var e_37;
  e_37.v = be_37.out;
  syn_fullseq s_21;
  dn_36.out = s_21.go.r;
  syn_recv rtv_21;
  syn_expr_var e_38;
  syn_var_init_false tv_21;
  tv_21.v = rtv_21.v;
  e_38.v = tv_21.v;
  s_21.r.r = e_37.go_r;
  s_21.r = rtv_21.go;
  e_37.out.t = rtv_21.in.t;
  e_37.out.f = rtv_21.in.f;
  s_21.go.a = e_38.go_r;
  /* assign */
  bundled_expr_vararray<32> be_39;
  be_39.v[0] = const_1.v;
  be_39.v[1] = const_0.v;
  be_39.v[2] = const_0.v;
  be_39.v[3] = const_0.v;
  be_39.v[4] = const_0.v;
  be_39.v[5] = const_0.v;
  be_39.v[6] = const_0.v;
  be_39.v[7] = const_0.v;
  be_39.v[8] = const_0.v;
  be_39.v[9] = const_0.v;
  be_39.v[10] = const_0.v;
  be_39.v[11] = const_0.v;
  be_39.v[12] = const_0.v;
  be_39.v[13] = const_0.v;
  be_39.v[14] = const_0.v;
  be_39.v[15] = const_0.v;
  be_39.v[16] = const_0.v;
  be_39.v[17] = const_0.v;
  be_39.v[18] = const_0.v;
  be_39.v[19] = const_0.v;
  be_39.v[20] = const_0.v;
  be_39.v[21] = const_0.v;
  be_39.v[22] = const_0.v;
  be_39.v[23] = const_0.v;
  be_39.v[24] = const_0.v;
  be_39.v[25] = const_0.v;
  be_39.v[26] = const_0.v;
  be_39.v[27] = const_0.v;
  be_39.v[28] = const_0.v;
  be_39.v[29] = const_0.v;
  be_39.v[30] = const_0.v;
  be_39.v[31] = const_0.v;
  a1of1 c_12;
  delay<64> dn_39;
  dn_39.in = c_12.r;
  bundled_vararray_to_dualrail<32> be_40;
  be_40.d = dn_39.out;
  (i:32: be_40.in[i] = be_39.out[i];)
  syn_expr_vararray<32> e_40;
  e_40.go_r = dn_39.out;
  (i:32: e_40.v[i] = be_40.out[i];)
  syn_fullseq s_23;
  e_40.go_r = s_23.go.r;
  bundled_recv<32> brtv_23;
  syn_expr_vararray<32> e_41;
  syn_var_init_false tv_23[32];
  (i:32: e_41.v[i] = tv_23[i].v;)
  (i:32: e_41.v[i] = brtv_23.v[i];)
  s_23.r.r = brtv_23.go.r;
  s_23.r.a = brtv_23.go.a;
  (i:32: e_40.out[i].t = brtv_23.in.d[i].t;
         e_40.out[i].f = brtv_23.in.d[i].f;)
  s_23.go.a = e_41.go_r;
  bundled_recv<32> s_22;
  s_22.go.r = e_41.go_r;
  s_22.go.a = c_12.a;
  (i:32: s_22.in.d[i].t = e_41.out[i].t;
         s_22.in.d[i].f = e_41.out[i].f;
         s_22.v[i] = var_z[i].v;)

  e_38.out.t = c_12.r;
  gc_3.t = c_12.a;
  gc_3.f = e_38.out.f;
  r1of2 gc_4;
  bundled_expr_vararray<32> be_42;
  (i:32: be_42.v[i] = var_x[i].v;)
  bundled_expr_vararray<32> be_43;
  (i:32: be_43.v[i] = var_y[i].v;)
  bundled_lt<32> be_44;
  (i:32: be_44.in1[i] = be_42.out[i];)
  (i:32: be_44.in2[i] = be_43.out[i];)
  delay<128> de_44;
  de_44.in = gc_4.r;
  delay<2> dn_44;
  dn_44.in = de_44.out;
  bundled_var_to_dualrail be_45;
  be_45.d = dn_44.out;
  be_45.in = be_44.out;
  syn_expr_var e_45;
  e_45.v = be_45.out;
  syn_fullseq s_24;
  dn_44.out = s_24.go.r;
  syn_recv rtv_24;
  syn_expr_var e_46;
  syn_var_init_false tv_24;
  tv_24.v = rtv_24.v;
  e_46.v = tv_24.v;
  s_24.r.r = e_45.go_r;
  s_24.r = rtv_24.go;
  e_45.out.t = rtv_24.in.t;
  e_45.out.f = rtv_24.in.f;
  s_24.go.a = e_46.go_r;
  /* assign */
  bundled_expr_vararray<32> be_47;
  be_47.v[0] = const_0.v;
  be_47.v[1] = const_1.v;
  be_47.v[2] = const_0.v;
  be_47.v[3] = const_0.v;
  be_47.v[4] = const_0.v;
  be_47.v[5] = const_0.v;
  be_47.v[6] = const_0.v;
  be_47.v[7] = const_0.v;
  be_47.v[8] = const_0.v;
  be_47.v[9] = const_0.v;
  be_47.v[10] = const_0.v;
  be_47.v[11] = const_0.v;
  be_47.v[12] = const_0.v;
  be_47.v[13] = const_0.v;
  be_47.v[14] = const_0.v;
  be_47.v[15] = const_0.v;
  be_47.v[16] = const_0.v;
  be_47.v[17] = const_0.v;
  be_47.v[18] = const_0.v;
  be_47.v[19] = const_0.v;
  be_47.v[20] = const_0.v;
  be_47.v[21] = const_0.v;
  be_47.v[22] = const_0.v;
  be_47.v[23] = const_0.v;
  be_47.v[24] = const_0.v;
  be_47.v[25] = const_0.v;
  be_47.v[26] = const_0.v;
  be_47.v[27] = const_0.v;
  be_47.v[28] = const_0.v;
  be_47.v[29] = const_0.v;
  be_47.v[30] = const_0.v;
  be_47.v[31] = const_0.v;
  a1of1 c_13;
  delay<64> dn_47;
  dn_47.in = c_13.r;
  bundled_vararray_to_dualrail<32> be_48;
  be_48.d = dn_47.out;
  (i:32: be_48.in[i] = be_47.out[i];)
  syn_expr_vararray<32> e_48;
  e_48.go_r = dn_47.out;
  (i:32: e_48.v[i] = be_48.out[i];)
  syn_fullseq s_26;
  e_48.go_r = s_26.go.r;
  bundled_recv<32> brtv_26;
  syn_expr_vararray<32> e_49;
  syn_var_init_false tv_26[32];
  (i:32: e_49.v[i] = tv_26[i].v;)
  (i:32: e_49.v[i] = brtv_26.v[i];)
  s_26.r.r = brtv_26.go.r;
  s_26.r.a = brtv_26.go.a;
  (i:32: e_48.out[i].t = brtv_26.in.d[i].t;
         e_48.out[i].f = brtv_26.in.d[i].f;)
  s_26.go.a = e_49.go_r;
  bundled_recv<32> s_25;
  s_25.go.r = e_49.go_r;
  s_25.go.a = c_13.a;
  (i:32: s_25.in.d[i].t = e_49.out[i].t;
         s_25.in.d[i].f = e_49.out[i].f;
         s_25.v[i] = var_z[i].v;)

  e_46.out.t = c_13.r;
  gc_4.t = c_13.a;
  gc_4.f = e_46.out.f;
  r1of2 gc_5;
  bundled_expr_vararray<32> be_50;
  (i:32: be_50.v[i] = var_x[i].v;)
  bundled_expr_vararray<32> be_51;
  (i:32: be_51.v[i] = var_y[i].v;)
  bundled_eq<32> be_52;
  (i:32: be_52.in1[i] = be_50.out[i];)
  (i:32: be_52.in2[i] = be_51.out[i];)
  delay<128> de_52;
  de_52.in = gc_5.r;
  delay<2> dn_52;
  dn_52.in = de_52.out;
  bundled_var_to_dualrail be_53;
  be_53.d = dn_52.out;
  be_53.in = be_52.out;
  syn_expr_var e_53;
  e_53.v = be_53.out;
  syn_fullseq s_27;
  dn_52.out = s_27.go.r;
  syn_recv rtv_27;
  syn_expr_var e_54;
  syn_var_init_false tv_27;
  tv_27.v = rtv_27.v;
  e_54.v = tv_27.v;
  s_27.r.r = e_53.go_r;
  s_27.r = rtv_27.go;
  e_53.out.t = rtv_27.in.t;
  e_53.out.f = rtv_27.in.f;
  s_27.go.a = e_54.go_r;
  /* assign */
  bundled_expr_vararray<32> be_55;
  be_55.v[0] = const_1.v;
  be_55.v[1] = const_1.v;
  be_55.v[2] = const_0.v;
  be_55.v[3] = const_0.v;
  be_55.v[4] = const_0.v;
  be_55.v[5] = const_0.v;
  be_55.v[6] = const_0.v;
  be_55.v[7] = const_0.v;
  be_55.v[8] = const_0.v;
  be_55.v[9] = const_0.v;
  be_55.v[10] = const_0.v;
  be_55.v[11] = const_0.v;
  be_55.v[12] = const_0.v;
  be_55.v[13] = const_0.v;
  be_55.v[14] = const_0.v;
  be_55.v[15] = const_0.v;
  be_55.v[16] = const_0.v;
  be_55.v[17] = const_0.v;
  be_55.v[18] = const_0.v;
  be_55.v[19] = const_0.v;
  be_55.v[20] = const_0.v;
  be_55.v[21] = const_0.v;
  be_55.v[22] = const_0.v;
  be_55.v[23] = const_0.v;
  be_55.v[24] = const_0.v;
  be_55.v[25] = const_0.v;
  be_55.v[26] = const_0.v;
  be_55.v[27] = const_0.v;
  be_55.v[28] = const_0.v;
  be_55.v[29] = const_0.v;
  be_55.v[30] = const_0.v;
  be_55.v[31] = const_0.v;
  a1of1 c_14;
  delay<64> dn_55;
  dn_55.in = c_14.r;
  bundled_vararray_to_dualrail<32> be_56;
  be_56.d = dn_55.out;
  (i:32: be_56.in[i] = be_55.out[i];)
  syn_expr_vararray<32> e_56;
  e_56.go_r = dn_55.out;
  (i:32: e_56.v[i] = be_56.out[i];)
  syn_fullseq s_29;
  e_56.go_r = s_29.go.r;
  bundled_recv<32> brtv_29;
  syn_expr_vararray<32> e_57;
  syn_var_init_false tv_29[32];
  (i:32: e_57.v[i] = tv_29[i].v;)
  (i:32: e_57.v[i] = brtv_29.v[i];)
  s_29.r.r = brtv_29.go.r;
  s_29.r.a = brtv_29.go.a;
  (i:32: e_56.out[i].t = brtv_29.in.d[i].t;
         e_56.out[i].f = brtv_29.in.d[i].f;)
  s_29.go.a = e_57.go_r;
  bundled_recv<32> s_28;
  s_28.go.r = e_57.go_r;
  s_28.go.a = c_14.a;
  (i:32: s_28.in.d[i].t = e_57.out[i].t;
         s_28.in.d[i].f = e_57.out[i].f;
         s_28.v[i] = var_z[i].v;)

  e_54.out.t = c_14.r;
  gc_5.t = c_14.a;
  gc_5.f = e_54.out.f;
  a1of1 c_15;
  /* gc cascade, start = 3, end = 5 */
  gc_3.f = gc_4.r;
  gc_4.f = gc_5.r;
  syn_bool_notand na_30;
  na_30.in1 = c_15.r;
  na_30.out = gc_3.r;
  syn_bool_or or_31;
  or_31.in1 = gc_3.t;
  or_31.in2 = gc_4.t;
  or_31.out = c_15.a;
  gc_5.f = na_30.in2;
  /* end of gc (#1) */

  syn_seq s_32;
  s_32.go = c_11;
  s_32.s1 = c_15;
  a1of1 c_16;
  s_32.s2 = c_16;

  /* assign */
  bundled_expr_vararray<32> be_58;
  be_58.v[0] = const_0.v;
  be_58.v[1] = const_1.v;
  be_58.v[2] = const_1.v;
  be_58.v[3] = const_0.v;
  be_58.v[4] = const_1.v;
  be_58.v[5] = const_1.v;
  be_58.v[6] = const_1.v;
  be_58.v[7] = const_0.v;
  be_58.v[8] = const_1.v;
  be_58.v[9] = const_1.v;
  be_58.v[10] = const_1.v;
  be_58.v[11] = const_1.v;
  be_58.v[12] = const_1.v;
  be_58.v[13] = const_0.v;
  be_58.v[14] = const_1.v;
  be_58.v[15] = const_1.v;
  be_58.v[16] = const_0.v;
  be_58.v[17] = const_0.v;
  be_58.v[18] = const_1.v;
  be_58.v[19] = const_0.v;
  be_58.v[20] = const_1.v;
  be_58.v[21] = const_1.v;
  be_58.v[22] = const_0.v;
  be_58.v[23] = const_1.v;
  be_58.v[24] = const_0.v;
  be_58.v[25] = const_0.v;
  be_58.v[26] = const_0.v;
  be_58.v[27] = const_1.v;
  be_58.v[28] = const_1.v;
  be_58.v[29] = const_0.v;
  be_58.v[30] = const_1.v;
  be_58.v[31] = const_1.v;
  a1of1 c_17;
  delay<64> dn_58;
  dn_58.in = c_17.r;
  bundled_vararray_to_dualrail<32> be_59;
  be_59.d = dn_58.out;
  (i:32: be_59.in[i] = be_58.out[i];)
  syn_expr_vararray<32> e_59;
  e_59.go_r = dn_58.out;
  (i:32: e_59.v[i] = be_59.out[i];)
  syn_fullseq s_34;
  e_59.go_r = s_34.go.r;
  bundled_recv<32> brtv_34;
  syn_expr_vararray<32> e_60;
  syn_var_init_false tv_34[32];
  (i:32: e_60.v[i] = tv_34[i].v;)
  (i:32: e_60.v[i] = brtv_34.v[i];)
  s_34.r.r = brtv_34.go.r;
  s_34.r.a = brtv_34.go.a;
  (i:32: e_59.out[i].t = brtv_34.in.d[i].t;
         e_59.out[i].f = brtv_34.in.d[i].f;)
  s_34.go.a = e_60.go_r;
  bundled_recv<32> s_33;
  s_33.go.r = e_60.go_r;
  s_33.go.a = c_17.a;
  (i:32: s_33.in.d[i].t = e_60.out[i].t;
         s_33.in.d[i].f = e_60.out[i].f;
         s_33.v[i] = var_x[i].v;)

  syn_seq s_35;
  s_35.go = c_16;
  s_35.s1 = c_17;


  /* emit individual gc (#2) [selection] */
  r1of2 gc_6;
  bundled_expr_vararray<32> be_61;
  (i:32: be_61.v[i] = var_x[i].v;)
  bundled_expr_vararray<32> be_62;
  (i:32: be_62.v[i] = var_y[i].v;)
  bundled_gt<32> be_63;
  (i:32: be_63.in1[i] = be_61.out[i];)
  (i:32: be_63.in2[i] = be_62.out[i];)
  delay<192> de_63;
  de_63.in = gc_6.r;
  delay<2> dn_63;
  dn_63.in = de_63.out;
  bundled_var_to_dualrail be_64;
  be_64.d = dn_63.out;
  be_64.in = be_63.out;
  syn_expr_var e_64;
  e_64.v = be_64.out;
  syn_fullseq s_36;
  dn_63.out = s_36.go.r;
  syn_recv rtv_36;
  syn_expr_var e_65;
  syn_var_init_false tv_36;
  tv_36.v = rtv_36.v;
  e_65.v = tv_36.v;
  s_36.r.r = e_64.go_r;
  s_36.r = rtv_36.go;
  e_64.out.t = rtv_36.in.t;
  e_64.out.f = rtv_36.in.f;
  s_36.go.a = e_65.go_r;
  /* assign */
  bundled_expr_vararray<32> be_66;
  be_66.v[0] = const_1.v;
  be_66.v[1] = const_0.v;
  be_66.v[2] = const_0.v;
  be_66.v[3] = const_0.v;
  be_66.v[4] = const_0.v;
  be_66.v[5] = const_0.v;
  be_66.v[6] = const_0.v;
  be_66.v[7] = const_0.v;
  be_66.v[8] = const_0.v;
  be_66.v[9] = const_0.v;
  be_66.v[10] = const_0.v;
  be_66.v[11] = const_0.v;
  be_66.v[12] = const_0.v;
  be_66.v[13] = const_0.v;
  be_66.v[14] = const_0.v;
  be_66.v[15] = const_0.v;
  be_66.v[16] = const_0.v;
  be_66.v[17] = const_0.v;
  be_66.v[18] = const_0.v;
  be_66.v[19] = const_0.v;
  be_66.v[20] = const_0.v;
  be_66.v[21] = const_0.v;
  be_66.v[22] = const_0.v;
  be_66.v[23] = const_0.v;
  be_66.v[24] = const_0.v;
  be_66.v[25] = const_0.v;
  be_66.v[26] = const_0.v;
  be_66.v[27] = const_0.v;
  be_66.v[28] = const_0.v;
  be_66.v[29] = const_0.v;
  be_66.v[30] = const_0.v;
  be_66.v[31] = const_0.v;
  a1of1 c_18;
  delay<64> dn_66;
  dn_66.in = c_18.r;
  bundled_vararray_to_dualrail<32> be_67;
  be_67.d = dn_66.out;
  (i:32: be_67.in[i] = be_66.out[i];)
  syn_expr_vararray<32> e_67;
  e_67.go_r = dn_66.out;
  (i:32: e_67.v[i] = be_67.out[i];)
  syn_fullseq s_38;
  e_67.go_r = s_38.go.r;
  bundled_recv<32> brtv_38;
  syn_expr_vararray<32> e_68;
  syn_var_init_false tv_38[32];
  (i:32: e_68.v[i] = tv_38[i].v;)
  (i:32: e_68.v[i] = brtv_38.v[i];)
  s_38.r.r = brtv_38.go.r;
  s_38.r.a = brtv_38.go.a;
  (i:32: e_67.out[i].t = brtv_38.in.d[i].t;
         e_67.out[i].f = brtv_38.in.d[i].f;)
  s_38.go.a = e_68.go_r;
  bundled_recv<32> s_37;
  s_37.go.r = e_68.go_r;
  s_37.go.a = c_18.a;
  (i:32: s_37.in.d[i].t = e_68.out[i].t;
         s_37.in.d[i].f = e_68.out[i].f;
         s_37.v[i] = var_z[i].v;)

  e_65.out.t = c_18.r;
  gc_6.t = c_18.a;
  gc_6.f = e_65.out.f;
  r1of2 gc_7;
  bundled_expr_vararray<32> be_69;
  (i:32: be_69.v[i] = var_x[i].v;)
  bundled_expr_vararray<32> be_70;
  (i:32: be_70.v[i] = var_y[i].v;)
  bundled_lt<32> be_71;
  (i:32: be_71.in1[i] = be_69.out[i];)
  (i:32: be_71.in2[i] = be_70.out[i];)
  delay<128> de_71;
  de_71.in = gc_7.r;
  delay<2> dn_71;
  dn_71.in = de_71.out;
  bundled_var_to_dualrail be_72;
  be_72.d = dn_71.out;
  be_72.in = be_71.out;
  syn_expr_var e_72;
  e_72.v = be_72.out;
  syn_fullseq s_39;
  dn_71.out = s_39.go.r;
  syn_recv rtv_39;
  syn_expr_var e_73;
  syn_var_init_false tv_39;
  tv_39.v = rtv_39.v;
  e_73.v = tv_39.v;
  s_39.r.r = e_72.go_r;
  s_39.r = rtv_39.go;
  e_72.out.t = rtv_39.in.t;
  e_72.out.f = rtv_39.in.f;
  s_39.go.a = e_73.go_r;
  /* assign */
  bundled_expr_vararray<32> be_74;
  be_74.v[0] = const_0.v;
  be_74.v[1] = const_1.v;
  be_74.v[2] = const_0.v;
  be_74.v[3] = const_0.v;
  be_74.v[4] = const_0.v;
  be_74.v[5] = const_0.v;
  be_74.v[6] = const_0.v;
  be_74.v[7] = const_0.v;
  be_74.v[8] = const_0.v;
  be_74.v[9] = const_0.v;
  be_74.v[10] = const_0.v;
  be_74.v[11] = const_0.v;
  be_74.v[12] = const_0.v;
  be_74.v[13] = const_0.v;
  be_74.v[14] = const_0.v;
  be_74.v[15] = const_0.v;
  be_74.v[16] = const_0.v;
  be_74.v[17] = const_0.v;
  be_74.v[18] = const_0.v;
  be_74.v[19] = const_0.v;
  be_74.v[20] = const_0.v;
  be_74.v[21] = const_0.v;
  be_74.v[22] = const_0.v;
  be_74.v[23] = const_0.v;
  be_74.v[24] = const_0.v;
  be_74.v[25] = const_0.v;
  be_74.v[26] = const_0.v;
  be_74.v[27] = const_0.v;
  be_74.v[28] = const_0.v;
  be_74.v[29] = const_0.v;
  be_74.v[30] = const_0.v;
  be_74.v[31] = const_0.v;
  a1of1 c_19;
  delay<64> dn_74;
  dn_74.in = c_19.r;
  bundled_vararray_to_dualrail<32> be_75;
  be_75.d = dn_74.out;
  (i:32: be_75.in[i] = be_74.out[i];)
  syn_expr_vararray<32> e_75;
  e_75.go_r = dn_74.out;
  (i:32: e_75.v[i] = be_75.out[i];)
  syn_fullseq s_41;
  e_75.go_r = s_41.go.r;
  bundled_recv<32> brtv_41;
  syn_expr_vararray<32> e_76;
  syn_var_init_false tv_41[32];
  (i:32: e_76.v[i] = tv_41[i].v;)
  (i:32: e_76.v[i] = brtv_41.v[i];)
  s_41.r.r = brtv_41.go.r;
  s_41.r.a = brtv_41.go.a;
  (i:32: e_75.out[i].t = brtv_41.in.d[i].t;
         e_75.out[i].f = brtv_41.in.d[i].f;)
  s_41.go.a = e_76.go_r;
  bundled_recv<32> s_40;
  s_40.go.r = e_76.go_r;
  s_40.go.a = c_19.a;
  (i:32: s_40.in.d[i].t = e_76.out[i].t;
         s_40.in.d[i].f = e_76.out[i].f;
         s_40.v[i] = var_z[i].v;)

  e_73.out.t = c_19.r;
  gc_7.t = c_19.a;
  gc_7.f = e_73.out.f;
  r1of2 gc_8;
  bundled_expr_vararray<32> be_77;
  (i:32: be_77.v[i] = var_x[i].v;)
  bundled_expr_vararray<32> be_78;
  (i:32: be_78.v[i] = var_y[i].v;)
  bundled_eq<32> be_79;
  (i:32: be_79.in1[i] = be_77.out[i];)
  (i:32: be_79.in2[i] = be_78.out[i];)
  delay<128> de_79;
  de_79.in = gc_8.r;
  delay<2> dn_79;
  dn_79.in = de_79.out;
  bundled_var_to_dualrail be_80;
  be_80.d = dn_79.out;
  be_80.in = be_79.out;
  syn_expr_var e_80;
  e_80.v = be_80.out;
  syn_fullseq s_42;
  dn_79.out = s_42.go.r;
  syn_recv rtv_42;
  syn_expr_var e_81;
  syn_var_init_false tv_42;
  tv_42.v = rtv_42.v;
  e_81.v = tv_42.v;
  s_42.r.r = e_80.go_r;
  s_42.r = rtv_42.go;
  e_80.out.t = rtv_42.in.t;
  e_80.out.f = rtv_42.in.f;
  s_42.go.a = e_81.go_r;
  /* assign */
  bundled_expr_vararray<32> be_82;
  be_82.v[0] = const_1.v;
  be_82.v[1] = const_1.v;
  be_82.v[2] = const_0.v;
  be_82.v[3] = const_0.v;
  be_82.v[4] = const_0.v;
  be_82.v[5] = const_0.v;
  be_82.v[6] = const_0.v;
  be_82.v[7] = const_0.v;
  be_82.v[8] = const_0.v;
  be_82.v[9] = const_0.v;
  be_82.v[10] = const_0.v;
  be_82.v[11] = const_0.v;
  be_82.v[12] = const_0.v;
  be_82.v[13] = const_0.v;
  be_82.v[14] = const_0.v;
  be_82.v[15] = const_0.v;
  be_82.v[16] = const_0.v;
  be_82.v[17] = const_0.v;
  be_82.v[18] = const_0.v;
  be_82.v[19] = const_0.v;
  be_82.v[20] = const_0.v;
  be_82.v[21] = const_0.v;
  be_82.v[22] = const_0.v;
  be_82.v[23] = const_0.v;
  be_82.v[24] = const_0.v;
  be_82.v[25] = const_0.v;
  be_82.v[26] = const_0.v;
  be_82.v[27] = const_0.v;
  be_82.v[28] = const_0.v;
  be_82.v[29] = const_0.v;
  be_82.v[30] = const_0.v;
  be_82.v[31] = const_0.v;
  a1of1 c_20;
  delay<64> dn_82;
  dn_82.in = c_20.r;
  bundled_vararray_to_dualrail<32> be_83;
  be_83.d = dn_82.out;
  (i:32: be_83.in[i] = be_82.out[i];)
  syn_expr_vararray<32> e_83;
  e_83.go_r = dn_82.out;
  (i:32: e_83.v[i] = be_83.out[i];)
  syn_fullseq s_44;
  e_83.go_r = s_44.go.r;
  bundled_recv<32> brtv_44;
  syn_expr_vararray<32> e_84;
  syn_var_init_false tv_44[32];
  (i:32: e_84.v[i] = tv_44[i].v;)
  (i:32: e_84.v[i] = brtv_44.v[i];)
  s_44.r.r = brtv_44.go.r;
  s_44.r.a = brtv_44.go.a;
  (i:32: e_83.out[i].t = brtv_44.in.d[i].t;
         e_83.out[i].f = brtv_44.in.d[i].f;)
  s_44.go.a = e_84.go_r;
  bundled_recv<32> s_43;
  s_43.go.r = e_84.go_r;
  s_43.go.a = c_20.a;
  (i:32: s_43.in.d[i].t = e_84.out[i].t;
         s_43.in.d[i].f = e_84.out[i].f;
         s_43.v[i] = var_z[i].v;)

  e_81.out.t = c_20.r;
  gc_8.t = c_20.a;
  gc_8.f = e_81.out.f;
  a1of1 c_21;
  /* gc cascade, start = 6, end = 8 */
  gc_6.f = gc_7.r;
  gc_7.f = gc_8.r;
  syn_bool_notand na_45;
  na_45.in1 = c_21.r;
  na_45.out = gc_6.r;
  syn_bool_or or_46;
  or_46.in1 = gc_6.t;
  or_46.in2 = gc_7.t;
  or_46.out = c_21.a;
  gc_8.f = na_45.in2;
  /* end of gc (#2) */

  s_35.s2 = c_21;

  go = c_0;
}

toplevel t;
