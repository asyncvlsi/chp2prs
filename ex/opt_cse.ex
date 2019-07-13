import "/home/user/Documents/ADCO/act/syn.act";

defproc toplevel (a1of1 go)
{
  /* --- declaring all variables and channels --- */
  syn_var_init_false var_w[32];
  syn_var_init_false var_x[32];
  syn_var_init_false var_y[32];
  syn_var_init_false var_z[32];
  /* --- end of declarations --- */

  /* semicolon */
  a1of1 c_0;

  /* assign */
  syn_var_init_false const_0;
  syn_var_init_true const_1;
  syn_expr_vararray<32> e_1;
  e_1.v[0] = const_0.v;
  e_1.v[1] = const_0.v;
  e_1.v[2] = const_0.v;
  e_1.v[3] = const_0.v;
  e_1.v[4] = const_1.v;
  e_1.v[5] = const_0.v;
  e_1.v[6] = const_0.v;
  e_1.v[7] = const_0.v;
  e_1.v[8] = const_0.v;
  e_1.v[9] = const_1.v;
  e_1.v[10] = const_1.v;
  e_1.v[11] = const_0.v;
  e_1.v[12] = const_1.v;
  e_1.v[13] = const_0.v;
  e_1.v[14] = const_1.v;
  e_1.v[15] = const_0.v;
  e_1.v[16] = const_1.v;
  e_1.v[17] = const_0.v;
  e_1.v[18] = const_1.v;
  e_1.v[19] = const_1.v;
  e_1.v[20] = const_0.v;
  e_1.v[21] = const_1.v;
  e_1.v[22] = const_0.v;
  e_1.v[23] = const_0.v;
  e_1.v[24] = const_0.v;
  e_1.v[25] = const_1.v;
  e_1.v[26] = const_1.v;
  e_1.v[27] = const_1.v;
  e_1.v[28] = const_0.v;
  e_1.v[29] = const_0.v;
  e_1.v[30] = const_1.v;
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
  a1of1 c_2;
  s_2.s2 = c_2;

  /* assign */
  syn_expr_vararray<32> e_3;
  e_3.v[0] = const_1.v;
  e_3.v[1] = const_1.v;
  e_3.v[2] = const_1.v;
  e_3.v[3] = const_1.v;
  e_3.v[4] = const_0.v;
  e_3.v[5] = const_0.v;
  e_3.v[6] = const_0.v;
  e_3.v[7] = const_0.v;
  e_3.v[8] = const_1.v;
  e_3.v[9] = const_1.v;
  e_3.v[10] = const_1.v;
  e_3.v[11] = const_1.v;
  e_3.v[12] = const_0.v;
  e_3.v[13] = const_0.v;
  e_3.v[14] = const_1.v;
  e_3.v[15] = const_0.v;
  e_3.v[16] = const_1.v;
  e_3.v[17] = const_1.v;
  e_3.v[18] = const_0.v;
  e_3.v[19] = const_0.v;
  e_3.v[20] = const_0.v;
  e_3.v[21] = const_0.v;
  e_3.v[22] = const_1.v;
  e_3.v[23] = const_0.v;
  e_3.v[24] = const_1.v;
  e_3.v[25] = const_0.v;
  e_3.v[26] = const_1.v;
  e_3.v[27] = const_0.v;
  e_3.v[28] = const_1.v;
  e_3.v[29] = const_0.v;
  e_3.v[30] = const_0.v;
  e_3.v[31] = const_0.v;
  a1of1 c_3;
  syn_fullseq s_4;
  c_3.r = s_4.go.r;
  syn_recv rtv_4[32];
  syn_expr_vararray<32> e_4;
  syn_var_init_false tv_4[32];
  (i:32: e_4.v[i] = tv_4[i].v;)
  (i:32: e_4.v[i] = rtv_4[i].v;)
  s_4.r.r = e_3.go_r;
  (i:32: s_4.r.r = rtv_4[i].go.r;)
  syn_ctree<32> ct_4;
  (i:32: ct_4.in[i] = rtv_4[i].go.a;)
  s_4.r.a = ct_4.out;
  (i:32: e_3.out[i].t = rtv_4[i].in.t;
         e_3.out[i].f = rtv_4[i].in.f;)
  s_4.go.a = e_4.go_r;
  syn_recv s_3[32];
  (i:32: s_3[i].go.r = c_3.r;)
  (i:32: s_3[i].in.t = e_4.out[i].t;
         s_3[i].in.f = e_4.out[i].f;
         s_3[i].v = var_y[i].v;)
  syn_ctree<32> ct_3;
  (i:32: ct_3.in[i] = s_3[i].go.a;)
  ct_3.out = c_3.a;

  syn_seq s_5;
  s_5.go = c_2;
  s_5.s1 = c_3;

  /* comma */
  a1of1 c_4;

  /* assign */
  syn_expr_vararray<32> e_5;
  (i:32: e_5.v[i] = var_x[i].v;)
  syn_expr_vararray<32> e_6;
  (i:32: e_6.v[i] = var_y[i].v;)
  e_6.go_r = e_5.go_r;
  syn_add<32> e_7;
  (i:32: e_7.in1[i] = e_5.out[i];)
  (i:32: e_7.in2[i] = e_6.out[i];)
  a1of1 c_5;
  syn_fullseq s_7;
  c_5.r = s_7.go.r;
  syn_recv rtv_7[32];
  syn_expr_vararray<32> e_8;
  syn_var_init_false tv_7[32];
  (i:32: e_8.v[i] = tv_7[i].v;)
  (i:32: e_8.v[i] = rtv_7[i].v;)
  s_7.r.r = e_5.go_r;
  (i:32: s_7.r.r = rtv_7[i].go.r;)
  syn_ctree<32> ct_7;
  (i:32: ct_7.in[i] = rtv_7[i].go.a;)
  s_7.r.a = ct_7.out;
  (i:32: e_7.out[i].t = rtv_7[i].in.t;
         e_7.out[i].f = rtv_7[i].in.f;)
  s_7.go.a = e_8.go_r;
  syn_recv s_6[32];
  (i:32: s_6[i].go.r = c_5.r;)
  (i:32: s_6[i].in.t = e_8.out[i].t;
         s_6[i].in.f = e_8.out[i].f;
         s_6[i].v = var_z[i].v;)
  syn_ctree<32> ct_6;
  (i:32: ct_6.in[i] = s_6[i].go.a;)
  ct_6.out = c_5.a;

  syn_par s_8;
  s_8.go = c_4;
  s_8.s1 = c_5;

  /* assign */
  syn_expr_vararray<32> e_9;
  (i:32: e_9.v[i] = var_y[i].v;)
  syn_expr_vararray<32> e_10;
  (i:32: e_10.v[i] = var_x[i].v;)
  e_10.go_r = e_9.go_r;
  a1of1 c_6;
  syn_fullseq s_10;
  c_6.r = s_10.go.r;
  syn_recv rtv_10[32];
  syn_expr_vararray<32> e_11;
  syn_var_init_false tv_10[32];
  (i:32: e_11.v[i] = tv_10[i].v;)
  (i:32: e_11.v[i] = rtv_10[i].v;)
  s_10.r.r = e_9.go_r;
  (i:32: s_10.r.r = rtv_10[i].go.r;)
  syn_ctree<32> ct_10;
  (i:32: ct_10.in[i] = rtv_10[i].go.a;)
  s_10.r.a = ct_10.out;
  (i:32: e_7.out[i].t = rtv_10[i].in.t;
         e_7.out[i].f = rtv_10[i].in.f;)
  s_10.go.a = e_11.go_r;
  syn_recv s_9[32];
  (i:32: s_9[i].go.r = c_6.r;)
  (i:32: s_9[i].in.t = e_11.out[i].t;
         s_9[i].in.f = e_11.out[i].f;
         s_9[i].v = var_w[i].v;)
  syn_ctree<32> ct_9;
  (i:32: ct_9.in[i] = s_9[i].go.a;)
  ct_9.out = c_6.a;

  s_8.s2 = c_6;

  s_5.s2 = c_4;

  go = c_0;
}

toplevel t;
