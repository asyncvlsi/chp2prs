module recv_t (
    in0, in1, in2, out1, out2
);

    input in0, in1, in2;
    output out1, out2;

    recv g00 (.m1_ad_ad_50_6(in0), .zero_ad_ad_50_6(in1), .p1_aa(in2), .m1_aa(out1), .data_ad_ad_50_6(out2));

endmodule
