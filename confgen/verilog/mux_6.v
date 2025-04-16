module mux_6 (
    c0, c1, c2, c3, c4, c5,
    in0, in1, in2, in3, in4, in5,
    out
);

    input c0, c1, c2, c3, c4, c5,
        in0, in1, in2, in3, in4, in5;
    output out;

    mux6 g00 (.c_50_6(c0), .c_51_6(c1), .c_52_6(c2),  .c_53_6(c3), .c_54_6(c4), .c_55_6(c5), 
                .din_50_6_50_6(in0), .din_51_6_50_6(in1), .din_52_6_50_6(in2), .din_53_6_50_6(in3), .din_54_6_50_6(in4), .din_55_6_50_6(in5), 
                .dout_50_6(out));

endmodule
