module mux_4 (
    c0, c1, c2, c3, 
    in0, in1, in2, in3, 
    out
);

    input c0, c1, c2, c3, 
        in0, in1, in2, in3;
    output out;

    mux4 g00 (.c_50_6(c0), .c_51_6(c1), .c_52_6(c2),  .c_53_6(c3), 
                .din_50_6_50_6(in0), .din_51_6_50_6(in1), .din_52_6_50_6(in2), .din_53_6_50_6(in3), 
                .dout_50_6(out));

endmodule
