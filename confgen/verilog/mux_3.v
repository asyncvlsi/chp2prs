module mux_3 (
    c0, c1, c2, 
    in0, in1, in2, 
    out
);

    input c0, c1, c2, 
        in0, in1, in2;
    output out;

    mux3 g00 (.c_50_6(c0), .c_51_6(c1), .c_52_6(c2), .din_50_6_50_6(in0), .din_51_6_50_6(in1), .din_52_6_50_6(in2), .dout_50_6(out));

endmodule
