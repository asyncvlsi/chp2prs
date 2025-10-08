module mux_2 (
    c0, c1, 
    in0, in1, 
    out
);

    input c0, c1, 
        in0, in1;
    output out;

    mux2 g00 (.c_50_6(c0), .c_51_6(c1), .din_50_6_50_6(in0), .din_51_6_50_6(in1), .dout_50_6(out));

endmodule
