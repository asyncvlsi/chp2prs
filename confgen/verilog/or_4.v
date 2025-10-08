module or_4 (
    in0, in1, in2, in3, out
);

    input in0, in1, in2, in3;
    output out;

    or4 g00 (.in_50_6(in0), .in_51_6(in1), .in_52_6(in2), .in_53_6(in3), .out(out));

endmodule
