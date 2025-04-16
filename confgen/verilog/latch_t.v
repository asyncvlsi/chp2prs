module latch_t (
    in0, in1, in2, out
);

    input in0, in1, in2;
    output out;

    latchlo g00 (.D(in0), .CLK(in1), .R(in2), .Q(out));

endmodule
