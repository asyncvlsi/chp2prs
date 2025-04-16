module invt (
    in0, out
);

    input in0;
    output out;

    invx1 g00 (.A(in0), .Y(out));

endmodule
