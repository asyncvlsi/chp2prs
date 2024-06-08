# Ring Synthesis : State of the Tool

A quick summary of the state of ring synthesis.
```
Typical usage: synth2 -F ring -E abc [-e <exprfile>] [-o <outfile>] -p <proc> <actfile>
```


# CHP Constraints

Some constraints on the input CHP in order for the synthesis to work correctly. 

* Each process has a single top-level loop, optionally with initial conditions (assignments to variables) before the loop:
```
    chp {
        x1 := e1;
        ...
        xn := en;
        *[
            ...
            (no loops (*[...]) appear in here)
            ...
         ]
    }
``` 

* Each channel must be accessed at most once in the program. No multiple channel access:
```
    chp {
        ...
        *[
            ...
            A!x;
            ...
            A!x;
            ...
         ]
    }
``` 

* Probes are currently unsupported. 

Fixes for the internal loop constraint and the multiple channel access constraint have been implemented, but are yet to be tested extensively. 

# Synthesis Example

Suppose `in.act` contains:
```
defproc src (chan!(int<4>) r)
{
    chp{
        *[ r!5 ]
    }
}

defproc snk (chan?(int<4>) l)
{
    int<4> x;
    chp{
        *[ l?x ; log("%d", x) ]
    }
}

defproc merge (chan?(int<4>) l1; chan?(int<4>) l2; 
				chan?(int<4>) l3; chan!(int<4>) r)
{
	int<4> x;
	int<4> y;
	chp{
            *[ l1?x; 
                [x=5 -> l2?y
                []x=0 -> l3?y
                ]; 
                r!y
             ]
	}
}

defproc testproc ()
{
    chan(int<4>) C, X1, X2, Y;
    merge b (C, X1, X2, Y);
    src ic (C);
    src i1 (X1);
    src i2 (X2);
    snk o  (Y);
}
```
Running
```
synth2 -F ring -E abc -o out.act -p testproc in.act
```
will produce `out.act`, which contains the synthesized process, called `ring_testproc`, and `expr.act`, which contains combinational logic that is imported to implemented expressions. The output can be simulated using:
```
actsim -ref=1 out.act ring_testproc
```




