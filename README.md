# chp2prs

### Usage

```
Usage: [executable] <actfile> <process> <outfile>
```
Note that `process` should be of the form `"[procname]<>"` to ensure correct execution.

### Overview

A continuation of a project with Rajit Manohar's AVLSI group (see [the original](https://github.com/zebmehring/ADCO), which was developed by Zeb Mehring as part of his senior project at Yale). A program which takes in an `.act` file of the form:
```
defproc foo() {
  /* variable delcarations */
  chp {
    ...
  }
}

foo f;
```
and compiles the constituent CHP into ACT:
```
defproc toplevel(a1of1 go) {
  /* compiled ACT */
  ...
}

toplevel t;
```
using the libraries provided.

The conversion is accomplished using syntax-directed translation, which is jusfitied using direct process decomposition.

### Notes and Installation

This program is for use with [the ACT toolkit](https://github.com/asyncvlsi/act).

   * Please install the ACT toolkit first; installation instructions are [here](https://github.com/asyncvlsi/act/blob/master/README.md).
   * Build this program using the standard ACT tool install instructions [here](https://github.com/asyncvlsi/act/blob/master/README_tool.md).

### Test Suite
```
make runtest
```
will test the correctness of the repository by iterating through the the test/unit_tests folder and reporting the number of passing or failing tests.
`make runtest optimize=1` will pass the "--optimize" flag to the chp2prs executable to check the correctness of the unit tests with sequencer optimizations.
`make runtest bundled=1` will pass the "--bundled" flag to the chp2prs executable to check the correctness of the unit tests with bundled data.

```
make testreps unit={unit_test_name}
```
can be used to test the robustness of a test in terms of random timing. The command repeatedly runs the prsim script of unit test and feeds a different random_seed number each time. The output of the prsim tests are fed into a unit_test's `test_rand.prsim` file. The script will stop when it encounters a prsim failure.


### License

The source code for this tool is released under the GNU GPLv2. The ACT files
that provide supporting library functions (the lib/ directory) are released under GNU LGPLv2.1.
