# chp2prs

### Usage

```
Usage: [executable] [-Ob] [-e <exprfile>] [-o yosys] <actfile> <process> <outfile>
```
The options are:
   * `-b` : use bundled data datapath. Default is to use a QDI datapath
   * `-O` : run CHP optimizations. Requires the chp-opt package.
   * `-e <exprfile>`: process definitions for each expression evaluation are saved in `<exprfile>`. The default is `expr.act`
   * `-o yosys` : run expression optimization using `yosys`. Requires the expropt package.
   * `<actfile>` : the input ACT file that contains the design
   * `<process>` : the name of the ACT process to be translated (the top-level process)
   * `<outfile>` : where the result should be saved.


### Installation

This program is for use with [the ACT toolkit](https://github.com/asyncvlsi/act).

   * Please install the ACT toolkit first; installation instructions are [here](https://github.com/asyncvlsi/act/blob/master/README.md).
   * Install the ACT standard [library](https://github.com/asyncvlsi/stdlib)
   * Run ./configure
   * Build this program using the standard ACT tool install instructions [here](https://github.com/asyncvlsi/act/blob/master/README_tool.md).


### Overview

A continuation of a project with Rajit Manohar's AVLSI group (see [the original](https://github.com/zebmehring/ADCO), which was developed by Zeb Mehring as part of his senior project at Yale). A program which takes in an `.act` file of the form:
```
defproc foo() {
  /* variable delcarations */
  chp {
    ...
  }
}
```
and compiles the constituent CHP into ACT:
```
defproc sdt_foo <: foo() {
  /* compiled ACT */
  ...
}
```
using the libraries provided. The translation is created by using the `refine { ... }` module, and so use the `-ref=1` command-line option to ACT tools to use the generated circuit.

The conversion is accomplished using syntax-directed translation, which is jusfitied using direct process decomposition.

### Test Suite
```
make runtest
```
will test the correctness of the repository by iterating through the the test/unit_tests folder and reporting the number of passing or failing tests. This runs through all variants of the translation as well.

### License

The source code for this tool is released under the GNU GPLv2. The ACT files
that provide supporting library functions (the lib/ directory) are released under GNU LGPLv2.1.
