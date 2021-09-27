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

### Contributors and History

Many have contributed to this implementation over the years, and the history is roughly the following. 
The initial version was hacked together by Rajit Manohar, as a sample solution to a class lab assignment, with support for a small subset of CHP. 
Zeb Mehring took this and turned it into a much more complete implementation (see [the original](https://github.com/zebmehring/ADCO)) as part of his senior project at Yale (~2018).
As part of her senior project, Amanda Hansen took Zeb's version, added many of the tests, and made many significant changes to the generated ACT as well as the library.
In collaboration with Linc Berkeley, she also added certain optimizations to the translation to reduce overhead (~2020).
After several discussions with Marly Roncken and Ebele Esimai who were modifying the tool to support their own flow, the entire implementation was re-factored
in summer/fall 2020. The tool was converted over to the ACT pass framework in 2021.
