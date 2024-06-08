# chp2prs: Translation of CHP programs into production rules

[![CircleCI](https://dl.circleci.com/status-badge/img/gh/asyncvlsi/chp2prs/tree/master.svg?style=svg)](https://dl.circleci.com/status-badge/redirect/gh/asyncvlsi/chp2prs/tree/master)

### Usage

```
Usage: [executable] [-OdRbh] [-e <exprfile>] [-o <file>] [-E abc|yosys|genus] [-p <proc>] <actfile>
```

The options are:

   * `-h` : show this message
   * `-O` : run CHP optimizations. Requires the chp-opt package.
   * `-F dataflow|sdt|ring` : select synthesis output format
      * `dataflow` : dataflow output generation
      * `sdt` : syntax-directed translation for prs generation
      * `ring` : ring-based synthesis for prs generation; implies bundled data output
   * `-d` : generate dataflow output [deprecated, use `-F dataflow`])
   * `-R` : synthesize with ring approach [deprecated, use `-F ring`]
   * `-b` : use bundled data datapath for SDT (default is QDI)
   * `-m <int>` : matched delay-line multiplier (in percentage) for ring synthesis. Default is 100 (1x).
   * `-e <exprfile>`: process definitions for each expression evaluation are saved in `<exprfile>`. The default is `expr.act`
   * `-E abc|yosys|genus` : run expression optimization using the specified logic synthesis engine.
   * `-p <proc>` : the name of the ACT process to be translated (the top-level process). This is required.
   * `-o <file>` : where the result should be saved. Default is stdout.
   * `<actfile>` : the input ACT file that contains the design


### Installation

This program is for use with [the ACT toolkit](https://github.com/asyncvlsi/act).

   * Please install the ACT toolkit first; installation instructions are [here](https://github.com/asyncvlsi/act/blob/master/README.md).
   * Install the ACT standard [library](https://github.com/asyncvlsi/stdlib)
   * Run ./configure
   * Build this program using the standard ACT tool install instructions [here](https://github.com/asyncvlsi/act/blob/master/README_tool.md).


### Overview

**XXX: update this!**

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

   * 2017: The initial version was hacked together by Rajit Manohar, as a sample solution to a class lab assignment, with support for a small subset of CHP.
   * 2018: Zeb Mehring took this and turned it into a much more complete implementation (see [the original](https://github.com/zebmehring/ADCO)) as part of his senior project at Yale.
   * 2020: As part of her senior project, Amanda Hansen took Zeb's version, added many of the tests, and made many significant changes to the generated ACT as well as the library.
In collaboration with Linc Berkeley, she also added certain optimizations to the translation to reduce overhead (2020). After several discussions with Marly Roncken and Ebele Esimai who were modifying the tool to support their own flow, the entire implementation was re-factored
in summer/fall 2020.
   * 2021: The tool was converted over to the ACT pass framework. Ole Richter added external expression optimization that invokes existing logic synthesis tools to optimize the logic for expressions.
   * 2021: Linc Berkeley implemented a number of CHP and dataflow optimizations
   * 2022: Henry Heffan designed a cleaner intermediate representation, and moved over and updated the CHP optimizations Linc had implemented into the new data structures.
   * 2023: Rajit integrated all the CHP optimizations into the core synthesis framework and implemented dataflow generation. The entire codebase was re-factored so that different synthesis engines could be added to the framework. Karthi Srinivasan added a new ring-based synthesis approach as an alternative to syntax-directed translation.

