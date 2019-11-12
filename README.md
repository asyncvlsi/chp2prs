# chp2act

### Usage

```
Usage: [executable] <actfile> <process> <outfile>
```
Note that `process` should be of the form `"[procname]<>"` to ensure correct execution.

### Overview

A continuation of a project with Rajit Manohar's AVLSI group (see [the original](https://github.com/zebmehring/ADCO)). A program which takes in an `.act` file of the form:
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

### Notes

This program is for use with [the ACT toolkit](https://github.com/asyncvlsi/act), please see the installation instructions [here](http://avlsi.csl.yale.edu/act/doku.php?id=install).

### License

The source code for this tool is released under the GNU GPLv2. The ACT files
that provide supporting library functions (the lib/ directory) are released under GNU LGPLv2.1.
