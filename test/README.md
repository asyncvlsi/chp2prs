### Test Suite
Running

```
make runtest
```
will test the correctness of the repository by iterating through the the test/unit_tests folder, and for each test:
* run the chp2prs executable on each `test.act` file
* print the results into an `output/output.act` file
* use `aflat` to convert the output to an `output/test.prs` file
* run the unit test's `test.prsim` script on the production rules, and check the output of the prsim test, copied to `output/prsim.out` for any wrong assertions or fatal errors.


# Unit Test File Structure
-------------------
Each test has a dedicated directory.
In each directory, there are the following files:
[ Necessary Inputs ]
 * test.act = the file to be conerted via the chp2prs executable into prs
 * test.prsim = the prsim file to test the output of the chp2prs prs
[ Outputted Files ]
Outputted files are placed in the output/, output_bundled/, or output_opt/ folder depending on the option passed to the chp2prs compilation
 * output.act = the raw .act outputted by the chp2prs executable
 * test.prs = the production rules created from aflat of test_final.act
 * prsim.out = the stdout from running the prsim test
[ Optional Files ]
 * test_writer.txt = a file used to create the test.prsim for the unit test
 * test_opt.prsim = a prsim file to test the optimized output of the chp2prs prs
