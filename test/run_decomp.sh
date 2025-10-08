#!/bin/sh

(cd loopexc_tests; ./run.sh)
(cd ..)

(cd multichan_tests; ./run.sh)
(cd ..)

(cd decomp_tests; ./run.sh)
(cd ..)

