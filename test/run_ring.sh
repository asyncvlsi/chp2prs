#!/bin/sh

(cd ring_tests; ./run.sh)
(cd ..)

(cd loopexc_tests; ./run.sh)
(cd ..)

(cd multichan_tests; ./run.sh)
(cd ..)