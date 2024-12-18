#!/bin/sh

(cd ring_tests; ./run.sh)
(cd ..)
(cd ring_qdi_tests; ./run.sh)
(cd ..)