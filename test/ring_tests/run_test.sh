#!/bin/sh

REF="-ref=1"
#REF=

file=$1

fail=0

export ACT_PATH=runs

actsim $REF ${file}tst.act test <<EOF > runs/${file}_sim.out 2>&1
mode reset
set Reset 1
set Vdd 1
set GND 0
cycle
mode run
set Reset 0
step 50000
EOF
if grep SUCCESS runs/${file}_sim.out >/dev/null 2>&1
then
	exit 0
else
	exit 1
fi
