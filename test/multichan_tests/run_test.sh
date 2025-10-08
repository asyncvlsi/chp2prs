#!/bin/sh

REF="-ref=1"
# REF=

file=$1

fail=0

export ACT_PATH=runs

actsim $REF ${file}tst.act test <<EOF > runs/${file}_sim.out 2>&1
cycle
EOF
if grep SUCCESS runs/${file}_sim.out >/dev/null 2>&1
then
	exit 0
else
	exit 1
fi
