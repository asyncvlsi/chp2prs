#!/bin/sh

file=$1

fail=0

# 
# Run dataflow mapper
#
dflowmap -ref=1 -p df_testproc -o ${file}_out runs/${file}_df.act > runs/${file}_df.out 2>&1  || fail=1

if [ $fail -eq 1 ]
then
	exit 1
fi

export ACT_PATH=runs/${file}_out

actsim -Wlang_subst:off ${file}tst.act test <<EOF > runs/${file}_sim.out 2>&1
cycle
EOF
if grep SUCCESS runs/${file}_sim.out >/dev/null 2>&1
then
	exit 0
else
	exit 1
fi
