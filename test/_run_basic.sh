#!/bin/sh

ARCH=`$ACT_HOME/scripts/getarch`
OS=`$ACT_HOME/scripts/getos`
EXT=${ARCH}_${OS}
if [ ! x$ACT_TEST_INSTALL = x ] || [ ! -f ../chp2prs.$EXT ]; then
  ACTTOOL=$ACT_HOME/bin/chp2prs
  echo "testing installation"
echo
else
  ACTTOOL=../../chp2prs.$EXT
fi

fail=0
faildirs=""
proc=""
bold=$(tput bold)
normal=$(tput sgr0)
und=$(tput smul)

faildirs=""
failed=0

#
#  run_test name [option]
#
run_test () {
    echo "Testing ${bold}$1 ${normal}(options: none)"
    # clear run directory
    if [ -d $1/run ];
    then
	rm -rf $1/run
    fi
    mkdir $1/run
    $ACTTOOL $1/test.act "$1" $1/run/sdt.act
    cat > $1/run/tst.act <<EOF
import "$1/run/sdt.act";
sdt_$1 t;
EOF
    cp init.prs $1/run/test.prs
    if aflat -ref=1 $1/run/tst.act >> $1/run/test.prs
    then
	cat init_qdi.prsim $1/test.prsim | prsim -r $1/run/test.prs > $1/run/prsim.out
	if egrep '(WRONG|WARNING|Node)' $1/run/prsim.out >/dev/null
	then
	    echo "${bold}*** simulation failed ***${normal}"
	    faildirs="${faildirs} ${1}-sim"
	    failed=1
        if [ ! x$ACT_TEST_VERBOSE = x ]; then
            cat $1/run/prsim.out
        fi
	    echo	
	fi
    else
	echo "${bold}*** circuit construction failed ***${normal}"
	faildirs="${faildirs} ${1}-ckt"
	failed=1
    if [ ! x$ACT_TEST_VERBOSE = x ]; then
            cat $1/run/sdt.act
            cat $1/run/test.prs
    fi
    echo
    fi
}

if [ ! -x $ACT_HOME/bin/aflat -o ! -x $ACT_HOME/bin/prsim ];
then
    echo "${bold}Error:${bold} aflat & prsim necessary for tests."
    exit 1
fi

if [ ! -d "unit_tests" ];
then
    echo "${bold}Error:${bold} no unit_tests directory."
    exit 1
fi    

cd "unit_tests"

for i in *
do
    if [ -d $i -a -f $i/test.act ]
    then
	run_test $i
    fi
done

if [ $failed -eq 1 ]
then
    echo ""
    echo "${bold}*********************************"
    echo "* FAILED DIRECTORIES:${normal}$faildirs ${bold}*"
    echo "*********************************${normal}"
fi

exit $failed
