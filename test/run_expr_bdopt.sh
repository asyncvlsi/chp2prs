#!/bin/sh

ARCH=`$VLSI_TOOLS_SRC/scripts/getarch`
OS=`$VLSI_TOOLS_SRC/scripts/getos`
EXT=${ARCH}_${OS}
ACTTOOL=../../chp2prs.$EXT

if ! $ACT_HOME/scripts/findpkg -i expropt
then
	echo "Expression optimization package not installed"
	exit 0
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
    echo "Testing ${bold}$1 ${normal}(options: -b -o $2 -e $1/run/expr.act)"
    # clear run directory
    if [ -d $1/run ];
    then
	rm -rf $1/run
    fi
    mkdir $1/run
    $ACTTOOL -o $2 -b -e$1/run/expr.act $1/test.act "$1" $1/run/sdt.act 
    cat > $1/run/tst.act <<EOF
import "$1/run/sdt.act";
sdt_$1 t;
EOF
    cp init.prs $1/run/test.prs
    if aflat $1/run/tst.act >> $1/run/test.prs
    then
	cat init_bd.prsim $1/test.prsim | prsim -r $1/run/test.prs > $1/run/prsim.out
    # removed warning to ignore X caused by hazards
    #if egrep '(WRONG|WARNING|Node)' $1/run/prsim.out >/dev/null
	if egrep '(WRONG|Node)' $1/run/prsim.out >/dev/null
	then
	    echo "${bold}*** simulation failed ***${normal}"
	    faildirs="${faildirs} ${1}-sim"
	    failed=1
	    echo	
	fi
    else
	echo "${bold}*** circuit construction failed ***${normal}"
	faildirs="${faildirs} ${1}-ckt"
	failed=1
        echo
    fi
    echo
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

if [ -z $2 ];
then
for i in *
do
    if [ -d $i -a -f $i/test.act ]
    then
        if [ -z $1 ]; then
            run_test $i yosys
        else
            run_test $i $1
        fi
    fi
done
else
    if [ -z $1 ]; then
        run_test $2 yosys
    else
        run_test $2 $1
    fi
fi

if [ $failed -eq 1 ]
then
    echo ""
    echo "${bold}*********************************"
    echo "* FAILED DIRECTORIES:${normal}$faildirs ${bold}*"
    echo "*********************************${normal}"
fi

exit $failed
