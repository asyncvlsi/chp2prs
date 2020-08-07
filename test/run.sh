#!/bin/sh

ARCH=`$VLSI_TOOLS_SRC/scripts/getarch`
OS=`$VLSI_TOOLS_SRC/scripts/getos`
EXT=${ARCH}_${OS}
ACTTOOL=../../chp2prs.$EXT

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
function run_test {
    if [ "x$2" = "x" ]
    then
	opt="none"
    else
	opt=$2
    fi
    echo "Testing ${bold}$1 ${normal}(options: $opt)"
    # clear run directory
    if [ -d $1/run ];
    then
	rm -rf $1/run
    fi
    mkdir $1/run
    cat > $1/run/tst.act <<EOF
import "$1/test.act";
$1 tst;
EOF
    $ACTTOOL $1/run/tst.act "$1<>" $1/run/sdt.act
    cat > $1/run/tst2.act <<EOF
import "$1/test.act";
import "$1/run/sdt.act";
sdt_$1 t;
EOF
    if aflat $1/run/tst2.act > $1/run/test.prs
    then
	prsim -r $1/run/test.prs < $1/test.prsim > $1/run/prsim.out
	if egrep '(WRONG|WARNING)' $1/run/prsim.out >/dev/null
	then
	    echo "${bold}*** simulation failed ***${normal}"
	    faildirs="${faildirs} ${1}-sim"
	    failed=1
	fi
    else
	echo "${bold}*** circuit construction failed ***${normal}"
	faildirs="${faildirs} ${1}-ckt"
	failed=1
    fi
    echo
}

echo
echo "*******************************"
echo "*   ${bold}Testing chp2prs library${normal}   *"
echo "*******************************"
echo

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
    if [ -d $i ]
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
