#!/bin/sh

echo
echo "************************************************************************"
echo "*                      Testing loop excision                           *"
echo "************************************************************************"
echo

if [ "x$ACT_HOME_SANDBOX" = x ]
then
	ACT_HOME_SANDBOX=$ACT_HOME_SANDBOX
fi

ARCH=`$ACT_HOME_SANDBOX/scripts/getarch`
OS=`$ACT_HOME_SANDBOX/scripts/getos`
EXT=${ARCH}_${OS}
if [ ! x$ACT_TEST_INSTALL = x ] || [ ! -f ../../synth2.$EXT ]; then
  ACTTOOL=$ACT_HOME_SANDBOX/bin/synth2
  echo "testing installation"
  echo
else
  ACTTOOL=../../synth2.$EXT
fi

check_echo=0
myecho()
{
  if [ $check_echo -eq 0 ]
  then
        check_echo=1
        count=`echo -n "" | wc -c | awk '{print $1}'`
        if [ $count -gt 0 ]
        then
                check_echo=2
        fi
  fi
  if [ $check_echo -eq 1 ]
  then
        echo -n "$@"
  else
        echo "$@\c"
  fi
}


fail=0

if [ ! -d runs ]
then
        mkdir runs
fi


myecho " "
num=0
count=0
lim=10
while [ -f ${count}tst.act ]
do
	orig=${count}
        i=${count}.act
        count=`expr $count + 1`
        bname=`expr $i : '\(.*\).act'`
        num=`expr $num + 1`
        if [ $bname -lt 10 ]
        then
           myecho ".[0$bname]"
        else
           myecho ".[$bname]"
        fi
        ok=1
        for opt in "-F decomp"
	do
        if [ $ok -eq 1 ]
        then
	$ACTTOOL $opt -E abc -e runs/${orig}_expr.act -p le${orig} $i > runs/${orig}decomp.act  2>runs/$i.t.stderr
        if test -s runs/$i.t.stderr 
        then
                echo 
                myecho "** FAILED LOOP EXCISION, $opt TEST $i: stderr"
                fail=`expr $fail + 1`
                ok=0
                if [ ! x$ACT_TEST_VERBOSE = x ]; then
                        cat runs/$i.t.stderr
                fi
        fi
	if [ $ok -eq 1 ]
	then
		./run_test.sh $orig
		code=$?
		if [ $code -ne 0 ]
		then
			if [ $code -eq 2 ]
			then
				myecho "** FAILED LOOP EXCISION, TEST $i"
			else
				myecho "** FAILED LOOP EXCISION SIMULATION, $opt TEST $i"
			fi
			fail=`expr $fail + 1`
			ok=0
		fi	
	fi
        fi
        done
        if [ $ok -eq 1 ]
        then
                if [ $num -eq $lim ]
                then
                        echo 
                        myecho " "
                        num=0
                fi
        else
                echo " **"
                myecho " "
                num=0
        fi
done

if [ $num -ne 0 ]
then
        echo
fi


if [ $fail -ne 0 ]
then
        if [ $fail -eq 1 ]
        then
                echo "--- Summary: 1 test failed ---"
        else
                echo "--- Summary: $fail tests failed ---"
        fi
        exit 1
else
        echo
        echo "SUCCESS! All tests passed."
fi
echo
