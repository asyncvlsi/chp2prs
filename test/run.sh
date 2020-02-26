#!/bin/sh

echo
echo "*******************************"
echo "*   Testing chp2prs library   *"
echo "*******************************"
echo

fail=0
faildirs=""
proc=""

if [ -d "unit_tests" ];
then
    cd "unit_tests"
    for i in *
    do
        echo "========================="
        echo "Test $i:"
        if [ -d $i -a -f $i/"test.act" -a -f $i/"test.prsim" -a -f $i/"proc.txt" ];
        then
            if [ -f $i/"test.prs" ];
            then
            :
            else
                echo "... creating test.prs"
                touch "$i/test.prs"
            fi
            if [ -f $i/"test.out" ];
            then
            :
            else
                echo "... creating test.out"
                touch "$i/test.out"
            fi
            if [ -f $i/"test_out.act" ];
            then
            :
            else
                echo "... creating test_out.act"
                touch "$i/test_out.act"
            fi
            proc=$(cat "$i/proc.txt")
            echo "... checking chp2prs for proc $proc"
            if (../../chp2prs.i386_darwin19_2_0 "$i/test.act" $proc "$i/test_out.act");
            then
                echo "... chp2prs complete, checking aflat"
                if ($ACT_HOME/bin/aflat "$i/test_out.act" > "$i/test.prs");
                then
                    echo "... aflat passes, checking prsim"
                    (($ACT_HOME/bin/prsim "$i/test.prs" < "$i/test.prsim") > "$i/test.out");
                    if (cat "$i/test.out" | grep "WRONG ASSERT")
                    then
                        echo ""
                        echo "==> test FAILED prsim running aflat prs."
                        fail=`expr $fail + 1`
                        faildirs="$i $faildirs"
                    else
                        if [ $(wc -l "$i/test.out") -gt 0 ];
                        then
                            echo ""
                            echo "==> test ~PASSED~ prsim running chp2prs prs!"
                        else
                            echo ""
                            echo "==> test FAILED prsim test output empty."
                            fail=`expr $fail + 1`
                            faildirs="$i $faildirs"
                        fi
                    fi
                else
                    echo ""
                    echo "==> test FAILED in aflat conversion."
                    fail=`expr $fail + 1`
                    faildirs="$i $faildirs"
                fi
            else
                echo ""
                echo "==> test FAILED in chp2prs conversion."
                fail=`expr $fail + 1`
                faildirs="$i $faildirs"
            fi
        else
            echo ""
            echo "==> Error: missing .act or .prsim in test folder $i"
            fail=`expr $fail + 1`
            faildirs="$i $faildirs"
        fi
    done
    (cd "..");
    
    echo ""
    echo "*******************************"
    echo "* Number of directories that failed: $fail"
    echo "* Directories: $faildirs"
    echo "*******************************"
else
    echo "Error: no unit_tests directory."
fi

# for i in *
# do
# done

