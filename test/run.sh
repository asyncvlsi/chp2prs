#!/bin/sh

echo
echo "*******************************"
echo "*   Testing chp2prs library   *"
echo "*******************************"
echo

fail=0
faildirs=""
proc=""
linecount=0

if [ -d "unit_tests" ];
then
    cd "unit_tests"
    for i in *
    do
        echo "========================="
        echo "Test $i:"
        if [ -d $i -a -f $i/"test.act" -a -f $i/"test.prsim" -a -f $i/"proc.txt" ];
        then
            if [ ! -f $i/"test.prs" ];
            then
                echo "... creating test.prs"
                touch "$i/test.prs"
            fi
            if [ ! -f $i/"test.out" ];
            then
                echo "... creating test.out"
                touch "$i/test.out"
            fi
            if [ ! -f $i/"test_out.act" ];
            then
                echo "... creating test_out.act"
                touch "$i/test_out.act"
            fi
            if [ ! -f $i/"test_final.act" ];
            then
                echo "... creating test_final.act"
                touch "$i/test_final.act"
            fi
            if [ ! -f $i/"vars.txt" ];
            then
                echo "... creating vars.txt"
                touch "$i/vars.txt"
            fi
            proc=$(cat "$i/proc.txt")
            echo "... checking chp2prs for proc $proc"
            if (../../chp2prs.i386_darwin19_2_0 "$i/test.act" $proc "$i/test_out.act");
            then
                echo "... chp2prs complete, adding vars"
                (grep -e "import" "$i/test_out.act" > "$i/test_final.act");
                (cat "$i/vars.txt" >> "$i/test_final.act");
                (tail -n +3 "$i/test_out.act" >> "$i/test_final.act");
                echo "... vars complete, checking aflat"
                if ($ACT_HOME/bin/aflat "$i/test_final.act" > "$i/test.prs");
                then
                    echo "... aflat passes, checking prsim"
                    (($ACT_HOME/bin/prsim "$i/test.prs" < "$i/test.prsim") > "$i/test.out");
                    if (cat "$i/test.out" | grep -e "WRONG ASSERT" -e "FATAL" -e "not found")
                    then
                        echo ""
                        echo "==> test FAILED prsim running aflat prs."
                        fail=`expr $fail + 1`
                        faildirs="$i $faildirs"
                    else
#                        if [ $(wc -l "$i/test.out") > 0 ];
#                        if ( diff "$i/test.out" "empty.txt" )
#                        then
                        echo ""
                        echo "==> test ~PASSED~ prsim running chp2prs prs!"
#                        else
#                            echo ""
#                            echo "==> test FAILED prsim test output empty."
#                            fail=`expr $fail + 1`
#                            faildirs="$i $faildirs"
#                        fi
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

