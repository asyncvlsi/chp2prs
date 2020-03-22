#!/bin/sh

fail=0
faildirs=""
proc=""
bold=$(tput bold)
normal=$(tput sgr0)
und=$(tput smul)

echo
echo "*******************************"
echo "*   ${bold}Testing chp2prs library${normal}   *"
echo "*******************************"
echo

if [ -d "unit_tests" ];
then
    cd "unit_tests"
    
    # loop through tests
    # each test MUST have: test.act, test.prsim, proc.txt in order to run
    for i in *
    do
        echo "========================="
        echo "${bold}Test $i${normal}"
        if [ -d $i -a -f $i/"test.act" -a -f $i/"proc.txt" ];
        then
            if [ ! -f $i/"test.prs" ];
            then
                echo "... creating test.prs"
                touch "$i/test.prs"
            fi
            if [ ! -f $i/"prsim.out" ];
            then
                echo "... creating prsim.out"
                touch "$i/prsim.out"
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
            if [ ! -f $i/"test.prsim" ];
            then
                if [ -f $i/"test_writer.txt" ]
                then
                    ($ACT_HOME/bin/test_writer "$i/test_writer.txt" "$i/test.prsim" --prsim --reset --_reset);
                fi
            fi

            # run chp2prs
            proc=$(cat "$i/proc.txt")
            echo "... checking chp2prs for proc $proc"
            if (../../chp2prs.i386_darwin19_2_0 "$i/test.act" $proc "$i/test_out.act");
            then
                echo "... chp2prs complete, adding vars"
#                (grep -e "import" "$i/test_out.act" > "$i/test_final.act"); # syn.act line
                (cat "$i/vars.txt" > "$i/test_final.act");
                (tail -n +3 "$i/test_out.act" >> "$i/test_final.act");
                echo "... vars complete, checking aflat"
                
                # check aflat
                if ($ACT_HOME/bin/aflat "$i/test_final.act" > "$i/test.prs");
                then
                    echo "... aflat complete, checking in prsim"
                    
                  # run prsim on prs
                    (($ACT_HOME/bin/prsim "$i/test.prs" < "$i/test.prsim") > "$i/prsim.out");
                    if (cat "$i/prsim.out" | grep -e "WRONG ASSERT" -e "FATAL" -e "not found")
                    then
                        echo ""
                        echo "==> test ${und}FAILED${normal} prsim running aflat prs."
                        fail=`expr $fail + 1`
                        faildirs="$faildirs $i"
                    else
                        echo ""
                        echo "==> test ${und}~PASSED~${normal} prsim running chp2prs prs!!!"
                    fi
                else
                    echo ""
                    echo "==> test ${und}FAILED${normal} in aflat conversion."
                    fail=`expr $fail + 1`
                    faildirs="$faildirs $i"
                fi
            else
                echo ""
                echo "==> test ${und}FAILED${normal} in chp2prs conversion."
                fail=`expr $fail + 1`
                faildirs="$faildirs $i"
            fi
        else
#            echo ""
            echo "${und}ERROR${normal}: missing test.act, test.prsim, or proc.txt in test folder $i"
            fail=`expr $fail + 1`
            faildirs="$faildirs $i"
        fi
    done
    (cd "..");
    
    echo ""
    echo "*******************************"
    echo "* ${bold}# FAILED DIRECTORS:${normal} $fail"
    echo "* ${bold}FAILED DIRECTORIES:${normal}$faildirs"
    echo "*******************************"
else
    echo "Error: no unit_tests directory."
fi
