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

echo
echo "*******************************"
echo "*   ${bold}Testing chp2prs library${normal}   *"
echo "*******************************"
echo

if [ -x $ACT_HOME/bin/aflat -a -x $ACT_HOME/bin/prsim ]
then
    if [ -d "unit_tests" ];
    then
        cd "unit_tests"
        
        # loop through tests
        # each test MUST have: test.act, test.prsim, proc.txt in order to run
        for i in *
        do
            echo "========================="
            echo "${bold}Test $i${normal}"
            if [ -d $i -a -f $i/"test.act" -a -f $i/"test.prsim" ];
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
                if [ ! -f $i/"test_final.act" ];
                then
                    echo "... creating test_final.act"
                    touch "$i/test_final.act"
                fi

                # run chp2prs
#                proc=$(cat "$i/proc.txt")
                proc="$i<>"
                echo "... checking chp2prs for proc $proc"
                if ($ACTTOOL "$i/test.act" $proc "$i/test_final.act");
                then
                    echo "... chp2prs complete, checking aflat"
                    
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
                            echo "==> test ${und}~PASSED~${normal} prsim running chp2prs prs!"
                        fi
                    else
                        echo ""
                        echo "==> test ${bold}FAILED${normal} in aflat conversion."
                        fail=`expr $fail + 1`
                        faildirs="$faildirs $i"
                    fi
                else
                    echo ""
                    if [ $i == "nochp0" ]
                    then
                      echo "==> test ${und}~PASSED~${normal} -- no chp2prs conversion expected."
                    else
                      echo "==> test ${bold}FAILED${normal} in chp2prs conversion."
                      fail=`expr $fail + 1`
                      faildirs="$faildirs $i"
                    fi
                fi
            else
    #            echo ""
                echo "${und}ERROR${normal}: missing test.act or test.prsim in test folder $i"
                fail=`expr $fail + 1`
                faildirs="$faildirs $i"
            fi
        done
        (cd "..");
        
        echo ""
        echo "${bold}*********************************"
        echo "* # FAILED DIRECTORS:${normal} $fail ${bold}*"
        echo "* FAILED DIRECTORIES:${normal}$faildirs ${bold}*"
        echo "*********************************${normal}"
        
        
        # repeatedlly prsim on seqgc prs
        echo ""
        echo ""
        echo "${bold}*********************"
        echo "* seqgc repetitions *"
        echo "*********************${normal}"
        i="seqgc"
        has_failed=0
        iter=0
        while [ has_failed=0 -a $iter -lt 100 ]
        do
          (echo "\nTEST $iter\n" >> "$i/prsim.out");
          if (($ACT_HOME/bin/prsim "$i/test.prs" < "$i/test.prsim") >> "$i/prsim.out");
          then
            if (cat "$i/prsim.out" | grep -e "WRONG ASSERT" -e "FATAL" -e "not found" -e "WARNING:");
            then
                echo ""
                echo "${bold}==> test #${iter} ${und}FAILED${normal}"
                has_failed=1
            else
                echo "==> passed test #${iter}"
                iter=`expr $iter + 1`
            fi
          else
            has_failed=1
          fi
        done
        (cd ".."; pwd);
      
        
    else
        echo "${bold}Error:${bold} no unit_tests directory."
    fi
else
    echo "${bold}Error:${bold} aflat & prsim necessary for tests."
fi
