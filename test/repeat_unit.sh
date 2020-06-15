# repeatedly run prsim on one unit_test's PRs
ARCH=`$VLSI_TOOLS_SRC/scripts/getarch`
OS=`$VLSI_TOOLS_SRC/scripts/getos`
EXT=${ARCH}_${OS}
ACTTOOL=../../chp2prs.$EXT
bold=$(tput bold)
normal=$(tput sgr0)
und=$(tput smul)
i=${unit}
proc="$i<>"

echo ""
echo "${bold}******************************************"
echo "* ${i} repetitions , show warnings=${warning}*"
echo "******************************************${normal}"

# chp2prs conversion
cd "unit_tests"
echo "${bold}... checking chp2prs for proc $proc{normal}"
if ($ACTTOOL "$i/test.act" $proc "$i/test_final.act");
then
    echo "${bold}... chp2prs complete, checking aflat${normal}"
    
    # check aflat
    if ($ACT_HOME/bin/aflat "$i/test_final.act" > "$i/test.prs");
    then
        echo "${bold}... aflat complete, checking in prsim${normal}"
        
      # run prsim on prs
        (($ACT_HOME/bin/prsim "$i/test.prs" < "$i/test.prsim") > "$i/prsim.out");
        if (cat "$i/prsim.out" | grep -e "WRONG ASSERT" -e "FATAL" -e "not found")
        then
            echo "FAILED in first prsim"
        else
            
            # show warnings setting
            if [ "${warning}" = "1" ];
            then
              echo "${bold}Exit on prsim warnings turned ON${normal}"
            else
              echo "${bold}Exit on prsim warnings turned OFF${normal}"
            fi
            echo ""
            
            # begin prsim test loop
            has_failed=0
            iter=0
            echo "${bold}using random_seed${normal}"
            echo "\nusing random_seed" >> "$i/prsim.out"
            while [ has_failed=0 ]
            do
                # write prsim test file with random seed appended to top of file
                (echo "random_seed $iter \nrandom" > "$i/test_rand.prsim");
                (cat "$i/test.prsim" >> "$i/test_rand.prsim");
                (echo "\nTEST $iter\n" >> "$i/prsim.out");
                
                # validate prsim output
                if (($ACT_HOME/bin/prsim "$i/test.prs" < "$i/test_rand.prsim") >> "$i/prsim.out");
                then
                    if [ "${warning}" = "1" ];
                    then
                      if (cat "$i/prsim.out" | grep -e "WRONG ASSERT" -e "FATAL" -e "not found" -e "WARNING:");
                      then
                          echo "${bold}==> test #${iter} ${und}FAILED in prsim${normal}"
                          has_failed=1
                          exit 0
                      else
                          echo "==> passed test #${iter}"
                          iter=$(($iter + 1))
                      fi
                    else
                      if (cat "$i/prsim.out" | grep -e "WRONG ASSERT" -e "FATAL" -e "not found");
                      then
                          echo "${bold}==> test #${iter} ${und}FAILED in prsim${normal}"
                          has_failed=1
                          exit 0
                      else
                          echo "==> passed test #${iter}"
                          iter=$(($iter + 1))
                      fi
                    fi
                else
                    has_failed=1
                    exit 0
                fi
            done
            (cd ".."; pwd);
            exit 1
        fi
    else
        echo "FAILED in aflat conversion"
    fi
else
    echo "FAILED in chp2prs conversion"
fi
