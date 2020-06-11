# repeatedly run prsim on seqgc prs
ARCH=`$VLSI_TOOLS_SRC/scripts/getarch`
OS=`$VLSI_TOOLS_SRC/scripts/getos`
EXT=${ARCH}_${OS}
ACTTOOL=../../chp2prs.$EXT
bold=$(tput bold)
normal=$(tput sgr0)
und=$(tput smul)
i="seqgc"
proc="$i<>"

echo ""
echo "${bold}*********************"
echo "* seqgc repetitions *"
echo "*********************${normal}"

cd "unit_tests"
pwd

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
            echo "FAILED in first prsim"
        else
            has_failed=0
            iter=0
            echo "using random_seed"
            echo "\nusing random_seed" >> "$i/prsim.out"
            while [ has_failed=0 -a $iter -lt 15000 ]
            do
                (echo "random_seed $iter \nrandom" > "$i/test_rand.prsim");
                (cat "$i/test.prsim" >> "$i/test_rand.prsim");
                (echo "\nTEST $iter\n" >> "$i/prsim.out");
                if (($ACT_HOME/bin/prsim "$i/test.prs" < "$i/test_rand.prsim") >> "$i/prsim.out");
                then
                    if (cat "$i/prsim.out" | grep -e "WRONG ASSERT" -e "FATAL" -e "not found" -e "WARNING:");
                    then
                        echo ""
                        echo "${bold}==> test #${iter} ${und}FAILED${normal}"
                        has_failed=1
                    else
                        echo "==> passed test #${iter}"
                        if [ $iter -gt 99 ];
                        then
                          iter=`expr $iter + 100`
                        else
                          iter=`expr $iter + 2`
                        fi                    fi
                else
                    has_failed=1
                fi
            done
            max=0
            iter=0
            echo "using random with max as multiplied from iter"
            echo "\nusing random with max as multiplied from iter" >> "$i/prsim.out"
            while [ has_failed=0 -a $iter -lt 15000 ]
            do
                max=$(($iter * 2))
                (echo "random $iter $max" > "$i/test_rand.prsim");
                (cat "$i/test.prsim" >> "$i/test_rand.prsim");
                (echo "\nTEST $iter\n" >> "$i/prsim.out");
                if (($ACT_HOME/bin/prsim "$i/test.prs" < "$i/test_rand.prsim") >> "$i/prsim.out");
                then
                    if (cat "$i/prsim.out" | grep -e "WRONG ASSERT" -e "FATAL" -e "not found" -e "WARNING:");
                    then
                        echo ""
                        echo "${bold}==> test #${iter} ${und}FAILED${normal}"
                        has_failed=1
                    else
                        echo "==> passed test #${iter}"
                        if [ $iter -gt 99 ];
                        then
                          iter=`expr $iter + 100`
                        else
                          iter=`expr $iter + 2`
                        fi
                    fi
                else
                    has_failed=1
                fi
            done
            iter=0
            echo ""
            echo "using random with max as constant added to iter"
            echo "\nusing random with max as constant added to iter" >> "$i/prsim.out"
            while [ has_failed=0 -a $iter -lt 15000 ]
            do
                max=$(($iter + 50))
                (echo "random $iter $max" > "$i/test_rand.prsim");
                (cat "$i/test.prsim" >> "$i/test_rand.prsim");
                (echo "\nTEST $iter\n" >> "$i/prsim.out");
                if (($ACT_HOME/bin/prsim "$i/test.prs" < "$i/test_rand.prsim") >> "$i/prsim.out");
                then
                    if (cat "$i/prsim.out" | grep -e "WRONG ASSERT" -e "FATAL" -e "not found" -e "WARNING:");
                    then
                        echo ""
                        echo "${bold}==> test #${iter} ${und}FAILED${normal}"
                        has_failed=1
                    else
                        echo "==> passed test #${iter}"
                        if [ $iter -gt 99 ];
                        then
                          iter=`expr $iter + 100`
                        else
                          iter=`expr $iter + 2`
                        fi
                     fi
                else
                    has_failed=1
                fi
            done
            (cd ".."; pwd);
        fi
    else
        echo "FAILED in aflat conversion"
    fi
else
    echo "FAILED in chp2prs conversion"
fi
