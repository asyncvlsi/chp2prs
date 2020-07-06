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

if [ -x $ACT_HOME/bin/aflat -a -x $ACT_HOME/bin/prsim ];
then
    if [ -d "unit_tests" ];
    then
        cd "unit_tests"
        
        if [ "1" == "$optimize" ];
        then
            ### run OPTIMIZED unit test
            for i in *
            do
                echo ""
                echo "${bold}$i Optimized Test${normal}"
                
                if [ ! -d $i/"output_opt" ];
                then
                    echo "... creating output_opt/"
                    mkdir "$i/output_opt"
                fi
                if [ ! -f $i/"output_opt/test.prs" ];
                then
                    echo "... creating test.prs"
                    touch "$i/output_opt/test.prs"
                fi
                if [ ! -f $i/"output_opt/prsim.out" ];
                then
                    echo "... creating prsim.out"
                    touch "$i/output_opt/prsim.out"
                fi
                if [ ! -f $i/"output_opt/output.act" ];
                then
                    echo "... creating output_opt.act"
                    touch "$i/output_opt/output.act"
                fi
                
                ### REMOVE TESTING LATER
                if [ -f $i/"output_opt/output.act" ];
                then
                    echo "... REMOVING output_opt/output.act"
                    rm $i/"output_opt/output.act"
                fi
                
                if [ -d $i -a -f $i/"test.act" -a -f $i/"test.prsim" ];
                then
                    # run chp2prs
    #                proc=$(cat "$i/proc.txt")
                    proc="$i<>"
                    echo "... checking chp2prs for proc $proc"
                    if ($ACTTOOL "$i/test.act" $proc "$i/output_opt/output_opt.act" "--optimize");
                    then
                        echo "... optimized chp2prs complete, checking aflat"
                        
                        # check aflat
                        if ($ACT_HOME/bin/aflat "$i/output_opt/output_opt.act" > "$i/output_opt/test.prs");
                        then
                            echo "... aflat complete, checking in prsim"
                            
                          # run prsim on prs
                            (($ACT_HOME/bin/prsim "$i/output_opt/test.prs" < "$i/test.prsim") > "$i/output_opt/prsim.out");
                            if (cat "$i/output_opt/prsim.out" | grep -e "WRONG ASSERT" -e "FATAL" -e "not found")
                            then
                                echo ""
                                echo "==> test ${und}FAILED${normal} prsim running aflat prs."
                                fail=`expr $fail + 1`
                                faildirs="$faildirs ${i}_opt"
                            else
                                echo ""
                                echo "==> test ${und}~PASSED~${normal} prsim running chp2prs prs!"
                            fi
                        else
                            echo ""
                            echo "==> test ${bold}FAILED${normal} in aflat conversion."
                            fail=`expr $fail + 1`
                            faildirs="$faildirs ${i}_opt"
                        fi
                    else
                        echo ""
                        if [ $i == "nochp0" ]
                        then
                          echo "==> test ${und}~PASSED~${normal} -- no chp2prs conversion expected."
                        else
                          echo "==> test ${bold}FAILED${normal} in optimized chp2prs conversion."
                          fail=`expr $fail + 1`
                          faildirs="$faildirs ${i}_opt"
                        fi
                    fi
                else
                    echo "${und}ERROR${normal}: missing test.act or test.prsim in test folder $i"
                    fail=`expr $fail + 1`
                    faildirs="$faildirs ${i}_opt"
                fi
            done
        elif [ "1" == "$bundled" ];
        then
            # run BUNDLED unit test
            for i in *
            do
                echo ""
                echo "${bold}$i Bundled Data Test${normal}"
                
                if [ ! -d $i/"output_bundled" ];
                then
                    echo "... creating output_bundled/"
                    mkdir "$i/output_bundled"
                fi
                if [ ! -f $i/"output_bundled/test.prs" ];
                then
                    echo "... creating output_bundled/test.prs"
                    touch "$i/output_bundled/test.prs"
                fi
                if [ ! -f $i/"output_bundled/prsim.out" ];
                then
                    echo "... creating output_bundled/prsim.out"
                    touch "$i/output_bundled/prsim.out"
                fi
                if [ ! -f $i/"output_bundled/output_bundled.act" ];
                then
                    echo "... creating output_bundled/output_bundled.act"
                    touch "$i/output_bundled/output_bundled.act"
                fi
                
                if [ -d $i -a -f $i/"test.act" -a -f $i/"test.prsim" ];
                then
                    # run chp2prs
                    proc="$i<>"
                    echo "... checking chp2prs for proc $proc"
                    if ($ACTTOOL "$i/test.act" $proc "$i/output_bundled/output_bundled.act" "--bundled");
                    then
                        echo "... bundled chp2prs complete, checking aflat"
                        
                        # check aflat
                        if ( $ACT_HOME/bin/aflat "$i/output_bundled/output_bundled.act" > "$i/output_bundled/test.prs");
                        then
                            echo "... aflat complete, checking in prsim"
                            
                          # run prsim on prs
                            (($ACT_HOME/bin/prsim "$i/output_bundled/test.prs" < "$i/test.prsim") > "$i/output_bundled/prsim.out");
                            if (cat "$i/output_bundled/prsim.out" | grep -e "WRONG ASSERT" -e "FATAL" -e "not found")
                            then
                                echo ""
                                echo "==> test ${und}FAILED${normal} prsim running aflat prs."
                                fail=`expr $fail + 1`
                                faildirs="$faildirs ${i}_bundled"
                            else
                                echo ""
                                echo "==> test ${und}~PASSED~${normal} prsim running chp2prs prs!"
                            fi
                        else
                            echo ""
                            echo "==> test ${bold}FAILED${normal} in aflat conversion."
                            fail=`expr $fail + 1`
                            faildirs="$faildirs ${i}_bundled"
                        fi
                    else
                        echo ""
                        if [ $i == "nochp0" ]
                        then
                          echo "==> test ${und}~PASSED~${normal} -- no chp2prs conversion expected."
                        else
                          echo "==> test ${bold}FAILED${normal} in optimized chp2prs conversion."
                          fail=`expr $fail + 1`
                          faildirs="$faildirs ${i}_bundled"
                        fi
                    fi
                else
                    echo "${und}ERROR${normal}: missing test.act or test.prsim in test folder $i"
                    fail=`expr $fail + 1`
                    faildirs="$faildirs ${i}_bundled"
                fi
            done
        else
            ### loop through regular unit_tests
            for i in *
            do
                echo "=================================================="
                echo "${bold}$i Test${normal}"
                
                ### REMOVE TESTING LATER
                if [ -f $i/"output_bundled/output.act" ];
                then
                    echo "... REMOVING output_bundled/output.act"
                    rm $i/"output_bundled/output.act"
                fi
                            
                ### run regular tests
                if [ ! -d $i/"output" ];
                then
                    echo "... creating output/"
                    mkdir "$i/output"
                fi
                if [ ! -f $i/"output/test.prs" ];
                then
                    echo "... creating test.prs"
                    touch "$i/output/test.prs"
                fi
                if [ ! -f $i/"output/prsim.out" ];
                then
                    echo "... creating prsim.out"
                    touch "$i/output/prsim.out"
                fi
                if [ ! -f $i/"output/output.act" ];
                then
                    echo "... creating output.act"
                    touch "$i/output/output.act"
                fi
                
                if [ -d $i -a -f $i/"test.act" -a -f $i/"test.prsim" ];
                then
                    # run chp2prs
                    proc="$i<>"
                    echo "... checking chp2prs for proc $proc"
                    if ($ACTTOOL "$i/test.act" $proc "$i/output/output.act");
                    then
                        echo "... chp2prs complete, checking aflat"
                        
                        # check aflat
                        if ($ACT_HOME/bin/aflat "$i/output/output.act" > "$i/output/test.prs");
                        then
                            echo "... aflat complete, checking in prsim"
                            
                          # run prsim on prs
                            (($ACT_HOME/bin/prsim "$i/output/test.prs" < "$i/test.prsim") > "$i/output/prsim.out");
                            if (cat "$i/output/prsim.out" | grep -e "WRONG ASSERT" -e "FATAL" -e "not found")
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
                    echo "${und}ERROR${normal}: missing test.act or test.prsim in test folder $i"
                    fail=`expr $fail + 1`
                    faildirs="$faildirs $i"
                fi
            done
        fi
      
        (cd "..");
        
        echo ""
        echo "${bold}*********************************"
        echo "* # FAILED DIRECTORS:${normal} $fail ${bold}*"
        echo "* FAILED DIRECTORIES:${normal}$faildirs ${bold}*"
        echo "*********************************${normal}"
    else
        echo "${bold}Error:${bold} no unit_tests directory."
    fi
else
    echo "${bold}Error:${bold} aflat & prsim necessary for tests."
fi
