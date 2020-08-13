#!/bin/sh

bold=$(tput bold)
normal=$(tput sgr0)
und=$(tput smul)
echo
echo "*******************************"
echo "*   ${bold}Testing chp2prs library${normal}   *"
echo "*******************************"
echo

echo "${und}BASELINE MODE${normal}"
echo
./run_basic.sh

echo
echo "${und}BLOCK EXPRESSION MODE${normal}"
echo
./run_expr.sh
