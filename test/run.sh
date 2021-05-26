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

echo
echo "${und}QDI external EXPRESSION syntesis MODE${normal}"

if ! command -v yosys >/dev/null; then
  echo "yosys not found skipping yosys tests"
else
./run_expr_qdiopt.sh yosys
fi

if ! command -v genus >/dev/null; then
  echo "genus not found skipping genus tests"
else
./run_expr_qdiopt.sh genus
fi

echo
echo "${und}BD external EXPRESSION syntesis MODE${normal}"
echo

if ! command -v yosys >/dev/null; then
  echo "yosys not found skipping yosys tests"
else
./run_expr_bdopt.sh yosys
fi

if ! command -v genus >/dev/null; then
  echo "genus not found skipping genus tests"
else
./run_expr_bdopt.sh genus
fi

