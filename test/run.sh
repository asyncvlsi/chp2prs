#!/bin/sh

bold=$(tput bold)
normal=$(tput sgr0)
und=$(tput smul)
echo
echo "*******************************"
echo "*   ${bold}Testing chp2prs library${normal}   *"
echo "*******************************"
echo

echo
echo "${und}BASELINE MODE${normal}"
echo
./run_expr.sh || exit 1

echo
echo "${und}QDI external EXPRESSION syntesis MODE${normal}"

if ! command -v yosys >/dev/null; then
  echo "yosys not found skipping yosys tests"
else
./run_expr_qdiopt.sh yosys || exit 1
fi

if ! command -v genus >/dev/null; then
  echo "genus not found skipping genus tests"
else
./run_expr_qdiopt.sh genus || exit 1
fi

echo
echo "${und}BD external EXPRESSION syntesis MODE${normal}"
echo

if ! command -v yosys >/dev/null; then
  echo "yosys not found skipping yosys tests"
else
./run_expr_bdopt.sh yosys || exit 1
fi

if ! command -v genus >/dev/null; then
  echo "genus not found skipping genus tests"
else
./run_expr_bdopt.sh genus || exit 1
fi

if [ `grep FOUND_abc ../config_pkg.h | wc -l` -eq 0 ]
then
	echo "abc not found; skipping abc tests"
else
	./run_expr_bdoptabc.sh || exit 1
fi
