#!/bin/sh

echo "Building chp2prs..."
make install_inc
(cd opt; make install_inc)
(cd ring; make install_inc)
(cd decomp; make install_inc)
make "$@" depend && make "$@" && make install
