#!/bin/bash
# -----------------------
# Run Stata code
# -----------------------
cd stata

mkdir -p out
mkdir -p log

stata -b do code/make_shifty.do

mv *.log log/
mv out/* ../out

rm -rf out

# -----------------------
# Run R code
# -----------------------
cd ..
make -f makefile_r
