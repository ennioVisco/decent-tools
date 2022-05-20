#!/bin/bash

# Script used to perform the first experiment and generate the associated graphs

SCRIPT_DIR=$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )

cd ${SCRIPT_DIR}

if [ $# -eq 0 ]
then
    SEED=0
else
    SEED=$1
fi


rm *.txt

cd ../../../..

make decentmon
./decentmon.native -enforce true -n 1000 -st 100 -abs true -univ true -exis true -bexis true -prec true -resp true -precc true -respc true -consc true -dalpha "{a1,a2|b1,b2|c1,c2}" -keep_samples true -prod_tex true -seed $SEED -optimistic true

mv -t ${SCRIPT_DIR} *.txt

cd scripts/enforcement

./clean.sh
./generate-R-all-pattern.sh experiment_2/optimistic/RESULTS_ENF.txt

mv -t ${SCRIPT_DIR} *.pdf