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
./decentmon.native -enforce true -n 1000 -st 100 -msf 6 -dalpha "{a1,a2|b1,b2|c1,c2}" -keep_samples true -prod_tex true -seed $SEED

mv -t ${SCRIPT_DIR} *.txt

cd scripts/enforcement

./clean.sh
./generate-R-all-random.sh experiment_1/pessimistic/RESULTS_ENF.txt

mv -t ${SCRIPT_DIR} *.pdf