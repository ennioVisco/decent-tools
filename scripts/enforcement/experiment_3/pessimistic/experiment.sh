#!/bin/bash

# Script used to perform the third experiment

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

make decent
./decent.native -enforce true -n 1000 -st 100 -sf 6 -multi_dalpha scripts/enforcement/experiment_3/alphabets -n_alpha 6 -keep_samples true -prod_tex true -seed $SEED

mv -t ${SCRIPT_DIR} *.txt