#!/bin/bash

SCRIPT_DIR=$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )
cd ${SCRIPT_DIR}

rm RESULTS_ENF.txt Rplots.pdf
./experiment_1/optimistic/clean.sh
./experiment_1/pessimistic/clean.sh
./experiment_2/optimistic/clean.sh
./experiment_2/pessimistic/clean.sh