#!/bin/bash

# Script used to only generate the graphs if the data has already been generated
# Otherwise, refer to experiment.sh

SCRIPT_DIR=$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )

cd ${SCRIPT_DIR}/../..

./clean.sh
./generate-R-all-random.sh experiment_2/optimistic/RESULTS_ENF.txt
