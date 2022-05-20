#!/bin/bash

# Script used to perform the first experiment and generate the associated graphs

SCRIPT_DIR=$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )

cd ${SCRIPT_DIR}

SEED=$(($(echo $RANDOM) % 10000 + 1))

./experiment_3/optimistic/experiment.sh $SEED
./experiment_3/pessimistic/experiment.sh $SEED
