#!/bin/bash

SCRIPT_DIR=$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )
cd ${SCRIPT_DIR}

# We use the same seed on both the optimistic and pessimistic version for each experiment to compare the results on the same formulas/traces

SEED=$(($(echo $RANDOM) % 10000 + 1))

./experiment_1/optimistic/experiment.sh $SEED
./experiment_1/pessimistic/experiment.sh $SEED

SEED=$(($(echo $RANDOM) % 10000 + 1))

./experiment_2/optimistic/experiment.sh $SEED
./experiment_2/pessimistic/experiment.sh $SEED

SEED=$(($(echo $RANDOM) % 10000 + 1))

./experiment_3/optimistic/experiment.sh $SEED
./experiment_3/pessimistic/experiment.sh $SEED
