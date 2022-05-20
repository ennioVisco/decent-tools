#!/bin/bash

SCRIPT_DIR=$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )
cd ${SCRIPT_DIR}

./experiment_1/optimistic/gen_graphs.sh
./experiment_1/pessimistic/gen_graphs.sh
./experiment_2/optimistic/gen_graphs.sh
./experiment_2/pessimistic/gen_graphs.sh