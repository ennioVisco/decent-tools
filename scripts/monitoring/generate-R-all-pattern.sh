#!/bin/bash

# There are warnings in the R scripts (about missing values) because we do not include bexist in the graphs

SCRIPT_DIR=$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )

if [ -z "$1" ];
then
echo "No argument passed. Exiting."
exit 1
fi
if [ ! -f $1 ]; then
"File $1 does not exist. Exiting."
exit 1
fi

cp $1 ${SCRIPT_DIR}
cd ${SCRIPT_DIR}

./generate-R-files-pattern-box.sh $1
./generate-R-files-pattern-crossbar.sh $1
./generate-R-files-pattern-scatter.sh $1