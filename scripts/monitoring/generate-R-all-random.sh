#!/bin/bash

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

./generate-R-files-random-box.sh $1
./generate-R-files-random-crossbar.sh $1
./generate-R-files-random-scatter.sh $1