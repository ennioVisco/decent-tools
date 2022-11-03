#!/bin/bash 

DATE=$(date +%F_%H-%M-%S) 

# time = 1000
#CMD=`dune exec decent -- -n 50000 -nb_samples 1000 -msf 50 -st 1000 -dalpha "{a|b|c}" -prt_full true -keep_samples true -seed 3 -file $DATE\_output.log -abs true -exis true -bexis true -univ true -prec true -resp true -precc true -respc true -consc true`

# time = 100
CMD=`dune exec decent -- -n 500000 -nb_samples 1000 -msf 50 -st 100 -dalpha "{a|b|c}" -prt_full true -keep_samples true -seed 3 -file $DATE\_output.log -abs true -exis true -bexis true -univ true -prec true -resp true -precc true -respc true -consc true`

# time = 100; formula length = 15
#CMD=`dune exec decent -- -n 500000 -nb_samples 1000 -msf 50 -st 100 -dalpha "{a|b|c}" -prt_full true -keep_samples true -seed 3 -file $DATE\_output.log -abs true -exis true -bexis true -univ true -prec true -resp true -precc true -respc true -consc true`

eval "$CMD"
