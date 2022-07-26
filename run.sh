#!/bin/bash 

DATE=$(date +%F_%H-%M-%S) 

# time = 1000
#CMD=`dune exec decent -- -n 50000 -nb_samples 1000 -msf 50 -st 1000 -dalpha "{a|b|c}" -prt_full true -keep_samples true -seed 3 -file $DATE\_output.log -abs true -exis true -bexis true -univ true -prec true -resp true -precc true -respc true -consc true`

# time = 100
CMD=`dune exec decent -- -n 500000 -nb_samples 1000 -msf 50 -st 100 -dalpha "{a|b|c}" -prt_full true -keep_samples true -seed 3 -file $DATE\_output.log -abs true -exis true -bexis true -univ true -prec true -resp true -precc true -respc true -consc true`

# time = 100; formula length = 15
#CMD=`dune exec decent -- -n 500000 -nb_samples 1000 -msf 50 -st 100 -dalpha "{a|b|c}" -prt_full true -keep_samples true -seed 3 -file $DATE\_output.log -abs true -exis true -bexis true -univ true -prec true -resp true -precc true -respc true -consc true`

##################### EXPERIMENT BATCH #####################

# time = 100; formula length = 15 - abs
#CMD=`dune exec decent -- -n 500000 -nb_samples 1000 -msf 50 -st 100 -dalpha "{a|b|c}" -prt_full true -keep_samples true -seed 3 -file $DATE\_output.log -abs true -exis false -bexis false -univ false -prec false -resp false -precc false -respc false -consc false`

# time = 100; formula length = 15 - exis
#CMD=`dune exec decent -- -n 500000 -nb_samples 1000 -msf 50 -st 100 -dalpha "{a|b|c}" -prt_full true -keep_samples true -seed 3 -file $DATE\_output.log -abs false -exis true -bexis false -univ false -prec false -resp false -precc false -respc false -consc false`

# time = 100; formula length = 15 - bexis
#CMD=`dune exec decent -- -n 500000 -nb_samples 1000 -msf 50 -st 100 -dalpha "{a|b|c}" -prt_full true -keep_samples true -seed 3 -file $DATE\_output.log -abs false -exis false -bexis true -univ false -prec false -resp false -precc false -respc false -consc false`

# time = 100; formula length = 15 - univ
#CMD=`dune exec decent -- -n 500000 -nb_samples 1000 -msf 50 -st 100 -dalpha "{a|b|c}" -prt_full true -keep_samples true -seed 3 -file $DATE\_output.log -abs false -exis false -bexis false -univ true -prec false -resp false -precc false -respc false -consc false`

# time = 100; formula length = 15 - prec
#CMD=`dune exec decent -- -n 500000 -nb_samples 1000 -msf 50 -st 100 -dalpha "{a|b|c}" -prt_full true -keep_samples true -seed 3 -file $DATE\_output.log -abs false -exis false -bexis false -univ false -prec true -resp false -precc false -respc false -consc false`

# time = 100; formula length = 15 - resp
#CMD=`dune exec decent -- -n 500000 -nb_samples 1000 -msf 50 -st 100 -dalpha "{a|b|c}" -prt_full true -keep_samples true -seed 3 -file $DATE\_output.log -abs false -exis false -bexis false -univ false -prec false -resp true -precc false -respc false -consc false`

# time = 100; formula length = 15 - precc
#CMD=`dune exec decent -- -n 500000 -nb_samples 1000 -msf 50 -st 100 -dalpha "{a|b|c}" -prt_full true -keep_samples true -seed 3 -file $DATE\_output.log -abs false -exis false -bexis false -univ false -prec false -resp false -precc true -respc false -consc false`

# time = 100; formula length = 15 - respc
#CMD=`dune exec decent -- -n 500000 -nb_samples 1000 -msf 50 -st 100 -dalpha "{a|b|c}" -prt_full true -keep_samples true -seed 3 -file $DATE\_output.log -abs false -exis false -bexis false -univ false -prec false -resp false -precc true -respc true -consc false`

# time = 100; formula length = 15 - consc
#CMD=`dune exec decent -- -n 500000 -nb_samples 1000 -msf 50 -st 100 -dalpha "{a|b|c}" -prt_full true -keep_samples true -seed 3 -file $DATE\_output.log -abs false -exis false -bexis false -univ false -prec false -resp false -precc true -respc false -consc true`

eval "$CMD"
