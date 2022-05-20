EXEC_NAME=test.native

./$EXEC_NAME -n 10000 -nb_samples 1000 -msf 5 -st 100 -dalpha "{a|b|c|d|e}" -prt_full true
./$EXEC_NAME -n 10000 -msf 5 -st 100 -dalpha "{a|b|c|d|e}" -prt_full true -bias
./$EXEC_NAME -n 10000 -nb_samples 1000 -abs true -exis true -univ true -prec true -resp true -precc true -respc true -consc true -st 100 -dalpha "{a|b|cLd|e}" -prt_full true
./$EXEC_NAME -n 10000 -nb_samples 1000 -abs true -exis true -univ true -prec true -resp true -precc true -respc true -consc true -st 100 -dalpha "{a|b|c|d|e}" -prt_full true -bias

./$EXEC_NAME -n 1000 -nb_samples 100 -msf 5 -st 100 -dalpha "{a|b|c|d|e|f|g}" -prt_full true
./$EXEC_NAME -n 1000 -nb_samples 100 -msf 5 -st 100 -dalpha "{a|b|c|d|e|f|g}" -prt_full true -bias
./$EXEC_NAME -n 1000 -nb_samples 100 -abs true -exis true -univ true -prec true -resp true -precc true -respc true -consc true -st 100 -dalpha "{a|b|c|d|e|f|g}" -prt_full true
./$EXEC_NAME -n 1000 -nb_samples 100 -abs true -exis true -univ true -prec true -resp true -precc true -respc true -consc true -st 100 -dalpha "{a|b|c|d|e|f|g}" -prt_full true -bias