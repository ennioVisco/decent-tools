EXEC_NAME=test.native

./$EXEC_NAME -n 1000 -nb_samples 100 -msf 5 -st 100 -dalpha "{a|b|c}" -prt_full true -bernouilli 0.1
./$EXEC_NAME -n 1000 -nb_samples 100 -msf 5 -st 100 -dalpha "{a|b|c}" -prt_full true -bias -bernouilli 0.1
./$EXEC_NAME -n 1000 -nb_samples 100 -abs true -exis true -univ true -prec true -resp true -precc true -respc true -consc true -st 100 -dalpha "{a|b|c}" -prt_full true -bernouilli 0.1
./$EXEC_NAME -n 1000 -nb_samples 100 -abs true -exis true -univ true -prec true -resp true -precc true -respc true -consc true -st 100 -dalpha "{a|b|c}" -prt_full true -bias -bernouilli 0.1

./$EXEC_NAME -n 1000 -nb_samples 100 -msf 5 -st 100 -dalpha "{a|b|c}" -prt_full true -bernouilli 0.1 -only_changes
./$EXEC_NAME -n 1000 -nb_samples 100 -msf 5 -st 100 -dalpha "{a|b|c}" -prt_full true -bias -bernouilli 0.1 -only_changes
./$EXEC_NAME -n 1000 -nb_samples 100 -abs true -exis true -univ true -prec true -resp true -precc true -respc true -consc true -st 100 -dalpha "{a|b|c}" -prt_full true -bernouilli 0.1 -only_changes
./$EXEC_NAME -n 1000 -nb_samples 100 -abs true -exis true -univ true -prec true -resp true -precc true -respc true -consc true -st 100 -dalpha "{a|b|c}" -prt_full true -bias -bernouilli 0.1 -only_changes