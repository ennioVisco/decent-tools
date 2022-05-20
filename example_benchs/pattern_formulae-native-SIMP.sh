EXEC_NAME=decentmon.native

NB_TESTS_SMALL=1000
NB_SAMPLES_SMALL=10
SIZE_TRACE=100

./$EXEC_NAME -n $NB_TESTS_SMALL -nb_samples $NB_SAMPLES_SMALL -abs true -exis true -univ true -prec true -resp true -precc true -respc true -consc true -st $SIZE_TRACE -dalpha "{a|b|c}" -prt_full true -keep_samples true -file pattern-${NB_SAMPLES_SMALL}-msf5-st100-abc.txt
./$EXEC_NAME -n $NB_TESTS_SMALL -nb_samples $NB_SAMPLES_SMALL -abs true -exis true -univ true -prec true -resp true -precc true -respc true -consc true -st $SIZE_TRACE -dalpha "{a1,a2|b1,b2|c1,c2}" -prt_full true -keep_samples true -file pattern-${NB_SAMPLES_SMALL}-msf5-st100-a1a2b1b2c1c2.txt
