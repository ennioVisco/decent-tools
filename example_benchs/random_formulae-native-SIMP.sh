EXEC_NAME=decent.native

NB_TESTS_SMALL=1000
NB_SAMPLES_SMALL=10
SIZE_TRACE_SMALL=50

./$EXEC_NAME -n $NB_TESTS_SMALL -nb_samples $NB_SAMPLES_SMALL -msf 5 -st $SIZE_TRACE_SMALL -dalpha "{a|b|c}" -prt_full true -keep_samples true -file random-${NB_SAMPLES_SMALL}-msf5-st${SIZE_TRACE_SMALL}-abc.txt
./$EXEC_NAME -n $NB_TESTS_SMALL -nb_samples $NB_SAMPLES_SMALL -msf 5 -st $SIZE_TRACE_SMALL -dalpha "{a1,a2|b1,b2|c1,c2}" -prt_full true -keep_samples true -file random-${NB_SAMPLES_SMALL}-msf5-st${SIZE_TRACE_SMALL}-a1a2b1b2c1c2.txt
