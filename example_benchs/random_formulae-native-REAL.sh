EXEC_NAME=test.native

NB_TESTS_LARGE=1000
NB_TESTS_SMALL=100
SIZE_TRACE_SMALL=5
SIZE_TRACE_LONG=100

./$EXEC_NAME -n $NB_TESTS_LARGE -msf 5 -st $SIZE_TRACE_LONG -dalpha "{a|b|c}" -prt_full true
./$EXEC_NAME -n $NB_TESTS_SMALL -msf 5 -st $SIZE_TRACE_SMALL -dalpha "{a1,a2|b1,b2|c1,c2}" -prt_full true
./$EXEC_NAME -n $NB_TESTS_SMALL -msf 5 -st $SIZE_TRACE_SMALL -dalpha "{a1|a2|b1|b2|c1|c2}" -prt_full true
./$EXEC_NAME -n $NB_TESTS_SMALL -msf 5 -st $SIZE_TRACE_SMALL -dalpha "{a1,a2,a3|b1,b2,a3|c1,c2,c3}" -prt_full true
