EXEC_NAME=test.native

NB_TESTS_LARGE=1000
NB_TESTS_SMALL=50
SIZE_TRACE_LONG=100

./$EXEC_NAME -n $NB_TESTS_LARGE -abs true -exis true -univ true -prec true -resp true -precc true -respc true -consc true -st $SIZE_TRACE_LONG -dalpha "{a|b|c}" -prt_full true
./$EXEC_NAME -n $NB_TESTS_SMALL -abs true -exis true -univ true -prec true -resp true -precc true -respc true -consc true -st $SIZE_TRACE_LONG -dalpha "{a1,a2|b1,b2|c1,c2}" -prt_full true
./$EXEC_NAME -n $NB_TESTS_SMALL -abs true -exis true -univ true -prec true -resp true -precc true -respc true -consc true -st $SIZE_TRACE_LONG -dalpha "{a1|a2|b1|b2|c1|c2}" -prt_full true