EXEC_NAME=test.native

./$EXEC_NAME -n 500000 -nb_samples 1000 -msf 5 -st 100 -dalpha "{a|b|c}" -prt_full true -keep_samples true -file random-n1000-msf5-st100-abc.txt
./$EXEC_NAME -n 500000 -nb_samples 1000 -msf 5 -st 100 -dalpha "{a|b|c}" -prt_full true -bias -keep_samples true -file random-n1000-msf5-st100-abc-BIAS.txt