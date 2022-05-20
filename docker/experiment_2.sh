# The results displayed on the terminal are stored in STATS_RESULTS_ENF.txt

if [ $# -eq 1 ]
then
    OPTIMISTIC=$1
else
    echo "Usage: ./enforcement_1.sh bool"
    echo "The boolean indicates whether or not the experiment is performed with the optimistic mode (true = optimistic, false = pessimistic)"
    exit 0
fi

./decent.native -enforce true -n 1000 -st 100 -abs true -univ true -exis true -bexis true -prec true -resp true -precc true -respc true -consc true -dalpha "{a1,a2|b1,b2|c1,c2}" -keep_samples true -optimistic $OPTIMISTIC
