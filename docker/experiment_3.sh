# The results displayed on the terminal are stored in STATS_RESULTS_ENF.txt

if [ $# -eq 1 ]
then
    OPTIMISTIC=$1
else
    echo "Usage: ./enforcement_1.sh bool"
    echo "The boolean indicates whether or not the experiment is performed with the optimistic mode (true = optimistic, false = pessimistic)"
    exit 0
fi

./decentmon.native -enforce true -n 1000 -st 100 -sf 6 -multi_dalpha alphabets -n_alpha 6 -keep_samples true -optimistic $OPTIMISTIC
