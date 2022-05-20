SCRIPT_DIR=$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )

if [ -z "$1" ];
then
    echo "No argument passed. Exiting."
    exit 1
fi
if [ ! -f $1 ]; then
    "File $1 does not exist. Exiting."
    exit 1
fi
EXECUTE=true
PRODUCE_PDF=true
DATA_FILE="$1"
DATA_FILE_BASE="${DATA_FILE%.*}"
DATA_FILE_EXTENSION=".txt"
# Display options
Y_COEF=1.1
STYLE="\"quantile\""
HEIGHT=120
WIDTH=300
# Options of scatterplots
COLOR_CENTRALIZED=\"steelblue4\"
COLOR_GLOBAL=\"brown1\"
COLOR_LOCAL=\"palegreen2\"
ALPHA=1 
LEGEND_FONT_SIZE=17
X_LABELS_FONT_SIZE=14
Y_LABELS_FONT_SIZE=14
JITTER_WIDTH=0.2
X_LAB="expression( paste (\"|\", varphi, \"|\"))"
AXIS_TILE_POSITION="2"
AXIS_LABEL_POSITION=".7"
AXIS_LINE_POSITION="0"
LEGEND_X=0.1
LEGEND_Y=0.9
LEGEND_FONT_SIZE=15
LEGEND_POSITION=none
LEGEND_TEXT=
TR_LAB="\"trace length (|tr|)\""
NBMESS_LAB="\"number of messages (#msg)\""
SIZEMESS_LAB="\"size of messages (|msg|)\""
NBPROG_LAB="\"number of progressions (#prog)\""

for i in "nb_modif,$TR_LAB" "nb_modif,$NBMESS_LAB" "msg_size,$SIZEMESS_LAB" "tcl_size,$NBPROG_LAB";
do
    METRIC=${i%,*};
    METRIC_TEXT=${i#*,};
    OUTPUT_FILE="${SCRIPT_DIR}/${DATA_FILE_BASE}_${METRIC}_crossbar.R"
    OUTPUT_PDF="${SCRIPT_DIR}/${DATA_FILE_BASE}_${METRIC}_crossbar.pdf"
    if [ -f $OUTPUT_FILE ]; then
    rm $OUTPUT_FILE;
    touch $OUTPUT_FILE
    fi
    ## Inject library call
    echo "library(ggplot2)" >> $OUTPUT_FILE
    echo "library(tidyr)" >> $OUTPUT_FILE
    echo "library(plyr)" >> $OUTPUT_FILE
    ### Inject Cleaning of Environment
    echo "#Cleaning environment" >> $OUTPUT_FILE
    echo "rm(list=ls(all=TRUE))" >> $OUTPUT_FILE
    ### Inject global settings
    echo "# Global Settings" >> $OUTPUT_FILE
    echo "color.centralized <- $COLOR_CENTRALIZED" >> $OUTPUT_FILE
    echo "color.global <- $COLOR_GLOBAL" >> $OUTPUT_FILE
    echo "color.local <- $COLOR_LOCAL" >> $OUTPUT_FILE
    ### Inject Data Loading
    echo "# Load Data" >> $OUTPUT_FILE
    echo "data <- read.table(\"$DATA_FILE\", sep= \" \", header = T, as.is = T)" >> $OUTPUT_FILE
    ###

    echo "data <- data[, c(\"x\", \"${METRIC}_cent\", \"${METRIC}_g\", \"${METRIC}_l\")]" >> $OUTPUT_FILE
    echo "data_long <- gather(data, condition, measurement, ${METRIC}_cent, ${METRIC}_g, ${METRIC}_l)" >> $OUTPUT_FILE
    echo "cdata <- ddply(data_long, c(\"x\",\"condition\"), summarise," >> $OUTPUT_FILE
    echo "N    = length(measurement)," >> $OUTPUT_FILE
    echo "mean = mean(measurement)," >> $OUTPUT_FILE
    echo "median = median(measurement)," >> $OUTPUT_FILE
    echo "sd   = sd(measurement)," >> $OUTPUT_FILE
    echo "se   = sd / sqrt(N)" >> $OUTPUT_FILE
    echo ")" >> $OUTPUT_FILE
    echo "pd <- position_dodge(0.5)" >> $OUTPUT_FILE
    echo "cols <- c(\"${METRIC}_cent\" = color.centralized, \"${METRIC}_g\" = color.global, \"${METRIC}_l\" = color.local)" >> $OUTPUT_FILE
    echo "ggplot(cdata, aes(x=x, colour=condition)) +" >> $OUTPUT_FILE
    echo "  geom_point(position=pd, aes(y = mean))+" >> $OUTPUT_FILE
    echo "  geom_point(position=pd, aes(y = median), shape = 4, size = 3)+" >> $OUTPUT_FILE
#    echo "  geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd, y = mean), width=.4, position=pd) +" >> $OUTPUT_FILE
    echo "  geom_crossbar(aes(ymin=mean - 2.575 * se, ymax=mean + 2.575 * se, y = mean), width=.3, position=pd) +" >> $OUTPUT_FILE
    echo "  scale_colour_manual(values = cols, breaks = c(\"${METRIC}_cent\", \"${METRIC}_g\", \"${METRIC}_l\"), labels=c(\"centralized\", \"global\", \"local\")) +" >> $OUTPUT_FILE
    echo "  theme_bw() +" >> $OUTPUT_FILE
    echo "  theme(panel.grid.minor.y = element_line(colour = \"gray\", linetype = \"dotted\"),panel.grid.minor.x = element_line(colour = \"gray\", linetype = \"dashed\"), legend.title = element_blank(), text=element_text(family=\"Times\"),legend.background = element_rect(fill=alpha(0.4)),legend.key.size = unit(1, \"cm\"), axis.text.x = element_text(size = ${LEGEND_FONT_SIZE}),axis.text.y = element_text(size = 14),legend.text=element_text(size=$LEGEND_FONT_SIZE), legend.position=c(0,1), legend.justification=c(0,0.9), panel.grid.major.x = element_blank(), panel.border = element_rect(size = 2), axis.title.x=element_blank(), axis.title.y=element_blank())" >> $OUTPUT_FILE
    echo "ggsave(file = \"$OUTPUT_PDF\", width = 5, height = 3.5)"  >> $OUTPUT_FILE

    ### Execute generated R file
    Rscript $OUTPUT_FILE
    # If a PDF is produced, crop it, and remove the uncropped file
    if [ "$PRODUCE_PDF" = true ]; then
    pdfcrop $OUTPUT_PDF
    rm $OUTPUT_PDF
    fi
done