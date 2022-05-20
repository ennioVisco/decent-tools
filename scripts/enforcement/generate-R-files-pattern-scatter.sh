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
HEIGHT=400
WIDTH=700
# Options of boxplots
COLOR_CENTRALIZED=\"steelblue4\"
COLOR_GLOBAL=\"brown1\"
COLOR_LOCAL=\"palegreen2\"
ALPHA=0.5
LEGEND_FONT_SIZE=17
X_LABELS_FONT_SIZE=14
Y_LABELS_FONT_SIZE=14
JITTER_WIDTH=0.3
# Maximum y value plotted (can be max, hinges, whiskers)
X_LAB="\"pattern\""
AXIS_TILE_POSITION="2"
AXIS_LABEL_POSITION=".7"
AXIS_LINE_POSITION="0"
LEGEND_X=0.1
LEGEND_Y=0.9
SIZE_LEGEND=9
LEGEND_FONT_SIZE=15
LEGEND_POSITION=none
LEGEND_TEXT=
TR_LAB="\"trace length (|tr|)\""
NBMESS_LAB="\"number of messages (#msg)\""
SIZEMESS_LAB="\"size of messages (|msg|)\""
NBPROG_LAB="\"number of progressions (#prog)\""

for scale in "normal" "log";
do
for i in "nb_modif,$TR_LAB" "nb_msg,$NBMESS_LAB" "msg_size,$SIZEMESS_LAB" "tcl_size,$NBPROG_LAB";
do
    METRIC=${i%,*};
    METRIC_TEXT=${i#*,};
    OUTPUT_FILE="${SCRIPT_DIR}/${DATA_FILE_BASE}_${METRIC}_scatter_${scale}.R"
    OUTPUT_PDF="${SCRIPT_DIR}/${DATA_FILE_BASE}_${METRIC}_scatter_${scale}.pdf"
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
    echo "data <- data[, c(\"x\", \"${METRIC}_cent\", \"${METRIC}_g\", \"${METRIC}_l\")]" >> $OUTPUT_FILE
    echo "data_long <- gather(data, condition, measurement, ${METRIC}_cent, ${METRIC}_g, ${METRIC}_l)" >> $OUTPUT_FILE
    echo "cols <- c(\"${METRIC}_cent\" = color.centralized, \"${METRIC}_g\" = color.global, \"${METRIC}_l\" = color.local)" >> $OUTPUT_FILE

    ###
    echo "ggplot(data_long, aes(x=x, colour=condition)) +" >> $OUTPUT_FILE
    echo "  geom_point(aes(y = measurement), position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.9))+" >> $OUTPUT_FILE
    echo "  scale_x_discrete(limits=c(\"abs\", \"exist\", \"unive\", \"prec\", \"resp\", \"pchain\", \"rchain\", \"cchain\") )+" >> $OUTPUT_FILE
    echo "  scale_colour_manual(values = cols, breaks = c(\"${METRIC}_cent\", \"${METRIC}_g\", \"${METRIC}_l\"), labels=c(\"centralized\", \"global\", \"local\")) +" >> $OUTPUT_FILE
    echo "  theme_bw() +" >> $OUTPUT_FILE
    if [ ${scale} = "log" ]; then
    echo "  scale_y_continuous(trans='log10') +" >> $OUTPUT_FILE
    fi
    echo "  guides(colour = guide_legend(override.aes = list(size=10))) +" >> $OUTPUT_FILE
    echo "  geom_vline(xintercept = 1.5, colour = \"gray\", linetype = \"dashed\") +" >> $OUTPUT_FILE
    echo "  geom_vline(xintercept = 2.5, colour = \"gray\", linetype = \"dashed\") +" >> $OUTPUT_FILE
    echo "  geom_vline(xintercept = 3.5, colour = \"gray\", linetype = \"dashed\") +" >> $OUTPUT_FILE
    echo "  geom_vline(xintercept = 4.5, colour = \"gray\", linetype = \"dashed\") +" >> $OUTPUT_FILE
    echo "  geom_vline(xintercept = 5.5, colour = \"gray\", linetype = \"dashed\") +" >> $OUTPUT_FILE
    echo "  geom_vline(xintercept = 6.5, colour = \"gray\", linetype = \"dashed\") +" >> $OUTPUT_FILE
    echo "  geom_vline(xintercept = 7.5, colour = \"gray\", linetype = \"dashed\") +" >> $OUTPUT_FILE
    echo "  theme(panel.grid.major = element_blank() , panel.grid.minor = element_line(colour = \"gray\", linetype = \"dotted\"), legend.title = element_blank(), text=element_text(family=\"Times\"),legend.background = element_rect(fill=alpha(0.4)),legend.key.size = unit(1, \"cm\"), axis.text.x = element_text(size = 14),axis.text.y = element_text(size = 14),legend.text=element_text(size=${LEGEND_FONT_SIZE}), legend.position=c(0,1), legend.justification=c(0,0.9), panel.border = element_rect(size = 2), axis.title.x=element_blank(), axis.title.y=element_blank())" >> $OUTPUT_FILE
    echo "ggsave(file = \"$OUTPUT_PDF\", width = 12, height = 6)"  >> $OUTPUT_FILE
    ### Execute generated R file
    Rscript $OUTPUT_FILE
    # If a PDF is produced, crop it
    if [ "$PRODUCE_PDF" = true ]; then
        pdfcrop $OUTPUT_PDF
        rm $OUTPUT_PDF
    fi
done
done