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
HEIGHT=480
WIDTH=600
# Options of scatterplots
COLOR_ORCHESTRATION=\"steelblue4\"
COLOR_MIGRATION=\"brown1\"
COLOR_CHOREOGRAPHY=\"palegreen2\"
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

for i in "trace_len,$TR_LAB" "num_mess,$NBMESS_LAB" "size_mess,$SIZEMESS_LAB" "nb_progressions,$NBPROG_LAB";
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
echo "color.orchestration <- $COLOR_ORCHESTRATION" >> $OUTPUT_FILE
echo "color.migration <- $COLOR_MIGRATION" >> $OUTPUT_FILE
echo "color.choreography <- $COLOR_CHOREOGRAPHY" >> $OUTPUT_FILE
### Inject Data Loading
echo "# Load Data" >> $OUTPUT_FILE
echo "data <- read.table(\"$DATA_FILE\", sep= \" \", header = T, as.is = T)" >> $OUTPUT_FILE
###

echo "data <- data[, c(\"x\", \"cent_${METRIC}\", \"decent_${METRIC}\", \"odecent_${METRIC}\")]" >> $OUTPUT_FILE
echo "data_long <- gather(data, condition, measurement, cent_${METRIC}, decent_${METRIC}, odecent_${METRIC})" >> $OUTPUT_FILE
echo "cdata <- ddply(data_long, c(\"x\",\"condition\"), summarise," >> $OUTPUT_FILE
echo "  N    = length(measurement)," >> $OUTPUT_FILE
echo "  mean = mean(measurement)," >> $OUTPUT_FILE
echo "  median = median(measurement)," >> $OUTPUT_FILE
echo "  sd   = sd(measurement)," >> $OUTPUT_FILE
echo "  se   = sd / sqrt(N)" >> $OUTPUT_FILE
echo ")">> $OUTPUT_FILE
echo "pd <- position_dodge(0.5)" >> $OUTPUT_FILE
echo "cols <- c(\"cent_${METRIC}\" = color.orchestration, \"decent_${METRIC}\" = color.migration, \"odecent_${METRIC}\" = color.choreography)" >> $OUTPUT_FILE
echo "ggplot(cdata, aes(x=x, colour=condition)) +" >> $OUTPUT_FILE
echo "  geom_point(position=pd, aes(y = mean))+" >> $OUTPUT_FILE
echo "  geom_point(position=pd, aes(y = median), shape = 4, size = 3)+" >> $OUTPUT_FILE
echo "  scale_x_discrete(limits=c(\"abs\", \"exist\", \"unive\", \"prec\", \"resp\", \"pchain\", \"rchain\", \"cchain\") )+" >> $OUTPUT_FILE
#echo "  geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd, y = mean), width=.4, position=pd) +" >> $OUTPUT_FILE
echo "  geom_crossbar(aes(ymin=mean - 2.575 * se, ymax=mean + 2.575 * se, y = mean), width=.3, position=pd) +" >> $OUTPUT_FILE
echo "  scale_colour_manual(values = cols, breaks = c(\"cent_${METRIC}\", \"decent_${METRIC}\", \"odecent_${METRIC}\"), labels=c(\"orchestration\", \"migration\", \"choreography\")) +" >> $OUTPUT_FILE
echo "  geom_vline(xintercept = 1.5, colour = \"gray\", linetype = \"dashed\") +" >> $OUTPUT_FILE
echo "  geom_vline(xintercept = 2.5, colour = \"gray\", linetype = \"dashed\") +" >> $OUTPUT_FILE
echo "  geom_vline(xintercept = 3.5, colour = \"gray\", linetype = \"dashed\") +" >> $OUTPUT_FILE
echo "  geom_vline(xintercept = 4.5, colour = \"gray\", linetype = \"dashed\") +" >> $OUTPUT_FILE
echo "  geom_vline(xintercept = 5.5, colour = \"gray\", linetype = \"dashed\") +" >> $OUTPUT_FILE
echo "  geom_vline(xintercept = 6.5, colour = \"gray\", linetype = \"dashed\") +" >> $OUTPUT_FILE
echo "  geom_vline(xintercept = 7.5, colour = \"gray\", linetype = \"dashed\") +" >> $OUTPUT_FILE
echo "  theme_bw() +" >> $OUTPUT_FILE
echo "  theme(panel.grid.major = element_blank() ,panel.grid.minor = element_line(colour = \"gray\", linetype = \"dotted\"), legend.title = element_blank(), text=element_text(family=\"Times\"),legend.background = element_rect(fill=alpha(0.4)),legend.key.size = unit(1, \"cm\"), axis.text.x = element_text(size = 14),axis.text.y = element_text(size = 14),legend.text=element_text(size=${LEGEND_FONT_SIZE=15}), legend.position=c(0,1), legend.justification=c(0,0.9), panel.grid.major.x = element_blank(), panel.border = element_rect(size = 2), axis.title.x=element_blank(), axis.title.y=element_blank())" >> $OUTPUT_FILE
echo "ggsave(file = \"$OUTPUT_PDF\", width = 12, height = 6)"  >> $OUTPUT_FILE

### Execute generated R file
Rscript $OUTPUT_FILE
# If a PDF is produced, crop it, and remove the uncropped file
if [ "$PRODUCE_PDF" = true ]; then
pdfcrop $OUTPUT_PDF
rm $OUTPUT_PDF
fi
done