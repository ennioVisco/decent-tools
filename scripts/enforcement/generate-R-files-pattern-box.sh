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
COLOR_CENTRALIZED=\"steelblue4\"
COLOR_GLOBAL=\"brown1\"
COLOR_LOCAL=\"palegreen2\"
ALPHA=1
LEGEND_FONT_SIZE=15
X_LABELS_FONT_SIZE=14
Y_LABELS_FONT_SIZE=14
JITTER_WIDTH=0.2
X_LAB="expression( paste (\"|\", varphi, \"|\"))"
AXIS_TILE_POSITION="2"
AXIS_LABEL_POSITION=".7"
AXIS_LINE_POSITION="0"
LEGEND_X=0.1
LEGEND_Y=0.9
LEGEND_POSITION=none
LEGEND_TEXT=
TR_LAB="\"trace length (|tr|)\""
NBMESS_LAB="\"number of messages (#msg)\""
SIZEMESS_LAB="\"size of messages (|msg|)\""
NBPROG_LAB="\"number of progressions (#prog)\""


for i in "nb_modif,$TR_LAB" "nb_msg,$NBMESS_LAB" "msg_size,$SIZEMESS_LAB" "tcl_size,$NBPROG_LAB";
do
METRIC=${i%,*};
METRIC_TEXT=${i#*,};
OUTPUT_FILE="${SCRIPT_DIR}/${DATA_FILE_BASE}_${METRIC}_box.R"
OUTPUT_PDF="${SCRIPT_DIR}/${DATA_FILE_BASE}_${METRIC}_box.pdf"
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
for cpt in abs exist unive prec resp pchain rchain cchain;
do
echo "data.cent.$cpt <- boxplot.stats((subset(data, x == '$cpt'))\$${METRIC}_cent)" >> $OUTPUT_FILE
echo "data.global.$cpt <- boxplot.stats(subset(data, x == '$cpt')\$${METRIC}_g)" >> $OUTPUT_FILE
echo "data.local.$cpt <- boxplot.stats(subset(data, x == '$cpt')\$${METRIC}_l)" >> $OUTPUT_FILE
done
echo "y.max.cent <- max (data.cent.abs\$stats[5], data.cent.exist\$stats[5], data.cent.unive\$stats[5], data.cent.prec\$stats[5], data.cent.resp\$stats[5], data.cent.pchain\$stats[5], data.cent.rchain\$stats[5], data.cent.cchain\$stats[5])" >> $OUTPUT_FILE
echo "y.max.global <- max (data.global.abs\$stats[5], data.global.exist\$stats[5], data.global.unive\$stats[5], data.global.prec\$stats[5], data.global.resp\$stats[5], data.global.pchain\$stats[5], data.global.rchain\$stats[5], data.global.cchain\$stats[5])" >> $OUTPUT_FILE
echo "y.max.local <- max (data.local.abs\$stats[5], data.local.exist\$stats[5], data.local.unive\$stats[5], data.local.prec\$stats[5], data.local.resp\$stats[5], data.local.pchain\$stats[5], data.local.rchain\$stats[5], data.local.cchain\$stats[5])" >> $OUTPUT_FILE
echo "y.min.cent <- min (data.cent.abs\$stats[1], data.cent.exist\$stats[1], data.cent.unive\$stats[1], data.cent.prec\$stats[1], data.cent.resp\$stats[1], data.cent.pchain\$stats[1], data.cent.rchain\$stats[1], data.cent.cchain\$stats[1])" >> $OUTPUT_FILE
echo "y.min.global <- min (data.global.abs\$stats[1], data.global.exist\$stats[1], data.global.unive\$stats[1], data.global.prec\$stats[1], data.global.resp\$stats[1], data.global.pchain\$stats[1], data.global.rchain\$stats[1], data.global.cchain\$stats[1])" >> $OUTPUT_FILE
echo "y.min.local <- min (data.local.abs\$stats[1], data.local.exist\$stats[1], data.local.unive\$stats[1], data.local.prec\$stats[1], data.local.resp\$stats[1], data.local.pchain\$stats[1], data.local.rchain\$stats[1], data.local.cchain\$stats[1])" >> $OUTPUT_FILE


echo "y.min <- min (y.min.cent, y.min.global, y.min.local)" >> $OUTPUT_FILE
echo "y.max <- max (y.max.cent, y.max.global, y.max.local)" >> $OUTPUT_FILE

echo "data_long <- gather(data, condition, measurement, ${METRIC}_cent, ${METRIC}_g, ${METRIC}_l)" >> $OUTPUT_FILE
echo "cols <- c(\"${METRIC}_cent\" = color.centralized, \"${METRIC}_g\" = color.global, \"${METRIC}_l\" = color.local)" >> $OUTPUT_FILE

###
echo "ggplot(data_long, aes(x=x, fill=condition)) +" >> $OUTPUT_FILE
echo "  geom_boxplot(aes(x = factor(x), y = measurement), position=position_dodge(1), outlier.color = NA)+" >> $OUTPUT_FILE
echo "  scale_x_discrete(limits=c(\"abs\", \"exist\", \"unive\", \"prec\", \"resp\", \"pchain\", \"rchain\", \"cchain\") )+" >> $OUTPUT_FILE
echo "  scale_fill_manual(values = cols, breaks = c(\"${METRIC}_cent\", \"${METRIC}_g\", \"${METRIC}_l\"), labels=c(\"centralized\", \"global\", \"local\")) +" >> $OUTPUT_FILE
echo "  theme_bw() +" >> $OUTPUT_FILE
echo "  coord_cartesian(y = c(y.min, y.max)) +" >> $OUTPUT_FILE
echo "  guides(colour = guide_legend(override.aes = list(size=10))) +" >> $OUTPUT_FILE
echo "  geom_vline(xintercept = 1.5, colour = \"gray\", linetype = \"dashed\") +" >> $OUTPUT_FILE
echo "  geom_vline(xintercept = 2.5, colour = \"gray\", linetype = \"dashed\") +" >> $OUTPUT_FILE
echo "  geom_vline(xintercept = 3.5, colour = \"gray\", linetype = \"dashed\") +" >> $OUTPUT_FILE
echo "  geom_vline(xintercept = 4.5, colour = \"gray\", linetype = \"dashed\") +" >> $OUTPUT_FILE
echo "  geom_vline(xintercept = 5.5, colour = \"gray\", linetype = \"dashed\") +" >> $OUTPUT_FILE
echo "  geom_vline(xintercept = 6.5, colour = \"gray\", linetype = \"dashed\") +" >> $OUTPUT_FILE
echo "  geom_vline(xintercept = 7.5, colour = \"gray\", linetype = \"dashed\") +" >> $OUTPUT_FILE
echo "  theme(panel.grid.major = element_blank(), panel.grid.minor = element_line(colour = \"gray\", linetype = \"dotted\"), legend.title = element_blank(), text=element_text(family=\"Times\"),legend.background = element_rect(fill=alpha(0.4)),legend.key.size = unit(1, \"cm\"), axis.text.x = element_text(size = 14),axis.text.y = element_text(size = 14),legend.text=element_text(size=${LEGEND_FONT_SIZE}), legend.position=c(0,1), legend.justification=c(0,0.9), panel.border = element_rect(size = 2), axis.title.x=element_blank(), axis.title.y=element_blank())" >> $OUTPUT_FILE
echo "ggsave(file = \"$OUTPUT_PDF\", width = 12, height = 6)"  >> $OUTPUT_FILE


### Execute generated R file
Rscript $OUTPUT_FILE
# If a PDF is produced, crop it, and remove the uncropped file
if [ "$PRODUCE_PDF" = true ]; then
pdfcrop $OUTPUT_PDF
rm $OUTPUT_PDF
fi
done