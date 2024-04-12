library(readxl)
library(writexl)
library(dplyr)
library(data.table)
library(ggplot2)
library(tidyr)
library(cowplot)
library(png)
library(grid)
library(gridExtra) 
library(magick)
require(devtools)

val <- list('asimpc01' = c("A" = "#b6c5cc", "B" = "#aa3929", "C" = "#f8a31b", "D" = "#e2c59f"), #https://www.color-hex.com/
            'sexcd' = c("F" = "#ffbc79", "M" = "#a3cce9"),
            'nligr1'= c("C01 - C04" = "#4f6980", "C05 - C08" = "#849db1", "T01 - T04" = "#a2ceaa", "T05 - T08" = "#638b66", "T09 - T12" =  "#d7ce9f"),
            'bmi_status' = c("Underweight" = "#bfbb60", "Healthy weight" = "#f47942", "Overweight" = "#fbb04e", "Obesity" = "#b66353"),
            'tx1_r' = c("P" = "#ee905d", "D1" = "#c86754", "D2" = "#a42919"),
            'cluster' = c("1" = "#DDCC77", "2" = "#117733", "3" = "#44AA99", "4" = "#999933", "5" = "#6699CC", "6" = "#888888"))
ncl <- c('lems' = 3, 'uems' = 4, 'ltscore' = 2, 'ppscore' = 2, 'modben' = 2)
label <- c('lems' = 'Lower Extremity \n Motor Score', 'uems' = 'Upper Extremity \n Motor Score', 'ltscore' = 'Light Touch \n Total Score', 'ppscore' = 'Pinprick \n Total Score', 'modben' = 'Modified \n Benzel Scale', 'age' = 'Age [years]', 'sexcd' = 'Sex', 'asimpc01' = 'AIS Grade', 'nligr1' = 'Neurological Level \n of Injury at Baseline', 'bmi_status' = 'Weight Status', 'tx1_r' = 'Treatment Group', 'cluster' = 'Cluster')
data <- bind_rows(lapply(c('lems', 'uems', 'ltscore', 'ppscore', 'modben'), function(outcome){
  dat <- read_excel(paste0("C:\\Users\\LAURA\\Documents\\ETHZ\\Master\\Master thesis\\SCI project\\code - final version - 2\\Main analysis\\results - 2\\data_imputed_", outcome, "_renamed2.xlsx"))
  dat$cluster <- dat[[paste0("kmeans-clusters-", ncl[outcome])]]
  dat <- dat[, c('age', 'asimpc01', 'sexcd', 'nligr1', 'bmi_status', 'tx1_r', 'cluster')]
  reshaped_dat <- bind_rows(lapply(c('age', 'asimpc01', 'sexcd', 'nligr1', 'bmi_status', 'tx1_r'), function(var){
    subre <- dat[, c(var, 'cluster')]
    names(subre) <- c("Value", "Cluster")
    subre$Variable <- label[var]
    subre$Value <- as.character(subre$Value)
    subre$Cluster <- as.character(subre$Cluster)
    subre[, c("Variable", "Value", "Cluster")]
  }))
  reshaped_dat$outcome <- label[outcome]
  reshaped_dat
}))

detach("package:ggplot2", unload=TRUE) 
install.packages("ggplot2")
library(ggplot2)

# AGE (X = AGE, Y = DENSITY --> DISTPLOT)
dat_age <- data[data$Variable == 'Age [years]',]
dat_age$Variable <- 'Age \n [years]'
dat_age$Value <- as.numeric(dat_age$Value)
dat_age$outcome <- factor(dat_age$outcome, levels = c('Lower Extremity \n Motor Score', 'Upper Extremity \n Motor Score', 'Light Touch \n Total Score', 'Pinprick \n Total Score', 'Modified \n Benzel Scale'))
p_age <- ggplot(data = dat_age, mapping = aes(x = Value, color = Cluster, fill = Cluster)) + geom_density(alpha = 0.3) + scale_fill_manual(values = val[['cluster']][1:4]) + scale_color_manual(values = val[['cluster']][1:4]) + theme_cowplot() + facet_grid(outcome ~ Variable) + labs(y = NULL, x = NULL) + theme(legend.position = 'none', plot.margin = unit(c(0, 0, 0.1, 0), "cm"), text = element_text(size = 23), axis.line = element_line(size = 1), axis.ticks = element_line(size = 1), strip.text.x = element_text(size = 23), axis.text = element_text(size = 18), strip.background.y = element_blank(), strip.text.y = element_blank()) #+ ylim(0,1)

# CATEGORICAL VARIABLES (X = VAR, Y = PROPORTION OF CLUSTER --> STACKED BAR)
dat_cat <- data[data$Variable != 'Age [years]',]
agg_cat_1 <- aggregate(dat_cat$Cluster, list(dat_cat$Value, dat_cat$Variable, dat_cat$Cluster, dat_cat$outcome), length)
names(agg_cat_1) <- c("Value", "Variable", "Cluster", "outcome", "Counts_clust")
agg_cat_2 <- aggregate(dat_cat$Cluster, list(dat_cat$Variable, dat_cat$Value, dat_cat$outcome), length)
names(agg_cat_2) <- c("Variable", "Value", "outcome", "Counts_var")
agg_cat <- merge(agg_cat_1, agg_cat_2, by = c("Variable", "Value", "outcome"))
agg_cat$Prop <- agg_cat$Counts_clust/agg_cat$Counts_var
agg_cat$outcome <- factor(agg_cat$outcome, levels = c('Lower Extremity \n Motor Score', 'Upper Extremity \n Motor Score', 'Light Touch \n Total Score', 'Pinprick \n Total Score', 'Modified \n Benzel Scale'))
p_cat <- ggplot(data = agg_cat, mapping = aes(x = Value, y = Prop, fill = as.factor(Cluster), color = as.factor(Cluster))) + geom_bar(stat="identity") + scale_fill_manual(values = c("1" = "#DDCC77", "2" = "#117733", "3" = "#44AA99", "4" = "#999933", "5" = "#6699CC", "6" = "#888888")) + scale_color_manual(values = c("1" = "#DDCC77", "2" = "#117733", "3" = "#44AA99", "4" = "#999933", "5" = "#6699CC", "6" = "#888888"))  + theme_cowplot()  + facet_grid(outcome ~ Variable, scales = "free", space = "free") + labs(y = NULL, x = NULL, color = "Cluster", fill = "Cluster") + theme( plot.margin = unit(c(0, 0, 0, 0), "cm"), text = element_text(size = 23), axis.line = element_line(size = 1), axis.ticks = element_line(size = 1), axis.text = element_text(size = 18), strip.text = element_text(size = 23), legend.title = element_text(size=23), legend.text = element_text(size=18), legend.position="bottom", axis.text.x = element_text(size = 18, angle =90, vjust = 0.5, hjust=1))

# Combine
png(file="C:\\Users\\LAURA\\Documents\\ETHZ\\Master\\Master thesis\\SCI project\\code - final version - 2\\Main analysis\\results - 2\\plot_groups_distr.png" ,width=1200, height=1200)
p_cat
dev.off()
