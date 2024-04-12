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

# AGE (X = CLUSTER, Y = AGE --> BOX)
dat_age <- data[data$Variable == 'Age [years]',]
dat_age$Variable <- 'Age \n [years]'
dat_age$Value <- as.numeric(dat_age$Value)
dat_age$outcome <- factor(dat_age$outcome, levels = c('Lower Extremity \n Motor Score', 'Upper Extremity \n Motor Score', 'Light Touch \n Total Score', 'Pinprick \n Total Score', 'Modified \n Benzel Scale'))
p_age <- ggplot(data = dat_age, mapping = aes(x = Cluster, y = Value)) + geom_boxplot(fill = "#eaeaea") + theme_cowplot() + facet_grid(Variable ~ outcome, scales = "free_x", space = "free_x") + labs(y = NULL) + theme(legend.position = 'none', axis.title.x = element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank(), axis.line.x = element_blank(), plot.margin = unit(c(0, 0, 0.1, 0.52), "cm"), text = element_text(size = 23), axis.line.y = element_line(size = 1), axis.ticks.y = element_line(size = 1), strip.text = element_text(size = 23), axis.text.y = element_text(size = 18)) 

# CATEGORICAL VARIABLES (X = CLUSTER, Y = PROPORTION OF VAR --> STACKED BAR)
dat_cat <- data[data$Variable != 'Age [years]',]
agg_cat_1 <- aggregate(dat_cat$Value, list(dat_cat$Value, dat_cat$Variable, dat_cat$Cluster, dat_cat$outcome), length)
names(agg_cat_1) <- c("Value", "Variable", "Cluster", "outcome", "Counts_val")
agg_cat_2 <- aggregate(dat_cat$Value, list(dat_cat$Variable, dat_cat$Cluster, dat_cat$outcome), length)
names(agg_cat_2) <- c("Variable", "Cluster", "outcome", "Counts_clust")
agg_cat <- merge(agg_cat_1, agg_cat_2, by = c("Variable", "Cluster", "outcome"))
agg_cat$Prop <- agg_cat$Counts_val/agg_cat$Counts_clust
agg_cat$outcome <- factor(agg_cat$outcome, levels = c('Lower Extremity \n Motor Score', 'Upper Extremity \n Motor Score', 'Light Touch \n Total Score', 'Pinprick \n Total Score', 'Modified \n Benzel Scale'))
val2 <- val
names(val2) <- NULL
val2 <- unlist(val2)
p_cat <- ggplot(data = agg_cat, mapping = aes(x = Cluster, y = Prop, fill = as.factor(Value), color = as.factor(Value))) + geom_bar(stat="identity") + scale_fill_manual(values = val2) + scale_color_manual(values = val2)  + theme_cowplot()  + facet_grid(Variable ~ outcome, scales = "free_x", space = "free_x") + labs(y = NULL) + theme(legend.position = 'none', strip.background.x = element_blank(), strip.text.x = element_blank(), plot.margin = unit(c(0, 0, 0, 0), "cm"), text = element_text(size = 23), axis.line = element_line(size = 1), axis.ticks = element_line(size = 1), axis.text = element_text(size = 18), strip.text.y = element_text(size = 23))


# Combine
png(file="C:\\Users\\LAURA\\Documents\\ETHZ\\Master\\Master thesis\\SCI project\\code - final version - 2\\Main analysis\\results - 2\\plot_cluster_composition.png" ,width=1200, height=1500)
plot_grid(p_age, p_cat, ncol = 1, rel_heights = c(1, 4.5))
dev.off()

png(file="C:\\Users\\LAURA\\Documents\\ETHZ\\Master\\Master thesis\\SCI project\\code - final version - 2\\Main analysis\\results - 2\\plot_cluster_composition-narrow.png" ,width=970, height=1500)
plot_grid(p_age, p_cat, ncol = 1, rel_heights = c(1, 4.5))
dev.off()

# Create legends
cat_vars <- c('Sex', 'AIS Grade', 'Neurological Level \n of Injury at Baseline', 'Weight Status', 'Treatment Group')
legends <- lapply(cat_vars, function(var){
  dat_var <- data[data$Variable == var, c("Value", "Cluster", "outcome")]
  names(dat_var) <- c(var, "Cluster", "outcome")
  plot_for_legend <- ggplot(data = dat_var, mapping = aes(x = Cluster, color = dat_var[[var]], fill = dat_var[[var]])) + geom_bar() + labs(color = var, fill = var) + scale_fill_manual(values = val2) + scale_color_manual(values = val2) + theme(legend.title = element_text(size=23), legend.text = element_text(size=18))
  get_legend(plot_for_legend)
})

png(file="C:\\Users\\LAURA\\Documents\\ETHZ\\Master\\Master thesis\\SCI project\\code - final version - 2\\Main analysis\\results - 2\\legend - sex.png" ,width=200, height=200)
grid.newpage()
grid.draw(legends[[1]])
dev.off()

png(file="C:\\Users\\LAURA\\Documents\\ETHZ\\Master\\Master thesis\\SCI project\\code - final version - 2\\Main analysis\\results - 2\\legend - aisgrade.png" ,width=200, height=200)
grid.newpage()
grid.draw(legends[[2]])
dev.off()

png(file="C:\\Users\\LAURA\\Documents\\ETHZ\\Master\\Master thesis\\SCI project\\code - final version - 2\\Main analysis\\results - 2\\legend - nli.png" ,width=250, height=200)
grid.newpage()
grid.draw(legends[[3]])
dev.off()

png(file="C:\\Users\\LAURA\\Documents\\ETHZ\\Master\\Master thesis\\SCI project\\code - final version - 2\\Main analysis\\results - 2\\legend - weight_status.png" ,width=200, height=200)
grid.newpage()
grid.draw(legends[[4]])
dev.off()

png(file="C:\\Users\\LAURA\\Documents\\ETHZ\\Master\\Master thesis\\SCI project\\code - final version - 2\\Main analysis\\results - 2\\legend - treatment_gr.png" ,width=200, height=200)
grid.newpage()
grid.draw(legends[[5]])
dev.off()