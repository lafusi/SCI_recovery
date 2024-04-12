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

ncl <- c('lems' = 3, 'uems' = 4, 'ltscore' = 2, 'ppscore' = 2, 'modben' = 2)
cols <- list('lems' = c('lower01', 'lower04', 'lower08', 'lower16', 'lower26', 'lower52'),
             'uems' = c('upper01', 'upper04', 'upper08', 'upper16', 'upper26', 'upper52'),
             'ltscore' = c('ltscor01', 'ltscor04', 'ltscor08', 'ltscor16', 'ltscor26', 'ltscor52'),
             'ppscore' = c('ppscor01', 'ppscor04', 'ppscor08', 'ppscor16', 'ppscor26', 'ppscor52'),
             'modben' = c('modben04', 'modben08', 'modben16', 'modben26', 'modben52'))
label <- c('lems' = 'Lower Extremities Motor Score', 'uems' = 'Upper Extremities Motor Score', 'ltscore' = 'Light Touch Total Score', 'ppscore' = 'Pin Prick Total Score', 'modben' = 'Modified Benzel Scale')
ymax <- c('lems' = 50, 'uems' = 50, 'ltscore' = 112, 'ppscore' = 112, 'modben' = 7)

eval_all <- read_excel("C:\\Users\\LAURA\\Documents\\ETHZ\\Master\\Master thesis\\SCI project\\code - final version - 2\\Main analysis\\results - 2\\summary_evaluation_2.xlsx")
eval_all$method <- ifelse(eval_all$method == 'kmeans', "k-means", "Latent profile analysis")
eval_all <- bind_rows(lapply(unique(eval_all$outcome), function(out){
  subs <- eval_all[eval_all$outcome == out,]
  subs$best <- ifelse(subs$Calinski.Harabatz == max(subs$Calinski.Harabatz), TRUE, FALSE)
  subs$outcome <- label[out]
  subs
}))
eval_all$outcome <- factor(eval_all$outcome, levels = label)

png(file="C:\\Users\\LAURA\\Documents\\ETHZ\\Master\\Master thesis\\SCI project\\code - final version - 2\\Main analysis\\results - 2\\plot_CH_index.png" ,width=900, height=400)
ggplot(data = eval_all, mapping = aes(x = G, y = Calinski.Harabatz, color = method, group = method)) + geom_line() + geom_point() + scale_color_manual(values = c('k-means' = 'darkred', 'Latent profile analysis' = 'lightpink')) + geom_point(mapping = aes(x = G, y = Calinski.Harabatz), data = eval_all[eval_all$best == TRUE,], color = '#fc0f8a', shape = 18, size = 3) + facet_wrap(~outcome, ncol = 5) + labs(x = "Number of Clusters", y = "Calinski-Harabatz Index", color = 'Method') #+ theme_cowplot() + theme(text = element_text(size = 20)) 
dev.off() 
