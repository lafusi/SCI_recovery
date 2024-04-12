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

ncl <- c('lems' = 3, 'uems' = 4, 'ltscore' = 3, 'ppscore' = 3, 'modben' = 2)
cols <- list('lems' = c('lower01', 'lower04', 'lower08', 'lower16', 'lower26', 'lower52'),
             'uems' = c('upper01', 'upper04', 'upper08', 'upper16', 'upper26', 'upper52'),
             'ltscore' = c('ltscor01', 'ltscor04', 'ltscor08', 'ltscor16', 'ltscor26', 'ltscor52'),
             'ppscore' = c('ppscor01', 'ppscor04', 'ppscor08', 'ppscor16', 'ppscor26', 'ppscor52'),
             'modben' = c('modben04', 'modben08', 'modben16', 'modben26', 'modben52'))
#colors <- c("1" = "#DDCC77", "2" = "#117733", "3" = "#44AA99", "4" = "#999933", "5" = "#6699CC", "6" = "#888888")
#colors_mean <- c("1" = "#b0a35f", "2" = "#0b5323", "3" = "#2f766b", "4" = "#6b6b23", "5" = "#476b8e", "6" = "#5f5f5f")
#names(colors) <- paste0(names(colors), " - imputed")
#names(colors_mean) <- paste0(names(colors_mean), " - observed")
#colors_tot <- c(colors, colors_mean)
#colors_tot <- colors_tot[sort(names(colors_tot))]
colors_imp <- c("1" = "#e7d4e8", "2" = "#c2a5cf", "3" = "#9970ab", "4" = "#762a83")
colors_obs <- c("1" = "#d9f0d3", "2" = "#a6dba0", "3" = "#5aae61", "4" = "#1b7837")
names(colors_imp) <- paste0(names(colors_imp), " - imputed")
names(colors_obs) <- paste0(names(colors_obs), " - observed")
colors_tot <- c(colors_imp, colors_obs)
colors_tot <- colors_tot[sort(names(colors_tot))]
idx_colors <- 2*ncl

label <- c('lems' = 'Lower Extremity Motor Score', 'uems' = 'Upper Extremity Motor Score', 'ltscore' = 'Light Touch Total Score', 'ppscore' = 'Pinprick Total Score', 'modben' = 'Modified Benzel Scale')
ymax <- c('lems' = 50, 'uems' = 50, 'ltscore' = 112, 'ppscore' = 112, 'modben' = 7)

plots <- lapply(c('lems', 'uems', 'ltscore', 'ppscore', 'modben'), function(outcome){
  dat <- read_excel(paste0("C:\\Users\\LAURA\\Documents\\ETHZ\\Master\\Master thesis\\SCI project\\code - final version - 2\\Sensitivity analysis\\data_sensitivity_analysis - 2\\agg_", outcome, "_2.xlsx"))
  dat <- dat[(dat$method == 'kmeans')&(dat$G == ncl[outcome]), c('week', 'estimate', 'std.error', 'alloc', 'imp')]
  dat$imp_alloc <- paste0(dat$alloc, " - ", dat$imp)
  #expanded_df <- as.data.frame(expand_grid(unique(dat$alloc), 0:52))
  #names(expanded_df) <- c('alloc', 'week')
  #dat <- merge(dat, expanded_df, by = c('alloc', 'week'), all = TRUE)
  #dat$week <- as.character(dat$week)
  
  ggplot() + geom_line(mapping = aes(x = week, y = estimate, color = imp_alloc), data = dat, linewidth = 1) + geom_ribbon(mapping = aes(x = week, ymin = estimate - std.error, ymax = estimate + std.error, fill = imp_alloc), data = dat, alpha = 0.3, color = NA) + 
    #geom_boxplot(mapping = aes(x = week, y = outcome, color = imp_alloc, fill = imp_alloc, group = interaction(week, imp_alloc)), data = dat2_reshaped[dat2_reshaped$alloc == 1,], alpha = 0.3, outlier.shape = NA, width = 2) + geom_boxplot(mapping = aes(x = week, y = outcome, color = imp_alloc, fill = imp_alloc, group = interaction(week, imp_alloc)), data = dat2_reshaped[dat2_reshaped$alloc == 2,], alpha = 0.3, outlier.shape = NA, width = 2) + geom_boxplot(mapping = aes(x = week, y = outcome, color = imp_alloc, fill = imp_alloc, group = interaction(week, imp_alloc)), data = dat2_reshaped[dat2_reshaped$alloc == 3,], alpha = 0.3, outlier.shape = NA, width = 2) + 
    scale_color_manual(values = colors_tot[1:idx_colors[outcome]]) + scale_fill_manual(values = colors_tot[1:idx_colors[outcome]]) + labs(x = "Time after Injury [Weeks]", y = label[outcome]) + 
    theme_cowplot() + theme(legend.position = 'none', text = element_text(size = 23), axis.line = element_line(size = 1), axis.ticks = element_line(size = 1), axis.text = element_text(size = 18), plot.margin = unit(c(1.5,1,1,1.5), "lines")) + ylim(0,ymax[outcome])
})

#png(file="C:\\Users\\LAURA\\Documents\\ETHZ\\Master\\Master thesis\\SCI project\\code - final version - 2\\Sensitivity analysis\\data_sensitivity_analysis - 2\\plot_for_report-2.0.png" ,width=500, height=2500)
#plot_grid(plotlist = plots, labels = "AUTO", ncol = 1, label_size = 30, label_x = -0.01)
#dev.off()

#png(file="C:\\Users\\LAURA\\Documents\\ETHZ\\Master\\Master thesis\\SCI project\\code - final version - 2\\Sensitivity analysis\\data_sensitivity_analysis - 2\\plot_for_report_2-2.0.png" ,width=1000, height=1500)
#plot_grid(plotlist = plots, labels = "AUTO", ncol = 2, label_size = 30, label_x = -0.01)
#dev.off()

#png(file="C:\\Users\\LAURA\\Documents\\ETHZ\\Master\\Master thesis\\SCI project\\code - final version - 2\\Sensitivity analysis\\data_sensitivity_analysis - 2\\plot_for_report_3-2.0.png" ,width=1500, height=1000)
#plot_grid(plotlist = plots, labels = "AUTO", ncol = 3, label_size = 30, label_x = -0.01)
#dev.off()

png(file="C:\\Users\\LAURA\\Documents\\ETHZ\\Master\\Master thesis\\SCI project\\code - final version - 2\\Sensitivity analysis\\data_sensitivity_analysis - 2\\plot_for_report_3b-2.0-NEW.png" ,width=1200, height=800)
plot_grid(plotlist = plots, labels = "AUTO", ncol = 3, label_size = 30, label_x = -0.02)
dev.off()

# legends
plot_legends <- lapply(c('lems', 'uems', 'ltscore', 'ppscore', 'modben'), function(outcome){
  dat <- read_excel(paste0("C:\\Users\\LAURA\\Documents\\ETHZ\\Master\\Master thesis\\SCI project\\code - final version - 2\\Sensitivity analysis\\data_sensitivity_analysis - 2\\agg_", outcome, "_2.xlsx"))
  dat <- dat[(dat$method == 'kmeans')&(dat$G == ncl[outcome]), c('week', 'estimate', 'std.error', 'alloc', 'imp')]
  dat$imp_alloc <- paste0(dat$alloc, " - ", dat$imp)
  #expanded_df <- as.data.frame(expand_grid(unique(dat$alloc), 0:52))
  #names(expanded_df) <- c('alloc', 'week')
  #dat <- merge(dat, expanded_df, by = c('alloc', 'week'), all = TRUE)
  #dat$week <- as.character(dat$week)
  
  p <- ggplot() + geom_line(mapping = aes(x = week, y = estimate, color = imp_alloc), data = dat, linewidth = 1) + geom_ribbon(mapping = aes(x = week, ymin = estimate - std.error, ymax = estimate + std.error, fill = imp_alloc), data = dat, alpha = 0.3, color = NA) + 
    #geom_boxplot(mapping = aes(x = week, y = outcome, color = imp_alloc, fill = imp_alloc, group = interaction(week, imp_alloc)), data = dat2_reshaped[dat2_reshaped$alloc == 1,], alpha = 0.3, outlier.shape = NA, width = 2) + geom_boxplot(mapping = aes(x = week, y = outcome, color = imp_alloc, fill = imp_alloc, group = interaction(week, imp_alloc)), data = dat2_reshaped[dat2_reshaped$alloc == 2,], alpha = 0.3, outlier.shape = NA, width = 2) + geom_boxplot(mapping = aes(x = week, y = outcome, color = imp_alloc, fill = imp_alloc, group = interaction(week, imp_alloc)), data = dat2_reshaped[dat2_reshaped$alloc == 3,], alpha = 0.3, outlier.shape = NA, width = 2) + 
    scale_color_manual(values = colors_tot[1:idx_colors[outcome]]) + scale_fill_manual(values = colors_tot[1:idx_colors[outcome]]) + labs(x = "Time after Injury [Weeks]", y = label[outcome], color = "Cluster Number", fill = "Cluster Number") + 
    theme_cowplot() + theme(text = element_text(size = 23), axis.line = element_line(size = 1), axis.ticks = element_line(size = 1), axis.text = element_text(size = 18), legend.title = element_text(size=23), legend.text = element_text(size=18)) + ylim(0,ymax[outcome])
  get_legend(p)
})

png(file="C:\\Users\\LAURA\\Documents\\ETHZ\\Master\\Master thesis\\SCI project\\code - final version - 2\\Sensitivity analysis\\data_sensitivity_analysis - 2\\common_legend-2.0-NEW.png" , width=200, height=200)
grid.newpage()
grid.draw(plot_legends[[2]])
dev.off()

p <- image_read("C:\\Users\\LAURA\\Documents\\ETHZ\\Master\\Master thesis\\SCI project\\code - final version - 2\\Sensitivity analysis\\data_sensitivity_analysis - 2\\plot_for_report_3b-2.0-NEW.png")
l <- image_read("C:\\Users\\LAURA\\Documents\\ETHZ\\Master\\Master thesis\\SCI project\\code - final version - 2\\Sensitivity analysis\\data_sensitivity_analysis - 2\\common_legend-2.0-NEW.png")

image_write(image_composite(p, l, offset = "+900+500"), "C:\\Users\\LAURA\\Documents\\ETHZ\\Master\\Master thesis\\SCI project\\code - final version - 2\\Sensitivity analysis\\data_sensitivity_analysis - 2\\plot_with_legend-2.0-NEW.png")
