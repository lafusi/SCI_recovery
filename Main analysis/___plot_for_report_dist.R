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
colors <- c("1" = "#DDCC77", "2" = "#117733", "3" = "#44AA99", "4" = "#999933", "5" = "#6699CC", "6" = "#888888")
colors_mean <- c("1" = "#b0a35f", "2" = "#0b5323", "3" = "#2f766b", "4" = "#6b6b23", "5" = "#476b8e", "6" = "#5f5f5f")
label <- c('lems' = 'Lower Extremities Motor Score', 'uems' = 'Upper Extremities Motor Score', 'ltscore' = 'Light Touch Total Score', 'ppscore' = 'Pin Prick Total Score', 'modben' = 'Modified Benzel Scale')
ymax <- c('lems' = 50, 'uems' = 50, 'ltscore' = 112, 'ppscore' = 112, 'modben' = 7)

plots <- lapply(c('lems', 'uems', 'ltscore', 'ppscore', 'modben'), function(outcome){
  dat <- read_excel(paste0("C:\\Users\\LAURA\\Documents\\ETHZ\\Master\\Master thesis\\SCI project\\code - final version - 2\\Main analysis\\results - 2\\agg_", outcome, "_2.xlsx"))
  dat <- dat[(dat$method == 'kmeans')&(dat$G == ncl[outcome]), c('week', 'estimate', 'std.error', 'alloc')]
  #expanded_df <- as.data.frame(expand_grid(unique(dat$alloc), 0:52))
  #names(expanded_df) <- c('alloc', 'week')
  #dat <- merge(dat, expanded_df, by = c('alloc', 'week'), all = TRUE)
  #dat$week <- as.character(dat$week)
  dat2 <- read_excel(paste0("C:\\Users\\LAURA\\Documents\\ETHZ\\Master\\Master thesis\\SCI project\\code - final version - 2\\Main analysis\\results - 2\\data_imputed_", outcome, "_renamed2.xlsx"))
  dat2$alloc <- dat2[[paste0('kmeans-clusters-', ncl[outcome])]]
  dat2_reshaped <- bind_rows(lapply(cols[[outcome]], function(w){
    d <- dat2[, c('.imp', 'alloc', w)]
    names(d) <- c('.imp', 'alloc', 'outcome')
    d$week <- as.numeric(substr(w, nchar(w)-1, nchar(w)))
    d$grp <- paste0(d$alloc, "-", d$week)
    d
  }))
  ggplot(data = dat2_reshaped, mapping = aes(x = outcome, color = as.character(alloc), fill = as.character(alloc))) + geom_density(alpha = 0.3) + 
    scale_color_manual(values = colors_mean[1:ncl[outcome]]) + scale_fill_manual(values = colors_mean[1:ncl[outcome]]) + labs(x = label[outcome], y = "Density") + 
    theme_cowplot() + theme(legend.position = 'none', text = element_text(size = 20)) + xlim(0,ymax[outcome]) + ylim(0,1) + facet_wrap(~week, nrow = 1)
})

png(file="C:\\Users\\LAURA\\Documents\\ETHZ\\Master\\Master thesis\\SCI project\\code - final version - 2\\Main analysis\\results - 2\\plot_for_report_dist_3b-.png" ,width=1200, height=1200)
plot_grid(plotlist = plots, labels = "AUTO", ncol = 3, label_size = 30, label_x = -0.01)
dev.off()

# legends
plot_legends <- lapply(c('lems', 'uems', 'ltscore', 'ppscore', 'modben'), function(outcome){
  dat <- read_excel(paste0("C:\\Users\\LAURA\\Documents\\ETHZ\\Master\\Master thesis\\SCI project\\code - final version - 2\\Main analysis\\results - 2\\agg_", outcome, "_2.xlsx"))
  dat <- dat[(dat$method == 'kmeans')&(dat$G == ncl[outcome]), c('week', 'estimate', 'std.error', 'alloc')]
  #expanded_df <- as.data.frame(expand_grid(unique(dat$alloc), 0:52))
  #names(expanded_df) <- c('alloc', 'week')
  #dat <- merge(dat, expanded_df, by = c('alloc', 'week'), all = TRUE)
  #dat$week <- as.character(dat$week)
  dat2 <- read_excel(paste0("C:\\Users\\LAURA\\Documents\\ETHZ\\Master\\Master thesis\\SCI project\\code - final version - 2\\Main analysis\\results - 2\\data_imputed_", outcome, "_renamed2.xlsx"))
  dat2$alloc <- dat2[[paste0('kmeans-clusters-', ncl[outcome])]]
  dat2_reshaped <- bind_rows(lapply(cols[[outcome]], function(w){
    d <- dat2[, c('.imp', 'alloc', w)]
    names(d) <- c('.imp', 'alloc', 'outcome')
    d$week <- as.numeric(substr(w, nchar(w)-1, nchar(w)))
    d$grp <- paste0(d$alloc, "-", d$week)
    d
  }))
  p <- ggplot() + geom_line(mapping = aes(x = week, y = estimate, color = as.character(alloc)), data = dat, linewidth = 1) + geom_ribbon(mapping = aes(x = week, ymin = estimate - std.error, ymax = estimate + std.error, fill = as.character(alloc)), data = dat, alpha = 0.5, color = NA) + 
    #geom_boxplot(mapping = aes(x = week, y = outcome, color = as.character(alloc), fill = as.character(alloc), group = interaction(week, as.character(alloc))), data = dat2_reshaped[dat2_reshaped$alloc == 1,], alpha = 0.3, outlier.shape = NA, width = 2) + geom_boxplot(mapping = aes(x = week, y = outcome, color = as.character(alloc), fill = as.character(alloc), group = interaction(week, as.character(alloc))), data = dat2_reshaped[dat2_reshaped$alloc == 2,], alpha = 0.3, outlier.shape = NA, width = 2) + geom_boxplot(mapping = aes(x = week, y = outcome, color = as.character(alloc), fill = as.character(alloc), group = interaction(week, as.character(alloc))), data = dat2_reshaped[dat2_reshaped$alloc == 3,], alpha = 0.3, outlier.shape = NA, width = 2) + 
    scale_color_manual(values = colors_mean[1:ncl[outcome]]) + scale_fill_manual(values = colors_mean[1:ncl[outcome]]) + labs(x = "Time after Injury [Weeks]", y = label[outcome], color = "Cluster Number", fill = "Cluster Number") + 
    theme_cowplot() + theme(text = element_text(size = 20)) + ylim(0,ymax[outcome])
  get_legend(p)
})

png(file="C:\\Users\\LAURA\\Documents\\ETHZ\\Master\\Master thesis\\SCI project\\code - final version - 2\\Main analysis\\results - 2\\common_legend.png" ,width=150, height=100)
grid.newpage()
grid.draw(plot_legends[[2]])
dev.off()

p <- image_read("C:\\Users\\LAURA\\Documents\\ETHZ\\Master\\Master thesis\\SCI project\\code - final version - 2\\Main analysis\\results - 2\\plot_for_report_3-.png")
l <- image_read("C:\\Users\\LAURA\\Documents\\ETHZ\\Master\\Master thesis\\SCI project\\code - final version - 2\\Main analysis\\results - 2\\common_legend.png")

image_write(image_composite(p, l, offset = "+1175+700"), "C:\\Users\\LAURA\\Documents\\ETHZ\\Master\\Master thesis\\SCI project\\code - final version - 2\\Main analysis\\results - 2\\plot_with_legend.png")
