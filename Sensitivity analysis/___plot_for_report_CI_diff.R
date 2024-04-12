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
legend_term <- c("asimpc01" = "AIS Grade", "bmi_status" = "Weight Status", "nligr1" = "Neurological Level of \n Injury at Baseline", "sexcd" = "Sex", "tx1_r" = "Treatment Group")

datfull <- bind_rows(lapply(c('lems', 'uems', 'ltscore', 'ppscore', 'modben'), function(outcome){
  dat <- read_excel(paste0("C:\\Users\\LAURA\\Documents\\ETHZ\\Master\\Master thesis\\SCI project\\code - final version - 2\\Sensitivity analysis\\data_sensitivity_analysis - 2\\multinom_results_nomed_", outcome, "_2_test_difference.xlsx"))
  dat <- dat[(dat$method == 'kmeans')&(dat$G == ncl[outcome]), ]
  dat$term_beautiful <- dat$term
  dat[dat$term_beautiful == "(Intercept)",]$term_beautiful <- "Intercept"
  dat[dat$term_beautiful == "age",]$term_beautiful <- "Age [years]"
  for (i in names(legend_term)){
    dat[grepl(i, dat$term_beautiful),]$term_beautiful <- paste0(legend_term[i], " = ", substr(dat[grepl(i, dat$term_beautiful),]$term_beautiful, nchar(i)+1, nchar(dat[grepl(i, dat$term_beautiful),]$term_beautiful)))
  }
  dat$significance <- ifelse((dat$ci.lower.diff < 0)&(dat$ci.upper.diff > 0), "yes", ifelse(((dat$ci.lower.diff > 0)&(dat$ci.upper.diff > 0))|((dat$ci.lower.diff < 0)&(dat$ci.upper.diff < 0)), "no", "not determined"))
  dat$outcome <- outcome
  dat
}))
datfull$outcome <- factor(datfull$outcome, levels = c('lems', 'uems', 'ltscore', 'ppscore', 'modben'))
datfull$out_G <- paste0(datfull$outcome, datfull$y.level)
datfull$out_G <- factor(datfull$out_G, levels = c('lems2', 'lems3', 'uems2', 'uems3', 'uems4', 'ltscore2', 'ltscore3', 'ppscore2', 'ppscore3', 'modben2'))
cl_names <- list('lems2'="2", 'lems3'="3", 'uems2'="2", 'uems3'="3", 'uems4'="4", 'ltscore2'="2", 'ltscore3'="3", 'ppscore2'="2", 'ppscore3'="3", 'modben2'="2")
cl_labeller <- function(variable,value){
  return(cl_names[value])
}

png(file="C:\\Users\\LAURA\\Documents\\ETHZ\\Master\\Master thesis\\SCI project\\code - final version - 2\\Sensitivity analysis\\data_sensitivity_analysis - 2\\plot_sensitivity-NEW-2.png" ,width=1200, height=1500)
ggplot(data = datfull, mapping = aes(x = estimate.diff, y = term_beautiful)) + geom_vline(xintercept=0, color = "#E5E5E5", size = 1) + 
  geom_errorbarh(aes(xmin = ci.lower.diff, xmax = ci.upper.diff), height = 0.5, size = 1) + geom_point(aes(color = significance), shape = 15, size = 5) + facet_wrap(~out_G, labeller=cl_labeller, nrow = 2) + scale_color_manual(values = c("yes"="gold", "no"="royalblue", "not determined" = "darkgrey")) + 
  labs(x = "Absolute Difference between Observed and Estimated Coefficients", y = "Multinomial Regression Coefficient") + theme_cowplot() + theme(legend.position = 'none', text = element_text(size = 23), axis.line = element_line(size = 1), axis.ticks = element_line(size = 1), axis.text = element_text(size = 18), strip.text = element_text(size = 18)) 
dev.off()

#p <- image_read("C:\\Users\\LAURA\\Documents\\ETHZ\\Master\\Master thesis\\SCI project\\code - final version - 2\\Sensitivity analysis\\data_sensitivity_analysis - 2\\plot_sensitivity.png")

#image_write(image_composite(p, l, offset = "+1175+650"), "C:\\Users\\LAURA\\Documents\\ETHZ\\Master\\Master thesis\\SCI project\\code - final version - 2\\Sensitivity analysis\\data_sensitivity_analysis - 2\\plot_with_legend-2.0.png")
