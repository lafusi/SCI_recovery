# patient and injury characteristics
library(readxl)
library(writexl)
library(dplyr)
library(data.table)
library(caret)
library(nnet)
library(mice)
library(miceadds)

min_non_NA <- 2

setwd(paste0("C:/Users/LAURA/Documents/ETHZ/Master/Master thesis/SCI project/code - final version - 2/Sensitivity analysis/data_sensitivity_analysis - ", min_non_NA))

lapply(c('lems', 'uems', 'ltscore', 'ppscore', 'modben'), function(outcome){
  res <- read_excel(paste0("C:/Users/LAURA/Documents/ETHZ/Master/Master thesis/SCI project/code - final version - 2/Sensitivity analysis/data_sensitivity_analysis - ", min_non_NA, "/multinom_results_nomed_", outcome, "_2_no_transformation_newref.xlsx"))
  imp_res <- res[res$imp == 'imputed', colnames(res)[!(colnames(res) %in% c('imp', 'statistic', 'p.value'))]]
  obs_res <- res[res$imp == 'observed', colnames(res)[!(colnames(res) %in% c('imp', 'statistic', 'p.value'))]]
  colnames(imp_res) <- c('method',	'G',	'y.level',	'term',	'estimate.imp',	'std.error.imp')
  colnames(obs_res) <- c('method',	'G',	'y.level',	'term',	'estimate.obs',	'std.error.obs')
  res_new <- merge(imp_res, obs_res, by = c('method',	'G',	'y.level',	'term'))
  res_new$estimate.diff <- abs(res_new$estimate.obs - res_new$estimate.imp)
  res_new$std.error.diff <- sqrt((res_new$std.error.obs)^2 + (res_new$std.error.imp)^2) #https://stats.stackexchange.com/questions/97690/alternative-formulas-for-standard-error-of-a-difference-between-two-means
  res_new$ci.upper.diff <- res_new$estimate.diff + 1.96*res_new$std.error.diff
  res_new$ci.lower.diff <- res_new$estimate.diff - 1.96*res_new$std.error.diff
  write_xlsx(res_new, paste0("C:\\Users\\LAURA\\Documents\\ETHZ\\Master\\Master thesis\\SCI project\\code - final version - 2\\Sensitivity analysis\\data_sensitivity_analysis - ", min_non_NA, "\\multinom_results_nomed_", outcome, "_2_test_difference.xlsx"))
})