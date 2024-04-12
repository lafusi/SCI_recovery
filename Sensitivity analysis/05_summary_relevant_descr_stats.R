# summary evaluation scores
library(readxl)
library(writexl)
library(dplyr)
library(data.table)

min_non_NA <- 2

setwd(paste0("C:/Users/LAURA/Documents/ETHZ/Master/Master thesis/SCI project/code - final version - 2/Sensitivity analysis/data_sensitivity_analysis - ", min_non_NA))
ncl <- c('lems' = 3, 'uems' = 4, 'ltscore' = 3, 'ppscore' = 3, 'modben' = 2)

res_eval <- bind_rows(lapply(c('lems', 'uems', 'ltscore', 'ppscore', 'modben'), function(outcome){
  df <- read_excel(paste0("descriptive_statistics_results_", outcome, "_2.xlsx"))
  df <- df[(df$G == ncl[outcome])&(df$method == 'kmeans'),]
  bind_rows(lapply(c("sexcd",	"asimpc01",	"age", "tx1_r",	"nligr1", "bmi_status"), function(x){
    imputed <- df[df$imp == 'imputed', x]
    observed <- df[df$imp == 'observed', x]
    colnames(imputed) <- 'imputed'
    colnames(observed) <- 'observed'
    all <- cbind(imputed, observed)
    all$var <- x
    all$outcome <- outcome
    all
  }))
}))

write_xlsx(res_eval, paste0("C:\\Users\\LAURA\\Documents\\ETHZ\\Master\\Master thesis\\SCI project\\code - final version - 2\\Sensitivity analysis\\data_sensitivity_analysis - ", min_non_NA, "\\descr_stats_summary_2.xlsx"))

