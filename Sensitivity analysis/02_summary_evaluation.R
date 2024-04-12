# summary evaluation scores
library(readxl)
library(writexl)
library(dplyr)
library(data.table)

min_non_NA <- 2

setwd(paste0("C:/Users/LAURA/Documents/ETHZ/Master/Master thesis/SCI project/code - final version - 2/Sensitivity analysis/data_sensitivity_analysis - ", min_non_NA))

all_eval <- bind_rows(lapply(c('lems', 'uems', 'ltscore', 'ppscore', 'modben'), function(outcome){
  mm <- lapply(c("lpa", "kmeans"), function(meth){
    bind_rows(lapply(c("imputed", "full"), function(i){
      df <- read_excel(paste0(meth, "_quality_criteria_", i, "_", outcome, "_renamed2.xlsx"))
      df$outcome <- outcome
      df$method <- meth
      df$analysis <- ifelse(i == "imputed", "imputed", "observed")
      df
    }))
  })
}))

summary_df <- aggregate(all_eval$Calinski.Harabatz, list(all_eval$analysis, all_eval$outcome, all_eval$method, all_eval$G), mean)
names(summary_df) <- c('analysis', 'outcome', 'method', 'G', 'Calinski.Harabatz')
summary_df <- summary_df[order(summary_df$method),]
summary_df <- summary_df[order(summary_df$analysis),]
summary_df <- summary_df[order(summary_df$outcome),]
write_xlsx(summary_df, paste0("C:\\Users\\LAURA\\Documents\\ETHZ\\Master\\Master thesis\\SCI project\\code - final version - 2\\Sensitivity analysis\\data_sensitivity_analysis - ", min_non_NA, "\\summary_evaluation_2.xlsx"))

