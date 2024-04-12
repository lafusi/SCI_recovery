library(readxl)
library(writexl)
library(tidyverse)
library(dplyr)

ncl <- c('lems' = 3, 'uems' = 4, 'ltscore' = 2, 'ppscore' = 2, 'modben' = 2)
summary_df <- bind_rows(lapply(names(ncl), function(outcome){
  res <- read_excel(paste0("C:\\Users\\LAURA\\Documents\\ETHZ\\Master\\Master thesis\\SCI project\\code - final version - 2\\Main analysis\\results - 2\\multinom_results_nomed_", outcome, "_2_no_transformation_newref.xlsx"))
  res <- select(res[(res$method == "kmeans")&(res$G == ncl[outcome]),], -c("method", "G", "std.error", "statistic"))
  res$outcome <- outcome
  res
}))

write_xlsx(summary_df[order(summary_df$term),], "C:\\Users\\LAURA\\Documents\\ETHZ\\Master\\Master thesis\\SCI project\\code - final version - 2\\Main analysis\\results - 2\\multinom_results_summary_2.xlsx")
