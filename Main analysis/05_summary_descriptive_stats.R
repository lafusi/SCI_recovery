library(readxl)
library(writexl)
library(tidyverse)
library(dplyr)

ncl <- c('lems' = 3, 'uems' = 4, 'ltscore' = 2, 'ppscore' = 2, 'modben' = 2)
summary_df <- bind_rows(lapply(names(ncl), function(outcome){
  p.values <- read_excel(paste0("C:\\Users\\LAURA\\Documents\\ETHZ\\Master\\Master thesis\\SCI project\\code - final version - 2\\Main analysis\\results - 2\\descriptive_statistics_results_", outcome, "_2.xlsx"))
  test.stats <- read_excel(paste0("C:\\Users\\LAURA\\Documents\\ETHZ\\Master\\Master thesis\\SCI project\\code - final version - 2\\Main analysis\\results - 2\\descriptive_statistics_results_test_stat_", outcome, "_2.xlsx"))
  dof <- read_excel(paste0("C:\\Users\\LAURA\\Documents\\ETHZ\\Master\\Master thesis\\SCI project\\code - final version - 2\\Main analysis\\results - 2\\descriptive_statistics_results_dof_", outcome, "_2.xlsx"))
  p.values <- select(p.values[(p.values$method == "kmeans")&(p.values$G == ncl[outcome]),], -c("method", "G"))
  p.values.new <- data.frame("term" = colnames(p.values), "p.value" = unlist(p.values))
  test.stats <- select(test.stats[(test.stats$method == "kmeans")&(test.stats$G == ncl[outcome]),], -c("method", "G"))
  test.stats.new <- data.frame("term" = colnames(test.stats), "test.stat" = unlist(test.stats))
  dof <- select(dof[(dof$method == "kmeans")&(dof$G == ncl[outcome]),], -c("method", "G"))
  dof.new <- data.frame("term" = colnames(dof), "dof" = unlist(dof))
  sdf <- reduce(list(dof.new, test.stats.new, p.values.new), full_join, by = "term")
  sdf$outcome <- outcome
  sdf
}))

write_xlsx(summary_df[order(summary_df$term),], "C:\\Users\\LAURA\\Documents\\ETHZ\\Master\\Master thesis\\SCI project\\code - final version - 2\\Main analysis\\results - 2\\descriptive_statistics_results_summary_2.xlsx")
