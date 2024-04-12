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

setwd(paste0("C:/Users/LAURA/Documents/ETHZ/Master/Master thesis/SCI project/code - final version - 2/Main analysis/results - ", min_non_NA))

lapply(c('lems', 'uems', 'ltscore', 'ppscore', 'modben'), function(outcome){
  data_outcome_imputed <- read_xlsx(paste0("C:\\Users\\LAURA\\Documents\\ETHZ\\Master\\Master thesis\\SCI project\\code - final version - 2\\Main analysis\\results - ", min_non_NA, "\\data_imputed_", outcome, "_renamed2.xlsx"))
  
  # analysis with imputed datasets
  mm <- lapply(c("lpa", "kmeans"), function(meth){
    gg <- lapply(2:6, function(g){
      imps <- lapply(1:20, function(i){
        d <- data_outcome_imputed[data_outcome_imputed$.imp == i,]
        #maxi <- max(unlist(data_outcome_imputed[paste0(meth, "-clusters-", g)]))
        d$cluster <- as.character(unlist(d[paste0(meth, "-clusters-", g)]))
        d
      })
      micemids <- datlist2mids(imps)
      withres_nomed <- with(micemids, multinom(factor(cluster, levels = as.character(1:max(unlist(data_outcome_imputed[paste0(meth, "-clusters-", g)]))))~sexcd+asimpc01+age+tx1_r+nligr1+bmi_status)) # https://www.geeksforgeeks.org/multinomial-logistic-regression-in-r/
      withres_med <- with(micemids, multinom(factor(cluster, levels = as.character(1:max(unlist(data_outcome_imputed[paste0(meth, "-clusters-", g)]))))~sexcd+asimpc01+age+tx1_r+nligr1+bmi_status+Famotidine_critical+Ranitidine_critical+Cimetidine_critical)) # https://www.geeksforgeeks.org/multinomial-logistic-regression-in-r/
      poolres_nomed <- pool(withres_nomed)
      poolres_med <- pool(withres_med)
      summary_nomed <- as.data.frame(summary(poolres_nomed))
      summary_med <- as.data.frame(summary(poolres_med))
      # get 95% confidence interval
      summary_ci_nomed <- summary_nomed
      summary_ci_nomed$ci_lower <- summary_ci_nomed$estimate - 1.96*summary_ci_nomed$std.error
      summary_ci_nomed$ci_upper <- summary_ci_nomed$estimate + 1.96*summary_ci_nomed$std.error
      summary_ci_med <- summary_med
      summary_ci_med$ci_lower <- summary_ci_med$estimate - 1.96*summary_ci_med$std.error
      summary_ci_med$ci_upper <- summary_ci_med$estimate + 1.96*summary_ci_med$std.error
      # get OR and corresponding CI
      summary_ci_OR_nomed <- summary_ci_nomed[, c('y.level', 'term', 'estimate', 'ci_lower', 'ci_upper', 'p.value')]
      summary_ci_OR_nomed$estimate <- exp(summary_ci_OR_nomed$estimate)
      summary_ci_OR_nomed$ci_lower <- exp(summary_ci_OR_nomed$ci_lower)
      summary_ci_OR_nomed$ci_upper <- exp(summary_ci_OR_nomed$ci_upper)
      summary_ci_OR_nomed$method <- meth
      summary_ci_OR_nomed$G <- g
      summary_ci_OR_nomed <- summary_ci_OR_nomed %>% relocate(c('method', 'G'))
      summary_ci_OR_med <- summary_ci_med[, c('y.level', 'term', 'estimate', 'ci_lower', 'ci_upper', 'p.value')]
      summary_ci_OR_med$estimate <- exp(summary_ci_OR_med$estimate)
      summary_ci_OR_med$ci_lower <- exp(summary_ci_OR_med$ci_lower)
      summary_ci_OR_med$ci_upper <- exp(summary_ci_OR_med$ci_upper)
      summary_ci_OR_med$method <- meth
      summary_ci_OR_med$G <- g
      summary_ci_OR_med <- summary_ci_OR_med %>% relocate(c('method', 'G'))
      
      if (g == 2){
        summary_ci_OR_nomed$y.level <- "2"
        summary_ci_OR_med$y.level <- "2"
      }
      
      list(summary_ci_OR_nomed, summary_ci_OR_med)
    })
    list(bind_rows(lapply(gg, function(x){x[[1]]})), bind_rows(lapply(gg, function(x){x[[2]]})))
  })
  imp_res_nomed <- bind_rows(lapply(mm, function(x){x[[1]]}))
  imp_res_med <- bind_rows(lapply(mm, function(x){x[[2]]}))
  
  write_xlsx(imp_res_med, paste0("C:\\Users\\LAURA\\Documents\\ETHZ\\Master\\Master thesis\\SCI project\\code - final version - 2\\Main analysis\\results - ", min_non_NA, "\\multinom_results_med_", outcome, "_2.xlsx"))
  write_xlsx(imp_res_nomed, paste0("C:\\Users\\LAURA\\Documents\\ETHZ\\Master\\Master thesis\\SCI project\\code - final version - 2\\Main analysis\\results - ", min_non_NA, "\\multinom_results_nomed_", outcome, "_2.xlsx"))
})