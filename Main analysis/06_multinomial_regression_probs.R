# patient and injury characteristics
library(readxl)
library(writexl)
library(dplyr)
library(data.table)
library(caret)
library(nnet)
library(mice)
library(miceadds)
library(tidyverse)

min_non_NA <- 2

setwd(paste0("C:/Users/LAURA/Documents/ETHZ/Master/Master thesis/SCI project/code - final version - 2/Main analysis/results - ", min_non_NA))

lapply(c('lems', 'uems', 'ltscore', 'ppscore', 'modben'), function(outcome){
  data_outcome_imputed <- read_xlsx(paste0("C:\\Users\\LAURA\\Documents\\ETHZ\\Master\\Master thesis\\SCI project\\code - final version - 2\\Main analysis\\results - ", min_non_NA, "\\data_imputed_", outcome, "_renamed2.xlsx"))
  data_outcome_imputed$sexcd <- as.factor(data_outcome_imputed$sexcd)
  data_outcome_imputed$asimpc01 <- as.factor(data_outcome_imputed$asimpc01)
  data_outcome_imputed$tx1_r <- factor(data_outcome_imputed$tx1_r, levels = c("P", "D1", "D2"))
  data_outcome_imputed$bmi_status <- as.factor(data_outcome_imputed$bmi_status)
  data_outcome_imputed$nligr1 <- as.factor(data_outcome_imputed$nligr1)
  grid_med <- expand_grid('sexcd' = unique(data_outcome_imputed$sexcd), 'asimpc01' = unique(data_outcome_imputed$asimpc01), 'age' = (1:10)*8, 'tx1_r' = unique(data_outcome_imputed$tx1_r), 'nligr1' = unique(data_outcome_imputed$nligr1), 'bmi_status' = unique(data_outcome_imputed$bmi_status), 'Famotidine_critical' = unique(data_outcome_imputed$Famotidine_critical), 'Ranitidine_critical' = unique(data_outcome_imputed$Ranitidine_critical), 'Cimetidine_critical' = unique(data_outcome_imputed$Cimetidine_critical))
  grid_nomed <- expand_grid('sexcd' = unique(data_outcome_imputed$sexcd), 'asimpc01' = unique(data_outcome_imputed$asimpc01), 'age' = (2:16)*5, 'tx1_r' = unique(data_outcome_imputed$tx1_r), 'nligr1' = unique(data_outcome_imputed$nligr1), 'bmi_status' = unique(data_outcome_imputed$bmi_status))
  
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
      withres_nomed <- with(micemids, predict(multinom(factor(cluster, levels = as.character(1:max(unlist(data_outcome_imputed[paste0(meth, "-clusters-", g)]))))~sexcd+asimpc01+age+tx1_r+nligr1+bmi_status),newdata=grid_nomed,type="probs")) # https://www.geeksforgeeks.org/multinomial-logistic-regression-in-r/
      withres_med <- with(micemids, predict(multinom(factor(cluster, levels = as.character(1:max(unlist(data_outcome_imputed[paste0(meth, "-clusters-", g)]))))~sexcd+asimpc01+age+tx1_r+nligr1+bmi_status+Famotidine_critical+Ranitidine_critical+Cimetidine_critical),newdata=grid_med,type="probs")) # https://www.geeksforgeeks.org/multinomial-logistic-regression-in-r/
      withres_nomed_dfs <- withres_nomed[[4]]
      withres_med_dfs <- withres_med[[4]]
      check_cols_nomed <- lapply(withres_nomed_dfs, function(x){colnames(x)})
      check_cols_med <- lapply(withres_med_dfs, function(x){colnames(x)})
      if (length(unique(check_cols_nomed)) > 1){
        new_cols <- sort(unique(unlist(unique(check_cols_nomed))))
        withres_nomed_dfs <- lapply(withres_nomed_dfs, function(df){
          df <- as.data.frame(df)
          for (c in new_cols){
            if (!(c %in% colnames(df))){
              df[c] <- 0
            }
          }
          as.matrix(df[, new_cols])
        })
      }
      if (length(unique(check_cols_med)) > 1){
        new_cols <- sort(unique(unlist(unique(check_cols_med))))
        withres_med_dfs <- lapply(withres_med_dfs, function(df){
          df <- as.data.frame(df)
          for (c in new_cols){
            if (!(c %in% colnames(df))){
              df[c] <- 0
            }
          }
          as.matrix(df[, new_cols])
        })
      }
      pnomed <- Reduce("+",withres_nomed_dfs)/length(withres_nomed_dfs)
      pmed <- Reduce("+",withres_med_dfs)/length(withres_med_dfs)
      if (g == 2){
        pnomed <- data.frame("1" = 1-pnomed, "2"=pnomed)
        pmed <- data.frame("1" = 1-pmed, "2"=pmed)
        names(pnomed) <- c("1", "2")
        names(pmed) <- c("1", "2")
      } else {
        pnomed <- as.data.frame(pnomed)
        pmed <- as.data.frame(pmed)
      }
      probs_nomed <- bind_cols(grid_nomed,pnomed)
      probs_med <- bind_cols(grid_med,pmed)
      reshape_nomed <- lapply(colnames(pnomed), function(cl){
        df <- probs_nomed[,c(colnames(grid_nomed), cl)]
        colnames(df) <- c(colnames(grid_nomed), 'prob')
        df$y.level <- cl
        df
      })
      probs_nomed <- bind_rows(reshape_nomed)
      reshape_med <- lapply(colnames(pmed), function(cl){
        df <- probs_med[,c(colnames(grid_med), cl)]
        colnames(df) <- c(colnames(grid_med), 'prob')
        df$y.level <- cl
        df
      })
      probs_med <- bind_rows(reshape_med)
      probs_nomed$method <- meth
      probs_med$method <- meth
      probs_nomed$G <- g
      probs_med$G <- g
      probs_nomed <- probs_nomed %>% relocate(c('method', 'G'))
      probs_med <- probs_med %>% relocate(c('method', 'G'))
      
      list(probs_nomed, probs_med)
    })
    write_xlsx(bind_rows(lapply(gg, function(x){x[[1]]})), paste0("C:\\Users\\LAURA\\Documents\\ETHZ\\Master\\Master thesis\\SCI project\\code - final version - 2\\Main analysis\\results - ", min_non_NA, "\\multinom_results_nomed_", outcome, "_", meth, "_2_probs.xlsx"))
    write_xlsx(bind_rows(lapply(gg, function(x){x[[2]]})), paste0("C:\\Users\\LAURA\\Documents\\ETHZ\\Master\\Master thesis\\SCI project\\code - final version - 2\\Main analysis\\results - ", min_non_NA, "\\multinom_results_med_", outcome, "_", meth, "_2_probs.xlsx"))
  })
})

#https://stackoverflow.com/questions/75314806/how-do-i-combine-fitted-models-on-imputed-data-into-a-usable-model-for-new-predi
