# LOADING LIBRARIES --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# For loading data and writing files
library(readxl)
library(writexl)
library(png)
# For data manipulation
library(dplyr)
library(data.table)
# For plotting
library(ggplot2)
# For clustering
library(mclust)
# For evaluation
library(longitudinalData)

# For reproducibility
set.seed(2023)

# Min number of observed values per trajectories
min_non_NA <- 2

# LOADING DATA -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Set working directory
setwd("C:/Users/LAURA/Documents/ETHZ/Master/Master thesis/SCI project/code - final version - 2")
# Load data
data_list <- lapply(c('lems', 'uems', 'ltscore', 'ppscore', 'modben'), function(outcome){
  data_outcome_full <- read_xlsx(paste0("C:\\Users\\LAURA\\Documents\\ETHZ\\Master\\Master thesis\\SCI project\\code - final version - 2\\Sensitivity analysis\\data_sensitivity_analysis - ", min_non_NA, "\\data_full_", outcome, ".xlsx"))
  data_outcome_imputed <- read_xlsx(paste0("C:\\Users\\LAURA\\Documents\\ETHZ\\Master\\Master thesis\\SCI project\\code - final version - 2\\Sensitivity analysis\\data_sensitivity_analysis - ", min_non_NA, "\\data_imputed_", outcome, ".xlsx"))
  data_outcome_full$.imp <- 0
  data_outcome_full <- data_outcome_full[, colnames(data_outcome_imputed)]
  rbind(data_outcome_full, data_outcome_imputed)
})

names(data_list) <- c('lems', 'uems', 'ltscore', 'ppscore', 'modben')
tot_scores <- list('lems' = paste0('lower', c('01', '04', '08', '16', '26', '52')), 'uems' = paste0('upper', c('01', '04', '08', '16', '26', '52')), 'ltscore' = paste0('ltscor', c('01', '04', '08', '16', '26', '52')), 'ppscore' = paste0('ppscor', c('01', '04', '08', '16', '26', '52')), 'modben' = paste0('modben', c('04', '08', '16', '26', '52')))

# LPA ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
list_new <- lapply(c('lems', 'uems', 'ltscore', 'ppscore', 'modben'), function(outcome){
  datafull <- data_list[[outcome]]
  ll <- lapply(0:20, function(imp){
    data <- datafull[datafull$.imp == imp,]
    data <- data %>% relocate(.imp, .after = last_col()) %>% relocate(c("ptid", tot_scores[[outcome]]))
    
    # Put data in the desired format: matrix where rows = different observations, and columns = different variables
    if (outcome == 'modben') {
      lpa_data <- as.matrix(data[,2:6])
      crit_data <- as.matrix(data[,2:6])
    } else {
      lpa_data <- as.matrix(data[,2:7])
      crit_data <- as.matrix(data[,2:7])
    }
    
    # Run the clustering algorithm
    quality_df <- NULL
    for (G in 2:6) {
      mclust_model <- Mclust(data = lpa_data, G = G, modelNames = "EEE")
      data[paste0('lpa-clusters-', as.character(G))] <- mclust_model$classification
      probs_lpa <- as.data.frame(mclust_model$z)
      colnames(probs_lpa) <- unlist(lapply(1:ncol(probs_lpa), function(x){paste0('lpa-clusters-', as.character(G), "-alloc-probs-", x)}))
      data <- cbind(data, probs_lpa)
      
      clust_numbers_for_eval <- mclust_model$classification
      if (max(clust_numbers_for_eval) != length(unique(clust_numbers_for_eval))){ # if there is any empty cluster, the evaluation algorithm does not understand it and it thinks there is a problem because in the classification vector not all numbers from 1 to max are present
        for (clustnb in 1:length(unique(clust_numbers_for_eval))){
          if (!(clustnb %in% unique(clust_numbers_for_eval))) {
            maxi <- max(clust_numbers_for_eval)
            clust_numbers_for_eval[clust_numbers_for_eval == maxi] <- clustnb
            colnames(data)[colnames(data) %in% c(paste0('lpa-clusters-', as.character(G), "-alloc-probs-", clustnb), paste0('lpa-clusters-', as.character(G), "-alloc-probs-", maxi))] <- c(paste0('lpa-clusters-', as.character(G), "-alloc-probs-", maxi), paste0('lpa-clusters-', as.character(G), "-alloc-probs-", clustnb)) # make sure that number of alloc prob col changes accordingly
            }
        }
      }
      criteria <- qualityCriterion(as.matrix(crit_data), clust_numbers_for_eval)
      data[paste0('lpa-clusters-', as.character(G))] <- clust_numbers_for_eval # want to have numbers that correspond, in case they were changed for eval
      for (g in 2:ncol(probs_lpa)){
        data <- data %>% relocate(paste0('lpa-clusters-', as.character(G), "-alloc-probs-", g), .after = paste0('lpa-clusters-', as.character(G), "-alloc-probs-", as.character(g-1))) # make sure columns for probs are always sorted correctly after changing number
      }
      
      q_df <- as.data.frame(t(as.data.frame(criteria$criters)))
      rownames(q_df) <- NULL
      q_df$G <- G
      q_df <- q_df %>% relocate('G') 
      q_df[, unlist(lapply(1:6, function(x){paste0("post-probs-", x)}))] <- NA
      post_probs_each_cluster <- as.data.frame(t(as.data.frame(criteria$postProbaEachCluster)))
      rownames(post_probs_each_cluster) <- NULL
      colnames(post_probs_each_cluster) <- unlist(lapply(sort(unique(clust_numbers_for_eval)), function(x){paste0("post-probs-", x)}))
      q_df[, colnames(post_probs_each_cluster)] <- post_probs_each_cluster
      
      if (is.null(quality_df)){
        quality_df <- q_df
      } else {
        quality_df <- rbind(quality_df, q_df)
      }
      
      post_probs_lpa <- as.data.frame(criteria$postProba)
      names(post_probs_lpa) <- unlist(lapply(sort(unique(clust_numbers_for_eval)), function(x){paste0('lpa-clusters-', as.character(G), "-post-probs-", x)}))
      data[, names(post_probs_lpa)] <- post_probs_lpa
    }
    
    
    data <- data %>% relocate('.imp')
    quality_df$.imp <- imp
    quality_df <- quality_df %>% relocate('.imp')
    
    list(data, quality_df)
  })
  ll_data <- lapply(ll, function(x){x[[1]]})
  ll_quality <- lapply(ll, function(x){x[[2]]})
  
  list(bind_rows(ll_data), bind_rows(ll_quality))
})
data_list_new <- lapply(list_new, function(x){x[[1]]})
quality_list <- lapply(list_new, function(x){x[[2]]})

names(data_list_new) <- c('lems', 'uems', 'ltscore', 'ppscore', 'modben')
names(quality_list) <- c('lems', 'uems', 'ltscore', 'ppscore', 'modben')

# Save the complete and the imputed datasets
lapply(c('lems', 'uems', 'ltscore', 'ppscore', 'modben'), function(outcome){
  data <- data_list_new[[outcome]]
  quality <- quality_list[[outcome]]
  write_xlsx(select(data[data$.imp == 0,], -c(".imp")), paste0("C:\\Users\\LAURA\\Documents\\ETHZ\\Master\\Master thesis\\SCI project\\code - final version - 2\\Sensitivity analysis\\data_sensitivity_analysis - ", min_non_NA, "\\data_full_", outcome, ".xlsx"))
  write_xlsx(data[data$.imp != 0,], paste0("C:\\Users\\LAURA\\Documents\\ETHZ\\Master\\Master thesis\\SCI project\\code - final version - 2\\Sensitivity analysis\\data_sensitivity_analysis - ", min_non_NA, "\\data_imputed_", outcome, ".xlsx"))
  write_xlsx(select(quality[quality$.imp == 0,], -c(".imp")), paste0("C:\\Users\\LAURA\\Documents\\ETHZ\\Master\\Master thesis\\SCI project\\code - final version - 2\\Sensitivity analysis\\data_sensitivity_analysis - ", min_non_NA, "\\lpa_quality_criteria_full_", outcome, ".xlsx"))
  write_xlsx(quality[quality$.imp != 0,], paste0("C:\\Users\\LAURA\\Documents\\ETHZ\\Master\\Master thesis\\SCI project\\code - final version - 2\\Sensitivity analysis\\data_sensitivity_analysis - ", min_non_NA, "\\lpa_quality_criteria_imputed_", outcome, ".xlsx"))
})