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
library(kml)
# For evaluation
library(longitudinalData)

min_non_NA <- 2

# For reproducibility
set.seed(2023)

# LOADING DATA -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Set working directory
setwd("C:/Users/LAURA/Documents/ETHZ/Master/Master thesis/SCI project/code - final version - 2")
# Load data
data_list <- lapply(c('lems', 'uems', 'ltscore', 'ppscore', 'modben'), function(outcome){
  read_xlsx(paste0("C:\\Users\\LAURA\\Documents\\ETHZ\\Master\\Master thesis\\SCI project\\code - final version - 2\\Main analysis\\results - ", min_non_NA, "\\data_imputed_", outcome, ".xlsx"))
})

names(data_list) <- c('lems', 'uems', 'ltscore', 'ppscore', 'modben')
tot_scores <- list('lems' = paste0('lower', c('01', '04', '08', '16', '26', '52')), 'uems' = paste0('upper', c('01', '04', '08', '16', '26', '52')), 'ltscore' = paste0('ltscor', c('01', '04', '08', '16', '26', '52')), 'ppscore' = paste0('ppscor', c('01', '04', '08', '16', '26', '52')), 'modben' = paste0('modben', c('04', '08', '16', '26', '52')))

# KML ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
data_list_new <- list()
quality_list <- list()
for (outcome in c('lems', 'uems', 'ltscore', 'ppscore', 'modben')){
  datafull <- data_list[[outcome]]
  ll <- list()
  q_imp_list <- list()
  for (imp in 1:20){
    data <- datafull[datafull$.imp == imp,]
    data <- data %>% relocate(.imp, .after = last_col()) %>% relocate(c("ptid", tot_scores[[outcome]]))
    
    # Put data in the desired format: convert to cld class
    if (outcome == 'modben') {
      kml_data <- cld(as.data.frame(data), timeInData = 2:6, maxNA = 0)
      crit_data <- as.matrix(data[,2:6])
    } else {
      kml_data <- cld(as.data.frame(data), timeInData = 2:7, maxNA = 0)
      crit_data <- as.matrix(data[,2:7])
    }
    
    # Run the clustering algorithm
    kml(kml_data)
    
    quality_df <- NULL
    for (G in 2:6){
      clusters <- getClusters(kml_data, G, asInteger = TRUE)
      data[paste0('kmeans-clusters-', as.character(G))] <- clusters
      
      clust_numbers_for_eval <- clusters
      if (max(clust_numbers_for_eval) != length(unique(clust_numbers_for_eval))){ # if there is any empty cluster, the evaluation algorithm does not understand it and it thinks there is a problem because in the classification vector not all numbers from 1 to max are present
        for (clustnb in 1:length(unique(clust_numbers_for_eval))){
          if (!(clustnb %in% unique(clust_numbers_for_eval))) {
            clust_numbers_for_eval[clust_numbers_for_eval == max(clust_numbers_for_eval)] <- clustnb
          }
        }
      }
      criteria <- qualityCriterion(as.matrix(crit_data), clust_numbers_for_eval)
      data[paste0('kmeans-clusters-', as.character(G))] <- clust_numbers_for_eval # want to have numbers that correspond, in case they were changed for eval
      
      q_df <- as.data.frame(t(as.data.frame(criteria$criters)))
      rownames(q_df) <- NULL
      q_df$G <- G
      q_df <- q_df %>% relocate('G') 
      q_df[, unlist(lapply(1:6, function(x){paste0("post-probs-", x)}))] <- NA
      post_probs_each_cluster <- as.data.frame(t(as.data.frame(criteria$postProbaEachCluster)))
      rownames(post_probs_each_cluster) <- NULL
      colnames(post_probs_each_cluster) <- unlist(lapply(1:G, function(x){paste0("post-probs-", x)}))
      q_df[, colnames(post_probs_each_cluster)] <- post_probs_each_cluster
      
      if (is.null(quality_df)){
        quality_df <- q_df
      } else {
        quality_df <- rbind(quality_df, q_df)
      }
      
      post_probs_kmeans <- as.data.frame(criteria$postProba)
      names(post_probs_kmeans) <- unlist(lapply(1:G, function(x){paste0('kmeans-clusters-', as.character(G), "-post-probs-", x)}))
      data[, names(post_probs_kmeans)] <- post_probs_kmeans
    }
    
    
    data <- data %>% relocate('.imp')
    ll[[as.character(imp)]] <- data
    quality_df$.imp <- imp
    quality_df <- quality_df %>% relocate('.imp')
    q_imp_list[[as.character(imp)]] <- quality_df
  }
  
  data_list_new[[outcome]] <- bind_rows(ll)
  quality_list[[outcome]] <- bind_rows(q_imp_list)
}

# Save the complete and the imputed datasets
lapply(c('lems', 'uems', 'ltscore', 'ppscore', 'modben'), function(outcome){
  write_xlsx(data_list_new[[outcome]], paste0("C:\\Users\\LAURA\\Documents\\ETHZ\\Master\\Master thesis\\SCI project\\code - final version - 2\\Main analysis\\results - ", min_non_NA, "\\data_imputed_", outcome, ".xlsx"))
  write_xlsx(quality_list[[outcome]], paste0("C:\\Users\\LAURA\\Documents\\ETHZ\\Master\\Master thesis\\SCI project\\code - final version - 2\\Main analysis\\results - ", min_non_NA, "\\kmeans_quality_criteria_imputed_", outcome, ".xlsx"))
})