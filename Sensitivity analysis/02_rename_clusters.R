# In this document the cluster names are sorted and renamed accordingly
library(readxl)
library(writexl)
library(dplyr)
library(longitudinalData)

# Min number of observed values per trajectories
min_non_NA <- 2

out_cols <- list('lems' = c('lower01', 'lower04', 'lower08', 'lower16', 'lower26', 'lower52'),
                 'uems' = c('upper01', 'upper04', 'upper08', 'upper16', 'upper26', 'upper52'),
                 'ltscore' = c('ltscor01', 'ltscor04', 'ltscor08', 'ltscor16', 'ltscor26', 'ltscor52'),
                 'ppscore' = c('ppscor01', 'ppscor04', 'ppscor08', 'ppscor16', 'ppscor26', 'ppscor52'),
                 'modben' = c('modben04', 'modben08', 'modben16', 'modben26', 'modben52'))

lapply(c('lems', 'uems', 'ltscore', 'ppscore', 'modben'), function(outcome){
  out_id <- list('lems' = 'lower', 'uems' = 'upper', 'ltscore' = 'ltscor', 'ppscore' = 'ppscor', 'modben' = 'modben')
  data_outcome_imputed <- read_xlsx(paste0("C:\\Users\\LAURA\\Documents\\ETHZ\\Master\\Master thesis\\SCI project\\code - final version - 2\\Sensitivity analysis\\data_sensitivity_analysis - ", min_non_NA, "\\data_imputed_", outcome, ".xlsx"))
  data_outcome_full <- read_xlsx(paste0("C:\\Users\\LAURA\\Documents\\ETHZ\\Master\\Master thesis\\SCI project\\code - final version - 2\\Sensitivity analysis\\data_sensitivity_analysis - ", min_non_NA, "\\data_full_", outcome, ".xlsx"))
  data_outcome_full$.imp <- 0
  data_outcome_imputed <- data_outcome_imputed[, colnames(data_outcome_full)]
  data_outcome <- rbind(data_outcome_full, data_outcome_imputed)
  all_names <- c("kmeans", "lpa")
  col_names <- lapply(2:6, function(g){
    paste0(all_names, "-clusters-", g)
  })
  col_names <- unlist(col_names)
  imp_list <- lapply(0:20, function(imp){
    data_outcome_imp <- data_outcome[data_outcome$.imp == imp,]
    
    for (n in col_names) {
      if (n %in% colnames(data_outcome_imp)){
        cl_list <- list()
        for (c in sort(unique(unlist(data_outcome_imp[n])))){ # for (c in 1:max(unlist(data_outcome_imp[n]))){
          cl_list[[as.character(c)]] <- mean(unlist(data_outcome_imp[data_outcome_imp[n] == c, paste0(out_id[[outcome]], "52")]))
        }
        
        new_cl <- as.list(order(unlist(cl_list)))
        new_new <- as.list(sort(unique(unlist(data_outcome_imp[n])))) # as.list(1:max(unlist(data_outcome_imp[n])))
        names(new_new) <- unlist(new_cl)
        
        #new_cl <- as.list(order(unlist(cl_list)))
        #new_new <- as.list(as.numeric(unlist(new_cl))) 
        #names(new_new) <- as.list(as.numeric(names(unlist(cl_list)[order(unlist(cl_list))]))) 
        
        new_list <- as.list(c(1:6)[!(c(1:6) %in% new_new)])
        names(new_list) <- as.character(c(1:6)[!(c(1:6) %in% as.numeric(names(new_new)))])
        new_new <- c(new_new, new_list)
        new_new <- new_new[sort(names(new_new))]
        
        data_outcome_imp$new_cluster <- NA
        rename_col_probs_a <- NULL
        rename_col_probs_p <- NULL
        for (i in names(new_new)){
          data_outcome_imp[unlist(data_outcome_imp[n]) == as.numeric(i), ]$new_cluster <- new_new[[i]]
          if (paste0(n, "-post-probs-", i) %in% colnames(data_outcome_imp)){
            if (is.null(rename_col_probs_p)){
              rename_col_probs_p <- paste0(n, "-post-probs-", i)
            } else {
              rename_col_probs_p <- c(rename_col_probs_p, paste0(n, "-post-probs-", i))
            }
          }
          
          if (paste0(n, "-alloc-probs-", i) %in% colnames(data_outcome_imp)){
            if (is.null(rename_col_probs_a)){
              rename_col_probs_a <- paste0(n, "-alloc-probs-", i)
            } else {
              rename_col_probs_a <- c(rename_col_probs_a, paste0(n, "-alloc-probs-", i))
            }
          }
          
        }
        colnames(data_outcome_imp)[colnames(data_outcome_imp) %in% rename_col_probs_a] <- unlist(lapply(names(new_new), function(i){paste0(n, "-alloc-probs-", new_new[[i]])}))[1:length(colnames(data_outcome_imp)[colnames(data_outcome_imp) %in% rename_col_probs_a])]
        colnames(data_outcome_imp)[colnames(data_outcome_imp) %in% rename_col_probs_p] <- unlist(lapply(names(new_new), function(i){paste0(n, "-post-probs-", new_new[[i]])}))[1:length(colnames(data_outcome_imp)[colnames(data_outcome_imp) %in% rename_col_probs_p])]
        
        data_outcome_imp[n] <- data_outcome_imp$new_cluster
        data_outcome_imp <- select(data_outcome_imp, -c('new_cluster'))
      }
      
    }
    data_outcome_imp
  })
  
  data <- Reduce(function(x, y){rbind(x, y)}, imp_list)
  write_xlsx(select(data[data$.imp == 0,], -c(".imp")), paste0("C:\\Users\\LAURA\\Documents\\ETHZ\\Master\\Master thesis\\SCI project\\code - final version - 2\\Sensitivity analysis\\data_sensitivity_analysis - ", min_non_NA, "\\data_full_", outcome, "_renamed.xlsx"))
  write_xlsx(data[data$.imp != 0,], paste0("C:\\Users\\LAURA\\Documents\\ETHZ\\Master\\Master thesis\\SCI project\\code - final version - 2\\Sensitivity analysis\\data_sensitivity_analysis - ", min_non_NA, "\\data_imputed_", outcome, "_renamed.xlsx"))
  
})


# also need to adapt eval scores
lapply(c('lems', 'uems', 'ltscore', 'ppscore', 'modben'), function(outcome){
  out_cols <- list('lems' = paste0('lower', c("01", "04", "08", "16", "26", "52")), 'uems' = paste0('upper', c("01", "04", "08", "16", "26", "52")), 'ltscore' =paste0('ltscor', c("01", "04", "08", "16", "26", "52")), 'ppscore' = paste0('ppscor', c("01", "04", "08", "16", "26", "52")), 'modben' = paste0('modben', c("04", "08", "16", "26", "52")))
  data_outcome_imputed <- read_xlsx(paste0("C:\\Users\\LAURA\\Documents\\ETHZ\\Master\\Master thesis\\SCI project\\code - final version - 2\\Sensitivity analysis\\data_sensitivity_analysis - ", min_non_NA, "\\data_imputed_", outcome, ".xlsx"))
  data_outcome_full <- read_xlsx(paste0("C:\\Users\\LAURA\\Documents\\ETHZ\\Master\\Master thesis\\SCI project\\code - final version - 2\\Sensitivity analysis\\data_sensitivity_analysis - ", min_non_NA, "\\data_full_", outcome, ".xlsx"))
  data_outcome_full$.imp <- 0
  data_outcome_imputed <- data_outcome_imputed[, colnames(data_outcome_full)]
  data_outcome <- rbind(data_outcome_full, data_outcome_imputed)
  lapply(c("lpa", "kmeans"), function(meth){
    imp_list <- lapply(0:20, function(imp){
      G_list <- lapply(2:6, function(G){
        data <- as.matrix(data_outcome[data_outcome$.imp == imp, out_cols[[outcome]]])
        clusters <- unlist(data_outcome[data_outcome$.imp == imp, c(paste0(meth, "-clusters-", G))])
        criteria <- qualityCriterion(data, clusters)
        
        q_df <- as.data.frame(t(as.data.frame(criteria$criters)))
        rownames(q_df) <- NULL
        q_df$G <- G
        q_df <- q_df %>% relocate('G') 
        q_df[, unlist(lapply(1:6, function(x){paste0("post-probs-", x)}))] <- NA
        post_probs_each_cluster <- as.data.frame(t(as.data.frame(criteria$postProbaEachCluster)))
        rownames(post_probs_each_cluster) <- NULL
        colnames(post_probs_each_cluster) <- unlist(lapply(sort(unique(clusters)), function(x){paste0("post-probs-", x)}))
        q_df[, colnames(post_probs_each_cluster)] <- post_probs_each_cluster
        q_df
      })
      quality_df <- bind_rows(G_list)
      quality_df$.imp <- imp
      quality_df <- quality_df %>% relocate('.imp')
      quality_df
    })
    quality <- bind_rows(imp_list)
    write_xlsx(select(quality[quality$.imp == 0,], -c(".imp")), paste0("C:\\Users\\LAURA\\Documents\\ETHZ\\Master\\Master thesis\\SCI project\\code - final version - 2\\Sensitivity analysis\\data_sensitivity_analysis - ", min_non_NA, "\\", meth, "_quality_criteria_full_", outcome, "_renamed.xlsx"))
    write_xlsx(quality[quality$.imp != 0,], paste0("C:\\Users\\LAURA\\Documents\\ETHZ\\Master\\Master thesis\\SCI project\\code - final version - 2\\Sensitivity analysis\\data_sensitivity_analysis - ", min_non_NA, "\\", meth, "_quality_criteria_imputed_", outcome, "_renamed.xlsx"))
  })
})
