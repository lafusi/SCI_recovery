library(readxl)
library(writexl)
library(dplyr)
library(ramify)
library(dichromat)
library(RColorBrewer)

min_non_NA <- 2

lapply(c('lems', 'uems', 'ltscore', 'ppscore', 'modben'), function(outcome){
  data_outcome_imputed <- read_xlsx(paste0("C:\\Users\\LAURA\\Documents\\ETHZ\\Master\\Master thesis\\SCI project\\code - final version - 2\\Sensitivity analysis\\data_sensitivity_analysis - ", min_non_NA, "\\data_imputed_", outcome, "_renamed2.xlsx"))
  data_outcome_full <- read_xlsx(paste0("C:\\Users\\LAURA\\Documents\\ETHZ\\Master\\Master thesis\\SCI project\\code - final version - 2\\Sensitivity analysis\\data_sensitivity_analysis - ", min_non_NA, "\\data_full_", outcome, "_renamed2.xlsx"))
  all_names <- c("kmeans", "lpa")
  col_names <- lapply(2:6, function(g){
    paste0(all_names, "-clusters-", g)
  })
  col_names <- unlist(col_names)
  imp_list <- lapply(1:20, function(imp){
    data_outcome_imp <- data_outcome_imputed[data_outcome_imputed$.imp == imp,]
    
    matrix_list <- list()
    for (n in col_names) {
      if (n %in% colnames(data_outcome_imp)){
        nclust <- as.numeric(substr(n, nchar(n), nchar(n)))
        col_probs <- NULL
        for (i in 1:6){
          if (paste0(n, "-alloc-probs-", i) %in% colnames(data_outcome_imp)){ # if algorithm computed allocation probabilities
            if (is.null(col_probs)){
              col_probs <- paste0(n, "-alloc-probs-", i)
            } else {
              col_probs <- c(col_probs, paste0(n, "-alloc-probs-", i))
            }
          } else { # if allocation probabilities do not exist for that algorithm or if that cluster number just does not exist
            data_outcome_imp[paste0(n, "-alloc-probs-", i)] <- ifelse(unlist(data_outcome_imp[n]) == i, 1, 0)
            if (is.null(col_probs)){
              col_probs <- paste0(n, "-alloc-probs-", i)
            } else {
              col_probs <- c(col_probs, paste0(n, "-alloc-probs-", i))
            }
          }
        }
        prob_df <- data_outcome_imp[,col_probs]
        prob_df <- prob_df[, colSums(prob_df != 0) > 0]
        prob_df <- prob_df[, sort(names(prob_df))]
        probs_matrix <- as.matrix(prob_df)
        #colnames(probs_matrix) <- NULL
      }
      matrix_list[[n]] <- probs_matrix
    }
    matrix_list
  })
  summe <- Reduce(function(x,y){mapply("+", x, y, SIMPLIFY = FALSE)}, imp_list)
  probs <- lapply(summe, function(x){x/20})
  data_outcome_full_vs_imputed <- data_outcome_full
  max_p <- lapply(probs, function(x){apply(x, 1, max)})
  names(max_p) <- paste0(names(max_p), "-estimated-prob")
  max_p_idx <- lapply(probs, function(x){argmax(x)})
  names(max_p_idx) <- paste0(names(max_p_idx), "-estimated")
  nn <- names(max_p_idx)
  max_p_idx <- as.data.frame(max_p_idx)
  names(max_p_idx) <- nn
  prob_dfs <- lapply(probs, function(x){
    colnames(x) <- paste0(substr(colnames(x), 1, nchar(colnames(x))-8), "-estimated", substr(colnames(x), nchar(colnames(x))-7, nchar(colnames(x))))
    as.data.frame(x)
  })
  all_probs <- Reduce(function(x, y){cbind(x, y)}, prob_dfs)
  
  data_outcome_full_vs_imputed <- cbind(data_outcome_full_vs_imputed, max_p_idx, all_probs)
  
  
  
  # get accuracy
  mm <- lapply(c("lpa", "kmeans"), function(meth){
    gg <- lapply(2:6, function(g){
      cl_imp <- unlist(data_outcome_full_vs_imputed[paste0(meth, "-clusters-", g, "-estimated")])
      cl_obs <- unlist(data_outcome_full_vs_imputed[paste0(meth, "-clusters-", g)])
      accuracy <- sum(as.numeric(cl_imp == cl_obs))/nrow(data_outcome_full_vs_imputed)
      data.frame("method" = meth, "G" = g, "accuracy" = accuracy)
    })
    bind_rows(gg)# get heatmap # idea: do a heatmap where the darkness/ shade of the color is proportional to the probability of being in a certain cluster by merging the clusters, and the color itself indicates whether the these two people are actually together
    #comb <- t(combn(data_outcome_full_vs_imputed$ptid, 2))
    
  })
  write_xlsx(bind_rows(mm), paste0("C:\\Users\\LAURA\\Documents\\ETHZ\\Master\\Master thesis\\SCI project\\code - final version - 2\\Sensitivity analysis\\data_sensitivity_analysis - ", min_non_NA, "\\cluster_accuracy_", outcome, "_2.xlsx"))
  write_xlsx(data_outcome_full_vs_imputed, paste0("C:\\Users\\LAURA\\Documents\\ETHZ\\Master\\Master thesis\\SCI project\\code - final version - 2\\Sensitivity analysis\\data_sensitivity_analysis - ", min_non_NA, "\\data_full_vs_imputed_", outcome, "_2.xlsx"))
})

