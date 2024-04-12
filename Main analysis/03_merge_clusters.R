library(readxl)
library(writexl)
library(dplyr)
library(ramify)
library(dichromat)
library(RColorBrewer)

min_non_NA <- 2

source("C:\\Users\\LAURA\\Documents\\ETHZ\\Master\\Master thesis\\SCI project\\code - final version - 2\\00_dataset.R")
getSubset <- function(data = sygen_data, outcome_variable = 'all', min_non_NA = 2){
  if ((outcome_variable == 'all')|(!(outcome_variable %in% c('lems', 'uems', 'ltscore', 'ppscore', 'modben')))){
    filtered_data <- data
  } else {
    out_cols <- list('lems' = c('lower01', 'lower04', 'lower08', 'lower16', 'lower26', 'lower52'),
                     'uems' = c('upper01', 'upper04', 'upper08', 'upper16', 'upper26', 'upper52'),
                     'ltscore' = c('ltscor01', 'ltscor04', 'ltscor08', 'ltscor16', 'ltscor26', 'ltscor52'),
                     'ppscore' = c('ppscor01', 'ppscor04', 'ppscor08', 'ppscor16', 'ppscor26', 'ppscor52'),
                     'modben' = c('modben04', 'modben08', 'modben16', 'modben26', 'modben52'))
    outcome <- data[, out_cols[[outcome_variable]]]
    
    if (outcome_variable == 'modben'){
      condition <- as.numeric(!is.na(outcome[,1])) +
        as.numeric(!is.na(outcome[,2])) +
        as.numeric(!is.na(outcome[,3])) +
        as.numeric(!is.na(outcome[,4])) +
        as.numeric(!is.na(outcome[,5])) >= min_non_NA
    } else {
      condition <- as.numeric(!is.na(outcome[,1])) +
        as.numeric(!is.na(outcome[,2])) +
        as.numeric(!is.na(outcome[,3])) +
        as.numeric(!is.na(outcome[,4])) +
        as.numeric(!is.na(outcome[,5])) +
        as.numeric(!is.na(outcome[,6])) >= min_non_NA
    }
    filtered_data <- data[condition,]
  }
  return(filtered_data)
}

lapply(c('lems', 'uems', 'ltscore', 'ppscore', 'modben'), function(outcome){
  data_NA <- getSubset(data = sygen_data, outcome_variable = outcome, min_non_NA = min_non_NA)
  data_outcome_imputed <- read_xlsx(paste0("C:\\Users\\LAURA\\Documents\\ETHZ\\Master\\Master thesis\\SCI project\\code - final version - 2\\Main analysis\\results - ", min_non_NA, "\\data_imputed_", outcome, "_renamed2.xlsx"))
  cols <- list('lems' = c('lower01', 'lower04', 'lower08', 'lower16', 'lower26', 'lower52'), 'uems' = c('upper01', 'upper04', 'upper08', 'upper16', 'upper26', 'upper52'), 'ltscore' = c('ltscor01', 'ltscor04', 'ltscor08', 'ltscor16', 'ltscor26', 'ltscor52'), 'ppscore' = c('ppscor01', 'ppscor04', 'ppscor08', 'ppscor16', 'ppscor26', 'ppscor52'), 'modben' = c('modben04', 'modben08', 'modben16', 'modben26', 'modben52'))[[outcome]]
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
  data_outcome_imputed_est_cl <- data_NA[, c('ptid', cols)]
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
  
  data_outcome_imputed_est_cl <- cbind(data_outcome_imputed_est_cl, max_p_idx, all_probs)
  
  
  write_xlsx(data_outcome_imputed_est_cl, paste0("C:\\Users\\LAURA\\Documents\\ETHZ\\Master\\Master thesis\\SCI project\\code - final version - 2\\Main analysis\\results - ", min_non_NA, "\\data_imputed_est_cl_", outcome, "_2.xlsx"))
})

