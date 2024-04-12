# In this document the cluster names are sorted and renamed accordingly
library(readxl)
library(writexl)
library(dplyr)
library(combinat)
library(longitudinalData)

# Min number of observed values per trajectories
min_non_NA <- 2

lapply(c('lems', 'uems', 'ltscore', 'ppscore', 'modben'), function(outcome){
  out_id <- list('lems' = 'lower', 'uems' = 'upper', 'ltscore' = 'ltscor', 'ppscore' = 'ppscor', 'modben' = 'modben')
  data_outcome_imputed <- read_xlsx(paste0("C:\\Users\\LAURA\\Documents\\ETHZ\\Master\\Master thesis\\SCI project\\code - final version - 2\\Sensitivity analysis\\data_sensitivity_analysis - ", min_non_NA, "\\data_imputed_", outcome, "_renamed.xlsx"))
  data_outcome_full <- read_xlsx(paste0("C:\\Users\\LAURA\\Documents\\ETHZ\\Master\\Master thesis\\SCI project\\code - final version - 2\\Sensitivity analysis\\data_sensitivity_analysis - ", min_non_NA, "\\data_full_", outcome, "_renamed.xlsx"))
  data_outcome_full$.imp <- 0
  data_outcome_imputed <- data_outcome_imputed[, colnames(data_outcome_full)]
  data_outcome <- rbind(data_outcome_full, data_outcome_imputed)
  all_names <- c("kmeans", "lpa")
  col_names <- lapply(2:6, function(g){
    paste0(all_names, "-clusters-", g)
  })
  col_names <- unlist(col_names)
  
  for (n in col_names) {
    if (n %in% colnames(data_outcome)){
      nb_cl <- unlist(lapply(0:20, function(i){
        length(unique(unlist(data_outcome[data_outcome$.imp == i, n])))
      }))
      if (length(unique(nb_cl)) != 1) {
        keep_or_check <- lapply(0:20, function(i){
          sub <- data_outcome[data_outcome$.imp == i,]
          if (length(unique(unlist(sub[n]))) == max(nb_cl)) {
            sub$keep_or_check <- 'keep'
          } else {
            sub$keep_or_check <- 'check'
          }
          sub
        })
        data_outcome <- bind_rows(keep_or_check)
        keep <- select(data_outcome[data_outcome$keep_or_check == 'keep',], -c('keep_or_check'))
        check <- select(data_outcome[data_outcome$keep_or_check == 'check',], -c('keep_or_check'))
        keep_agg <- aggregate(unlist(keep[paste0(out_id[[outcome]], "52")]), list(unlist(keep[n])), mean)
        names(keep_agg) <- c('cluster_k', 'm_k')
        agg_check <- aggregate(unlist(check[paste0(out_id[[outcome]], "52")]), list(unlist(check[n]), check$.imp), mean)
        names(agg_check) <- c('cluster_c', '.imp', 'm_c')
        def_new_cl <- lapply(unique(agg_check$.imp), function(i){
          combin <- as.data.frame(combn(1:max(nb_cl), length(unique(agg_check[agg_check$.imp == i,]$cluster_c))))
          reshappp <- lapply(1:ncol(combin), function(cc){
            data.frame('comb' = cc, 'cluster_c' = 1:length(unique(agg_check[agg_check$.imp == i,]$cluster_c)), 'cluster_k' = combin[,cc])
          })
          sub <- bind_rows(reshappp)
          sub <- merge(sub, agg_check[agg_check$.imp == i,], by = 'cluster_c', all = TRUE)
          sub <- merge(sub, keep_agg, by = 'cluster_k', all = TRUE)
          sub$d <- abs(sub$m_k - sub$m_c)
          sub_agg <- aggregate(sub$d, list(sub$comb), sum)
          names(sub_agg) <- c('comb', 'sum_d')
          sub_final <- sub[sub$comb == sub_agg[sub_agg$sum_d == min(sub_agg$sum_d), ]$comb,]
          old_cl <- sub_final$cluster_c
          new_cl <- sub_final$cluster_k
          old_cl <- c(old_cl, new_cl[!(new_cl %in% old_cl)])
          new_cl <- c(new_cl, old_cl[!(old_cl %in% new_cl)])
          sub_check <- check[check$.imp == i,]
          index_vector <- match(unlist(sub_check[n]), old_cl)
          sub_check[n] <- new_cl[index_vector] #replace(unlist(sub_check[n]), unlist(sub_check[n]) %in% old_cl, new_cl)
          
          old_cl <- c(old_cl, new_cl[!(new_cl %in% old_cl)])
          new_cl <- c(new_cl, old_cl[!(old_cl %in% new_cl)])
          
          rename_col_probs_p <- NULL
          nms_p <- NULL
          rename_col_probs_a <- NULL
          nms_a <- NULL
          
          for (o in 1:length(old_cl)){
            if (paste0(n, "-post-probs-", old_cl[o]) %in% colnames(sub_check)){
              if (is.null(rename_col_probs_p)){
                rename_col_probs_p <- paste0(n, "-post-probs-", new_cl[o])
                nms_p <- paste0(n, "-post-probs-", old_cl[o])
              } else {
                rename_col_probs_p <- c(rename_col_probs_p, paste0(n, "-post-probs-", new_cl[o]))
                nms_p <- c(nms_p, paste0(n, "-post-probs-", old_cl[o]))
              }
              names(rename_col_probs_p) <- nms_p
            }
            
            if (paste0(n, "-alloc-probs-", old_cl[o]) %in% colnames(sub_check)){
              if (is.null(rename_col_probs_a)){
                rename_col_probs_a <- paste0(n, "-alloc-probs-", new_cl[o])
                nms_a <- paste0(n, "-alloc-probs-", old_cl[o])
              } else {
                rename_col_probs_a <- c(rename_col_probs_a, paste0(n, "-alloc-probs-", new_cl[o]))
                nms_a <- c(nms_a, paste0(n, "-alloc-probs-", old_cl[o]))
              }
              names(rename_col_probs_a) <- nms_a
            }
            
          }
          
          colnames(sub_check)[colnames(sub_check) %in% names(rename_col_probs_p)] <- unlist(lapply(colnames(sub_check)[colnames(sub_check) %in% names(rename_col_probs_p)], function(np){
            nn <- rename_col_probs_p[np]
            names(nn) <- NULL
            nn
          }))
          
          colnames(sub_check)[colnames(sub_check) %in% names(rename_col_probs_a)] <- unlist(lapply(colnames(sub_check)[colnames(sub_check) %in% names(rename_col_probs_a)], function(na){
            nn <- rename_col_probs_a[na]
            names(nn) <- NULL
            nn
          }))
          
          sub_check[, colnames(keep)]
          
        })
        new_check <- bind_rows(def_new_cl)
        data_outcome <- rbind(keep, new_check)
      }
    }
  }
  
  if ('keep_or_check' %in% colnames(data_outcome)){
    data_outcome <- select(data_outcome, -c('keep_or_check'))
  }
  
  write_xlsx(select(data_outcome[data_outcome$.imp == 0,], -c(".imp")), paste0("C:\\Users\\LAURA\\Documents\\ETHZ\\Master\\Master thesis\\SCI project\\code - final version - 2\\Sensitivity analysis\\data_sensitivity_analysis - ", min_non_NA, "\\data_full_", outcome, "_renamed2.xlsx"))
  write_xlsx(data_outcome[data_outcome$.imp != 0,], paste0("C:\\Users\\LAURA\\Documents\\ETHZ\\Master\\Master thesis\\SCI project\\code - final version - 2\\Sensitivity analysis\\data_sensitivity_analysis - ", min_non_NA, "\\data_imputed_", outcome, "_renamed2.xlsx"))
  
})


# also need to adapt eval scores
lapply(c('lems', 'uems', 'ltscore', 'ppscore', 'modben'), function(outcome){
  cols_imp <- c('.imp',	'G',	'Calinski.Harabatz',	'Calinski.Harabatz2',	'Calinski.Harabatz3',	'Ray.Turi',	'Davies.Bouldin',	'BIC',	'BIC2',	'AIC',	'AICc',	'AICc2',	'postProbaGlobal',	'random')
  cols_full <- c('G',	'Calinski.Harabatz',	'Calinski.Harabatz2',	'Calinski.Harabatz3',	'Ray.Turi',	'Davies.Bouldin',	'BIC',	'BIC2',	'AIC',	'AICc',	'AICc2',	'postProbaGlobal',	'random')
  lapply(c("lpa", "kmeans"), function(meth){
    q_imp <- read_excel(paste0("C:\\Users\\LAURA\\Documents\\ETHZ\\Master\\Master thesis\\SCI project\\code - final version - 2\\Sensitivity analysis\\data_sensitivity_analysis - ", min_non_NA, "\\", meth, "_quality_criteria_imputed_", outcome, "_renamed.xlsx"))
    write_xlsx(q_imp[, cols_imp], paste0("C:\\Users\\LAURA\\Documents\\ETHZ\\Master\\Master thesis\\SCI project\\code - final version - 2\\Sensitivity analysis\\data_sensitivity_analysis - ", min_non_NA, "\\", meth, "_quality_criteria_imputed_", outcome, "_renamed2.xlsx"))
    q_full <- read_excel(paste0("C:\\Users\\LAURA\\Documents\\ETHZ\\Master\\Master thesis\\SCI project\\code - final version - 2\\Sensitivity analysis\\data_sensitivity_analysis - ", min_non_NA, "\\", meth, "_quality_criteria_full_", outcome, "_renamed.xlsx"))
    write_xlsx(q_full[, cols_full], paste0("C:\\Users\\LAURA\\Documents\\ETHZ\\Master\\Master thesis\\SCI project\\code - final version - 2\\Sensitivity analysis\\data_sensitivity_analysis - ", min_non_NA, "\\", meth, "_quality_criteria_full_", outcome, "_renamed2.xlsx"))
  })
})
