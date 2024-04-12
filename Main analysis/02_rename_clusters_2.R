# The previous file renamed clusters so that the "best" cluster had the largest number
# In imputed datasets, it might be that the number of clusters is not consistent across datasets
# If this is the case, it might be that still a cluster with a certain number in a certain imputed dataset does not match the cluster with the same number in another dataset, resulting in "weird" results in the following analyses anyway

library(readxl)
library(writexl)
library(dplyr)
library(combinat)
library(longitudinalData)

min_non_NA <- 2

out_cols <- list('lems' = c('lower01', 'lower04', 'lower08', 'lower16', 'lower26', 'lower52'),
                 'uems' = c('upper01', 'upper04', 'upper08', 'upper16', 'upper26', 'upper52'),
                 'ltscore' = c('ltscor01', 'ltscor04', 'ltscor08', 'ltscor16', 'ltscor26', 'ltscor52'),
                 'ppscore' = c('ppscor01', 'ppscor04', 'ppscor08', 'ppscor16', 'ppscor26', 'ppscor52'),
                 'modben' = c('modben04', 'modben08', 'modben16', 'modben26', 'modben52'))

lapply(c('lems', 'uems', 'ltscore', 'ppscore', 'modben'), function(outcome){
  out_id <- list('lems' = 'lower', 'uems' = 'upper', 'ltscore' = 'ltscor', 'ppscore' = 'ppscor', 'modben' = 'modben')
  data_outcome_imputed <- read_xlsx(paste0("C:\\Users\\LAURA\\Documents\\ETHZ\\Master\\Master thesis\\SCI project\\code - final version - 2\\Main analysis\\results - ", min_non_NA, "\\data_imputed_", outcome, "_renamed.xlsx"))
  all_names <- c("kmeans", "lpa")
  col_names <- lapply(2:6, function(g){
    paste0(all_names, "-clusters-", g)
  })
  col_names <- unlist(col_names)
  
  for (n in col_names) {
    if (n %in% colnames(data_outcome_imputed)){
      nb_cl <- unlist(lapply(1:20, function(i){
        length(unique(unlist(data_outcome_imputed[data_outcome_imputed$.imp == i, n])))
      }))
      if (length(unique(nb_cl)) != 1) {
        keep_or_check <- lapply(1:20, function(i){
          sub <- data_outcome_imputed[data_outcome_imputed$.imp == i,]
          if (length(unique(unlist(sub[n]))) == max(nb_cl)) {
            sub$keep_or_check <- 'keep'
          } else {
            sub$keep_or_check <- 'check'
          }
          sub
        })
        data_outcome_imputed <- bind_rows(keep_or_check)
        keep <- select(data_outcome_imputed[data_outcome_imputed$keep_or_check == 'keep',], -c('keep_or_check'))
        check <- select(data_outcome_imputed[data_outcome_imputed$keep_or_check == 'check',], -c('keep_or_check'))
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
        data_outcome_imputed <- rbind(keep, new_check)
      }
    }
  }
  
  if ('keep_or_check' %in% colnames(data_outcome_imputed)){
    data_outcome_imputed <- select(data_outcome_imputed, -c('keep_or_check'))
  }
  
  write_xlsx(data_outcome_imputed, paste0("C:\\Users\\LAURA\\Documents\\ETHZ\\Master\\Master thesis\\SCI project\\code - final version - 2\\Main analysis\\results - ", min_non_NA, "\\data_imputed_", outcome, "_renamed2.xlsx"))
  
})


# also need to adapt eval scores
lapply(c('lems', 'uems', 'ltscore', 'ppscore', 'modben'), function(outcome){
  cols <- c('.imp',	'G',	'Calinski.Harabatz',	'Calinski.Harabatz2',	'Calinski.Harabatz3',	'Ray.Turi',	'Davies.Bouldin',	'BIC',	'BIC2',	'AIC',	'AICc',	'AICc2',	'postProbaGlobal',	'random')
  lapply(c("lpa", "kmeans"), function(meth){
    q <- read_excel(paste0("C:\\Users\\LAURA\\Documents\\ETHZ\\Master\\Master thesis\\SCI project\\code - final version - 2\\Main analysis\\results - ", min_non_NA, "\\", meth, "_quality_criteria_imputed_", outcome, "_renamed.xlsx"))
    write_xlsx(q[, cols], paste0("C:\\Users\\LAURA\\Documents\\ETHZ\\Master\\Master thesis\\SCI project\\code - final version - 2\\Main analysis\\results - ", min_non_NA, "\\", meth, "_quality_criteria_imputed_", outcome, "_renamed2.xlsx"))
  })
})

