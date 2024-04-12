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

setwd(paste0("C:/Users/LAURA/Documents/ETHZ/Master/Master thesis/SCI project/code - final version - 2/Sensitivity analysis/data_sensitivity_analysis - ", min_non_NA))

lapply(c('lems', 'uems', 'ltscore', 'ppscore', 'modben'), function(outcome){
  data_outcome_imputed <- read_xlsx(paste0("C:\\Users\\LAURA\\Documents\\ETHZ\\Master\\Master thesis\\SCI project\\code - final version - 2\\Sensitivity analysis\\data_sensitivity_analysis - ", min_non_NA, "\\data_imputed_", outcome, "_renamed2.xlsx"))
  data_outcome_full <- read_xlsx(paste0("C:\\Users\\LAURA\\Documents\\ETHZ\\Master\\Master thesis\\SCI project\\code - final version - 2\\Sensitivity analysis\\data_sensitivity_analysis - ", min_non_NA, "\\data_full_", outcome, "_renamed2.xlsx"))
  
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
  
  # analysis with observed data
  mm <- lapply(c("lpa", "kmeans"), function(meth){
    gg <- lapply(2:6, function(g){
      data_outcome_full$cluster <- factor(as.character(unlist(data_outcome_full[paste0(meth, "-clusters-", g)])), levels = as.character(1:max(unlist(data_outcome_full[paste0(meth, "-clusters-", g)]))))
      model_nomed <- multinom(cluster~sexcd+asimpc01+age+tx1_r+nligr1+bmi_status, data=data_outcome_full)
      model_med <- multinom(cluster~sexcd+asimpc01+age+tx1_r+nligr1+bmi_status+Famotidine_critical+Ranitidine_critical+Cimetidine_critical, data=data_outcome_full)
      # reshape summary
      df_list <- lapply(list(model_nomed, model_med), function(mod){
        cse <- lapply(list(summary(mod)$coefficients, summary(mod)$standard.errors), function(summary_part){
          if (nrow(data.frame(summary_part)) == length(mod$coefnames)) {
            df <- data.frame(t(summary_part))
            if (g == 2) {
              df$y.level <- 2
            } else {
              df$y.level <- row.names(df)
            }
            colnames(df)[1] <- "(Intercept)"
            df <- relocate(df, "y.level")
          } else {
            df <- as.data.frame(summary_part)
            df$y.level <- row.names(df)
            df <- relocate(df, "y.level")
          }
          
          colnames(df) <- c('y.level', mod$coefnames)
          reshaped_subdfs <- lapply(mod$coefnames, function(cn){
            subdf <- df[, c('y.level', cn)]
            colnames(subdf) <- c('y.level', 'estimate')
            subdf$term <- cn
            rownames(subdf) <- NULL
            subdf
          })
          bind_rows(reshaped_subdfs)
        })
        colnames(cse[[2]]) <- c('y.level', 'std.error', 'term')
        df_reshaped <- Reduce(full_join,cse)
        df_reshaped$statistic <- df_reshaped$estimate/df_reshaped$std.error
        df_reshaped$p.value <- (1 - pnorm(abs(df_reshaped$statistic)))*2
        df_reshaped$ci_lower <- df_reshaped$estimate - 1.96*df_reshaped$std.error
        df_reshaped$ci_upper <- df_reshaped$estimate + 1.96*df_reshaped$std.error
        df_reshaped <- df_reshaped[, c('y.level', 'term', 'estimate', 'ci_lower', 'ci_upper', 'p.value')]
        df_reshaped$estimate <- exp(df_reshaped$estimate)
        df_reshaped$ci_upper <- exp(df_reshaped$ci_upper)
        df_reshaped$ci_lower <- exp(df_reshaped$ci_lower)
        df_reshaped$G <- g
        df_reshaped$method <- meth
        df_reshaped$y.level <- as.factor(as.character(df_reshaped$y.level))
        df_reshaped
      })
      df_list
    })
    list(bind_rows(lapply(gg, function(x){x[[1]]})), bind_rows(lapply(gg, function(x){x[[2]]})))
  })
  obs_res_nomed <- bind_rows(lapply(mm, function(x){x[[1]]}))[, colnames(imp_res_nomed)]
  obs_res_med <- bind_rows(lapply(mm, function(x){x[[2]]}))[, colnames(imp_res_med)]
  
  
  imp_res_med$imp <- 'imputed'
  obs_res_med$imp <- 'observed'
  all_res_med <- rbind(imp_res_med, obs_res_med)
  imp_res_nomed$imp <- 'imputed'
  obs_res_nomed$imp <- 'observed'
  all_res_nomed <- rbind(imp_res_nomed, obs_res_nomed)
  
  all_res_med_new <- all_res_med
  all_res_nomed_new <- all_res_nomed
  
  all_res_med_new$method <- ifelse(all_res_med_new$method == 'kmeans', "k-Means", "LPA")
  all_res_med_new$methimp <- paste0(all_res_med_new$method, " - ", all_res_med_new$imp)
  all_res_nomed_new$method <- ifelse(all_res_nomed_new$method == 'kmeans', "k-Means", "LPA")
  all_res_nomed_new$methimp <- paste0(all_res_nomed_new$method, " - ", all_res_nomed_new$imp)
  
  png(paste0("C:/Users/LAURA/Documents/ETHZ/Master/Master thesis/SCI project/code - final version - 2/Sensitivity analysis/data_sensitivity_analysis - ", min_non_NA, "/multinom_results_med_", outcome, ".png"), width = 2000, height = 1000)
  ggplot(data = all_res_med_new, mapping = aes(x = G, y = p.value, color = methimp, group = methimp)) + geom_line() + scale_color_manual(values = c('k-Means - observed' = "red", 'k-Means - imputed' = "#990000", 'LPA - observed' = "blue", 'LPA - imputed' = "#00008b")) + facet_wrap(~variable, labeller = labeller(variable = c('sexcd' = "Sex", 'asimpc01' = 'AIS Grade at Baseline', 'age' = "Age", 'tx1_r' = "Treatment Group", 'nligr1' = "Neurological Level of Injury at Baseline", 'lvlgr' = "Spinal Level", "Famotidine_critical" = "Famotidine Exposure \n during the First 7 Days after Injury", "Ranitidine_critical" = "Ranitidine Exposure \n during the First 7 Days after Injury", "Cimetidine_critical" = "Cimetidine Exposure \n during the First 7 Days after Injury", 'bmi_status' = 'Weight Status (BMI)'))) + labs(x = 'Number of Clusters', y = 'p-value', color = 'Method', group = "Method")
  dev.off()
  
  png(paste0("C:/Users/LAURA/Documents/ETHZ/Master/Master thesis/SCI project/code - final version - 2/Sensitivity analysis/data_sensitivity_analysis - ", min_non_NA, "/multinom_results_nomed_", outcome, ".png"), width = 2000, height = 1000)
  ggplot(data = all_res_nomed_new, mapping = aes(x = G, y = p.value, color = methimp, group = methimp)) + geom_line() + scale_color_manual(values = c('k-Means - observed' = "red", 'k-Means - imputed' = "#990000", 'LPA - observed' = "blue", 'LPA - imputed' = "#00008b")) + facet_wrap(~variable, labeller = labeller(variable = c('sexcd' = "Sex", 'asimpc01' = 'AIS Grade at Baseline', 'age' = "Age", 'tx1_r' = "Treatment Group", 'nligr1' = "Neurological Level of Injury at Baseline", 'lvlgr' = "Spinal Level", "Famotidine_critical" = "Famotidine Exposure \n during the First 7 Days after Injury", "Ranitidine_critical" = "Ranitidine Exposure \n during the First 7 Days after Injury", "Cimetidine_critical" = "Cimetidine Exposure \n during the First 7 Days after Injury", 'bmi_status' = 'Weight Status (BMI)'))) + labs(x = 'Number of Clusters', y = 'p-value', color = 'Method', group = "Method")
  dev.off()
  
  write_xlsx(all_res_med, paste0("C:\\Users\\LAURA\\Documents\\ETHZ\\Master\\Master thesis\\SCI project\\code - final version - 2\\Sensitivity analysis\\data_sensitivity_analysis - ", min_non_NA, "\\multinom_results_med_", outcome, "_2.xlsx"))
  write_xlsx(all_res_nomed, paste0("C:\\Users\\LAURA\\Documents\\ETHZ\\Master\\Master thesis\\SCI project\\code - final version - 2\\Sensitivity analysis\\data_sensitivity_analysis - ", min_non_NA, "\\multinom_results_nomed_", outcome, "_2.xlsx"))
})