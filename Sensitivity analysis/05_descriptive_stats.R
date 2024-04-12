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
      ggmm <- data.frame("method" = meth, "G" = g)
      for (col in c('sexcd', 'asimpc01', 'age', 'tx1_r', 'nligr1', 'lvlgr', "Famotidine_critical", "Ranitidine_critical", "Cimetidine_critical", 'bmi_status')) {
        imps <- lapply(1:20, function(i){
          d <- data_outcome_imputed[data_outcome_imputed$.imp == i,]
          #maxi <- max(unlist(data_outcome_imputed[paste0(meth, "-clusters-", g)]))
          #d$cluster <- factor(as.character(unlist(d[paste0(meth, "-clusters-", g)])), levels = as.character(1:maxi))
          #if (col != 'age'){
          #  d$group <- as.factor(as.character(unlist(d[col])))
          #}
          d$cluster <- as.character(unlist(d[paste0(meth, "-clusters-", g)]))
          d$group <- as.character(unlist(d[col]))
          d
        })
        micemids <- datlist2mids(imps)
        if (col == 'age'){ # ANOVA for continuous variables
          anovares <- mi.anova(micemids, "age ~ as.factor(cluster)")
          p.value <- anovares$anova.table['as.factor(cluster)', 'Pr(>F)']
        } else { # Chi-square test for discrete variables
          #chisq <- with(micemids, chisq.test(xtabs(~ group + cluster)))
          statvector <- unlist(lapply(1:20, function(i){
            xtab <- xtabs(~ as.factor(group) + factor(cluster, levels = as.character(1:max(unlist(data_outcome_imputed[paste0(meth, "-clusters-", g)])))), data = imps[[i]])
            xsq <- chisq.test(xtab)
            xsq$statistic
          }))
          dof <- unique(unlist(lapply(1:20, function(i){
            xtab <- xtabs(~ as.factor(group) + factor(cluster, levels = as.character(1:max(unlist(data_outcome_imputed[paste0(meth, "-clusters-", g)])))), data = imps[[i]])
            xsq <- chisq.test(xtab)
            xsq$parameter
          })))
          chires <- micombine.chisquare(dk = statvector, df = dof) # if the result is NA, it means that not all the imputed datasets had the same number of clusters, eg some had one empty cluster, other did not
          p.value <- chires['p']
        }
        ggmm[col] <- p.value
      }
      ggmm
    })
    bind_rows(gg)
  })
  imp_res <- bind_rows(mm)
  
  # analysis with observed data
  mm <- lapply(c("lpa", "kmeans"), function(meth){
    gg <- lapply(2:6, function(g){
      ggmm <- data.frame("method" = meth, "G" = g)
      for (col in c('sexcd', 'asimpc01', 'age', 'tx1_r', 'nligr1', 'lvlgr', "Famotidine_critical", "Ranitidine_critical", "Cimetidine_critical", 'bmi_status')) {
        data_outcome_full$cluster <- factor(as.character(unlist(data_outcome_full[paste0(meth, "-clusters-", g)])), levels = as.character(1:max(unlist(data_outcome_full[paste0(meth, "-clusters-", g)]))))
        if (col == 'age'){ # ANOVA for continuous variables
          anovaTest <- aov(age ~ cluster, data = data_outcome_full)
          p.value <- summary(anovaTest)[[1]][["Pr(>F)"]][1]
        } else { # Chi-square test for discrete variables
          data_outcome_full$group <- as.factor(as.character(unlist(data_outcome_full[col])))
          xtab <- xtabs(~ group + cluster, data = data_outcome_full)
          xsq <- chisq.test(xtab)
          p.value <- xsq$p.value
        }
        ggmm[col] <- p.value
      }
      ggmm
    })
    bind_rows(gg)
  })
  obs_res <- bind_rows(mm)
  
  
  imp_res$imp <- 'imputed'
  obs_res$imp <- 'observed'
  all_res <- rbind(imp_res, obs_res)
  newshape_list <- lapply(c('sexcd', 'asimpc01', 'age', 'tx1_r', 'nligr1', 'lvlgr', "Famotidine_critical", "Ranitidine_critical", "Cimetidine_critical", 'bmi_status'), function(col){
    newdf <- all_res[, c('method', 'G', 'imp', col)]
    colnames(newdf) <- c('method', 'G', 'imp', 'p.value')
    newdf$variable <- col
    newdf %>% relocate('variable', .before = 'p.value')
  })
  new_res <- bind_rows(newshape_list)
  new_res$method <- ifelse(new_res$method == 'kmeans', "k-Means", "LPA")
  new_res$methimp <- paste0(new_res$method, " - ", new_res$imp)
  
  png(paste0("C:/Users/LAURA/Documents/ETHZ/Master/Master thesis/SCI project/code - final version - 2/Sensitivity analysis/data_sensitivity_analysis - ", min_non_NA, "/descriptive_statistics_results_", outcome, ".png"), width = 2000, height = 1000)
  ggplot(data = new_res, mapping = aes(x = G, y = p.value, color = methimp, group = methimp)) + geom_line() + scale_color_manual(values = c('k-Means - observed' = "red", 'k-Means - imputed' = "#990000", 'LPA - observed' = "blue", 'LPA - imputed' = "#00008b")) + facet_wrap(~variable, labeller = labeller(variable = c('sexcd' = "Sex", 'asimpc01' = 'AIS Grade at Baseline', 'age' = "Age", 'tx1_r' = "Treatment Group", 'nligr1' = "Neurological Level of Injury at Baseline", 'lvlgr' = "Spinal Level", "Famotidine_critical" = "Famotidine Exposure \n during the First 7 Days after Injury", "Ranitidine_critical" = "Ranitidine Exposure \n during the First 7 Days after Injury", "Cimetidine_critical" = "Cimetidine Exposure \n during the First 7 Days after Injury", 'bmi_status' = 'Weight Status (BMI)'))) + labs(x = 'Number of Clusters', y = 'p-value', color = 'Method', group = "Method")
  dev.off()
  
  write_xlsx(all_res, paste0("C:\\Users\\LAURA\\Documents\\ETHZ\\Master\\Master thesis\\SCI project\\code - final version - 2\\Sensitivity analysis\\data_sensitivity_analysis - ", min_non_NA, "\\descriptive_statistics_results_", outcome, "_2.xlsx"))
})