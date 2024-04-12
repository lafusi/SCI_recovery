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

setwd(paste0("C:/Users/LAURA/Documents/ETHZ/Master/Master thesis/SCI project/code - final version - 2/Main analysis/results - ", min_non_NA))

lapply(c('lems', 'uems', 'ltscore', 'ppscore', 'modben'), function(outcome){
  data_outcome_imputed <- read_xlsx(paste0("C:\\Users\\LAURA\\Documents\\ETHZ\\Master\\Master thesis\\SCI project\\code - final version - 2\\Main analysis\\results - ", min_non_NA, "\\data_imputed_", outcome, "_renamed2.xlsx"))
  
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
          data_outcome_imputed <- data_outcome_imputed
          meth <- meth
          g <- g
          anovares <- mi.anova(micemids, "age ~ as.factor(cluster)")
          p.value <- anovares$anova.table['as.factor(cluster)', 'Pr(>F)']
          test.stat <- anovares$anova.table['as.factor(cluster)', 'F value']
          dof.tot <- unlist(anovares$anova.table['as.factor(cluster)', c('df1', 'df2')])
          
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
          test.stat <- chires['D']
          dof.tot <- chires[c('df', 'df2')]
        }
        ggmm[col] <- as.character(Reduce(function(x,y){paste(x,y,sep=", ")}, dof.tot))
      }
      ggmm
    })
    bind_rows(gg)
  })
  imp_res <- bind_rows(mm)
  
  write_xlsx(imp_res, paste0("C:\\Users\\LAURA\\Documents\\ETHZ\\Master\\Master thesis\\SCI project\\code - final version - 2\\Main analysis\\results - ", min_non_NA, "\\descriptive_statistics_results_dof_", outcome, "_2.xlsx"))
})