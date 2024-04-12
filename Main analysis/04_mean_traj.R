library(ggplot2)
library(readxl)
library(dplyr)
#library(dichromat)
library(cowplot)
library(grid)
#library(ggplotify)
#library(magick)
library(png)
library(mice)
library(miceadds)
library(writexl)

min_non_NA <- 2

setwd(paste0("C:/Users/LAURA/Documents/ETHZ/Master/Master thesis/SCI project/code - final version - 2/Main analysis/results - ", min_non_NA))

lapply(c('lems', 'uems', 'ltscore', 'ppscore', 'modben'), function(outcome){
  tot_scores <- list('lems' = paste0('lower', c('01', '04', '08', '16', '26', '52')), 'uems' = paste0('upper', c('01', '04', '08', '16', '26', '52')), 'ltscore' = paste0('ltscor', c('01', '04', '08', '16', '26', '52')), 'ppscore' = paste0('ppscor', c('01', '04', '08', '16', '26', '52')), 'modben' = paste0('modben', c('04', '08', '16', '26', '52')))[[outcome]]
  imputed <- read_xlsx(paste0("C:\\Users\\LAURA\\Documents\\ETHZ\\Master\\Master thesis\\SCI project\\code - final version - 2\\Main analysis\\results - ", min_non_NA, "\\data_imputed_", outcome, "_renamed2.xlsx"))
  imputed <- imputed %>% relocate('.imp')
  
  df <- imputed
  meth_list <- lapply(c("lpa", "kmeans"), function(meth){
    g_list <- lapply(2:6, function(G){
      selection_df <- df[, c('.imp', tot_scores, paste0(meth, "-clusters-", G))]
      selection_df$G <- G
      names(selection_df) <- c('.imp', tot_scores, 'alloc', "G")
      selection_df
    })
    meth_df <- bind_rows(g_list)
    meth_df$method <- meth
    meth_df
  })
  tot_df <- bind_rows(meth_list)
  tot_df <- tot_df[, c(".imp", tot_scores, "method", "G", 'alloc')]
  w_list <- lapply(tot_scores, function(w){
    w_df <- tot_df[, c(".imp", "method", "G", 'alloc', w)]
    names(w_df) <- c(".imp", "method", "G", 'alloc', 'outcome')
    w_df$week <- as.integer(substr(w, nchar(w)-1, nchar(w)))
    w_df
  })
  imp_end_df <- bind_rows(w_list)
  mm <- lapply(unique(imp_end_df$method), function(meth){
    gg <- lapply(unique(imp_end_df$G), function(g){
      ww <- lapply(unique(imp_end_df$week), function(w){
        w_imp_df <- imp_end_df[(imp_end_df$method == meth)&(imp_end_df$G == g)&(imp_end_df$week == w),]
        aa <- lapply(unique(w_imp_df$alloc), function(a){
          imps <- lapply(1:20, function(i){
            d <- w_imp_df[w_imp_df$.imp == i,]
            #d$alloc <- factor(as.character(d$alloc), levels = as.character(1:max(w_imp_df$alloc)))
            #d$alloc <- relevel(d$alloc, ref = as.character(a))
            d
          })
          micemids <- datlist2mids(imps)
          withres <- with(micemids, lm(outcome~factor(alloc, levels = c(as.character(a), as.character(1:max(w_imp_df$alloc))[as.character(1:max(w_imp_df$alloc)) != as.character(a)])))) # https://thestatsgeek.com/2021/09/22/summary-statistics-after-imputation-with-mice/
          poolres <- pool(withres)
          summary_df <- as.data.frame(summary(poolres))
          rownames(summary_df) <- NULL
          summary_df <- summary_df[, c("estimate", "std.error")]
          summary_df$week <- w
          summary_df$alloc <- c(as.character(a), as.character(1:max(w_imp_df$alloc))[as.character(1:max(w_imp_df$alloc)) != as.character(a)])
          summary_df$G <- g
          summary_df$method <- meth
          summary_df[1,]
        })
        bind_rows(aa)
      })
      bind_rows(ww)
    })
    bind_rows(gg)
  })
  agg_imp <- bind_rows(mm)
  
  simple_list <- lapply(tot_scores, function(ts){
    long_df <- df[, c('.imp', 'ptid', ts)]
    names(long_df) <- c('.imp', 'ptid', 'outcome')
    long_df$week <- as.integer(substr(ts, nchar(ts)-1, nchar(ts)))
    long_df
  })
  simple_df <- bind_rows(simple_list)
  www <- lapply(unique(agg_imp$week), function(w){
    imps <- lapply(1:20, function(i){
      simple_df[(simple_df$.imp == i)&(simple_df$week == w),]
    })
    micemids <- datlist2mids(imps)
    withres <- with(micemids, lm(outcome~1)) # https://thestatsgeek.com/2021/09/22/summary-statistics-after-imputation-with-mice/
    poolres <- pool(withres)
    summary_df <- as.data.frame(summary(poolres))
    rownames(summary_df) <- NULL
    summary_df <- summary_df[, c("estimate", "std.error")]
    summary_df$week <- w
    summary_df$alloc <- 1
    summary_df$G <- 1
    summary_df$method <- 'none'
    summary_df
  })
  
  agg_imp <- rbind(agg_imp, bind_rows(www))
  
  write_xlsx(agg_imp, paste0("C:\\Users\\LAURA\\Documents\\ETHZ\\Master\\Master thesis\\SCI project\\code - final version - 2\\Main analysis\\results - ", min_non_NA, "\\agg_", outcome, "_2.xlsx"))
})