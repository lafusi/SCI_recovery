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

setwd(paste0("C:/Users/LAURA/Documents/ETHZ/Master/Master thesis/SCI project/code - final version - 2/Sensitivity analysis/data_sensitivity_analysis - ", min_non_NA))

lapply(c('lems', 'uems', 'ltscore', 'ppscore', 'modben'), function(outcome){
  tot_scores <- list('lems' = paste0('lower', c('01', '04', '08', '16', '26', '52')), 'uems' = paste0('upper', c('01', '04', '08', '16', '26', '52')), 'ltscore' = paste0('ltscor', c('01', '04', '08', '16', '26', '52')), 'ppscore' = paste0('ppscor', c('01', '04', '08', '16', '26', '52')), 'modben' = paste0('modben', c('04', '08', '16', '26', '52')))[[outcome]]
  true <- read_xlsx(paste0("C:\\Users\\LAURA\\Documents\\ETHZ\\Master\\Master thesis\\SCI project\\code - final version - 2\\Sensitivity analysis\\data_sensitivity_analysis - ", min_non_NA, "\\data_full_", outcome, "_renamed2.xlsx"))
  imputed <- read_xlsx(paste0("C:\\Users\\LAURA\\Documents\\ETHZ\\Master\\Master thesis\\SCI project\\code - final version - 2\\Sensitivity analysis\\data_sensitivity_analysis - ", min_non_NA, "\\data_imputed_", outcome, "_renamed2.xlsx"))
  true$.imp <- "observed"
  #imputed$.imp <- "imputed"
  true <- true %>% relocate('.imp')
  imputed <- imputed %>% relocate('.imp')
  
  df <- rbind(true, imputed)
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
  end_df <- bind_rows(w_list)
  imp_end_df <- end_df[end_df$.imp != 'observed',]
  mm <- lapply(unique(imp_end_df$method), function(meth){
    gg <- lapply(unique(imp_end_df$G), function(g){
      ww <- lapply(unique(imp_end_df$week), function(w){
        w_imp_df <- imp_end_df[(imp_end_df$method == meth)&(imp_end_df$G == g)&(imp_end_df$week == w),]
        w_imp_df$.imp <- as.numeric(w_imp_df$.imp)
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
          summary_df <- summary_df[1, c("estimate", "std.error")]
          summary_df$week <- w
          summary_df$alloc <- as.character(a) #c(as.character(a), as.character(1:max(w_imp_df$alloc))[as.character(1:max(w_imp_df$alloc)) != as.character(a)])
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
  agg_imp$imp <- 'imputed'
  
  obs_end_df <- end_df[end_df$.imp == 'observed',]
  mm <- lapply(unique(obs_end_df$method), function(meth){
    gg <- lapply(unique(obs_end_df$G), function(g){
      ww <- lapply(unique(obs_end_df$week), function(w){
        w_obs_df <- obs_end_df[(obs_end_df$method == meth)&(obs_end_df$G == g)&(obs_end_df$week == w),]
        aa <- lapply(unique(w_obs_df$alloc), function(a){
          w_obs_df$alloc <- as.factor(as.character(w_obs_df$alloc))
          w_obs_df$alloc <- relevel(w_obs_df$alloc, ref = as.character(a))
          res <- lm(outcome~alloc, data = w_obs_df)
          summary_df <- as.data.frame(coef(summary(res)))
          rownames(summary_df) <- NULL
          summary_df <- summary_df[, c("Estimate", "Std. Error")]
          colnames(summary_df) <- c("estimate", "std.error")
          summary_df$week <- w
          summary_df$alloc <- levels(w_obs_df$alloc)
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
  agg_obs <- bind_rows(mm)
  agg_obs$imp <- 'observed'
  
  agg <- rbind(agg_obs, agg_imp)
  agg2 <- agg
  agg2$method <- ifelse(agg2$method == 'kmeans', "k-Means", "LPA")
  agg2$methcl <- paste0(agg2$method, " - ", agg2$G, " clusters")
  agg2$impalloc <- paste0("cluster ", agg2$alloc, " - ", agg2$imp)
  colors_imputed <- c("1" = "#DDCC77", "2" = "#117733", "3" = "#44AA99", "4" = "#999933", "5" = "#6699CC", "6" = "#888888")
  colors_observed <- c("1" = "#b0a35f", "2" = "#0b5323", "3" = "#2f766b", "4" = "#6b6b23", "5" = "#476b8e", "6" = "#5f5f5f")
  names(colors_imputed) <- paste0("cluster ", names(colors_imputed), " - ", "imputed")
  names(colors_observed) <- paste0("cluster ", names(colors_observed), " - ", "observed")
  out_label <- list('lems' = "Lower Extremities Motor Score", 'uems' = "Upper Extremities Motor Score", 'ltscore' = "Light Touch Total Score", "ppscore" = "Pin Prick Total Score", "modben" = "Modified Benzel Scale")[[outcome]]
  brks <- list('lems' = c(0, 10, 20, 30, 40, 50), 'uems' = c(0, 10, 20, 30, 40, 50), 'ltscore' = c(0, 20, 40, 60, 80, 100, 120), 'ppscore' = c(0, 20, 40, 60, 80, 100, 120), 'modben' = c(1, 2, 3, 4, 5, 6, 7))[[outcome]]
  png(paste0("C:/Users/LAURA/Documents/ETHZ/Master/Master thesis/SCI project/code - final version - 2/Sensitivity analysis/data_sensitivity_analysis - ", min_non_NA, "/clusters-", outcome, ".png"), width = 2000, height = 1000)
  ggplot(data = agg2, mapping = aes(x = week, y = estimate, color = impalloc, group = impalloc, fill = impalloc)) + geom_line() + geom_ribbon(aes(ymin = estimate - 1.96*std.error, ymax = estimate + 1.96*std.error), alpha = 0.3, color = NA) + scale_color_manual(values = c(colors_observed, colors_imputed)) + scale_fill_manual(values = c(colors_observed, colors_imputed)) + facet_wrap(~methcl, nrow = 2) + labs(x = "Time after Injury [Weeks]", y = out_label, color = "Cluster", fill = "Cluster") + scale_y_continuous(breaks = brks)
  dev.off()
  
  simple_list <- lapply(tot_scores, function(ts){
    long_df <- df[, c('.imp', 'ptid', ts)]
    names(long_df) <- c('.imp', 'ptid', 'outcome')
    long_df$week <- as.integer(substr(ts, nchar(ts)-1, nchar(ts)))
    long_df
  })
  simple_df <- bind_rows(simple_list)
  www <- lapply(unique(agg$week), function(w){
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
    summary_df$imp <- 'imputed'
    summary_df
  })
  
  agg <- rbind(agg, bind_rows(www))
  
  www <- lapply(unique(agg$week), function(w){
    obs <- simple_df[(simple_df$.imp == 'observed')&(simple_df$week == w),]
    res <- lm(outcome~1, data = obs)
    summary_df <- as.data.frame(coef(summary(res)))
    rownames(summary_df) <- NULL
    summary_df <- summary_df[, c("Estimate", "Std. Error")]
    colnames(summary_df) <- c("estimate", "std.error")
    summary_df$week <- w
    summary_df$alloc <- 1
    summary_df$G <- 1
    summary_df$method <- 'none'
    summary_df$imp <- 'observed'
    summary_df
  })
  
  agg <- rbind(agg, bind_rows(www))
  
  write_xlsx(agg, paste0("C:\\Users\\LAURA\\Documents\\ETHZ\\Master\\Master thesis\\SCI project\\code - final version - 2\\Sensitivity analysis\\data_sensitivity_analysis - ", min_non_NA, "\\agg_", outcome, "_2.xlsx"))
})