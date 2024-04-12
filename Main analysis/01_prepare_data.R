# Load libraries
library(writexl)
library(readxl)
library(dplyr)
library(data.table)
library(caret)
library(nnet)
library(mice)

# Set working directory
basic_path <- "C:/Users/LAURA/Documents/ETHZ/Master/Master thesis/SCI project/code - final version - 2" 
setwd(basic_path)

# Min number of observed values per trajectories
min_non_NA <- 2

# select
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

# Load data
source("00_dataset.R")

# Set random state for reproducibility
set.seed(2023)

# get partial scores names (and expressions for imputation)
tot_name_v <- list("ltscore" = c(), "ppscore" = c(), "lems" = c(), "uems" = c(), "modben" = c())
part_name_v <- list("ltscore" = c(), "ppscore" = c(), "lems" = c(), "uems" = c(), "modben" = c())
tot_part_name_v <- list("ltscore" = list(), "ppscore" = list(), "lems" = list(), "uems" = list(), "modben" = list())
expr_v <- list("ltscore" = list(), "ppscore" = list(), "lems" = list(), "uems" = list(), "modben" = list())
# sensory scores
for (examination_type in c('lt', 'pp')) {
  for (week in c('01', '04', '08', '16', '26', '52')){
    tot_name <- paste0(examination_type, 'scor', week)
    tot_name_v[[paste0(examination_type, "score")]] <- c(tot_name_v[[paste0(examination_type, "score")]], tot_name)
    tot_part_name_v[[paste0(examination_type, "score")]][[tot_name]] <- c()
    for (level in rev(c('c2', 'c3', 'c4', 'c5', 'c6', 'c7', 'c8', 't1', 't2', 't3', 't4', 't5', 't6', 't7', 't8', 't9', 't10', 't11', 't12', 'l1', 'l2', 'l3', 'l4', 'l5', 's1', 's2', 's3', 's45'))) {
      for (side in c('l', 'r')){
        name <- paste0(level, examination_type, side, week)
        part_name_v[[paste0(examination_type, "score")]] <- c(part_name_v[[paste0(examination_type, "score")]], name)
        tot_part_name_v[[paste0(examination_type, "score")]][[tot_name]] <- c(tot_part_name_v[[paste0(examination_type, "score")]][[tot_name]], name)
      }
    }
    prepre <- paste0("as.integer(", tot_part_name_v[[paste0(examination_type, "score")]][[tot_name]], ")")
    pre <- Reduce(function(x, y){paste(x, y, sep = " + ")}, prepre)
    expr_v[[paste0(examination_type, "score")]][[tot_name]] <- paste0(paste0("~I(", pre, ")"))
  }
}
# uems
for (week in c('01', '04', '08', '16', '26', '52')){
  tot_name <- paste0('upper', week)
  tot_name_v[['uems']] <- c(tot_name_v[['uems']], tot_name)
  for (muscle in c('elbex', 'elbfl', 'finab', 'finfl', 'wrext')) {
    for (side in c('l', 'r')) {
      name <- paste0(muscle, side, week)
      part_name_v[['uems']] <- c(part_name_v[['uems']], name)
      tot_part_name_v[['uems']][[tot_name]] <- c(tot_part_name_v[['uems']][[tot_name]], name)
    }
  }
  prepre <- paste0("as.integer(", tot_part_name_v[['uems']][[tot_name]], ")")
  pre <- Reduce(function(x, y){paste(x, y, sep = " + ")}, prepre)
  expr_v[['uems']][[tot_name]] <- paste0(paste0("~I(", pre, ")"))
}
# lems
for (week in c('01', '04', '08', '16', '26', '52')){
  tot_name <- paste0('lower', week)
  tot_name_v[['lems']] <- c(tot_name_v[['lems']], tot_name)
  for (muscle in c('ankdo', 'ankpl', 'greto', 'hipfl')) {
    for (side in c('l', 'r')) {
      name <- paste0(muscle, side, week)
      part_name_v[['lems']] <- c(part_name_v[['lems']], name)
      tot_part_name_v[['lems']][[tot_name]] <- c(tot_part_name_v[['lems']][[tot_name]], name)
    }
  }
  part_name_v[['lems']] <- c(part_name_v[['lems']], paste0('kneexl', week), paste0('kneetr', week))
  tot_part_name_v[['lems']][[tot_name]] <- c(tot_part_name_v[['lems']][[tot_name]], paste0('kneexl', week), paste0('kneetr', week))
  prepre <- paste0("as.integer(", tot_part_name_v[['lems']][[tot_name]], ")")
  pre <- Reduce(function(x, y){paste(x, y, sep = " + ")}, prepre)
  expr_v[['lems']][[tot_name]] <- paste0(paste0("~I(", pre, ")"))
}
# modben
for (week in c('04', '08', '16', '26', '52')){
  tot_name <- paste0('modben', week)
  tot_name_v[['modben']] <- c(tot_name_v[['modben']], tot_name)
  tot_part_name_v[['modben']][[tot_name]] <- part_name_v[['modben']]
}

# Select columns of interest
lems_data <- sygen_data[, c('ptid', tot_name_v[['lems']], part_name_v[['lems']], 'sexcd', 'asimpc01', 'age', 'tx1_r', 'nligr1', 'Famotidine_critical', 'Ranitidine_critical', 'Cimetidine_critical', 'bmi_status', 'nli1', 'nli1_broad', 'lvl', 'splvl', 'lvlgr', 'htm', 'wtm')]
uems_data <- sygen_data[, c('ptid', tot_name_v[['uems']], part_name_v[['uems']], 'sexcd', 'asimpc01', 'age', 'tx1_r', 'nligr1', 'Famotidine_critical', 'Ranitidine_critical', 'Cimetidine_critical', 'bmi_status', 'nli1', 'nli1_broad', 'lvl', 'splvl', 'lvlgr', 'htm', 'wtm')]
ltscore_data <- sygen_data[, c('ptid', tot_name_v[['ltscore']], part_name_v[['ltscore']], 'sexcd', 'asimpc01', 'age', 'tx1_r', 'nligr1', 'Famotidine_critical', 'Ranitidine_critical', 'Cimetidine_critical', 'bmi_status', 'nli1', 'nli1_broad', 'lvl', 'splvl', 'lvlgr', 'htm', 'wtm')]
ppscore_data <- sygen_data[, c('ptid', tot_name_v[['ppscore']], part_name_v[['ppscore']], 'sexcd', 'asimpc01', 'age', 'tx1_r', 'nligr1', 'Famotidine_critical', 'Ranitidine_critical', 'Cimetidine_critical', 'bmi_status', 'nli1', 'nli1_broad', 'lvl', 'splvl', 'lvlgr', 'htm', 'wtm')]
modben_data <- sygen_data[, c('ptid', tot_name_v[['modben']], part_name_v[['modben']], 'sexcd', 'asimpc01', 'age', 'tx1_r', 'nligr1', 'Famotidine_critical', 'Ranitidine_critical', 'Cimetidine_critical', 'bmi_status', 'nli1', 'nli1_broad', 'lvl', 'splvl', 'lvlgr', 'htm', 'wtm')]
data_list_all <- list('lems' = getSubset(data = lems_data, outcome_variable = 'lems', min_non_NA = min_non_NA), 'uems' = getSubset(data = uems_data, outcome_variable = 'uems', min_non_NA = min_non_NA), 'ltscore' = getSubset(data = ltscore_data, outcome_variable = 'ltscore', min_non_NA = min_non_NA), 'ppscore' = getSubset(data = ppscore_data, outcome_variable = 'ppscore', min_non_NA = min_non_NA), 'modben' = getSubset(data = modben_data, outcome_variable = 'modben', min_non_NA = min_non_NA))

# MICE
data_list_imputed <- list() #'sexcd', 'asimpc01', 'age', 'tx1_r', 'Famotidine_critical', 'Ranitidine_critical', 'Cimetidine_critical', 'lvlgr'
for (outcome in names(data_list_all)) {
  data <- as.data.frame(data_list_all[[outcome]])
  data <- select(data, -c("nli1", "nli1_broad", "splvl", "lvl")) #, "sexcd", "asimpc01", "age", "tx1_r", "nli1", "Famotidine_critical", "Ranitidine_critical", "Cimetidine_critical", "bmi_status", "number_observed", "ptid"))
  col <- colnames(data)
  col_other <- colnames(data)[!(colnames(data) %in% c(tot_name_v[[outcome]], part_name_v[[outcome]], 'ptid', 'number_observed'))]
  data <- data[,c('ptid', col_other, part_name_v[[outcome]], tot_name_v[[outcome]])]
  data <- data %>% relocate('htm', .after = 'wtm') %>% relocate('bmi_status', .after = 'htm') 
  #data$nli1 <- droplevels(data$nli1)
  ini <- mice(data, maxit = 0)
  meth <- ini$method
  pred <- ini$predictorMatrix # basically here all is one but not the diagonal
  visSeq <- ini$visitSequence
  # sum scores
  for (tot_score in names(expr_v[[outcome]])){
    meth[tot_score] <- expr_v[[outcome]][[tot_score]] # update methods for the sums
    pred[tot_part_name_v[[outcome]][[tot_score]], tot_score] <- 0 # tot_score should not be used as predictor for part_scores
    pred[tot_score,] <- 0 # tot_score can only be predicted by part_scores (see line 338)
    pred[, tot_part_name_v[[outcome]][[tot_score]]] <- 0 # part_scores should not be used to update other variables...
    pred[c(tot_part_name_v[[outcome]][[tot_score]], tot_score), tot_part_name_v[[outcome]][[tot_score]]] <- 1 # ...only theirselves and the total score
    pred[tot_part_name_v[[outcome]][[tot_score]], part_name_v[[outcome]][!(part_name_v[[outcome]] %in% tot_part_name_v[[outcome]][[tot_score]])]] <- 0 # part_scores cannot be predicted by other part_scores
    #pred[col_other, tot_score] <- 0
    if (outcome == 'modben'){
      pred[tot_score, ] <- 1
      meth[tot_score] <- ini$method[tot_score]
    } 
    # check for constant values in part_scores (algorithm does not work for constant columns)
    logev <- ini$loggedEvents
    methconst <- logev[logev$meth == "constant",]$out # this will most likely happen for dermatomes at the C2 level
    methcoll <- logev[logev$meth == "collinear",]$out
    for (k in methconst){ # for constant column --> these are a problem
      d <- unlist(data[k])
      d <- unique(d)[!is.na(unique(d))]
      data[k] <- d # replace with the same value
      meth[k] <- "" # not impute
      pred[k, ] <- 0 # no need to be predicted
      pred[, k] <- 0 # not use as predictor
      pred[tot_score, k] <- 1 # only use as predictor for tot_score (needed for the sum)
    }
    for (k2 in tot_part_name_v[[outcome]][[tot_score]][!(tot_part_name_v[[outcome]][[tot_score]] %in% methconst)]){
      agg <- aggregate(unlist(data[k2]), list(unlist(data[k2])), FUN = function(x){length(x)})
      agg$x <- agg$x/sum(agg$x)
      colnames(agg) <- c("level", "prop")
      if (any(agg$prop > 0.99)){ # "nearly constant" --> nearly constant scores cause problems in imputation
        data[is.na(unlist(data[k2])), k2] <- agg[agg$prop > 0.95,]$level # replace with the value that is the most present
        meth[k2] <- "" # not impute
        pred[k2, ] <- 0 # no need to be predicted
        pred[, k2] <- 0 # not use as predictor
        pred[tot_score, k2] <- 1 # only use as predictor for tot_score (needed for the sum)
      }
    }
  } 
  meth['bmi_status'] <- "~I(as.factor(ifelse(wtm/(htm*htm/10000) < 18.5, 'Underweight', ifelse((wtm/(htm*htm/10000) >= 18.5)&(wtm/(htm*htm/10000) < 25), 'Healthy weight', ifelse((wtm/(htm*htm/10000) >= 25)&(wtm/(htm*htm/10000) < 30), 'Overweight', 'Obesity')))))"
  pred[c('htm', 'wtm'), 'bmi_status'] <- 0 # bmi_status should not be used as predictor for htm and wtm
  pred[, c('htm', 'wtm')] <- 0 # these should only be used to update each other and bmi_status
  pred['bmi_status', c('htm', 'wtm')] <- 0
  diag(pred) <- 0
  #meth['htm'] <- ""
  #meth['wtm'] <- ""
  #meth['nligr1'] <- ""
  # variables that should not be imputed
  pred[c("ptid"), ] <- 0 # , "bmi_status", "nligr1", 'htm', 'wtm' # the idea was not to impute them, but not imputing them does not allow to impute other variables by using these as predictors
  # variables should not serve as predictors                                         # just saying no predictor for them means imputing them based on nothing
  pred[, c("ptid")] <- 0
  # multiply imputed missing values 
  imp <- mice(data, pred = pred, meth = meth, m = 20, maxit = 20, seed = 1) # m = number of datasets to be created # , ridge = 0.001, threshold = 1
  imp_data <- complete(imp, 'long', include = FALSE)
  imp_data <- select(imp_data, -c(".id"))
  data_list_imputed[[outcome]] <- imp_data
}


# Save the complete and the imputed datasets
lapply(c('lems', 'uems', 'ltscore', 'ppscore', 'modben'), function(outcome){
  write_xlsx(data_list_all[[outcome]], paste0(basic_path, "\\Main analysis\\results - ", min_non_NA, "\\data_NA_", outcome, ".xlsx"))
  write_xlsx(data_list_imputed[[outcome]], paste0(basic_path, "\\Main analysis\\results - ", min_non_NA, "\\data_imputed_", outcome, ".xlsx"))
})


# what to do: https://stefvanbuuren.name/fimd/sec-knowledge.html
# how to do: https://stefvanbuuren.name/publications/Flexible%20multivariate%20-%20TNO99054%201999.pdf
# be careful to: https://stackoverflow.com/questions/51289801/passive-imputation-with-mice-gives-wrong-sumscore
# be careful to: https://stackoverflow.com/questions/61670675/mice-in-r-why-is-passive-imputation-result-influenced-by-column-position
# issue: https://github.com/amices/mice/issues/321