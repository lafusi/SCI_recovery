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
data_list_all <- list('lems' = lems_data, 'uems' = uems_data, 'ltscore' = ltscore_data, 'ppscore' = ppscore_data, 'modben' = modben_data)

# Remove rows with missing values in outcome scores
data_list <- list()
for (outcome in names(data_list_all)) {
  data <- data_list_all[[outcome]]
  outcome_col_name <- list('lems' = 'lower', 'uems' = 'upper', 'ltscore' = 'ltscor', 'ppscore' = 'ppscor', 'modben' = 'modben')[[outcome]]
  col_outcome <- unlist(lapply(c('01', '04', '08', '16', '26', '52'), function(x){paste0(outcome_col_name, x)}))
  if (outcome == 'modben') {
    col_outcome <- col_outcome[2:length(col_outcome)]
  }
  data_no_na <- NULL
  for (col in col_outcome) {
    if (is.null(data_no_na)) {
      data_no_na <- data[!is.na(unlist(data[col])),]
    } else {
      data_no_na <- data_no_na[!is.na(unlist(data_no_na[col])),]
    }
  }
  data_list[[outcome]] <- data_no_na
}

# "full" dataset
data_full_lems <- data_list[['lems']]
data_full_uems <- data_list[['uems']]
data_full_ltscore <- data_list[['ltscore']]
data_full_ppscore <- data_list[['ppscore']]
data_full_modben <- data_list[['modben']]

# Look at the proportions of missing data in the complete dataset
# Function
getSubset2 <- function(data = sygen_data, outcome_variable = 'all', number_observed = 0){
  if ((outcome_variable == 'all')|(!(outcome_variable %in% c('lems', 'uems', 'ltscore', 'ppscore', 'modben')))){
    filtered_data <- data
  } else {
    if (outcome_variable == 'lems') {
      outcome <- list('lems' = dplyr::select(data, lower01, lower04, lower08, lower16, lower26, lower52))
    } else if (outcome_variable == 'uems') {
      outcome <- list('uems' = dplyr::select(data, upper01, upper04, upper08, upper16, upper26, upper52))
    } else if (outcome_variable == 'ltscore') {
      outcome <- list('ltscore' = dplyr::select(data, ltscor01, ltscor04, ltscor08, ltscor16, ltscor26, ltscor52))
    } else if (outcome_variable == 'ppscore') {
      outcome <- list('ppscore' = dplyr::select(data, ppscor01, ppscor04, ppscor08, ppscor16, ppscor26, ppscor52))
    } else {
      outcome <- list('modben' = dplyr::select(data, modben04, modben08, modben16, modben26, modben52))
    }
    condition <- as.numeric(!is.na(outcome[[outcome_variable]][[1]])) +
      as.numeric(!is.na(outcome[[outcome_variable]][[2]])) +
      as.numeric(!is.na(outcome[[outcome_variable]][[3]])) +
      as.numeric(!is.na(outcome[[outcome_variable]][[4]])) +
      as.numeric(!is.na(outcome[[outcome_variable]][[5]]))
    if (outcome_variable != 'modben') {
      condition <- condition + as.numeric(!is.na(outcome[[outcome_variable]][[6]])) == number_observed
    } else {
      condition <- condition == number_observed
    }
      
    filtered_data <- data[condition,]
  }
  return(filtered_data)
}
# LEMS
lems0 <- getSubset2(data = lems_data, outcome_variable = 'lems', number_observed = 0)
lems1 <- getSubset2(data = lems_data, outcome_variable = 'lems', number_observed = 1)
lems2 <- getSubset2(data = lems_data, outcome_variable = 'lems', number_observed = 2)
lems3 <- getSubset2(data = lems_data, outcome_variable = 'lems', number_observed = 3)
lems4 <- getSubset2(data = lems_data, outcome_variable = 'lems', number_observed = 4)
lems5 <- getSubset2(data = lems_data, outcome_variable = 'lems', number_observed = 5)
lems6 <- getSubset2(data = lems_data, outcome_variable = 'lems', number_observed = 6)
# UEMS
uems0 <- getSubset2(data = uems_data, outcome_variable = 'uems', number_observed = 0)
uems1 <- getSubset2(data = uems_data, outcome_variable = 'uems', number_observed = 1)
uems2 <- getSubset2(data = uems_data, outcome_variable = 'uems', number_observed = 2)
uems3 <- getSubset2(data = uems_data, outcome_variable = 'uems', number_observed = 3)
uems4 <- getSubset2(data = uems_data, outcome_variable = 'uems', number_observed = 4)
uems5 <- getSubset2(data = uems_data, outcome_variable = 'uems', number_observed = 5)
uems6 <- getSubset2(data = uems_data, outcome_variable = 'uems', number_observed = 6)
# LTSCORE
ltscore0 <- getSubset2(data = ltscore_data, outcome_variable = 'ltscore', number_observed = 0)
ltscore1 <- getSubset2(data = ltscore_data, outcome_variable = 'ltscore', number_observed = 1)
ltscore2 <- getSubset2(data = ltscore_data, outcome_variable = 'ltscore', number_observed = 2)
ltscore3 <- getSubset2(data = ltscore_data, outcome_variable = 'ltscore', number_observed = 3)
ltscore4 <- getSubset2(data = ltscore_data, outcome_variable = 'ltscore', number_observed = 4)
ltscore5 <- getSubset2(data = ltscore_data, outcome_variable = 'ltscore', number_observed = 5)
ltscore6 <- getSubset2(data = ltscore_data, outcome_variable = 'ltscore', number_observed = 6)
# PPSCORE
ppscore0 <- getSubset2(data = ppscore_data, outcome_variable = 'ppscore', number_observed = 0)
ppscore1 <- getSubset2(data = ppscore_data, outcome_variable = 'ppscore', number_observed = 1)
ppscore2 <- getSubset2(data = ppscore_data, outcome_variable = 'ppscore', number_observed = 2)
ppscore3 <- getSubset2(data = ppscore_data, outcome_variable = 'ppscore', number_observed = 3)
ppscore4 <- getSubset2(data = ppscore_data, outcome_variable = 'ppscore', number_observed = 4)
ppscore5 <- getSubset2(data = ppscore_data, outcome_variable = 'ppscore', number_observed = 5)
ppscore6 <- getSubset2(data = ppscore_data, outcome_variable = 'ppscore', number_observed = 6)
# MODBEN
modben0 <- getSubset2(data = modben_data, outcome_variable = 'modben', number_observed = 0)
modben1 <- getSubset2(data = modben_data, outcome_variable = 'modben', number_observed = 1)
modben2 <- getSubset2(data = modben_data, outcome_variable = 'modben', number_observed = 2)
modben3 <- getSubset2(data = modben_data, outcome_variable = 'modben', number_observed = 3)
modben4 <- getSubset2(data = modben_data, outcome_variable = 'modben', number_observed = 4)
modben5 <- getSubset2(data = modben_data, outcome_variable = 'modben', number_observed = 5)
modben6 <- getSubset2(data = modben_data, outcome_variable = 'modben', number_observed = 6)
# sum
sum_lems <- nrow(lems0) + nrow(lems1) + nrow(lems2) + nrow(lems3) + nrow(lems4) + nrow(lems5) + nrow(lems6)
sum_uems <- nrow(uems0) + nrow(uems1) + nrow(uems2) + nrow(uems3) + nrow(uems4) + nrow(uems5) + nrow(uems6)
sum_ltscore <- nrow(ltscore0) + nrow(ltscore1) + nrow(ltscore2) + nrow(ltscore3) + nrow(ltscore4) + nrow(ltscore5) + nrow(ltscore6)
sum_ppscore <- nrow(ppscore0) + nrow(ppscore1) + nrow(ppscore2) + nrow(ppscore3) + nrow(ppscore4) + nrow(ppscore5) + nrow(ppscore6)
sum_modben <- nrow(modben0) + nrow(modben1) + nrow(modben2) + nrow(modben3) + nrow(modben4) + nrow(modben5) + nrow(modben6)
# summary_table
number_observed <- c(0, 1, 2, 3, 4, 5, 6)
lems_prop <- c(nrow(lems0)/sum_lems, nrow(lems1)/sum_lems, nrow(lems2)/sum_lems, nrow(lems3)/sum_lems, nrow(lems4)/sum_lems, nrow(lems5)/sum_lems, nrow(lems6)/sum_lems)
uems_prop <- c(nrow(uems0)/sum_uems, nrow(uems1)/sum_uems, nrow(uems2)/sum_uems, nrow(uems3)/sum_uems, nrow(uems4)/sum_uems, nrow(uems5)/sum_uems, nrow(uems6)/sum_uems)
ltscore_prop <- c(nrow(ltscore0)/sum_ltscore, nrow(ltscore1)/sum_ltscore, nrow(ltscore2)/sum_ltscore, nrow(ltscore3)/sum_ltscore, nrow(ltscore4)/sum_ltscore, nrow(ltscore5)/sum_ltscore, nrow(ltscore6)/sum_ltscore)
ppscore_prop <- c(nrow(ppscore0)/sum_ppscore, nrow(ppscore1)/sum_ppscore, nrow(ppscore2)/sum_ppscore, nrow(ppscore3)/sum_ppscore, nrow(ppscore4)/sum_ppscore, nrow(ppscore5)/sum_ppscore, nrow(ppscore6)/sum_ppscore)
modben_prop <- c(nrow(modben0)/sum_modben, nrow(modben1)/sum_modben, nrow(modben2)/sum_modben, nrow(modben3)/sum_modben, nrow(modben4)/sum_modben, nrow(modben5)/sum_modben, nrow(modben6)/sum_modben)
summary_table <- data.frame(number_observed, lems_prop, uems_prop, ltscore_prop, ppscore_prop, modben_prop)
# get random attribution of how many values to get per participant
data_full_lems$number_observed <- sample(summary_table[summary_table$number_observed >= min_non_NA, ]$number_observed, nrow(data_full_lems), replace = TRUE, prob = summary_table[summary_table$number_observed >= min_non_NA, ]$lems_prop/sum(summary_table[summary_table$number_observed >= min_non_NA, ]$lems_prop))
data_full_uems$number_observed <- sample(summary_table[summary_table$number_observed >= min_non_NA, ]$number_observed, nrow(data_full_uems), replace = TRUE, prob = summary_table[summary_table$number_observed >= min_non_NA, ]$uems_prop/sum(summary_table[summary_table$number_observed >= min_non_NA, ]$uems_prop))
data_full_ltscore$number_observed <- sample(summary_table[summary_table$number_observed >= min_non_NA, ]$number_observed, nrow(data_full_ltscore), replace = TRUE, prob = summary_table[summary_table$number_observed >= min_non_NA, ]$ltscore_prop/sum(summary_table[summary_table$number_observed >= min_non_NA, ]$ltscore_prop))
data_full_ppscore$number_observed <- sample(summary_table[summary_table$number_observed >= min_non_NA, ]$number_observed, nrow(data_full_ppscore), replace = TRUE, prob = summary_table[summary_table$number_observed >= min_non_NA, ]$ppscore_prop/sum(summary_table[summary_table$number_observed >= min_non_NA, ]$ppscore_prop))
data_full_modben$number_observed <- sample(summary_table[summary_table$number_observed >= min_non_NA, ]$number_observed, nrow(data_full_modben), replace = TRUE, prob = summary_table[summary_table$number_observed >= min_non_NA, ]$modben_prop/sum(summary_table[summary_table$number_observed >= min_non_NA, ]$modben_prop))
# check how many observed values per week per variable
observed_list <- list()
for (outcome in names(data_list_all)) {
  data <- data_list_all[[outcome]]
  outcome_col_name <- list('lems' = 'lower', 'uems' = 'upper', 'ltscore' = 'ltscor', 'ppscore' = 'ppscor', 'modben' = 'modben')[[outcome]]
  col_outcome <- unlist(lapply(c('01', '04', '08', '16', '26', '52'), function(x){paste0(outcome_col_name, x)}))
  if (outcome == 'modben') {
    col_outcome <- col_outcome[2:length(col_outcome)]
  }
  observed_outcome <- c()
  for (col in col_outcome) {
    observed_outcome <- c(observed_outcome, nrow(data[!is.na(unlist(data[col])),])/nrow(data))
  }
  if (outcome == 'modben') {
    observed_outcome <- c(0, observed_outcome)
  }
  observed_list[[outcome]] <- observed_outcome
}
summary_observed_week <- data.frame('week' = c(1, 4, 8, 16, 26, 52), 'lems' = observed_list[['lems']], 'uems' = observed_list[['uems']], 'ltscore' = observed_list[['ltscore']], 'ppscore' = observed_list[['ppscore']], 'modben' = observed_list[['modben']])
summary_observed_week_prop <- data.frame('week' = c(1, 4, 8, 16, 26, 52), 'lems' = observed_list[['lems']]/sum(observed_list[['lems']]), 'uems' = observed_list[['uems']]/sum(observed_list[['uems']]), 'ltscore' = observed_list[['ltscore']]/sum(observed_list[['ltscore']]), 'ppscore' = observed_list[['ppscore']]/sum(observed_list[['ppscore']]), 'modben' = observed_list[['modben']]/sum(observed_list[['modben']]))

# Get missing/ non-missing dataframe
data_list_full <- list('lems' = data_full_lems, 'uems' = data_full_uems, 'ltscore' = data_full_ltscore, 'ppscore' = data_full_ppscore, 'modben' = data_full_modben)
data_list_random_NA <- list()
for (outcome in names(data_list_full)) {
  data <- data_list_full[[outcome]]
  row_list <- list()
   for (x in 1:nrow(data)){
    if (outcome == 'modben') {
      pos <- sample(1:5, prob = summary_observed_week_prop$modben[2:6])
      maxi <- 5
    } else {
      pos <- sample(1:6, prob = unlist(summary_observed_week_prop[outcome]))
      maxi <- 6
    }
    number_observed <- data$number_observed[as.integer(x)]
    ro <- data[as.integer(x),]
    if (number_observed != maxi) {
      for (n in (number_observed+1):maxi) {
        ro[, (pos[n]+1)] <- NA
      }
    }
    row_list[[x]] <- ro
  }
  random_NA_df <- rbindlist(row_list)
  rownames(random_NA_df) <- NULL
  data_list_random_NA[[outcome]] <- random_NA_df
}

# Same job for partial scores: if a certain score is missing, how many and which partial scores are missing?
# Function
getSubset2partialscores <- function(data = NULL, outcome = NULL, total_score = NULL, number_scores_observed = 0){
  if((!is.null(outcome))&(!is.null(total_score))&(!is.null(data))){
    part_score_v <- tot_part_name_v[[outcome]][[tot_score]]
    condition <- rep(0, nrow(data))
    for (x in part_score_v){
      condition <- condition + as.numeric(!is.na(unlist(data[x])))
    }
    condition <- condition == number_scores_observed
    #condition <- Reduce(function(x, y){as.numeric(!is.na(unlist(data[x])))+as.numeric(!is.na(unlist(data[y])))}, part_score_v) == number_scores_observed
    filtered_data <- data[condition,]
  } else {
    filtered_data <- data
  }
  return(filtered_data)
}
for (outcome in names(data_list_random_NA)) {
  for (tot_score in names(tot_part_name_v[[outcome]])){
    part_scores <- tot_part_name_v[[outcome]][[tot_score]]
    proportions_nx <- c()
    proportions_sx <- c()
    dat_all <- data_list_all[[outcome]][is.na(unlist(data_list_all[[outcome]][tot_score])),]
    # how many
    for (n in 1:length(part_scores)){
      sub <- getSubset2partialscores(data = dat_all, outcome = outcome, total_score = tot_score, number_scores_observed = n)
      proportions_nx <- c(proportions_nx, c(nrow(sub)/nrow(dat_all)))
    }
    dat_rand_na <- as.data.frame(data_list_random_NA[[outcome]])
    dat_rand_na$nx <- 0
    dat_rand_na[is.na(unlist(dat_rand_na[tot_score])),]$nx <- sample(c(1:length(part_scores)), nrow(dat_rand_na[is.na(unlist(dat_rand_na[tot_score])),]), replace = TRUE, prob = proportions_nx)
    # which
    for (s in part_scores){
      proportions_sx <- c(proportions_sx, nrow(dat_all[!is.na(unlist(dat_all[s])),])/nrow(dat_all))
    }
    proportions_sx_final <- proportions_sx/sum(proportions_sx)
    names(proportions_sx_final) <- part_scores
    # add NA
    for (id in dat_rand_na[is.na(unlist(dat_rand_na[tot_score])),]$ptid){
      pos <- sample(part_scores, prob = proportions_sx_final)
      if (dat_rand_na[dat_rand_na$ptid == id, ]$nx != length(part_scores)){
        dat_rand_na[dat_rand_na$ptid == id, pos[(dat_rand_na[dat_rand_na$ptid == id, ]$nx+1):length(part_scores)]] <- NA
      }
    }
    
    # save
    data_list_random_NA[[outcome]] <- as.data.frame(dplyr::select(dat_rand_na, -c('nx')))
  }
}




# MICE
data_list_imputed <- list()
for (outcome in names(data_list_random_NA)) {
  data <- as.data.frame(data_list_random_NA[[outcome]])
  data <- select(data, -c("nli1", "nli1_broad", "splvl", "lvl")) #, "sexcd", "asimpc01", "age", "tx1_r", "nli1", "Famotidine_critical", "Ranitidine_critical", "Cimetidine_critical", "bmi_status", "number_observed", "ptid"))
  col <- colnames(data)
  col_other <- colnames(data)[!(colnames(data) %in% c(tot_name_v[[outcome]], part_name_v[[outcome]], 'ptid', 'number_observed'))]
  data <- data[,c('ptid', 'number_observed', col_other, part_name_v[[outcome]], tot_name_v[[outcome]])]
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
  pred[c("ptid", 'number_observed'), ] <- 0 # , "bmi_status", "nligr1", 'htm', 'wtm' # the idea was not to impute them, but not imputing them does not allow to impute other variables by using these as predictors
  # variables should not serve as predictors                                         # just saying no predictor for them means imputing them based on nothing
  pred[, c("ptid", "number_observed")] <- 0
  # multiply imputed missing values 
  imp <- mice(data, pred = pred, meth = meth, m = 20, maxit = 20, seed = 1) # m = number of datasets to be created # , ridge = 0.001, threshold = 1
  imp_data <- complete(imp, 'long', include = FALSE)
  imp_data <- select(imp_data, -c(".id"))
  data_list_imputed[[outcome]] <- imp_data
}


# Save the complete and the imputed datasets
lapply(c('lems', 'uems', 'ltscore', 'ppscore', 'modben'), function(outcome){
  write_xlsx(data_list_full[[outcome]], paste0(basic_path, "\\Sensitivity analysis\\data_sensitivity_analysis - ", min_non_NA, "\\data_full_", outcome, ".xlsx"))
  write_xlsx(data_list_imputed[[outcome]], paste0(basic_path, "\\Sensitivity analysis\\data_sensitivity_analysis - ", min_non_NA, "\\data_imputed_", outcome, ".xlsx"))
  write_xlsx(data_list_random_NA[[outcome]], paste0(basic_path, "\\Sensitivity analysis\\data_sensitivity_analysis - ", min_non_NA, "\\data_randomNA_", outcome, ".xlsx"))
})


# what to do: https://stefvanbuuren.name/fimd/sec-knowledge.html
# how to do: https://stefvanbuuren.name/publications/Flexible%20multivariate%20-%20TNO99054%201999.pdf
# be careful to: https://stackoverflow.com/questions/51289801/passive-imputation-with-mice-gives-wrong-sumscore
# be careful to: https://stackoverflow.com/questions/61670675/mice-in-r-why-is-passive-imputation-result-influenced-by-column-position
# issue: https://github.com/amices/mice/issues/321