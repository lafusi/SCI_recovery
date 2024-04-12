library(readxl)
library(writexl)

# get partial scores names (and expressions for imputation)
tot_name_v <- list("ltscore" = c(), "ppscore" = c(), "lems" = c(), "uems" = c(), "modben" = c())
part_name_v <- list("ltscore" = c(), "ppscore" = c(), "lems" = c(), "uems" = c(), "modben" = c())
tot_part_name_v <- list("ltscore" = list(), "ppscore" = list(), "lems" = list(), "uems" = list(), "modben" = list())
expr_v <- list("ltscore" = list(), "ppscore" = list(), "lems" = list(), "uems" = list(), "modben" = list())
# sensory scores
for (examination_type in c('lt', 'pp')) {
  for (week in c('00', '01', '04', '08', '16', '26', '52', '54')){
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
for (week in c('00', '01', '04', '08', '16', '26', '52', '54')){
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
for (week in c('00', '01', '04', '08', '16', '26', '52', '54')){
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
for (week in c('04', '08', '16', '26', '52', '54')){
  tot_name <- paste0('modben', week)
  tot_name_v[['modben']] <- c(tot_name_v[['modben']], tot_name)
  tot_part_name_v[['modben']][[tot_name]] <- part_name_v[['modben']]
}

cols_to_remove <- unlist(part_name_v)

sygen_data <- "C:/Users/LAURA/Documents/ETHZ/Master/Master thesis/SCI project/code - final version - 2/Dataset for shiny app/sygen_data.xlsx"
files_list_res <- list.files("C:/Users/LAURA/Documents/ETHZ/Master/Master thesis/SCI project/code - final version - 2/Dataset for shiny app/results - 2", pattern = ".xlsx")
files_list_res <- files_list_res[2:length(files_list_res)]
files_list_res <- lapply(files_list_res, function(x){paste0("C:/Users/LAURA/Documents/ETHZ/Master/Master thesis/SCI project/code - final version - 2/Dataset for shiny app/results - 2/", x)})
files_list_sens <- list.files("C:/Users/LAURA/Documents/ETHZ/Master/Master thesis/SCI project/code - final version - 2/Dataset for shiny app/data_sensitivity_analysis - 2", pattern = ".xlsx")
files_list_sens <- lapply(files_list_sens, function(x){paste0("C:/Users/LAURA/Documents/ETHZ/Master/Master thesis/SCI project/code - final version - 2/Dataset for shiny app/data_sensitivity_analysis - 2/", x)})
lapply(c(sygen_data, files_list_res, files_list_sens), function(f){
  doc <- read_excel(f)
  doc <- doc[, colnames(doc)[!(colnames(doc) %in% cols_to_remove)]]
  doc <- doc[, colnames(doc)[!grepl("probs", colnames(doc))]]
  write_xlsx(doc, f)
})

med_grid <- expand.grid(c("FAMOTIDINE", "CIMETIDINE", "RANITIDINE"), 0:365)
med_cols_original <- paste0(med_grid$Var1, "_", med_grid$Var2)
med_cols <- c(med_cols_original, "Famotidine", "Ranitidine", "Cimetidine", "Famotidine_critical", "Ranitidine_critical", "Cimetidine_critical")
char_cols <- c('ptid', unlist(tot_name_v)[(!grepl("00", unlist(tot_name_v)))&(!grepl("54", unlist(tot_name_v)))], 'sexcd', 'asimpc01', 'age', 'tx1_r', 'nligr1', 'bmi_status', 'nli1', 'nli1_broad', 'lvl', 'splvl', 'lvlgr', 'htm', 'wtm')
cols_of_interest <- c(char_cols, med_cols, "age_range")
doc <- read_excel(sygen_data)
doc <- doc[, cols_of_interest[cols_of_interest %in% colnames(doc)]]
write_xlsx(doc, sygen_data)