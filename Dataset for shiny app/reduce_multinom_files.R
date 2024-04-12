library(readxl)
library(writexl)

files_list_res <- list.files("C:/Users/LAURA/Documents/ETHZ/Master/Master thesis/SCI project/code - final version - 2/Dataset for shiny app/results - 2", pattern = "probs.xlsx")
#files_list_res <- files_list_res[2:length(files_list_res)]
files_list_res <- lapply(files_list_res, function(x){paste0("C:/Users/LAURA/Documents/ETHZ/Master/Master thesis/SCI project/code - final version - 2/Dataset for shiny app/results - 2/", x)})
files_list_sens <- list.files("C:/Users/LAURA/Documents/ETHZ/Master/Master thesis/SCI project/code - final version - 2/Dataset for shiny app/data_sensitivity_analysis - 2", pattern = "probs.xlsx")
files_list_sens <- lapply(files_list_sens, function(x){paste0("C:/Users/LAURA/Documents/ETHZ/Master/Master thesis/SCI project/code - final version - 2/Dataset for shiny app/data_sensitivity_analysis - 2/", x)})
lapply(c(files_list_res, files_list_sens), function(f){
  doc <- read_excel(f)
  doc <- doc[(doc$age %in% c(10, 30, 50, 70))&(doc$y.level!="1"), colnames(doc)[colnames(doc) != 'method']]
  write_xlsx(doc, f)
})