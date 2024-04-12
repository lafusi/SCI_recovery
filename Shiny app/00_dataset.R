# This script converts variables to the correct data type

# Load libraries
library(readxl)

# Load data
sygen_data <- read_excel("sygen_data.xlsx")
# factors with levels
sygen_data$splvl <- factor(sygen_data$splvl, levels = c("T12", "T11", "T10", "T09", "T08", "T07", "T06", "T05", "T04", "T03", "T02", "T01", "C08", "C07", "C06", "C05", "C04", "C03", "C02", "C01"))
sygen_data$asimpc01 <- factor(sygen_data$asimpc01, levels = c("A", "B", "C", "D"))
#sygen_data$asimpc00 <- factor(sygen_data$asimpc00, levels = c("A", "B", "C", "D"))
sygen_data$sexcd <- as.factor(sygen_data$sexcd)
sygen_data$tx1_r <- factor(sygen_data$tx1_r, levels = c("P", "D1", "D2", "None"))
sygen_data$lvl <- factor(sygen_data$lvl, levels = c("Thoracic", "Cervical"))
sygen_data$lvlgr <- factor(sygen_data$lvlgr, levels = c("T09 - T12", "T05 - T08", "T01 - T04", "C05 - C08", "C01 - C04"))
for (w in c("1")) {
  th <- c("T12", "T11", "T10", "T09", "T08", "T07", "T06", "T05", "T04", "T03", "T02", "T01")
  cerv <- c("C08", "C07", "C06", "C05", "C04", "C03", "C02", "C01")
  c_upper <- c("C04", "C03", "C02", "C01")
  c_lower <- c("C08", "C07", "C06", "C05")
  t_upper <- c("T04", "T03", "T02", "T01")
  t_middle <- c("T08", "T07", "T06", "T05")
  t_lower <- c("T12", "T11", "T10", "T09")
  sygen_data[paste0('nli', w)] <- factor(sygen_data[[paste0('nli', w)]], levels = c(th, cerv))
  sygen_data[paste0('nli', w, "_broad")] <- factor(sygen_data[[paste0('nli', w, "_broad")]], levels =  c("Thoracic", "Cervical"))
  sygen_data[paste0('nligr', w)] <- factor(sygen_data[[paste0('nligr', w)]], levels = c("T09 - T12", "T05 - T08", "T01 - T04", "C05 - C08", "C01 - C04"))
}
sygen_data$age_range <- factor(sygen_data$age_range, levels = c("0 - 9", "10 - 19", "20 - 29", "30 - 39", "40 - 49", "50 - 59", "60 - 69", "70 - 79", "80 - 89", "90 - 99"))
sygen_data$bmi_status <- factor(sygen_data$bmi_status, levels = c("Underweight", "Healthy weight", "Overweight", "Obesity"))