# This script converts variables to the correct data type

# Load libraries
library(ggplot2)
library(plyr)
library(dplyr)
library(stringr)
library(reshape2)
library(readxl)

# Load data
sygen_data <- read_excel("dataset_11302023-no-pw.xlsx")

# Display data structure
#str(sygen_data)

# ptid: Patient ID --> numeric --> OK
# colnames(sygen_data)[[1]] <- "ptid"
# Reason for/ comment for/ comment on
dict <- read.csv("JohnKramersProject_DataDictionary_2023-02-15.csv")
only_needed <- dict[dict$"Variable...Field.Name" %in% colnames(sygen_data),]
comments <- only_needed[(str_detect(only_needed$Field.Label, "reason for"))|(str_detect(only_needed$Field.Label, "comment for"))|(str_detect(only_needed$Field.Label, "comment on")),]$"Variable...Field.Name"
sygen_data[comments] <- lapply(sygen_data[comments], function(x) {
  x <- factor(x)
})
sygen_data[c("ptid", comments)] <- reshape2::dcast(mutate(melt(sygen_data[c("ptid", comments)],id.var="ptid"), value = mapvalues(value, c('1', '2', '3', '4', '5'), c('amputation', 'orthopaedic device', 'pain', 'burn or wound', 'other'))), ptid ~ variable)
# age: Age at time of injury --> numeric --> OK
# upper00-52, lower00-52, asiatot00-52: upper extremities motor score, lower extremities motor, total motor score --> character --> should be numeric
sygen_data <- sygen_data %>% mutate_at(c('upper00', 'upper01', 'upper04', 'upper08', 'upper16', 'upper26', 'upper52', 'lower00', 'lower01', 'lower04', 'lower08', 'lower16', 'lower26', 'lower52', 'asiatot00', 'asiatot01', 'asiatot04', 'asiatot08', 'asiatot16', 'asiatot26', 'asiatot52'), as.numeric)
# splvl, asimpc00-01: neurological level of injury, AIS grade --> character --> should be factor
sygen_data$splvl <- factor(sygen_data$splvl, levels = c("T12", "T11", "T10", "T09", "T08", "T07", "T06", "T05", "T04", "T03", "T02", "T01", "C08", "C07", "C06", "C05", "C04", "C03", "C02", "C01"))
sygen_data$asimpc01 <- factor(sygen_data$asimpc01, levels = c("A", "B", "C", "D"))
sygen_data$asimpc00 <- factor(sygen_data$asimpc00, levels = c("A", "B", "C", "D"))

# sexcd: sex --> numeric --> should be a factor --> also convert to M/ F
sygen_data$sexcd <- as.character(sygen_data$sexcd)
sygen_data$sexcd <- ifelse(sygen_data$sexcd == "1", "F", "M")
sygen_data$sexcd <- as.factor(sygen_data$sexcd)
# tx1_r: treatment group --> character --> should be factor
sygen_data[is.na(sygen_data$tx1_r),]$tx1_r <- "None"
sygen_data$tx1_r <- factor(sygen_data$tx1_r, levels = c("P", "D1", "D2", "None"))

# Create column with summarized level of injury
sygen_data$lvl <- as.character(sygen_data$splvl)
#s_c <- c("Coc01", "S05", "S04", "S03", "S02", "S01")
#lumb <- c("L05", "L04", "L03", "L02", "L01")
th <- c("T12", "T11", "T10", "T09", "T08", "T07", "T06", "T05", "T04", "T03", "T02", "T01")
cerv <- c("C08", "C07", "C06", "C05", "C04", "C03", "C02", "C01")
#sygen_data$lvl[sygen_data$lvl %in% s_c] <- "Sacral/ coccygeal"
#sygen_data$lvl[sygen_data$lvl %in% lumb] <- "Lumbar"
sygen_data$lvl[sygen_data$lvl %in% th] <- "Thoracic"
sygen_data$lvl[sygen_data$lvl %in% cerv] <- "Cervical"
sygen_data$lvl <- factor(sygen_data$lvl, levels = c("Thoracic", "Cervical"))
# Groups
c_upper <- c("C04", "C03", "C02", "C01")
c_lower <- c("C08", "C07", "C06", "C05")
t_upper <- c("T04", "T03", "T02", "T01")
t_middle <- c("T08", "T07", "T06", "T05")
t_lower <- c("T12", "T11", "T10", "T09")
sygen_data$lvlgr <- as.character(sygen_data$splvl)
sygen_data$lvlgr[sygen_data$lvlgr %in% c_upper] <- "C01 - C04"
sygen_data$lvlgr[sygen_data$lvlgr %in% c_lower] <- "C05 - C08"
sygen_data$lvlgr[sygen_data$lvlgr %in% t_upper] <- "T01 - T04"
sygen_data$lvlgr[sygen_data$lvlgr %in% t_middle] <- "T05 - T08"
sygen_data$lvlgr[sygen_data$lvlgr %in% t_lower] <- "T09 - T12"
sygen_data$lvlgr <- factor(sygen_data$lvlgr, levels = c("T09 - T12", "T05 - T08", "T01 - T04", "C05 - C08", "C01 - C04"))
# NLI
for (w in c("1", "4", "8", "16", "26", "52")) {
  sygen_data[paste0('nli', w)] <- ifelse(nchar(sygen_data[[paste0('nli', w)]]) == 2, paste0(substr(sygen_data[[paste0('nli', w)]], 1, 1), "0", substr(sygen_data[[paste0('nli', w)]], 2, 2)), sygen_data[[paste0('nli', w)]])
  sygen_data[paste0('nli', w)] <- ifelse(sygen_data[[paste0('nli', w)]] == "N0A", NA, sygen_data[[paste0('nli', w)]])
  sygen_data[paste0('nli', w)] <- factor(sygen_data[[paste0('nli', w)]], levels = c(th, cerv))
  sygen_data[paste0('nli', w, '_broad')] <- sygen_data[paste0('nli', w)]
  sygen_data[paste0('nli', w, '_broad')] <- ifelse(sygen_data[[paste0('nli', w, '_broad')]] %in% th, "Thoracic", ifelse(!is.na(sygen_data[[paste0('nli', w, '_broad')]]), "Cervical", NA))
  sygen_data[paste0('nli', w, '_broad')] <- factor(sygen_data[[paste0('nli', w, '_broad')]], levels = c("Thoracic", "Cervical"))
  sygen_data[paste0('nligr', w)] <- sygen_data[paste0('nli', w)]
  sygen_data[paste0('nligr', w)] <- ifelse(sygen_data[[paste0('nligr', w)]] %in% t_lower, "T09 - T12", ifelse(sygen_data[[paste0('nligr', w)]] %in% t_middle, "T05 - T08", ifelse(sygen_data[[paste0('nligr', w)]] %in% t_upper, "T01 - T04", ifelse(sygen_data[[paste0('nligr', w)]] %in% c_upper, "C01 - C04", ifelse(sygen_data[[paste0('nligr', w)]] %in% c_lower, "C05 - C08", NA)))))
  sygen_data[paste0('nligr', w)] <- factor(sygen_data[[paste0('nligr', w)]], levels = c("T09 - T12", "T05 - T08", "T01 - T04", "C05 - C08", "C01 - C04"))
}

# Create column with ASIA impairment level change from emergency room to baseline
sygen_data$asimpc00to01 <- NA
sygen_data$asimpc00to01 <- ifelse(!is.na(sygen_data$asimpc00)&!is.na(sygen_data$asimpc01), paste(sygen_data$asimpc00, sygen_data$asimpc01, sep = " to "), NA)

# create column with age ranges
round_down <- function(x) {
  return(floor(x/10)*10)
}
sygen_data$age_range <- round_down(sygen_data$age)
sygen_data <- sygen_data %>% mutate(age_range = paste(as.character(sygen_data$age_range), as.character(sygen_data$age_range + 9), sep = " - "))
sygen_data$age_range <- factor(sygen_data$age_range, levels = c("0 - 9", "10 - 19", "20 - 29", "30 - 39", "40 - 49", "50 - 59", "60 - 69", "70 - 79", "80 - 89", "90 - 99"))

# Get BMI = kg/m^2
sygen_data$wtm <- as.numeric(sygen_data$wtm)
sygen_data$htm <- as.numeric(sygen_data$htm)
sygen_data$BMI <- sygen_data$wtm/(sygen_data$htm*sygen_data$htm/10000)
sygen_data$bmi_status <- ifelse(sygen_data$BMI < 18.5, "Underweight", ifelse((sygen_data$BMI >= 18.5)&(sygen_data$BMI < 25), "Healthy weight", ifelse((sygen_data$BMI >= 25)&(sygen_data$BMI < 30), "Overweight", "Obesity")))
sygen_data$bmi_status <- factor(sygen_data$bmi_status, levels = c("Underweight", "Healthy weight", "Overweight", "Obesity"))

# Change class med columns (character --> numeric) and calculate whether patient got medications at least once
for (med in c("FAMOTIDINE", "RANITIDINE", "CIMETIDINE")) {
  for (day in 0:365) {
    sygen_data[paste0(med, "_", day)] <- as.numeric(sygen_data[[paste0(med, "_", day)]])
    if (day == 0) {
      sygen_data[med] <- ifelse(is.na(sygen_data[[paste0(med, "_", day)]]), 0, 1)
    } else {
      sygen_data[med] <- ifelse(is.na(sygen_data[[paste0(med, "_", day)]]), 0, 1) + sygen_data[[med]]
    }
  }
  sygen_data[med] <- ifelse(sygen_data[[med]] == 0, 0, 1)
}

# Rename medication columns
colnames(sygen_data)[(length(colnames(sygen_data))-2):length(colnames(sygen_data))] <- c("Famotidine", "Ranitidine", "Cimetidine")
names(sygen_data$Famotidine) <- NULL
names(sygen_data$Cimetidine) <- NULL
names(sygen_data$Ranitidine) <- NULL

# Exposure during critical period (first seven days)
for (med in c("FAMOTIDINE", "RANITIDINE", "CIMETIDINE")) {
  for (day in 0:6) {
    sygen_data[paste0(med, "_", day)] <- as.numeric(sygen_data[[paste0(med, "_", day)]])
    if (day == 0) {
      sygen_data[paste0(med, "_critical")] <- ifelse(is.na(sygen_data[[paste0(med, "_", day)]]), 0, 1)
    } else {
      sygen_data[paste0(med, "_critical")] <- ifelse(is.na(sygen_data[[paste0(med, "_", day)]]), 0, 1) + sygen_data[[paste0(med, "_critical")]]
    }
  }
  sygen_data[paste0(med, "_critical")] <- ifelse(sygen_data[[paste0(med, "_critical")]] == 0, 0, 1)
}
colnames(sygen_data)[(length(colnames(sygen_data))-2):length(colnames(sygen_data))] <- c("Famotidine_critical", "Ranitidine_critical", "Cimetidine_critical")
names(sygen_data$Famotidine_critical) <- NULL
names(sygen_data$Cimetidine_critical) <- NULL
names(sygen_data$Ranitidine_critical) <- NULL

# solve problem with sensory scores (abnormal values)
# ASIA sensory evaluation (0 = absent, 1 = impaired, 2 = normal, 9 = not testable):
# anyana01-54: any anal sensation (0 = no, 1 = yes)
# level (c2, ..., c8, t1, ..., t12, l1, ..., l5, s1, ..., s45) + type (lt = light touch, pp = pin prick) + L/R (l = left, r = right) + week (01-54) --> e.g. c2ltl01
# ltscor01-54: light touch total score (L+R)
# ppscor01-54: pin prick total score (L+R)
for (examination_type in c('lt', 'pp')) {
  for (week in c('01', '04', '08', '16', '26', '52', '54')){
    tot_name <- paste0(examination_type, 'scor', week)
    sygen_data[tot_name] <- 0
    for (side in c('l', 'r')) {
      for (level in c('c2', 'c3', 'c4', 'c5', 'c6', 'c7', 'c8', 't1', 't2', 't3', 't4', 't5', 't6', 't7', 't8', 't9', 't10', 't11', 't12', 'l1', 'l2', 'l3', 'l4', 'l5', 's1', 's2', 's3', 's45')){
        name <- paste0(level, examination_type, side, week)
        sygen_data[name] <- as.numeric(sygen_data[[name]])
        sygen_data[name] <- replace(sygen_data[name], sygen_data[name] == 9, NA)
        sygen_data[tot_name] <- sygen_data[tot_name] + sygen_data[name]
      }
    }
  }
}

# Get uems
for (week in c('01', '04', '08', '16', '26', '52', '54')){
  tot_name <- paste0('upper', week)
  sygen_data[tot_name] <- 0
  for (muscle in c('elbex', 'elbfl', 'finab', 'finfl', 'wrext')) {
    for (side in c('l', 'r')) {
      name <- paste0(muscle, side, week)
      sygen_data[name] <- as.numeric(sygen_data[[name]])
      sygen_data[tot_name] <- sygen_data[tot_name] + sygen_data[name]
    }
  }
}

# Get lems
for (week in c('01', '04', '08', '16', '26', '52', '54')){
  tot_name <- paste0('lower', week)
  sygen_data[tot_name] <- 0
  for (muscle in c('ankdo', 'ankpl', 'greto', 'hipfl')) {
    for (side in c('l', 'r')) {
      name <- paste0(muscle, side, week)
      sygen_data[name] <- as.numeric(sygen_data[[name]])
      sygen_data[tot_name] <-  sygen_data[tot_name] + sygen_data[name]
    }
  }
  sygen_data[paste0('kneexl', week)] <- as.numeric(sygen_data[[paste0('kneexl', week)]])
  sygen_data[paste0('kneetr', week)] <- as.numeric(sygen_data[[paste0('kneetr', week)]])
  sygen_data[tot_name] <- sygen_data[tot_name] + sygen_data[paste0('kneexl', week)] + sygen_data[paste0('kneetr', week)]
}

# get tms
for (week in c('01', '04', '08', '16', '26', '52', '54')){
  sygen_data[paste0('asiatot', week)] <- sygen_data[paste0('lower', week)] + sygen_data[paste0('upper', week)]
}

# ankdol00-54/ ankdor00-54: ankle dorsiflexion (tibialis anterior l4 and l5) L/R
# ankpll00-54/ ankplr00-54: ankle plantar flexion (gastrocnemius s1 and s2) L/R
# elbexl00-54/ elbexr00-54: elbow extension (triceps c7) L/R
# elbfll00-54/ elbflr00-54: elbow flexion (biceps c5) L/R
# finabl00-54/ finabr00-54: finger abduction (little finger) (hand intrinsics t1) L/R
# finfll00-54/ finflr00-54: finger flexion (distal phalynx of middle finger) (flexor profundus c8) L/R
# gretol00-54/ gretor00-54: great toe extension (ext hallucis longus l5 and s1) L/R
# hipfll00-54/ hipflr00-54: hip flexion (illiopsoas l1 and l2 and l3) L/R
# kneexl00-54/ kneetr00-54: knee extension (quadriceps femoris l2 and l3 and l4) L/R
# wrextl00-54/ wrextr00-54: wrist extension (wrist extensors c6) L/R

# Modified Benzel scale
for (week in c('04', '08', '16', '26', '52', '54')){
  name <- paste0('modben', week)
  sygen_data[name] <- as.numeric(sygen_data[[name]])
  sygen_data[name] <- replace(sygen_data[name], sygen_data[name] == 9, NA)
}

