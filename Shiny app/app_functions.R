# Load libraries (see main file)

# Function to calculate standard error 
se <- function(x, narm = TRUE) {
  sd(x, na.rm = narm)/sqrt(length(x))
}

# Function to filter the data so that there are at least two observations for the outcome variable of interest
getSubset <- function(data = sygen_data, outcome_variable = 'all', min_non_NA = 2){
  if ((outcome_variable == 'all')||(!(outcome_variable %in% c('lems', 'uems', 'ltscore', 'ppscore', 'modben')))){
    filtered_data <- data
  } else {
    outcome <- list('lems' = dplyr::select(data, lower01, lower04, lower08, lower16, lower26, lower52),
                    'uems' = dplyr::select(data, upper01, upper04, upper08, upper16, upper26, upper52),
                    'ltscore' = dplyr::select(data, ltscor01, ltscor04, ltscor08, ltscor16, ltscor26, ltscor52),
                    'ppscore' = dplyr::select(data, ppscor01, ppscor04, ppscor08, ppscor16, ppscor26, ppscor52),
                    'modben' = dplyr::select(data, modben04, modben08, modben16, modben26, modben52))
    if (outcome_variable == 'modben'){
      condition <- as.numeric(!is.na(outcome[[outcome_variable]][[1]])) + as.numeric(!is.na(outcome[[outcome_variable]][[2]])) + as.numeric(!is.na(outcome[[outcome_variable]][[3]])) + as.numeric(!is.na(outcome[[outcome_variable]][[4]])) + as.numeric(!is.na(outcome[[outcome_variable]][[5]])) >= min_non_NA
    } else {
      condition <- as.numeric(!is.na(outcome[[outcome_variable]][[1]])) + as.numeric(!is.na(outcome[[outcome_variable]][[2]])) + as.numeric(!is.na(outcome[[outcome_variable]][[3]])) + as.numeric(!is.na(outcome[[outcome_variable]][[4]])) + as.numeric(!is.na(outcome[[outcome_variable]][[5]])) + as.numeric(!is.na(outcome[[outcome_variable]][[6]])) >= min_non_NA
    }
    filtered_data <- data[condition,]
  }
  return(filtered_data)
}

# Function for plotting --> updated compared to the other one
customizedPlot <- function(data = sygen_data, main = 'none', color = 'none', panel = 'none', type, exposure_main = NULL, exposure_color = NULL, exposure_panel = NULL, imp_or_all = NULL, imp_data = NULL){
  # main = c("lems", "uems", "age", "sex", "severity", "level_fine", "level_broad", "medication", "bmi_status", "treatment_gr")
  # color = c("none", "sex", "severity", "level_fine", "level_broad", "bmi_status", "treatment_gr")
  # panel = c("none", "sex", "severity", "level_broad", "bmi_status", "treatment_gr")
  # type = c("distribution", "bar", "box", "spaghetti", "pie", "upset")
  if (!("Famotidine" %in% colnames(data))){
    if (!is.null(imp_or_all)){
      data$Famotidine <- rep(sygen_data[sygen_data$ptid %in% data$ptid,]$Famotidine, 20)
    } else {
      data$Famotidine <- sygen_data[sygen_data$ptid %in% data$ptid,]$Famotidine
    }
    
  }
  if (!("Ranitidine" %in% colnames(data))){
    if (!is.null(imp_or_all)){
      data$Ranitidine <- rep(sygen_data[sygen_data$ptid %in% data$ptid,]$Ranitidine, 20)
    } else {
      data$Ranitidine <- sygen_data[sygen_data$ptid %in% data$ptid,]$Ranitidine
    }
    
  }
  if (!("Cimetidine" %in% colnames(data))){
    if (!is.null(imp_or_all)){
      data$Cimetidine <- rep(sygen_data[sygen_data$ptid %in% data$ptid,]$Cimetidine, 20)
    } else {
      data$Cimetidine <- sygen_data[sygen_data$ptid %in% data$ptid,]$Cimetidine
    }
    
  }
  if (!(main %in% c('medication', 'famotidine', 'cimetidine', 'ranitidine'))) {
    exposure_main <- c(0, 365)
  }
  if (!(color %in% c('medication', 'famotidine', 'cimetidine', 'ranitidine'))) {
    exposure_color <- c(0, 365)
  }
  if (!(panel %in% c('medication', 'famotidine', 'cimetidine', 'ranitidine'))) {
    exposure_panel <- c(0, 365)
  }
  data$fam <- ifelse(data$Famotidine == 1, "Yes", "No")
  data$ran <- ifelse(data$Ranitidine == 1, "Yes", "No")
  data$cim <- ifelse(data$Cimetidine == 1, "Yes", "No")
  ptid <- c(data$ptid, data$ptid, data$ptid, data$ptid, data$ptid, data$ptid)
  age <- c(data$age, data$age, data$age, data$age, data$age, data$age)
  age_range <- c(data$age_range, data$age_range, data$age_range, data$age_range, data$age_range, data$age_range)
  sexcd <- c(data$sexcd, data$sexcd, data$sexcd, data$sexcd, data$sexcd, data$sexcd)
  week <- c(rep(1, nrow(data)), rep(4, nrow(data)), rep(8, nrow(data)), rep(16, nrow(data)), rep(26, nrow(data)), rep(52, nrow(data)))
  uems <- c(data$upper01, data$upper04, data$upper08, data$upper16, data$upper26, data$upper52)
  lems <- c(data$lower01, data$lower04, data$lower08, data$lower16, data$lower26, data$lower52)
  ltscore <- c(data$ltscor01, data$ltscor04, data$ltscor08, data$ltscor16, data$ltscor26, data$ltscor52)
  ppscore <- c(data$ppscor01, data$ppscor04, data$ppscor08, data$ppscor16, data$ppscor26, data$ppscor52)
  modben <- c(rep(NA, nrow(data)), data$modben04, data$modben08, data$modben16, data$modben26, data$modben52)
  asimpc01 <- c(data$asimpc01, data$asimpc01, data$asimpc01, data$asimpc01, data$asimpc01, data$asimpc01)
  splvl <- c(data$splvl, data$splvl, data$splvl, data$splvl, data$splvl, data$splvl)
  lvl <- c(data$lvl, data$lvl, data$lvl, data$lvl, data$lvl, data$lvl)
  lvlgr <- c(data$lvlgr, data$lvlgr, data$lvlgr, data$lvlgr, data$lvlgr, data$lvlgr)
  nli1 <- c(data$nli1, data$nli1, data$nli1, data$nli1, data$nli1, data$nli1)
  nli1_broad <- c(data$nli1_broad, data$nli1_broad, data$nli1_broad, data$nli1_broad, data$nli1_broad, data$nli1_broad)
  nligr1 <- c(data$nligr1, data$nligr1, data$nligr1, data$nligr1, data$nligr1, data$nligr1)
  fam <- c(data$fam, data$fam, data$fam, data$fam, data$fam, data$fam)
  ran <- c(data$ran, data$ran, data$ran, data$ran, data$ran, data$ran)
  cim <- c(data$cim, data$cim, data$cim, data$cim, data$cim, data$cim)
  tx1_r <- c(data$tx1_r, data$tx1_r, data$tx1_r, data$tx1_r, data$tx1_r, data$tx1_r)
  bmi_status <- c(data$bmi_status, data$bmi_status, data$bmi_status, data$bmi_status, data$bmi_status, data$bmi_status)
  datlist <- list("ptid"=ptid, "age"=age, "age_range"=age_range, "sexcd"=sexcd, "week"=week, "uems"=uems, "lems"=lems, "ltscore"=ltscore, "ppscore"=ppscore, "modben"=modben, "asimpc01"=asimpc01, "splvl"=splvl, "lvl"=lvl, "nli1"=nli1, "nli1_broad"=nli1_broad, "fam"=fam, "ran"=ran, "cim"=cim, "tx1_r"=tx1_r, "bmi_status"=bmi_status, "nligr1"=nligr1, "lvlgr"=lvlgr)[lengths(list(ptid, age, age_range, sexcd, week, uems, lems, ltscore, ppscore, modben, asimpc01, splvl, lvl, nli1, nli1_broad, fam, ran, cim, tx1_r, bmi_status, nligr1, lvlgr)) == nrow(data)*6]
  reshaped_data <- as.data.frame(datlist) #data.frame(ptid, age, age_range, sexcd, week, uems, lems, ltscore, ppscore, modben, asimpc01, splvl, lvl, nli1, nli1_broad, fam, ran, cim, tx1_r, bmi_status, nligr1, lvlgr)
  for (component in c(main, panel, color)) {
    if (component == main) {
      exposure <- exposure_main
    } else if (component == color) {
      exposure <- exposure_color
    } else if (component == panel) {
      exposure <- exposure_panel
    }
    for (med in c('famotidine', 'ranitidine', 'cimetidine')) {
      if ((component == med)|(component == 'medication')) {
        for (day in exposure[1]:exposure[2]) {
          if (sum(exposure == c(0, 365)) != 2) {
            for (day in exposure[1]:exposure[2]) {
              data[paste0(toupper(med), "_", day)] <- as.numeric(data[[paste0(toupper(med), "_", day)]])
              if (day == 0) {
                data[paste0(toupper(med), "_", exposure[1], "_", exposure[2])] <- ifelse(is.na(data[[paste0(toupper(med), "_", day)]]), 0, 1)
              } else {
                data[paste0(toupper(med), "_", exposure[1], "_", exposure[2])] <- ifelse(is.na(data[[paste0(toupper(med), "_", day)]]), 0, 1) + data[[paste0(toupper(med), "_", exposure[1], "_", exposure[2])]]
              }
            }
            data[paste0(substr(med, 1, 3), "_", exposure[1], "_", exposure[2])] <- ifelse(data[[paste0(toupper(med), "_", exposure[1], "_", exposure[2])]] > 0, "Yes", "No")
          } else {
            data[paste0(substr(med, 1, 3), "_", exposure[1], "_", exposure[2])] <- data[[substr(med, 1, 3)]]
          }
        } 
      } else {
        data[paste0(substr(med, 1, 3), "_", exposure[1], "_", exposure[2])] <- data[[substr(med, 1, 3)]]
      }
      reshaped_data[paste0(substr(med, 1, 3), "_", exposure[1], "_", exposure[2])] <- c(data[[paste0(substr(med, 1, 3), "_", exposure[1], "_", exposure[2])]], data[[paste0(substr(med, 1, 3), "_", exposure[1], "_", exposure[2])]], data[[paste0(substr(med, 1, 3), "_", exposure[1], "_", exposure[2])]], data[[paste0(substr(med, 1, 3), "_", exposure[1], "_", exposure[2])]], data[[paste0(substr(med, 1, 3), "_", exposure[1], "_", exposure[2])]], data[[paste0(substr(med, 1, 3), "_", exposure[1], "_", exposure[2])]])
    }
  }
  val <- list('severity' = c("A" = "#1589FF", "B" = "#CD1076", "C" = "#FF8C00", "D" = "#FFD700"), #https://www.color-hex.com/
              'sex' = c("F" = "#F08080", "M" = "#63B8FF"),
              'nli_broad' = c("Cervical" = "#5CACEE", "Thoracic" = "#32CD32", "Lumbar" = "#CAFF70", "Sacral/ coccygeal" = "#FF7F50"),
              'nli' = c("C01" = "#4078a6", "C02" = "#4989be", "C03" = "#529ad6", "C04" = "#5cacee", "C05" = "#6cb4ef", "C06" = "#7cbcf1", "C07" = "#8cc4f3", "C08" = "#9dcdf4",
                               "T01" = "#196619", "T02" = "#1e7b1e", "T03" = "#238f23", "T04" = "#28a428", "T05" = "#2db82d", "T06" = "#32cd32", "T07" = "#46d246", "T08" = "#5ad75a", "T09" = "#6fdc6f", "T10" = "#84e184", "T11" = "#98e698", "T12" = "#adebad",
                               "L01" = "#a1cc59", "L02" = "#b5e564", "L03" = "#caff70", "L04" = "#d4ff8c", "L05" = "#dfffa9",
                               "S01" = "#b25838", "S02" = "#cc6540", "S03" = "#e57248", "S04" = "#ff7f50", "S05" = "#ff8b61", "Coc01" = "#ff9872"),
              'level_broad' = c("Cervical" = "#5CACEE", "Thoracic" = "#32CD32", "Lumbar" = "#CAFF70", "Sacral/ coccygeal" = "#FF7F50"),
              'level_fine' = c("C01" = "#4078a6", "C02" = "#4989be", "C03" = "#529ad6", "C04" = "#5cacee", "C05" = "#6cb4ef", "C06" = "#7cbcf1", "C07" = "#8cc4f3", "C08" = "#9dcdf4",
                               "T01" = "#196619", "T02" = "#1e7b1e", "T03" = "#238f23", "T04" = "#28a428", "T05" = "#2db82d", "T06" = "#32cd32", "T07" = "#46d246", "T08" = "#5ad75a", "T09" = "#6fdc6f", "T10" = "#84e184", "T11" = "#98e698", "T12" = "#adebad",
                               "L01" = "#a1cc59", "L02" = "#b5e564", "L03" = "#caff70", "L04" = "#d4ff8c", "L05" = "#dfffa9",
                               "S01" = "#b25838", "S02" = "#cc6540", "S03" = "#e57248", "S04" = "#ff7f50", "S05" = "#ff8b61", "Coc01" = "#ff9872"),
              'nli_gr'= c("C01 - C04" = "#5cacee", "C05 - C08" = "#9dcdf4", "T01 - T04" = "#28a428", "T05 - T08" = "#5ad75a", "T09 - T12" =  "#adebad"),
              'level_gr' = c("C01 - C04" = "#5cacee", "C05 - C08" = "#9dcdf4", "T01 - T04" = "#28a428", "T05 - T08" = "#5ad75a", "T09 - T12" =  "#adebad"),
              'bmi_status' = c("Underweight" = "#728FCE", "Healthy weight" = "#66CDAA", "Overweight" = "#F89880", "Obesity" = "#E55451"),
              'treatment_gr' = c("P" = "#FEA3AA", "D1" = "#C25283", "D2" = "#E75480"),
              'famotidine' = c("No" = "#749ee3", "Yes" = "#97d95d"),
              'ranitidine' = c("No" = "#749ee3", "Yes" = "#97d95d"),
              'cimetidine' = c("No" = "#749ee3", "Yes" = "#97d95d"),
              'famotidine_main' = c("No" = "#749ee3", "Yes" = "#97d95d"),
              'ranitidine_main' = c("No" = "#749ee3", "Yes" = "#97d95d"),
              'cimetidine_main' = c("No" = "#749ee3", "Yes" = "#97d95d"),
              'famotidine_color' = c("No" = "#749ee3", "Yes" = "#97d95d"),
              'ranitidine_color' = c("No" = "#749ee3", "Yes" = "#97d95d"),
              'cimetidine_color' = c("No" = "#749ee3", "Yes" = "#97d95d"),
              'famotidine_panel' = c("No" = "#749ee3", "Yes" = "#97d95d"),
              'ranitidine_panel' = c("No" = "#749ee3", "Yes" = "#97d95d"),
              'cimetidine_panel' = c("No" = "#749ee3", "Yes" = "#97d95d"),
              'age_range' = c("10 - 19" = "#FDE725FF", "20 - 29" = "#7AD151FF", "30 - 39" = "#22A884FF", "40 - 49" = "#2A788EFF", "50 - 59" = "#414487FF", "60 - 69" = "#440154FF"),
              'cluster' = c("1" = "#DDCC77", "2" = "#117733", "3" = "#44AA99", "4" = "#999933", "5" = "#6699CC", "6" = "#888888"))
  label <- c('lems' = 'Lower extremity motor score (LEMS)',
             'uems' = 'Upper extremity motor score (UEMS)',
             'ltscore' = 'Light touch total score',
             'ppscore' = 'Pinprick total score',
             'modben' = 'Modified Benzel scale',
             'age' = 'Age [years]',
             'age_range' = 'Age range [years]',
             'sex' = 'Sex',
             'severity' = 'AIS Grade',
             'nli' = 'Neurological Level of Injury at Baseline',
             'nli_broad' = 'Neurological Level of Injury at Baseline',
             'nli_gr' = 'Neurological Level of Injury at Baseline',
             'level_gr' = 'Spinal Level',
             'level_broad' = 'Spinal Level',
             'level_fine' = 'Spinal Level',
             'medication' = 'Medication',
             'famotidine' = 'Famotidine',
             'ranitidine' = 'Ranitidine',
             'cimetidine' = 'Cimetidine',
             'famotidine_main' = 'Famotidine',
             'ranitidine_main' = 'Ranitidine',
             'cimetidine_main' = 'Cimetidine',
             'famotidine_color' = 'Famotidine',
             'ranitidine_color' = 'Ranitidine',
             'cimetidine_color' = 'Cimetidine',
             'famotidine_panel' = 'Famotidine',
             'ranitidine_panel' = 'Ranitidine',
             'cimetidine_panel' = 'Cimetidine',
             'bmi_status' = 'Weight Status',
             'treatment_gr' = 'Treatment Group',
             'cluster' = 'Cluster')
  if (main %in% c('famotidine', 'ranitidine', 'cimetidine')) {
    main <- paste0(main, "_main")
  } 
  if (color %in% c('famotidine', 'ranitidine', 'cimetidine')) {
    color <- paste0(color, "_main")
  }
  if (panel %in% c('famotidine', 'ranitidine', 'cimetidine')) {
    panel <- paste0(panel, "_main")
  }
  if ((main == 'lems')|(main == 'uems')|(main == 'ltscore')|(main == 'ppscore')|(main == 'modben')){
    data <- reshaped_data
    data_col <- list('lems' = data$lems, 'uems' = data$uems, 'ltscore' = data$ltscore, 'ppscore' = data$ppscore, 'modben' = data$modben, 'age_range' = data$age_range, 'sex' = data$sexcd, 'severity' = data$asimpc01, 'nli_gr' = data$nligr1, 'level_gr' = data$lvlgr, 'level_fine' = data$splvl, 'level_broad' = data$lvl, 'patient' = data$ptid, 'week' = data$week, 'famotidine' = data$fam, 'ranitidine' = data$ran, 'cimetidine' = data$cim, 'bmi_status' = data$bmi_status, 'treatment_gr' = data$tx1_r, 'nli' = data$nli1, 'nli_broad' = data$nli1_broad, 'nli_gr' = data$nligr1, 'level_gr' = data$lvlgr, 'none' = NULL, 'famotidine_main' = data[[paste0("fam_", exposure_main[1], "_", exposure_main[2])]], 'famotidine_color' = data[[paste0("fam_", exposure_color[1], "_", exposure_color[2])]], 'famotidine_panel' = data[[paste0("fam_", exposure_panel[1], "_", exposure_panel[2])]], 'ranitidine_main' = data[[paste0("ran_", exposure_main[1], "_", exposure_main[2])]], 'ranitidine_color' = data[[paste0("ran_", exposure_color[1], "_", exposure_color[2])]], 'ranitidine_panel' = data[[paste0("ran_", exposure_panel[1], "_", exposure_panel[2])]], 'cimetidine_main' = data[[paste0("cim_", exposure_main[1], "_", exposure_main[2])]], 'cimetidine_color' = data[[paste0("cim_", exposure_color[1], "_", exposure_color[2])]], 'cimetidine_panel' = data[[paste0("cim_", exposure_panel[1], "_", exposure_panel[2])]])
    if ('cluster' %in% colnames(data)){
      data_col[['cluster']] <- data$cluster
    }
    if (type == "spaghetti"){
      if (color == 'none'){
        p <- ggplot(data = data, aes(x = data_col[['week']], y = data_col[[main]], group = data_col[['patient']])) + geom_line(color = "navy") + labs(y = label[[main]], x = "Time after injury (weeks)")
      } else {
        p <- ggplot(data = data, aes(x = data_col[['week']], y = data_col[[main]], group = data_col[['patient']], color = data_col[[color]])) + geom_line() + labs(y = label[[main]], x = "Time after injury (weeks)", color = label[[color]]) + scale_color_manual(values = val[[color]])
      }
      if (panel != 'none'){
        p <- p + facet_wrap(~data_col[[panel]])
      }
      return(p)
    } else if (type == "box"){
      if (color == 'none'){
        p <- ggplot(data = data, aes(x = as.factor(data_col[['week']]), y = data_col[[main]])) + geom_boxplot(alpha=.3, fill = "navy") + labs(y = label[[main]], x = "Time after injury (weeks)")
      } else {
        p <- ggplot(data = data, aes(x = as.factor(data_col[['week']]), y = data_col[[main]], fill = data_col[[color]])) + geom_boxplot() + labs(y = label[[main]], x = "Time after injury (weeks)", fill = label[[color]]) + scale_fill_manual(values = val[[color]])
      }
      if (panel != 'none'){
        p <- p + facet_wrap(~data_col[[panel]])
      }
      return(p)
    } else if (type == "line") {
      if (main %in% c('lems', 'uems')) {
        y_max <- 50
      } else if (main %in% c('ltscore', 'ppscore')) {
        y_max <- 112
      } else if (main == 'modben') {
        y_max <- 7
      }
      if ((color == "none")&(panel == "none")){
        df_mean <- aggregate(data_col[[main]], list(as.factor(data_col[['week']])), FUN = function(x){mean(x, na.rm = TRUE)})
        colnames(df_mean) <- c("week", "outcome_mean")
        df_se <- aggregate(data_col[[main]], list(as.factor(data_col[['week']])), FUN = function(x){se(x, narm = TRUE)})
        colnames(df_se) <- c("week", "outcome_se")
        df <- merge(df_mean, df_se, by = "week")
        p <- ggplot(data = df, aes(x = as.numeric(as.character(week)), y = outcome_mean, ymin = (outcome_mean - outcome_se), ymax = (outcome_mean + outcome_se))) + geom_line(size = 1.5, color = 'navy') + geom_ribbon(fill = 'navy', alpha = 0.3, color = NA) + labs(y = label[[main]], x = "Time after injury (weeks)") + ylim(0, y_max)
      } else if ((color != "none")&(panel == "none")){
        df_mean <- aggregate(data_col[[main]], list(as.factor(as.character(data_col[['week']])), data_col[[color]]), FUN = function(x){mean(x, na.rm = TRUE)})
        colnames(df_mean) <- c("week", color, "outcome_mean")
        df_se <- aggregate(data_col[[main]], list(as.factor(as.character(data_col[['week']])), data_col[[color]]), FUN = function(x){se(x, narm = TRUE)})
        colnames(df_se) <- c("week", color, "outcome_se")
        df <- merge(df_mean, df_se, by = c("week", color))
        p <- ggplot(data = df, aes(x = as.numeric(as.character(week)), y = outcome_mean, ymin = (outcome_mean - outcome_se), ymax = (outcome_mean + outcome_se), color = df[[color]], fill = df[[color]])) + geom_line(size = 1.5) + geom_ribbon(alpha = 0.3, color = NA) + labs(y = label[[main]], x = "Time after injury (weeks)", color = label[[color]], fill = label[[color]]) + ylim(0, y_max) + scale_fill_manual(values = val[[color]]) + scale_color_manual(values = val[[color]])
      } else if ((color == "none")&(panel != "none")){
        df_mean <- aggregate(data_col[[main]], list(as.factor(as.character(data_col[['week']])), data_col[[panel]]), FUN = function(x){mean(x, na.rm = TRUE)})
        colnames(df_mean) <- c("week", panel, "outcome_mean")
        df_se <- aggregate(data_col[[main]], list(as.factor(as.character(data_col[['week']])), data_col[[panel]]), FUN = function(x){se(x, narm = TRUE)})
        colnames(df_se) <- c("week", panel, "outcome_se")
        df <- merge(df_mean, df_se, by = c("week", panel))
        p <- ggplot(data = df, aes(x = as.numeric(as.character(week)), y = outcome_mean, ymin = (outcome_mean - outcome_se), ymax = (outcome_mean + outcome_se))) + geom_line(size = 1.5, color = 'navy') + geom_ribbon(color = 'navy', alpha = 0.3, color = NA) + labs(y = label[[main]], x = "Time after injury (weeks)") + ylim(0, y_max) + facet_wrap(~df[[panel]])
      } else {
        df_mean <- aggregate(data_col[[main]], list(as.factor(as.character(data_col[['week']])), data_col[[color]], data_col[[panel]]), FUN = function(x){mean(x, na.rm = TRUE)})
        colnames(df_mean) <- c("week", color, panel, "outcome_mean")
        df_se <- aggregate(data_col[[main]], list(as.factor(as.character(data_col[['week']])), data_col[[color]], data_col[[panel]]), FUN = function(x){se(x, narm = TRUE)})
        colnames(df_se) <- c("week", color, panel, "outcome_se")
        df <- merge(df_mean, df_se, by = c("week", color, panel))
        p <- ggplot(data = df, aes(x = as.numeric(as.character(week)), y = outcome_mean, ymin = (outcome_mean - outcome_se), ymax = (outcome_mean + outcome_se), color = df[[color]], fill = df[[color]])) + geom_line(size = 1.5) + geom_ribbon(alpha = 0.3, color = NA) + labs(y = label[[main]], x = "Time after injury (weeks)", color = label[[color]], fill = label[[color]]) + ylim(0, y_max) + scale_fill_manual(values = val[[color]]) + scale_color_manual(values = val[[color]]) + facet_wrap(~df[[panel]])
      }
      return(p)
    } else {
      return(NULL)
    }
  } else {
    data_col <- list('age' = data$age, 'sex' = data$sexcd, 'nli_gr' = data$nligr1, 'level_gr' = data$lvlgr, 'severity' = data$asimpc01, 'level_fine' = data$splvl, 'level_broad' = data$lvl, 'patient' = data$ptid, 'bmi_status' = data$bmi_status, 'treatment_gr' = data$tx1_r, 'famotidine' = data$fam, 'ranitidine' = data$ran, 'cimetidine' = data$cim, 'nli' = data$nli1, 'nli_broad' = data$nli1_broad, 'none' = NULL, 'famotidine_main' = data[[paste0("fam_", exposure_main[1], "_", exposure_main[2])]], 'famotidine_color' = data[[paste0("fam_", exposure_color[1], "_", exposure_color[2])]], 'famotidine_panel' = data[[paste0("fam_", exposure_panel[1], "_", exposure_panel[2])]], 'ranitidine_main' = data[[paste0("ran_", exposure_main[1], "_", exposure_main[2])]], 'ranitidine_color' = data[[paste0("ran_", exposure_color[1], "_", exposure_color[2])]], 'ranitidine_panel' = data[[paste0("ran_", exposure_panel[1], "_", exposure_panel[2])]], 'cimetidine_main' = data[[paste0("cim_", exposure_main[1], "_", exposure_main[2])]], 'cimetidine_color' = data[[paste0("cim_", exposure_color[1], "_", exposure_color[2])]], 'cimetidine_panel' = data[[paste0("cim_", exposure_panel[1], "_", exposure_panel[2])]])
    data_col2 <- list('age' = 'age', 'sex' = 'sexcd', 'severity' = 'asimpc01', 'nli_gr' = 'nligr1', 'level_gr' = 'lvlgr', 'level_fine' = 'splvl', 'level_broad' = 'lvl', 'patient' = 'ptid', 'bmi_status' = 'bmi_status', 'treatment_gr' = 'tx1_r', 'famotidine' = 'fam', 'ranitidine' = 'ran', 'cimetidine' = 'cim', 'nli' = 'nli1', 'nli_broad' = 'nli1_broad', 'none' = NULL, 'famotidine_main' = paste0("fam_", exposure_main[1], "_", exposure_main[2]), 'famotidine_color' = paste0("fam_", exposure_color[1], "_", exposure_color[2]), 'famotidine_panel' = paste0("fam_", exposure_panel[1], "_", exposure_panel[2]), 'ranitidine_main' = paste0("ran_", exposure_main[1], "_", exposure_main[2]), 'ranitidine_color' = paste0("ran_", exposure_color[1], "_", exposure_color[2]), 'ranitidine_panel' = paste0("ran_", exposure_panel[1], "_", exposure_panel[2]), 'cimetidine_main' = paste0("cim_", exposure_main[1], "_", exposure_main[2]), 'cimetidine_color' = paste0("cim_", exposure_color[1], "_", exposure_color[2]), 'cimetidine_panel' = paste0("cim_", exposure_panel[1], "_", exposure_panel[2]))
    if ('cluster' %in% colnames(data)){
      data_col[['cluster']] <- data$cluster
      data_col2[['cluster']] <- 'cluster'
    }
    if (main == "age"){
      if (!is.null(imp_or_all)){
        if (imp_or_all == 'imp'){
          data <- data[data$.imp == imp_data,]
          data_col <- list('age' = data$age, 'sex' = data$sexcd, 'nli_gr' = data$nligr1, 'level_gr' = data$lvlgr, 'severity' = data$asimpc01, 'level_fine' = data$splvl, 'level_broad' = data$lvl, 'patient' = data$ptid, 'bmi_status' = data$bmi_status, 'treatment_gr' = data$tx1_r, 'famotidine' = data$fam, 'ranitidine' = data$ran, 'cimetidine' = data$cim, 'nli' = data$nli1, 'nli_broad' = data$nli1_broad, 'none' = NULL, 'famotidine_main' = data[[paste0("fam_", exposure_main[1], "_", exposure_main[2])]], 'famotidine_color' = data[[paste0("fam_", exposure_color[1], "_", exposure_color[2])]], 'famotidine_panel' = data[[paste0("fam_", exposure_panel[1], "_", exposure_panel[2])]], 'ranitidine_main' = data[[paste0("ran_", exposure_main[1], "_", exposure_main[2])]], 'ranitidine_color' = data[[paste0("ran_", exposure_color[1], "_", exposure_color[2])]], 'ranitidine_panel' = data[[paste0("ran_", exposure_panel[1], "_", exposure_panel[2])]], 'cimetidine_main' = data[[paste0("cim_", exposure_main[1], "_", exposure_main[2])]], 'cimetidine_color' = data[[paste0("cim_", exposure_color[1], "_", exposure_color[2])]], 'cimetidine_panel' = data[[paste0("cim_", exposure_panel[1], "_", exposure_panel[2])]])
          data_col2 <- list('age' = 'age', 'sex' = 'sexcd', 'severity' = 'asimpc01', 'nli_gr' = 'nligr1', 'level_gr' = 'lvlgr', 'level_fine' = 'splvl', 'level_broad' = 'lvl', 'patient' = 'ptid', 'bmi_status' = 'bmi_status', 'treatment_gr' = 'tx1_r', 'famotidine' = 'fam', 'ranitidine' = 'ran', 'cimetidine' = 'cim', 'nli' = 'nli1', 'nli_broad' = 'nli1_broad', 'none' = NULL, 'famotidine_main' = paste0("fam_", exposure_main[1], "_", exposure_main[2]), 'famotidine_color' = paste0("fam_", exposure_color[1], "_", exposure_color[2]), 'famotidine_panel' = paste0("fam_", exposure_panel[1], "_", exposure_panel[2]), 'ranitidine_main' = paste0("ran_", exposure_main[1], "_", exposure_main[2]), 'ranitidine_color' = paste0("ran_", exposure_color[1], "_", exposure_color[2]), 'ranitidine_panel' = paste0("ran_", exposure_panel[1], "_", exposure_panel[2]), 'cimetidine_main' = paste0("cim_", exposure_main[1], "_", exposure_main[2]), 'cimetidine_color' = paste0("cim_", exposure_color[1], "_", exposure_color[2]), 'cimetidine_panel' = paste0("cim_", exposure_panel[1], "_", exposure_panel[2]))
          if ('cluster' %in% colnames(data)){
            data_col[['cluster']] <- data$cluster
            data_col2[['cluster']] <- 'cluster'
          }
        }
      }
      if (type == "distribution") {
        if (color == 'none'){
          p <- ggplot(data = data, aes(x = data_col[[main]])) + geom_density(alpha=.3, color = "navy", fill = "navy") + labs(x = label[[main]], y = "Density") # + geom_vline(xintercept = mean(data_col[[main]]), color = "navy")
        } else {
          p <- ggplot(data = data, aes(x = data_col[[main]], color = data_col[[color]], fill = data_col[[color]])) + geom_density(alpha=.3) + scale_fill_manual(values = val[[color]]) + scale_color_manual(values = val[[color]]) + labs(x = label[[main]], y = "Density", color = label[[color]], fill = label[[color]])
        }
        if (panel != 'none'){
          p <- p + facet_wrap(~data_col[[panel]])
        } 
        return(p)
      } else if (type == "box"){
        if (color == 'none'){
          p <- ggplot(data = data, aes(x = "", y = data_col[[main]])) + geom_boxplot(alpha=.3, fill = "navy") + labs(y = label[[main]], x = "All")
        } else {
          p <- ggplot(data = data, aes(x = data_col[[color]], y = data_col[[main]], fill = data_col[[color]])) + geom_boxplot() + labs(y = label[[main]], x = label[[color]], fill = label[[color]]) + scale_fill_manual(values = val[[color]])
        }
        if (panel != 'none'){
          p <- p + facet_wrap(~data_col[[panel]])
        }
        return(p)
      } else {
        return(NULL)
      }
      
    } else if (main == "medication") {
      data$Famotidine <- as.numeric(ifelse(data[[paste0("fam_", exposure_main[1], "_", exposure_main[2])]] == "Yes", 1, 0))
      data$Ranitidine <- as.numeric(ifelse(data[[paste0("ran_", exposure_main[1], "_", exposure_main[2])]] == "Yes", 1, 0))
      data$Cimetidine <- as.numeric(ifelse(data[[paste0("cim_", exposure_main[1], "_", exposure_main[2])]] == "Yes", 1, 0))
      if (type == 'upset'){
        if ((color == 'none')&(panel == 'none')){
          #p <- UpSetR::upset(data, sets = c("Famotidine", "Ranitidine", "Cimetidine"), mb.ratio = c(0.55, 0.45), order.by = "freq")
          p <- ComplexUpset::upset(
            data, 
            c("Famotidine", "Ranitidine", "Cimetidine"), 
            base_annotations = list(
              "Intersection size" = list(
                aes=aes(x=intersection),
                geom=list(
                  geom_bar(stat='count')
                ))), name = "")
        } else if ((color != 'none')&((panel == 'none')|(panel == color))) {
          #p <- NULL
          #for (x in 1:length(levels(data_col[[color]])[levels(data_col[[color]]) %in% unique(data_col[[color]])])){
            #name <- levels(data_col[[color]])[levels(data_col[[color]]) %in% unique(data_col[[color]])][[x]]
            #p <- p | ComplexUpset::upset(data[data_col[[color]] == name,], c("Famotidine", "Ranitidine", "Cimetidine"), name = name)
          #}
          p <- ComplexUpset::upset(
            data,
            c("Famotidine", "Ranitidine", "Cimetidine"),
            base_annotations=list(
              'Intersection size'=intersection_size(
                counts=FALSE,
                mapping=aes(fill = data_col[[color]])
              ) + scale_fill_manual(values = val[[color]]) + labs(fill = label[[color]])
            ), name = ""
          )
        } else if ((color == 'none')&(panel != 'none')){
          #p <- NULL
          #for (x in 1:length(levels(data_col[[panel]])[levels(data_col[[panel]]) %in% unique(data_col[[panel]])])){
            #name <- levels(data_col[[panel]])[levels(data_col[[panel]]) %in% unique(data_col[[panel]])][[x]]
            #p <- p | ComplexUpset::upset(data[data_col[[panel]] == name,], c("Famotidine", "Ranitidine", "Cimetidine"), name = name)
          #}
          p <- ComplexUpset::upset(
            data,
            c("Famotidine", "Ranitidine", "Cimetidine"),
            base_annotations=list(
              'Intersection size'=intersection_size(
                counts=FALSE,
                mapping=aes(fill = data_col[[panel]])
              ) + scale_fill_manual(values = val[[panel]]) + labs(fill = label[[panel]])
            ), name = ""
          )
        } else {
          #p <- NULL
          #for (x in 1:length(levels(data_col[[color]])[levels(data_col[[color]]) %in% unique(data_col[[color]])])){
            #name_x <- levels(data_col[[color]])[levels(data_col[[color]]) %in% unique(data_col[[color]])][[x]]
            #px <- NULL
            #for (y in 1:length(levels(data_col[[panel]])[levels(data_col[[panel]]) %in% unique(data_col[[panel]])])){
              #name_y <- levels(data_col[[panel]])[levels(data_col[[panel]]) %in% unique(data_col[[panel]])][[y]]
              #name <- paste(name_x, name_y, sep = " - ")
              #px <- px | ComplexUpset::upset(data[(data_col[[color]] == name_x)&(data_col[[panel]] == name_y),], c("Famotidine", "Ranitidine", "Cimetidine"), name = name)
            #}
            #p <- p / px
          #}
          plist <- lapply(as.list(1:length(levels(data_col[[panel]])[levels(data_col[[panel]]) %in% unique(data_col[[panel]])])), FUN = function(x){
            name <- levels(data_col[[panel]])[levels(data_col[[panel]]) %in% unique(data_col[[panel]])][[x]]
            plot_data <- data[data_col[[panel]] == name,]
            data_col_plot <- list('age' = plot_data$age, 'sex' = plot_data$sexcd, 'severity' = plot_data$asimpc01, 'nli_gr' = data$nligr1, 'level_gr' = data$lvlgr, 'level_fine' = plot_data$splvl, 'level_broad' = plot_data$lvl, 'patient' = plot_data$ptid, 'bmi_status' = plot_data$bmi_status, 'treatment_gr' = plot_data$tx1_r, 'famotidine' = plot_data$fam, 'ranitidine' = plot_data$ran, 'cimetidine' = plot_data$cim, 'none' = NULL, 'famotidine_main' = data[[paste0("fam_", exposure_main[1], "_", exposure_main[2])]], 'famotidine_color' = data[[paste0("fam_", exposure_color[1], "_", exposure_color[2])]], 'famotidine_panel' = data[[paste0("fam_", exposure_panel[1], "_", exposure_panel[2])]], 'ranitidine_main' = data[[paste0("ran_", exposure_main[1], "_", exposure_main[2])]], 'ranitidine_color' = data[[paste0("ran_", exposure_color[1], "_", exposure_color[2])]], 'ranitidine_panel' = data[[paste0("ran_", exposure_panel[1], "_", exposure_panel[2])]], 'cimetidine_main' = data[[paste0("cim_", exposure_main[1], "_", exposure_main[2])]], 'cimetidine_color' = data[[paste0("cim_", exposure_color[1], "_", exposure_color[2])]], 'cimetidine_panel' = data[[paste0("cim_", exposure_panel[1], "_", exposure_panel[2])]])
            if ('cluster' %in% colnames(plot_data)){
              data_col_plot[['cluster']] <- plot_data$cluster
            }
            p_add <- ComplexUpset::upset(
              plot_data,
              c("Famotidine", "Ranitidine", "Cimetidine"),
              base_annotations=list(
                'Intersection size'=intersection_size(
                  counts=FALSE,
                  mapping=aes(fill = data_col_plot[[color]])
                ) + scale_fill_manual(values = val[[color]]) + labs(fill = label[[color]])
              ), name = name
            )
          })
          p <- plot_grid(plotlist = plist)
        }
        return(p)
      } else if (type == 'upset2'){
        if ((color == 'none')&(panel == 'none')){
          p <- ComplexUpset::upset(
            data, 
            c("Famotidine", "Ranitidine", "Cimetidine"), 
            base_annotations = list(
              "Percentage" = list(
                aes=aes(x=intersection, fill=intersection),
                geom=list(
                geom_bar(stat='count', position='fill'),
                scale_y_continuous(labels=scales::percent_format())
              ))), name = "")
        } else if ((color != 'none')&((panel == 'none')|(panel == color))) {
          #p <- NULL
          #for (x in 1:length(levels(data_col[[color]])[levels(data_col[[color]]) %in% unique(data_col[[color]])])){
          #name <- levels(data_col[[color]])[levels(data_col[[color]]) %in% unique(data_col[[color]])][[x]]
          #p <- p | ComplexUpset::upset(data[data_col[[color]] == name,], c("Famotidine", "Ranitidine", "Cimetidine"), name = name)
          #}
          p <- ComplexUpset::upset(
            data, 
            c("Famotidine", "Ranitidine", "Cimetidine"), 
            base_annotations = list(
              "Percentage" = list(
                aes=aes(x=intersection, fill=data_col[[color]]),
                geom=list(
                  geom_bar(stat='count', position='fill'),
                  scale_y_continuous(labels=scales::percent_format()),
                  scale_fill_manual(values = val[[color]]),
                  labs(fill = label[[color]])
                ))), name = "")
        } else if ((color == 'none')&(panel != 'none')){
          #p <- NULL
          #for (x in 1:length(levels(data_col[[panel]])[levels(data_col[[panel]]) %in% unique(data_col[[panel]])])){
          #name <- levels(data_col[[panel]])[levels(data_col[[panel]]) %in% unique(data_col[[panel]])][[x]]
          #p <- p | ComplexUpset::upset(data[data_col[[panel]] == name,], c("Famotidine", "Ranitidine", "Cimetidine"), name = name)
          #}
          p <- ComplexUpset::upset(
            data, 
            c("Famotidine", "Ranitidine", "Cimetidine"), 
            base_annotations = list(
              "Percentage" = list(
                aes=aes(x=intersection, fill=data_col[[panel]]),
                geom=list(
                  geom_bar(stat='count', position='fill'),
                  scale_y_continuous(labels=scales::percent_format()),
                  scale_fill_manual(values = val[[panel]]),
                  labs(fill = label[[panel]])
                ))), name = "")
        } else {
          #p <- NULL
          #for (x in 1:length(levels(data_col[[color]])[levels(data_col[[color]]) %in% unique(data_col[[color]])])){
          #name_x <- levels(data_col[[color]])[levels(data_col[[color]]) %in% unique(data_col[[color]])][[x]]
          #px <- NULL
          #for (y in 1:length(levels(data_col[[panel]])[levels(data_col[[panel]]) %in% unique(data_col[[panel]])])){
          #name_y <- levels(data_col[[panel]])[levels(data_col[[panel]]) %in% unique(data_col[[panel]])][[y]]
          #name <- paste(name_x, name_y, sep = " - ")
          #px <- px | ComplexUpset::upset(data[(data_col[[color]] == name_x)&(data_col[[panel]] == name_y),], c("Famotidine", "Ranitidine", "Cimetidine"), name = name)
          #}
          #p <- p / px
          #}
          
          plist <- lapply(as.list(1:length(levels(data_col[[panel]])[levels(data_col[[panel]]) %in% unique(data_col[[panel]])])), FUN = function(x) {
            name <- levels(data_col[[panel]])[levels(data_col[[panel]]) %in% unique(data_col[[panel]])][[x]]
            plot_data <- data[data_col[[panel]] == name,]
            data_col_plot <- list('age' = plot_data$age, 'sex' = plot_data$sexcd, 'severity' = plot_data$asimpc01, 'nli_gr' = data$nligr1, 'level_gr' = data$lvlgr, 'level_fine' = plot_data$splvl, 'level_broad' = plot_data$lvl, 'patient' = plot_data$ptid, 'bmi_status' = plot_data$bmi_status, 'treatment_gr' = plot_data$tx1_r, 'famotidine' = plot_data$fam, 'ranitidine' = plot_data$ran, 'cimetidine' = plot_data$cim, 'none' = NULL, 'famotidine_main' = data[[paste0("fam_", exposure_main[1], "_", exposure_main[2])]], 'famotidine_color' = data[[paste0("fam_", exposure_color[1], "_", exposure_color[2])]], 'famotidine_panel' = data[[paste0("fam_", exposure_panel[1], "_", exposure_panel[2])]], 'ranitidine_main' = data[[paste0("ran_", exposure_main[1], "_", exposure_main[2])]], 'ranitidine_color' = data[[paste0("ran_", exposure_color[1], "_", exposure_color[2])]], 'ranitidine_panel' = data[[paste0("ran_", exposure_panel[1], "_", exposure_panel[2])]], 'cimetidine_main' = data[[paste0("cim_", exposure_main[1], "_", exposure_main[2])]], 'cimetidine_color' = data[[paste0("cim_", exposure_color[1], "_", exposure_color[2])]], 'cimetidine_panel' = data[[paste0("cim_", exposure_panel[1], "_", exposure_panel[2])]])
            if ('cluster' %in% colnames(plot_data)){
              data_col_plot[['cluster']] <- plot_data$cluster
            }
            ComplexUpset::upset(
              plot_data, 
              c("Famotidine", "Ranitidine", "Cimetidine"), 
              base_annotations = list(
                "Percentage" = list(
                  aes=aes(x=intersection, fill=data_col_plot[[color]]),
                  geom=list(
                    geom_bar(stat='count', position='fill'),
                    scale_y_continuous(labels=scales::percent_format()),
                    scale_fill_manual(values = val[[color]]),
                    labs(fill = label[[color]])
                  ))), name = name)
          })
          p <- plot_grid(plotlist = plist)
        }
        return(p)
      } else {
        return(NULL)
      }
    } else {
      if (type == "bar"){
        if (!is.null(imp_or_all)){
          if (imp_or_all == 'all'){
            count1 <- data %>% dplyr::count(data_col[[main]], data_col[[color]], data_col[[panel]], data$.imp)
            count1 <- as.data.frame(count1)
            nam <- names(count1)
            count <- aggregate(count1$n, lapply(1:(length(nam)-2), function(col){count1[[col]]}), function(x)c("m" = mean(x), "se" = se(x)))
            count$m <- count$x[,"m"]
            count$se <- count$x[,"se"]
            count <- dplyr::select(count, -c("x"))
            names(count) <- c(names(count1)[1:(length(nam)-2)], "n", 'se')
          } else {
            count1 <- data %>% dplyr::count(data_col[[main]], data_col[[color]], data_col[[panel]], data$.imp)
            count1 <- as.data.frame(count1)
            count <- count1[count1[[ncol(count1)-1]] == imp_data,]
          }
        } else {
          count <- data %>% dplyr::count(data_col[[main]], data_col[[color]], data_col[[panel]])
          count <- as.data.frame(count)
        }
        #if ((main == 'level_fine')&(length(colnames(count)) == 2)){
        #  l <- c("Coc01", "S05", "S04", "S03", "S02", "S01", "L05", "L04", "L03", "L02", "L01", "T12", "T11", "T10", "T09", "T08", "T07", "T06", "T05", "T04", "T03", "T02", "T01", "C08", "C07", "C06", "C05", "C04", "C03", "C02", "C01")
        #  for (x in l[!(l %in% count[[1]])]){
        #    count <- rbind(count, list(x, 0))
        #  }
        #  count[[1]] <- factor(count[[1]], levels = l)
        #}
        #if ((main == 'level_broad')&(length(colnames(count)) == 2)){
        #  l <- c("Sacral/ coccygeal", "Lumbar", "Thoracic", "Cervical")
        #  for (x in l[!(l %in% count[[1]])]){
        #    count <- rbind(count, list(x, 0))
        #  }
        #  count[[1]] <- factor(count[[1]], levels = l)
        #}
        if ((main == 'level_broad')|(main == 'level_fine')|(main == 'level_gr')|(main == 'nli_broad')|(main == 'nli_fine')|(main == 'nli_gr')){
          p <- ggplot(data = count, aes(x = factor(count[[1]]), y = n, fill = factor(count[[1]]))) + geom_bar(stat="identity") + labs(x = label[[main]], y = "Counts") + scale_fill_manual(values = val[[main]]) + theme(legend.position = "none") + coord_flip()
          if (color != 'none'){
            if (panel == 'none'){
              p <- p + facet_wrap(~count[[2]])
            } else {
              p <- p + facet_grid(count[[2]]~count[[3]])
            }
          }
        } else {
          if (color == 'none'){
            p <- ggplot(data = count, aes(x = factor(count[[1]]), y = n, fill = factor(count[[1]]))) + geom_bar(stat="identity") + labs(x = label[[main]], y = "Counts", fill = label[[main]]) + scale_fill_manual(values = val[[main]])
            if (panel != 'none'){
              p <- p + facet_wrap(~count[[2]])
            }
          } else {
            p <- ggplot(data = count, aes(x = factor(count[[2]]), y = n, fill = count[[1]])) + geom_bar(stat="identity", position = position_dodge()) + labs(x = label[[color]], y = "Counts", fill = label[[main]]) + scale_fill_manual(values = val[[main]])
            if (panel != 'none'){
              if (color != panel){
                p <- p + facet_wrap(~count[[3]])
              } else {
                p <- p + facet_wrap(~count[[2]])
              }
            }
          }
        }
        if (!is.null(imp_or_all)){
          if (imp_or_all == 'all'){
            p <- p + geom_errorbar(aes(ymin = n-se, ymax = n+se), width=.2,position=position_dodge(.9))
          }
        }
        return(p)
      } else if (type == "pie") {
        if (!is.null(imp_or_all)){
          if (imp_or_all == 'all'){
            count1 <- data %>% dplyr::count(data_col[[main]], data_col[[color]], data_col[[panel]], data$.imp)
            count1 <- as.data.frame(count1)
            nam <- names(count1)
            count <- aggregate(count1$n, lapply(1:(length(nam)-2), function(col){count1[[col]]}), function(x)c("m" = mean(x), "se" = se(x)))
            count$m <- count$x[,"m"]
            count$se <- count$x[,"se"]
            count <- dplyr::select(count, -c("x"))
            names(count) <- c(names(count1)[1:(length(nam)-2)], "n", 'se')
          } else {
            count1 <- data %>% dplyr::count(data_col[[main]], data_col[[color]], data_col[[panel]], data$.imp)
            count1 <- as.data.frame(count1)
            count <- count1[count1[[ncol(count1)-1]] == imp_data,]
          }
        } else {
          count <- data %>% dplyr::count(data_col[[main]], data_col[[color]], data_col[[panel]])
          count <- as.data.frame(count)
        }
        #if ((main == 'level_fine')&(length(colnames(count)) == 2)){
        #  l <- c("Coc01", "S05", "S04", "S03", "S02", "S01", "L05", "L04", "L03", "L02", "L01", "T12", "T11", "T10", "T09", "T08", "T07", "T06", "T05", "T04", "T03", "T02", "T01", "C08", "C07", "C06", "C05", "C04", "C03", "C02", "C01")
        #  for (x in l[!(l %in% count[[1]])]){
        #    count <- rbind(count, list(x, 0))
        #  }
        #  count[[1]] <- factor(count[[1]], levels = l)
        #}
        #if ((main == 'level_broad')&(length(colnames(count)) == 2)){
        #  l <- c("Sacral/ coccygeal", "Lumbar", "Thoracic", "Cervical")
        #  for (x in l[!(l %in% count[[1]])]){
        #    count <- rbind(count, list(x, 0))
        #  }
        #  count[[1]] <- factor(count[[1]], levels = l)
        #}
        cp <- coord_polar(theta = "y", start = 0)
        cp$is_free <- function() TRUE
        if (color == 'none') {
          if (panel == 'none') {
            p <- ggplot(data=count, aes(x="", y = n, fill=factor(count[[1]]))) + geom_bar(width = 1, stat="identity") + labs(fill = label[[main]], y = "Counts") + scale_fill_manual(values = val[[main]]) + theme_void() + theme(axis.text.x=element_blank(), plot.title = element_text(hjust = 0.5)) + cp + geom_label(aes(label = paste(as.character(round((n/sum(n))*100)), "%")), position = position_stack(vjust = 0.5), show.legend = FALSE, label.size = NA)
          } else {
            agg <- aggregate(count$n, by=list(cat = count[[2]]), FUN = sum)
            colnames(agg) <- c(colnames(count)[[2]], 'summe')
            count <- merge(count, agg, by = colnames(count)[[2]]) # column order changes! 
            p <- ggplot(data=count, aes(x="", y = n, fill=factor(count[[2]]))) + geom_bar(width = 1, stat="identity") + labs(fill = label[[main]], y = "Counts") + scale_fill_manual(values = val[[main]]) + theme_void() + theme(axis.text.x=element_blank(), plot.title = element_text(hjust = 0.5)) + cp + geom_label(aes(label = paste(as.character(round((n/as.numeric(n))*100)), "%")), position = position_stack(vjust = 0.5), show.legend = FALSE, label.size = NA) + facet_wrap(~count[[1]], scales = "free")
          }
        } else {
          if ((panel == 'none')|(panel == color)) {
            agg <- aggregate(count$n, by=list(cat = count[[2]]), FUN = sum)
            colnames(agg) <- c(colnames(count)[[2]], 'summe')
            count <- merge(count, agg, by = colnames(count)[[2]])
            p <- ggplot(data=count, aes(x="", y = n, fill=factor(count[[2]]))) + geom_bar(width = 1, stat="identity") + labs(fill = label[[main]], y = "Counts") + scale_fill_manual(values = val[[main]]) + theme_void() + theme(axis.text.x=element_blank(), plot.title = element_text(hjust = 0.5)) + cp + geom_label(aes(label = paste(as.character(round((n/summe)*100)), "%")), position = position_stack(vjust = 0.5), show.legend = FALSE, label.size = NA) + facet_wrap(~count[[1]], scales = "free")
          } else {
            agg <- aggregate(count$n, by=list(cat1 = count[[2]], cat2 = count[[3]]), FUN = sum)
            colnames(agg) <- c(colnames(count)[[2]], colnames(count)[[3]], 'summe')
            count <- merge(x = count, y = agg, by.x = c(colnames(count)[[2]], colnames(count)[[3]]), by.y = c(colnames(count)[[2]], colnames(count)[[3]]))
            p <- ggplot(data=count, aes(x="", y = n, fill=factor(count[[3]]))) + geom_bar(width = 1, stat="identity") + labs(fill = label[[main]], y = "Counts") + scale_fill_manual(values = val[[main]]) + theme_void() + theme(axis.text.x=element_blank(), plot.title = element_text(hjust = 0.5)) + cp + geom_label(aes(label = paste(as.character(round((n/summe)*100)), "%")), position = position_stack(vjust = 0.5), show.legend = FALSE, label.size = NA) + facet_wrap(~count[[1]]+count[[2]], scales = "free", ncol = length(unique(count[[2]])))
          }
        }
        return(p)
      } else {
        return(NULL)
      }
    }
  }
}


# Function to create a demographic table based on selected input
createDemTable <- function(data = sygen_data, main = 'none', color = 'none', panel = 'none', exposure_main = NULL, exposure_color = NULL, exposure_panel = NULL, imp_or_all = NULL, imp_data = NULL){ # parameters named according to the plotting function
  # if only main is given, the function will display that characteristic as explanatory and no dependent will be given
  # if color or panel (or color = panel) is given additionally, this will be the explanatory
  # if color and panel are given (and are different), two tables will be returned
  if (!("Famotidine" %in% colnames(data))){
    if (!is.null(imp_or_all)){
      data$Famotidine <- rep(sygen_data[sygen_data$ptid %in% data$ptid,]$Famotidine, 20)
    } else {
      data$Famotidine <- sygen_data[sygen_data$ptid %in% data$ptid,]$Famotidine
    }
    
  }
  if (!("Ranitidine" %in% colnames(data))){
    if (!is.null(imp_or_all)){
      data$Ranitidine <- rep(sygen_data[sygen_data$ptid %in% data$ptid,]$Ranitidine, 20)
    } else {
      data$Ranitidine <- sygen_data[sygen_data$ptid %in% data$ptid,]$Ranitidine
    }
    
  }
  if (!("Cimetidine" %in% colnames(data))){
    if (!is.null(imp_or_all)){
      data$Cimetidine <- rep(sygen_data[sygen_data$ptid %in% data$ptid,]$Cimetidine, 20)
    } else {
      data$Cimetidine <- sygen_data[sygen_data$ptid %in% data$ptid,]$Cimetidine
    }
    
  }
  if (!(main %in% c('medication', 'famotidine', 'cimetidine', 'ranitidine'))) {
    exposure_main <- c(0, 365)
  }
  if (!(color %in% c('medication', 'famotidine', 'cimetidine', 'ranitidine'))) {
    exposure_color <- c(0, 365)
  }
  if (!(panel %in% c('medication', 'famotidine', 'cimetidine', 'ranitidine'))) {
    exposure_panel <- c(0, 365)
  }
  data$fam <- factor(ifelse(data$Famotidine == 1, "Yes", "No"), levels = c("Yes", "No"))
  data$ran <- factor(ifelse(data$Ranitidine == 1, "Yes", "No"), levels = c("Yes", "No"))
  data$cim <- factor(ifelse(data$Cimetidine == 1, "Yes", "No"), levels = c("Yes", "No"))
  for (component in c(main, panel, color)) {
    if (component == main) {
      exposure <- exposure_main
    } else if (component == color) {
      exposure <- exposure_color
    } else if (component == panel) {
      exposure <- exposure_panel
    }
    for (med in c('famotidine', 'ranitidine', 'cimetidine')) {
      if ((component == med)|(component == 'medication')) {
        for (day in exposure[1]:exposure[2]) {
          if (sum(exposure == c(0, 365)) != 2) {
            for (day in exposure[1]:exposure[2]) {
              data[paste0(toupper(med), "_", day)] <- as.numeric(data[[paste0(toupper(med), "_", day)]])
              if (day == 0) {
                data[paste0(toupper(med), "_", exposure[1], "_", exposure[2])] <- ifelse(is.na(data[[paste0(toupper(med), "_", day)]]), 0, 1)
              } else {
                data[paste0(toupper(med), "_", exposure[1], "_", exposure[2])] <- ifelse(is.na(data[[paste0(toupper(med), "_", day)]]), 0, 1) + data[[paste0(toupper(med), "_", exposure[1], "_", exposure[2])]]
              }
            }
            data[paste0(substr(med, 1, 3), "_", exposure[1], "_", exposure[2])] <- factor(ifelse(data[[paste0(toupper(med), "_", exposure[1], "_", exposure[2])]] > 0, "Yes", "No"), levels = c("Yes", "No"))
          } else {
            data[paste0(substr(med, 1, 3), "_", exposure[1], "_", exposure[2])] <- data[[substr(med, 1, 3)]]
          }
        } 
      } else {
        data[paste0(substr(med, 1, 3), "_", exposure[1], "_", exposure[2])] <- data[[substr(med, 1, 3)]]
      }
    }
  }
  if (!is.null(imp_or_all)){
    if (imp_or_all == 'imp'){
      data <- data[data$.imp == imp_data,]
    }
  }
  data_col2 <- list('age' = data$age, 'sex' = data$sexcd, 'severity' = data$asimpc01, 'nli_gr' = data$nligr1, 'level_gr' = data$lvlgr, 'level_fine' = data$splvl, 'level_broad' = data$lvl, 'patient' = data$ptid, 'bmi_status' = data$bmi_status, 'treatment_gr' = data$tx1_r, 'famotidine' = data$fam, 'ranitidine' = data$ran, 'cimetidine' = data$cim, 'nli' = data$nli1, 'nli_broad' = data$nli1_broad, 'none' = NULL, 'famotidine_main' = data[[paste0("fam_", exposure_main[1], "_", exposure_main[2])]], 'famotidine_color' = data[[paste0("fam_", exposure_color[1], "_", exposure_color[2])]], 'famotidine_panel' = data[[paste0("fam_", exposure_panel[1], "_", exposure_panel[2])]], 'ranitidine_main' = data[[paste0("ran_", exposure_main[1], "_", exposure_main[2])]], 'ranitidine_color' = data[[paste0("ran_", exposure_color[1], "_", exposure_color[2])]], 'ranitidine_panel' = data[[paste0("ran_", exposure_panel[1], "_", exposure_panel[2])]], 'cimetidine_main' = data[[paste0("cim_", exposure_main[1], "_", exposure_main[2])]], 'cimetidine_color' = data[[paste0("cim_", exposure_color[1], "_", exposure_color[2])]], 'cimetidine_panel' = data[[paste0("cim_", exposure_panel[1], "_", exposure_panel[2])]])
  data_col <- list('age' = 'age', 'sex' = 'sexcd', 'severity' = 'asimpc01', 'nli_gr' = 'nligr1', 'level_gr' = 'lvlgr', 'level_fine' = 'splvl', 'level_broad' = 'lvl', 'patient' = 'ptid', 'bmi_status' = 'bmi_status', 'treatment_gr' = 'tx1_r', 'famotidine' = 'fam', 'ranitidine' = 'ran', 'cimetidine' = 'cim', 'nli' = 'nli1', 'nli_broad' = 'nli1_broad', 'none' = NULL, 'famotidine_main' = paste0("fam_", exposure_main[1], "_", exposure_main[2]), 'famotidine_color' = paste0("fam_", exposure_color[1], "_", exposure_color[2]), 'famotidine_panel' = paste0("fam_", exposure_panel[1], "_", exposure_panel[2]), 'ranitidine_main' = paste0("ran_", exposure_main[1], "_", exposure_main[2]), 'ranitidine_color' = paste0("ran_", exposure_color[1], "_", exposure_color[2]), 'ranitidine_panel' = paste0("ran_", exposure_panel[1], "_", exposure_panel[2]), 'cimetidine_main' = paste0("cim_", exposure_main[1], "_", exposure_main[2]), 'cimetidine_color' = paste0("cim_", exposure_color[1], "_", exposure_color[2]), 'cimetidine_panel' = paste0("cim_", exposure_panel[1], "_", exposure_panel[2]))
  if (main == 'medication') {
    data_col$medication <- c(data_col$famotidine_main, data_col$ranitidine_main, data_col$cimetidine_main)
    data_col2$medication <- list(data_col2$famotidine_main, data_col2$ranitidine_main, data_col2$cimetidine_main)
  }
  if ('cluster' %in% colnames(data)){
    data_col2[['cluster']] <- data$cluster
    data_col[['cluster']] <- 'cluster'
  }
  label <- c('lems' = 'Lower extremity motor score (LEMS)',
             'uems' = 'Upper extremity motor score (UEMS)',
             'ltscore' = 'Light touch total score',
             'ppscore' = 'Pinprick total score',
             'modben' = 'Modified Benzel scale',
             'age' = 'Age [years]',
             'sex' = 'Sex',
             'severity' = 'AIS Grade',
             'nli' = 'Neurological Level of Injury at Baseline',
             'nli_broad' = 'Neurological Level of Injury at Baseline',
             'nli_gr' = 'Neurological Level of Injury at Baseline',
             'level_gr' = 'Spinal Level',
             'level_broad' = 'Spinal Level',
             'level_fine' = 'Spinal Level',
             'medication' = 'Medication',
             'famotidine' = 'Famotidine',
             'ranitidine' = 'Ranitidine',
             'cimetidine' = 'Cimetidine',
             'famotidine_main' = 'Famotidine',
             'ranitidine_main' = 'Ranitidine',
             'cimetidine_main' = 'Cimetidine',
             'famotidine_color' = 'Famotidine',
             'ranitidine_color' = 'Ranitidine',
             'cimetidine_color' = 'Cimetidine',
             'famotidine_panel' = 'Famotidine',
             'ranitidine_panel' = 'Ranitidine',
             'cimetidine_panel' = 'Cimetidine',
             'bmi_status' = 'Weight Status',
             'treatment_gr' = 'Treatment Group',
             'cluster' = 'Cluster',
             'none' = NA)
  if (main %in% c('famotidine', 'ranitidine', 'cimetidine')) {
    main <- paste0(main, "_main")
  } 
  if (color %in% c('famotidine', 'ranitidine', 'cimetidine')) {
    color <- paste0(color, "_main")
  }
  if (panel %in% c('famotidine', 'ranitidine', 'cimetidine')) {
    panel <- paste0(panel, "_main")
  }
  if (main == 'age'){
    if ((panel == 'none')&(color == 'none')){
      all <- rep("All", nrow(data))
    } else {
      all <- NULL
    }
    if (is.null(imp_or_all)){
      dem <- aggregate(data_col2[[main]], Filter(Negate(is.null), list(data_col2[[color]], data_col2[[panel]], all)), function(x)c(
        "m" = mean(x),
        "se" = se(x)
      ))
      dem$x <- paste0(round(dem$x[,"m"], 2), " (", round(dem$x[,"se"], 2), ")")
    } else {
      if (imp_or_all == 'imp'){
        dem <- aggregate(data_col2[[main]], Filter(Negate(is.null), list(data_col2[[color]], data_col2[[panel]], all)), function(x)c(
          "m" = mean(x),
          "se" = se(x)
        ))
        dem$x <- paste0(round(dem$x[,"m"], 2), " (", round(dem$x[,"se"], 2), ")")
      } else {
        dem1 <- aggregate(data_col2[[main]], Filter(Negate(is.null), list(data_col2[[color]], data_col2[[panel]], all, data$.imp)), function(x)c(
          "m" = mean(x),
          "se" = se(x)
        ))
        dem_mean <- aggregate(dem1$x[,"m"], lapply(1:(ncol(dem1)-2), function(col){dem1[[col]]}), mean)
        dem_vw <- aggregate(dem1$x[,"se"], lapply(1:(ncol(dem1)-2), function(col){dem1[[col]]}), function(x){mean(x^2)})
        dem_vb <- aggregate(dem1$x[,"m"], lapply(1:(ncol(dem1)-2), function(col){dem1[[col]]}), var)
        dem <- dem_mean
        dem$x <- paste0(round(dem$x,2), " (", round(dem_vw$x + dem_vb$x + dem_vb$x/20,2), ")")
      }
    }
    if ((panel == 'none')&(color == 'none')){
      colnames(dem) <- c("", "Age mean (standard error) [years]")
    } else {
      colnames(dem) <- c(label[c(color, panel)[c(color, panel)!="none"]], "Age mean (standard error) [years]")
    }
  } else {
    if (main == 'medication'){
      d <- data_col2[[main]][[1]]
      col_list <- c(data_col2[[main]], list(data_col2[[color]], data_col2[[panel]]))
    } else {
      d <- data_col2[[main]]
      col_list <- list(data_col2[[main]], data_col2[[color]], data_col2[[panel]])
    }
    dem <- aggregate(d, Filter(Negate(is.null), col_list), function(x)c(
      "n" = length(x),
      "p" = (length(x)/length(d))*100
    ))
    if (!is.null(imp_or_all)){
      if (imp_or_all == 'all'){
        dem$x[,"n"] <- dem$x[,"n"]/20
      }
    }
    
    dem$x <- paste0(round(dem$x[,"n"],2), " (", round(dem$x[,"p"],2), ")")
    
    if (main == 'medication') {
      m <- c("famotidine", "ranitidine", "cimetidine")
    } else {
      m <- main
    }
    
    colnames(dem) <- c(label[c(m, color, panel)[c(m, color, panel)!="none"]], "N (%)")
  }
  return(dem)
  #if ((color == 'none')&(panel == 'none')){
  #  dem <- data %>% summary_factorlist(explanatory = data_col[[main]], na_include = TRUE)
  #  colnames(dem)[[1]] <- "Variable"
  #  colnames(dem)[[2]] <- "Levels"
  #  colnames(dem)[[3]] <- "All"
  #  if (main != 'medication'){
  #    dem$Variable <- replace(dem$Variable, dem$Variable == data_col[[main]], label[[main]])
  #  } else {
  #    dem$Variable <- c("Famotidine", "", "Ranitidine", "", "Cimetidine", "")
  #  }
  #  return(dem)
  #} else if ((color != 'none')&((panel == 'none')|(panel == color))){
  #  dem <- data %>% summary_factorlist(explanatory = data_col[[main]], dependent = data_col[[color]], na_include = TRUE)
  #  colnames(dem)[[1]] <- "Variable"
  #  colnames(dem)[[2]] <- "Levels"
  #  if (main != 'medication'){
  #    dem$Variable <- replace(dem$Variable, dem$Variable == data_col[[main]], label[[main]])
  #  } else {
  #    dem$Variable <- c("Famotidine", "", "Ranitidine", "", "Cimetidine", "")
  #  }
  #  return(dem)
  #} else if ((color == 'none')&(panel != 'none')){
  #  dem <- data %>% summary_factorlist(explanatory = data_col[[main]], dependent = data_col[[panel]], na_include = TRUE)
  #  colnames(dem)[[1]] <- "Variable"
  #  colnames(dem)[[2]] <- "Levels"
  #  if (main != 'medication'){
  #    dem$Variable <- replace(dem$Variable, dem$Variable == data_col[[main]], label[[main]])
  #  } else {
  #    dem$Variable <- c("Famotidine", "", "Ranitidine", "", "Cimetidine", "")
  #  }
  #  #return(dem)
  #} else {
  #  dem_list <- list()
  #  n <- list()
  #  i <- 1
  #  for (x in 1:length(levels(data_col2[[panel]]))) {
  #    if (levels(data_col2[[panel]])[[x]] %in% data_col2[[panel]]){
  #      df <- data %>% filter(data_col2[[panel]] == levels(data_col2[[panel]])[[x]])
  #      dem <- df %>% summary_factorlist(explanatory = data_col[[main]], dependent = data_col[[color]], na_include = TRUE)
  #      colnames(dem)[[1]] <- "Variable"
  #      colnames(dem)[[2]] <- "Levels"
  #      if (main != 'medication'){
  #        dem$Variable <- replace(dem$Variable, dem$Variable == data_col[[main]], label[[main]])
  #      } else {
  #        dem$Variable <- c("Famotidine", "", "Ranitidine", "", "Cimetidine", "")
  #      }
  #      dem_list[[i]] <- as.data.frame(dem) #dem_list <- append(dem_list, dem)
  #      n[[i]] <- levels(data_col2[[panel]])[[x]]
  #      i <- i + 1
  #    }
  #  }
  #  names(dem_list) <- n #names(dem_list) <- levels(data_col2[[panel]])
  #  return(dem_list) # see example_caption_table2.R
  #}
}


plotMissingData <- function(data = sygen_data, outcome, out_type, dem, plot_type, exposure_fam = NULL, exposure_ran = NULL, exposure_cim = NULL){
  if (!('famotidine' %in% dem)) {
    exposure_fam <- c(0, 365)
  }
  if (!('ranitidine' %in% dem)) {
    exposure_ran <- c(0, 365)
  }
  if (!('cimetidine' %in% dem)) {
    exposure_cim <- c(0, 365)
  }
  cols <- list('lems' = c('lower01', 'lower04', 'lower08', 'lower16', 'lower26', 'lower52'),
            'uems' = c('upper01', 'upper04', 'upper08', 'upper16', 'upper26', 'upper52'),
            'ltscore' = c('ltscor01', 'ltscor04', 'ltscor08', 'ltscor16', 'ltscor26', 'ltscor52'),
            'ppscore' = c('ppscor01', 'ppscor04', 'ppscor08', 'ppscor16', 'ppscor26', 'ppscor52'),
            'modben' = c('modben01', 'modben04', 'modben08', 'modben16', 'modben26', 'modben52'))
  data_col <- list('age' = 'age', 'sex' = 'sexcd', 'severity' = 'asimpc01', 'nli_gr' = 'nligr1', 'level_gr' = 'lvlgr', 'level_fine' = 'splvl', 'level_broad' = 'lvl', 'patient' = 'ptid', 'bmi_status' = 'bmi_status', 'treatment_gr' = 'tx1_r', 'famotidine' = 'fam', 'ranitidine' = 'ran', 'cimetidine' = 'cim', 'nli' = 'nli1', 'nli_broad' = 'nli1_broad', 'none' = NULL)
  label <- c('lems' = 'LEMS',
             'uems' = 'UEMS',
             'ltscore' = 'Light touch total score',
             'ppscore' = 'Pinprick total score', 
             'modben' = 'Modified Benzel scale',
             'age' = 'Age [years]',
             'sex' = 'Sex',
             'severity' = 'AIS Grade',
             'nli' = 'Neurological Level of Injury at Baseline',
             'nli_broad' = 'Neurological Level of Injury at Baseline',
             'nli_gr' = 'Neurological Level of Injury at Baseline',
             'level_gr' = 'Spinal Level',
             'level_broad' = 'Spinal Level',
             'level_fine' = 'Spinal Level',
             'medication' = 'Medication',
             'famotidine' = 'Famotidine',
             'ranitidine' = 'Ranitidine',
             'cimetidine' = 'Cimetidine',
             'famotidine_main' = 'Famotidine',
             'ranitidine_main' = 'Ranitidine',
             'cimetidine_main' = 'Cimetidine',
             'famotidine_color' = 'Famotidine',
             'ranitidine_color' = 'Ranitidine',
             'cimetidine_color' = 'Cimetidine',
             'famotidine_panel' = 'Famotidine',
             'ranitidine_panel' = 'Ranitidine',
             'cimetidine_panel' = 'Cimetidine',
             'bmi_status' = 'Weight Status',
             'treatment_gr' = 'Treatment Group')
  for (n in names(cols)){
    new_lab <- sapply(cols[[n]], FUN = function(x){paste0(label[n], " - week ", as.character(as.numeric(substr(x, nchar(x) - 1, nchar(x)))))})
    names(new_lab) <- cols[[n]]
    label <- c(label, new_lab)
  }
  label_NA <- sapply(label, function(x){paste0(x, " (missing)")})
  names(label_NA) <- sapply(names(label), FUN = function(x){paste0(x, "_NA")})
  data$fam <- ifelse(data$Famotidine == 1, "Yes", "No")
  data$ran <- ifelse(data$Ranitidine == 1, "Yes", "No")
  data$cim <- ifelse(data$Cimetidine == 1, "Yes", "No")
  ptid <- c(data$ptid, data$ptid, data$ptid, data$ptid, data$ptid, data$ptid)
  age <- c(data$age, data$age, data$age, data$age, data$age, data$age)
  age_range <- c(data$age_range, data$age_range, data$age_range, data$age_range, data$age_range, data$age_range)
  sexcd <- c(data$sexcd, data$sexcd, data$sexcd, data$sexcd, data$sexcd, data$sexcd)
  week <- c(rep(1, nrow(data)), rep(4, nrow(data)), rep(8, nrow(data)), rep(16, nrow(data)), rep(26, nrow(data)), rep(52, nrow(data)))
  uems <- c(data$upper01, data$upper04, data$upper08, data$upper16, data$upper26, data$upper52)
  lems <- c(data$lower01, data$lower04, data$lower08, data$lower16, data$lower26, data$lower52)
  ltscore <- c(data$ltscor01, data$ltscor04, data$ltscor08, data$ltscor16, data$ltscor26, data$ltscor52)
  ppscore <- c(data$ppscor01, data$ppscor04, data$ppscor08, data$ppscor16, data$ppscor26, data$ppscor52)
  modben <- c(rep(1, nrow(data)), data$modben04, data$modben08, data$modben16, data$modben26, data$modben52)
  asimpc01 <- c(data$asimpc01, data$asimpc01, data$asimpc01, data$asimpc01, data$asimpc01, data$asimpc01)
  nli1 <- c(data$nli1, data$nli1, data$nli1, data$nli1, data$nli1, data$nli1)
  nli1_broad <- c(data$nli1_broad, data$nli1_broad, data$nli1_broad, data$nli1_broad, data$nli1_broad, data$nli1_broad)
  splvl <- c(data$splvl, data$splvl, data$splvl, data$splvl, data$splvl, data$splvl)
  lvl <- c(data$lvl, data$lvl, data$lvl, data$lvl, data$lvl, data$lvl)
  lvlgr <- c(data$lvlgr, data$lvlgr, data$lvlgr, data$lvlgr, data$lvlgr, data$lvlgr)
  nligr1 <- c(data$nligr1, data$nligr1, data$nligr1, data$nligr1, data$nligr1, data$nligr1)
  fam <- c(data$fam, data$fam, data$fam, data$fam, data$fam, data$fam)
  ran <- c(data$ran, data$ran, data$ran, data$ran, data$ran, data$ran)
  cim <- c(data$cim, data$cim, data$cim, data$cim, data$cim, data$cim)
  tx1_r <- c(data$tx1_r, data$tx1_r, data$tx1_r, data$tx1_r, data$tx1_r, data$tx1_r)
  bmi_status <- c(data$bmi_status, data$bmi_status, data$bmi_status, data$bmi_status, data$bmi_status, data$bmi_status)
  reshaped_data <- data.frame(ptid, age, age_range, sexcd, week, uems, lems, ltscore, ppscore, modben, asimpc01, splvl, lvl, nli1, nli1_broad, nligr1, lvlgr, fam, ran, cim, tx1_r, bmi_status)
  for (component in c('famotidine', 'ranitidine', 'cimetidine')) {
    if (component == 'famotidine') {
      exposure <- exposure_fam
    } else if (component == 'ranitidine') {
      exposure <- exposure_ran
    } else if (component == 'cimetidine') {
      exposure <- exposure_cim
    }
    for (med in c('famotidine', 'ranitidine', 'cimetidine')) {
      if (sum(exposure == c(0, 365)) != 2) {
        for (day in exposure[1]:exposure[2]) {
          data[paste0(toupper(med), "_", day)] <- as.numeric(data[[paste0(toupper(med), "_", day)]])
          if (day == 0) {
            data[paste0(toupper(med), "_", exposure[1], "_", exposure[2])] <- ifelse(is.na(data[[paste0(toupper(med), "_", day)]]), 0, 1)
          } else {
            data[paste0(toupper(med), "_", exposure[1], "_", exposure[2])] <- ifelse(is.na(data[[paste0(toupper(med), "_", day)]]), 0, 1) + data[[paste0(toupper(med), "_", exposure[1], "_", exposure[2])]]
          }
        }
        data[substr(med, 1, 3)] <- ifelse(data[[paste0(toupper(med), "_", exposure[1], "_", exposure[2])]] > 0, "Yes", "No")
      } 
      reshaped_data[substr(med, 1, 3)] <- c(data[[substr(med, 1, 3)]], data[[substr(med, 1, 3)]], data[[substr(med, 1, 3)]], data[[substr(med, 1, 3)]], data[[substr(med, 1, 3)]], data[[substr(med, 1, 3)]])
    }
  }
  if (is.null(outcome)) {
    out_type <- NULL
    out_cols <- NULL
  } else {
    if (out_type == 'overall') {
      data <- reshaped_data
      out_cols <- outcome
    } else if (out_type == 'weekly') {
      out_cols <- cols[[outcome]]
      names(out_cols) <- NULL
    }
  }
  if (!is.null(dem)){
    dem_cols <- data_col[dem]
  } else {
    dem_cols <- NULL
  }
  cols_tot <- c(out_cols, dem_cols)
  cols_tot_lab <- c(out_cols, dem)
  cols_tot_NA <- unlist(lapply(cols_tot, FUN = function(x){paste0(x, "_NA")}))
  cols_tot_lab_NA <- unlist(lapply(cols_tot_lab, FUN = function(x){paste0(x, "_NA")}))
  data_NA <- data %>% bind_shadow()
  lab_tot <- c(label, label_NA)
  if (length(cols_tot) <= 1) {
    p <- NULL
  } else if (plot_type == 'upset'){
    data_plot <- data_NA[, cols_tot_NA]
    data_plot <- ifelse(data_plot == "!NA", 0, 1)
    cn <- label_NA[cols_tot_lab_NA]
    names(cn) <- NULL
    colnames(data_plot) <- cn
    data_plot <- as.data.frame(data_plot)
    p <- UpSetR::upset(data_plot, sets = colnames(data_plot), mb.ratio = c(0.55, 0.45), order.by = "freq", keep.order = TRUE)
  } else if (plot_type == 'counts') {
    data_plot <- data_NA
    for (col in cols_tot_NA){
      data_plot[col] <- factor(data_plot[[col]], levels = c("NA", "!NA"))
    }
    color_list <- c("NA" = "darkgrey", "!NA" = "#a3f081")
    plot_list <- sapply(cols_tot, FUN = function(x){
      lapply(cols_tot_NA, FUN = function(y) {
        if (class(data_plot[[x]]) %in% c("integer", "numeric")) {
          # boxplot
          ggplot(data_plot, aes(x = data_plot[[y]], y = data_plot[[x]], fill = data_plot[[y]])) +  geom_boxplot() + scale_x_discrete(breaks = factor(c("NA", "!NA")), drop = FALSE) + coord_flip() + theme(legend.position = "none", axis.title.x = element_blank(), axis.title.y = element_blank()) + scale_fill_manual(values = color_list)
        } else {
          # barplot
          ggplot(data_plot, aes(x = data_plot[[x]], fill = data_plot[[y]])) +  geom_bar(aes(x = data_plot[[x]], fill = data_plot[[y]])) + theme(legend.position = "none", axis.title.x = element_blank(), axis.title.y = element_blank()) + scale_fill_manual(values = color_list)
        }
      })
    })
    remap <- c()
    for (m in 1:length(cols_tot)){
      remap <- c(remap, c(1:length(cols_tot_NA))*length(cols_tot) - (length(cols_tot) - m))
    }
    plot_list <- plot_list[remap]
    grob_list <- as.list(rep(NA, (length(cols_tot)+1)*(length(cols_tot_NA)+1)))
    grob_list[[length(cols_tot)+1]] <- textGrob(" ", rot = 0, hjust = 0.5)
    grob_list[1:length(cols_tot)] <- lapply(cols_tot_lab, function(x){grobTree(rectGrob(gp = gpar(fill="grey", col = NA)), textGrob(lab_tot[x], rot = 0, hjust = 0.5, vjust = 0.5))})
    grob_list[c(2:(length(cols_tot_NA)+1))*(length(cols_tot)+1)] <- lapply(cols_tot_lab_NA, function(x){grobTree(rectGrob(gp = gpar(fill="grey", col = NA)), textGrob(lab_tot[x], rot = -90, hjust = 0.5, vjust = 0.5))})
    grob_list[which(is.na(grob_list))] <- plot_list
    p <- grid.arrange(grobs = grob_list, ncol = length(cols_tot)+1, nrow = length(cols_tot_NA)+1, widths = as.list(c(rep(6, length(cols_tot)), 0.8)), heights = as.list(c(0.8, rep(6, length(cols_tot_NA)))))
  } else if (plot_type == 'prop') {
    data_plot <- data_NA
    for (col in cols_tot_NA){
      data_plot[col] <- factor(data_plot[[col]], levels = c("NA", "!NA"))
    }
    color_list <- c("NA" = "darkgrey", "!NA" = "#a3f081")
    plot_list <- sapply(cols_tot, FUN = function(x){
      lapply(cols_tot_NA, FUN = function(y) {
        if (class(data_plot[[x]]) %in% c("integer", "numeric")) {
          # boxplot
          ggplot(data_plot, aes(x = data_plot[[y]], y = data_plot[[x]], fill = data_plot[[y]])) +  geom_boxplot() + scale_x_discrete(breaks = factor(c("NA", "!NA")), drop = FALSE) + coord_flip() + theme(legend.position = "none", axis.title.x = element_blank(), axis.title.y = element_blank()) + scale_fill_manual(values = color_list)
        } else {
          # barplot
          ggplot(data_plot, aes(x = data_plot[[x]], fill = data_plot[[y]])) +  geom_bar(aes(x = data_plot[[x]], fill = data_plot[[y]]), position = "fill") + theme(legend.position = "none", axis.title.x = element_blank(), axis.title.y = element_blank()) + scale_fill_manual(values = color_list)
        }
      })
    })
    remap <- c()
    for (m in 1:length(cols_tot)){
      remap <- c(remap, c(1:length(cols_tot_NA))*length(cols_tot) - (length(cols_tot) - m))
    }
    plot_list <- plot_list[remap]
    grob_list <- as.list(rep(NA, (length(cols_tot)+1)*(length(cols_tot_NA)+1)))
    grob_list[[length(cols_tot)+1]] <- textGrob(" ", rot = 0, hjust = 0.5)
    grob_list[1:length(cols_tot)] <- lapply(cols_tot_lab, function(x){grobTree(rectGrob(gp = gpar(fill="grey", col = NA)), textGrob(lab_tot[x], rot = 0, hjust = 0.5, vjust = 0.5))})
    grob_list[c(2:(length(cols_tot_NA)+1))*(length(cols_tot)+1)] <- lapply(cols_tot_lab_NA, function(x){grobTree(rectGrob(gp = gpar(fill="grey", col = NA)), textGrob(lab_tot[x], rot = -90, hjust = 0.5, vjust = 0.5))})
    grob_list[which(is.na(grob_list))] <- plot_list
    p <- grid.arrange(grobs = grob_list, ncol = length(cols_tot)+1, nrow = length(cols_tot_NA)+1, widths = as.list(c(rep(6, length(cols_tot)), 0.8)), heights = as.list(c(0.8, rep(6, length(cols_tot_NA)))))
  } else if (plot_type == 'pattern') {
    data_plot <- data[, unlist(cols_tot)]
    cn <- label[cols_tot_lab]
    names(cn) <- NULL
    colnames(data_plot) <- cn
    data_plot <- as.data.frame(data_plot)
    p <- missing_pattern(data_plot, explanatory = cn)
  } else {
    p <- NULL
  }
  return(p)
}


visualizeClusters <- function(outcome_variable, nb_clusters, method, show, imp_or_all = 'all', imp_data = NULL, labb = FALSE) {
  G <- nb_clusters
  colors <- c("1" = "#DDCC77", "2" = "#117733", "3" = "#44AA99", "4" = "#999933", "5" = "#6699CC", "6" = "#888888")
  colors_mean <- c("1" = "#b0a35f", "2" = "#0b5323", "3" = "#2f766b", "4" = "#6b6b23", "5" = "#476b8e", "6" = "#5f5f5f")
  label <- c('lems' = "Lower extremity motor score (LEMS)",
             'uems' = "Upper extremity motor score (UEMS)",
             'ltscore' = "Light touch total score",
             'ppscore' = "Pinprick total score",
             'modben' = "Modified Benzel scale")
  cols <- list('lems' = c('lower01', 'lower04', 'lower08', 'lower16', 'lower26', 'lower52'), 'uems' = c('upper01', 'upper04', 'upper08', 'upper16', 'upper26', 'upper52'), 'ltscore' = c('ltscor01', 'ltscor04', 'ltscor08', 'ltscor16', 'ltscor26', 'ltscor52'), 'ppscore' = c('ppscor01', 'ppscor04', 'ppscor08', 'ppscor16', 'ppscor26', 'ppscor52'), 'modben' = c('modben04', 'modben08', 'modben16', 'modben26', 'modben52'))[[outcome_variable]]
  
  if (imp_or_all == 'all'){
    data <- read_excel(file.path("results - 2", paste0("data_NA_", outcome_variable, ".xlsx")))
    agg <- read_excel(file.path("results - 2", paste0("agg_", outcome_variable, "_2.xlsx")))
    
    if (nb_clusters == 1) {
      dat_list <- lapply(cols, function(c){
        d <- data[, c('ptid', c)]
        colnames(d) <- c('ptid', 'outcome')
        d$week <- as.numeric(substr(c, nchar(c)-1, nchar(c)))
        d
      })
      dat <- bind_rows(dat_list)
      
      p <- ggplot()
      if ('single_t' %in% show){
        p <- p + geom_line(mapping = aes(x = week, y = outcome, group = ptid), data = dat, color = "#4c4ca6") 
      }
      if ('mean_t' %in% show){
        p <- p + geom_line(mapping = aes(x = week, y = estimate), data = agg[agg$G == 1,], size = 1.5, color = 'navy')
      }
      if ('se_t' %in% show){
        p <- p + geom_ribbon(mapping = aes(x = week, y = estimate, ymin = (estimate - std.error), ymax = (estimate + std.error)), data = agg[agg$G == 1,], alpha = 0.3, fill = 'navy')
      }
      if (length(show) == 0) {
        p <- NULL
      } else {
        p <- p + theme(legend.position = "none") + labs(y = label[outcome_variable], x = "Time after injury (weeks)") 
      }
    } else { # more than one group
      #data$cluster <- unlist(data[paste0(method, "-clusters-", G, "-estimated")])
      #data$maxp <- apply(data[, paste0(method, "-clusters-", G, "-alloc-estimated-probs-", 1:G)], 1, max, na.rm = TRUE)
      
      #dat_list <- lapply(cols, function(c){
      #  d <- data[, c('ptid', c, 'cluster', 'maxp')]
      #  colnames(d) <- c('ptid', 'outcome', 'cluster', 'maxp')
      #  d$week <- as.numeric(substr(c, nchar(c)-1, nchar(c)))
      #  d
      #})
      #dat <- bind_rows(dat_list)
      
      p <- ggplot()
      if ('mean_t' %in% show){
        for (a in 1:G) {
          p <- p + geom_line(mapping = aes(x = week, y = estimate), data = agg[(agg$G == G)&(agg$method == method)&(agg$alloc == a),], size = 1.5, color = colors_mean[[as.character(a)]]) 
        }
      }
      if ('se_t' %in% show){
        for (a in 1:G) {
          p <- p + geom_ribbon(mapping = aes(x = week, y = estimate, ymin = pmax((estimate - std.error),0), ymax = (estimate + std.error)), data = agg[(agg$G == G)&(agg$method == method)&(agg$alloc == a),], alpha = 0.3, fill = colors_mean[[as.character(a)]])
        }
      }
      if ('single_t' %in% show){
        for (g in 1:G) {
          #p <- p + geom_line(mapping = aes(x = week, y = outcome, group = ptid), data = dat[dat$cluster == as.character(g),], color = colors[[as.character(g)]])  
          p <- p + geom_label(aes(x = 26, y = list("lems" = 25, "uems" = 25, "ppscore" = 56, "ltscore" = 56, "modben" = 3.5)[[outcome_variable]], label = "Unique individual trajectories are not available \n because of multiple imputation"), label.size = NA, alpha = 0.7, color = "darkred", size = 6)
        }
      }
      if (length(show) == 0) {
        p <- NULL
      } else {
        p <- p + theme(legend.position = "none") + labs(y = label[outcome_variable], x = "Time after injury (weeks)") 
        
        if (labb == TRUE) {
          for (g in 1:G) {
            p <- p + annotate("text", x = 54, y = agg[(agg$week == 52)&(agg$method == method)&(agg$G == G)&(agg$alloc == g),]$estimate, label = as.character(g), fontface = "bold", size = 8)
          }
        }
        
      }
    }
    
  } else { # divide by imputation dataset
    data <- read_excel(file.path("results - 2", paste0("data_imputed_", outcome_variable, "_renamed2.xlsx")))
    if ((imp_or_all == 'imp')&(!is.null(imp_data))){
      data <- data[data$.imp == imp_data,]
    }
    
    if (nb_clusters == 1) {
      dat_list <- lapply(cols, function(c){
        d <- data[, c('.imp', 'ptid', c)]
        colnames(d) <- c('.imp', 'ptid', 'outcome')
        d$week <- as.numeric(substr(c, nchar(c)-1, nchar(c)))
        d
      })
      dat <- bind_rows(dat_list) 
      
      p <- ggplot()
      if ('single_t' %in% show){
        p <- p + geom_line(mapping = aes(x = week, y = outcome, group = ptid), data = dat, color = "#4c4ca6", linewidth = 0.1) 
      }
      if ('mean_t' %in% show){
        agg <- aggregate(dat$outcome, list(dat$week, dat$.imp), mean)
        names(agg) <- c('week', '.imp', 'outcome_mean')
        p <- p + geom_line(mapping = aes(x = week, y = outcome_mean), data = agg, size = 0.3, color = 'navy')
      }
      if ('se_t' %in% show){
        agg <- aggregate(dat$outcome, list(dat$week, dat$.imp), function(x)c('m' = mean(x), 's' = se(x)))
        agg$outcome_mean <- agg$x[,'m']
        agg$outcome_se <- agg$x[,'s']
        agg <- dplyr::select(agg, -c('x'))
        names(agg) <- c('week', '.imp', 'outcome_mean', 'outcome_se')
        p <- p + geom_ribbon(mapping = aes(x = week, y = outcome_mean, ymin = (outcome_mean - outcome_se), ymax = (outcome_mean + outcome_se)), data = agg, alpha = 0.3, fill = 'navy')
      }
      if (length(show) == 0) {
        p <- NULL
      } else {
        p <- p + theme(legend.position = "none") + labs(y = label[outcome_variable], x = "Time after injury (weeks)") 
      }
    } else { # more than one group
      data$cluster <- data[[paste0(method, "-clusters-", G)]]
      
      dat_list <- lapply(cols, function(c){
        d <- data[, c('.imp', 'ptid', c, 'cluster')]
        colnames(d) <- c('.imp', 'ptid', 'outcome', 'cluster')
        d$week <- as.numeric(substr(c, nchar(c)-1, nchar(c)))
        d
      })
      dat <- bind_rows(dat_list)
      
      p <- ggplot()
      if ('single_t' %in% show){
        for (g in 1:G) {
          p <- p + geom_line(mapping = aes(x = week, y = outcome, group = ptid), data = dat[dat$cluster == as.character(g),], color = colors[[as.character(g)]], linewidth = 0.1)  
        }
      }
      if ('mean_t' %in% show){
        agg <- aggregate(dat$outcome, list(dat$week, dat$cluster, dat$.imp), mean)
        names(agg) <- c('week', 'cluster', '.imp', 'outcome_mean')
        for (a in 1:G) {
          p <- p + geom_line(mapping = aes(x = week, y = outcome_mean), data = agg[agg$cluster == a,], size = 1.5, color = colors_mean[[as.character(a)]], linewidth = 0.3) 
        }
      }
      if ('se_t' %in% show){
        agg <- aggregate(dat$outcome, list(dat$week, dat$cluster, dat$.imp), function(x)c('m' = mean(x), 's' = se(x)))
        agg$outcome_mean <- agg$x[,'m']
        agg$outcome_se <- agg$x[,'s']
        agg <- dplyr::select(agg, -c('x'))
        names(agg) <- c('week', 'cluster', '.imp', 'outcome_mean', 'outcome_se')
        for (a in 1:G) {
          p <- p + geom_ribbon(mapping = aes(x = week, y = outcome_mean, ymin = (outcome_mean - outcome_se), ymax = (outcome_mean + outcome_se)), data = agg[agg$cluster == a,], alpha = 0.3, fill = colors_mean[[as.character(a)]])
        }
      }
      if (length(show) == 0) {
        p <- NULL
      } else {
        p <- p + theme(legend.position = "none") + labs(y = label[outcome_variable], x = "Time after injury (weeks)") 
        
        if (labb == TRUE) {
          for (g in 1:G) {
            df <- data.frame('.imp' = 1:20, 'lab' = rep(as.character(g)), 20)
            if (nrow(dat[dat$cluster == g,] != 0)){
              df <- df[df$.imp %in% aggregate(dat[(dat$week == 52)&(dat$cluster == g),]$outcome, list(dat[(dat$week == 52)&(dat$cluster == g),]$.imp), mean)[, 1], ]#removed unlist from aggregate(...)[,1]
              p <- p + geom_text(data = df, mapping = aes(label = lab), x = 54, y = aggregate(dat[(dat$week == 52)&(dat$cluster == g),]$outcome, list(dat[(dat$week == 52)&(dat$cluster == g),]$.imp), mean)$x, fontface = "bold", size = 3)
            }
          }
        }
      }
    }
  }
  
  if (!is.null(p)){
    if (imp_or_all == 'imp'){
      p <- p + facet_wrap(~.imp, ncol = 5)
    }
    if (outcome_variable %in% c('uems', 'lems')){
      p <- p + ylim(0, 50)
    } else if (outcome_variable %in% c('ltscore', 'ppscore')){
      p <- p + ylim(0, 112)
    } else {
      p <- p + ylim(0, 7)
    }
  }
  return(p)
}

clusterDistribution <- function(outcome_variable, nb_clusters, method, imp_or_all = 'all') {
  G <- nb_clusters
  data <- read_excel(file.path("results - 2", paste0("data_imputed_", outcome_variable, "_renamed2.xlsx")))
  if (nb_clusters == 1) {
    df <- data.frame("Cluster" = c("1"), "Number" = c(as.integer(nrow(data)/20)), "Proportion" = c("100%"))
  } else {
    data$cluster <- data[[paste0(method, '-clusters-', G)]]
    agg <- aggregate(data$cluster, list(data$cluster, data$.imp), length)
    names(agg) <- c('cluster', '.imp', 'counts')
    agg2 <- aggregate(data$cluster, list(data$.imp), length)
    names(agg2) <- c('.imp', 'tot')
    agg$percentage <- (agg$counts/unique(agg2$tot))*100
    pre_df_list <- lapply(c('counts', 'percentage'), function(what){
      pre_df <- aggregate(agg[[what]], list(agg$cluster), function(x)c(
        'mean' = mean(x),
        'std' = sd(x)
      ))
      pre_df[paste0(what, "_mean")] <- pre_df$x[,'mean']
      pre_df[paste0(what, "_std")] <- pre_df$x[,'std']
      pre_df <- dplyr::select(pre_df, -c("x"))
      names(pre_df) <- c('cluster', paste0(what, "_mean"), paste0(what, "_std"))
      pre_df
    })
    df_pre <- pre_df_list %>% reduce(inner_join, by = "cluster")
    df <- data.frame("Cluster" = as.character(df_pre$cluster), "Mean number ± std" = paste0(as.integer(df_pre$counts_mean), " ± ", round(df_pre$counts_std)), "Mean proportion ± std" = paste0("(", round(df_pre$percentage_mean, 2), " ± ", round(df_pre$percentage_std, 2), ") %"))
    names(df) <- c("Cluster", "Mean number ± std", "Mean proportion ± std")
  }
  #if (imp_or_all == 'all') {
  #  data <- read_excel(file.path("results - 2", paste0("data_imputed_est_cl_", outcome_variable, "_2.xlsx")))
  #  if (nb_clusters == 1) {
  #    df <- data.frame("Cluster" = c("1"), "Number" = c(nrow(data)), "Proportion" = c("100%"))
  #  } else {
  #    data$cluster <- unlist(data[paste0(method, '-clusters-', G, '-estimated')])
  #    df <- aggregate(data$cluster, list(data$cluster), length)
  #    names(df) <- c('Cluster', 'Number')
  #    df$Proportion <- paste0(round((df$Number/nrow(data))*100, 2), "%")
  #    df$Number <- as.integer(df$Number)
  #    df$Cluster <- as.character(df$Cluster)
  #  }
  #} else {
  #  data <- read_excel(file.path("results - 2", paste0("data_imputed_", outcome_variable, "_renamed2.xlsx")))
  #  if (nb_clusters == 1) {
  #    df <- data.frame("Cluster" = c("1"), "Number" = c(nrow(data)), "Proportion" = c("100%"))
  #  } else {
  #    data$cluster <- unlist(data[paste0(method, '-clusters-', G)])
  #    agg <- aggregate(data$cluster, list(data$cluster, data$.imp), length)
  #    names(agg) <- c('cluster', '.imp', 'counts')
  #    agg2 <- aggregate(data$cluster, list(data$.imp), length)
  #    names(agg2) <- c('.imp', 'tot')
  #    agg$percentage <- (agg$counts/unique(agg2$tot))*100
  #    pre_df_list <- lapply(c('counts', 'percentage'), function(what){
  #      pre_df <- aggregate(unlist(agg[what]), list(agg$cluster), function(x)c(
  #        'mean' = mean(x),
  #        'std' = sd(x)
  #      ))
  #      pre_df[paste0(what, "_mean")] <- pre_df$x[,'mean']
  #      pre_df[paste0(what, "_std")] <- pre_df$x[,'std']
  #      pre_df <- dplyr::select(pre_df, -c("x"))
  #      names(pre_df) <- c('cluster', paste0(what, "_mean"), paste0(what, "_std"))
  #      pre_df
  #    })
  #    df_pre <- pre_df_list %>% reduce(inner_join, by = "cluster")
  #    df <- data.frame("Cluster" = as.character(df_pre$cluster), "Mean number ± std" = paste0(as.integer(df_pre$counts_mean), " ± ", round(df_pre$counts_std)), "Mean proportion ± std" = paste0("(", round(df_pre$percentage_mean, 2), " ± ", round(df_pre$percentage_std, 2), ") %"))
  #    names(df) <- c("Cluster", "Mean number ± std", "Mean proportion ± std")
  #  }
  #}
  return(df)
}

aboutClusters <- function(outcome_variable, nb_clusters, method){
  #data_all <- read_excel(file.path("results - 2", paste0("data_imputed_est_cl_", outcome_variable, "_2.xlsx")))
  data_imp <- read_excel(file.path("results - 2", paste0("data_imputed_", outcome_variable, "_renamed2.xlsx")))
  if (nb_clusters == 1) {
    empty_overall <- 'NO'
    empty_imp <- 'NO'
    consistency <- 'YES'
  } else {
    
    otherq <- sapply(1:20, function(i){
      sub <- data_imp[data_imp$.imp == i,]
      length(unique(sub[[paste0(method, '-clusters-', nb_clusters)]]))
    })
    if (any(otherq != nb_clusters)){
      empty_imp <- 'YES'
    } else {
      empty_imp <- 'NO'
    }
    if (length(unique(otherq)) > 1) {
      consistency <- 'NO'
    } else {
      consistency <- 'YES'
    }
  }
  return(list('empty_imp' = empty_imp, 'consistency' = consistency))
}

plotEvaluation <- function(outcome_variable, nb_clusters, method, criterion, imp_or_all = 'all') {
  # https://rdrr.io/cran/longitudinalData/man/qualityCriterion.html
  
  p <- NULL
  
  quality_df_list <- lapply(c('kmeans', 'lpa'), function(m){
    quality_df <- read_excel(file.path("results - 2", paste0(m, "_quality_criteria_imputed_", outcome_variable, "_renamed2.xlsx")))
    quality_df$meth <- m
    quality_df
  })
  quality <- bind_rows(quality_df_list)
  
  reshape_list <- lapply(c('Calinski.Harabatz',	'Calinski.Harabatz2',	'Calinski.Harabatz3',	'BIC',	'BIC2',	'AIC',	'AICc',	'AICc2',	'postProbaGlobal', 'Ray.Turi',	'Davies.Bouldin'), function(crit){
    sub_df <- quality[, c('.imp', 'meth', 'G', crit)]
    names(sub_df) <- c('.imp', 'meth', 'G', 'value')
    sub_df$criterion <- crit
    find_best_list <- lapply(c('kmeans', 'lpa'), function(m){
      imp_best <- lapply(1:20, function(i){
        sdf <- sub_df[(sub_df$meth == m)&(sub_df$.imp == i),]
        sdf$best <- ifelse(sdf$value == max(sdf$value), TRUE, FALSE)
        sdf
      })
      bind_rows(imp_best)
    })
    bind_rows(find_best_list)
  })
  quality_reshaped <- bind_rows(reshape_list)
  
  agg <- aggregate(quality_reshaped$value, list(quality_reshaped$meth, quality_reshaped$G, quality_reshaped$criterion), mean)
  names(agg) <- c('meth', 'G', 'criterion', 'mean_criterion')
  find_best_agg <- lapply(c('lpa', 'kmeans'), function(m){
    crit_best <- lapply(unique(agg$criterion), function(c){
      sdf <- agg[(agg$meth == m)&(agg$criterion == c),]
      sdf$best <- ifelse(sdf$mean_criterion == max(sdf$mean_criterion), TRUE, FALSE)
      sdf
    })
    bind_rows(crit_best)
  })
  agg <- bind_rows(find_best_agg)
  
  methods <- c('themethod' = method, "othermethod" = c("kmeans", "lpa")[c("kmeans", "lpa") != method])
  colors_tot <- c('themethod' = "#990000", "othermethod" = "darkgrey")
  colors_tot2 <- c('themethod' = "#FF1493", "othermethod" = "lightpink")
  names(colors_tot) <- methods[names(colors_tot)]
  names(colors_tot2) <- methods[names(colors_tot2)]
  
  lab_names <- c('Calinski.Harabatz' = 'Calinski and Harabatz criterion', "Calinski.Harabatz2" = "Calinski and Harabatz criterion \n modified by Krysczuk",	"Calinski.Harabatz3" = "Calinski and Harabatz criterion \n modified by Genolini",	"Ray.Turi" = "Ray and Turi criterion",	"Davies.Bouldin" = "Davies and Bouldin criterion",	"BIC" = "Bayesian Information Criterion \n (based on the number of individuals)", "BIC2" = "Bayesian Information Criterion \n (based on the number of measurements)", "AIC" = "Akaike Information Criterion", "AICc" = "Akaike Information Criterion with correction \n (based on the number of individuals)",	"AICc2" = "Akaike Information Criterion with correction \n (based on the number of measurements)",	"postProbaGlobal" = "Global posterior probability")
  
  if (nb_clusters == 1){
    p <- NULL
  } else {
    if (imp_or_all == 'all') {
      p <- ggplot(data = agg[agg$criterion == criterion,], mapping = aes(x = G, y = mean_criterion, group = meth, color = meth)) + geom_line() + geom_point() + scale_color_manual(values = colors_tot) + labs(x = 'Number of clusters', y = lab_names[criterion]) + theme(legend.position = "none") # main lines/ points
      p <- p + geom_point(mapping = aes(x = G, y = mean_criterion), data = agg[(agg$G == nb_clusters)&(agg$meth == method)&(agg$criterion == criterion),], shape = 21, size = 5, color = "#990000", fill = "gold") # add selected option
      p <- p + geom_point(mapping = aes(x = G, y = mean_criterion), data = agg[(agg$meth == method)&(agg$best == TRUE)&(agg$criterion == criterion),], color = colors_tot2[method], size = 2) + geom_point(mapping = aes(x = G, y = mean_criterion), data = agg[(agg$meth == c("kmeans", "lpa")[c("kmeans", "lpa") != method])&(agg$best == TRUE)&(agg$criterion == criterion),], color = colors_tot2[c("kmeans", "lpa")[c("kmeans", "lpa") != method]], size = 2) # add best score
    } else {
      quality_reshaped$im <- paste0(quality_reshaped$.imp, quality_reshaped$meth)
      p <- ggplot(data = quality_reshaped[quality_reshaped$criterion == criterion,], mapping = aes(x = G, y = value, group = im, color = meth)) + geom_line(linewidth = 0.1) + geom_point(size = 0.3) + scale_color_manual(values = colors_tot) + labs(x = 'Number of clusters', y = lab_names[criterion]) + theme(legend.position = "none") # main lines/ points
      p <- p + geom_point(mapping = aes(x = G, y = value), data = quality_reshaped[(quality_reshaped$G == nb_clusters)&(quality_reshaped$meth == method)&(quality_reshaped$criterion == criterion),], shape = 21, size = 1, color = "#990000", fill = "gold") # add selected option
      p <- p + geom_point(mapping = aes(x = G, y = value), data = quality_reshaped[(quality_reshaped$meth == method)&(quality_reshaped$best == TRUE)&(quality_reshaped$criterion == criterion),], color = colors_tot2[method], size = 0.4) + geom_point(mapping = aes(x = G, y = value), data = quality_reshaped[(quality_reshaped$meth == c("kmeans", "lpa")[c("kmeans", "lpa") != method])&(quality_reshaped$best == TRUE)&(quality_reshaped$criterion == criterion),], color = colors_tot2[c("kmeans", "lpa")[c("kmeans", "lpa") != method]], size = 0.4) # add best score
    }
  }
  
  return(p)
}

getDatasetWithClusters <- function(outcome_variable, nb_clusters, method) {
  data <- read_excel(paste0("results - 2/data_imputed_", outcome_variable, "_renamed2.xlsx"))
  
  G <- nb_clusters
  
  if (nb_clusters == 1) {
    data$cluster <- "1"
  } else {
    data$cluster <- as.factor(as.character(data[[paste0(method, '-clusters-', as.character(G))]]))
  } 
  
  return(data)
}


descriptiveStastsP <- function(outcome_variable, explanatory_variable, nb_clusters, method, p_value){
  p_value <- as.numeric(p_value) # significance level
  label <- c('lems' = 'Lower extremity motor score (LEMS)',
             'uems' = 'Upper extremity motor score (UEMS)',
             'ltscore' = 'Light touch total score',
             'ppscore' = 'Pinprick total score',
             'modben' = 'Modified Benzel scale',
             'age' = 'Age',
             'age_range' = 'Age Range',
             'sex' = 'Sex',
             'severity' = 'AIS Grade',
             'nli' = 'Neurological Level of Injury at Baseline',
             'nli_broad' = 'Neurological Level of Injury at Baseline',
             'nli_gr' = 'Neurological Level of Injury at Baseline',
             'level_gr' = 'Spinal Level',
             'level_broad' = 'Spinal Level',
             'level_fine' = 'Spinal Level',
             'medication' = 'Medication',
             'famotidine' = 'Famotidine',
             'ranitidine' = 'Ranitidine',
             'cimetidine' = 'Cimetidine',
             'famotidine_main' = 'Famotidine',
             'ranitidine_main' = 'Ranitidine',
             'cimetidine_main' = 'Cimetidine',
             'famotidine_color' = 'Famotidine',
             'ranitidine_color' = 'Ranitidine',
             'cimetidine_color' = 'Cimetidine',
             'famotidine_panel' = 'Famotidine',
             'ranitidine_panel' = 'Ranitidine',
             'cimetidine_panel' = 'Cimetidine',
             'bmi_status' = 'Weight Status',
             'treatment_gr' = 'Treatment Group',
             'cluster' = 'Cluster')
  data <- read_excel(paste0("results - 2/descriptive_statistics_results_", outcome_variable, "_2.xlsx"))
  subs <- data[data$method == method,]
  data_col <- list('age' = 'age', 'sex' = 'sexcd', 'severity' = 'asimpc01', 'nli_gr' = 'nligr1', 'level_gr' = 'lvlgr', 'level_fine' = 'splvl', 'level_broad' = 'lvl', 'patient' = 'ptid', 'bmi_status' = 'bmi_status', 'treatment_gr' = 'tx1_r', 'famotidine' = 'Famotidine_critical', 'ranitidine' = 'Ranitidine_critical', 'cimetidine' = 'Cimetidine_critical', 'nli' = 'nli1', 'nli_broad' = 'nli1_broad', 'none' = NULL)
  p <- ggplot(data = subs, mapping = aes(x = as.integer(G), y = subs[[data_col[[explanatory_variable]]]])) + geom_line(color = "#990000") + geom_point(color = "#990000") + geom_point(mapping = aes(x = nb_clusters, y = unlist(subs[subs$G == nb_clusters, data_col[[explanatory_variable]]])), shape = 21, size = 5, color = "#990000", fill = "gold") + ylim(0, 1) + xlim(2, 6) + geom_hline(yintercept = p_value, linetype="dashed", color = "#FF1493")
  if (explanatory_variable == 'age'){
    p <- p  + labs(x = "Number of clusters", y = 'p-value', title = paste("Effect of", label[[explanatory_variable]], "in an ANOVA Test")) + theme(plot.title = element_text(hjust = 0.5))
  } else if (explanatory_variable %in% c('famotidine', 'ranitidine', 'cimetidine')) {
    p <- p  + labs(x = "Number of clusters", y = 'p-value', caption = '* Exposure during the first 7 days after injury', title = paste("Effect of", label[[explanatory_variable]], "in a Chi-Square Test *")) + theme(plot.title = element_text(hjust = 0.5))
  } else {
    p <- p  + labs(x = "Number of clusters", y = 'p-value', title = paste("Effect of", label[[explanatory_variable]], "in a Chi-Square Test")) + theme(plot.title = element_text(hjust = 0.5))
  } 
  return(p)
}


multinomProbs <- function(outcome_variable, explanatory_variable1, explanatory_variable2, nb_clusters, method, levels = c('age' = 30, 'sex' = "M", 'severity' = "A", 'nli_gr' = "C01 - C04", 'treatment_gr' = "P", 'bmi_status' = "Healthy weight")){
  colors <- c("1" = "#DDCC77", "2" = "#117733", "3" = "#44AA99", "4" = "#999933", "5" = "#6699CC", "6" = "#888888")
  colors_mean <- c("1" = "#b0a35f", "2" = "#0b5323", "3" = "#2f766b", "4" = "#6b6b23", "5" = "#476b8e", "6" = "#5f5f5f")
  label <- c('lems' = 'Lower extremity motor score (LEMS)',
             'uems' = 'Upper extremity motor score (UEMS)',
             'ltscore' = 'Light touch total score',
             'ppscore' = 'Pinprick total score',
             'modben' = 'Modified Benzel scale',
             'age' = 'Age [years]',
             'age_range' = 'Age Range',
             'sex' = 'Sex',
             'severity' = 'AIS Grade',
             'nli' = 'Neurological Level of Injury at Baseline',
             'nli_broad' = 'Neurological Level of Injury at Baseline',
             'nli_gr' = 'Neurological Level of Injury at Baseline',
             'level_gr' = 'Spinal Level',
             'level_broad' = 'Spinal Level',
             'level_fine' = 'Spinal Level',
             'medication' = 'Medication',
             'famotidine' = 'Famotidine',
             'ranitidine' = 'Ranitidine',
             'cimetidine' = 'Cimetidine',
             'famotidine_main' = 'Famotidine',
             'ranitidine_main' = 'Ranitidine',
             'cimetidine_main' = 'Cimetidine',
             'famotidine_color' = 'Famotidine',
             'ranitidine_color' = 'Ranitidine',
             'cimetidine_color' = 'Cimetidine',
             'famotidine_panel' = 'Famotidine',
             'ranitidine_panel' = 'Ranitidine',
             'cimetidine_panel' = 'Cimetidine',
             'bmi_status' = 'Weight Status',
             'treatment_gr' = 'Treatment Group',
             'cluster' = 'Cluster')
  data_col <- list('age' = 'age', 'sex' = 'sexcd', 'severity' = 'asimpc01', 'nli_gr' = 'nligr1', 'level_gr' = 'lvlgr', 'level_fine' = 'splvl', 'level_broad' = 'lvl', 'patient' = 'ptid', 'bmi_status' = 'bmi_status', 'treatment_gr' = 'tx1_r', 'famotidine' = 'Famotidine_critical', 'ranitidine' = 'Ranitidine_critical', 'cimetidine' = 'Cimetidine_critical', 'nli' = 'nli1', 'nli_broad' = 'nli1_broad', 'none' = NULL)
  probs <- read_excel(paste0("results - 2/multinom_results_nomed_", outcome_variable, "_", method, "_2_probs.xlsx"))
  levels_for_plot <- levels[names(levels)[!(names(levels) %in% unique(c(explanatory_variable1, explanatory_variable2)))]]
  row_condition <- Reduce("&", lapply(names(levels_for_plot), function(ln){
    probs[[data_col[[ln]]]] == levels_for_plot[ln]
  }))
  probs_for_plot <- probs[row_condition, unique(c(data_col[[explanatory_variable1]], data_col[[explanatory_variable2]], "y.level", "prob", "G"))]
  probs_for_plot <- probs_for_plot[probs_for_plot$G == nb_clusters,]
  probs_1 <- aggregate(probs_for_plot$prob, lapply(colnames(probs_for_plot)[!(colnames(probs_for_plot) %in% c('y.level', 'prob'))], function(col){probs_for_plot[[col]]}), function(x){1 - sum(x)})
  colnames(probs_1) <- c(colnames(probs_for_plot)[!(colnames(probs_for_plot) %in% c('y.level', 'prob'))], 'prob')
  probs_1$y.level <- "1"
  probs_1 <- probs_1[,colnames(probs_for_plot)]
  probs_for_plot <- rbind(probs_for_plot, probs_1)
  
  if (explanatory_variable1 == "age"){
    p <- ggplot(data = probs_for_plot, mapping = aes(x = age, y = prob, color = y.level)) + geom_line() + scale_color_manual(values = colors_mean[unique(probs_for_plot$y.level)]) + labs(x = label['age'], y = "Probabilities", color = "Cluster") + ylim(0,1)
    if (explanatory_variable2 != 'none'){
      p <- p + facet_wrap(~probs_for_plot[[data_col[[explanatory_variable2]]]])
    }
  } else {
    p <- ggplot(data = probs_for_plot, mapping = aes(x = probs_for_plot[[data_col[[explanatory_variable1]]]], y = prob, color = y.level, fill = y.level)) + geom_bar(stat="identity") + labs(x = label[explanatory_variable1], y = "Probabilities", color = "Cluster", fill = "Cluster") + ylim(0,1) + scale_color_manual(values = colors_mean[unique(probs_for_plot$y.level)]) + scale_fill_manual(values = colors_mean[unique(probs_for_plot$y.level)])
    if ((explanatory_variable2 != 'none')&(explanatory_variable2 != explanatory_variable1)){
      p <- p + facet_wrap(~probs_for_plot[[data_col[[explanatory_variable2]]]])
    }
  }
  
  return(p)
}




visualizeClustersSens <- function(outcome_variable, nb_clusters, method, show, labb = FALSE) {
  G <- nb_clusters
  colors <- c("1" = "#DDCC77", "2" = "#117733", "3" = "#44AA99", "4" = "#999933", "5" = "#6699CC", "6" = "#888888")
  colors_mean <- c("1" = "#b0a35f", "2" = "#0b5323", "3" = "#2f766b", "4" = "#6b6b23", "5" = "#476b8e", "6" = "#5f5f5f")
  label <- c('lems' = "Lower extremity motor score (LEMS)",
             'uems' = "Upper extremity motor score (UEMS)",
             'ltscore' = "Light touch total score",
             'ppscore' = "Pinprick total score",
             'modben' = "Modified Benzel scale")
  cols <- list('lems' = c('lower01', 'lower04', 'lower08', 'lower16', 'lower26', 'lower52'), 'uems' = c('upper01', 'upper04', 'upper08', 'upper16', 'upper26', 'upper52'), 'ltscore' = c('ltscor01', 'ltscor04', 'ltscor08', 'ltscor16', 'ltscor26', 'ltscor52'), 'ppscore' = c('ppscor01', 'ppscor04', 'ppscor08', 'ppscor16', 'ppscor26', 'ppscor52'), 'modben' = c('modben04', 'modben08', 'modben16', 'modben26', 'modben52'))[[outcome_variable]]
  
  agg <- read_excel(file.path("data_sensitivity_analysis - 2", paste0("agg_", outcome_variable, "_2.xlsx")))
  
  if (nb_clusters == 1) {
    
    p <- ggplot()
    if ('mean_t' %in% show){
      p <- p + geom_line(mapping = aes(x = week, y = estimate), data = agg[agg$G == 1,], size = 1.5, color = 'navy')
    }
    if ('se_t' %in% show){
      p <- p + geom_ribbon(mapping = aes(x = week, y = estimate, ymin = (estimate - std.error), ymax = (estimate + std.error)), data = agg[agg$G == 1,], alpha = 0.3, fill = 'navy')
    }
    if (length(show) == 0) {
      p <- NULL
    } else {
      p <- p + theme(legend.position = "none") + labs(y = label[outcome_variable], x = "Time after injury (weeks)") 
    }
  } else { # more than one group
    
    p <- ggplot()
    if ('mean_t' %in% show){
      for (a in 1:G) {
        p <- p + geom_line(mapping = aes(x = week, y = estimate), data = agg[(agg$G == G)&(agg$method == method)&(agg$alloc == a),], size = 1.5, color = colors_mean[[as.character(a)]]) 
      }
    }
    if ('se_t' %in% show){
      for (a in 1:G) {
        p <- p + geom_ribbon(mapping = aes(x = week, y = estimate, ymin = pmax((estimate - std.error), 0), ymax = (estimate + std.error)), data = agg[(agg$G == G)&(agg$method == method)&(agg$alloc == a),], alpha = 0.3, fill = colors_mean[[as.character(a)]])
      }
    }
    if (length(show) == 0) {
      p <- NULL
    } else {
      p <- p + theme(legend.position = "none") + labs(y = label[outcome_variable], x = "Time after injury (weeks)") 
      
      if (labb == TRUE) {
        for (g in 1:G) {
          df <- data.frame('imp' = unique(agg[(agg$week == 52)&(agg$method == method)&(agg$G == G)&(agg$alloc == g),]$imp), 'lab' = rep(as.character(g)), length(unique(agg[(agg$week == 52)&(agg$method == method)&(agg$G == G)&(agg$alloc == g),]$imp)))
          if (nrow(agg[agg$G == g,] != 0)){
            p <- p + geom_text(data = df, mapping = aes(label = lab), x = 54, y = agg[(agg$week == 52)&(agg$method == method)&(agg$G == G)&(agg$alloc == g),]$estimate, fontface = "bold", size = 6)
          }
        }
      }
      
      #if (labb == TRUE) {
      #  for (g in 1:G) {
      #    p <- p + annotate("text", x = 54, y = agg[(agg$week == 52)&(agg$method == method)&(agg$G == G)&(agg$alloc == g),]$estimate, label = as.character(g), fontface = "bold", size = 8)
      #  }
      #}
      
    }
  }
  
  if (!is.null(p)){
    p <- p + facet_wrap(~imp, ncol = 2)
    if (outcome_variable %in% c('uems', 'lems')){
      p <- p + ylim(0, 50)
    } else if (outcome_variable %in% c('ltscore', 'ppscore')){
      p <- p + ylim(0, 112)
    } else {
      p <- p + ylim(0, 7)
    }
  }
  return(p)
}

clusterDistributionSens <- function(outcome_variable, nb_clusters, method, imp_or_all = 'all') {
  G <- nb_clusters
  data_i <- read_excel(file.path("data_sensitivity_analysis - 2", paste0("data_imputed_", outcome_variable, "_renamed2.xlsx")))
  data_o <- read_excel(file.path("data_sensitivity_analysis - 2", paste0("data_full_", outcome_variable, "_renamed2.xlsx")))
  if (nb_clusters == 1) {
    df <- data.frame("Cluster" = c("1"), "Observed - Number (Proportion)" = c(paste0(nrow(data_o), " (100%)")), "Imputed - Number (Proportion)" = c(paste0(as.integer(nrow(data_i)/20), " (100%)")))
    names(df) <- c("Cluster", "Observed - Number (Proportion)", "Imputed - Number (Proportion)")
  } else {
    data_i$cluster <- data_i[[paste0(method, '-clusters-', G)]]
    agg_i <- aggregate(data_i$cluster, list(data_i$cluster, data_i$.imp), length)
    names(agg_i) <- c('cluster', '.imp', 'counts')
    agg2_i <- aggregate(data_i$cluster, list(data_i$.imp), length)
    names(agg2_i) <- c('.imp', 'tot')
    agg_i$percentage <- (agg_i$counts/unique(agg2_i$tot))*100
    pre_df_list_i <- lapply(c('counts', 'percentage'), function(what){
      pre_df_i <- aggregate(agg_i[[what]], list(agg_i$cluster), function(x)c(
        'mean' = mean(x),
        'std' = sd(x)
      ))
      pre_df_i[paste0(what, "_mean")] <- pre_df_i$x[,'mean']
      pre_df_i[paste0(what, "_std")] <- pre_df_i$x[,'std']
      pre_df_i <- dplyr::select(pre_df_i, -c("x"))
      names(pre_df_i) <- c('cluster', paste0(what, "_mean"), paste0(what, "_std"))
      pre_df_i
    })
    df_pre_i <- pre_df_list_i %>% reduce(inner_join, by = "cluster")
    
    data_o$cluster <- data_o[[paste0(method, '-clusters-', G)]]
    agg_o <- aggregate(data_o$cluster, list(data_o$cluster), function(x)c("l"=length(x), "p"=length(x)/length(data_o$cluster)))
    agg_o$l <- agg_o$x[,'l']
    agg_o$p <- agg_o$x[,'p']
    agg_o <- dplyr::select(agg_o, -c("x"))
    names(agg_o) <- c('cluster', 'counts', 'prop')
    
    for (g in 1:G){
      if ((g %in% agg_o$cluster)&(!(g %in% df_pre_i$cluster))){
        df_pre_i <- rbind(def_pre_i, data.frame('cluster' = g, 'counts_mean' = 0, 'counts_std' = 0, 'percentage_mean' = 0, 'percentage_std' = 0))
      } else if ((!(g %in% agg_o$cluster))&(g %in% df_pre_i$cluster)) {
        agg_o <- rbind(agg_o, data.frame('cluster' = g, 'counts' = 0, 'prop' = 0))
      }
    }
    
    df_pre_i <- df_pre_i[order(df_pre_i$cluster),]
    agg_o <- agg_o[order(agg_o$cluster),]
    
    df <- data.frame("Cluster" = as.character(df_pre_i$cluster), "Observed - Number (Proportion)" = paste0(as.integer(agg_o$counts), " (", round(agg_o$prop, 2), ")"), "Imputed - mean number ± std (mean proportion ± std)" = paste0(as.integer(df_pre_i$counts_mean), " ± ", round(df_pre_i$counts_std), " (", "(", round(df_pre_i$percentage_mean, 2), " ± ", round(df_pre_i$percentage_std, 2), ") %", ")"))
    names(df) <- c("Cluster", "Observed - Number (Proportion)", "Imputed - mean number ± std (mean proportion ± std)")
  }

  return(df)
}

aboutClustersSens <- function(outcome_variable, nb_clusters, method){
  data_imp <- read_excel(file.path("data_sensitivity_analysis - 2", paste0("data_imputed_", outcome_variable, "_renamed2.xlsx")))
  data_full <- read_excel(file.path("data_sensitivity_analysis - 2", paste0("data_full_", outcome_variable, "_renamed2.xlsx")))
  if (nb_clusters == 1) {
    empty_imp <- 'NO'
    consistency_imp <- 'YES'
    empty_full <- 'NO'
  } else {
    otherq <- sapply(1:20, function(i){
      sub <- data_imp[data_imp$.imp == i,]
      length(unique(sub[[paste0(method, '-clusters-', nb_clusters)]]))
    })
    if (any(otherq != nb_clusters)){
      empty_imp <- 'YES'
    } else {
      empty_imp <- 'NO'
    }
    if (length(unique(otherq)) > 1) {
      consistency_imp <- 'NO'
    } else {
      consistency_imp <- 'YES'
    }
    if (length(unique(data_full[[paste0(method, '-clusters-', nb_clusters)]])) != nb_clusters){
      empty_full <- 'YES'
    } else {
      empty_full <- 'NO'
    }
  }
  return(list('empty_imp' = empty_imp, 'consistency_imp' = consistency_imp, 'empty_full' = empty_full))
}

plotEvaluationSens <- function(outcome_variable, nb_clusters, method, criterion) {
  # https://rdrr.io/cran/longitudinalData/man/qualityCriterion.html
  
  p <- NULL
  
  quality_df_list1 <- lapply(c('kmeans', 'lpa'), function(m){
    quality_df1 <- read_excel(file.path("data_sensitivity_analysis - 2", paste0(m, "_quality_criteria_imputed_", outcome_variable, "_renamed2.xlsx")))
    quality_df1$meth <- m
    quality_df1
  })
  quality1 <- bind_rows(quality_df_list1)
  quality1$imp <- 'imputed'
  
  quality_df_list2 <- lapply(c('kmeans', 'lpa'), function(m){
    quality_df2 <- read_excel(file.path("data_sensitivity_analysis - 2", paste0(m, "_quality_criteria_full_", outcome_variable, "_renamed2.xlsx")))
    quality_df2$meth <- m
    quality_df2
  })
  quality2 <- bind_rows(quality_df_list2)
  quality2$imp <- 'observed'
  
  reshape_list1 <- lapply(c('Calinski.Harabatz',	'Calinski.Harabatz2',	'Calinski.Harabatz3',	'BIC',	'BIC2',	'AIC',	'AICc',	'AICc2',	'postProbaGlobal', 'Ray.Turi',	'Davies.Bouldin'), function(crit){
    sub_df <- quality1[, c('.imp', 'meth', 'G', crit)]
    names(sub_df) <- c('.imp', 'meth', 'G', 'value')
    sub_df$criterion <- crit
    find_best_list <- lapply(c('kmeans', 'lpa'), function(m){
      imp_best <- lapply(1:20, function(i){
        sdf <- sub_df[(sub_df$meth == m)&(sub_df$.imp == i),]
        sdf$best <- ifelse(sdf$value == max(sdf$value), TRUE, FALSE)
        sdf
      })
      bind_rows(imp_best)
    })
    bind_rows(find_best_list)
  })
  quality_reshaped1 <- bind_rows(reshape_list1)
  
  agg <- aggregate(quality_reshaped1$value, list(quality_reshaped1$meth, quality_reshaped1$G, quality_reshaped1$criterion), mean)
  names(agg) <- c('meth', 'G', 'criterion', 'mean_criterion')
  find_best_agg <- lapply(c('lpa', 'kmeans'), function(m){
    crit_best <- lapply(unique(agg$criterion), function(c){
      sdf <- agg[(agg$meth == m)&(agg$criterion == c),]
      sdf$best <- ifelse(sdf$mean_criterion == max(sdf$mean_criterion), TRUE, FALSE)
      sdf
    })
    bind_rows(crit_best)
  })
  agg <- bind_rows(find_best_agg)
  
  reshape_list2 <- lapply(c('Calinski.Harabatz',	'Calinski.Harabatz2',	'Calinski.Harabatz3',	'BIC',	'BIC2',	'AIC',	'AICc',	'AICc2',	'postProbaGlobal', 'Ray.Turi',	'Davies.Bouldin'), function(crit){
    sub_df <- quality2[, c('meth', 'G', crit)]
    names(sub_df) <- c('meth', 'G', 'value')
    sub_df$criterion <- crit
    find_best_list <- lapply(c('kmeans', 'lpa'), function(m){
      sdf <- sub_df[sub_df$meth == m,]
      sdf$best <- ifelse(sdf$value == max(sdf$value), TRUE, FALSE)
      sdf
    })
    bind_rows(find_best_list)
  })
  quality_reshaped2 <- bind_rows(reshape_list2)
  
  agg$imp <- 'imputed'
  quality_reshaped2$imp <- 'observed'
  names(agg) <- c('meth', 'G', 'criterion', 'value', 'best', 'imp')
  agg <- agg[, colnames(quality_reshaped2)]
  agg <- rbind(agg, quality_reshaped2)
  
  methods <- c('themethod' = method, "othermethod" = c("kmeans", "lpa")[c("kmeans", "lpa") != method])
  colors_tot <- c('themethod' = "#990000", "othermethod" = "darkgrey")
  colors_tot2 <- c('themethod' = "#FF1493", "othermethod" = "lightpink")
  names(colors_tot) <- methods[names(colors_tot)]
  names(colors_tot2) <- methods[names(colors_tot2)]
  
  lab_names <- c('Calinski.Harabatz' = 'Calinski and Harabatz criterion', "Calinski.Harabatz2" = "Calinski and Harabatz criterion \n modified by Krysczuk",	"Calinski.Harabatz3" = "Calinski and Harabatz criterion \n modified by Genolini",	"Ray.Turi" = "Ray and Turi criterion",	"Davies.Bouldin" = "Davies and Bouldin criterion",	"BIC" = "Bayesian Information Criterion \n (based on the number of individuals)", "BIC2" = "Bayesian Information Criterion \n (based on the number of measurements)", "AIC" = "Akaike Information Criterion", "AICc" = "Akaike Information Criterion with correction \n (based on the number of individuals)",	"AICc2" = "Akaike Information Criterion with correction \n (based on the number of measurements)",	"postProbaGlobal" = "Global posterior probability")
  
  if (nb_clusters == 1){
    p <- NULL
  } else {
    p <- ggplot(data = agg[agg$criterion == criterion,], mapping = aes(x = G, y = value, group = meth, color = meth)) + geom_line() + geom_point() + scale_color_manual(values = colors_tot) + labs(x = 'Number of clusters', y = lab_names[criterion]) + theme(legend.position = "none") # main lines/ points
    p <- p + geom_point(mapping = aes(x = G, y = value), data = agg[(agg$G == nb_clusters)&(agg$meth == method)&(agg$criterion == criterion),], shape = 21, size = 5, color = "#990000", fill = "gold") # add selected option
    p <- p + geom_point(mapping = aes(x = G, y = value), data = agg[(agg$meth == method)&(agg$best == TRUE)&(agg$criterion == criterion),], color = colors_tot2[method], size = 2) + geom_point(mapping = aes(x = G, y = value), data = agg[(agg$meth == c("kmeans", "lpa")[c("kmeans", "lpa") != method])&(agg$best == TRUE)&(agg$criterion == criterion),], color = colors_tot2[c("kmeans", "lpa")[c("kmeans", "lpa") != method]], size = 2) # add best score
    p <- p + facet_wrap(~imp)
  }
  
  return(p)
}

getDatasetWithClustersSens <- function(outcome_variable, nb_clusters, method, obs_or_imp) {
  if (obs_or_imp == 'obs'){
    data <- read_excel(paste0("data_sensitivity_analysis - 2/data_full_", outcome_variable, "_renamed2.xlsx"))
  } else {
    data <- read_excel(paste0("data_sensitivity_analysis - 2/data_imputed_", outcome_variable, "_renamed2.xlsx"))
  }
  
  G <- nb_clusters
  
  if (nb_clusters == 1) {
    data$cluster <- "1"
  } else {
    data$cluster <- as.factor(as.character(data[[paste0(method, '-clusters-', as.character(G))]]))
  } 
  
  return(data)
}


descriptiveStastsPSens <- function(outcome_variable, explanatory_variable, nb_clusters, method, p_value){
  p_value <- as.numeric(p_value) # significance level
  label <- c('lems' = 'Lower extremity motor score (LEMS)',
             'uems' = 'Upper extremity motor score (UEMS)',
             'ltscore' = 'Light touch total score',
             'ppscore' = 'Pinprick total score',
             'modben' = 'Modified Benzel scale',
             'age' = 'Age',
             'age_range' = 'Age Range',
             'sex' = 'Sex',
             'severity' = 'AIS Grade',
             'nli' = 'Neurological Level of Injury at Baseline',
             'nli_broad' = 'Neurological Level of Injury at Baseline',
             'nli_gr' = 'Neurological Level of Injury at Baseline',
             'level_gr' = 'Spinal Level',
             'level_broad' = 'Spinal Level',
             'level_fine' = 'Spinal Level',
             'medication' = 'Medication',
             'famotidine' = 'Famotidine',
             'ranitidine' = 'Ranitidine',
             'cimetidine' = 'Cimetidine',
             'famotidine_main' = 'Famotidine',
             'ranitidine_main' = 'Ranitidine',
             'cimetidine_main' = 'Cimetidine',
             'famotidine_color' = 'Famotidine',
             'ranitidine_color' = 'Ranitidine',
             'cimetidine_color' = 'Cimetidine',
             'famotidine_panel' = 'Famotidine',
             'ranitidine_panel' = 'Ranitidine',
             'cimetidine_panel' = 'Cimetidine',
             'bmi_status' = 'Weight Status',
             'treatment_gr' = 'Treatment Group',
             'cluster' = 'Cluster')
  data <- read_excel(paste0("data_sensitivity_analysis - 2/descriptive_statistics_results_", outcome_variable, "_2.xlsx"))
  subs <- data[data$method == method,]
  data_col <- list('age' = 'age', 'sex' = 'sexcd', 'severity' = 'asimpc01', 'nli_gr' = 'nligr1', 'level_gr' = 'lvlgr', 'level_fine' = 'splvl', 'level_broad' = 'lvl', 'patient' = 'ptid', 'bmi_status' = 'bmi_status', 'treatment_gr' = 'tx1_r', 'famotidine' = 'Famotidine_critical', 'ranitidine' = 'Ranitidine_critical', 'cimetidine' = 'Cimetidine_critical', 'nli' = 'nli1', 'nli_broad' = 'nli1_broad', 'none' = NULL)
  p <- ggplot(data = subs, mapping = aes(x = as.integer(G), y = subs[[data_col[[explanatory_variable]]]])) + geom_line(color = "#990000") + geom_point(color = "#990000") + ylim(0, 1) + xlim(2, 6) + geom_hline(yintercept = p_value, linetype="dashed", color = "#FF1493")+ geom_vline(xintercept = nb_clusters, linetype="dashed", color = "gold")
  if (explanatory_variable == 'age'){
    p <- p  + labs(x = "Number of clusters", y = 'p-value', title = paste("Effect of", label[[explanatory_variable]], "in an ANOVA Test")) + theme(plot.title = element_text(hjust = 0.5))
  } else if (explanatory_variable %in% c('famotidine', 'ranitidine', 'cimetidine')) {
    p <- p  + labs(x = "Number of clusters", y = 'p-value', caption = '* Exposure during the first 7 days after injury', title = paste("Effect of", label[[explanatory_variable]], "in a Chi-Square Test *")) + theme(plot.title = element_text(hjust = 0.5))
  } else {
    p <- p  + labs(x = "Number of clusters", y = 'p-value', title = paste("Effect of", label[[explanatory_variable]], "in a Chi-Square Test")) + theme(plot.title = element_text(hjust = 0.5))
  } 
  p <- p + facet_wrap(~imp)
  return(p)
}


multinomProbsSens <- function(outcome_variable, explanatory_variable1, explanatory_variable2, nb_clusters, method, levels = c('age' = 30, 'sex' = "M", 'severity' = "A", 'nli_gr' = "C01 - C04", 'treatment_gr' = "P", 'bmi_status' = "Healthy weight")){
  colors <- c("1" = "#DDCC77", "2" = "#117733", "3" = "#44AA99", "4" = "#999933", "5" = "#6699CC", "6" = "#888888")
  colors_mean <- c("1" = "#b0a35f", "2" = "#0b5323", "3" = "#2f766b", "4" = "#6b6b23", "5" = "#476b8e", "6" = "#5f5f5f")
  label <- c('lems' = 'Lower extremity motor score (LEMS)',
             'uems' = 'Upper extremity motor score (UEMS)',
             'ltscore' = 'Light touch total score',
             'ppscore' = 'Pinprick total score',
             'modben' = 'Modified Benzel scale',
             'age' = 'Age [years]',
             'age_range' = 'Age Range',
             'sex' = 'Sex',
             'severity' = 'AIS Grade',
             'nli' = 'Neurological Level of Injury at Baseline',
             'nli_broad' = 'Neurological Level of Injury at Baseline',
             'nli_gr' = 'Neurological Level of Injury at Baseline',
             'level_gr' = 'Spinal Level',
             'level_broad' = 'Spinal Level',
             'level_fine' = 'Spinal Level',
             'medication' = 'Medication',
             'famotidine' = 'Famotidine',
             'ranitidine' = 'Ranitidine',
             'cimetidine' = 'Cimetidine',
             'famotidine_main' = 'Famotidine',
             'ranitidine_main' = 'Ranitidine',
             'cimetidine_main' = 'Cimetidine',
             'famotidine_color' = 'Famotidine',
             'ranitidine_color' = 'Ranitidine',
             'cimetidine_color' = 'Cimetidine',
             'famotidine_panel' = 'Famotidine',
             'ranitidine_panel' = 'Ranitidine',
             'cimetidine_panel' = 'Cimetidine',
             'bmi_status' = 'Weight Status',
             'treatment_gr' = 'Treatment Group',
             'cluster' = 'Cluster')
  data_col <- list('age' = 'age', 'sex' = 'sexcd', 'severity' = 'asimpc01', 'nli_gr' = 'nligr1', 'level_gr' = 'lvlgr', 'level_fine' = 'splvl', 'level_broad' = 'lvl', 'patient' = 'ptid', 'bmi_status' = 'bmi_status', 'treatment_gr' = 'tx1_r', 'famotidine' = 'Famotidine_critical', 'ranitidine' = 'Ranitidine_critical', 'cimetidine' = 'Cimetidine_critical', 'nli' = 'nli1', 'nli_broad' = 'nli1_broad', 'none' = NULL)
  probs1 <- read_excel(paste0("data_sensitivity_analysis - 2/multinom_results_full_nomed_", outcome_variable, "_", method, "_2_probs.xlsx"))
  probs2 <- read_excel(paste0("data_sensitivity_analysis - 2/multinom_results_imputed_nomed_", outcome_variable, "_", method, "_2_probs.xlsx"))
  probs1$imp <- 'observed'
  probs2$imp <- 'imputed'
  probs <- rbind(probs1, probs2)
  levels_for_plot <- levels[names(levels)[!(names(levels) %in% unique(c(explanatory_variable1, explanatory_variable2)))]]
  row_condition <- Reduce("&", lapply(names(levels_for_plot), function(ln){
    probs[[data_col[[ln]]]] == levels_for_plot[ln]
  }))
  probs_for_plot <- probs[row_condition, unique(c(data_col[[explanatory_variable1]], data_col[[explanatory_variable2]], "y.level", "prob", "G", "imp"))]
  probs_for_plot <- probs_for_plot[probs_for_plot$G == nb_clusters,]
  probs_1 <- aggregate(probs_for_plot$prob, lapply(colnames(probs_for_plot)[!(colnames(probs_for_plot) %in% c('y.level', 'prob'))], function(col){probs_for_plot[[col]]}), function(x){1 - sum(x)})
  colnames(probs_1) <- c(colnames(probs_for_plot)[!(colnames(probs_for_plot) %in% c('y.level', 'prob'))], 'prob')
  probs_1$y.level <- "1"
  probs_1 <- probs_1[,colnames(probs_for_plot)]
  probs_for_plot <- rbind(probs_for_plot, probs_1)
  
  if (explanatory_variable1 == "age"){
    p <- ggplot(data = probs_for_plot, mapping = aes(x = age, y = prob, color = y.level)) + geom_line() + scale_color_manual(values = colors_mean[unique(probs_for_plot$y.level)]) + labs(x = label['age'], y = "Probabilities", color = "Cluster") + ylim(0,1) + facet_wrap(~imp)
    if (explanatory_variable2 != 'none'){
      p <- p + facet_grid(probs_for_plot[[data_col[[explanatory_variable2]]]]~imp)
    }
  } else {
    p <- ggplot(data = probs_for_plot, mapping = aes(x = probs_for_plot[[data_col[[explanatory_variable1]]]], y = prob, color = y.level, fill = y.level)) + geom_bar(stat="identity") + labs(x = label[explanatory_variable1], y = "Probabilities", color = "Cluster", fill = "Cluster") + ylim(0,1) + scale_color_manual(values = colors_mean[unique(probs_for_plot$y.level)]) + scale_fill_manual(values = colors_mean[unique(probs_for_plot$y.level)]) + facet_wrap(~imp)
    if ((explanatory_variable2 != 'none')&(explanatory_variable2 != explanatory_variable1)){
      p <- p + facet_grid(probs_for_plot[[data_col[[explanatory_variable2]]]]~imp)
    }
  }
  
  return(p)
}