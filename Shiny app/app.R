#------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# BASICS
#------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

# Load libraries
library(rsconnect)
library(readxl)
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(shinyjs)
library(ggplot2)
library(bslib)
library(thematic)
library(fresh)
library(tidyverse)
library(dplyr)
library(finalfit)
library(summaryBox)
library(UpSetR)
library(ComplexUpset)
library(cowplot)
library(scales)
library(data.table)
library(naniar)
library(grid)
library(gridExtra)

# unload mclust and magick if loaded (otherwise error)
if("mclust" %in% (.packages())){
  detach("package:mclust", unload=TRUE) 
}

if("magick" %in% (.packages())){
  detach("package:magick", unload=TRUE) 
}

# Set working directory
#basic_path <- "C:/Users/LAURA/Documents/ETHZ/Master/Master thesis/SCI project/code - final version - 2/Shiny app" # THIS IS THE PATH YOU NEED TO CHANGE
#setwd(basic_path) 

# Load the data
source("00_dataset.R")

# Load extra functions and text for the app
source("app_functions.R")
source("app_text.R")

#------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# USER INTERFACE
#------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

ui <- dashboardPage(#skin = "blue",
  # A dashboardPage consists of 3 elements: dashboardHeader, dashboardSidebar, and dashboardBody
  
  # dashboardHeader: title of the menu
  dashboardHeader(title = "SCI Recovery"), 
  
  # dashboardSidebar: defines the items in the menu (icons: https://fontawesome.com/search)
  dashboardSidebar(
    sidebarMenu(
      menuItem("Home", tabName = "home", icon = icon("house")), # Home page
      menuItem("Abbreviations and Definitions", tabName = "explain", icon = icon("lightbulb")), # There variables names and their abbreviations will be explained
      menuItem("Sygen Data", tabName = "data", icon = icon("database"), # This section will allow the user to play with slider bars and understand data with the help of interactive plots and tables
               menuSubItem("About the Study", tabName = "info", icon = icon("circle-info")), # Information about the study
               menuSubItem("Baseline Characteristics", tabName = "demo", icon = icon("user")), # Explore demographic variables
               menuSubItem("Outcome Variables", tabName = "outcome", icon = icon("tree")), # Explore outcome variables
               menuSubItem("Missing Data", tabName = "missing", icon = icon("droplet")) # Explore missing data mechanism
               ),
      menuItem("Clustering", tabName = "cluster", icon = icon("circle-nodes"), # Understanding clustering methods
               menuSubItem("About the Methods", tabName = "info-methods", icon = icon("circle-info")), # Explanation of the methods
               menuSubItem("Method Comparison", tabName = "results", icon = icon("pen")), # Exploration of the methods though plots as well as the corresponding evaluation metrics
               menuSubItem("Patient and Injury Characteristics", tabName = "char_cluster", icon = icon("thumbtack"))# Interactive plots
               ),
      menuItem("Sensitivity Analysis", tabName = "sensitivity", icon = icon("feather-pointed"), # Are the results obtained with missing data and imputation significantly different from results obtained with no missing data?
               menuSubItem("About the Sensitivity Analysis", tabName = "info-sens", icon = icon("circle-info")), # Explanation of the sensitivity analysis
               menuSubItem("Clustering Results", tabName = "clust_sens", icon = icon("diagram-project")), # How different are the clustering results?
               menuSubItem("Patient and Injury Characteristics", tabName = "char_sens", icon = icon("chart-pie")) # How different are the effects of the other variables?
      )
      )
    
    ),
  
  # dashboardBody: actual text and interactive elements
  dashboardBody(
    
    # Choose colors
    tags$head(tags$style(HTML(' # personalization - more on: https://stackoverflow.com/questions/52198452/how-to-change-the-background-color-of-the-shiny-dashboard-body
                                #                            https://stackoverflow.com/questions/31711307/how-to-change-color-in-shiny-dashboard
                                #                            https://gist.github.com/shaun-jacks/29c5e29c88a447267447ebbc26187c25 
                                #                            https://itecnote.com/tecnote/r-how-to-change-color-in-shiny-dashboard/
                                #                            https://www.appsloveworld.com/r/100/13/how-to-change-the-background-color-of-the-shiny-dashboard-body?expand_article=1
        .logo {
                              background-color: #0381FF;
                              }
        
        /* logo */
        .skin-blue .main-header .logo {
                              background-color: #0381FF;
                              }

        /* logo when hovered */
        .skin-blue .main-header .logo:hover {
                              background-color: #0381FF;
                              }

        /* navbar (rest of the header) */
        .skin-blue .main-header .navbar {
                              background-color: #0381FF;
                              }       
        
        /* Background of the page */                      
        .content-wrapper, .right-side {
                             background-color: #DBE9FA;
                             }                      
                              
        /* Sidebar menu items when active or hovered */                      
        .skin-blue .sidebar-menu > li.active > a,
        .skin-blue .sidebar-menu > li:hover > a {
                             border-left-color: #0381FF;
                             }                     
                             
        /* toggle button when hovered  */
        .skin-blue .main-header .navbar .sidebar-toggle:hover{
                             background-color: #0381FF;
                             } 
         #sidebar {
                             background-color: #f4f8fd;
                             }                     
         
        #my_tab_box{height:200px !important;}
        
        .js-irs-0 .irs-single, .js-irs-0 .irs-bar-edge, .js-irs-0 .irs-bar, .js-irs-0 .irs-from, .js-irs-0 .irs-to {background: #0381FF}
        .js-irs-1 .irs-single, .js-irs-1 .irs-bar-edge, .js-irs-1 .irs-bar, .js-irs-1 .irs-from, .js-irs-1 .irs-to {background: #0381FF}
        .js-irs-2 .irs-single, .js-irs-2 .irs-bar-edge, .js-irs-2 .irs-bar, .js-irs-2 .irs-from, .js-irs-2 .irs-to {background: #0381FF}
        .js-irs-3 .irs-single, .js-irs-3 .irs-bar-edge, .js-irs-3 .irs-bar, .js-irs-3 .irs-from, .js-irs-3 .irs-to {background: #0381FF}
        .js-irs-4 .irs-single, .js-irs-4 .irs-bar-edge, .js-irs-4 .irs-bar, .js-irs-4 .irs-from, .js-irs-4 .irs-to {background: #0381FF}
        .js-irs-5 .irs-single, .js-irs-5 .irs-bar-edge, .js-irs-5 .irs-bar, .js-irs-5 .irs-from, .js-irs-5 .irs-to {background: #0381FF} 
        .js-irs-6 .irs-single, .js-irs-6 .irs-bar-edge, .js-irs-6 .irs-bar, .js-irs-6 .irs-from, .js-irs-6 .irs-to {background: #0381FF}
        .js-irs-7 .irs-single, .js-irs-7 .irs-bar-edge, .js-irs-7 .irs-bar, .js-irs-7 .irs-from, .js-irs-7 .irs-to {background: #0381FF}
        .js-irs-8 .irs-single, .js-irs-8 .irs-bar-edge, .js-irs-8 .irs-bar, .js-irs-8 .irs-from, .js-irs-8 .irs-to {background: #0381FF}
        .js-irs-9 .irs-single, .js-irs-9 .irs-bar-edge, .js-irs-9 .irs-bar, .js-irs-9 .irs-from, .js-irs-9 .irs-to {background: #0381FF}
        .js-irs-10 .irs-single, .js-irs-10 .irs-bar-edge, .js-irs-10 .irs-bar, .js-irs-10 .irs-from, .js-irs-10 .irs-to {background: #0381FF}
        .js-irs-11 .irs-single, .js-irs-11 .irs-bar-edge, .js-irs-11 .irs-bar, .js-irs-11 .irs-from, .js-irs-11 .irs-to {background: #0381FF}
        .js-irs-12 .irs-single, .js-irs-12 .irs-bar-edge, .js-irs-12 .irs-bar, .js-irs-12 .irs-from, .js-irs-12 .irs-to {background: #0381FF}
        .js-irs-13 .irs-single, .js-irs-13 .irs-bar-edge, .js-irs-13 .irs-bar, .js-irs-13 .irs-from, .js-irs-13 .irs-to {background: #0381FF}
        .js-irs-14 .irs-single, .js-irs-14 .irs-bar-edge, .js-irs-14 .irs-bar, .js-irs-14 .irs-from, .js-irs-14 .irs-to {background: #0381FF}
        .js-irs-15 .irs-single, .js-irs-15 .irs-bar-edge, .js-irs-15 .irs-bar, .js-irs-15 .irs-from, .js-irs-15 .irs-to {background: #0381FF}
        .js-irs-16 .irs-single, .js-irs-16 .irs-bar-edge, .js-irs-16 .irs-bar, .js-irs-16 .irs-from, .js-irs-16 .irs-to {background: #0381FF}
        .js-irs-17 .irs-single, .js-irs-17 .irs-bar-edge, .js-irs-17 .irs-bar, .js-irs-17 .irs-from, .js-irs-17 .irs-to {background: #0381FF}
        .js-irs-18 .irs-single, .js-irs-18 .irs-bar-edge, .js-irs-18 .irs-bar, .js-irs-18 .irs-from, .js-irs-18 .irs-to {background: #0381FF}
        .js-irs-19 .irs-single, .js-irs-19 .irs-bar-edge, .js-irs-19 .irs-bar, .js-irs-19 .irs-from, .js-irs-19 .irs-to {background: #0381FF}
        .js-irs-20 .irs-single, .js-irs-20 .irs-bar-edge, .js-irs-20 .irs-bar, .js-irs-20 .irs-from, .js-irs-20 .irs-to {background: #0381FF}
        .js-irs-21 .irs-single, .js-irs-21 .irs-bar-edge, .js-irs-21 .irs-bar, .js-irs-21 .irs-from, .js-irs-21 .irs-to {background: #0381FF}
        .js-irs-22 .irs-single, .js-irs-22 .irs-bar-edge, .js-irs-22 .irs-bar, .js-irs-22 .irs-from, .js-irs-22 .irs-to {background: #0381FF}
        .js-irs-23 .irs-single, .js-irs-23 .irs-bar-edge, .js-irs-23 .irs-bar, .js-irs-23 .irs-from, .js-irs-23 .irs-to {background: #0381FF}
        .js-irs-24 .irs-single, .js-irs-24 .irs-bar-edge, .js-irs-24 .irs-bar, .js-irs-24 .irs-from, .js-irs-24 .irs-to {background: #0381FF}
        .js-irs-25 .irs-single, .js-irs-25 .irs-bar-edge, .js-irs-25 .irs-bar, .js-irs-25 .irs-from, .js-irs-25 .irs-to {background: #0381FF}
        .js-irs-26 .irs-single, .js-irs-26 .irs-bar-edge, .js-irs-26 .irs-bar, .js-irs-26 .irs-from, .js-irs-26 .irs-to {background: #0381FF}
        .js-irs-27 .irs-single, .js-irs-27 .irs-bar-edge, .js-irs-27 .irs-bar, .js-irs-27 .irs-from, .js-irs-27 .irs-to {background: #0381FF}
                              '))),
    
    
    # Create a page for each menu item and add content
    tabItems(
      
      # HOME PAGE ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
      tabItem(tabName = "home",
              fluidPage(
                titlePanel(h1(span(strong(title), style = "color:#000080"), align = "center")),
                fluidRow(
                  br(),
                  p(home_p1[1], em(home_p1[2]), home_p1[3], align = "justify"),
                  br(),
                  h4(strong('Background')),
                  br(),
                  p(home_p2, align = "justify"),
                  br(),
                  h4(strong('Structure of the App')),
                  br(),
                  p(home_p3, align = "justify"),
                  br(),
                  fluidRow(
                    column(6, 
                           box(id = 'box-def', fluidRow(
                             column(2, p(h1(span(icon("lightbulb"), style = "font-size:60px")), align = "center")),
                             column(10, strong("Abbreviations and Definitions"),
                                    p(home_p3_def, align = "justify"),
                                    br())
                           ), width = 12)),
                    column(6, 
                           box(id = 'box-sygen', fluidRow(
                             column(2, p(h1(span(icon("database"), style = "font-size:60px")), align = "center")),
                             column(10, strong("Sygen Data"),
                                    p(home_p3_sygen, align = "justify"))
                           ), width = 12))
                  ),
                  fluidRow(
                    column(6, 
                           box(id = 'box-clustering', fluidRow(
                             column(2, p(h1(span(icon("circle-nodes"), style = "font-size:60px")), align = "center")),
                             column(10, strong("Clustering"),
                                    p(home_p3_clustering, align = "justify"))
                           ), width = 12)),
                    column(6, 
                           box(id = 'box-sensitivity', fluidRow(
                             column(2, p(h1(span(icon("feather-pointed"), style = "font-size:60px")), align = "center")),
                             column(10, strong("Sensitivity Analysis"),
                                    p(home_p3_sensitivity, align = "justify"))
                           ), width = 12))
                  ),
                  br(),
                  h4(strong('Ethics')),
                  br(),
                  p(home_p4, align = "justify"),
                  br(),
                  #h4(strong('Contributions')),
                  #br(),
                  #p(home_p5, align = "justify"),
                  #br(),
                  h4(strong('Funding')),
                  br(),
                  a(href="https://www.studyfoundation.ch/", imageOutput("ssf_logo", height = '100px')),
                  br(),
                  h4(strong('References')),
                  br(),
                  p(home_ref1, align = "justify"),
                  p(home_ref2, align = "justify"),
                  p(home_ref3, align = "justify"),
                  p(home_ref4, align = "justify"),
                  p(home_ref5, align = "justify"),
                  p(home_ref6, align = "justify"),
                  p(home_ref7, align = "justify"),
                  p(home_ref8, align = "justify"),
                  p(home_ref9, align = "justify"),
                  p(home_ref10, align = "justify"),
                  p(home_ref11, align = "justify"),
                  p(home_ref12, align = "justify"),
                  p(home_ref13, align = "justify"),
                  p(home_ref14, align = "justify"),
                  p(home_ref15, align = "justify")
                )
                )
              ),
  
      # ABBREVIATIONS AND DEFINITIONS -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
    
      tabItem(tabName = "explain",
              fluidPage(
                titlePanel(h3(span(strong("Abbreviations and Definitions"), style = "color:#000080"), align = "center")),
                fluidRow(
                  br(),
                  p(va_p1, align = "justify"),
                  useShinyjs(),
                  box( # LEMS
                    fluidRow(
                      column(10, p(strong("LEMS"), ": Lower Extremity Motor Score")),
                      column(2, actionButton("lems_btn", icon('info'), style="background-color: #ADDFFF; border-color: #ADDFFF"), align = "right")
                    ),
                    p(id = "lems_exp", lems, align = "justify"),
                    width = 12
                  ),
                  
                  box( # UEMS
                    fluidRow(
                      column(10, p(strong("UEMS"), ": Upper Extremity Motor Score")),
                      column(2, actionButton("uems_btn", icon('info'), style="background-color: #ADDFFF; border-color: #ADDFFF"), align = "right")
                    ),
                    p(id = "uems_exp", uems, align = "justify"),
                    width = 12
                  ),
                  
                  box( # TMS
                    fluidRow(
                      column(10, p(strong("TMS"), ": Total Motor Score")),
                      column(2, actionButton("tms_btn", icon('info'), style="background-color: #ADDFFF; border-color: #ADDFFF"), align = "right")
                    ),
                    p(id = "tms_exp", tms, align = "justify"),
                    width = 12
                  ),
                  
                  box( # Sensory scores
                    fluidRow(
                      column(10, p(strong("Sensory scores"), ": Pinprick total score and light touch total score")),
                      column(2, actionButton("ss_btn", icon('info'), style="background-color: #ADDFFF; border-color: #ADDFFF"), align = "right")
                    ),
                    p(id = "ss_exp", ss, align = "justify"),
                    width = 12
                  ),
                  
                  box( # AIS
                    fluidRow(
                      column(10, p(strong("AIS"), ": ASIA Impairment Scale")),
                      column(2, actionButton("ais_btn", icon('info'), style="background-color: #ADDFFF; border-color: #ADDFFF"), align = "right")
                    ),
                    p(id = "ais_exp", ais, align = "justify"),
                    #tableOutput("ais_table"),
                    width = 12
                  ),
                  
                  box( # Benzel
                    fluidRow(
                      column(10, p(strong("MBC"), ": Modified Benzel Classification")),
                      column(2, actionButton("mbc_btn", icon('info'), style="background-color: #ADDFFF; border-color: #ADDFFF"), align = "right")
                    ),
                    p(id = "mbc_exp", mbc, align = "justify"),
                    #tableOutput("mbc_table"),
                    width = 12
                  ),
                  
                  box( # Neurological level of injury
                    fluidRow(
                      column(10, p(strong("NLI"), ": Neurological Level of Injury")),
                      column(2, actionButton("nli_btn", icon('info'), style="background-color: #ADDFFF; border-color: #ADDFFF"), align = "right")
                    ),
                    p(id = "nli_exp", nli, align = "justify"),
                    width = 12
                  ),
                  
                  box( # Spinal level
                    fluidRow(
                      column(10, p(strong("Spinal level"))),
                      column(2, actionButton("splvl_btn", icon('info'), style="background-color: #ADDFFF; border-color: #ADDFFF"), align = "right")
                    ),
                    p(id = "splvl_exp", splvl, align = "justify"),
                    width = 12
                  ),
                  
                  box( # ER
                    fluidRow(
                      column(10, p(strong("ER"), ": Emergency Room")),
                      column(2, actionButton("er_btn", icon('info'), style="background-color: #ADDFFF; border-color: #ADDFFF"), align = "right")
                    ),
                    p(id = "er_exp", er, align = "justify"),
                    width = 12
                  ),
                  
                  box( # Baseline
                    fluidRow(
                      column(10, p(strong("Baseline"))),
                      column(2, actionButton("baseline_btn", icon('info'), style="background-color: #ADDFFF; border-color: #ADDFFF"), align = "right")
                    ),
                    p(id = "baseline_exp", baseline, align = "justify"),
                    width = 12
                  ),
                  
                  box( # age
                    fluidRow(
                      column(10, p(strong("Age"))),
                      column(2, actionButton("age_btn", icon('info'), style="background-color: #ADDFFF; border-color: #ADDFFF"), align = "right")
                    ),
                    p(id = "age_exp", age, align = "justify"),
                    width = 12
                  ),
                  
                  box( # gender
                    fluidRow(
                      column(10, p(strong("Sex"))),
                      column(2, actionButton("gender_btn", icon('info'), style="background-color: #ADDFFF; border-color: #ADDFFF"), align = "right")
                    ),
                    p(id = "gender_exp", gender, align = "justify"),
                    width = 12
                  ),
                  
                  box( # BMI
                    fluidRow(
                      column(10, p(strong("BMI"), ": Body Mass Index")),
                      column(2, actionButton("bmi_btn", icon('info'), style="background-color: #ADDFFF; border-color: #ADDFFF"), align = "right")
                    ),
                    p(id = "bmi_exp", bmi, align = "justify"),
                    width = 12
                  ),
                  
                  box( # Treatment group
                    fluidRow(
                      column(10, p(strong("Treatment group"))),
                      column(2, actionButton("tr_gr_btn", icon('info'), style="background-color: #ADDFFF; border-color: #ADDFFF"), align = "right")
                    ),
                    p(id = "tr_gr_exp", tr_gr, align = "justify"),
                    width = 12
                  ),
                  
                  box( # Concomitant medication
                    fluidRow(
                      column(10, p(strong("Concomitant medication"))),
                      column(2, actionButton("med_btn", icon('info'), style="background-color: #ADDFFF; border-color: #ADDFFF"), align = "right")
                    ),
                    p(id = "med_exp", med, align = "justify"),
                    width = 12
                  ),
                  
                  box( # MCAR
                    fluidRow(
                      column(10, p(strong("MCAR"), ": Missing Completely at Random")),
                      column(2, actionButton("mcar_btn", icon('info'), style="background-color: #ADDFFF; border-color: #ADDFFF"), align = "right")
                    ),
                    p(id = "mcar_exp", mcar, align = "justify"),
                    width = 12
                  ),
                  
                  box( # MAR
                    fluidRow(
                      column(10, p(strong("MAR"), ": Missing at Random")),
                      column(2, actionButton("mar_btn", icon('info'), style="background-color: #ADDFFF; border-color: #ADDFFF"), align = "right")
                    ),
                    p(id = "mar_exp", mar, align = "justify"),
                    width = 12
                  ),
                  
                  box( # MNAR
                    fluidRow(
                      column(10, p(strong("MNAR"), ": Missing not at Random")),
                      column(2, actionButton("mnar_btn", icon('info'), style="background-color: #ADDFFF; border-color: #ADDFFF"), align = "right")
                    ),
                    p(id = "mnar_exp", mnar, align = "justify"),
                    width = 12
                  ),
                  br(),
                  h4(strong('References')),
                  br(),
                  p(va_ref_1, align = "justify"),
                  p(va_ref_2, align = "justify"),
                  p(va_ref_3, align = "justify"),
                  p(va_ref_4, align = "justify"),
                  p(va_ref_5, align = "justify"),
                  p(va_ref_6, align = "justify"),
                  p(va_ref_7, align = "justify"),
                  p(va_ref_8, align = "justify"),
                  p(va_ref_9, align = "justify"),
                  p(va_ref_10, align = "justify"),
                  p(va_ref_11, align = "justify")
                  )
                  )
              ),
  
      # SYGEN DATA --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
      # About the Study ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
      tabItem(tabName = "info",
              fluidPage(
                titlePanel(h3(span(strong("About the Study"), style = "color:#000080"), align = "center")),
                br(),
                fluidRow(
                  summaryBox2("Patients", "797", width = 3, icon = "fas fa-users", style = "primary"),
                  summaryBox2("Centers", "28", width = 3, icon = "fas fa-earth-americas", style = "primary"),
                  summaryBox2("Study Period", "1992-1998", width = 3, icon = "fas fa-hourglass-end", style = "primary"),
                  summaryBox2("Follow-up", "1 Year", width = 3, icon = "fas fa-clock", style = "primary")
                ),
                br(),
                box(
                  fluidRow(
                    column(10, h4(strong("Study Design")),
                           p(about_p1, align = "justify")),
                    column(2, p(h1(span(icon("building-columns")), style = "color:#ADDFFF;font-size:60px")), align = "center")
                  ), width = 12),
                br(),
                box(
                  fluidRow(
                  column(10, h4(strong("Aim")),
                         p(about_p2, align = "justify")),
                  column(2, p(h1(span(icon("crosshairs"), style = "color:#ADDFFF;font-size:60px"))), align = "center")
                ), width = 12),
                br(),
                box(
                  fluidRow(
                    column(10, h4(strong("Patients")),
                           p(about_p3, align = "justify")),
                    column(2, p(h1(span(icon("users"), style = "color:#ADDFFF;font-size:60px"))), align = "center")
                  ), width = 12),
                br(),
                box(id = "my_tab_box",
                  fluidRow(
                    column(10, h4(strong("Initial Examination and Follow-Up Assessments")),
                           p(about_p4, align = "justify")),
                    column(2, p(h1(span(icon("calendar"), style = "color:#ADDFFF;font-size:60px"))), align = "center")
                  ),
                  fluidRow(column(12, imageOutput("timeline_sygen", height = "100px")), align = "center"), width = 12),
                br(),
                h4(strong('References')),
                br(),
                p(about_ref_1, align = "justify"),
                p(about_ref_2, align = "justify"),
                p(about_ref_3, align = "justify")
              )
      ),
    
      # Demographic Variables ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
      tabItem(tabName = "demo",
              fluidPage(
                titlePanel(h3(span(strong("Baseline Characteristics"), style = "color:#000080"), align = "center")),
                fluidRow(
                  br(),
                  p(dem_p1, align = "justify")),
                sidebarLayout(
                  sidebarPanel(id = 'sidebar',
                    useShinyjs(),
                    br(),
                    selectInput("out_dem", h4(strong("Oucome variable of interest")),
                                choices = list("LEMS" = 'lems', 
                                               "UEMS" = 'uems',
                                               "Light touch total score" = 'ltscore',
                                               "Pinprick total score" = 'ppscore',
                                               "Modified Benzel scale" = 'modben'), 
                                selected = 'lems'),
                    br(),
                    selectInput("main_dem", h4(strong("Main variable of interest")),
                                 choices = list("Age" = 'age', 
                                                "Sex" = 'sex', 
                                                "AIS grade" = 'severity', 
                                                "Neurological level of injury at baseline" = 'nli_gr', 
                                                "Spinal level" = 'level_gr', 
                                                "Concomitant medication (famotidine, ranitidine, cimetidine)" = 'medication',
                                                "Famotidine" = "famotidine",
                                                "Cimetidine" = "cimetidine",
                                                "Ranitidine" = "ranitidine",
                                                "Treatment group" = 'treatment_gr',
                                                "Weight status (~BMI)" = 'bmi_status'), 
                                 selected = 'age'),
                    br(),
                    sliderInput("main_med", h4(strong("Exposure period (days)")),
                                min = 0, max = 365, value = c(0, 6), step = 1),
                    br(),
                    selectInput("color_dem", h4(strong("Distinguish between")),
                                 choices = list("None" = 'none', 
                                                "Sex" = 'sex', 
                                                "AIS grade" = 'severity', 
                                                "Neurological level of injury at baseline" = 'nli_gr', 
                                                "Spinal level" = 'level_gr', 
                                                "Famotidine" = "famotidine",
                                                "Cimetidine" = "cimetidine",
                                                "Ranitidine" = "ranitidine",
                                                "Treatment group" = 'treatment_gr',
                                                "Weight status (~BMI)" = 'bmi_status'), 
                                 selected = 'none'),
                    br(),
                    sliderInput("color_med", h4(strong("Exposure period (days)")),
                                min = 0, max = 365, value = c(0, 6), step = 1),
                    br(),
                    selectInput("panel_dem", h4(strong("Further divide by")),
                                 choices = list("None" = 'none', 
                                                "Sex" = 'sex', 
                                                "AIS grade" = 'severity', 
                                                "Neurological level of injury at baseline" = 'nli_gr', 
                                                "Spinal level" = 'level_gr', 
                                                "Treatment group" = 'treatment_gr',
                                                "Famotidine" = "famotidine",
                                                "Cimetidine" = "cimetidine",
                                                "Ranitidine" = "ranitidine",
                                                "Weight status (~BMI)" = 'bmi_status'),
                                 selected = 'none'),
                    br(),
                    sliderInput("panel_med", h4(strong("Exposure period (days)")),
                                min = 0, max = 365, value = c(0, 6), step = 1),
                    ),
                  mainPanel(
                    tabsetPanel(id = "tab_dem",
                                tabPanel("Demographic Table", div(style = 'overflow-x: scroll', uiOutput("dem_tab"))),
                                tabPanel("Distribution Plot",plotOutput("dist_plot")),
                                tabPanel("Box Plot", plotOutput("box_plot")),
                                tabPanel("Bar Plot", plotOutput("bar_plot")),
                                tabPanel("Pie Chart", plotOutput("pie_chart")),
                                tabPanel("Upset Plot", plotOutput("upset_plot")),
                                tabPanel("Upset Plot Proportions", plotOutput("upset_plot_2"))
                    )
                  )
                )
              )
      ),
    
      # Outcome Variables -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
      tabItem(tabName = "outcome",
              fluidPage(
                titlePanel(h3(span(strong("Outcome Variables"), style = "color:#000080"), align = "center")),
                fluidRow(
                  br(),
                  p(out_p1, align = "justify")),
                sidebarLayout(
                  sidebarPanel(id = 'sidebar',
                               useShinyjs(),
                               selectInput("main_out", h4(strong("Oucome variable of interest")),
                                            choices = list("LEMS" = 'lems', 
                                                           "UEMS" = 'uems',
                                                           "Light touch total score" = 'ltscore',
                                                           "Pinprick total score" = 'ppscore',
                                                           "Modified Benzel scale" = 'modben'), 
                                            selected = 'lems'),
                               br(),
                               selectInput("color_out", h4(strong("Distinguish between")),
                                            choices = list("None" = 'none', 
                                                           "Sex" = 'sex', 
                                                           "Age range" = "age_range",
                                                           "AIS grade" = 'severity', 
                                                           "Neurological level of injury at baseline" = 'nli_gr', 
                                                           "Spinal level" = 'level_gr', 
                                                           "Famotidine" = "famotidine",
                                                           "Cimetidine" = "cimetidine",
                                                           "Ranitidine" = "ranitidine",
                                                           "Treatment group" = 'treatment_gr',
                                                           "Weight status (~BMI)" = 'bmi_status'), 
                                            selected = 'none'),
                               br(),
                               sliderInput("color_med_out", h4(strong("Exposure period (days)")),
                                           min = 0, max = 365, value = c(0, 6), step = 1),
                               br(),
                               selectInput("panel_out", h4(strong("Further divide by")),
                                            choices = list("None" = 'none', 
                                                           "Sex" = 'sex',  
                                                           "Age range" = "age_range",
                                                           "AIS grade" = 'severity', 
                                                           "Neurological level of injury at baseline" = 'nli_gr', 
                                                           "Spinal level" = 'level_gr', 
                                                           "Famotidine" = "famotidine",
                                                           "Cimetidine" = "cimetidine",
                                                           "Ranitidine" = "ranitidine",
                                                           "Treatment group" = 'treatment_gr',
                                                           "Weight status (~BMI)" = 'bmi_status'),
                                            selected = 'none'),
                               br(),
                               sliderInput("panel_med_out", h4(strong("Exposure period (days)")),
                                           min = 0, max = 365, value = c(0, 6), step = 1),
                  ),
                  mainPanel(
                    tabsetPanel(id = "tab_out",
                                #tabPanel("Demographic Table", uiOutput("out_tab")),
                                tabPanel("Spaghetti Plot",plotOutput("spaghetti_plot")),
                                tabPanel("Line Plot",plotOutput("line_plot")),
                                tabPanel("Box Plot", plotOutput("box_plot_out"))
                    )
                  )
                )
              )
      ),
    
      # Missing Data ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
      tabItem(tabName = "missing",
              fluidPage(
                titlePanel(h3(span(strong("Missing Data"), style = "color:#000080"), align = "center")),
                fluidRow(
                  br(),
                  p(na_p1, align = "justify")),
                sidebarLayout(
                  sidebarPanel(id = 'sidebar',
                               useShinyjs(),
                               selectInput("out_na", h4(strong("Oucome variable(s) of interest")),
                                                  choices = list("LEMS" = 'lems', 
                                                                 "UEMS" = 'uems',
                                                                 "Light touch total score" = 'ltscore',
                                                                 "Pinprick total score" = 'ppscore',
                                                                 "Modified Benzel scale" = 'modben'), 
                                                  selected = 'lems'),
                               br(),
                               radioButtons("out_type", h4(strong("How to look at the outcome")),
                                            choices = list("Week per week" = 'weekly', 
                                                           "All together" = 'overall'), 
                                            selected = 'weekly'),
                               br(),
                               checkboxGroupInput("dem_na", h4(strong("Baseline characteristic(s) of interest")),
                                                  choices = list("Sex" = 'sex', 
                                                                 "Age" = "age",
                                                                 "AIS grade" = 'severity', 
                                                                 "Neurological level of injury at baseline" = 'nli_gr', 
                                                                 "Spinal level" = 'level_gr', 
                                                                 "Famotidine" = "famotidine",
                                                                 "Cimetidine" = "cimetidine",
                                                                 "Ranitidine" = "ranitidine",
                                                                 "Treatment group" = 'treatment_gr',
                                                                 "Weight status (~BMI)" = 'bmi_status'), 
                                                  selected = NULL),
                               br(),
                               sliderInput("fam_exposure", h4(strong("Exposure period to famotidine (days)")),
                                           min = 0, max = 365, value = c(0, 6), step = 1),
                               br(),
                               sliderInput("ran_exposure", h4(strong("Exposure period to ranitidine (days)")),
                                           min = 0, max = 365, value = c(0, 6), step = 1),
                               br(),
                               sliderInput("cim_exposure", h4(strong("Exposure period to cimetidine (days)")),
                                           min = 0, max = 365, value = c(0, 6), step = 1)
                  ),
                  mainPanel(
                    tabsetPanel(id = "tab_na",
                                tabPanel("Upset Plot", plotOutput("upset_na"),
                                         br(),
                                         p(strong("Interpretation"), ":", upset_na_int, align = "justify")),
                                tabPanel("Pairs Plot", 
                                         radioButtons("prop_or_counts", " ",
                                                      choices = list("Counts" = 'counts', 
                                                                     "Proportions" = 'prop'), 
                                                      selected = 'counts'),
                                         plotOutput("pairs_na"),
                                         br(),
                                         p(strong("Interpretation"), ":", pairs_na_int, align = "justify")),
                                tabPanel("Pattern Plot", 
                                         p(strong("Interpretation"), ":", pattern_na_int, align = "justify"),
                                         p(strong("Reference"), ":", ref_pattern, align = "justify"),
                                         br(),
                                         imageOutput("pattern_na"))
                    )
                  )
                )
              )
      ),
  
      # CLUSTERING --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
      # About the Methods -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
      tabItem(tabName = "info-methods",
              fluidPage(
                titlePanel(h3(span(strong("About the Methods"), style = "color:#000080"), align = "center")),
                fluidRow(
                  br(),
                  p(methods_p1, align = "justify"),
                  br(),
                         h4(p(strong("Methods")), align = 'justify')),
                br(),
                fluidRow(
                  column(6, 
                         box(id = 'kmeans-box', 
                             p(strong(em('k')), strong('-means clustering'), align = "center"),
                             p(kmeans_exp, align = 'justify'),
                             #p(imageOutput('kmeans_img', height = "150px"), align = 'center'),
                             width = 12)),
                  column(6, 
                         box(id = 'lpa-box', 
                             p(strong("Latent profile analysis (LPA)"), align = "center"),
                             p(lpa_exp, align = 'justify'),
                             #p(imageOutput('lpa_img', height = "150px"), align = 'center'),
                             width = 12))
                         ),
                fluidRow(
                  br(),
                  h4(p(strong("References"), align = 'justify')),
                  br(),
                  p(meth_ref1, align = 'justify'),
                  p(meth_ref2, align = 'justify')
                )
              )
      ),
    
      # Method Comparison -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
      tabItem(tabName = "results",
              fluidPage(
                titlePanel(h3(span(strong("Method Comparison"), style = "color:#000080"), align = "center")),
                fluidRow(br(),
                         p(comp_p1, align = "justify")),
                sidebarLayout(
                  sidebarPanel(id = 'sidebar',
                               useShinyjs(),
                               selectInput("res_outcome", h4(strong("Outcome variable of interest")),
                                            choices = list("LEMS" = 'lems', 
                                                           "UEMS" = 'uems',
                                                           "Light touch total score" = 'ltscore',
                                                           "Pinprick total score" = 'ppscore',
                                                           "Modified Benzel scale" = 'modben'), 
                                            selected = 'lems'),
                               br(),
                               sliderInput("res_nb_cl", h4(strong("Number of clusters")),
                                           min = 1, max = 6, value = 3, step = 1),
                               br(),
                               selectInput("res_method", h4(strong("Clustering method")),
                                            choices = list("K-means" = 'kmeans', 
                                                           "Latent profile analysis (LPA)" = 'lpa'),
                                            selected = 'kmeans'),
                               br(),
                               selectInput("res_what_to_visualize", h4(strong("What to visualize")),
                                           choices = list("Overall results" = 'all', 
                                                          "Results for individual imputation datasets" = 'imp'),
                                           selected = 'all')
                  ),
                  mainPanel(
                    tabsetPanel(id = "tab_res",
                                tabPanel("Trajectories", 
                                         fluidRow(
                                           column(6, checkboxGroupInput("res_show", strong("Show"),
                                                                        choices = list("Individual trajectories" = 'single_t', 
                                                                                       "Mean trajectories" = 'mean_t',
                                                                                       "Mean ± standard error bands" = 'se_t'),
                                                                        selected = c('single_t', 'mean_t'))),
                                           column(6, strong("Cluster distribution"), tableOutput("cl_dist"))),
                                         strong("Plot"), plotOutput("spaghetti_res"),
                                         br(),
                                         #textOutput('empty_overall'),
                                         textOutput('empty_imp'),
                                         textOutput('consistency')),
                                tabPanel("Evaluation", 
                                         fluidRow(
                                           column(4,
                                                  selectInput("res_eval_criterion", strong("Criterion"),
                                                               choices = list("Calinski and Harabatz criterion" = 'Calinski.Harabatz',
                                                                              "Calinski and Harabatz criterion modified by Krysczuk" = "Calinski.Harabatz2",
                                                                              "Calinski and Harabatz criterion modified by Genolini" = "Calinski.Harabatz3",
                                                                              "Bayesian Information Criterion (based on the number of individuals)" = "BIC",
                                                                              "Bayesian Information Criterion (based on the number of measurements)" = "BIC2",
                                                                              "Akaike Information Criterion" = "AIC",
                                                                              "Akaike Information Criterion with correction (based on the number of individuals)" = "AICc",
                                                                              "Akaike Information Criterion with correction (based on the number of measurements)" = "AICc2",
                                                                              "Global posterior probability" = "postProbaGlobal",
                                                                              "Ray and Turi criterion" = "Ray.Turi",
                                                                              "Davies and Bouldin criterion" = "Davies.Bouldin"),
                                                               selected = 'Calinski.Harabatz'),
                                                  br(),
                                                  br(),
                                                  br(),
                                                  br(),
                                                  br(),
                                                  br(),
                                                  br(),
                                                  p("The evaluation index for the selected method is shown in red, the one for the other method is shown in grey.", align = 'justify')),
                                           column (8, plotOutput("ev_plot_res")),
                                           br(),
                                           box(id = "info_box_crit",
                                               fluidRow(column(6,
                                                               p(strong("Information on the criterion")),
                                                               p(span(textOutput("ev_criteria_exp"), style = "font-style:italic"), align = "justify"),
                                                               br(),
                                                               p("Notation:"),
                                                               p(em("k"), " = Number of clusters"), 
                                                               p(em("n"), " = Number of individuals/ trajectories"),
                                                               p(em("t"), " = Number of time measurement"),
                                                               p(em("N"), " = ", em("n•t"), " = Number of measurements"),
                                                               p(em("B"), " = Between variance"),
                                                               p(em("W"), " = Within variance"),
                                                               p(em("L"), " = Likelihood"),
                                                               p(em("h"), " = Number of parameters")),
                                                        column(6, p(strong("Important note"), imp_note, align = 'justify'))),
                                               width = 12)
                                         )
                                         )
                    )
                  )
                )
              )
      ),
      
      
      # Patient and Injury characteristics --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
      tabItem(tabName = "char_cluster",
              fluidPage(
                titlePanel(h3(span(strong("Patient and Injury Characteristics"), style = "color:#000080"), align = "center")),
                fluidRow(br(),
                         p(char_p1, align = "justify")),
                box(id = 'sidebar',
                    h4(strong("Method settings")),
                    br(),
                    fluidRow(
                      useShinyjs(),
                      column(6, 
                             selectInput("char_outcome", strong("Oucome variable of interest"),
                                          choices = list("LEMS" = 'lems', 
                                                         "UEMS" = 'uems',
                                                         "Light touch total score" = 'ltscore',
                                                         "Pinprick total score" = 'ppscore',
                                                         "Modified Benzel scale" = 'modben'), 
                                          selected = 'lems'),
                             br(),
                             sliderInput("char_nb_cl", strong("Number of clusters"),
                                         min = 1, max = 6, value = 3, step = 1),
                             br(),
                             selectInput("char_method", strong("Clustering method"),
                                          choices = list("K-means" = 'kmeans', 
                                                         "Latent profile analysis (LPA)" = 'lpa'),
                                          selected = 'kmeans')
                             ),
                      column(6, plotOutput("char_method_plot_cl"))
                    ), width = 12),
                sidebarLayout(
                  sidebarPanel(id = 'sidebar',
                               h4(strong("Characteristics settings")),
                               br(),
                               useShinyjs(),
                               selectInput("main_char", strong("Main variable of interest"),
                                            choices = list("Age" = 'age', 
                                                           "Sex" = 'sex', 
                                                           "AIS grade" = 'severity', 
                                                           "Neurological level of injury at baseline" = 'nli_gr', 
                                                           "Treatment group" = 'treatment_gr',
                                                           "Weight status (~BMI)" = 'bmi_status'), 
                                            selected = 'age'),
                               br(),
                               sliderInput("main_med_char", h4(strong("Exposure period (days)")),
                                           min = 0, max = 365, value = c(0, 6), step = 1),
                               br(),
                               selectInput("panel_char", strong("Distinguish between"),
                                            choices = list("None" = 'none', 
                                                           "Sex" = 'sex', 
                                                           "AIS grade" = 'severity', 
                                                           "Neurological level of injury at baseline" = 'nli_gr', 
                                                           "Treatment group" = 'treatment_gr',
                                                           "Weight status (~BMI)" = 'bmi_status'), 
                                            selected = 'none'),
                               br(),
                               sliderInput("panel_med_char", strong("Exposure period (days)"),
                                           min = 0, max = 365, value = c(0, 6), step = 1)#,
                               #br(),
                               #selectInput("char_what_to_visualize", strong("What to visualize"),
                               #            choices = list("Overall results" = 'all', 
                               #                           "Results for individual imputation datasets" = 'imp'),
                               #            selected = 'all'),
                               #br(),
                               #sliderInput("char_imp_dat", strong("Imputation dataset"),
                               #            min = 1, max = 20, value = 1, step = 1)
                  ),
                  mainPanel(
                    tabsetPanel(id = "tab_char",
                                tabPanel("Demographic Table", div(style = 'overflow-x: scroll', uiOutput("char_tab"))),
                                tabPanel("Distribution Plot",plotOutput("dist_plot_char")),
                                tabPanel("Box Plot", plotOutput("box_plot_char")),
                                tabPanel("Bar Plot", plotOutput("bar_plot_char")),
                                tabPanel("Pie Chart", plotOutput("pie_chart_char")),
                                tabPanel("Upset Plot", plotOutput("upset_plot_char")),
                                tabPanel("Upset Plot Proportions", plotOutput("upset_plot_2_char")),
                                tabPanel("Descriptive Statistics",
                                         selectInput("p_value", strong("Significance level"),
                                                     choices = list('0.1' = 0.1,
                                                                    '0.05' = 0.05,
                                                                    '0.01' = 0.01,
                                                                    '0.001' = 0.001), selected = 0.05),
                                         plotOutput("descr_plot"),
                                         br(),
                                         p(strong("Note: "), "In the chi-square tests p-values are missing if the number of clusters was not consistent across the different imputation datasets.")),
                                tabPanel("Multinomial Regression",
                                         fluidRow(column(9, plotOutput("probs_plot")),
                                                  column(3,
                                                         p(strong("Levels for prediction")),
                                                         br(),
                                                         sliderInput("age_slider", "Age",
                                                                     min = 10, max = 70, value = 30, step = 20),
                                                         br(),
                                                         selectInput("sex_input", "Sex",
                                                                     choices = list('Male' = 'M',
                                                                                    'Female' = 'F'), selected = 'M'),
                                                         br(),
                                                         selectInput("severity_input", "AIS grade",
                                                                     choices = list('A' = 'A',
                                                                                    'B' = 'B',
                                                                                    'C' = 'C',
                                                                                    'D' = 'D'), selected = 'A'),
                                                         br(),
                                                         selectInput("nli_input", "Neurological level of injury at baseline",
                                                                     choices = list('C01 - C04' = 'C01 - C04',
                                                                                    'C05 - C08' = 'C05 - C08',
                                                                                    'T01 - T04' = 'T01 - T04',
                                                                                    'T05 - T08' = 'T05 - T08',
                                                                                    'T09 - T12' = 'T09 - T12'), selected = 'C01 - C04'),
                                                         br(),
                                                         selectInput("trgr_input", "Treatment group",
                                                                     choices = list('Placebo' = 'P',
                                                                                    'Dose 1' = 'D1',
                                                                                    'Dose 2' = 'D2'), selected = 'P'),
                                                         br(),
                                                         selectInput("bmi_input", "Weight status",
                                                                     choices = list('Healthy weight' = 'Healthy weight',
                                                                                    'Obesity' = 'Obesity',
                                                                                    'Overweight' = 'Overweight',
                                                                                    'Underweight' = 'Underweight'), selected = 'Healthy weight'))))
                    )
                  )
                )
              )
      ),
  
      
      
      # SENSITIVITY ANALYSIS ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
      # About the Sensitivity Analysis -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
      tabItem(tabName = "info-sens",
              fluidPage(
                titlePanel(h3(span(strong("About the Sensitivity Analysis"), style = "color:#000080"), align = "center")),
                fluidRow(
                  br(),
                  p(sens_p1, align = "justify"),
                  br(),
                  h4(strong('Context'), align = 'justify'),
                  br(),
                  p(sens_p2, align = "justify"),
                  br(),
                  h4(strong('Objective'), align = 'justify'),
                  br(),
                  p(sens_p3, align = "justify"),
                  br(),
                  h4(strong('Approach'), align = 'justify'),
                  br(),
                  p(sens_p4, align = "justify"),
                  p(sens_p5, align = "justify"),
                  p(sens_p6, align = "justify"),
                  br(),
                  fluidRow(column(3),
                           column(6, imageOutput('sens_img')),
                           column(3))
                  )
              )
      ),
      
      # Clustering Results -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
      tabItem(tabName = "clust_sens",
              fluidPage(
                titlePanel(h3(span(strong("Clustering Results"), style = "color:#000080"), align = "center")),
                fluidRow(br(),
                         p(sens_comp_p1, align = "justify")),
                sidebarLayout(
                  sidebarPanel(id = 'sidebar',
                               useShinyjs(),
                               selectInput("sens_res_outcome", h4(strong("Outcome variable of interest")),
                                           choices = list("LEMS" = 'lems', 
                                                          "UEMS" = 'uems',
                                                          "Light touch total score" = 'ltscore',
                                                          "Pinprick total score" = 'ppscore',
                                                          "Modified Benzel scale" = 'modben'), 
                                           selected = 'lems'),
                               br(),
                               sliderInput("sens_res_nb_cl", h4(strong("Number of clusters")),
                                           min = 1, max = 6, value = 3, step = 1),
                               br(),
                               selectInput("sens_res_method", h4(strong("Clustering method")),
                                           choices = list("K-means" = 'kmeans', 
                                                          "Latent profile analysis (LPA)" = 'lpa'),
                                           selected = 'kmeans')
                  ),
                  mainPanel(
                    tabsetPanel(id = "sens_tab_res",
                                tabPanel("Trajectories", 
                                         fluidRow(
                                           column(4, checkboxGroupInput("sens_res_show", strong("Show"),
                                                                        choices = list("Mean trajectories" = 'mean_t',
                                                                                       "Mean ± standard error bands" = 'se_t'),
                                                                        selected = c('mean_t', 'se_t'))),
                                           column(8, strong("Cluster distribution"), tableOutput("sens_cl_dist"))),
                                         strong("Plot"), plotOutput("sens_spaghetti_res"),
                                         br(),
                                         textOutput('sens_empty_full'),
                                         textOutput('sens_empty_imp'),
                                         textOutput('sens_consistency')),
                                tabPanel("Evaluation", 
                                         fluidRow(
                                           column(4,
                                                  selectInput("sens_res_eval_criterion", strong("Criterion"),
                                                              choices = list("Calinski and Harabatz criterion" = 'Calinski.Harabatz',
                                                                             "Calinski and Harabatz criterion modified by Krysczuk" = "Calinski.Harabatz2",
                                                                             "Calinski and Harabatz criterion modified by Genolini" = "Calinski.Harabatz3",
                                                                             "Bayesian Information Criterion (based on the number of individuals)" = "BIC",
                                                                             "Bayesian Information Criterion (based on the number of measurements)" = "BIC2",
                                                                             "Akaike Information Criterion" = "AIC",
                                                                             "Akaike Information Criterion with correction (based on the number of individuals)" = "AICc",
                                                                             "Akaike Information Criterion with correction (based on the number of measurements)" = "AICc2",
                                                                             "Global posterior probability" = "postProbaGlobal",
                                                                             "Ray and Turi criterion" = "Ray.Turi",
                                                                             "Davies and Bouldin criterion" = "Davies.Bouldin"),
                                                              selected = 'Calinski.Harabatz'),
                                                  br(),
                                                  br(),
                                                  br(),
                                                  br(),
                                                  br(),
                                                  br(),
                                                  br(),
                                                  p("The evaluation index for the selected method is shown in red, the one for the other method is shown in grey.", align = 'justify')),
                                           column (8, plotOutput("sens_ev_plot_res"))
                                         )
                                )
                    )
                  )
                )
              )
      ),
      
      # Patient and Injury Characteristics -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
      tabItem(tabName = "char_sens",
              fluidPage(
                titlePanel(h3(span(strong("Patient and Injury Characteristics"), style = "color:#000080"), align = "center")),
                fluidRow(br(),
                         p(sens_char_p1, align = "justify")),
                box(id = 'sidebar',
                    h4(strong("Method settings")),
                    br(),
                    fluidRow(
                      useShinyjs(),
                      column(6, 
                             selectInput("sens_char_outcome", strong("Oucome variable of interest"),
                                         choices = list("LEMS" = 'lems', 
                                                        "UEMS" = 'uems',
                                                        "Light touch total score" = 'ltscore',
                                                        "Pinprick total score" = 'ppscore',
                                                        "Modified Benzel scale" = 'modben'), 
                                         selected = 'lems'),
                             br(),
                             sliderInput("sens_char_nb_cl", strong("Number of clusters"),
                                         min = 1, max = 6, value = 3, step = 1),
                             br(),
                             selectInput("sens_char_method", strong("Clustering method"),
                                         choices = list("K-means" = 'kmeans', 
                                                        "Latent profile analysis (LPA)" = 'lpa'),
                                         selected = 'kmeans')
                      ),
                      column(6, plotOutput("sens_char_method_plot_cl"))
                    ), width = 12),
                sidebarLayout(
                  sidebarPanel(id = 'sidebar',
                               h4(strong("Characteristics settings")),
                               br(),
                               useShinyjs(),
                               selectInput("sens_main_char", strong("Main variable of interest"),
                                           choices = list("Age" = 'age', 
                                                          "Sex" = 'sex', 
                                                          "AIS grade" = 'severity', 
                                                          "Neurological level of injury at baseline" = 'nli_gr', 
                                                          "Treatment group" = 'treatment_gr',
                                                          "Weight status (~BMI)" = 'bmi_status'), 
                                           selected = 'age'),
                               br(),
                               sliderInput("sens_main_med_char", h4(strong("Exposure period (days)")),
                                           min = 0, max = 365, value = c(0, 6), step = 1),
                               br(),
                               selectInput("sens_panel_char", strong("Distinguish between"),
                                           choices = list("None" = 'none', 
                                                          "Sex" = 'sex', 
                                                          "AIS grade" = 'severity', 
                                                          "Neurological level of injury at baseline" = 'nli_gr', 
                                                          "Treatment group" = 'treatment_gr',
                                                          "Weight status (~BMI)" = 'bmi_status'), 
                                           selected = 'none'),
                               br(),
                               sliderInput("sens_panel_med_char", strong("Exposure period (days)"),
                                           min = 0, max = 365, value = c(0, 6), step = 1)#,
                               #br(),
                               #selectInput("sens_char_what_to_visualize", strong("What to visualize"),
                               #            choices = list("Overall results" = 'all', 
                               #                           "Results for individual imputation datasets" = 'imp'),
                               #            selected = 'all'),
                               #br(),
                               #sliderInput("sens_char_imp_dat", strong("Imputation dataset"),
                               #            min = 1, max = 20, value = 1, step = 1)
                  ),
                  mainPanel(
                    tabsetPanel(id = "sens_tab_char",
                                tabPanel("Demographic Table", div(style = 'overflow-x: scroll', 
                                                                  fluidRow(column(6, p("Observed"), uiOutput("sens_char_tab_obs")),
                                                                           column(6, p("Imputed"), uiOutput("sens_char_tab_imp"))))),
                                tabPanel("Distribution Plot",
                                         fluidRow(column(6, p("Observed"), plotOutput("sens_dist_plot_char_obs")),
                                                  column(6, p("Imputed"), plotOutput("sens_dist_plot_char_imp")))),
                                tabPanel("Box Plot", 
                                         fluidRow(column(6, p("Observed"), plotOutput("sens_box_plot_char_obs")),
                                                  column(6, p("Imputed"), plotOutput("sens_box_plot_char_imp")))),
                                tabPanel("Bar Plot", fluidRow(column(6, p("Observed"), plotOutput("sens_bar_plot_char_obs")),
                                                              column(6, p("Imputed"), plotOutput("sens_bar_plot_char_imp")))),
                                tabPanel("Pie Chart", fluidRow(column(6, p("Observed"), plotOutput("sens_pie_chart_char_obs")),
                                                               column(6, p("Imputed"), plotOutput("sens_pie_chart_char_imp")))),
                                tabPanel("Upset Plot", fluidRow(column(6, p("Observed"), plotOutput("sens_upset_plot_char_obs")),
                                                                column(6, p("Imputed"), plotOutput("sens_upset_plot_char_imp")))),
                                tabPanel("Upset Plot Proportions", fluidRow(column(6, p("Observed"), plotOutput("sens_upset_plot_2_char_obs")),
                                                                            column(6, p("Imputed"), plotOutput("sens_upset_plot_2_char_imp")))),
                                tabPanel("Descriptive Statistics",
                                         selectInput("sens_p_value", strong("Significance level"),
                                                     choices = list('0.1' = 0.1,
                                                                    '0.05' = 0.05,
                                                                    '0.01' = 0.01,
                                                                    '0.001' = 0.001), selected = 0.05),
                                         plotOutput("sens_descr_plot"),
                                         br(),
                                         p(strong("Note: "), "In the chi-square tests p-values are missing if the number of clusters was not consistent across the different imputation datasets.")),
                                tabPanel("Multinomial Regression",
                                         fluidRow(column(9, plotOutput("sens_probs_plot")),
                                                  column(3,
                                                         p(strong("Levels for prediction")),
                                                         br(),
                                                         sliderInput("sens_age_slider", "Age",
                                                                     min = 10, max = 70, value = 30, step = 20),
                                                         br(),
                                                         selectInput("sens_sex_input", "Sex",
                                                                     choices = list('Male' = 'M',
                                                                                    'Female' = 'F'), selected = 'M'),
                                                         br(),
                                                         selectInput("sens_severity_input", "AIS grade",
                                                                     choices = list('A' = 'A',
                                                                                    'B' = 'B',
                                                                                    'C' = 'C',
                                                                                    'D' = 'D'), selected = 'A'),
                                                         br(),
                                                         selectInput("sens_nli_input", "Neurological level of injury at baseline",
                                                                     choices = list('C01 - C04' = 'C01 - C04',
                                                                                    'C05 - C08' = 'C05 - C08',
                                                                                    'T01 - T04' = 'T01 - T04',
                                                                                    'T05 - T08' = 'T05 - T08',
                                                                                    'T09 - T12' = 'T09 - T12'), selected = 'C01 - C04'),
                                                         br(),
                                                         selectInput("sens_trgr_input", "Treatment group",
                                                                     choices = list('Placebo' = 'P',
                                                                                    'Dose 1' = 'D1',
                                                                                    'Dose 2' = 'D2'), selected = 'P'),
                                                         br(),
                                                         selectInput("sens_bmi_input", "Weight status",
                                                                     choices = list('Healthy weight' = 'Healthy weight',
                                                                                    'Obesity' = 'Obesity',
                                                                                    'Overweight' = 'Overweight',
                                                                                    'Underweight' = 'Underweight'), selected = 'Healthy weight'))))
                    )
                  )
                )
              )
      )
    
    )  
  )
  
)

#------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# SERVER LOGIC
#------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

server <- function(input, output, session){
  thematic_shiny()
  useShinyjs()
  
  # HOME PAGE -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  output$ssf_logo <- renderImage({list(src = "SSFlogo.png", contentType = "image/png", width = "20%")}, deleteFile = FALSE)
  
  # ABBREVIATIONS AND DEFINITIONS ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  shinyjs::hide("lems_exp")
  observeEvent(input$lems_btn, {
    toggle("lems_exp")
  })
  
  shinyjs::hide("uems_exp")
  observeEvent(input$uems_btn, {
    toggle("uems_exp")
  })
  
  shinyjs::hide("tms_exp")
  observeEvent(input$tms_btn, {
    toggle("tms_exp")
  })
  
  shinyjs::hide("ss_exp")
  observeEvent(input$ss_btn, {
    toggle("ss_exp")
  })
  
  shinyjs::hide("ais_exp")
  observeEvent(input$ais_btn, {
    toggle("ais_exp")
  })
  #output$ais_table <- renderTable(
  #  if (input$ais_btn%%2 == 1){
  #    {ais_df}
  #  }
  #)
  
  shinyjs::hide("mbc_exp")
  observeEvent(input$mbc_btn, {
    toggle("mbc_exp")
  })
  #output$mbc_table <- renderTable(
  #  if (input$mbc_btn%%2 == 1){
  #    {mbc_df}
  #  }
  #)
  
  shinyjs::hide("nli_exp")
  observeEvent(input$nli_btn, {
    toggle("nli_exp")
  })
  
  shinyjs::hide("splvl_exp")
  observeEvent(input$splvl_btn, {
    toggle("splvl_exp")
  })
  
  shinyjs::hide("er_exp")
  observeEvent(input$er_btn, {
    toggle("er_exp")
  })
  
  shinyjs::hide("baseline_exp")
  observeEvent(input$baseline_btn, {
    toggle("baseline_exp")
  })
  
  shinyjs::hide("age_exp")
  observeEvent(input$age_btn, {
    toggle("age_exp")
  })
  
  shinyjs::hide("gender_exp")
  observeEvent(input$gender_btn, {
    toggle("gender_exp")
  })
  
  shinyjs::hide("bmi_exp")
  observeEvent(input$bmi_btn, {
    toggle("bmi_exp")
  })
  
  shinyjs::hide("tr_gr_exp")
  observeEvent(input$tr_gr_btn, {
    toggle("tr_gr_exp")
  })
  
  shinyjs::hide("med_exp")
  observeEvent(input$med_btn, {
    toggle("med_exp")
  })
  
  shinyjs::hide("mcar_exp")
  observeEvent(input$mcar_btn, {
    toggle("mcar_exp")
  })
  
  shinyjs::hide("mar_exp")
  observeEvent(input$mar_btn, {
    toggle("mar_exp")
  })
  
  shinyjs::hide("mnar_exp")
  observeEvent(input$mnar_btn, {
    toggle("mnar_exp")
  })
  
  # SYGEN DATA ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  # About the Study -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  output$timeline_sygen <- renderImage({list(src = "timeline_sygen.png", contentType = "image/png", width = "70%")}, deleteFile = FALSE)
  
  # Demographic Variables -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  hideTab(inputId = "tab_dem", target = "Distribution Plot")
  hideTab(inputId = "tab_dem", target = "Box Plot")
  hideTab(inputId = "tab_dem", target = "Bar Plot")
  hideTab(inputId = "tab_dem", target = "Pie Chart")
  hideTab(inputId = "tab_dem", target = "Upset Plot")
  hideTab(inputId = "tab_dem", target = "Upset Plot Proportions")
  
  observe(
    if (input$main_dem == 'age') {
    showTab(inputId = "tab_dem", target = "Distribution Plot")
    showTab(inputId = "tab_dem", target = "Box Plot")
    hideTab(inputId = "tab_dem", target = "Bar Plot")
    hideTab(inputId = "tab_dem", target = "Pie Chart")
    hideTab(inputId = "tab_dem", target = "Upset Plot")
    hideTab(inputId = "tab_dem", target = "Upset Plot Proportions")
  } else if (input$main_dem == 'medication') {
    hideTab(inputId = "tab_dem", target = "Distribution Plot")
    hideTab(inputId = "tab_dem", target = "Box Plot")
    hideTab(inputId = "tab_dem", target = "Bar Plot")
    hideTab(inputId = "tab_dem", target = "Pie Chart")
    showTab(inputId = "tab_dem", target = "Upset Plot")
    showTab(inputId = "tab_dem", target = "Upset Plot Proportions")
  } else {
    hideTab(inputId = "tab_dem", target = "Distribution Plot")
    hideTab(inputId = "tab_dem", target = "Box Plot")
    showTab(inputId = "tab_dem", target = "Bar Plot")
    showTab(inputId = "tab_dem", target = "Pie Chart")
    hideTab(inputId = "tab_dem", target = "Upset Plot")
    hideTab(inputId = "tab_dem", target = "Upset Plot Proportions")
  }
  )
  
  observe(
    if (class(createDemTable(data = getSubset(data = sygen_data, outcome_variable = input$out_dem, min_non_NA = 2), main = input$main_dem, color = input$color_dem, panel = input$panel_dem, exposure_main = input$main_med, exposure_color = input$color_med, exposure_panel = input$panel_med))[[1]] == "list"){
      output$dem_tab <- renderUI({
        lapply(names(createDemTable(data = getSubset(data = sygen_data, outcome_variable = input$out_dem, min_non_NA = 2), main = input$main_dem, color = input$color_dem, panel = input$panel_dem, exposure_main = input$main_med, exposure_color = input$color_med, exposure_panel = input$panel_med)), function(i) {
        tableOutput(paste0("dem_tab", i))
        })
      })
      i <- 1
      for (x in createDemTable(data = getSubset(data = sygen_data, outcome_variable = input$out_dem, min_non_NA = 2), main = input$main_dem, color = input$color_dem, panel = input$panel_dem, exposure_main = input$main_med, exposure_color = input$color_med, exposure_panel = input$panel_med)) {
        id <- paste0("dem_tab", names(createDemTable(data = getSubset(data = sygen_data, outcome_variable = input$out_dem, min_non_NA = 2), main = input$main_dem, color = input$color_dem, panel = input$panel_dem, exposure_main = input$main_med, exposure_color = input$color_med, exposure_panel = input$panel_med))[[i]])
        output[[id]] <- renderTable({x}, caption=names(createDemTable(data = getSubset(data = sygen_data, outcome_variable = input$out_dem, min_non_NA = 2), main = input$main_dem, color = input$color_dem, panel = input$panel_dem, exposure_main = input$main_med, exposure_color = input$color_med, exposure_panel = input$panel_med))[[i]], caption.placement = getOption("xtable.caption.placement", "top"), caption.width = getOption("xtable.caption.width", NULL)) 
        i <- i + 1
        }
      } else {
        output$dem_tab <- renderUI({tableOutput("dem_tab_unique")})
        output$dem_tab_unique <- renderTable({createDemTable(data = getSubset(data = sygen_data, outcome_variable = input$out_dem, min_non_NA = 2), main = input$main_dem, color = input$color_dem, panel = input$panel_dem, exposure_main = input$main_med, exposure_color = input$color_med, exposure_panel = input$panel_med)})
        }
  )
  
  shinyjs::hide('main_med')
  shinyjs::hide('color_med')
  shinyjs::hide('panel_med')
  observe({
    if (input$main_dem %in% c('medication', 'cimetidine', 'ranitidine', 'famotidine')) {
      shinyjs::show('main_med')
    } else {
      shinyjs::hide('main_med')
    }
    if (input$color_dem %in% c('medication', 'cimetidine', 'ranitidine', 'famotidine')) {
      shinyjs::show('color_med')
    } else {
      shinyjs::hide('color_med')
    }
    if (input$panel_dem %in% c('medication', 'cimetidine', 'ranitidine', 'famotidine')) {
      shinyjs::show('panel_med')
    } else {
      shinyjs::hide('panel_med')
    }
  })
  output$dist_plot <- renderPlot({customizedPlot(data = getSubset(data = sygen_data, outcome_variable = input$out_dem, min_non_NA = 2), main = input$main_dem, color = input$color_dem, panel = input$panel_dem, type = 'distribution', exposure_main = input$main_med, exposure_color = input$color_med, exposure_panel = input$panel_med)})
  output$box_plot <- renderPlot({customizedPlot(data = getSubset(data = sygen_data, outcome_variable = input$out_dem, min_non_NA = 2), main = input$main_dem, color = input$color_dem, panel = input$panel_dem, type = 'box', exposure_main = input$main_med, exposure_color = input$color_med, exposure_panel = input$panel_med)})
  output$bar_plot <- renderPlot({customizedPlot(data = getSubset(data = sygen_data, outcome_variable = input$out_dem, min_non_NA = 2), main = input$main_dem, color = input$color_dem, panel = input$panel_dem, type = 'bar', exposure_main = input$main_med, exposure_color = input$color_med, exposure_panel = input$panel_med)})
  output$pie_chart <- renderPlot({customizedPlot(data = getSubset(data = sygen_data, outcome_variable = input$out_dem, min_non_NA = 2), main = input$main_dem, color = input$color_dem, panel = input$panel_dem, type = 'pie', exposure_main = input$main_med, exposure_color = input$color_med, exposure_panel = input$panel_med)})
  output$upset_plot <- renderPlot({customizedPlot(data = getSubset(data = sygen_data, outcome_variable = input$out_dem, min_non_NA = 2), main = input$main_dem, color = input$color_dem, panel = input$panel_dem, type = 'upset', exposure_main = input$main_med, exposure_color = input$color_med, exposure_panel = input$panel_med)})
  output$upset_plot_2 <- renderPlot({customizedPlot(data = getSubset(data = sygen_data, outcome_variable = input$out_dem, min_non_NA = 2), main = input$main_dem, color = input$color_dem, panel = input$panel_dem, type = 'upset2', exposure_main = input$main_med, exposure_color = input$color_med, exposure_panel = input$panel_med)})
  
  # Outcome Variables ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  shinyjs::hide('color_med_out')
  shinyjs::hide('panel_med_out')
  observe({
    if (input$color_out %in% c('medication', 'cimetidine', 'ranitidine', 'famotidine')) {
      shinyjs::show('color_med_out')
    } else {
      shinyjs::hide('color_med_out')
    }
    if (input$panel_out %in% c('medication', 'cimetidine', 'ranitidine', 'famotidine')) {
      shinyjs::show('panel_med_out')
    } else {
      shinyjs::hide('panel_med_out')
    }
  })
  
  output$spaghetti_plot <- renderPlot({customizedPlot(data = getSubset(data = sygen_data, outcome_variable = input$main_out, min_non_NA = 2), main = input$main_out, color = input$color_out, panel = input$panel_out, type = 'spaghetti', exposure_main = input$main_med_out, exposure_color = input$color_med_out, exposure_panel = input$panel_med_out)})
  output$line_plot <- renderPlot({customizedPlot(data = getSubset(data = sygen_data, outcome_variable = input$main_out, min_non_NA = 2), main = input$main_out, color = input$color_out, panel = input$panel_out, type = 'line', exposure_main = input$main_med_out, exposure_color = input$color_med_out, exposure_panel = input$panel_med_out)})
  output$box_plot_out <- renderPlot({customizedPlot(data = getSubset(data = sygen_data, outcome_variable = input$main_out, min_non_NA = 2), main = input$main_out, color = input$color_out, panel = input$panel_out, type = 'box', exposure_main = input$main_med_out, exposure_color = input$color_med_out, exposure_panel = input$panel_med_out)})
  
  # Missing Data --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  observe({
    if (!is.null(input$out_na)){
      shinyjs::show("out_type")
    } else {
      shinyjs::hide("out_type")
    }
    
    if ('famotidine' %in% input$dem_na) {
      shinyjs::show('fam_exposure')
    } else {
      shinyjs::hide('fam_exposure')
    }
    
    if ('ranitidine' %in% input$dem_na) {
      shinyjs::show('ran_exposure')
    } else {
      shinyjs::hide('ran_exposure')
    }
    
    if ('cimetidine' %in% input$dem_na) {
      shinyjs::show('cim_exposure')
    } else {
      shinyjs::hide('cim_exposure')
    }
    
    output$upset_na <- renderPlot({plotMissingData(data = getSubset(data = sygen_data, outcome_variable = input$out_na, min_non_NA = 2), outcome = input$out_na, out_type = input$out_type, dem = input$dem_na, plot_type = 'upset', exposure_fam = input$fam_exposure, exposure_ran = input$ran_exposure, exposure_cim = input$cim_exposure)})
    output$pairs_na <- renderPlot({plotMissingData(data = getSubset(data = sygen_data, outcome_variable = input$out_na, min_non_NA = 2), outcome = input$out_na, out_type = input$out_type, dem = input$dem_na, plot_type = input$prop_or_counts, exposure_fam = input$fam_exposure, exposure_ran = input$ran_exposure, exposure_cim = input$cim_exposure)})
    output$pattern_na <- renderImage({
      png(filename="pattern.png", width = 600, height = 1000)
      plotMissingData(data = getSubset(data = sygen_data, outcome_variable = input$out_na, min_non_NA = 2), outcome = input$out_na, out_type = input$out_type, dem = input$dem_na, plot_type = 'pattern', exposure_fam = input$fam_exposure, exposure_ran = input$ran_exposure, exposure_cim = input$cim_exposure)
      dev.off()
      list(src = "pattern.png", contentType = "image/png", height = "100%")
      }, deleteFile = TRUE)
  })
  
  # CLUSTERING ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  # About the Methods ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  #output$kmeans_img <- renderImage({list(src = "kmeans-img.png", contentType = "image/png", width = "100%")}, deleteFile = FALSE)
  #output$lpa_img <- renderImage({list(src = "lpa-img.png", contentType = "image/png", width = "100%")}, deleteFile = FALSE)
  
  # Method Comparison ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  exp_list <- list('Calinski.Harabatz' = ch_exp,	
                   'Calinski.Harabatz2' = ch2_exp,	
                   'Calinski.Harabatz3' = ch3_exp,	
                   'BIC' = bic_exp,	
                   'BIC2' = bic2_exp,	
                   'AIC' = aic_exp,	
                   'AICc' = aicc_exp,	
                   'AICc2' = aicc2_exp,	
                   'postProbaGlobal' = ppc_exp,
                   'Ray.Turi' = rt_exp,
                   'Davies.Bouldin' = db_exp)

  shinyjs::hide("res_method")
  showTab(inputId = "tab_res", target = "Trajectories")
  hideTab(inputId = "tab_res", target = "Evaluation")
  observe({
    if (input$res_nb_cl == 1){
      shinyjs::hide("res_method")
      showTab(inputId = "tab_res", target = "Trajectories")
      hideTab(inputId = "tab_res", target = "Evaluation")
    } else {
      shinyjs::show("res_method")
      showTab(inputId = "tab_res", target = "Trajectories")
      showTab(inputId = "tab_res", target = "Evaluation")
    }
    output$spaghetti_res <- renderPlot({visualizeClusters(outcome_variable = input$res_outcome, nb_clusters = input$res_nb_cl, method = input$res_method, show = input$res_show, imp_or_all = input$res_what_to_visualize, labb = TRUE)}) 
    output$cl_dist <- renderTable({clusterDistribution(outcome_variable = input$res_outcome, nb_clusters = input$res_nb_cl, method = input$res_method, imp_or_all = input$res_what_to_visualize)})
    output$ev_plot_res <- renderPlot({plotEvaluation(outcome_variable = input$res_outcome, nb_clusters = input$res_nb_cl, method = input$res_method, criterion = input$res_eval_criterion, imp_or_all = input$res_what_to_visualize)})
    output$ev_criteria_exp <- renderText({exp_list[[input$res_eval_criterion]]})
    #output$empty_overall <- renderText({paste0("Empty clusters (overall): ", aboutClusters(outcome_variable = input$res_outcome, nb_clusters = input$res_nb_cl, method = input$res_method)[['empty_overall']])})
    output$empty_imp <- renderText({paste0("Empty clusters (in any of the imputed datasets): ", aboutClusters(outcome_variable = input$res_outcome, nb_clusters = input$res_nb_cl, method = input$res_method)[['empty_imp']])})
    output$consistency <- renderText({paste0("Number of clusters consistent across imputed datasets: ", aboutClusters(outcome_variable = input$res_outcome, nb_clusters = input$res_nb_cl, method = input$res_method)[['consistency']])})
  })
  
  
  # Patient and Injury Characteristics --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  hideTab(inputId = "tab_char", target = "Distribution Plot")
  hideTab(inputId = "tab_char", target = "Box Plot")
  hideTab(inputId = "tab_char", target = "Bar Plot")
  hideTab(inputId = "tab_char", target = "Pie Chart")
  hideTab(inputId = "tab_char", target = "Upset Plot")
  hideTab(inputId = "tab_char", target = "Upset Plot Proportions")
  hideTab(inputId = "tab_char", target = "Descriptive Statistics")
  hideTab(inputId = "tab_char", target = "Multinomial Regression")
  
  observe(
    if (input$main_char == 'age') {
      showTab(inputId = "tab_char", target = "Distribution Plot")
      showTab(inputId = "tab_char", target = "Box Plot")
      hideTab(inputId = "tab_char", target = "Bar Plot")
      hideTab(inputId = "tab_char", target = "Pie Chart")
      hideTab(inputId = "tab_char", target = "Upset Plot")
      hideTab(inputId = "tab_char", target = "Upset Plot Proportions")
    } else if (input$main_char == 'medication') {
      hideTab(inputId = "tab_char", target = "Distribution Plot")
      hideTab(inputId = "tab_char", target = "Box Plot")
      hideTab(inputId = "tab_char", target = "Bar Plot")
      hideTab(inputId = "tab_char", target = "Pie Chart")
      showTab(inputId = "tab_char", target = "Upset Plot")
      showTab(inputId = "tab_char", target = "Upset Plot Proportions")
    } else {
      hideTab(inputId = "tab_char", target = "Distribution Plot")
      hideTab(inputId = "tab_char", target = "Box Plot")
      showTab(inputId = "tab_char", target = "Bar Plot")
      showTab(inputId = "tab_char", target = "Pie Chart")
      hideTab(inputId = "tab_char", target = "Upset Plot")
      hideTab(inputId = "tab_char", target = "Upset Plot Proportions")
    }
  )
  
  observe(
    if (class(createDemTable(data = getDatasetWithClusters(outcome_variable = input$char_outcome, nb_clusters = input$char_nb_cl, method = input$char_method), main = input$main_char, color = 'cluster', panel = input$panel_char, exposure_main = input$main_med_char, exposure_color = NULL, exposure_panel = input$panel_med_char, imp_or_all = 'all', imp_data = NULL))[[1]] == "list"){
      output$char_tab <- renderUI({
        lapply(names(createDemTable(data = getDatasetWithClusters(outcome_variable = input$char_outcome, nb_clusters = input$char_nb_cl, method = input$char_method), main = input$main_char, color = 'cluster', panel = input$panel_char, exposure_main = input$main_med_char, exposure_color = NULL, exposure_panel = input$panel_med_char, imp_or_all = 'all', imp_data = NULL)), function(i) {
          tableOutput(paste0("char_tab", i))
        })
      })
      i <- 1
      for (x in createDemTable(data = getDatasetWithClusters(outcome_variable = input$char_outcome, nb_clusters = input$char_nb_cl, method = input$char_method), main = input$main_char, color = 'cluster', panel = input$panel_char, exposure_main = input$main_med_char, exposure_color = NULL, exposure_panel = input$panel_med_char, imp_or_all = 'all', imp_data = NULL)) {
        id <- paste0("char_tab", names(createDemTable(data = getDatasetWithClusters(outcome_variable = input$char_outcome, nb_clusters = input$char_nb_cl, method = input$char_method), main = input$main_char, color = 'cluster', panel = input$panel_char, exposure_main = input$main_med_char, exposure_color = NULL, exposure_panel = input$panel_med_char, imp_or_all = 'all', imp_data = NULL))[[i]])
        output[[id]] <- renderTable({x}, caption=names(createDemTable(data = getDatasetWithClusters(outcome_variable = input$char_outcome, nb_clusters = input$char_nb_cl, method = input$char_method), main = input$main_char, color = 'cluster', panel = input$panel_char, exposure_main = input$main_med_char, exposure_color = NULL, exposure_panel = input$panel_med_char, imp_or_all = 'all', imp_data = NULL))[[i]], caption.placement = getOption("xtable.caption.placement", "top"), caption.width = getOption("xtable.caption.width", NULL)) 
        i <- i + 1
      }
    } else {
      output$char_tab <- renderUI({tableOutput("char_tab_unique")})
      output$char_tab_unique <- renderTable({createDemTable(data = getDatasetWithClusters(outcome_variable = input$char_outcome, nb_clusters = input$char_nb_cl, method = input$char_method), main = input$main_char, color = 'cluster', panel = input$panel_char, exposure_main = input$main_med_char, exposure_color = NULL, exposure_panel = input$panel_med_char, imp_or_all = 'all', imp_data = NULL)})
    }
  )
  
  observe({
    if (input$char_nb_cl >= 2){
      showTab(inputId = "tab_char", target = "Descriptive Statistics")
      showTab(inputId = "tab_char", target = "Multinomial Regression")
      shinyjs::show("char_method")
      if (input$char_method == 'bma') {
        shinyjs::show("char_method_selection")
      } else {
        shinyjs::hide("char_method_selection")
      }
    } else {
      hideTab(inputId = "tab_char", target = "Descriptive Statistics")
      hideTab(inputId = "tab_char", target = "Multinomial Regression")
      shinyjs::hide("char_method")
      shinyjs::hide("char_method_selection")
    }
    
    observe({
      if (input$main_char %in% c('medication', 'cimetidine', 'ranitidine', 'famotidine')) {
        shinyjs::show('main_med_char')
      } else {
        shinyjs::hide('main_med_char')
      }
      if (input$panel_char %in% c('medication', 'cimetidine', 'ranitidine', 'famotidine')) {
        shinyjs::show('panel_med_char')
      } else {
        shinyjs::hide('panel_med_char')
      }
    })
    
    #observe({
    #  if (input$char_what_to_visualize == 'all'){
    #    shinyjs::hide('char_imp_dat')
    #  } else {
    #    shinyjs::show('char_imp_dat')
    #  }
    #})
    
    output$char_method_plot_cl <- renderPlot({visualizeClusters(outcome_variable = input$char_outcome, nb_clusters = input$char_nb_cl, method = input$char_method, show = c('mean_t', 'se_t'), labb = TRUE, imp_or_all = 'all', imp_data = NULL)})
    output$dist_plot_char <- renderPlot({customizedPlot(data = getDatasetWithClusters(outcome_variable = input$char_outcome, nb_clusters = input$char_nb_cl, method = input$char_method), main = input$main_char, color = 'cluster', panel = input$panel_char, type = 'distribution', exposure_main = input$main_med_char, exposure_color = NULL, exposure_panel = input$panel_med_char, imp_or_all = 'all', imp_data = NULL)})
    output$box_plot_char <- renderPlot({customizedPlot(data = getDatasetWithClusters(outcome_variable = input$char_outcome, nb_clusters = input$char_nb_cl, method = input$char_method), main = input$main_char, color = 'cluster', panel = input$panel_char, type = 'box', exposure_main = input$main_med_char, exposure_color = NULL, exposure_panel = input$panel_med_char, imp_or_all = 'all', imp_data = NULL)})
    output$bar_plot_char <- renderPlot({customizedPlot(data = getDatasetWithClusters(outcome_variable = input$char_outcome, nb_clusters = input$char_nb_cl, method = input$char_method), main = input$main_char, color = 'cluster', panel = input$panel_char, type = 'bar', exposure_main = input$main_med_char, exposure_color = NULL, exposure_panel = input$panel_med_char, imp_or_all = 'all', imp_data = NULL)})
    output$pie_chart_char <- renderPlot({customizedPlot(data = getDatasetWithClusters(outcome_variable = input$char_outcome, nb_clusters = input$char_nb_cl, method = input$char_method), main = input$main_char, color = 'cluster', panel = input$panel_char, type = 'pie', exposure_main = input$main_med_char, exposure_color = NULL, exposure_panel = input$panel_med_char, imp_or_all = 'all', imp_data = NULL)})
    output$upset_plot_char <- renderPlot({customizedPlot(data = getDatasetWithClusters(outcome_variable = input$char_outcome, nb_clusters = input$char_nb_cl, method = input$char_method), main = input$main_char, color = 'cluster', panel = input$panel_char, type = 'upset', exposure_main = input$main_med_char, exposure_color = NULL, exposure_panel = input$panel_med_char, imp_or_all = 'all', imp_data = NULL)})
    output$upset_plot_2_char <- renderPlot({customizedPlot(data = getDatasetWithClusters(outcome_variable = input$char_outcome, nb_clusters = input$char_nb_cl, method = input$char_method), main = input$main_char, color = 'cluster', panel = input$panel_char, type = 'upset2', exposure_main = input$main_med_char, exposure_color = NULL, exposure_panel = input$panel_med_char, imp_or_all = 'all', imp_data = NULL)})
    output$descr_plot <- renderPlot({descriptiveStastsP(outcome_variable = input$char_outcome, explanatory_variable = input$main_char, nb_clusters = input$char_nb_cl, method = input$char_method, p_value = input$p_value)})
    output$probs_plot <- renderPlot({multinomProbs(outcome_variable  = input$char_outcome, explanatory_variable1  = input$main_char, explanatory_variable2  = input$panel_char, nb_clusters = input$char_nb_cl, method = input$char_method, levels = c('age' = input$age_slider, 'sex' = input$sex_input, 'severity' = input$severity_input, 'nli_gr' = input$nli_input, 'treatment_gr' = input$trgr_input, 'bmi_status' = input$bmi_input))})
  })
  
  # SENSITIVITY ANALYSIS ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  # About the Sensitivity Analysis --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  output$sens_img <- renderImage({list(src = "sens-img-2.png", contentType = "image/png", width = "100%")}, deleteFile = FALSE)
  
  # Clustering Results --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  shinyjs::hide("sens_res_method")
  showTab(inputId = "sens_tab_res", target = "Trajectories")
  hideTab(inputId = "sens_tab_res", target = "Evaluation")
  observe({
    if (input$res_nb_cl == 1){
      shinyjs::hide("sens_res_method")
      showTab(inputId = "sens_tab_res", target = "Trajectories")
      hideTab(inputId = "sens_tab_res", target = "Evaluation")
    } else {
      shinyjs::show("sens_res_method")
      showTab(inputId = "sens_tab_res", target = "Trajectories")
      showTab(inputId = "sens_tab_res", target = "Evaluation")
    }
    output$sens_spaghetti_res <- renderPlot({visualizeClustersSens(outcome_variable = input$sens_res_outcome, nb_clusters = input$sens_res_nb_cl, method = input$sens_res_method, show = input$sens_res_show, labb = TRUE)}) 
    output$sens_cl_dist <- renderTable({clusterDistributionSens(outcome_variable = input$sens_res_outcome, nb_clusters = input$sens_res_nb_cl, method = input$sens_res_method)})
    output$sens_ev_plot_res <- renderPlot({plotEvaluationSens(outcome_variable = input$sens_res_outcome, nb_clusters = input$sens_res_nb_cl, method = input$sens_res_method, criterion = input$sens_res_eval_criterion)})
    output$sens_empty_full <- renderText({paste0("Empty clusters (in the analysis with the full dataset): ", aboutClustersSens(outcome_variable = input$sens_res_outcome, nb_clusters = input$sens_res_nb_cl, method = input$sens_res_method)[['empty_full']])})
    output$sens_empty_imp <- renderText({paste0("Empty clusters (in any of the imputed datasets): ", aboutClustersSens(outcome_variable = input$sens_res_outcome, nb_clusters = input$sens_res_nb_cl, method = input$sens_res_method)[['empty_imp']])})
    output$sens_consistency <- renderText({paste0("Number of clusters consistent across imputed datasets: ", aboutClustersSens(outcome_variable = input$sens_res_outcome, nb_clusters = input$sens_res_nb_cl, method = input$sens_res_method)[['consistency_imp']])})
  })
  
  # Patient and Injury Characteristics ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  hideTab(inputId = "sens_tab_char", target = "Distribution Plot")
  hideTab(inputId = "sens_tab_char", target = "Box Plot")
  hideTab(inputId = "sens_tab_char", target = "Bar Plot")
  hideTab(inputId = "sens_tab_char", target = "Pie Chart")
  hideTab(inputId = "sens_tab_char", target = "Upset Plot")
  hideTab(inputId = "sens_tab_char", target = "Upset Plot Proportions")
  hideTab(inputId = "sens_tab_char", target = "Descriptive Statistics")
  hideTab(inputId = "sens_tab_char", target = "Multinomial Regression")
  
  observe(
    if (input$sens_main_char == 'age') {
      showTab(inputId = "sens_tab_char", target = "Distribution Plot")
      showTab(inputId = "sens_tab_char", target = "Box Plot")
      hideTab(inputId = "sens_tab_char", target = "Bar Plot")
      hideTab(inputId = "sens_tab_char", target = "Pie Chart")
      hideTab(inputId = "sens_tab_char", target = "Upset Plot")
      hideTab(inputId = "sens_tab_char", target = "Upset Plot Proportions")
    } else if (input$sens_main_char == 'medication') {
      hideTab(inputId = "sens_tab_char", target = "Distribution Plot")
      hideTab(inputId = "sens_tab_char", target = "Box Plot")
      hideTab(inputId = "sens_tab_char", target = "Bar Plot")
      hideTab(inputId = "sens_tab_char", target = "Pie Chart")
      showTab(inputId = "sens_tab_char", target = "Upset Plot")
      showTab(inputId = "sens_tab_char", target = "Upset Plot Proportions")
    } else {
      hideTab(inputId = "sens_tab_char", target = "Distribution Plot")
      hideTab(inputId = "sens_tab_char", target = "Box Plot")
      showTab(inputId = "sens_tab_char", target = "Bar Plot")
      showTab(inputId = "sens_tab_char", target = "Pie Chart")
      hideTab(inputId = "sens_tab_char", target = "Upset Plot")
      hideTab(inputId = "sens_tab_char", target = "Upset Plot Proportions")
    }
  )
  
  observe( # obs table
    if (class(createDemTable(data = getDatasetWithClustersSens(outcome_variable = input$sens_char_outcome, nb_clusters = input$sens_char_nb_cl, method = input$sens_char_method, obs_or_imp = 'obs'), main = input$sens_main_char, color = 'cluster', panel = input$sens_panel_char, exposure_main = input$sens_main_med_char, exposure_color = NULL, exposure_panel = input$sens_panel_med_char, imp_or_all = NULL, imp_data = NULL))[[1]] == "list"){
      output$sens_char_tab_obs <- renderUI({
        lapply(names(createDemTable(data = getDatasetWithClustersSens(outcome_variable = input$sens_char_outcome, nb_clusters = input$sens_char_nb_cl, method = input$sens_char_method, obs_or_imp = 'obs'), main = input$sens_main_char, color = 'cluster', panel = input$sens_panel_char, exposure_main = input$sens_main_med_char, exposure_color = NULL, exposure_panel = input$sens_panel_med_char, imp_or_all = NULL, imp_data = NULL)), function(i) {
          tableOutput(paste0("sens_char_tab_obs", i))
        })
      })
      i <- 1
      for (x in createDemTable(data = getDatasetWithClustersSens(outcome_variable = input$sens_char_outcome, nb_clusters = input$sens_char_nb_cl, method = input$sens_char_method, obs_or_imp = 'obs'), main = input$sens_main_char, color = 'cluster', panel = input$sens_panel_char, exposure_main = input$sens_main_med_char, exposure_color = NULL, exposure_panel = input$sens_panel_med_char, imp_or_all = NULL, imp_data = NULL)) {
        id <- paste0("sens_char_tab_obs", names(createDemTable(data = getDatasetWithClustersSens(outcome_variable = input$sens_char_outcome, nb_clusters = input$sens_char_nb_cl, method = input$sens_char_method, obs_or_imp = 'obs'), main = input$sens_main_char, color = 'cluster', panel = input$sens_panel_char, exposure_main = input$sens_main_med_char, exposure_color = NULL, exposure_panel = input$sens_panel_med_char, imp_or_all = NULL, imp_data = NULL))[[i]])
        output[[id]] <- renderTable({x}, caption=names(createDemTable(data = getDatasetWithClustersSens(outcome_variable = input$sens_char_outcome, nb_clusters = input$sens_char_nb_cl, method = input$sens_char_method, obs_or_imp = 'obs'), main = input$sens_main_char, color = 'cluster', panel = input$sens_panel_char, exposure_main = input$sens_main_med_char, exposure_color = NULL, exposure_panel = input$sens_panel_med_char, imp_or_all = NULL, imp_data = NULL))[[i]], caption.placement = getOption("xtable.caption.placement", "top"), caption.width = getOption("xtable.caption.width", NULL)) 
        i <- i + 1
      }
    } else {
      output$sens_char_tab_obs <- renderUI({tableOutput("sens_char_tab_unique_obs")})
      output$sens_char_tab_unique_obs <- renderTable({createDemTable(data = getDatasetWithClustersSens(outcome_variable = input$sens_char_outcome, nb_clusters = input$sens_char_nb_cl, method = input$sens_char_method, obs_or_imp = 'obs'), main = input$sens_main_char, color = 'cluster', panel = input$sens_panel_char, exposure_main = input$sens_main_med_char, exposure_color = NULL, exposure_panel = input$sens_panel_med_char, imp_or_all = NULL, imp_data = NULL)})
    }
  )
  
  observe( # imp table
    if (class(createDemTable(data = getDatasetWithClustersSens(outcome_variable = input$sens_char_outcome, nb_clusters = input$sens_char_nb_cl, method = input$sens_char_method, obs_or_imp = 'imp'), main = input$sens_main_char, color = 'cluster', panel = input$sens_panel_char, exposure_main = input$sens_main_med_char, exposure_color = NULL, exposure_panel = input$sens_panel_med_char, imp_or_all = 'all', imp_data = NULL))[[1]] == "list"){
      output$sens_char_tab_imp <- renderUI({
        lapply(names(createDemTable(data = getDatasetWithClustersSens(outcome_variable = input$sens_char_outcome, nb_clusters = input$sens_char_nb_cl, method = input$sens_char_method, obs_or_imp = 'imp'), main = input$sens_main_char, color = 'cluster', panel = input$sens_panel_char, exposure_main = input$sens_main_med_char, exposure_color = NULL, exposure_panel = input$sens_panel_med_char, imp_or_all = 'all', imp_data = NULL)), function(i) {
          tableOutput(paste0("sens_char_tab_imp", i))
        })
      })
      i <- 1
      for (x in createDemTable(data = getDatasetWithClustersSens(outcome_variable = input$sens_char_outcome, nb_clusters = input$sens_char_nb_cl, method = input$sens_char_method, obs_or_imp = 'imp'), main = input$sens_main_char, color = 'cluster', panel = input$sens_panel_char, exposure_main = input$sens_main_med_char, exposure_color = NULL, exposure_panel = input$sens_panel_med_char, imp_or_all = 'all', imp_data = NULL)) {
        id <- paste0("sens_char_tab_imp", names(createDemTable(data = getDatasetWithClustersSens(outcome_variable = input$sens_char_outcome, nb_clusters = input$sens_char_nb_cl, method = input$sens_char_method, obs_or_imp = 'imp'), main = input$sens_main_char, color = 'cluster', panel = input$sens_panel_char, exposure_main = input$sens_main_med_char, exposure_color = NULL, exposure_panel = input$sens_panel_med_char, imp_or_all = 'all', imp_data = NULL))[[i]])
        output[[id]] <- renderTable({x}, caption=names(createDemTable(data = getDatasetWithClustersSens(outcome_variable = input$sens_char_outcome, nb_clusters = input$sens_char_nb_cl, method = input$sens_char_method, obs_or_imp = 'imp'), main = input$sens_main_char, color = 'cluster', panel = input$sens_panel_char, exposure_main = input$sens_main_med_char, exposure_color = NULL, exposure_panel = input$sens_panel_med_char, imp_or_all = 'all', imp_data = NULL))[[i]], caption.placement = getOption("xtable.caption.placement", "top"), caption.width = getOption("xtable.caption.width", NULL)) 
        i <- i + 1
      }
    } else {
      output$sens_char_tab_imp <- renderUI({tableOutput("sens_char_tab_unique_imp")})
      output$sens_char_tab_unique_imp <- renderTable({createDemTable(data = getDatasetWithClustersSens(outcome_variable = input$sens_char_outcome, nb_clusters = input$sens_char_nb_cl, method = input$sens_char_method, obs_or_imp = 'imp'), main = input$sens_main_char, color = 'cluster', panel = input$sens_panel_char, exposure_main = input$sens_main_med_char, exposure_color = NULL, exposure_panel = input$sens_panel_med_char, imp_or_all = 'all', imp_data = NULL)})
    }
  )
  
  observe({
    if (input$sens_char_nb_cl >= 2){
      showTab(inputId = "sens_tab_char", target = "Descriptive Statistics")
      showTab(inputId = "sens_tab_char", target = "Multinomial Regression")
      shinyjs::show("sens_char_method")
      if (input$sens_char_method == 'bma') {
        shinyjs::show("sens_char_method_selection")
      } else {
        shinyjs::hide("sens_char_method_selection")
      }
    } else {
      hideTab(inputId = "sens_tab_char", target = "Descriptive Statistics")
      hideTab(inputId = "sens_tab_char", target = "Multinomial Regression")
      shinyjs::hide("sens_char_method")
      shinyjs::hide("sens_char_method_selection")
    }
    
    observe({
      if (input$sens_main_char %in% c('medication', 'cimetidine', 'ranitidine', 'famotidine')) {
        shinyjs::show('sens_main_med_char')
      } else {
        shinyjs::hide('sens_main_med_char')
      }
      if (input$sens_panel_char %in% c('medication', 'cimetidine', 'ranitidine', 'famotidine')) {
        shinyjs::show('sens_panel_med_char')
      } else {
        shinyjs::hide('sens_panel_med_char')
      }
    })
    
    #observe({
    #  if (input$sens_char_what_to_visualize == 'all'){
    #    shinyjs::hide('sens_char_imp_dat')
    #  } else {
    #    shinyjs::show('sens_char_imp_dat')
    #  }
    #})
    
    output$sens_char_method_plot_cl <- renderPlot({visualizeClustersSens(outcome_variable = input$sens_char_outcome, nb_clusters = input$sens_char_nb_cl, method = input$sens_char_method, show = c('mean_t', 'se_t'), labb = TRUE)})
    output$sens_dist_plot_char_obs <- renderPlot({customizedPlot(data = getDatasetWithClustersSens(outcome_variable = input$sens_char_outcome, nb_clusters = input$sens_char_nb_cl, method = input$sens_char_method, obs_or_imp = 'obs'), main = input$sens_main_char, color = 'cluster', panel = input$sens_panel_char, type = 'distribution', exposure_main = input$sens_main_med_char, exposure_color = NULL, exposure_panel = input$sens_panel_med_char, imp_or_all = NULL, imp_data = NULL)})
    output$sens_dist_plot_char_imp <- renderPlot({customizedPlot(data = getDatasetWithClustersSens(outcome_variable = input$sens_char_outcome, nb_clusters = input$sens_char_nb_cl, method = input$sens_char_method, obs_or_imp = 'imp'), main = input$sens_main_char, color = 'cluster', panel = input$sens_panel_char, type = 'distribution', exposure_main = input$sens_main_med_char, exposure_color = NULL, exposure_panel = input$sens_panel_med_char, imp_or_all = 'all', imp_data = NULL)})
    output$sens_box_plot_char_obs <- renderPlot({customizedPlot(data = getDatasetWithClustersSens(outcome_variable = input$sens_char_outcome, nb_clusters = input$sens_char_nb_cl, method = input$sens_char_method, obs_or_imp = 'obs'), main = input$sens_main_char, color = 'cluster', panel = input$sens_panel_char, type = 'box', exposure_main = input$sens_main_med_char, exposure_color = NULL, exposure_panel = input$sens_panel_med_char, imp_or_all = NULL, imp_data = NULL)})
    output$sens_box_plot_char_imp <- renderPlot({customizedPlot(data = getDatasetWithClustersSens(outcome_variable = input$sens_char_outcome, nb_clusters = input$sens_char_nb_cl, method = input$sens_char_method, obs_or_imp = 'imp'), main = input$sens_main_char, color = 'cluster', panel = input$sens_panel_char, type = 'box', exposure_main = input$sens_main_med_char, exposure_color = NULL, exposure_panel = input$sens_panel_med_char, imp_or_all = 'all', imp_data = NULL)})
    output$sens_bar_plot_char_obs <- renderPlot({customizedPlot(data = getDatasetWithClustersSens(outcome_variable = input$sens_char_outcome, nb_clusters = input$sens_char_nb_cl, method = input$sens_char_method, obs_or_imp = 'obs'), main = input$sens_main_char, color = 'cluster', panel = input$sens_panel_char, type = 'bar', exposure_main = input$sens_main_med_char, exposure_color = NULL, exposure_panel = input$sens_panel_med_char, imp_or_all = NULL, imp_data = NULL)})
    output$sens_bar_plot_char_imp <- renderPlot({customizedPlot(data = getDatasetWithClustersSens(outcome_variable = input$sens_char_outcome, nb_clusters = input$sens_char_nb_cl, method = input$sens_char_method, obs_or_imp = 'imp'), main = input$sens_main_char, color = 'cluster', panel = input$sens_panel_char, type = 'bar', exposure_main = input$sens_main_med_char, exposure_color = NULL, exposure_panel = input$sens_panel_med_char, imp_or_all = 'all', imp_data = NULL)})
    output$sens_pie_chart_char_obs <- renderPlot({customizedPlot(data = getDatasetWithClustersSens(outcome_variable = input$sens_char_outcome, nb_clusters = input$sens_char_nb_cl, method = input$sens_char_method, obs_or_imp = 'obs'), main = input$sens_main_char, color = 'cluster', panel = input$sens_panel_char, type = 'pie', exposure_main = input$sens_main_med_char, exposure_color = NULL, exposure_panel = input$sens_panel_med_char, imp_or_all = NULL, imp_data = NULL)})
    output$sens_pie_chart_char_imp <- renderPlot({customizedPlot(data = getDatasetWithClustersSens(outcome_variable = input$sens_char_outcome, nb_clusters = input$sens_char_nb_cl, method = input$sens_char_method, obs_or_imp = 'imp'), main = input$sens_main_char, color = 'cluster', panel = input$sens_panel_char, type = 'pie', exposure_main = input$sens_main_med_char, exposure_color = NULL, exposure_panel = input$sens_panel_med_char, imp_or_all = 'all', imp_data = NULL)})
    output$sens_upset_plot_char_obs <- renderPlot({customizedPlot(data = getDatasetWithClustersSens(outcome_variable = input$sens_char_outcome, nb_clusters = input$sens_char_nb_cl, method = input$sens_char_method, obs_or_imp = 'obs'), main = input$sens_main_char, color = 'cluster', panel = input$sens_panel_char, type = 'upset', exposure_main = input$sens_main_med_char, exposure_color = NULL, exposure_panel = input$sens_panel_med_char, imp_or_all = NULL, imp_data = NULL)})
    output$sens_upset_plot_char_imp <- renderPlot({customizedPlot(data = getDatasetWithClustersSens(outcome_variable = input$sens_char_outcome, nb_clusters = input$sens_char_nb_cl, method = input$sens_char_method, obs_or_imp = 'imp'), main = input$sens_main_char, color = 'cluster', panel = input$sens_panel_char, type = 'upset', exposure_main = input$sens_main_med_char, exposure_color = NULL, exposure_panel = input$sens_panel_med_char, imp_or_all = 'all', imp_data = NULL)})
    output$sens_upset_plot_2_char_obs <- renderPlot({customizedPlot(data = getDatasetWithClustersSens(outcome_variable = input$sens_char_outcome, nb_clusters = input$sens_char_nb_cl, method = input$sens_char_method, obs_or_imp = 'obs'), main = input$sens_main_char, color = 'cluster', panel = input$sens_panel_char, type = 'upset2', exposure_main = input$sens_main_med_char, exposure_color = NULL, exposure_panel = input$sens_panel_med_char, imp_or_all = NULL, imp_data = NULL)})
    output$sens_upset_plot_2_char_imp <- renderPlot({customizedPlot(data = getDatasetWithClustersSens(outcome_variable = input$sens_char_outcome, nb_clusters = input$sens_char_nb_cl, method = input$sens_char_method, obs_or_imp = 'imp'), main = input$sens_main_char, color = 'cluster', panel = input$sens_panel_char, type = 'upset2', exposure_main = input$sens_main_med_char, exposure_color = NULL, exposure_panel = input$sens_panel_med_char, imp_or_all = 'all', imp_data = NULL)})
    output$sens_descr_plot <- renderPlot({descriptiveStastsPSens(outcome_variable = input$sens_char_outcome, explanatory_variable = input$sens_main_char, nb_clusters = input$sens_char_nb_cl, method = input$sens_char_method, p_value = input$sens_p_value)})
    output$sens_probs_plot <- renderPlot({multinomProbsSens(outcome_variable  = input$sens_char_outcome, explanatory_variable1  = input$sens_main_char, explanatory_variable2  = input$sens_panel_char, nb_clusters = input$sens_char_nb_cl, method = input$sens_char_method, levels = c('age' = input$sens_age_slider, 'sex' = input$sens_sex_input, 'severity' = input$sens_severity_input, 'nli_gr' = input$sens_nli_input, 'treatment_gr' = input$sens_trgr_input, 'bmi_status' = input$sens_bmi_input))})
  })
  
}

#------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# CREATE SHINY APP
#------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

shinyApp(ui = ui, server = server)