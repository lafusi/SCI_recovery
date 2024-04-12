# Title
title <- "Patterns of Recovery in Spinal Cord Injury: A Longitudinal Cluster Analysis"

# Text for the home page
home_p1 <- c('This app was created to support the findings of the study', 'Patterns of Recovery in Spinal Cord Injury: A Longitudinal Cluster Analysis', '.')
home_p2 <- 'Spinal cord injury (SCI) is a complex, life-disrupting disease involving incurable damage to the spinal cord and it mostly affects the young population [1–7]. Although the incidence is relatively low and the mortality is decreasing in high-income countries, SCI results in high economic costs for society [1,3–5,7–10]. The progression of spinal cord injury is heterogeneous, which makes it challenging to formulate a prognosis and detect potential effects of experimental treatments [11–15]. Additionally, the shape of the recovery trajectories has not been fully described yet. To face this knowledge gap, this study proposes the use of flexible longitudinal clustering methods, namely the k-means clustering and the latent profile analysis, to define recovery groups for five motor, sensory, and functional outcome measures, and includes a statistical description of the clusters. Finally, the study is enriched with a sensitivity analysis to verify the robustness to missing data and the impact of data imputation.' # Background
home_p3 <- 'This app comprises - besides this home page - four other sections:' # Structure of the app
home_p3_def <- 'Here you can find the the terms and the abbreviations that are used in the rest of the app with the corresponding definition.'
home_p3_sygen <- 'This tab contains some information about the original study where the data comes from and allows to explore the dataset with a lot of flexibility. Additionally, the patterns of data missingness can also be explored.'
home_p3_clustering <- 'This section contains the clustering analysis, including a comparison of the results obtained with different methods and the comparison of the resulting clusters.'
home_p3_maths <- 'Here you can find some information about the mathematical modeling of the trajectories.'
home_p3_sensitivity <- 'This big section contains the entire sensitivity analysis for all the parts of the previous analysis.'
home_p4 <- 'Ethical approval for the secondary use of the Sygen® data was obtained by the Research Ethics Board of The University of British Columbia.' # Ethics
home_p5 <- '...'#contributions
home_ref1 <- '[1] J. Bickenbach, A. Officer, T. Shakespeare, P. von Groote, World Health Organization, and The International Spinal Cord Society. International perspectives on spinal cord injury / edited by Jerome Bickenbach ... [et al]. World Health Organization, 2013.' 
home_ref2 <- '[2] Y. H. Kim, K. Y. Ha, and S. I. Kim. Spinal Cord Injury and Related Clinical Trials. Clinics in Orthopedic Surgery,
9(1):1–9, 2017.' # References
home_ref3 <- '[3] L. H. S. Sekhon and M. G. Fehlings. Epidemiology, Demographics, and Pathophysiology of Acute Spinal Cord Injury. Spine (Phila Pa 1976), 26(24S):S2–S12, 2001.'
home_ref4 <- '[4] A. Ackery, C. Tator, and A. Krassioukov. A Global Perspective on Spinal Cord Injury Epidemiology. Journal of Neurotrauma, 21(10):1355–1370, 2004.'
home_ref5 <- '[5] C. Sadowsky, O. Volshteyn, L. Schultz, and J. W. McDonald. Spinal cord injury. Disability and Rehabilitation, 24(13):680–687, 2002.'
home_ref6 <- '[6] J. Guest, N. Datta, G. Jimsheleishvili, and D. R. Gater Jr. Pathophysiology, Classification and Comorbidities after Traumatic Spinal Cord Injury. Journal of Personalized Medicine, 12(7):1126, 2022.'
home_ref7 <- '[7] S. Rossignol, M. Schwab, M. Schwartz, and M. G. Fehlings. Spinal cord injury: time to move? J Neurosci, 27(44):11782–11792, 2007.'
home_ref8 <- '[8] C. Barbiellini Amidei, L. Salmaso, S. Bellio, and M. Saia. Epidemiology of traumatic spinal cord injury: a large population-based study. Spinal Cord, 60(9):812–819, 2022.'
home_ref9 <- '[9] J. R. Wilson, D. W. Cadotte, and M. G. Fehlings. Clinical predictors of neurological outcome, functional status, and survival after traumatic spinal cord injury: a systematic review. Journal of Neurosurgery: Spine, 17(1 Suppl):11–26, 2012.'
home_ref10 <- '[10] J. H. Badhiwala, J. R. Wilson, A. V. Kulkarni, A. Kiss, J. S. Harrop, A. R. Vaccaro, B. Aarabi, F. H. Geisler, and M. G. Fehlings. A Novel Method to Classify Cervical Incomplete Spinal Cord Injury Based on Potential for Recovery: A Group-Based Trajectory Analysis. Journal of Neurotrauma, 39(23–24):1654–1664, 2022.'
home_ref11 <- '[11] B. Jaja, J. Badhiwala, J. Guest, J. Harrop, C. Shaffrey, M. Boakye, S. Kurpad, R. Grossman, E. Toups, F. Geisler, B. Kwon, B. Aarabi, M. Kotter, M. Fehlings, and J. Wilson. Trajectory-Based Classification of Recovery in Sensorimotor Complete Traumatic Cervical Spinal Cord Injury. Neurology, 96(22):e2736–e2748, 2021.'
home_ref12 <- '[12] N. Evaniew, N. Fallah, C. S. Rivers, V. K. Noonan, C. G. Fisher, . Dvorak, M. F, J. R. Wilson, and B. K. Kwon. Unbiased Recursive Partitioning to Stratify Patients with Acute Traumatic Spinal Cord Injuries: External Validity in an Observational Cohort Study. Journal of Neurotrauma, 36(18):2732–2742, 2019.'
home_ref13 <- '[13] M. F. Dvorak, V. K. Noonan, N. Fallah, C. G. Fisher, C. S. Rivers, H. Ahn, E. C. Tsai, A. G. Linassi, S. D. Christie, N. Attabib, R. J. Hurlbert, D. R. Fourney, M. G. Johnson, M. G. Fehlings, B. Drew, C. S. Bailey, J. Paquet, S. Parent, A. Townson, C. Ho, B. C. Craven, D. Gagnon, D. Tsui, R. Fox, J. M. Mac-Thiong, and B. K. Kwon. Minimizing errors in acute traumatic spinal cord injury trials by acknowledging the heterogeneity of spinal cord anatomy and injury severity: an observational Canadian cohort analysis. Journal of Neurotrauma, 31(18):1540–1547, 2014.'
home_ref14 <- '[14] R. J. Marino, S. Burns, D. E. Graves, B. E. Leiby, S. Kirshblum, and D. P. Lammertse. Upper- and lower-extremity motor recovery after traumatic cervical spinal cord injury: an update from the national spinal cord injury database. Archives of Physical Medicine and Rehabilitation, 92(3):369–375, 2011.'
home_ref15 <- '[15] L. G. Tanadini, J. D. Steeves, T. Hothorn, R. Abel, D. Maier, M. Schubert, N. Weidner, R. Rupp, and A. Curt. Identifying Homogeneous Subgroups in Neurological Disorders: Unbiased Recursive Partitioning in Cervical Complete Spinal Cord Injury. Neurorehabilitation and Neural Repair, 28(6):507–515, 2014.'

# Text for abbreviations and definitions
va_p1 <- 'The following is a list of the variables and the abbreviations used to refer to them. The button on the right allows to get an explanation if needed.'
lems <- 'This scores ranges from 0 (total paralysis of the lower extremities) to 50 (normal motor function in the lower extremities). It is based on 5 key muscles in the lower extremities: iliopsoas, quadriceps, tibialis anterior, extensor hallucis longus, and gastrocnemius. A score between 0 (absence of movement, total paralysis) and 5 (normal movement) is assigned to each of these muscles on each body side, and the scores for the single muscles are added together [1, 2].'
uems <- 'This scores ranges from 0 (total paralysis of the upper extremities) to 50 (normal motor function in the upper extremities). It is based on 5 key muscles in the upper extremities: biceps, wrist extensor, triceps, flexor profundus, and hand intrinsics. A score between 0 (absence of movement, total paralysis) and 5 (normal movement) is assigned to each of these muscles on each body side, and the scores for the single muscles are added together [1, 2].'
tms <- 'This score ranges from 0 (complete quadriplegia) to 100 (normal motor function) and it is the sum of the lower extremity motor score (LEMS) and the upper extremity motor score (UEMS) [1, 2].'
ss <- 'Each dermatome from C2 to S4-5 on both body sides is tested for pin prick sensation and light touch sensation and is assigned a score ranging from 0 to 2, where 0 means absent sensation, 1 means abnormal sensation, and 2 means normal sensation. The sum of all pin prick scores gives the pin prick total score, while the sum of all light touch scores gives the light touch total score. Each of these scores ranges from 0 to 112 (2 body sides x 28 key sensory points per body side x 2 max score per sensory key point) [2].'
ais <- 'The American Spinal Injury Association (ASIA) Impairment Scale grades the severity of an injury with a score ranging from A to E, where A refers to the most severe injury, and E refers to normal function [2-5]. Please, refer to the international standards in the references for the details.'
ais_df <- data.frame("Grade" = c("A", "B", "C", "D", "E"), 
                     "Definition" = c(
                       "Complete: No sensory or motor function is retained in the segments S4 and S5",
                       "Sensory incomplete:  Sensory function remains intact in the S4 and S5 segments, while no motor function is preserved in these segments, and no motor function is retained beyond three levels below the motor level on either side of the body",
                       "Motor incomplete: Motor function is preserved below the neurological level, and less than half of key muscles below the neurological level have a muscle grade ≥3",
                       "Motor incomplete: Motor function is preserved below the neurological level, and at least half of key muscles below the neurological level have a muscle grade ≥3",
                       "Normal: Sensory and motor functions are normal in all segments. This score is assigned only if the patient had deficits before, while no grade is assigned if there is no SCI"))
mbc <- 'The modified Benzel scale is a classification system which evaluates the functions that are preserved below the level of injury with a score ranging from 1 (no motor and no sensory function) to 7 (normal function except for minor deficits) [6]. Please, refer to the cited paper for additional details.'
mbc_df <- data.frame("Grade" = c("1", "2", "3", "4", "5", "6", "7"),
                        "Definition" = c(
                          "No motor or sensory function is retained in the segments S4 to S5",
                          "Sensory but not motor function is retained in the sacral segments S4 to S5",
                          "Motor function is preserved below the neurological level and the majority of key muscles below the neurological level have a muscle grade less than three and they are unable to walk",
                          "Unable to walk and some functional motor control below the level of injury that is significantly useful (for example assist in transfers) but that is not sufficient for independent walking",
                          "Limited walking and motor function allows assisted or unassisted walking with but patient mobility is limited by large problems resulting from lack of endurance or fear of falling",
                          "Capable of walking independently and unlimitedly and without substantial limitations except for either or both of the following: challenges with urination or a mildly uncoordinated gait", 
                          "Neurologically intact with except for deficits that do not result in functional problems"))
nli <- 'The most caudal spinal cord segment that provides normal sensory and motor function. From that point, motor and sensory functions are normal rostrally, while they are partially impaired to completely absent caudally [2]. The different levels are referred to as cervical (C01-C08), thoracic (T01-T12), lumbar (L01-L05), sacral (S01-S05), and coccygeal (Coc01).'
splvl <- 'First spinal level, first level of the spine where anatomical damage is found. The different levels are referred to as cervical (C01-C08), thoracic (T01-T12), lumbar (L01-L05), sacral (S01-S05), and coccygeal (Coc01).'
er <- 'When the patient arrives at the hospital.'
baseline <- 'Within 72 hours after injury.'
age <- 'Age of the patient at time of injury given in years.'
gender <- 'Sex refers to the biological difference between males and females.'
bmi <- 'The body mass index (BMI) is calculated as weight [kg]/(height [m])^2. If the BMI is smaller than 18.5, the person is underweight, if it is between 18.5 and 24.9 the person has a healthy weight, if it is between 25 and 29.9 the person is overweight, while if it is even larger the person is obese to various degrees of severity [7].'
tr_gr <- 'Referred to the treatment group in the Sygen study, i.e. placebo, low dose, and high dose [8]. For more information check the section Sygen Data > About the Study, or the article in the references.'
med <- 'Medications that were given to the patients in addition to the treatment investigated during the study. In this analysis, the effect of three concomitant medications is investigated: Famotidine, Ranitidine, and Cimetidine.'
mcar <- 'Data is missing completely at random if missingness does not depend on any other observed or unobserved variable, e.g. when something is not reported in a random sample of patients [9-11].'
mar <- 'Data is missing at random if missingness only depends on observed but not on unobserved variables, e.g. when a variable is recorded more often in females than in males [9-11].'
mnar <- 'Data is not missing at random if missingness depends on both observed and unobserved variables, i.e. also on factors that cannot be measured or considered by researchers. This is the most problematic mechanism of missing data because this can lead to biases [9-11].'
va_ref_1 <- '[1] F. H. Geisler, F. C. Dorsey, and W. P. Coleman. Recovery of motor function after spinal-cord injury–a randomized, placebo-controlled trial with GM-1 ganglioside. The New England Journal of Medicine, 324(26):1829–1838, 1991.'
va_ref_2 <- '[2] R. Rupp, F. Biering-Sørensen, S. P. Burns, D. E. Graves, J. Guest, L. Jones, M. Schmidt Read, G. M. Rodriguez, C. Schuld, K. E. Tansey, K. Walden, and S. Kirshblum. International Standards for Neurological Classification of Spinal Cord Injury: Revised 2019. Topics in Spinal Cord Injury Rehabilitation, 27(2):1–22, 2021.'
va_ref_3 <- '[3] A. S. Burns, R. J. Marino, A. E. Flanders, and H. Flett. Clinical diagnosis and prognosis following spinal cord injury. Handbook of Clinical Neurology, 109:47–62, 2012.'
va_ref_4 <- '[4] J. Guest, N. Datta, G. Jimsheleishvili, and D. R. Gater Jr. Pathophysiology, Classification and Comorbidities after Traumatic Spinal Cord Injury. Journal of Personalized Medicine, 12(7):1126, 2022.'
va_ref_5 <- '[5] B. Perrouin-Verbe, C. Lefevre, P. Kieny, R. Gross, B. Reiss, and M. Le Fort. Spinal cord injury: A multisystem physiological impairment/ dysfunction. Revue neurologique (Paris), 177(5):594–605, 2021.'
va_ref_6 <- '[6] F. H. Geisler, W. P. Coleman, G. Grieco, and D. Poonian. Measurements and recovery patterns in a multicenter study of acute spinal cord injury. Spine (Phila Pa 1976), 26(24 Suppl):S68–86, 2001.'
va_ref_7 <- '[7] W. C. Willett, W. H. Dietz, and G. A. Colditz. Guidelines for Healthy Weight. New England Journal of Medicine, 341(6):427–434, 1999.'
va_ref_8 <- '[8] F. H. Geisler, W. P. Coleman, G. Grieco, and D. Poonian. The Sygen multicenter acute spinal cord injury study. Spine (Phila Pa 1976), 26(24 Suppl):S87–98, 2001'
va_ref_9 <- '[9] E. Harrison. Missing data. https://cran.r-project.org/web/packages/finalfit/vignettes/missing.html. Accessed: 2023-10-10.'
va_ref_10 <- '[10] C. M. Salgado, C. Azevedo, and S. M. Proença, H. Vieira. Missing Data. In MIT Critical Data, editor, Secondary Analysis of Electronic Health Records, chapter 13. Springer, Cham (CH), 2016.'
va_ref_11 <- '[11] A. Alruhaymi and C. Kim. Study on the Missing Data Mechanisms and Imputation Methods. Open Journal of Statistics, 11:477–492, 2021.'

# Text for Sygen data
# about the study
about_p1 <- 'This study was a randomized, double-blind, multicenter study performed at 28 centers in North America.'
about_p2 <- 'The aim of the study was to demonstrate the primary efficacy GM-1 ganglioside treatment in acute spinal cord injury. In the end, no significant effect was found.'
about_p3 <- 'For the study, 797 patients were recruited. These patients had complete or incomplete, cervical or thoracic injuries, with AIS grade ranging from A to D. They were divided into 3 groups: placebo, «low-dose», «high-dose». 2 patients did not receive any treatment and 35 patients were considered ineligible for the study, therefore only 760 patients were followed and included in the final analysis.'
about_p4 <- 'The baseline examinations took place within 72 hours after the injury, and the following assessments were performed at 4, 8, 16, 26, and 52 weeks after SCI.'
about_ref_1 <- '[1] F. H. Geisler, F. C. Dorsey, and W. P. Coleman. Recovery of motor function after spinal-cord injury–a randomized, placebo-controlled trial with GM-1 ganglioside. The New England Journal of Medicine, 324(26):1829–1838, 1991.'
about_ref_2 <- '[2] F. H. Geisler, W. P. Coleman, G. Grieco, and D. Poonian. Recruitment and early treatment in a multicenter study of acute spinal cord injury. Spine (Phila Pa 1976), 26(24 Suppl):S58–S67, 2001.'
about_ref_3 <- '[3] F. H. Geisler, W. P. Coleman, G. Grieco, and D. Poonian. The Sygen multicenter acute spinal cord injury study. Spine (Phila Pa 1976), 26(24 Suppl):S87–98, 2001'

# demographic variables
dem_p1 <- 'In this section you have the opportunity to look into the baseline characteristics of the dataset you would like. In a first panel you will be able to see a demographic table, while on the following panels different types of plots will be available. You will need to select the dataset you want to look at. The various subset contain all the patients with at least two observations of the variable of interest. Additionally, you will be able to choose a main variable of interest, and you will be able to further differentiate different categories.'

# outcome variables
out_p1 <- 'This section gives an overview of the outcome variables over time for the single participants as well as the distribution of the variables at different timepoints. Additionally you will have the possibility to add colors and create panels to distinguish between different patient and injury characteristics. Note that when you select the outcome variable of interest, you automatically also select the corresponding data subset.'

# missing data
na_p1 <- 'This section has been created to understand data missingness. Each panel represents a different aspect of missingness and the short paragraph facilitates the understanding of the various elements. For the outcomes, you can choose whether you would like to look at the pattern of missingness for each patient week per week (when measurements were performed), or if you would rather like to look at the data in the long form. Note that these plots might take a long time to load if many variables are selected.'
upset_na_int <- 'Each row represents whether that specific information is missing or not (black = missing) and the corresponding bars on the side indicate how often it is missing. The columns and the corresponding bars on the top indicate how often two pieces of information are missing together.'
pairs_na_int <- 'In the box plots, the x-axis represent the values of the variable indicated on the top of the column, while the y-axis represents whether the information indicated on the right is missing or not. The boxes allow to compare the two distributions. On the other hand, in the bar plots, the x-axis represents the levels of the factor indicated on the top, and the y-axis represents how often the information given on the right is present or missing (grey indicates the information is missing). The y-axis can be a proportion or the counts depending on your choice.'
pattern_na_int <- 'The color of each cell represent whether the value indicated on the top is missing or not. Red (0) indicates that the value is missing, blue (1) indicates that the value is not missing. The different rows represent the patterns of missingness. The number on the left of each row represent how often that pattern appears in the dataset, while the number on the right represents the number of missing values in that pattern. The numbers at the bottom represent how many missing values there are for each variable indicated on the top. Finally, the number in the bottom-right corner is the total number of missing values in the dataset (the sum of the numbers at the bottom).'
ref_pattern <- 'Z. Zhang. Missing data exploration: highlighting graphical presentation of missing pattern. Annals of Translational Medicine, 3(22):356, 2015'

# Text for clustering
# about the methods
methods_p1 <- 'Here you have some information about the methods that were selected for this study.'
kmeans_exp <- 'This algorithm tries to find groups of observations tat are very close to each other. After a random centroid initialization (i.e. random choice of initial cluster centers), every observation is assigned to the closest centroid, and the centroids are updated based on the observations belonging to each cluster. The solution is found through this iterative process. [1]'
lpa_exp <- 'This algorithm is based on the probability that an observation belongs to a certain group. After random parameter initialization (i.e. random choice of initial cluster parameters), every observation is allocated to the cluster with largest posterior probability, and the parameters are updated based on based on the observations belonging to each cluster. The solution is found through this iterative process. [2]'
#hclust_exp <- 'Here the observations that are really similar to each other are put together up to a certain height.'
#bma_exp <- 'BMA combines the results of different clustering methods to obtain "the real clusters" and also to determine the number of clusters.'
meth_ref1 <- '[1] B. Chong. K-means clustering algorithm: a brief review. Academic Journal of Computing Information Science, 4(5):37–40, 2021.'
meth_ref2 <- '[2] D. Oberski. Human-computer Interaction Series, chapter Mixture Models: Latent Profile and Latent Class Analysis, pages 275–287. 2016.'

# method comparison
comp_p1 <- 'Here you can look at the clusters obtained with different clustering methods. In the trajectories tab you can visualize the trajectories of each cluster and the number of patients in each cluster. In the evaluation criteria tab you can see a plot of the selected index as a function of the number of clusters for the selected method (red) and for the other method (grey).'
ch_exp <- 'c(k) = Trace(B)/Trace(W)*(n-k)/(k-1)'
ch2_exp <- 'c(k) = Trace(B)/Trace(W)*(n-1)/(n-k)'
ch3_exp <- 'g(k) = Trace(B)/Trace(W)*(n-k)/sqrt(k-1)'
bic_exp <- 'BIC = 2*log(L) - h*log(n)'
bic2_exp <- 'BIC = 2*log(L) - h*log(N)'
aic_exp <- 'AIC = 2*log(L) - 2*h'
aicc_exp <- 'AIC = AIC + (2h(h+1))/(n-h-1)'
aicc2_exp <- 'AIC = AIC + (2h(h+1))/(N-h-1)'
ppc_exp <- 'Global posterior probability'
rt_exp <- 'r(k) = -Sum(dist(x,center(x)))/min(dist(center_i, center_j)^2)'
db_exp <- 'd(k) = -mean((DistInterne(cluster_i) + DistInterne(cluster_j))/(DistExterne(cluster_i, cluster_j)))'
imp_note <- ": All the criteria are computed in such a way that they need to be maximized to find the best partition, meaning that here you see the 'true' criterion for those criteria that normally need to be maximized, and the opposite of the 'true' criterion for those criteria that normally need to be minimized."


# patient and injury characteristics
char_p1 <- 'Here you can look into the differences in characteristics between clusters obtained with different methods. In the different tabs you have the cluster composition displayed in a table and in plotted in different ways. Additionally, the descriptive statistics tab shows whether significant differences were found with statistical tests for the first selected variable, and the multinomial regression tab shows the prediction probabilities for the selected variable, according to the chosen values of the other characteristics.'


# sensitivity analysis
sens_p1 <- 'This section allows to go deeper into the question, whether the number of accepted missing values and the number of necessary accepted values per trajectory affect the results of the analysis.'
sens_p2 <- 'In this analysis it was decided to use a multiple imputation algorithm to face the data missingness problem and account for the uncertainty of imputation at the same time. However, it is important to make sure that the results obtained with this procedure do not differ too much from the results one would have obtained with a dataset where no data was missing.'
sens_p3 <- 'To determine the robustness of the analysis to missing data, and the impact of the chosen methodology to face the missingness problem on the results.'
sens_p4 <- '1) Obtain a dataset with no missing data, and recreate missingness pattern'
sens_p5 <- '2) Run the analysis on the full dataset and on the imputed artificial datasets'
sens_p6 <- '3) Compare the results'
sens_comp_p1 <- 'Here you can compare the clustering results. The structure of this page mirrors the structure of the method comparison page in the clustering section.'
sens_char_p1 <- 'Here you can compare the results of the statistical analyses. The structure of this page mirrors the patient and injury characteristics page in the clustering section.'

