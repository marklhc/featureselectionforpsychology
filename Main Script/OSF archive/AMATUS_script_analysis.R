# March 2019; January 2024
# by Tobias Feuerecker and Maristella Lunardon
# Downloaded from https://osf.io/gszpb/

# Input:  AMATUS_dataset.csv
# Output: Table_of_instruments.txt
#         Correlationmatrix.txt
#         N_of_correlations.txt
#         demographic_table.csv



# ================================================================================
# Beginning script
# ================================================================================

# Function for calculating skewness and kurtosis in the same way SPSS does, retrieved from 
# http://www.stat.cmu.edu/~hseltman/files/spssSkewKurtosis.R
#
# Reference: pp 451-452 of
# http://support.spss.com/ProductsExt/SPSS/Documentation/Manuals/16.0/SPSS 16.0 Algorithms.pdf
# 
# See also: Suggestion for Using Powerful and Informative Tests of Normality,
# Ralph B. D'Agostino, Albert Belanger, Ralph B. D'Agostino, Jr.,
# The American Statistician, Vol. 44, No. 4 (Nov., 1990), pp. 316-321


spssSkewKurtosis=function(x) {
  w=length(x)
  m1=mean(x)
  m2=sum((x-m1)^2)
  m3=sum((x-m1)^3)
  m4=sum((x-m1)^4)
  s1=sd(x)
  skew=w*m3/(w-1)/(w-2)/s1^3
  sdskew=sqrt( 6*w*(w-1) / ((w-2)*(w+1)*(w+3)) )
  kurtosis=(w*(w+1)*m4 - 3*m2^2*(w-1)) / ((w-1)*(w-2)*(w-3)*s1^4)
  sdkurtosis=sqrt( 4*(w^2-1) * sdskew^2 / ((w-3)*(w+5)) )
  mat=matrix(c(skew,kurtosis, sdskew,sdkurtosis), 2,
             dimnames=list(c("skew","kurtosis"), c("estimate","se")))
  return(mat)
}


# --------------------------------------------------------------------------------
# Packages and loading file
# --------------------------------------------------------------------------------

# used packages
library(psych)


# load file
setwd("/home/maristella/Desktop/AMATUS/material/VALID_VERSION_Everything_Cipora/creating bigdata")


data <- read.csv("AMATUS_dataset.csv", sep = ";")


# --------------------------------------------------------------------------------
# Create table of instruments
# --------------------------------------------------------------------------------

# calculate Cronbach's Alpha for the instruments (raw alpha in output)
# (get the numeric value of Cronbach's Alpha with summary([output of the alpha function])[1,1])

# Math grade and influence
CA_MG <- NA
CA_MI <- NA
# Liking and persistence items
CA_LM <- NA
CA_LS <- NA
CA_LH <- NA
CA_PM <- NA
CA_PS <- NA
CA_PH <- NA
# Preference and ease
CA_PTL <- NA
CA_PTM <- NA
CA_PTS <- NA
CA_ETL <- NA
CA_ETM <- NA
CA_ETS <- NA

# AMAS
CA_AMAS_total <- alpha(data[c("AMAS1", "AMAS2", "AMAS3", "AMAS4", "AMAS5","AMAS6", "AMAS7", "AMAS8", "AMAS9")], check.keys = T)$total$raw_alpha
CA_AMAS_learning <- alpha(data[c("AMAS1", "AMAS3", "AMAS6", "AMAS7", "AMAS9")],check.keys = T)$total$raw_alpha
CA_AMAS_testing <- alpha(data[c("AMAS2", "AMAS4", "AMAS5", "AMAS8")],
                                  check.keys = T)$total$raw_alpha

# GAD
CA_GAD <- alpha(data[c("GAD1", "GAD2", "GAD3", "GAD4", "GAD5", "GAD6", "GAD7")],
                                 check.keys = T)$total$raw_alpha

# STAI
CA_STAI <- alpha(data[c("STAI1", "STAI2", "STAI3", "STAI4", "STAI5")],
                        check.keys = T)$total$raw_alpha

# TAI
CA_TAI <- alpha(data[c("TAI1", "TAI2", "TAI3", "TAI4", "TAI5")],
                         check.keys = T)$total$raw_alpha

# SDQ 3 Math
CA_SDQ3M <- alpha(data[c("SDQ_M1", "SDQ_M2", "SDQ_M3", "SDQ_M4")],
                        check.keys = T)$total$raw_alpha

# SDQ 3 Language
CA_SDQ3L <- alpha(data[c("SDQ_L1", "SDQ_L2", "SDQ_L3", "SDQ_L4")],
                          check.keys = T)$total$raw_alpha

# PISA math self-efficacy
CA_PISA <- alpha(data[c("PISA_ME1", "PISA_ME2", "PISA_ME3", "PISA_ME4", 
                                "PISA_ME5", "PISA_ME6")], check.keys = T)$total$raw_alpha

# BFI neuroticism subscale
CA_BFI <- alpha(data[c("BFI_N1", "BFI_N2", "BFI_N3", "BFI_N4", 
                                "BFI_N5", "BFI_N6", "BFI_N7", "BFI_N8")], 
                         check.keys = T)$total$raw_alpha

# Math-gender stereotype endorsement (FSMAS-SF)
CA_FSMAS <- alpha(data[c("FSMAS_SE1", "FSMAS_SE2", "FSMAS_SE3", 
                                 "FSMAS_SE4", "FSMAS_SE5", "FSMAS_SE6", 
                                 "FSMAS_SE7", "FSMAS_SE8", "FSMAS_SE9")], 
                        check.keys = T)$total$raw_alpha

# Arithmetic performance
# clean data first i.e. remove all subjects who did not answer the tasks in the correct order
data_cleaned <- data[data$arith_perf_correct_order == "yes", ]

# compute cronbach's alpha for Speeded arithmetic
CA_SA <- alpha(data_cleaned[c("arith_perf1_acc", "arith_perf2_acc", "arith_perf3_acc", 
                                      "arith_perf4_acc", "arith_perf5_acc", "arith_perf6_acc", 
                                      "arith_perf7_acc", "arith_perf8_acc", "arith_perf9_acc", 
                                      "arith_perf10_acc", "arith_perf11_acc", "arith_perf12_acc", 
                                      "arith_perf13_acc", "arith_perf14_acc", "arith_perf15_acc", 
                                      
                                      "arith_perf16_acc", "arith_perf17_acc", "arith_perf18_acc", 
                                      "arith_perf19_acc", "arith_perf20_acc", "arith_perf21_acc", 
                                      "arith_perf22_acc", "arith_perf23_acc", "arith_perf24_acc", 
                                      "arith_perf25_acc", "arith_perf26_acc", "arith_perf27_acc", 
                                      "arith_perf28_acc", "arith_perf29_acc", "arith_perf30_acc", 
                                      
                                      "arith_perf31_acc", "arith_perf32_acc", "arith_perf33_acc", 
                                      "arith_perf34_acc", "arith_perf35_acc", "arith_perf36_acc", 
                                      "arith_perf37_acc", "arith_perf38_acc", "arith_perf39_acc", 
                                      "arith_perf40_acc")], 
                          check.keys = T)$total$raw_alpha


# create a vector of all Cronbach Alpha's
cronbachs_alpha <- c(CA_MG, CA_MI, CA_LM, CA_LS, CA_LH, CA_PM, CA_PS, CA_PH, CA_AMAS_total, CA_AMAS_learning, 
                     CA_AMAS_testing, CA_GAD, CA_STAI, CA_TAI, CA_SDQ3M, CA_SDQ3L, CA_PISA,
                     CA_BFI, CA_PTL, CA_PTM, CA_PTS, CA_ETL, CA_ETM, CA_ETS, CA_FSMAS, CA_SA)


# --------------------------------------------------------------------------------
# calculate the ordinal Alpha for the instruments
# See scripts available at https://osf.io/y76fs/

# Math grade and influence
OA_MG <- NA
OA_MI <- NA
# Liking and persistence items
OA_LM <- NA
OA_LS <- NA
OA_LH <- NA
OA_PM <- NA
OA_PS <- NA
OA_PH <- NA
# Preference and ease
OA_PTL <- NA
OA_PTM <- NA
OA_PTS <- NA
OA_ETL <- NA
OA_ETM <- NA
OA_ETS <- NA

# AMAS
data_pc_tmp <- polychoric(data[c("AMAS1", "AMAS2", "AMAS3", "AMAS4", "AMAS5",
                             "AMAS6", "AMAS7", "AMAS8", "AMAS9")])
OA_AMAS_total <- alpha(data_pc_tmp$rho)$total$raw_alpha

data_pc_tmp <- polychoric(data[c("AMAS1", "AMAS3", "AMAS6", "AMAS7", "AMAS9")])
OA_AMAS_learning <- alpha(data_pc_tmp$rho)$total$raw_alpha

data_pc_tmp <- polychoric(data[c("AMAS2", "AMAS4", "AMAS5", "AMAS8")])
OA_AMAS_testing <- alpha(data_pc_tmp$rho)$total$raw_alpha

# GAD
data_pc_tmp <- polychoric(data[c("GAD1", "GAD2", "GAD3", "GAD4", "GAD5", "GAD6", "GAD7")])
OA_GAD <- alpha(data_pc_tmp$rho)$total$raw_alpha

# STAI
data_pc_tmp <- polychoric(data[c("STAI1", "STAI2", "STAI3", "STAI4", "STAI5")])
OA_STAI <- alpha(data_pc_tmp$rho)$total$raw_alpha

# TAI
data_pc_tmp <- polychoric(data[c("TAI1", "TAI2", "TAI3", "TAI4", "TAI5")])
OA_TAI <- alpha(data_pc_tmp$rho)$total$raw_alpha

# SDQ 3 Math
data_pc_tmp <- polychoric(data[c("SDQ_M1", "SDQ_M2", "SDQ_M3", "SDQ_M4")])
OA_SDQ3M <- alpha(data_pc_tmp$rho)$total$raw_alpha

# SDQ3 Language
data_pc_tmp <- polychoric(data[c("SDQ_L1", "SDQ_L2", "SDQ_L3", "SDQ_L4")])
OA_SDQ3L <- alpha(data_pc_tmp$rho)$total$raw_alpha

# PISA
data_pc_tmp <- polychoric(data[c("PISA_ME1", "PISA_ME2", "PISA_ME3", "PISA_ME4", 
                                 "PISA_ME5", "PISA_ME6")])
OA_PISA <- alpha(data_pc_tmp$rho)$total$raw_alpha

# BFI
data_pc_tmp <- polychoric(data[c("BFI_N1", "BFI_N2", "BFI_N3", "BFI_N4", 
                                 "BFI_N5", "BFI_N6", "BFI_N7", "BFI_N8")])
OA_BFI <- alpha(data_pc_tmp$rho)$total$raw_alpha

# FSMAS
data_pc_tmp <- polychoric(data[c("FSMAS_SE1", "FSMAS_SE2", "FSMAS_SE3", 
                                 "FSMAS_SE4", "FSMAS_SE5", "FSMAS_SE6", 
                                 "FSMAS_SE7", "FSMAS_SE8", "FSMAS_SE9")])
OA_FSMAS <- alpha(data_pc_tmp$rho)$total$raw_alpha

# Speeded arithmetic
data_pc_tmp <- polychoric(data_cleaned[c("arith_perf1_acc", "arith_perf2_acc", "arith_perf3_acc", 
                                         "arith_perf4_acc", "arith_perf5_acc", "arith_perf6_acc", 
                                         "arith_perf7_acc", "arith_perf8_acc", "arith_perf9_acc", 
                                         "arith_perf10_acc", "arith_perf11_acc", "arith_perf12_acc", 
                                         "arith_perf13_acc", "arith_perf14_acc", "arith_perf15_acc", 
                                         
                                         "arith_perf16_acc", "arith_perf17_acc", "arith_perf18_acc", 
                                         "arith_perf19_acc", "arith_perf20_acc", "arith_perf21_acc", 
                                         "arith_perf22_acc", "arith_perf23_acc", "arith_perf24_acc", 
                                         "arith_perf25_acc", "arith_perf26_acc", "arith_perf27_acc", 
                                         "arith_perf28_acc", "arith_perf29_acc", "arith_perf30_acc", 
                                         
                                         "arith_perf31_acc", "arith_perf32_acc", "arith_perf33_acc", 
                                         "arith_perf34_acc", "arith_perf35_acc", "arith_perf36_acc", 
                                         "arith_perf37_acc", "arith_perf38_acc", "arith_perf39_acc", 
                                         "arith_perf40_acc")])
OA_SA <- alpha(data_pc_tmp$rho)$total$raw_alpha

# Create a vector of all ordinal Alphas:
ordinal_alpha <- c(OA_MG, OA_MI, OA_LM, OA_LS, OA_LH, OA_PM, OA_PS, OA_PH, OA_AMAS_total, OA_AMAS_learning, 
                   OA_AMAS_testing, OA_GAD, OA_STAI, OA_TAI, OA_SDQ3M, OA_SDQ3L, OA_PISA,
                   OA_BFI, OA_PTL, OA_PTM, OA_PTS, OA_ETL, OA_ETM, OA_ETS, OA_FSMAS, OA_SA)


# --------------------------------------------------------------------------------
# create the other vectors that are needed for the final table

# Instrument names
instrument <- c("Math grade", 
                "Math influence study choice", 
                "Liking math", 
                "Liking science", 
                "Liking humanities", 
                "Persistence math", 
                "Persistence science", 
                "Persistence humanities", 
                "Math anxiety total", 
                "Math anxiety learning", 
                "Math anxiety testing", 
                "Trait anxiety", 
                "State anxiety", 
                "Test anxiety", 
                "Math self-concept", 
                "Language self-concept", 
                "Math self-efficacy", 
                "Neuroticism",
                "Preference teaching language", 
                "Preference teaching math", 
                "Preference teaching science", 
                "Ease teaching language", 
                "Ease teaching math", 
                "Ease teaching science",
                "Math-gender stereotype endorsement", 
                "Arithmetic performance")

# means of total scores
means <- c(mean(data$math_grade),
           mean(data$math_inf_program_choice),
           
           mean(data$liking_math),
           mean(data$liking_science),
           mean(data$liking_humanities),
           mean(data$persistence_math),
           mean(data$persistence_science),
           mean(data$persistence_humanities),
           mean(data$score_AMAS_total),
           mean(data$score_AMAS_learning),
           mean(data$score_AMAS_testing),
           mean(data$score_GAD),
           mean(data$score_STAI_state_short),
           mean(data$score_TAI_short),
           mean(data$score_SDQ_M),
           mean(data$score_SDQ_L),
           mean(data$score_PISA_ME),
           mean(data$score_BFI_N),
           
           mean(data$preference_teaching_language[data$preference_teaching_language %in% 1:5]),
           mean(data$preference_teaching_mathematics[data$preference_teaching_mathematics %in% 1:5]),
           mean(data$preference_teaching_science[data$preference_teaching_science %in% 1:5]),
           mean(data$ease_teaching_language[data$ease_teaching_language %in% 1:5]),
           mean(data$ease_teaching_mathematics[data$ease_teaching_mathematics %in% 1:5]),
           mean(data$ease_teaching_science[data$ease_teaching_science %in% 1:5]),
           
           mean(data$score_FSMAS_SE, na.rm = T),
           mean(data_cleaned$sum_arith_perf))

# standard deviations
standard_deviation <- c(sd(data$math_grade),
                        sd(data$math_inf_program_choice),
                        
                        sd(data$liking_math),
                        sd(data$liking_science),
                        sd(data$liking_humanities),
                        sd(data$persistence_math),
                        sd(data$persistence_science),
                        sd(data$persistence_humanities),
                        sd(data$score_AMAS_total),
                        sd(data$score_AMAS_learning),
                        sd(data$score_AMAS_testing),
                        sd(data$score_GAD),
                        sd(data$score_STAI_state_short),
                        sd(data$score_TAI_short),
                        sd(data$score_SDQ_M),
                        sd(data$score_SDQ_L),
                        sd(data$score_PISA_ME),
                        sd(data$score_BFI_N),
                        
                        sd(data$preference_teaching_language[data$preference_teaching_language %in% 1:5]),
                        sd(data$preference_teaching_mathematics[data$preference_teaching_mathematics %in% 1:5]),
                        sd(data$preference_teaching_science[data$preference_teaching_science %in% 1:5]),
                        sd(data$ease_teaching_language[data$ease_teaching_language %in% 1:5]),
                        sd(data$ease_teaching_mathematics[data$ease_teaching_mathematics %in% 1:5]),
                        sd(data$ease_teaching_science[data$ease_teaching_science %in% 1:5]),
                        
                        sd(data$score_FSMAS_SE, na.rm = T),
                        sd(data_cleaned$sum_arith_perf))

# Min
minimum <- c(min(data$math_grade),
             min(data$math_inf_program_choice),
             
             min(data$liking_math),
             min(data$liking_science),
             min(data$liking_humanities),
             min(data$persistence_math),
             min(data$persistence_science),
             min(data$persistence_humanities),
             min(data$score_AMAS_total),
             min(data$score_AMAS_learning),
             min(data$score_AMAS_testing),
             min(data$score_GAD),
             min(data$score_STAI_state_short),
             min(data$score_TAI_short),
             min(data$score_SDQ_M),
             min(data$score_SDQ_L),
             min(data$score_PISA_ME),
             min(data$score_BFI_N),
             
             min(data$preference_teaching_language[data$preference_teaching_language %in% 1:5]),
             min(data$preference_teaching_mathematics[data$preference_teaching_mathematics %in% 1:5]),
             min(data$preference_teaching_science[data$preference_teaching_science %in% 1:5]),
             min(data$ease_teaching_language[data$ease_teaching_language %in% 1:5]),
             min(data$ease_teaching_mathematics[data$ease_teaching_mathematics %in% 1:5]),
             min(data$ease_teaching_science[data$ease_teaching_science %in% 1:5]),
             
             min(data$score_FSMAS_SE, na.rm = T),
             min(data_cleaned$sum_arith_perf))

# Max
maximum <- c(max(data$math_grade),
             max(data$math_inf_program_choice),
             
             max(data$liking_math),
             max(data$liking_science),
             max(data$liking_humanities),
             max(data$persistence_math),
             max(data$persistence_science),
             max(data$persistence_humanities),
             max(data$score_AMAS_total),
             max(data$score_AMAS_learning),
             max(data$score_AMAS_testing),
             max(data$score_GAD),
             max(data$score_STAI_state_short),
             max(data$score_TAI_short),
             max(data$score_SDQ_M),
             max(data$score_SDQ_L),
             max(data$score_PISA_ME),
             max(data$score_BFI_N),
             
             max(data$preference_teaching_language[data$preference_teaching_language %in% 1:5]),
             max(data$preference_teaching_mathematics[data$preference_teaching_mathematics %in% 1:5]),
             max(data$preference_teaching_science[data$preference_teaching_science %in% 1:5]),
             max(data$ease_teaching_language[data$ease_teaching_language %in% 1:5]),
             max(data$ease_teaching_mathematics[data$ease_teaching_mathematics %in% 1:5]),
             max(data$ease_teaching_science[data$ease_teaching_science %in% 1:5]),
             
             max(data$score_FSMAS_SE, na.rm = T),
             max(data_cleaned$sum_arith_perf))

# Skewness (see funtion spssSkewKurtosis() above)
skewness <- c(spssSkewKurtosis(data$math_grade)[1,1],
              spssSkewKurtosis(data$math_inf_program_choice)[1,1],
              
              spssSkewKurtosis(data$liking_math)[1,1],
              spssSkewKurtosis(data$liking_science)[1,1],
              spssSkewKurtosis(data$liking_humanities)[1,1],
              spssSkewKurtosis(data$persistence_math)[1,1],
              spssSkewKurtosis(data$persistence_science)[1,1],
              spssSkewKurtosis(data$persistence_humanities)[1,1],
              spssSkewKurtosis(data$score_AMAS_total)[1,1],
              spssSkewKurtosis(data$score_AMAS_learning)[1,1],
              spssSkewKurtosis(data$score_AMAS_testing)[1,1],
              spssSkewKurtosis(data$score_GAD)[1,1],
              spssSkewKurtosis(data$score_STAI_state_short)[1,1],
              spssSkewKurtosis(data$score_TAI_short)[1,1],
              spssSkewKurtosis(data$score_SDQ_M)[1,1],
              spssSkewKurtosis(data$score_SDQ_L)[1,1],
              spssSkewKurtosis(data$score_PISA_ME)[1,1],
              spssSkewKurtosis(data$score_BFI_N)[1,1],
              
              spssSkewKurtosis(data$preference_teaching_language[data$preference_teaching_language %in% 1:5])[1,1],
              spssSkewKurtosis(data$preference_teaching_mathematics[data$preference_teaching_mathematics %in% 1:5])[1,1],
              spssSkewKurtosis(data$preference_teaching_science[data$preference_teaching_science %in% 1:5])[1,1],
              spssSkewKurtosis(data$ease_teaching_language[data$ease_teaching_language %in% 1:5])[1,1],
              spssSkewKurtosis(data$ease_teaching_mathematics[data$ease_teaching_mathematics %in% 1:5])[1,1],
              spssSkewKurtosis(data$ease_teaching_science[data$ease_teaching_science %in% 1:5])[1,1],
              
              spssSkewKurtosis(data$score_FSMAS_SE[!is.na(data$score_FSMAS_SE)])[1,1],
              spssSkewKurtosis(data_cleaned$sum_arith_perf)[1,1])

# SE of Skewness
skewness_se <- c(spssSkewKurtosis(data$math_grade)[1,2],
                 spssSkewKurtosis(data$math_inf_program_choice)[1,2],
                 
                 spssSkewKurtosis(data$liking_math)[1,2],
                 spssSkewKurtosis(data$liking_science)[1,2],
                 spssSkewKurtosis(data$liking_humanities)[1,2],
                 spssSkewKurtosis(data$persistence_math)[1,2],
                 spssSkewKurtosis(data$persistence_science)[1,2],
                 spssSkewKurtosis(data$persistence_humanities)[1,2],
                 spssSkewKurtosis(data$score_AMAS_total)[1,2],
                 spssSkewKurtosis(data$score_AMAS_learning)[1,2],
                 spssSkewKurtosis(data$score_AMAS_testing)[1,2],
                 spssSkewKurtosis(data$score_GAD)[1,2],
                 spssSkewKurtosis(data$score_STAI_state_short)[1,2],
                 spssSkewKurtosis(data$score_TAI_short)[1,2],
                 spssSkewKurtosis(data$score_SDQ_M)[1,2],
                 spssSkewKurtosis(data$score_SDQ_L)[1,2],
                 spssSkewKurtosis(data$score_PISA_ME)[1,2],
                 spssSkewKurtosis(data$score_BFI_N)[1,2],
                 
                 spssSkewKurtosis(data$preference_teaching_language[data$preference_teaching_language %in% 1:5])[1,2],
                 spssSkewKurtosis(data$preference_teaching_mathematics[data$preference_teaching_mathematics %in% 1:5])[1,2],
                 spssSkewKurtosis(data$preference_teaching_science[data$preference_teaching_science %in% 1:5])[1,2],
                 spssSkewKurtosis(data$ease_teaching_language[data$ease_teaching_language %in% 1:5])[1,2],
                 spssSkewKurtosis(data$ease_teaching_mathematics[data$ease_teaching_mathematics %in% 1:5])[1,2],
                 spssSkewKurtosis(data$ease_teaching_science[data$ease_teaching_science %in% 1:5])[1,2],
                 
                 spssSkewKurtosis(data$score_FSMAS_SE[!is.na(data$score_FSMAS_SE)])[1,2],
                 spssSkewKurtosis(data_cleaned$sum_arith_perf)[1,2])

# Kurtosis
kurtosis <- c(spssSkewKurtosis(data$math_grade)[2,1],
              spssSkewKurtosis(data$math_inf_program_choice)[2,1],
              
              spssSkewKurtosis(data$liking_math)[2,1],
              spssSkewKurtosis(data$liking_science)[2,1],
              spssSkewKurtosis(data$liking_humanities)[2,1],
              spssSkewKurtosis(data$persistence_math)[2,1],
              spssSkewKurtosis(data$persistence_science)[2,1],
              spssSkewKurtosis(data$persistence_humanities)[2,1],
              spssSkewKurtosis(data$score_AMAS_total)[2,1],
              spssSkewKurtosis(data$score_AMAS_learning)[2,1],
              spssSkewKurtosis(data$score_AMAS_testing)[2,1],
              spssSkewKurtosis(data$score_GAD)[2,1],
              spssSkewKurtosis(data$score_STAI_state_short)[2,1],
              spssSkewKurtosis(data$score_TAI_short)[2,1],
              spssSkewKurtosis(data$score_SDQ_M)[2,1],
              spssSkewKurtosis(data$score_SDQ_L)[2,1],
              spssSkewKurtosis(data$score_PISA_ME)[2,1],
              spssSkewKurtosis(data$score_BFI_N)[2,1],
              
              spssSkewKurtosis(data$preference_teaching_language[data$preference_teaching_language %in% 1:5])[2,1],
              spssSkewKurtosis(data$preference_teaching_mathematics[data$preference_teaching_mathematics %in% 1:5])[2,1],
              spssSkewKurtosis(data$preference_teaching_science[data$preference_teaching_science %in% 1:5])[2,1],
              spssSkewKurtosis(data$ease_teaching_language[data$ease_teaching_language %in% 1:5])[2,1],
              spssSkewKurtosis(data$ease_teaching_mathematics[data$ease_teaching_mathematics %in% 1:5])[2,1],
              spssSkewKurtosis(data$ease_teaching_science[data$ease_teaching_science %in% 1:5])[2,1],
              
              spssSkewKurtosis(data$score_FSMAS_SE[!is.na(data$score_FSMAS_SE)])[2,1],
              spssSkewKurtosis(data_cleaned$sum_arith_perf)[2,1])

# SE of Kurtosis
kurtosis_se <- c(spssSkewKurtosis(data$math_grade)[2,2],
                 spssSkewKurtosis(data$math_inf_program_choice)[2,2],
                 
                 spssSkewKurtosis(data$liking_math)[2,2],
                 spssSkewKurtosis(data$liking_science)[2,2],
                 spssSkewKurtosis(data$liking_humanities)[2,2],
                 spssSkewKurtosis(data$persistence_math)[2,2],
                 spssSkewKurtosis(data$persistence_science)[2,2],
                 spssSkewKurtosis(data$persistence_humanities)[2,2],
                 spssSkewKurtosis(data$score_AMAS_total)[2,2],
                 spssSkewKurtosis(data$score_AMAS_learning)[2,2],
                 spssSkewKurtosis(data$score_AMAS_testing)[2,2],
                 spssSkewKurtosis(data$score_GAD)[2,2],
                 spssSkewKurtosis(data$score_STAI_state_short)[2,2],
                 spssSkewKurtosis(data$score_TAI_short)[2,2],
                 spssSkewKurtosis(data$score_SDQ_M)[2,2],
                 spssSkewKurtosis(data$score_SDQ_L)[2,2],
                 spssSkewKurtosis(data$score_PISA_ME)[2,2],
                 spssSkewKurtosis(data$score_BFI_N)[2,2],
                 
                 spssSkewKurtosis(data$preference_teaching_language[data$preference_teaching_language %in% 1:5])[2,2],
                 spssSkewKurtosis(data$preference_teaching_mathematics[data$preference_teaching_mathematics %in% 1:5])[2,2],
                 spssSkewKurtosis(data$preference_teaching_science[data$preference_teaching_science %in% 1:5])[2,2],
                 spssSkewKurtosis(data$ease_teaching_language[data$ease_teaching_language %in% 1:5])[2,2],
                 spssSkewKurtosis(data$ease_teaching_mathematics[data$ease_teaching_mathematics %in% 1:5])[2,2],
                 spssSkewKurtosis(data$ease_teaching_science[data$ease_teaching_science %in% 1:5])[2,2],
                 
                 spssSkewKurtosis(data$score_FSMAS_SE[!is.na(data$score_FSMAS_SE)])[2,2],
                 spssSkewKurtosis(data_cleaned$sum_arith_perf)[2,2])


# --------------------------------------------------------------------------------
# N for measures for the single item instruments
N <- c(length(data$math_grade[!is.na(data$math_grade)]),
       length(data$math_inf_program_choice[!is.na(data$math_inf_program_choice)]),
       
       length(data$liking_math[!is.na(data$liking_math)]),
       length(data$liking_science[!is.na(data$liking_science)]),
       length(data$liking_humanities[!is.na(data$liking_humanities)]),
       length(data$persistence_math[!is.na(data$persistence_math)]),
       length(data$persistence_science[!is.na(data$persistence_science)]),
       length(data$persistence_humanities[!is.na(data$persistence_humanities)]),
       length(data$score_AMAS_total[!is.na(data$score_AMAS_total)]),
       length(data$score_AMAS_learning[!is.na(data$score_AMAS_learning)]),
       length(data$score_AMAS_testing[!is.na(data$score_AMAS_testing)]),
       length(data$score_GAD[!is.na(data$score_GAD)]),
       length(data$score_STAI_state_short[!is.na(data$score_STAI_state_short)]),
       length(data$score_TAI_short[!is.na(data$score_TAI_short)]),
       length(data$score_SDQ_M[!is.na(data$score_SDQ_M)]),
       length(data$score_SDQ_L[!is.na(data$score_SDQ_L)]),
       length(data$score_PISA_ME[!is.na(data$score_PISA_ME)]),
       length(data$score_BFI_N[!is.na(data$score_BFI_N)]),
       
       length(data$preference_teaching_language[data$preference_teaching_language %in% 1:5]),
       length(data$preference_teaching_mathematics[data$preference_teaching_mathematics %in% 1:5]),
       length(data$preference_teaching_science[data$preference_teaching_science %in% 1:5]),
       length(data$ease_teaching_language[data$ease_teaching_language %in% 1:5]),
       length(data$ease_teaching_mathematics[data$ease_teaching_mathematics %in% 1:5]),
       length(data$ease_teaching_science[data$ease_teaching_science %in% 1:5]),
       
       length(data$score_FSMAS_SE[!is.na(data$score_FSMAS_SE)]),
       length(data$sum_arith_perf[!is.na(data$sum_arith_perf)]))


# --------------------------------------------------------------------------------
# potential theoretical range of scores
theoretical_range <- c("1 - 6", "1 - 9", "1 - 5", "1 - 5","1 - 5", "1 - 5", "1 - 5", "1 - 5", "9 - 45",
                       "5 - 25", "4 - 25", "7 - 28", "5 - 20", "5 - 20", "4 - 16",
                       "4 - 16", "6 - 24", "8 - 40", "1 - 5", "1 - 5", "1 - 5", "1 - 5",
                       "1 - 5", "1 - 5", "9 - 45", "0 - 40")

# --------------------------------------------------------------------------------
# create the final table as data frame and save it as .txt file

final_table <- data.frame(instrument, N,theoretical_range, round(means, 2), 
                          round(standard_deviation, 2), minimum, maximum, 
                          round(cronbachs_alpha, 2), round(ordinal_alpha, 2), 
                          round(skewness, 2), round(skewness_se, 2), round(kurtosis, 2), 
                          round(kurtosis_se, 2))

# rename columns
names(final_table) <- c("instrument", "N", "theoretical_range", "mean", "SD", "min", "max",
                        "Cronbachs_alpha", "Ordinal_alpha", "Skewness", "SE_Skewness", 
                        "Kurtosis", "SE_Kurtosis")

# save table
write.table(final_table, "Table_of_instruments.txt", sep = ",", row.names = F)


# --------------------------------------------------------------------------------
# Create a correlation-matrix of all instruments and various scores
# --------------------------------------------------------------------------------

# choose the scores of interest.
data_corr <- data.frame(data$sum_arith_perf,
                        data$math_grade,
                        data$math_inf_program_choice,
                        data$liking_math,
                        data$liking_science,
                        data$liking_humanities,
                        data$persistence_math,
                        data$persistence_science,
                        data$persistence_humanities,
                        data$score_AMAS_total,
                        data$score_AMAS_learning,
                        data$score_AMAS_testing,
                        data$score_GAD,
                        data$score_STAI_state_short,
                        data$score_TAI_short,
                        data$score_SDQ_M,
                        data$score_SDQ_L,
                        data$score_PISA_ME,
                        data$score_BFI_N,
                        data$preference_teaching_language,
                        data$preference_teaching_mathematics,
                        data$preference_teaching_science,
                        data$ease_teaching_language,
                        data$ease_teaching_mathematics,
                        data$ease_teaching_science,
                        data$score_FSMAS_SE)




# since the columns of a dataframe have to be of equal length, all entries of the 
# teaching preference- and ease-vectors were included first, even "invalid" ones
# (i.e. -1 = "does not apply to me"), to create the dataframe above.
# Now set all entries to NA that are not in the valid answering-range (1-5), so that
# they are excluded when the correlationmatrix is computed
data_corr$data.preference_teaching_language[!(data_corr$data.preference_teaching_language %in% 1:5)] <- NA
data_corr$data.preference_teaching_mathematics[!(data_corr$data.preference_teaching_mathematics %in% 1:5)] <- NA
data_corr$data.preference_teaching_science[!(data_corr$data.preference_teaching_science %in% 1:5)] <- NA
data_corr$data.ease_teaching_language[!(data_corr$data.ease_teaching_language %in% 1:5)] <- NA
data_corr$data.ease_teaching_mathematics[!(data_corr$data.ease_teaching_mathematics %in% 1:5)] <- NA
data_corr$data.ease_teaching_science[!(data_corr$data.ease_teaching_science %in% 1:5)] <- NA


# rename columns of the data frame
names(data_corr) <- c("Arithmetic performance", 
                      "Math grade", 
                      "Math influence study choice", 
                      "Liking math", 
                      "Liking science", 
                      "Liking humanities", 
                      "Persistence math", 
                      "Persistence science", 
                      "Persistence humanities", 
                      "Math anxiety total", 
                      "Math anxiety learning", 
                      "Math anxiety testing", 
                      "Trait anxiety", 
                      "State anxiety", 
                      "Test anxiety", 
                      "Math self-concept", 
                      "Language self-concept", 
                      "Math self-efficacy", 
                      "Neuroticism",
                      "Preference teaching language", 
                      "Preference teaching math", 
                      "Preference teaching science", 
                      "Ease teaching language", 
                      "Ease teaching math", 
                      "Ease teaching science",
                      "Math-gender stereotype endorsement")


# create correlation-matrix
correlationmatrix <- cor(data_corr, method = "pearson", use = "pairwise.complete.obs")

# round to two decimals
correlationmatrix <- round(correlationmatrix, 2)

# fill the matrix as follows:
# - replace the 1s on the diagonal with "-"
# - remove the values on the upper half of the diagonal since they are redundant
# - if the correlation between the two variables is significant (after pearson, alpha = 0.05),
#   mark the entry in the matrix with a star (*)

# number of variables 
nv <- ncol(data_corr)

for(i in 1:nv) {
  for(j in 1:nv) {
    if(j > i) {                         # i.e. if the value is on the upper half of the diagonal
      correlationmatrix[i, j] <- " "
    } else if (j == i) {                 # i.e. if the value is on the diagonal
      correlationmatrix[i, j] <- "-"
    }
    else {                              # i.e. if the value is on the lower half of the diagonal
      # compute the p value of the correlation
      p_value_tmp <- cor.test(data_corr[, i], data_corr[, j], method = "pearson")$p.value
      if(p_value_tmp < 0.05) {
        correlationmatrix[i, j] <- paste(as.character(correlationmatrix[i, j]), "*", sep = "")
      }
      if(p_value_tmp < 0.01) {
        correlationmatrix[i, j] <- paste(as.character(correlationmatrix[i, j]), "*", sep = "")
      }
      if(p_value_tmp < 0.001) {
        correlationmatrix[i, j] <- paste(as.character(correlationmatrix[i, j]), "*", sep = "")
      }
    }
  }
}

# save Matrix as .txt file readable for Excel
write.table(correlationmatrix, "Correlationmatrix.txt", sep = ",", col.names = NA)


# --------------------------------------------------------------------------------
# compute a matrix that tells the N of each correlation of "correlationmatrix"


# copy of the correlationmatrix as a basis for our Matrix of Ns
correlationmatrix_nums <- correlationmatrix

# fill the entries of the copied correlationmatrix with the number of used observations
# for each correlation
for(i in 1:nv) {
  for(j in 1:nv) {
    if(j > i) {
      correlationmatrix_nums[i, j] <- " "
    } else if(j == i) {
      correlationmatrix_nums[i, j] <- "-"
    } else {
      # the "&"-operation returns a vector with NA at position i if at least one of the 
      # two vectors has a NA at position i. If there is no NA at position i in both vectors
      # "&" returns T or F at this location...
      tmp <- data_corr[ ,i] & data_corr[ ,j]
      # ... therefore we can get the number of complete pairs by removing the NA values 
      # from the resulting temporary vector and computing it's length...
      tmp_length <- length(tmp[!is.na(tmp)])
      # ... and save the number at the corresponding position in the matrix
      correlationmatrix_nums[i, j] <- tmp_length
    }
  }
}

# save the Matrix with the N of each correlation as .txt file readable for Excel
write.table(correlationmatrix_nums, "N_of_correlations.txt", sep = ",", col.names = NA)




# --------------------------------------------------------------------------------
# Table descriptives
# --------------------------------------------------------------------------------

# load packages
library(tidyverse)


n_all <- as.numeric(length(data$id_unique))
n_s1 <- data %>% filter(sample == "german_students") %>% count() %>% as.numeric()
n_s2 <- data %>% filter(sample == "german_teachers") %>% count() %>% as.numeric()
n_s3 <- data %>% filter(sample == "belgian_teachers") %>% count() %>% as.numeric()

# --------------------------------------------------------------------------------
# Age
# --------------------------------------------------------------------------------


age_range_all <- data %>% group_by(age_range) %>% count()
age_range_all <- age_range_all[c(4,1:3,5),]
age_s1 <- data %>% filter(sample == "german_students") %>% summarise(mean(age)) %>% as.numeric()
age_s1_sd <- data %>% filter(sample == "german_students") %>% summarise(sd(age)) %>% as.numeric()
age_range_s1 <- data %>% filter(sample == "german_students") %>% group_by(age_range) %>% count()
age_range_s1 <- age_range_s1[c(4,1:3,5),]
age_range_s2 <- data %>% filter(sample == "german_teachers") %>% group_by(age_range) %>% count()
age_range_s2 <- age_range_s2[c(4,1:3,5),]
age_range_s3 <- data %>% filter(sample == "belgian_teachers") %>% group_by(age_range) %>% count()
age_range_s3 <- age_range_s3[c(4,1:3,5),]



# age_all <- mean(data$age)
# age_all_sd <- sd(data$age)
# age_s2 <- data %>% filter(sample == "german_teachers") %>% summarise(mean(age)) %>% as.numeric()
# age_s2_sd <- data %>% filter(sample == "german_teachers") %>% summarise(sd(age)) %>% as.numeric()
# age_s3 <- data %>% filter(sample == "belgian_teachers") %>% summarise(mean(age)) %>% as.numeric()
# age_s3_sd <- data %>% filter(sample == "belgian_teachers") %>% summarise(sd(age)) %>% as.numeric()

# --------------------------------------------------------------------------------
# Gender
# --------------------------------------------------------------------------------

fem_all <- data  %>% filter(sex == "f") %>% count() %>% as.numeric()
fem_s1 <- data  %>% filter(sample == "german_students", sex == "f") %>% count() %>% as.numeric()
fem_s2 <- data  %>% filter(sample == "german_teachers", sex == "f") %>% count() %>% as.numeric()
fem_s3 <- data  %>% filter(sample == "belgian_teachers", sex == "f") %>% count() %>% as.numeric()

# --------------------------------------------------------------------------------
# Number per math proportion
# --------------------------------------------------------------------------------

n_math_majors_s1 <- data %>% filter(sample == "german_students") %>% count(math_load)

# --------------------------------------------------------------------------------
# Number teachers in-service and pre-service
# --------------------------------------------------------------------------------
n_inservice_s2 <- data %>% filter(sample == "german_teachers", teacher_experience == "in-service") %>% count() %>% as.numeric()
n_inservice_s3 <- data %>% filter(sample == "belgian_teachers", teacher_experience == "in-service") %>% count() %>% as.numeric()

n_preservice_s2 <- data %>% filter(sample == "german_teachers", teacher_experience == "pre-service") %>% count() %>% as.numeric()
n_preservice_s3 <- data %>% filter(sample == "belgian_teachers", teacher_experience == "pre-service") %>% count() %>% as.numeric()


# --------------------------------------------------------------------------------
# Number teachers with main focus math and non-math focus
# --------------------------------------------------------------------------------

n_math_focus_s2 <- data %>% filter(sample == "german_teachers", main_focus_mathematics == "yes") %>% count() %>% as.numeric()
n_math_focus_s3 <- data %>% filter(sample == "belgian_teachers", main_focus_mathematics == "yes") %>% count() %>% as.numeric()

n_NONmath_focus_s2 <- data %>% filter(sample == "german_teachers", number_main_foci > 0, main_focus_mathematics == "no") %>% count() %>% as.numeric()
n_NONmath_focus_s3 <- data %>% filter(sample == "belgian_teachers", number_main_foci > 0, main_focus_mathematics == "no") %>% count() %>% as.numeric()


# --------------------------------------------------------------------------------
# Creating a single table
# --------------------------------------------------------------------------------

# overall descriptives
desc_all <- c(n_all, "", age_range_all$n, paste0(fem_all, "/", n_all - fem_all), "", "", "")

# S1 descriptives
desc_s1 <- c(n_s1, paste0(round(age_s1, 2), " (", round(age_s1_sd, 2), ")"), age_range_s1$n, paste0(fem_s1, "/", n_s1 - fem_s1), paste(shQuote(n_math_majors_s1$n), collapse=", "), "", "")

# S2 descriptives
desc_s2 <- c(n_s2, "", age_range_s2$n, paste0(fem_s2, "/", n_s2 - fem_s2), "", paste0(n_math_focus_s2, "/", n_NONmath_focus_s2), paste0(n_inservice_s2, "/", n_preservice_s2))

# S3 descriptives
desc_s3 <- c(n_s3, "", age_range_s3$n, paste0(fem_s3, "/", n_s3 - fem_s3), "", paste0(n_math_focus_s3, "/", n_NONmath_focus_s3), paste0(n_inservice_s3, "/", n_preservice_s3))

# row names
variables <- c("N", "Age M (SD)", age_range_all$age_range, "Females/Males", "Math proportion (low, medium, high, other)", "Math focus", "In-service/Pre-service")

# table
table_descr <- as.data.frame(cbind(variables, desc_all, desc_s1, desc_s2, desc_s3))

write.csv(table_descr, "table_descriptives.csv")



# --------------------------------------------------------------------------------
# Test if the output files of the script can be read correctly
# --------------------------------------------------------------------------------

test1 <- read.table("Table_of_instruments.txt", sep = ",", header = T)
# row.names = 1 is necessary because the .txt file is made readable for Excel
test2 <- read.table("Correlationmatrix.txt", sep = ",", header = T, row.names = 1)
test3 <- read.table("N_of_correlations.txt", sep = ",", header = T, row.names = 1)
test4 <- read.table("table_descriptives.csv", sep = ",", header = T, row.names = 1)


# ================================================================================
# End script
# ================================================================================