#  May 2019; January 2024  
#  Authors: Julius Gervelmeyer, Maristella Lunardon and Andreas Schmitt
# Downloaded from https://osf.io/gszpb/

#  R-script for preprocessing the math-anxiety data from the belgian students and german students and teachers


# clear workspace. To avoid R-cache related errors, please use a new session.
rm(list=ls(all=TRUE))

# libraries
library(plyr)

# set working directory
wd <- setwd("/home/maristella/Desktop/AMATUS/material/VALID_VERSION_Everything_Cipora/creating bigdata - Maristella's changes for publication")


# load preprocessing() function from working directory
source(paste0(wd,'/functions/Preprocessing.R'))

##### Belgian Data #####

# loading the data
data <- read.csv("data_belgian_teachingstudents.csv", fileEncoding = "UCS-2LE", header = TRUE, dec = ",", na.strings = " ",
                 sep = "\t", stringsAsFactors = FALSE)

# creating a variable, that counts the number of participants from one on
for (i in 1:nrow(data)) {
  data$participant_number[i] <- i
}

# creatiung a column for high-in-math subjects like math, cog-sci, it and others
data$math_related_subject <- NA
data <- cbind(data[,1:13], data$math_related_subject, data[,15:ncol(data) - 1])

# creatiung a column for specific focus subjects
data$spec_focus <- NA
data <- cbind(data[,1:118], data$spec_focus, data[,120:ncol(data) - 1])

# creatiung a column for time spent at lottery page
data$time_lottery <- 0
data <- cbind(data[,1:165], data$time_lottery, data[,167:ncol(data) - 1])


# creating a name for each column in the table
names <- c("case", "serial", "ref", "questnnr", "mode", "start_time", "age", "sex",
           "native_speaker", "math_grade", "math_load", "study_program_low", 
           "study_program_middle", "study_program_high", "study_program_other",
           "math_influence_on_program_choice", "AMAS1", "AMAS2", "AMAS3", "AMAS4",
           "AMAS5", "AMAS6", "AMAS7", "AMAS8", "AMAS9", "GAD1", "GAD2","GAD3", "GAD4",
           "GAD5", "GAD6", "GAD7", "STAI1", "STAI2","STAI3", "STAI4", "STAI5", "TAI1",
           "TAI2","TAI3","TAI4","TAI5", "SDQ_M1","SDQ_M2_r", "SDQ_M3", "SDQ_M4_r",
           "SDQ_L1_r", "SDQ_L2_r", "SDQ_L3", "SDQ_L4", "PISA1", "PISA2", "PISA3", "PISA4",
           "PISA5", "PISA6", "liking_math", "liking_science", "liking_humanities", "persistence_math",
           "persistence_science", "persistence_humanities", "BFI1", "BFI2_r", "BFI3","BFI4", "BFI5_r",
           "BFI6", "BFI7_r","BFI8", "math_perf1_resp", "math_perf2_resp", "math_perf3_resp",
           "math_perf4_resp", "math_perf5_resp", "math_perf6_resp", "math_perf7_resp",
           "math_perf8_resp","math_perf9_resp", "math_perf10_resp", "math_perf11_resp",
           "math_perf12_resp", "math_perf13_resp","math_perf14_resp", "math_perf15_resp",
           "math_perf16_resp", "math_perf17_resp", "math_perf18_resp","math_perf19_resp",
           "math_perf20_resp", "math_perf21_resp", "math_perf22_resp", "math_perf23_resp",
           "math_perf24_resp", "math_perf25_resp", "math_perf26_resp", "math_perf27_resp",
           "math_perf28_resp","math_perf29_resp", "math_perf30_resp", "math_perf31_resp", 
           "math_perf32_resp", "math_perf33_resp","math_perf34_resp", "math_perf35_resp",
           "math_perf36_resp", "math_perf37_resp", "math_perf38_resp","math_perf39_resp",
           "math_perf40_resp", "GS01", "GS02", "GS02_03", "GS02_04", "GS03", "GS03_13", 
           "GS03_14", "GS03_08", "GS_03_08a", "GS03_01", "GS03_02", "GS03_05", "GS03_06", "GS03_07",
           "GS03_11", "GS03_10", "GS03_10a", "GS03_12", "GS04_01", "GS04_02", "GS04_03",
           "GS05_01", "GS05_02", "GS05_03", "SE02_01", "SE02_02", "SE02_03", "SE02_04",
           "SE02_05", "SE02_06", "SE02_07", "SE02_08", "SE02_09", "honesty", "breaks",
           "noise", "device", "comment", "time_info", "time_demographs", "time_BFI", 
           "time_GAD7", "time_TAI","time_AMAS", "time_SDQ_Math", "time_SDQ_Lang", 
           "time_PISA", "time_liking", "time_persistence", "time_explanation_math_test",
           "time_math_performance", "time_STAI", "time_GS", "time_SE", "time_quality", "time_lottery",
           "time_sum", "mail_sent", "end_time", "finished", "Q_VIEWER", "last_page", "MAXPAGE",
           "MISSING", "MISSREL","TIME_RSI", "DEG_TIME", "partNo")

# checking dimensions
print("Length of names-vector: ")
length(names)
print("Length of the table: ")
dim(data)[2]

# set the names
names(data) <- names

# preprocess belgian data
data_belgians <- preprocessing(data, 'belgian_teaching_staff')


##### German teachers data #####

# loading the data
# note: deleted second header-row in advance!
data <- read.csv("data_german_teachers.csv",  header = TRUE, dec = ",", na.strings = " ", sep = ";", stringsAsFactors = FALSE)

# creating a variable, that counts the number of participants from one on
for (i in 1:nrow(data)) {
  data$participant_number[i] <- i
}

# creating a name for each column in the table
names <- c("case", "serial", "ref", "questnnr", "mode", "start_time", "age", "sex",
           "native_speaker", "math_grade", "math_load", "study_program_low", 
           "study_program_middle", "study_program_high", "study_program_other",
           "math_influence_on_program_choice", "AMAS1", "AMAS2", "AMAS3", "AMAS4",
           "AMAS5", "AMAS6", "AMAS7", "AMAS8", "AMAS9", "GAD1", "GAD2","GAD3", "GAD4",
           "GAD5", "GAD6", "GAD7", "STAI1", "STAI2","STAI3", "STAI4", "STAI5", "TAI1",
           "TAI2","TAI3","TAI4","TAI5", "SDQ_M1","SDQ_M2_r", "SDQ_M3", "SDQ_M4_r",
           "SDQ_L1_r", "SDQ_L2_r", "SDQ_L3", "SDQ_L4", "PISA1", "PISA2", "PISA3", "PISA4",
           "PISA5", "PISA6", "liking_math", "liking_science", "liking_humanities", "persistence_math",
           "persistence_science", "persistence_humanities", "BFI1", "BFI2_r", "BFI3","BFI4", "BFI5_r",
           "BFI6", "BFI7_r","BFI8", "math_perf1_resp", "math_perf2_resp", "math_perf3_resp",
           "math_perf4_resp", "math_perf5_resp", "math_perf6_resp", "math_perf7_resp",
           "math_perf8_resp","math_perf9_resp", "math_perf10_resp", "math_perf11_resp",
           "math_perf12_resp", "math_perf13_resp","math_perf14_resp", "math_perf15_resp",
           "math_perf16_resp", "math_perf17_resp", "math_perf18_resp","math_perf19_resp",
           "math_perf20_resp", "math_perf21_resp", "math_perf22_resp", "math_perf23_resp",
           "math_perf24_resp", "math_perf25_resp", "math_perf26_resp", "math_perf27_resp",
           "math_perf28_resp","math_perf29_resp", "math_perf30_resp", "math_perf31_resp", 
           "math_perf32_resp", "math_perf33_resp","math_perf34_resp", "math_perf35_resp",
           "math_perf36_resp", "math_perf37_resp", "math_perf38_resp","math_perf39_resp",
           "math_perf40_resp", "GS01", "GS02", "GS02_03", "GS02_04", "GS03", "GS03_13", 
           "GS03_14", "GS03_08", "GS_03_08a", "GS03_01", "GS03_02", "GS03_05", "GS03_06", "GS03_07",
           "GS03_11", "GS03_10", "GS03_10a", "GS03_12", "GS04_01", "GS04_02", "GS04_03",
           "GS05_01", "GS05_02", "GS05_03", "SE02_01", "SE02_02", "SE02_03", "SE02_04",
           "SE02_05", "SE02_06", "SE02_07", "SE02_08", "SE02_09", "honesty", "breaks",
           "noise", "device", "comment", "time_info", "time_demographs", "time_BFI", 
           "time_GAD7", "time_TAI","time_AMAS", "time_SDQ_Math", "time_SDQ_Lang", 
           "time_PISA", "time_liking", "time_persistence", "time_explanation_math_test",
           "time_math_performance", "time_STAI", "time_GS", "time_SE", "time_quality", "time_lottery",
           "time_sum", "mail_sent", "end_time", "finished", "Q_VIEWER", "last_page", "MAXPAGE",
           "MISSING", "MISSREL","TIME_RSI", "DEG_TIME", "partNo")

# checking
print("Length of names-vector: ")
length(names)
print("Length of the table: ")
dim(data)[2]

# set the names
names(data) <- names

# preprocess german teachers' data
data_german_teachers <- preprocessing(data, 'german_teaching_staff')

##### German students data #####

# loading the data
data <- read.table("data_german_students.txt",  header = TRUE, dec = ",", na.strings = " ", sep = "\t", stringsAsFactors = FALSE)

# creating a variable, that counts the number of participants from one on
for (i in 1:nrow(data)) {
  data$participant_number[i] <- i
}

# add empty colums for GS and SE -tests
for (i in 1:33) {
  data[,ncol(data)+1] <- NA
}

# fill in GS and SE columns
data <- cbind(data[,1:grep("ML01_40", colnames(data))], 
              data[,140:ncol(data)], 
              data[,grep("QI01", colnames(data)):grep("participant_number", colnames(data))])

# add QI05
# add Time 14-18
for (i in 1:6) {
  data[,ncol(data)+1] <- NA
}
data <- cbind(data[,1:grep("QI03", colnames(data))], 
              data[,grep("participant_number", colnames(data))+1], # 1 new column
              data[,grep("QI04_01", colnames(data)):grep("TIME013", colnames(data))], 
              data[,(ncol(data)-4):ncol(data)],
              data[,grep("TIME_SUM", colnames(data)):grep("participant_number", colnames(data))])

# creating a name for each column in the table
names <- c("case", "serial", "ref", "questnnr", "mode", "start_time", "age", "sex",
           "native_speaker", "math_grade", "math_load", "study_program_low", 
           "study_program_middle", "study_program_high", "study_program_other",
           "math_influence_on_program_choice", "AMAS1", "AMAS2", "AMAS3", "AMAS4",
           "AMAS5", "AMAS6", "AMAS7", "AMAS8", "AMAS9", "GAD1", "GAD2","GAD3", "GAD4",
           "GAD5", "GAD6", "GAD7", "STAI1", "STAI2","STAI3", "STAI4", "STAI5", "TAI1",
           "TAI2","TAI3","TAI4","TAI5", "SDQ_M1","SDQ_M2_r", "SDQ_M3", "SDQ_M4_r",
           "SDQ_L1_r", "SDQ_L2_r", "SDQ_L3", "SDQ_L4", "PISA1", "PISA2", "PISA3", "PISA4",
           "PISA5", "PISA6", "liking_math", "liking_science", "liking_humanities", "persistence_math",
           "persistence_science", "persistence_humanities", "BFI1", "BFI2_r", "BFI3","BFI4", "BFI5_r",
           "BFI6", "BFI7_r","BFI8", "math_perf1_resp", "math_perf2_resp", "math_perf3_resp",
           "math_perf4_resp", "math_perf5_resp", "math_perf6_resp", "math_perf7_resp",
           "math_perf8_resp","math_perf9_resp", "math_perf10_resp", "math_perf11_resp",
           "math_perf12_resp", "math_perf13_resp","math_perf14_resp", "math_perf15_resp",
           "math_perf16_resp", "math_perf17_resp", "math_perf18_resp","math_perf19_resp",
           "math_perf20_resp", "math_perf21_resp", "math_perf22_resp", "math_perf23_resp",
           "math_perf24_resp", "math_perf25_resp", "math_perf26_resp", "math_perf27_resp",
           "math_perf28_resp","math_perf29_resp", "math_perf30_resp", "math_perf31_resp", 
           "math_perf32_resp", "math_perf33_resp","math_perf34_resp", "math_perf35_resp",
           "math_perf36_resp", "math_perf37_resp", "math_perf38_resp","math_perf39_resp",
           "math_perf40_resp", "GS01", "GS02", "GS02_03", "GS02_04", "GS03", "GS03_13", 
           "GS03_14", "GS03_08", "GS_03_08a", "GS03_01", "GS03_02", "GS03_05", "GS03_06", "GS03_07",
           "GS03_11", "GS03_10", "GS03_10a", "GS03_12", "GS04_01", "GS04_02", "GS04_03",
           "GS05_01", "GS05_02", "GS05_03", "SE02_01", "SE02_02", "SE02_03", "SE02_04",
           "SE02_05", "SE02_06", "SE02_07", "SE02_08", "SE02_09", "honesty", "breaks",
           "noise", "device", "comment", "time_info", "time_demographs", "time_BFI", 
           "time_GAD7", "time_TAI","time_AMAS", "time_SDQ_Math", "time_SDQ_Lang", 
           "time_PISA", "time_liking", "time_persistence", "time_explanation_math_test",
           "time_math_performance", "time_STAI", "time_GS", "time_SE", "time_quality", "time_lottery",
           "time_sum", "mail_sent", "end_time", "finished", "Q_VIEWER", "last_page", "MAXPAGE",
           "MISSING", "MISSREL","TIME_RSI", "DEG_TIME", "partNo")

# checking
print("Length of names-vector: ")
length(names)
print("Length of the table: ")
dim(data)[2]

# set the names
names(data) <- names

library(dplyr)
# math proportion: recathegorizing response "other"
data2 <- data %>%
  filter(math_load == 4) %>%
  mutate(math_load = case_when(study_program_other == "Arch\xe4ologie"|
                                       study_program_other == "Sportwissenschaften "|
                                       study_program_other == "Sozialp\xe4dagogik"|
                                       study_program_other == "Erziehungswissenschaften"|
                                       study_program_other == "Master Erwachsenenbildung/Wieterbildung"|
                                       study_program_other == "Italienisch,spanisch,katholische Theologie gympo"|
                                       study_program_other == "Japanologie"|
                                       study_program_other =="Koreanistik"|
                                       study_program_other == "Sinologie"|
                                       study_program_other == "Sprachen, Geschichte und Kulturen des Nahen Ostens"|
                                       study_program_other == "Jura"|
                                       study_program_other == "Katholische Theologie,Politik und Wirtschaft"
                                     ~ 1,
                                     study_program_other == "Geo\xf6kologie"|
                                       study_program_other =="Applied Environmental Geoscience"|
                                       study_program_other =="Geowissenschaft"|
                                       study_program_other =="Geowissenschaften "|
                                       study_program_other =="Umweltnaturwissenschaften"|
                                       study_program_other =="BWL Logistik"|
                                       study_program_other =="International Business Administration"| 
                                       study_program_other =="Internationaler Technischer Vertrieb"|
                                       study_program_other =="M.A. Literaturwissenschaft + M.Sc. Wirtschaftswissenschaft"|
                                       study_program_other =="Wirtschaftswissenschaften "|
                                       study_program_other =="Psychologie (NF)"|
                                       study_program_other =="Soziologie"|
                                       study_program_other =="Soziologie "|
                                       study_program_other =="Lehramt Chemie und Theologie"|
                                       study_program_other =="Lehramt Chemie, NwT und Erziehungswissenschaften"|
                                       study_program_other =="Soziologie, Kriminologie" 
                                     ~ 2,
                                     study_program_other =="Medizintechnik"|
                                       study_program_other == "Bauingenieurwesen" 
                                     ~ 3,
                                     study_program_other == "Leibniz Kolleg"|
                                       study_program_other == "Lehramt"
                                     ~ NA
    
  ))

detach("package:dplyr", unload=TRUE)
data <- rbind(subset(data, math_load != 4), data2)

rm(list = "data2")


# fill empty time variables with zeros
data$time_STAI <- 0
data$time_GS <- 0
data$time_SE <- 0
data$time_quality <- 0
data$time_lottery <- 0

# preprocess german students' data
data_german_students <- preprocessing(data, "german_students")

# glue it and write to file
bigdata <- rbind(data_german_students,data_german_teachers,data_belgians)

# replace "," with "." in total_time_minutes
bigdata$total_time_minutes <- gsub(",", ".", bigdata$total_time_minutes, fixed = TRUE)

# convert time into numeric
bigdata$total_time_minutes <- floor(as.numeric(bigdata$total_time_minutes))

# recode GS03_*
for (i in 1:length(bigdata[,1])) {
  if (!is.na(bigdata$GS03_13[i])) {
    bigdata$GS03_13[i] <- ifelse(bigdata$GS03_13[i] == 2, "yes", "no")
  }
  if (!is.na(bigdata$GS03_14[i])) {
    bigdata$GS03_14[i] <- ifelse(bigdata$GS03_14[i] == 2, "yes", "no")
  }
  if (!is.na(bigdata$GS03_08[i])) {
    bigdata$GS03_08[i] <- ifelse(bigdata$GS03_08[i] == 2, "yes", "no")
  }
  if (!is.na(bigdata$GS03_01[i])) {
    bigdata$GS03_01[i] <- ifelse(bigdata$GS03_01[i] == 2, "yes", "no")
  }
  if (!is.na(bigdata$GS03_02[i])) {
    bigdata$GS03_02[i] <- ifelse(bigdata$GS03_02[i] == 2, "yes", "no")
  }
  if (!is.na(bigdata$GS03_05[i])) {
    bigdata$GS03_05[i] <- ifelse(bigdata$GS03_05[i] == 2, "yes", "no")
  }
  if (!is.na(bigdata$GS03_06[i])) {
    bigdata$GS03_06[i] <- ifelse(bigdata$GS03_06[i] == 2, "yes", "no")
  }  
  if (!is.na(bigdata$GS03_07[i])) {
    bigdata$GS03_07[i] <- ifelse(bigdata$GS03_07[i] == 2, "yes", "no")
  }  
  if (!is.na(bigdata$GS03_11[i])) {
    bigdata$GS03_11[i] <- ifelse(bigdata$GS03_11[i] == 2, "yes", "no")
  }  
  if (!is.na(bigdata$GS03_10[i])) {
    bigdata$GS03_10[i] <- ifelse(bigdata$GS03_10[i] == 2, "yes", "no")
  }
  if (!is.na(bigdata$GS03_12[i])) {
    bigdata$GS03_12[i] <- ifelse(bigdata$GS03_12[i] == 2, "yes", "no")
  }
}

# column for id_unique
id <- seq(1:length(bigdata[,1]))
bigdata <- cbind(id, bigdata)

# removing start and end time
bigdata$start_time <- NULL
bigdata$end_time <- NULL


# new colnames
colnames(bigdata) <- c("id_unique", "id_sample", "sample", "sex", "age", "native_speaker", "math_grade", "math_load",
                       "math_inf_program_choice", "honesty","breaks", "noise", "device",
                       "total_time_minutes", "liking_math", "liking_science", "liking_humanities", "persistence_math",
                       "persistence_science", "persistence_humanities", "score_AMAS_total", "score_AMAS_learning",
                       "score_AMAS_testing", "AMAS1", "AMAS2", "AMAS3", "AMAS4", "AMAS5", "AMAS6", "AMAS7", "AMAS8",
                       "AMAS9", "score_GAD", "GAD1", "GAD2", "GAD3", "GAD4", "GAD5", "GAD6", "GAD7",
                       "score_STAI_state_short", "STAI1", "STAI2", "STAI3", "STAI4", "STAI5", "score_TAI_short",
                       "TAI1", "TAI2", "TAI3", "TAI4", "TAI5", "score_SDQ_M", "SDQ_M1", "SDQ_M2", "SDQ_M3", "SDQ_M4",
                       "score_SDQ_L", "SDQ_L1", "SDQ_L2", "SDQ_L3", "SDQ_L4", "score_PISA_ME", "PISA_ME1", "PISA_ME2",
                       "PISA_ME3", "PISA_ME4", "PISA_ME5", "PISA_ME6", "score_BFI_N", "BFI_N1", "BFI_N2", "BFI_N3",
                       "BFI_N4", "BFI_N5", "BFI_N6", "BFI_N7", "BFI_N8", "teacher", "teacher_stage", 
                       "teacher_duration", "other_current_activity", "number_main_foci", "main_focus_french", 
                       "main_focus_mathematics", "main_focus_sachunterricht", "main_focus_special", "main_focus_english",
                       "main_focus_german", "main_focus_religious_pedagogy", "main_focus_art", "main_focus_music",
                       "main_focus_sport", "main_focus_other", "main_focus_other_which", "main_focus_not_applicable",
                       "preference_teaching_language", "preference_teaching_mathematics", "preference_teaching_science",
                       "ease_teaching_language", "ease_teaching_mathematics", "ease_teaching_science",
                       "score_FSMAS_SE", "FSMAS_SE1", "FSMAS_SE2",
                       "FSMAS_SE3", "FSMAS_SE4", "FSMAS_SE5", "FSMAS_SE6", "FSMAS_SE7", "FSMAS_SE8",
                       "FSMAS_SE9", "sum_arith_perf", "arith_perf_correct_order", "arith_perf1_acc", "arith_perf2_acc",
                       "arith_perf3_acc", "arith_perf4_acc", "arith_perf5_acc", "arith_perf6_acc", "arith_perf7_acc",
                       "arith_perf8_acc", "arith_perf9_acc", "arith_perf10_acc", "arith_perf11_acc", "arith_perf12_acc",
                       "arith_perf13_acc", "arith_perf14_acc", "arith_perf15_acc", "arith_perf16_acc",
                       "arith_perf17_acc", "arith_perf18_acc", "arith_perf19_acc", "arith_perf20_acc",
                       "arith_perf21_acc", "arith_perf22_acc", "arith_perf23_acc", "arith_perf24_acc",
                       "arith_perf25_acc", "arith_perf26_acc", "arith_perf27_acc", "arith_perf28_acc",
                       "arith_perf29_acc", "arith_perf30_acc", "arith_perf31_acc", "arith_perf32_acc",
                       "arith_perf33_acc", "arith_perf34_acc", "arith_perf35_acc", "arith_perf36_acc",
                       "arith_perf37_acc", "arith_perf38_acc", "arith_perf39_acc", "arith_perf40_acc",
                       "arith_perf1_resp", "arith_perf2_resp", "arith_perf3_resp", "arith_perf4_resp",
                       "arith_perf5_resp", "arith_perf6_resp", "arith_perf7_resp", "arith_perf8_resp",
                       "arith_perf9_resp", "arith_perf10_resp", "arith_perf11_resp", "arith_perf12_resp",
                       "arith_perf13_resp", "arith_perf14_resp", "arith_perf15_resp", "arith_perf16_resp",
                       "arith_perf17_resp", "arith_perf18_resp", "arith_perf19_resp", "arith_perf20_resp",
                       "arith_perf21_resp", "arith_perf22_resp", "arith_perf23_resp", "arith_perf24_resp",
                       "arith_perf25_resp", "arith_perf26_resp", "arith_perf27_resp", "arith_perf28_resp",
                       "arith_perf29_resp", "arith_perf30_resp", "arith_perf31_resp", "arith_perf32_resp",
                       "arith_perf33_resp", "arith_perf34_resp", "arith_perf35_resp", "arith_perf36_resp",
                       "arith_perf37_resp", "arith_perf38_resp", "arith_perf39_resp", "arith_perf40_resp")

# recode honesty
bigdata$honesty <- ifelse(bigdata$honesty == 1, "yes", "no")

# recode breaks
bigdata$breaks <- ifelse(bigdata$breaks == 1, "yes", "no")

# recode device
for (i in 1:length(bigdata[,1])) {
  if (!is.na(bigdata$device[i])) {
    if (bigdata$device[i] == "1") {
      bigdata$device[i] <- "computer/laptop"
    }
    if (bigdata$device[i] == "2") {
      bigdata$device[i] <- "tablet"
    }
    if (bigdata$device[i] == "3") {
      bigdata$device[i] <- "smartphone"
    }
    if (bigdata$device[i] == "4") {
      bigdata$device[i] <- "e-reader"
    }
    if (bigdata$device[i] == "5") {
      bigdata$device[i] <- "other"
    }
  }
}

# recode arith_perf_correct_order
bigdata$arith_perf_correct_order <- ifelse(bigdata$arith_perf_correct_order == 1, "yes", "no")

# recode teacher
for (i in 1:length(bigdata[,1])) {
  if (!is.na(bigdata$teacher[i])) {
    bigdata$teacher[i] <- ifelse(bigdata$teacher[i] == 1, "yes", "no")
  }
}

# recode teacher_stage
for (i in 1:length(bigdata[,1])) {
  if (!is.na(bigdata$teacher_stage[i])) {
    if (bigdata$teacher_stage[i] == "1") {
      bigdata$teacher_stage[i] <- "study"
    }
    if (bigdata$teacher_stage[i] == "2") {
      bigdata$teacher_stage[i] <- "internship/probation period"
    }
    if (bigdata$teacher_stage[i] == "3") {
      bigdata$teacher_stage[i] <- "primary school teacher"
    }
    if (bigdata$teacher_stage[i] == "4") {
      bigdata$teacher_stage[i] <- "other"
    }
  }
}

# finally: fixing some dataset specific participant errors
#View(bigdata[bigdata$teacher_stage == "other",c(1,80:83)])
# -> ids 1038, 1054, 1083 selected category "other" correctly. All others are students.
list_of_ids_to_fix <- list_of_ids_to_fix <- bigdata$id_unique[bigdata$teacher_stage == "other" & !is.na(bigdata$teacher_stage)]
list_of_ids_to_fix <- list_of_ids_to_fix[-c((which(list_of_ids_to_fix==1038)),(which(list_of_ids_to_fix==1054)),(which(list_of_ids_to_fix==1083)))]
for (id in list_of_ids_to_fix) {
  bigdata$teacher_stage[bigdata$id_unique == id] = "study"
  bigdata$other_current_activity[bigdata$id_unique == id] = ""
}
# replace all "," with ".", especially participants' entries in GS02_03
bigdata <- as.data.frame(lapply(bigdata, gsub, pattern=',', replacement='.', fixed = TRUE))




# load packages
library(tidyverse)


##### Variable recategorization #####

# First, manual change of one response. 
# A subject indicated some main foci but also "not applicable".
# For this subject, I change the response from "yes" to "no" in not applicable.
bigdata[which(bigdata$main_focus_not_applicable == "yes" & bigdata$number_main_foci != 0),"main_focus_not_applicable"] <- "no"



### Teacher main foci ###

# Teacher could choose between one or more of these main foci: 
# among science lessons (if they chose this, they had to specify which special main focus), 
# French, mathematics, science, English, German, religious pedagogy, art, music and sport. 
# They could also choose "other" and write their focus.
# German students who chose Sachunterricht had to specify their focus.
# New main foci: literacy, mathematics, science, humanities.
bigdata$main_focus_literacy <- NA
bigdata$main_focus_humanities <- NA
bigdata$main_focus_science <- NA
bigdata$main_focus_mathematics2 <- NA

bigdata2 <- bigdata %>%
  filter(sample != "german_students") %>%
  mutate(main_focus_literacy = case_when(
    sample == "german_teaching_staff" & main_focus_german  == "yes" ~ "yes",
    sample == "belgian_teaching_staff" & main_focus_french  == "yes" ~ "yes",
  )) %>%
  mutate(main_focus_humanities = case_when(
    sample == "german_teaching_staff" & main_focus_french == "yes" ~ "yes",
    sample == "belgian_teaching_staff" & main_focus_german  == "yes" ~ "yes",
    main_focus_english == "yes"| 
      main_focus_religious_pedagogy == "yes"| 
      main_focus_art == "yes"| 
      main_focus_music == "yes"| 
      main_focus_sport == "yes"| 
      main_focus_special == "economy"| 
      main_focus_special == "everyday culture"|
      main_focus_special == "everyday culture and health"| 
      main_focus_special  == "history"| 
      main_focus_special == "politics"| 
      main_focus_special == "social science"| 
      main_focus_other_which == "dutch"| 
      main_focus_other_which == "everyday culture and health"| 
      main_focus_other_which  == "geography. history"| # also in science
      main_focus_other_which == "Kompetenzbereich german. arts. music"|
      main_focus_other_which == "pedagogy"| 
      main_focus_other_which == "pedagogy methodology"| 
      main_focus_other_which == "pedagogy. methodology"|
      main_focus_other_which == "psychologie "| 
      main_focus_other_which == "psychologies. pedagogy. etc."|
      main_focus_other_which == "psychology"| 
      main_focus_other_which == "Psychology + pedagogy"|
      main_focus_other_which == "psychology. pedagogy"|
      main_focus_other_which == "psychology..."|
      main_focus_other_which == "social science"|
      main_focus_other_which == "statistics. psychology"
    ~ "yes")) %>%
  mutate(main_focus_science = case_when(
    sample == "belgian_teaching_staff" & main_focus_sachunterricht == "yes" ~ "yes",
    main_focus_special == "biology"|
      main_focus_special == "chemistry"|
      main_focus_special  == "geography"|
      main_focus_special == "geography "|
      main_focus_special == "physics"|
      main_focus_special == "science"|
      main_focus_special == "technology"|
      main_focus_other_which == "computer science"|
      main_focus_other_which == "geography. history"| # also in humanities
      main_focus_other_which == "handwork/technology"|
      main_focus_other_which  == "scientific Sachunterricht focus chemistry"
    ~ "yes")) %>%
  mutate(main_focus_mathematics2 = if_else(main_focus_mathematics == "yes"|
                                             main_focus_other_which == "basic mathematics",
                                           "yes", "no"))

# replacinga NA with "no"
bigdata2$main_focus_literacy[is.na(bigdata2$main_focus_literacy)] <- "no"
bigdata2$main_focus_humanities[is.na(bigdata2$main_focus_humanities)] <- "no"
bigdata2$main_focus_science[is.na(bigdata2$main_focus_science)] <- "no"

bigdata <- rbind(subset(bigdata, sample == "german_students"), bigdata2)

rm(list = "bigdata2")



# removing columns for single subjects
bigdata$main_focus_french <- NULL
bigdata$main_focus_english <- NULL
bigdata$main_focus_german <- NULL
bigdata$main_focus_religious_pedagogy <- NULL
bigdata$main_focus_art <- NULL
bigdata$main_focus_music <- NULL
bigdata$main_focus_sport <- NULL
bigdata$main_focus_sachunterricht <- NULL
bigdata$main_focus_special <- NULL
bigdata$main_focus_other <- NULL
bigdata$main_focus_other_which <- NULL
bigdata$main_focus_mathematics <- NULL


# renaming main_focus_mathematics2
names(bigdata)[names(bigdata) == 'main_focus_mathematics2'] <- 'main_focus_mathematics'

# in main_focus_humanities and main_focus_science, changing NA into "no" for teachers
bigdata2 <- bigdata %>% filter(sample != "german_students")
bigdata2$main_focus_humanities[is.na(bigdata2$main_focus_humanities)] <- "no"
bigdata2$main_focus_science[is.na(bigdata2$main_focus_science)] <- "no"
bigdata <- rbind(subset(bigdata, sample == "german_students"), bigdata2)

rm(list = "bigdata2")



### Teacher in service and pre-service ###

# teacher_stage = "other": they are only 3 and, reading their notes, we argue that they are actually teachers
# I reassign these responses to the category "primary school teacher"
bigdata$teacher_stage[bigdata$teacher_stage == "other"] <- "primary school teacher"
bigdata$other_current_activity <- NULL

# Categorizing in "in-service" and "pre-service"
bigdata$teacher_experience <- NA
bigdata2 <- bigdata %>%
  filter(sample != "german_students") %>%
  mutate(teacher_experience = if_else(teacher_stage == "primary school teacher",
                                      "in-service", "pre-service"))
bigdata <- rbind(subset(bigdata, sample == "german_students"), bigdata2)

rm(list = "bigdata2")



### Age range ###

bigdata <- bigdata %>%
  mutate(age_range = case_when(
    age < 20 ~ "< 20 years",
    age >= 20 & age < 30 ~ "20-29 years",
    age >= 30 & age < 40 ~ "30-39 years",
    age >= 40 & age < 50 ~ "40-49 years",
    age >= 50 ~ "over 50 years"
  ))

# removing age in years for teachers
bigdata2 <- bigdata %>% filter(sample != "german_students")
bigdata2$age <- NA
bigdata <- rbind(subset(bigdata, sample == "german_students"), bigdata2)
rm(list = "bigdata2")




# Reordering columns

bigdata <- bigdata[, c(1:5, 187, 6:81, 186, 82, 182:185, 83:181)] 


# Renaming teachres samples
bigdata$sample <- factor(bigdata$sample)
levels(bigdata$sample)[levels(bigdata$sample) == "belgian_teaching_staff"] <- "belgian_teachers"
levels(bigdata$sample)[levels(bigdata$sample) == "german_teaching_staff"] <- "german_teachers"

# Writing csv table
write.csv2(bigdata, file = paste0("AMATUS_dataset_",Sys.Date(),".csv"), row.names = FALSE)
# be aware that Microsoft Excel (with german decimal-settings) might convert decimal values like
# "1.5" to a date format (1 of may)

