# May 2019    
# Julius Gervelmeyer and Andreas Schmitt
# Downloaded from https://osf.io/gszpb/

# function which preprocesses the math-anxiety data from belgian and german stundents, and german teachers
# returns processed data-frame

preprocessing <- function(data, sample_name){

  # Convert native_speaker to factor with yes/no 
  data$native_speaker <- as.factor(data$native_speaker)
  data$native_speaker <- ifelse(data$native_speaker == "1", "yes", "no")
  
  # Convert sex to factor with m/f 
  data$sex <- as.factor(data$sex)
  data$sex <- ifelse(data$sex == "1", "m", "f")
  
  # calculate total time (start minus end minus time on introduction page)
  # and convert Dates/Times into same international format. 
  # Note that each data-set has its own format.
  
  if(sample_name == 'belgian_teaching_staff') {
    data$start_time <- as.POSIXlt(as.character(data$start_time), format = "%d.%m.%y %H:%M")
    data$start_time <- as.character(as.POSIXlt(data$start_time, format = "%Y-%m-%d %H:%M"))
    
    data$end_time <- as.POSIXlt(as.character(data$end_time), format = "%d.%m.%y %H:%M")
    data$end_time <- as.character(as.POSIXlt(data$end_time, format = "%Y-%m-%d %H:%M"))
    } 
  
  if(sample_name == 'german_teaching_staff') {
    data$start_time <- as.POSIXlt(as.character(data$start_time), format = "%d.%m.%y %H:%M")
    data$start_time <- as.character(as.POSIXlt(data$start_time, format = "%Y-%m-%d %H:%M"))
    
    data$end_time <- as.POSIXlt(as.character(data$end_time), format = "%d.%m.%y %H:%M")
    data$end_time <- as.character(as.POSIXlt(data$end_time, format = "%Y-%m-%d %H:%M"))
  }
  
  if(sample_name == 'german_students') {
    data$start_time <- as.POSIXlt(as.character(data$start_time), format = "%m/%d/%Y %H:%M")
    data$start_time <- as.character(as.POSIXlt(data$start_time, format = "%Y-%m-%d %H:%M:%OS"))
    
    data$end_time <- as.POSIXlt(as.character(data$end_time), format = "%m/%d/%Y %H:%M")
    data$end_time <- as.character(as.POSIXlt(data$end_time, format = "%Y-%m-%d %H:%M:%OS"))
  }

  data$total_time_minutes <- difftime(as.POSIXlt(data$end_time),as.POSIXlt(data$start_time), units = "mins")
  data$total_time_minutes <- as.numeric(data$total_time_minutes - floor(data$time_info/60))
  
  # recode belgian math grade
  if(sample_name == 'belgian_teaching_staff') {
    data$math_grade <- abs(data$math_grade - 7)
  }
  
  # delete uniformative columns and only keep total_time_minutes
  data$total_time <- NULL
  data$time_demographs <- NULL
  data$time_BFI <- NULL 
  data$time_GAD7 <- NULL 
  data$time_TAI <- NULL
  data$time_AMAS <- NULL
  data$time_SDQ_Math <- NULL
  data$time_SDQ_Lang <- NULL
  data$time_PISA <- NULL
  data$time_liking <- NULL
  data$time_persistence <- NULL
  data$time_explanation_math_test <- NULL
  data$time_math_performance <- NULL
  data$time_STAI <- NULL
  data$time_GS <- NULL
  data$time_SE <- NULL
  data$time_quality <- NULL
  data$time_info <- NULL
  data$time_lottery <- NULL
  data$comment <- NULL
  
  #### Exclusions ####
  # table with exclusion criterions and amounts of excluded participants in sequential order
  number_exclusions <- as.data.frame(matrix(nrow = 1, ncol=10))
  colnames(number_exclusions) <- c("not_finished", "no_native_speakers_in_bachelor", "no_teacher", "too_noisy", "not_honest", 
                                   "too_long_time", "number_participants_input", "sum_exclusions", 
                                   "number_participants_output", "do_exclusions_sum_up")
  # save number of input rows
  number_exclusions$number_participants_input <- nrow(data)
  
  # keep only participants who finished
  number_exclusions$not_finished <- count(data, 'finished')$freq[1]
  data_excluded <- data[data$finished == 1,] 

  # exclude non-native speakers for the german_students data
  if(sample_name == 'german_students'){
    # how many non-native speakers
    number_exclusions$no_native_speakers_in_bachelor <- count(data_excluded, 'native_speaker')$freq[1]
    # only native speakers
    data_excluded <- data_excluded[data_excluded$native_speaker == 'yes',]
  }
  
  # exclude students that don't become teachers from the two teacher-studies
  if(sample_name != 'german_students'){
    # how many become teachers
    number_exclusions$no_teacher <- count(data_excluded, 'GS01')$freq[2]
    # exclude rest
    data_excluded <- data_excluded[data_excluded$GS01 == 1,]
  }
  
  # noise
  number_exclusions$too_noisy <- nrow(data_excluded[data_excluded$noise >= 5,])
  data_excluded <- data_excluded[data_excluded$noise < 5,]

  # honesty
  number_exclusions$not_honest <- ifelse(is.na(count(data_excluded, 'honesty')$freq[2]),0,count(data_excluded, 'honesty')$freq[2])
  data_excluded <- data_excluded[data_excluded$honesty == 1,]

  # time
  number_exclusions$too_long_time <- nrow(data_excluded[data_excluded$total_time_minutes >= 30,])
  data_excluded <- data_excluded[data_excluded$total_time_minutes < 30,]
  
  # sum of excluded participants
  number_exclusions$sum_exclusions <- sum(number_exclusions[1:6], na.rm = T)
 
  # number of participants in output-file
  number_exclusions$number_participants_output <- nrow(data_excluded)
  
  # do exclusions sum up?
  if(number_exclusions$number_participants_input - number_exclusions$sum_exclusions == number_exclusions$number_participants_output){
    number_exclusions$do_exclusions_sum_up <- T
  }else{
    number_exclusions$do_exclusions_sum_up <- F
    warning(paste('Exclusions do not sum up ', "in", sample_name))
  }
  
  
  #### math performance calculations ####
  
  # get index of first and last math_performance column
  start_perf <- grep("^math_perf1_resp$", colnames(data_excluded))
  end_perf <- grep("^math_perf40_resp$", colnames(data_excluded))
  
  # convert all math-performance entries to characters
  data_excluded[,start_perf:end_perf] <- data.frame(sapply(data_excluded[,start_perf:end_perf], as.character), stringsAsFactors = F)
  
  # fill empty cells with NAs. NAs are interpreted as the task had been skipped by the participant.
  data_excluded[,start_perf:end_perf] <- data.frame(lapply(data_excluded[,start_perf:end_perf], function(x) {gsub("^$|^ $", NA, x)}), stringsAsFactors = F)
  
  # replace commas by dots (in decimals) to avoid errors
  data_excluded[,start_perf:end_perf] <- data.frame(lapply(data_excluded[,start_perf:end_perf], function(x) {gsub(",", ".", x)}), stringsAsFactors = F)
  
  # find non-numeric entries in the math-tasks like "8...." or "?". 
  rows_of_weird_entries <- list()
  cols_of_weird_entries <- list()
  for (row in 1:nrow(data_excluded)) {
    for (col in start_perf:end_perf) {
      # to avoid factor-level-warnings (NA-creation)
      data_excluded[,col] <- as.character(data_excluded[,col]) 
      
      # if not is numeric or not is na and starts with digit(s)
      if(!(grepl("^[[:digit:]]+(\\.)?[[:digit:]]*$|^(NA)$", data_excluded[row,col]) | is.na(data_excluded[row,col]))){ 
        print(paste("weird entry found in row:",row,", col:", colnames(data_excluded[col]), ":", data_excluded[row,col]))
        
        # accept entries of form [digits][characters] as [digits], e.g. convert "8...." to "8"
        if((grepl("^[[:digit:]]+(\\D)*$", data_excluded[row,col]))){ # if digits are followed by non-digits (like dots)
          # then replace entry with its number
          data_excluded[row,col] <- as.numeric(gsub("([0-9]+).*$", "\\1", data_excluded[row,col]))
          print(paste("weird entry in row:",row,", col:", colnames(data_excluded[col]), "was converted into:", data_excluded[row,col]))
        
        # if entry starts with a dot followed by digit(s), add preceeding zero
        }else if((grepl("^(\\.){1}[[:digit:]]+$", data_excluded[row,col]))){ 
          data_excluded[row,col] <- as.numeric(as.character(paste0("0",data_excluded[row,col])))
          print(paste("weird entry in row:",row,", col:", colnames(data_excluded[col]), "was converted into:", data_excluded[row,col]))
          
        }else{ #if the content is not a number but weird content
          rows_of_weird_entries <- append(rows_of_weird_entries, row)
          cols_of_weird_entries <- append(cols_of_weird_entries, col)
        }
      }
    }
  }
  print(paste(length(rows_of_weird_entries),"weird entrie(s) found"))

  if(length(rows_of_weird_entries)>0){
    # print weird entries
    print(paste("rows:",rows_of_weird_entries))
    print(paste("columns:",cols_of_weird_entries))
    for (i in 1:length(rows_of_weird_entries)) {
      print(paste("entry", i,":", data_excluded[as.integer(rows_of_weird_entries[i]),as.integer(cols_of_weird_entries[i])]))
      # convert into "-1.0" which is a wrong entry, but numeric, and is not NA (which would be interpreted as
      # having skipped the task)
      data_excluded[as.integer(rows_of_weird_entries[i]),as.integer(cols_of_weird_entries[i])] <- as.numeric("-1.0")
      print(paste("entry replaced with:",data_excluded[as.integer(rows_of_weird_entries[i]),as.integer(cols_of_weird_entries[i])]))
    }
  }
  
  # convert all math columns into numeric
  data_excluded[,start_perf:end_perf] <- data.frame(lapply(data_excluded[,start_perf:end_perf], function(x) as.numeric(as.character(x))))
  # Here, no NAs must be generated! 
  
  # create new variable math_persistence to check whether a person skipped a math task: it is filled with a 1,
  # if one field is empty, while the following is not. Therefore we check on 39 comparisons
  math_performance <- subset(data_excluded, select=c(math_perf1_resp : math_perf40_resp))
  math_task_persistence <- math_performance
  math_task_persistence <- math_task_persistence[,-40]
  for (i in 1:39) {
    math_task_persistence[i] <- ifelse(is.na(math_performance[i]) & !is.na(math_performance[i+1]),1,0)
  }

  # creating a variable with the amount of wrong entries per person
  math_performance$sum_wrong_order <- apply(math_task_persistence, 1, sum)
  
  # the persons with a 0 in this including variable used the wrong order of math tasks
  math_performance$include_order <- ifelse(math_performance$sum_wrong_order == 0, 1, 0) 
  #table(math_performance$include_order)
  
  # Number of participants that ignored the task order
  number_wrong_order <- count(math_performance$include_order)$freq[1]
  print(paste("number of participants who ignored the task order: ",number_wrong_order))
  
  # print exclusions
  print("number of exclusions, sequentially from left to right: ")
  print(number_exclusions)

  # reverse the polarity of the reversed items
  data_excluded$BFI2 <- abs(data_excluded$BFI2_r - 6)
  data_excluded$BFI5 <- abs(data_excluded$BFI5_r - 6)
  data_excluded$BFI7 <- abs(data_excluded$BFI7_r - 6)

  data_excluded$SDQ_M2 <- abs(data_excluded$SDQ_M2_r - 5)
  data_excluded$SDQ_M4 <- abs(data_excluded$SDQ_M4_r - 5)
  data_excluded$SDQ_L1 <- abs(data_excluded$SDQ_L1_r - 5)
  data_excluded$SDQ_L2 <- abs(data_excluded$SDQ_L2_r - 5)
  
  data_excluded$SE02_01 <- abs(data_excluded$SE02_01 - 6)
  data_excluded$SE02_02 <- abs(data_excluded$SE02_02 - 6)
  data_excluded$SE02_03 <- abs(data_excluded$SE02_03 - 6)
  data_excluded$SE02_04 <- abs(data_excluded$SE02_04 - 6)

  # delete the original (reversed) ones
  data_excluded$BFI2_r <- NULL
  data_excluded$BFI5_r <- NULL
  data_excluded$BFI7_r <- NULL
  data_excluded$SDQ_M2_r <- NULL 
  data_excluded$SDQ_M4_r <- NULL 
  data_excluded$SDQ_L1_r <- NULL 
  data_excluded$SDQ_L2_r <- NULL

  # Sum-scores of the scales:
  data_excluded$score_AMAS_total <- data_excluded$AMAS1 + data_excluded$AMAS2 + 
    data_excluded$AMAS3 + data_excluded$AMAS4 + data_excluded$AMAS5 + data_excluded$AMAS6 + 
    data_excluded$AMAS7 + data_excluded$AMAS8 + data_excluded$AMAS9
  data_excluded$score_GAD <- data_excluded$GAD1 + data_excluded$GAD2 + data_excluded$GAD3 + 
    data_excluded$GAD4 + data_excluded$GAD5 + data_excluded$GAD6 + data_excluded$GAD7
  data_excluded$score_STAI <- data_excluded$STAI1 + data_excluded$STAI2 + data_excluded$STAI3 +
    data_excluded$STAI4 + data_excluded$STAI5 
  data_excluded$score_TAI <- data_excluded$TAI1 + data_excluded$TAI2 + data_excluded$TAI3 + 
    data_excluded$TAI4 + data_excluded$TAI5
  data_excluded$score_SDQ_M <- data_excluded$SDQ_M1 + data_excluded$SDQ_M2 + data_excluded$SDQ_M3 + 
    data_excluded$SDQ_M4
  data_excluded$score_SDQ_L <- data_excluded$SDQ_L1 + data_excluded$SDQ_L2 + data_excluded$SDQ_L3 + 
    data_excluded$SDQ_L4
  data_excluded$score_PISA <- data_excluded$PISA1 + data_excluded$PISA2 + data_excluded$PISA3 + 
    data_excluded$PISA4 + data_excluded$PISA5 + data_excluded$PISA6
  data_excluded$score_BFI <- data_excluded$BFI1 + data_excluded$BFI2 + data_excluded$BFI3 + 
    data_excluded$BFI4 + data_excluded$BFI5 + data_excluded$BFI6 + data_excluded$BFI7 + 
    data_excluded$BFI8 

  # Subscales for AMAS 
  data_excluded$score_AMAS_learning <- data_excluded$AMAS1 + data_excluded$AMAS3 + 
    data_excluded$AMAS6 + data_excluded$AMAS7 + data_excluded$AMAS9
  data_excluded$score_AMAS_testing <- data_excluded$AMAS2 + data_excluded$AMAS4 + 
    data_excluded$AMAS5 + data_excluded$AMAS8 

  # sum-score for SE (stereotype endorsement)
  data_excluded$score_SE <- data_excluded$SE02_01 + data_excluded$SE02_02 + data_excluded$SE02_03 +
    data_excluded$SE02_04 + data_excluded$SE02_05 + data_excluded$SE02_06 + data_excluded$SE02_07 +
    data_excluded$SE02_08 + data_excluded$SE02_09

  # Evaluating math performance
  # Creating variables, that show whether the response was correct = 1, false or missing = 0
  data_excluded$math_perf1_acc <- ifelse(data_excluded$math_perf1_resp == 227, 1, 0)
  data_excluded$math_perf2_acc <- ifelse(data_excluded$math_perf2_resp == 128, 1, 0)
  data_excluded$math_perf3_acc <- ifelse(data_excluded$math_perf3_resp == 8, 1, 0)
  data_excluded$math_perf4_acc <- ifelse(data_excluded$math_perf4_resp == 75, 1, 0)
  data_excluded$math_perf5_acc <- ifelse(data_excluded$math_perf5_resp == 98, 1, 0)
  data_excluded$math_perf6_acc <- ifelse(data_excluded$math_perf6_resp == 9, 1, 0)
  data_excluded$math_perf7_acc <- ifelse(data_excluded$math_perf7_resp == 24, 1, 0)
  data_excluded$math_perf8_acc <- ifelse(data_excluded$math_perf8_resp == 90, 1, 0)
  data_excluded$math_perf9_acc <- ifelse(data_excluded$math_perf9_resp == 121, 1, 0)
  data_excluded$math_perf10_acc <- ifelse(data_excluded$math_perf10_resp == 15, 1, 0)
  data_excluded$math_perf11_acc <- ifelse(data_excluded$math_perf11_resp == 9, 1, 0)
  data_excluded$math_perf12_acc <- ifelse(data_excluded$math_perf12_resp == 188, 1, 0)
  data_excluded$math_perf13_acc <- ifelse(data_excluded$math_perf13_resp == 3, 1, 0)
  data_excluded$math_perf14_acc <- ifelse(data_excluded$math_perf14_resp == 56, 1, 0)
  data_excluded$math_perf15_acc <- ifelse(data_excluded$math_perf15_resp == 171, 1, 0)
  data_excluded$math_perf16_acc <- ifelse(data_excluded$math_perf16_resp == 2, 1, 0)
  data_excluded$math_perf17_acc <- ifelse(data_excluded$math_perf17_resp == 175, 1, 0)
  data_excluded$math_perf18_acc <- ifelse(data_excluded$math_perf18_resp == 74, 1, 0)
  data_excluded$math_perf19_acc <- ifelse(data_excluded$math_perf19_resp == 18, 1, 0)
  data_excluded$math_perf20_acc <- ifelse(data_excluded$math_perf20_resp == 129, 1, 0)
  data_excluded$math_perf21_acc <- ifelse(data_excluded$math_perf21_resp == 16, 1, 0)
  data_excluded$math_perf22_acc <- ifelse(data_excluded$math_perf22_resp == 47, 1, 0)
  data_excluded$math_perf23_acc <- ifelse(data_excluded$math_perf23_resp == 9, 1, 0)
  data_excluded$math_perf24_acc <- ifelse(data_excluded$math_perf24_resp == 131, 1, 0)
  data_excluded$math_perf25_acc <- ifelse(data_excluded$math_perf25_resp == 8, 1, 0)
  data_excluded$math_perf26_acc <- ifelse(data_excluded$math_perf26_resp == 63, 1, 0)
  data_excluded$math_perf27_acc <- ifelse(data_excluded$math_perf27_resp == 4, 1, 0)
  data_excluded$math_perf28_acc <- ifelse(data_excluded$math_perf28_resp == 76, 1, 0)
  data_excluded$math_perf29_acc <- ifelse(data_excluded$math_perf29_resp == 112, 1, 0)
  data_excluded$math_perf30_acc <- ifelse(data_excluded$math_perf30_resp == 48, 1, 0)
  data_excluded$math_perf31_acc <- ifelse(data_excluded$math_perf31_resp == 60, 1, 0)
  data_excluded$math_perf32_acc <- ifelse(data_excluded$math_perf32_resp == 3, 1, 0)
  data_excluded$math_perf33_acc <- ifelse(data_excluded$math_perf33_resp == 91, 1, 0)
  data_excluded$math_perf34_acc <- ifelse(data_excluded$math_perf34_resp == 168, 1, 0)
  data_excluded$math_perf35_acc <- ifelse(data_excluded$math_perf35_resp == 126, 1, 0)
  data_excluded$math_perf36_acc <- ifelse(data_excluded$math_perf36_resp == 48, 1, 0)
  data_excluded$math_perf37_acc <- ifelse(data_excluded$math_perf37_resp == 8, 1, 0)
  data_excluded$math_perf38_acc <- ifelse(data_excluded$math_perf38_resp == 181, 1, 0)
  data_excluded$math_perf39_acc <- ifelse(data_excluded$math_perf39_resp == 19, 1, 0)
  data_excluded$math_perf40_acc <- ifelse(data_excluded$math_perf40_resp == 65, 1, 0)

  # get index of first and last math_performance_acc column
  start_perf_acc <- grep("^math_perf1_acc$", colnames(data_excluded))
  end_perf_acc <- grep("^math_perf40_acc$", colnames(data_excluded))
  
  # Replace NAs in Accuracy-variable of math performance with 0 for further calculations
  for (i in start_perf_acc:end_perf_acc) {
    data_excluded[,i][is.na(data_excluded[,i])] <- 0
  }
  
  # Creating variable indicating whether the participants ignored the task order or not
  data_excluded$math_performance_correct_order <- math_performance$include_order
  
  # creating a variable for math performance of all participants
  data_excluded$sum_math_perf_all <- data_excluded$math_perf1_acc + data_excluded$math_perf2_acc + data_excluded$math_perf3_acc + 
    data_excluded$math_perf4_acc + data_excluded$math_perf5_acc + data_excluded$math_perf6_acc + data_excluded$math_perf7_acc + data_excluded$math_perf8_acc + 
    data_excluded$math_perf9_acc + data_excluded$math_perf10_acc + data_excluded$math_perf11_acc + data_excluded$math_perf12_acc + data_excluded$math_perf13_acc +
    data_excluded$math_perf14_acc + data_excluded$math_perf15_acc + data_excluded$math_perf16_acc + data_excluded$math_perf17_acc + data_excluded$math_perf18_acc +
    data_excluded$math_perf19_acc + data_excluded$math_perf20_acc + data_excluded$math_perf21_acc + data_excluded$math_perf22_acc + data_excluded$math_perf23_acc +
    data_excluded$math_perf24_acc + data_excluded$math_perf25_acc + data_excluded$math_perf26_acc + data_excluded$math_perf27_acc + data_excluded$math_perf28_acc +
    data_excluded$math_perf29_acc + data_excluded$math_perf30_acc + data_excluded$math_perf31_acc + data_excluded$math_perf32_acc + data_excluded$math_perf33_acc +
    data_excluded$math_perf34_acc + data_excluded$math_perf35_acc + data_excluded$math_perf36_acc + data_excluded$math_perf37_acc + data_excluded$math_perf38_acc + 
    data_excluded$math_perf39_acc + data_excluded$math_perf40_acc

  # Replacing math performance score with NA for participants that ignored task order
  data_excluded$sum_math_perf_all <- ifelse(data_excluded$math_performance_correct_order == 0, NA, data_excluded$sum_math_perf_all)
  
  # Deleting further unnecessary variables
  data_excluded$serial <- NULL
  data_excluded$ref <- NULL
  data_excluded$questnnr <- NULL
  data_excluded$mode <- NULL
  data_excluded$mail_sent <- NULL
  data_excluded$finished <- NULL
  data_excluded$last_page <- NULL
  data_excluded$MAXPAGE <- NULL
  data_excluded$Q_VIEWER <- NULL
  data_excluded$time_sum <- NULL
  data_excluded$MISSING <- NULL
  data_excluded$MISSREL <- NULL
  data_excluded$TIME_RSI <- NULL
  data_excluded$DEG_TIME <- NULL
  data_excluded$study_program_low     <- NULL
  data_excluded$study_program_middle  <- NULL
  data_excluded$study_program_high    <- NULL
  data_excluded$study_program_other   <- NULL
  data_excluded$case <- NULL

  ### device usage (1 computer, 2 tablet, 3 smartphone)
  if(!unique(is.na(unique(data_excluded$device)))){
    device_usage <- as.data.frame(table(data_excluded$device))
    colnames(device_usage) <- c("device", "freq")
    device_usage$device <- as.character(device_usage$device)
    device_usage$device[1] <- "1 computer"
    device_usage$device[2] <- "2 tablet"
    device_usage$device[3] <- "3 smartphone"
    print("device usage:")
    print(device_usage)
  }else{
    print("no device usage reported")
  }
  
  ### breaks
  did_breaks <- count(data_excluded, 'breaks')$freq[2]
  print(paste("number of participants who did breaks:",did_breaks))
  
  # add string with sample name
  data_excluded$sample <- sample_name

  # math_load has no meaning except for german students
  if(sample_name != 'german_students'){
    data_excluded$math_load <- NA
  }
  
  # overwrite GS03-variable to code for number of main subjects
  if(sample_name != "german_students"){
    data_excluded$GS03 <- 0
    main_programs_indizes <- c(100:102,104:110) #columns
    for (row in 1:nrow(data_excluded)) {
      for (col in main_programs_indizes) {
        if(!is.na(data_excluded[row,col]) & data_excluded[row,col]==2){
          data_excluded$GS03[row] <- data_excluded$GS03[row]+1
        }
      }
    }
  }
  # order the final data table
  attach(data_excluded)
  data_ordered <- cbind(
    # participant number
    partNo,
    # sample
    sample,
    # demo
    sex, age, native_speaker, math_grade, math_load, math_influence_on_program_choice, honesty, breaks, noise, device,
    start_time, end_time, total_time_minutes, 
    # liking
    liking_math, liking_science, liking_humanities, persistence_math, persistence_science, persistence_humanities,
    # AMAS
    score_AMAS_total, score_AMAS_learning, score_AMAS_testing, AMAS1, AMAS2, AMAS3, AMAS4, AMAS5, AMAS6, AMAS7, AMAS8, AMAS9,
    # GAD
    score_GAD, GAD1, GAD2, GAD3, GAD4, GAD5, GAD6, GAD7,
    # STAI
    score_STAI, STAI1, STAI2, STAI3, STAI4, STAI5,
    # TAI 
    score_TAI, TAI1, TAI2, TAI3, TAI4, TAI5,
    # SDQ_M
    score_SDQ_M, SDQ_M1, SDQ_M2, SDQ_M3, SDQ_M4, 
    # SDQ_L
    score_SDQ_L, SDQ_L1, SDQ_L2, SDQ_L3, SDQ_L4,
    # PISA
    score_PISA, PISA1, PISA2, PISA3, PISA4, PISA5, PISA6,
    # BFI
    score_BFI, BFI1, BFI2, BFI3, BFI4, BFI5, BFI6, BFI7, BFI8, 
    # GS 
    GS01, GS02, GS02_03, GS02_04, GS03, GS03_13, GS03_14, GS03_08, GS_03_08a, GS03_01, GS03_02, GS03_05, GS03_06,
    GS03_07, GS03_11, GS03_10, GS03_10a, GS03_12, GS04_01, GS04_02, GS04_03, GS05_01, GS05_02, GS05_03,
    # SE
    score_SE, SE02_01, SE02_02, SE02_03, SE02_04, SE02_05, SE02_06, SE02_07, SE02_08, SE02_09,
    # math performance
    sum_math_perf_all, math_performance_correct_order, math_perf1_acc, math_perf2_acc, math_perf3_acc,
    math_perf4_acc, math_perf5_acc, math_perf6_acc, math_perf7_acc, math_perf8_acc, math_perf9_acc,  
    math_perf10_acc, math_perf11_acc, math_perf12_acc, math_perf13_acc, math_perf14_acc, math_perf15_acc,    
    math_perf16_acc, math_perf17_acc, math_perf18_acc, math_perf19_acc, math_perf20_acc, math_perf21_acc,   
    math_perf22_acc, math_perf23_acc, math_perf24_acc, math_perf25_acc, math_perf26_acc, math_perf27_acc,      
    math_perf28_acc, math_perf29_acc, math_perf30_acc, math_perf31_acc, math_perf32_acc, math_perf33_acc,         
    math_perf34_acc, math_perf35_acc, math_perf36_acc, math_perf37_acc, math_perf38_acc, math_perf39_acc,       
    math_perf40_acc, math_perf1_resp, math_perf2_resp, math_perf3_resp, math_perf4_resp, math_perf5_resp,                 
    math_perf6_resp, math_perf7_resp, math_perf8_resp, math_perf9_resp, math_perf10_resp, math_perf11_resp,               
    math_perf12_resp, math_perf13_resp, math_perf14_resp, math_perf15_resp, math_perf16_resp, math_perf17_resp,                
    math_perf18_resp, math_perf19_resp, math_perf20_resp, math_perf21_resp, math_perf22_resp, math_perf23_resp,                
    math_perf24_resp, math_perf25_resp, math_perf26_resp, math_perf27_resp, math_perf28_resp, math_perf29_resp,                
    math_perf30_resp, math_perf31_resp, math_perf32_resp, math_perf33_resp, math_perf34_resp, math_perf35_resp,                
    math_perf36_resp, math_perf37_resp, math_perf38_resp, math_perf39_resp, math_perf40_resp)
  detach(data_excluded)  
  data_excluded <- data_ordered
  
  return(as.data.frame(data_excluded, stringsAsFactors = F))
}
