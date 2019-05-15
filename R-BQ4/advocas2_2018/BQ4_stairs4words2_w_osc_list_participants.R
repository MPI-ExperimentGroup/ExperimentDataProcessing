##--Stairs for words----------------------------------------------------------##

library(jsonlite)
library(httr)
library(dplyr)
library(tidyr)
library(stringr)
source("shared/BQ4_supportFunctions_2018.R")


##--Configuration-------------------------------------------------------------##

experiment_abr <- "advocas2"
server_url <- "http://ems12.mpi.nl"
admin_url <- paste(server_url, "/", experiment_abr, "-admin", sep = "")


##--Authentication------------------------------------------------------------##

## Read username/password from file and authenticate on Admin interface

auth_values <- read.table(paste("login.", experiment_abr, ".csv", sep = ""), 
                          stringsAsFactors = FALSE)

auth_body <-
  list(
    username=auth_values$username[1],
    password=auth_values$password[1]
  ) 

login_url <- paste(admin_url, "login", sep = "/")
req_auth <- httr::POST(login_url, body = auth_body)

##--Get data from REST interface----------------------------------------------##

participants_columns <- c("userId", "workerId")
participants <- get_embedded("participants", participants_columns, admin_url)
participants_data <- unique(subset(participants[["currentData"]]))

tagevents <- read.csv("tagpairdata.csv", stringsAsFactors = FALSE)

#tagevents_columns <- c("userId", "screenName", "eventTag", 
#                           "tagValue")
#tagevents <- get_embedded("tagevents", tagevents_columns, admin_url, 1000)

#tagevents_data <- unique(tagevents[["currentData"]])
tagevents_data <- tagevents



fast_track <- subset(tagevents_data, EventTag == "fast_track")

fast_track$label <- lapply(strsplit(fast_track$TagValue2,";"), 
                                      function(x) x[1])

fast_track$BandNumber <- lapply(strsplit(fast_track$TagValue2,";"), 
                                      function(x) x[2])

fast_track$UserAnswer <- lapply(strsplit(fast_track$TagValue2,";"), 
                                           function(x) x[3])

fast_track$IsUserCorrect <- lapply(strsplit(fast_track$TagValue2,";"), 
                                           function(x) x[4])



fine_tuning <- subset(tagevents_data, EventTag == "fine_tuning")

fine_tuning$label <- lapply(strsplit(fine_tuning$TagValue2,";"), 
                                      function(x) x[2])

fine_tuning$BandNumber <- lapply(strsplit(fine_tuning$TagValue2,";"), 
                                           function(x) x[1])

fine_tuning$UserAnswer <- lapply(strsplit(fine_tuning$TagValue2,";"), 
                                           function(x) x[3])

fine_tuning$IsUserCorrect <- lapply(strsplit(fine_tuning$TagValue2,";"), 
                                              function(x) x[4])



# Olha

rating_buttons <- subset(tagevents_data, EventTag == "RatingButton")
rating_buttons$label <- lapply(strsplit(rating_buttons$TagValue1,"_"), 
                               function(x) x[1])
names(rating_buttons)[names(rating_buttons) == 'TagValue2'] <- 'UserAnswer'
rating_buttons$UserAnswer <- ifelse(rating_buttons$UserAnswer == "JA&#44; ik ken dit woord", "true", "false")


##--Functions-----------------------------------------------------------------##

# Olha
calculate_nonwords_ratio <- function(table, row) {
  n_nonwords <- 0
  for (i in 1:row){
    if (table[i,]$BandNumber == -1) {
      n_nonwords <- n_nonwords + 1
    }
  }
  return(as.double(n_nonwords)/as.double(row))
}

## split_rounds() splits the fine_tuning rows into a first and second round 

split_rounds <- function(fine_tuning){
  round_starts <- as.vector(c())
  for(row in 1:nrow(fine_tuning)){
    if (fine_tuning$TagValue1[row] == "row000000"){
      round_starts <- rbind(round_starts, row)
    }
  }
  return(list(round1 = fine_tuning[1:round_starts[2]-1,], 
              round2 = fine_tuning[round_starts[2]:nrow(fine_tuning),]))
}

#Aggregate:
  
#  2 summaries + duration + was test ended by oscillation criterium or because no more stimuli

#Raw data:
#  Duration is now total duration of test until then
#Needed: is difference between durations = duration per question

# Number of switches between fast track and finetuning because of non-word.

# stopping correctly yes/no according to 2 cycle and 3 cycle criterion
# final score
# is final score correct 
# correct final score


# How many times were 2 oscillations reached but not 3, for each user
# RA10: 16205a36065-90bf-e7c0-fb33-c864
# SPT80: 165857feb11-408a-e5a4-9cfa-c887
# SGT05: 1629acc2cc2-82a2-513c-a3dc-59a0

##--Get oscillations for each user--------------------------------------------##

user_scores <- data.frame()
item_data  <- as.vector(c())

participants_uuids <- read.csv("participants_pre_megapilot.csv", header = FALSE, sep =';',  fileEncoding="UTF-8-BOM")

options(warn=2)

for (rawUUID in participants_uuids$V1)  {
  
  
  user <- rawUUID
  
  print(user)
  userName <- subset(participants_data, userId == user)$workerId
  print(userName)
  
  user_summary <- subset(tagevents_data, UserId == user & 
                           EventTag == "user_summary" & 
                           TagValue1 == "row000001")
  
  # Olha (Florian complains that one of the user's was lost)
  ## Skip users that have no summary or that have a summary for only one round
  # if (!nrow(user_summary) || nrow(user_summary) == 1){
    # next
  #}
  
  user_summary_r1 <- user_summary[1,]
  user_summary_r2 <- user_summary[2,]
  
  user_summary_r1 <- lapply(strsplit(user_summary_r1$TagValue2,";"), 
                         function(x) x)
  user_summary_r2 <- lapply(strsplit(user_summary_r2$TagValue2,";"), 
                            function(x) x)
  user_score_r1 <- user_summary_r1[[1]][1]
  # Olha: commented out the duplicated row
  # user_score_r1 <- user_summary_r1[[1]][1]
  BestFastTrack_r1	<- user_summary_r1[[1]][2]
  Cycle2oscillation_r1	<- user_summary_r1[[1]][3]
  EnoughFineTuningStimuli_r1 <- user_summary_r1[[1]][4]
  Champion_r1 <- user_summary_r1[[1]][5]
  Loser_r1 <- user_summary_r1[[1]][5]

  user_score_r2 <- user_summary_r2[[1]][1]
  # Olha: commented the duplicated row
  # user_score_r2 <- user_summary_r2[[1]][1]
  BestFastTrack_r2	<- user_summary_r2[[1]][2]
  Cycle2oscillation_r2	<- user_summary_r2[[1]][3]
  EnoughFineTuningStimuli_r2 <- user_summary_r2[[1]][4]
  Champion_r2 <- user_summary_r2[[1]][5]
  Loser_r2 <- user_summary_r2[[1]][5]
  
  fine_tuning_1user <- subset(fine_tuning, UserId == user)
  
  fine_tuning_rounds <- split_rounds(fine_tuning_1user)
  
  fine_tuning_round1 <- fine_tuning_rounds[["round1"]]
  fine_tuning_round2 <- fine_tuning_rounds[["round2"]]
  
 
  # Olha: removing rows which are headers (one of the minuses of the old version: there were rows that were headers which lead to erroneous data reading)
  fine_tuning_round1 <- subset(fine_tuning_round1, !startsWith(TagValue2,"BandNumber;Label;UserAnswer;IsAnswerCorrect;"))
  fine_tuning_round2 <- subset(fine_tuning_round2, !startsWith(TagValue2,"BandNumber;Label;UserAnswer;IsAnswerCorrect;"))
  
  #Olha: commenting out apparently redundant operation
  #fine_tuning_1user <- fine_tuning_1user[order(fine_tuning_1user[,4]), ]
  
  raw_bandnumbers <- fine_tuning_1user$BandNumber
  raw_bandnumbers_r1 <- fine_tuning_round1[,c("BandNumber", "IsUserCorrect")]
  raw_bandnumbers_r2 <- fine_tuning_round2[,c("BandNumber", "IsUserCorrect")]
  
  raw_bandnumbers_r1 <- remove_second_empty(raw_bandnumbers_r1)
  raw_bandnumbers_r2 <- remove_second_empty(raw_bandnumbers_r2)
  
  # Olha (!): reduce_to_single gets now "fine_tuning_roundX" as a parameter
  # since it is used in one of the branches (see the comment in the code for this function)
  bandnumbers_r1 <- reduce_to_single(raw_bandnumbers_r1, fine_tuning_round1)
  bandnumbers_r2 <- reduce_to_single(raw_bandnumbers_r2, fine_tuning_round2)
  
  
  
  #fine_tuning_bands <- subset(fine_tuning_1user, BandNumber != "-1" &
  #                              BandNumber != " " &
  #                              BandNumber != "BandNumber")
  
  #bandnumbers <- fine_tuning_bands$BandNumber
  
  two_oscillations_output_r1 <- get_two_oscillations(bandnumbers_r1)
  num_2_oscillations_r1 <- two_oscillations_output_r1[['two_oscillations']]
  two_oscillations_score_r1 <- two_oscillations_output_r1[['score']]
  originalScoreCorrect_r1 <- as.integer(user_score_r1 == two_oscillations_score_r1)
  
  two_oscillations_output_r2 <- get_two_oscillations(bandnumbers_r2)
  num_2_oscillations_r2 <- two_oscillations_output_r2[['two_oscillations']]
  two_oscillations_score_r2 <- two_oscillations_output_r2[['score']]
  originalScoreCorrect_r2 <- as.integer(user_score_r2 == two_oscillations_score_r2)

  print(paste0("num 2-oscillations R1: ",as.character(num_2_oscillations_r1)))
  print(paste0("num 2-oscillations R2: ",as.character(num_2_oscillations_r2)))
  
  # Olha: updated output presentation
  new_user_scores <- data.frame(user, userName,
                                user_score_r1, 
                                BestFastTrack_r1,
                                Cycle2oscillation_r1, 
                                EnoughFineTuningStimuli_r1, 
                                Champion_r1, Loser_r1,
                                num_2_oscillations_r1, 
                                two_oscillations_score_r1,
                                originalScoreCorrect_r1,
                                user_score_r2, 
                                BestFastTrack_r2,
                                Cycle2oscillation_r2, 
                                EnoughFineTuningStimuli_r2, 
                                Champion_r2, Loser_r2,
                                num_2_oscillations_r2, 
                                two_oscillations_score_r2,
                                originalScoreCorrect_r2
  )
  names(new_user_scores) <- c("user", "userName", 
                              "score R1", 
                              "BestFastTrack R1",
                              "Cycle2oscillation R1", 
                              "EnoughFineTuningStimuli R1", 
                              "Champion R1", "Loser R1",
                              "num_2_oscillations R1", 
                              "2_oscillations_score R1",
                              "originalScoreCorrect R1",
                              "score R2", 
                              "BestFastTrack R2",
                              "Cycle2oscillation R2", 
                              "EnoughFineTuningStimuli R2", 
                              "Champion R2", "Loser R2",
                              "num_2_oscillations R2", 
                              "2_oscillations_score R2",
                              "originalScoreCorrect R2")
  
  user_scores <- rbind(user_scores, new_user_scores)
  
  
  # Olha: item data table
  
  fast_track_1user <- subset(fast_track, UserId == user)
  fast_track_rounds <- split_rounds(fast_track_1user)
  
  fast_track_round1 <- fast_track_rounds[["round1"]]
  fast_track_round2 <- fast_track_rounds[["round2"]]
  
  fast_track_round1 <- subset(fast_track_round1, !startsWith(TagValue2,"Label;BandNumber;UserAnswer;IsAnswerCorrect;") )
  fast_track_round2 <- subset(fast_track_round2, !startsWith(TagValue2,"Label;BandNumber;UserAnswer;IsAnswerCorrect;"))
  
  # Select only necessary columns
  fast_track_round1 <- fast_track_round1[,c("UserId","EventTag", "label",  "BandNumber",  "UserAnswer", "IsUserCorrect")]
  fast_track_round2 <- fast_track_round2[,c("UserId","EventTag", "label",  "BandNumber",  "UserAnswer", "IsUserCorrect")]
  
  
  fast_track_round1$nonwordRatioAtThisPoint <- NA
  for (i in 1:nrow(fast_track_round1)){
    fast_track_round1[i,]$nonwordRatioAtThisPoint <- calculate_nonwords_ratio(fast_track_round1, i)
  }
 
  fast_track_round2$nonwordRatioAtThisPoint <- NA
  for (i in 1:nrow(fast_track_round2)){
    fast_track_round2[i,]$nonwordRatioAtThisPoint <- calculate_nonwords_ratio(fast_track_round2, i)
  }
  
  names(fast_track_round1)[names(fast_track_round1) == 'EventTag'] <- 'phase'
  names(fast_track_round2)[names(fast_track_round2) == 'EventTag'] <- 'phase'
  
  fast_track_round1$Run <- 1
  fast_track_round2$Run <- 2
  
  fast_track_round1 <- fast_track_round1[,c(1,8,2,3,4,5,6,7)]
  fast_track_round2 <- fast_track_round2[,c(1,8,2,3,4,5,6,7)]
  
  fine_tuning_round1 <- subset(fine_tuning_round1, !startsWith(TagValue2, " ;"))
  fine_tuning_round2 <- subset(fine_tuning_round2, !startsWith(TagValue2, " ;"))
  
  fine_tuning_round1 <- fine_tuning_round1[,c("UserId","EventTag", "label",  "BandNumber",  "UserAnswer", "IsUserCorrect")]
  fine_tuning_round2 <- fine_tuning_round2[,c("UserId","EventTag", "label",  "BandNumber",  "UserAnswer", "IsUserCorrect")]
  
  fine_tuning_round1$nonwordRatioAtThisPoint <- NA
  fine_tuning_round2$nonwordRatioAtThisPoint <- NA
  
  names(fine_tuning_round1)[names(fine_tuning_round1) == 'EventTag'] <- 'phase'
  names(fine_tuning_round2)[names(fine_tuning_round2) == 'EventTag'] <- 'phase'
  
  fine_tuning_round1$Run <- 1
  fine_tuning_round2$Run <- 2
  
  fine_tuning_round1 <- fine_tuning_round1[,c(1,8,2,3,4,5,6,7)]
  fine_tuning_round2 <- fine_tuning_round2[,c(1,8,2,3,4,5,6,7)]
  
  fast_track_round1[] <- lapply(fast_track_round1, as.character)
  fine_tuning_round1[] <- lapply(fine_tuning_round1, as.character)
  
  fast_track_round2[] <- lapply(fast_track_round2, as.character)
  fine_tuning_round2[] <- lapply(fine_tuning_round2, as.character)
  
  item_data_r1 <- rbind(fast_track_round1, fine_tuning_round1)
  item_data_r1$phase <- paste0(item_data_r1$phase,"_1")
  
  item_data_r2 <- rbind(fast_track_round2, fine_tuning_round2)
  item_data_r2$phase <- paste0(item_data_r2$phase,"_2")
  
  # Join concatenated item data to rating button data to get fine grained 
  # timestamps in miliseconds
  rating_buttons_r1 <- subset(rating_buttons, UserId == user)
  rating_buttons_r1[] <- lapply(rating_buttons_r1, as.character)
  combined_r1 <- left_join(item_data_r1, 
                               rating_buttons_r1[,c("UserId", "label", "UserAnswer", "TagDate", "EventMs")], 
                               by = c("UserId", "label", "UserAnswer"))
 
  
  rating_buttons_r2 <- subset(rating_buttons, UserId == user)
  
  rating_buttons_r2[] <- lapply(rating_buttons_r2, as.character)
  combined_r2 <- left_join(item_data_r2, 
                           rating_buttons_r2[,c("UserId", "label", "UserAnswer", "TagDate", "EventMs")], 
                           by = c("UserId", "label", "UserAnswer"))
  
 
  # Calculate item durations
  combined_r1$duration <- c(combined_r1$EventMs[1], as.numeric(combined_r1$EventMs[2:nrow(combined_r1)]) - as.numeric(combined_r1$EventMs[1:nrow(combined_r1)-1]))
  combined_r2$duration <- c(combined_r2$EventMs[1], as.numeric(combined_r2$EventMs[2:nrow(combined_r2)]) - as.numeric(combined_r2$EventMs[1:nrow(combined_r2)-1]))
  
  
  item_data <- rbind(item_data, combined_r1)
  item_data <- rbind(item_data, combined_r2)
  
 
}


write.csv(user_scores, file=paste0(experiment_abr,".user_scores.csv"))

item_data$WordOrNonword <- ifelse (item_data$BandNumber == -1, "nonword", "word")

item_data <- item_data[, c(1,2,3,4,12,5,6,7,8,9,10, 11)]

write.csv(item_data, file=paste0(experiment_abr,".item_data.csv"))

##--Get oscillations for each user only followed by an error------------------##


# Olha: output presentation correction
user_scores  <- data.frame()


for (rawUUID in participants_uuids$V1)  {
  
  
  user <- rawUUID
  
  print(user)
  userName <- subset(participants_data, userId == user)$workerId
  print(userName)
  
  user_summary <- subset(tagevents_data, UserId == user & 
                           EventTag == "user_summary" & 
                           TagValue1 == "row000001")
  
  ## Skip users that have no summary or that have a summary for only one round
  #if (!nrow(user_summary) || nrow(user_summary) == 1){
  #}
  user_summary_r1 <- user_summary[1,]
  user_summary_r2 <- user_summary[2,]
  
  user_summary_r1 <- lapply(strsplit(user_summary_r1$TagValue2,";"), 
                            function(x) x)
  user_summary_r2 <- lapply(strsplit(user_summary_r2$TagValue2,";"), 
                            function(x) x)
  user_score_r1 <- user_summary_r1[[1]][1]
  BestFastTrack_r1	<- user_summary_r1[[1]][2]
  Cycle2oscillation_r1	<- user_summary_r1[[1]][3]
  EnoughFineTuningStimuli_r1 <- user_summary_r1[[1]][4]
  Champion_r1 <- user_summary_r1[[1]][5]
  Loser_r1 <- user_summary_r1[[1]][5]
  
  user_score_r2 <- user_summary_r2[[1]][1]
  BestFastTrack_r2	<- user_summary_r2[[1]][2]
  Cycle2oscillation_r2	<- user_summary_r2[[1]][3]
  EnoughFineTuningStimuli_r2 <- user_summary_r2[[1]][4]
  Champion_r2 <- user_summary_r2[[1]][5]
  Loser_r2 <- user_summary_r2[[1]][5]
  
  fine_tuning_1user <- subset(fine_tuning, UserId == user)
  
  fine_tuning_rounds <- split_rounds(fine_tuning_1user)
  
  fine_tuning_round1 <- fine_tuning_rounds[["round1"]]
  fine_tuning_round2 <- fine_tuning_rounds[["round2"]]
  
  # Olha: removing rows which are headers (one of the minuses of the old version: there were rows that were headers which lead to erroneous data reading)
  fine_tuning_round1 <- subset(fine_tuning_round1, !startsWith(TagValue2,"BandNumber;Label;UserAnswer;IsAnswerCorrect;"))
  fine_tuning_round2 <- subset(fine_tuning_round2, !startsWith(TagValue2,"BandNumber;Label;UserAnswer;IsAnswerCorrect;"))
  
  #Olha: commenting out apparently redundant operation
  # fine_tuning_1user <- fine_tuning_1user[order(fine_tuning_1user[,4]), ]
  
  raw_bandnumbers_r1 <- fine_tuning_round1[,c("BandNumber", "IsUserCorrect")]
  raw_bandnumbers_r2 <- fine_tuning_round2[,c("BandNumber", "IsUserCorrect")]
  
  raw_bandnumbers_r1 <- remove_second_empty(raw_bandnumbers_r1)
  raw_bandnumbers_r2 <- remove_second_empty(raw_bandnumbers_r2)
  
  # bandnumbers_r1 <- reduce_to_single(raw_bandnumbers_r1)
  # bandnumbers_r2 <- reduce_to_single(raw_bandnumbers_r2)
  
  # preparing data for reduce_to_single (they are needed to treat special case "pain-in_ass" for that function)
  fast_track_1user <- subset(fast_track, UserId == user)
  fast_track_rounds <- split_rounds(fast_track_1user)
  
  fast_track_round1 <- fast_track_rounds[["round1"]]
  fast_track_round2 <- fast_track_rounds[["round2"]]
  
  fast_track_round1 <- subset(fast_track_round1, !startsWith(TagValue2,"Label;BandNumber;UserAnswer;IsAnswerCorrect;") )
  fast_track_round2 <- subset(fast_track_round2, !startsWith(TagValue2,"Label;BandNumber;UserAnswer;IsAnswerCorrect;"))
  # Olha (!): reduce_to_single gets now "fast_track_round1X" as a parameter
  # since it is used in one of the branches (see the comment in the code for this function)
  
  bandnumbers_r1 <- reduce_to_single(raw_bandnumbers_r1, fast_track_round1, add_isUserCorrect = TRUE)
  bandnumbers_r2 <- reduce_to_single(raw_bandnumbers_r2, fast_track_round2, add_isUserCorrect = TRUE)
  
  #two_oscillations_output_r1 <- get_two_oscillations(bandnumbers_r1)
  two_oscillations_output_r1 <- get_two_oscillations_with_error(bandnumbers_r1)
  num_2_oscillations_r1 <- two_oscillations_output_r1[['two_oscillations']]
  two_oscillations_score_r1 <- two_oscillations_output_r1[['score']]
  originalScoreCorrect_r1 <- as.integer(user_score_r1 == two_oscillations_score_r1)
  
  #two_oscillations_output_r2 <- get_two_oscillations(bandnumbers_r2)
  two_oscillations_output_r2 <- get_two_oscillations_with_error(bandnumbers_r2)
  num_2_oscillations_r2 <- two_oscillations_output_r2[['two_oscillations']]
  two_oscillations_score_r2 <- two_oscillations_output_r2[['score']]
  originalScoreCorrect_r2 <- as.integer(user_score_r2 == two_oscillations_score_r2)
  
  print(paste0("num 2-oscillations R1: ",as.character(num_2_oscillations_r1)))
  print(paste0("num 2-oscillations R2: ",as.character(num_2_oscillations_r2)))
  
  # Olha: updated output presentation
  new_user_scores <- data.frame(user, userName,
                                      user_score_r1, 
                                      BestFastTrack_r1,
                                      Cycle2oscillation_r1, 
                                      EnoughFineTuningStimuli_r1, 
                                      Champion_r1, Loser_r1,
                                      num_2_oscillations_r1, 
                                      two_oscillations_score_r1,
                                      originalScoreCorrect_r1,
                                      user_score_r2, 
                                      BestFastTrack_r2,
                                      Cycle2oscillation_r2, 
                                      EnoughFineTuningStimuli_r2, 
                                      Champion_r2, Loser_r2,
                                      num_2_oscillations_r2, 
                                      two_oscillations_score_r2,
                                      originalScoreCorrect_r2
  )
  
  names(new_user_scores) <- c("user", "userName", 
                              "score R1", 
                              "BestFastTrack R1",
                              "Cycle2oscillation R1", 
                              "EnoughFineTuningStimuli R1", 
                              "Champion R1", "Loser R1",
                              "num_2_oscillations R1", 
                              "2_oscillations_score R1",
                              "originalScoreCorrect R1",
                              "score R2", 
                              "BestFastTrack R2",
                              "Cycle2oscillation R2", 
                              "EnoughFineTuningStimuli R2", 
                              "Champion R2", "Loser R2",
                              "num_2_oscillations R2", 
                              "2_oscillations_score R2",
                              "originalScoreCorrect R2")
  
  user_scores <- rbind(user_scores, new_user_scores)
}

write.csv(user_scores, file=paste0(experiment_abr,".user_scores_w_error_test.csv"))

