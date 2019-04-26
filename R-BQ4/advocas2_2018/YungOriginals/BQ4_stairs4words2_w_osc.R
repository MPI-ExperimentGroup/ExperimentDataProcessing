##--Stairs for words----------------------------------------------------------##

library(jsonlite)
library(httr)
library(dplyr)
library(tidyr)
source("shared/BQ4_supportFunctions.R")


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

tagevents <- read.csv("src/stairs4words2_results/tagpairdata.csv", stringsAsFactors = FALSE)

#tagevents_columns <- c("userId", "screenName", "eventTag", 
#                           "tagValue")
#tagevents <- get_embedded("tagevents", tagevents_columns, admin_url, 1000)

#tagevents_data <- unique(tagevents[["currentData"]])
tagevents_data <- tagevents

# concatenate fast track and finetuning

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

##--Functions-----------------------------------------------------------------##

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

user_scores  <- as.vector(c())
user_scores <- rbind(user_scores, c("user", "userName", 
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
                                    "originalScoreCorrect R2"
                                    ))

for(user in participants_data$userId){
  print(user)
  userName <- subset(participants_data, userId == user)$workerId
  print(userName)
  
  user_summary <- subset(tagevents_data, UserId == user & 
                           EventTag == "user_summary" & 
                           TagValue1 == "row000001")
  
  ## Skip users that have no summary or that have a summary for only one round
  if (!nrow(user_summary) || nrow(user_summary) == 1){
    next
  }
  user_summary_r1 <- user_summary[1,]
  user_summary_r2 <- user_summary[2,]
  
  user_summary_r1 <- lapply(strsplit(user_summary_r1$TagValue2,";"), 
                         function(x) x)
  user_summary_r2 <- lapply(strsplit(user_summary_r2$TagValue2,";"), 
                            function(x) x)
  user_score_r1 <- user_summary_r1[[1]][1]
  user_score_r1 <- user_summary_r1[[1]][1]
  BestFastTrack_r1	<- user_summary_r1[[1]][2]
  Cycle2oscillation_r1	<- user_summary_r1[[1]][3]
  EnoughFineTuningStimuli_r1 <- user_summary_r1[[1]][4]
  Champion_r1 <- user_summary_r1[[1]][5]
  Loser_r1 <- user_summary_r1[[1]][5]

  user_score_r2 <- user_summary_r2[[1]][1]
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
  
  fine_tuning_1user <- fine_tuning_1user[order(fine_tuning_1user[,4]), ]
  #raw_bandnumbers <- fine_tuning_1user$BandNumber
  raw_bandnumbers_r1 <- fine_tuning_round1[,c("BandNumber", "IsUserCorrect")]
  raw_bandnumbers_r2 <- fine_tuning_round2[,c("BandNumber", "IsUserCorrect")]
  
  raw_bandnumbers_r1 <- remove_second_empty(raw_bandnumbers_r1)
  raw_bandnumbers_r2 <- remove_second_empty(raw_bandnumbers_r2)
  
  bandnumbers_r1 <- reduce_to_single(raw_bandnumbers_r1)
  bandnumbers_r2 <- reduce_to_single(raw_bandnumbers_r2)
  
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
  user_scores <- rbind(user_scores, c(user, userName,
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
                                      ))
}

write.csv(user_scores, file=paste0(experiment_abr,".user_scores.csv"))

##--Get oscillations for each user only followed by an error------------------##


user_scores  <- as.vector(c())
user_scores <- rbind(user_scores, c("user", "userName", 
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
                                    "originalScoreCorrect R2"
))

for(user in participants_data$userId){
  print(user)
  userName <- subset(participants_data, userId == user)$workerId
  print(userName)
  
  user_summary <- subset(tagevents_data, UserId == user & 
                           EventTag == "user_summary" & 
                           TagValue1 == "row000001")
  
  ## Skip users that have no summary or that have a summary for only one round
  if (!nrow(user_summary) || nrow(user_summary) == 1){
    next
  }
  user_summary_r1 <- user_summary[1,]
  user_summary_r2 <- user_summary[2,]
  
  user_summary_r1 <- lapply(strsplit(user_summary_r1$TagValue2,";"), 
                            function(x) x)
  user_summary_r2 <- lapply(strsplit(user_summary_r2$TagValue2,";"), 
                            function(x) x)
  user_score_r1 <- user_summary_r1[[1]][1]
  user_score_r1 <- user_summary_r1[[1]][1]
  BestFastTrack_r1	<- user_summary_r1[[1]][2]
  Cycle2oscillation_r1	<- user_summary_r1[[1]][3]
  EnoughFineTuningStimuli_r1 <- user_summary_r1[[1]][4]
  Champion_r1 <- user_summary_r1[[1]][5]
  Loser_r1 <- user_summary_r1[[1]][5]
  
  user_score_r2 <- user_summary_r2[[1]][1]
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
  
  fine_tuning_1user <- fine_tuning_1user[order(fine_tuning_1user[,4]), ]
  
  raw_bandnumbers_r1 <- fine_tuning_round1[,c("BandNumber", "IsUserCorrect")]
  raw_bandnumbers_r2 <- fine_tuning_round2[,c("BandNumber", "IsUserCorrect")]
  
  raw_bandnumbers_r1 <- remove_second_empty(raw_bandnumbers_r1)
  raw_bandnumbers_r2 <- remove_second_empty(raw_bandnumbers_r2)
  
  # bandnumbers_r1 <- reduce_to_single(raw_bandnumbers_r1)
  # bandnumbers_r2 <- reduce_to_single(raw_bandnumbers_r2)
  
  bandnumbers_r1 <- reduce_to_single(raw_bandnumbers_r1, add_isUserCorrect = TRUE)
  bandnumbers_r2 <- reduce_to_single(raw_bandnumbers_r2, add_isUserCorrect = TRUE)
  
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
  user_scores <- rbind(user_scores, c(user, userName,
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
  ))
}

write.csv(user_scores, file=paste0(experiment_abr,".user_scores_w_error_test.csv"))

