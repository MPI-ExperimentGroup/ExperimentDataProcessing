##--Stairs for words English (2 rounds)---------------------------------------##

library(jsonlite)
library(httr)
library(dplyr)
library(tidyr)
source("../shared/BQ4_supportFunctions.R")


##--Configuration-------------------------------------------------------------##

experiment_abr <- "stairs4words2"
server_url <- "http://frinexproduction.mpi.nl"
admin_url <- paste(server_url, "/", experiment_abr, "-admin", sep = "")

# Newer version uses "tagpairevents"
tagevents <- "tagpairevents"

# These variables have different values for the Dutch and English versions of
# Stairs4words
nonWordValue <- "-1"
column_labels_row_label <- "Label"
start_screenName <- "stimuliScreenV"

##--Authentication------------------------------------------------------------##

## Read username/password from file and authenticate on Admin interface

auth_values <- read.table(paste("../login.", experiment_abr, ".csv", sep = ""), 
                          stringsAsFactors = FALSE)

auth_body <-
  list(
    username=auth_values$username[1],
    password=auth_values$password[1]
  ) 

login_url <- paste(admin_url, "login", sep = "/")
req_auth <- httr::POST(login_url, body = auth_body)

##--Get data from REST interface----------------------------------------------##

participants_uuids <- read.csv("../shared/participants.csv", header = FALSE)

participants_columns <- c("userId", "workerId")
participants <- get_embedded("participants", participants_columns, admin_url)
participants_data <- unique(subset(participants[["currentData"]]))

tagevents_columns <- c("userId", "screenName", "eventTag", 
                       "tagValue1", "tagValue2", "tagDate")
tagevents <- get_embedded(tagevents, tagevents_columns, admin_url, 1000)

tagevents_data <- unique(tagevents[["currentData"]])

screenviews_columns <- c("userId", "screenName", "viewDate")
screenviews <- get_embedded("screenviews", screenviews_columns, admin_url, 0)
screenviews_data <- unique(subset(screenviews[["currentData"]]))

##--Pre-process fast track and finetuning-------------------------------------##

fast_track <- subset(tagevents_data, eventTag == "fast_track")

fast_track$label <- lapply(strsplit(fast_track$tagValue2,";"), 
                           function(x) x[1])

fast_track$BandNumber <- lapply(strsplit(fast_track$tagValue2,";"), 
                                function(x) x[2])

fast_track$UserAnswer <- lapply(strsplit(fast_track$tagValue2,";"), 
                                function(x) x[3])

fast_track$IsUserCorrect <- lapply(strsplit(fast_track$tagValue2,";"), 
                                   function(x) x[4])

fast_track$NonwordsFrequencyAtThisPoint <- lapply(strsplit(fast_track$tagValue2,";"), 
                                                  function(x) x[6])

fine_tuning <- subset(tagevents_data, eventTag == "fine_tuning")

# Ordering of label and BandNumber is switched compared to fast_track.
# This is not the case in the English version
fine_tuning$label <- lapply(strsplit(fine_tuning$tagValue2,";"), 
                            function(x) x[2])

fine_tuning$BandNumber <- lapply(strsplit(fine_tuning$tagValue2,";"), 
                                 function(x) x[1])

fine_tuning$UserAnswer <- lapply(strsplit(fine_tuning$tagValue2,";"), 
                                 function(x) x[3])

fine_tuning$IsUserCorrect <- lapply(strsplit(fine_tuning$tagValue2,";"), 
                                    function(x) x[4])

fine_tuning$NonwordsFrequencyAtThisPoint <- NA

rating_buttons <- subset(tagevents_data, eventTag == "RatingButton")

rating_buttons$label <- lapply(strsplit(rating_buttons$tagValue1,"_"), 
                               function(x) x[1])

##--Functions-----------------------------------------------------------------##

## split_rounds() splits the fine_tuning rows into a first and second round 

split_rounds <- function(fine_tuning){
  round_starts <- as.vector(c())
  for(row in 1:nrow(fine_tuning)){
    if (fine_tuning$tagValue1[row] == "row000000"){
      round_starts <- rbind(round_starts, row)
    }
  }
  return(list(round1 = fine_tuning[1:round_starts[2]-1,], 
              round2 = fine_tuning[round_starts[2]:nrow(fine_tuning),]))
}

##--Write raw item data-------------------------------------------------------##

item_data  <- as.vector(c())

for (rawUUID in participants_uuids$V1)  {
  
  user <- paste0("uuid-", rawUUID)
  print(user)
  
  combined_2_rounds <- as.vector(c())
  
  # Skip participants without item data
  if (nrow(subset(fast_track, userId == user)) == 0){
    next
  }
  
  
  # Skip participants for whom start of fast track round 1 can't be determined
  fast_track_R1_start <- subset(screenviews_data, userId == user & 
           screenName == paste0(start_screenName,1))[,c("viewDate")]
  if (length(fast_track_R1_start) == 0){
    next
  }
  
  # Skip participants for whom start of fast track round 1 can't be determined
  fast_track_R2_start <- subset(screenviews_data, userId == user & 
            screenName == paste0(start_screenName,2))[,c("viewDate")]
  if (length(fast_track_R2_start) == 0){
    next
  }
  
  for(round in c("1","2")){
    round_screenName <- paste0(start_screenName,round)
    
    # select raw data for one user
    fine_tuning_1user <- subset(fine_tuning, userId == user & 
                                  screenName == round_screenName)
    fast_track_1user <- subset(fast_track, userId == user & 
                                 screenName == round_screenName)
    rating_buttons_1user <- subset(rating_buttons, userId == user & 
                                     screenName == round_screenName)
    
    # convert all columns to character type
    fine_tuning_1user[] <- lapply(fine_tuning_1user, as.character)
    fast_track_1user[] <- lapply(fast_track_1user, as.character)
    rating_buttons_1user[] <- lapply(rating_buttons_1user, as.character)
    
    # restore ordering
    fine_tuning_1user <- fine_tuning_1user[order(fine_tuning_1user[,4]), ]
    fast_track_1user <- fast_track_1user[order(fast_track_1user[,4]), ]
    
    # Remove empty rows
    fine_tuning_1user <- subset(fine_tuning_1user, tagValue2 != " ; ; ; ; ; ;")
    fast_track_1user <- subset(fast_track_1user, tagValue2 != " ; ; ; ; ; ;")
    
    # Concatenate fast track and fine tuning data
    item_data_1user <- rbind(fast_track_1user, fine_tuning_1user)
    
    # Get start of experiment from screenViews
    fast_track_start <- max(subset(screenviews_data, 
                                   userId == user & 
                                     screenName == round_screenName)[,c("viewDate")])
    # Join concatenated item data to rating button data to get fine grained 
    # timestamps in miliseconds
    combined_1_user <- left_join(item_data_1user, 
                                 rating_buttons_1user[,c("userId", "label", "tagDate")], 
                                 by = c("userId", "label"))
    
    # Add start of experiment so duration of first item can be calculated
    combined_1_user$tagDate.y[1] <- fast_track_start
    
    # Remove row consisting of column labels for fine_tuning
    combined_1_user <- subset(combined_1_user, 
                              !(label == column_labels_row_label & 
                                  eventTag == "fine_tuning"))
    
    
    # Calculate item durations
    combined_1_user$duration <- c(0, difftime(
      strptime(combined_1_user$tagDate.y[2:nrow(combined_1_user)], 
               format = "%Y-%m-%dT%H:%M:%OS+0000"),
      strptime(combined_1_user$tagDate.y[1:nrow(combined_1_user)-1], 
               format = "%Y-%m-%dT%H:%M:%OS+0000")
    ))
    combined_2_rounds <- rbind(combined_2_rounds, combined_1_user)
  }
  
  # Round duration to miliseconds
  combined_2_rounds$duration <- round(combined_2_rounds$duration, 3)
  
  # Remove row consisting of column labels for fast track
  combined_2_rounds <- subset(combined_2_rounds, label != column_labels_row_label)
  
  # Change values for IsUserCorrect and UserAnswer from true/false to 1/0
  combined_2_rounds$UserAnswer <- ifelse(combined_2_rounds$UserAnswer == "true", 1, 0)
  combined_2_rounds$IsUserCorrect <- ifelse(combined_2_rounds$IsUserCorrect == "true", 1, 0)
  
  
  # Select only required columns
  combined_2_rounds <- combined_2_rounds[,c("userId", "label", 
                                        "BandNumber", 
                                        "UserAnswer", "IsUserCorrect", 
                                        "duration", "eventTag",
                                        "NonwordsFrequencyAtThisPoint")]
  
  # Rename 'eventTag' column to 'phase'
  names(combined_2_rounds)[names(combined_2_rounds) == 'eventTag'] <- 'phase'
  
  # Add raw data for this user to complete item data
  item_data <- rbind(item_data, combined_2_rounds)
}

# Write item data to file
write.csv(item_data, file=paste0(experiment_abr,".item_data.csv"))


##--Get aggregates for each user----------------------------------------------##

user_scores  <- data.frame()

for (rawUUID in participants_uuids$V1)  {
  
  user <- paste0("uuid-", rawUUID)
  
  print(user)
  
  user_summary <- subset(tagevents_data, userId == user & 
                           eventTag == "user_summary" & 
                           tagValue1 == "row000001")
  
  ## Skip users that have no summary or that have a summary for only one round
  if (!nrow(user_summary) || nrow(user_summary) == 1){
    next
  }
  user_summary_r1 <- user_summary[1,]
  user_summary_r2 <- user_summary[2,]
  
  user_summary_r1 <- lapply(strsplit(user_summary_r1$tagValue2,";"), 
                            function(x) x)
  user_summary_r2 <- lapply(strsplit(user_summary_r2$tagValue2,";"), 
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
  
  fast_track_1user <- subset(fast_track, userId == user)
  
  fast_track_rounds <- split_rounds(fast_track_1user)
  
  fast_track_round1 <- fast_track_rounds[["round1"]]
  fast_track_round2 <- fast_track_rounds[["round2"]]
  
  last_fast_track_r1 <- fast_track_round1[nrow(fast_track_round1),]
  
  fast_track_r1_BandNumber <- lapply(strsplit(last_fast_track_r1$tagValue2,";"), 
                                     function(x) x[2])[[1]]
  
  switchOnNonWord_r1 <- ifelse(fast_track_r1_BandNumber == nonWordValue, 1, 0)
  
  last_fast_track_r2 <- fast_track_round2[nrow(fast_track_round2),]
  
  fast_track_r2_BandNumber <- lapply(strsplit(last_fast_track_r2$tagValue2,";"), 
                                     function(x) x[2])[[1]]
  
  switchOnNonWord_r2 <- ifelse(fast_track_r2_BandNumber == nonWordValue, 1, 0)
  
  new_user_scores <- data.frame(user, 
                                user_score_r1, 
                                BestFastTrack_r1,
                                Cycle2oscillation_r1, 
                                EnoughFineTuningStimuli_r1, 
                                Champion_r1, Loser_r1,
                                switchOnNonWord_r1,
                                user_score_r2, 
                                BestFastTrack_r2,
                                Cycle2oscillation_r2, 
                                EnoughFineTuningStimuli_r2, 
                                Champion_r2, Loser_r2,
                                switchOnNonWord_r2
  )
  
  names(new_user_scores) <- c("user",  
                              "score_R1", 
                              "BestFastTrack_R1",
                              "Cycle2oscillation_R1", 
                              "EnoughFineTuningStimuli_R1", 
                              "Champion_R1", "Loser_R1",
                              "switchOnNonWord_R1",
                              "score_R2", 
                              "BestFastTrack_R2",
                              "Cycle2oscillation_R2", 
                              "EnoughFineTuningStimuli_R2", 
                              "Champion_R2", "Loser_R2",
                              "switchOnNonWord_R2")
  
  user_scores <- rbind(user_scores, new_user_scores)  
}

user_scores$Cycle2oscillation_R1 <- ifelse(
  user_scores$Cycle2oscillation_R1 == "true", 1, 0)
user_scores$Cycle2oscillation_R2 <- ifelse(
  user_scores$Cycle2oscillation_R2 == "true", 1, 0)

user_scores$EnoughFineTuningStimuli_R1 <- ifelse(
  user_scores$EnoughFineTuningStimuli_R1 == "true", 1, 0)
user_scores$EnoughFineTuningStimuli_R2 <- ifelse(
  user_scores$EnoughFineTuningStimuli_R2 == "true", 1, 0)
user_scores$Champion_R1 <- ifelse(user_scores$Champion_R1 == "true", 1, 0)
user_scores$Champion_R2 <- ifelse(user_scores$Champion_R2 == "true", 1, 0)
user_scores$Loser_R1 <- ifelse(user_scores$Loser_R1 == "true", 1, 0)
user_scores$Loser_R2 <- ifelse(user_scores$Loser_R2 == "true", 1, 0)

write.csv(user_scores, file=paste0(experiment_abr,".user_scores.csv"))
