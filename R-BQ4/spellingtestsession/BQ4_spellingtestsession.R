##--spellingtestsession ---------------------------------------------------------------##

library(jsonlite)
library(httr)
library(dplyr)
library(tidyr)
library(stringr)
source("../shared/BQ4_supportFunctions.R")

## parameters (specification) --------------------------------------------------####

n_stimuli <-  60
n_correct_words <- 30
n_incorrect_words <- 30


##--Configuration-------------------------------------------------------------##

experiment_abr <- "spellingtestsession"
server_url <- "https://frinexproduction.mpi.nl"
admin_url <- paste(server_url, "/", experiment_abr, "-admin", sep = "")


# Use this to skip users if users from earlier application versions with 
# incompatible data definitions are still in the database.


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


###--Get all data from an experiment-----------------------------------------###

participants_uuids <- read.csv("../shared/participants.csv", header = FALSE)

participants_columns <- c("userId", "workerId", "staleCopy", "submitDate")
participants <- get_embedded("participants", participants_columns, admin_url)


tagpairevents_columns <- c("userId", "screenName", "eventTag", "tagDate",
                           "tagValue1", "tagValue2")
tagpairevents <- get_embedded("tagpairevents", tagpairevents_columns, admin_url, 1000)
tagpairevents_data <- unique(tagpairevents[["currentData"]])


screenviews_columns <- c("userId", "screenName", "submitDate")
screenviews <- get_embedded("screenviews", screenviews_columns, admin_url)
screenviews_data <- screenviews[["currentData"]]


participants_data <- unique(subset(participants[["currentData"]], 
                                   staleCopy == FALSE))


participant_events <- left_join(tagpairevents[["currentData"]], 
                                participants_data, 
                                by = "userId")

stimulus_resp_columns <- c("userId", "screenName", "stimulusId", "response", "isCorrect", "tagDate")
stimulusresponses <- get_embedded("stimulusresponses", stimulus_resp_columns, admin_url, 1000)
stimulusresponses_data <- unique(subset(stimulusresponses[["currentData"]]))


user_scores <- data.frame()
user_responses <- data.frame()

users_multiple_submission <- data.frame()
stimulusIDs <- data.frame()


for (rawUUID in participants_uuids$V1)  {
  
  
  user <- paste0("uuid-", rawUUID)
  print(user)
  
  stimulusresponses_user_raw <- subset(stimulusresponses_data, userId==user)
  
  if(nrow(stimulusIDs) != n_stimuli) {
    stimulusIDs <- unique(subset(stimulusresponses_user_raw, select=c(stimulusId)))
    n_correct_check <- length(which(endsWith(stimulusIDs$stimulusId, "correct")))
    if (n_correct_check != n_correct_words) {
      print("Sanity error: number of correct-word-marked stimuli differs of the number of the correct words given in the specification")
      print( n_correct_check)
      print(n_correct_words)
      stop()
    }
    n_fout_check <- length(which(endsWith(stimulusIDs$stimulusId, "fout")))
    if (n_fout_check != n_incorrect_words) {
      print("Sanity error: number of incorrect-word-marked stimuli differs of the number of incorrect words given in the specification")
      print(n_fout_check)
      print(n_incorrect_words)
      stop()
    }
    
  }
  
  
  
  # on the multiple submission the last attempt is taken as the end result
  # the staring time is the time stamp of the last apperanace of the stimulus screen
  # the ending time  is the timestamp of the last scoring attempt
  score_events <- subset(tagpairevents_data,  userId == user & startsWith(tagValue2, "totalScore:"))
  score_events <- score_events[order(score_events[,"tagDate"]), ]
  
  if (nrow(score_events) > 1) {
    new_multiple_submission <- data.frame("userId"=c(user))
    users_multiple_submission <- rbind(users_multiple_submission, new_multiple_submission)
  }
  
  if (nrow(score_events) < 1) {
    print("Error in the submission of the participant's results")
    stop()
  }
  
  # last attempt to submit
  submitDate <- score_events[nrow(score_events),]$tagDate
  # submitDate <- subset(participants_data, userId == user)$submitDate
  
  
  userName <- subset(participants_data, userId == user)$workerId
  
  isAuthor <- 0
  user_response <- ""
  
  stimulus_responses_user <- data.frame()
  
  for (stimulusID in  stimulusIDs$stimulusId) {
    stimulus_event <- subset(stimulusresponses_user_raw, stimulusId == stimulusID)
    first_time <- max(stimulus_event$tagDate)
    stimulus_event <- subset(stimulus_event, tagDate ==  first_time)
    stimulus_responses_user <- rbind(stimulus_responses_user, stimulus_event)
  }
  
  
  print("Num responses for user:")
  print(nrow(stimulus_responses_user))
  
  
  
  if (nrow(stimulus_responses_user) != n_stimuli) {
    print("Something went terribly wrong: the amount of rows of the post-processed table in the R-script still differs from the # of stimuli")
    print(nrow(stimulus_responses_user))
    print(n_stimuli)
    stop()
  }
  
  stimulus_responses_user$isFoutWoord <- ifelse(endsWith(stimulus_responses_user$stimulusId, "fout"), 1, 0)
  stimulus_responses_user$response <- ifelse(stimulus_responses_user$response == "true", 1, 0)
  stimulus_responses_user$isResponseCorrect <- ifelse(stimulus_responses_user$isCorrect == TRUE, 1, 0)
  
  #sanity
  for (i in 1: nrow(stimulus_responses_user)) {
    
    if (!endsWith(stimulus_responses_user[i,]$stimulusId, "correct") && !endsWith(stimulus_responses_user[i,]$stimulusId, "fout")) {
      print("Sanity error: the following stimulus is not marked as a correct or a fout")
      print(stimulus_responses_user[i,]$stimulusId)
      stop()
    }
    if (endsWith(stimulus_responses_user[i,]$stimulusId, "correct") && (stimulus_responses_user[i,]$isFoutWoord == 1)) {
      print("Sanity error: the following stimulus is marked as fout")
      print(stimulus_responses_user[i,]$stimulusId)
      stop()
    }
    
    if (endsWith(stimulus_responses_user[i,]$stimulusId, "fout") && (stimulus_responses_user[i,]$isFoutWoord == 0)) {
      print("Sanity error: the following stimulus is marked as correctr")
      print(stimulus_responses_user[i,]$stimulusId)
      stop()
    }
    
    if (stimulus_responses_user[i,]$isFoutWoord == 0) {
      if  (stimulus_responses_user[i,]$response == 1) {
        if (stimulus_responses_user[i,]$isResponseCorrect == 1) {
          print("Sanity error: misevaluation of the response")
          print(stimulus_responses_user[i,]$stimulusId)
          stop()
        }
      }
    }
    if (stimulus_responses_user[i,]$isFoutWoord == 0) {
      if  (stimulus_responses_user[i,]$response == 0) {
        if (stimulus_responses_user[i,]$isResponseCorrect == 0) {
          print("Sanity error: misevaluation of the response")
          print(stimulus_responses_user[i,]$stimulusId)
          stop()
        }
      }
    }
    if (stimulus_responses_user[i,]$isFoutWoord == 1) {
      if  (stimulus_responses_user[i,]$response == 0) {
        if (stimulus_responses_user[i,]$isResponseCorrect == 1) {
          print("Sanity error: misevaluation of the response")
          print(stimulus_responses_user[i,]$stimulusId)
          stop()
        }
      }
    }
    if (stimulus_responses_user[i,]$isFout == 1) {
      if  (stimulus_responses_user[i,]$response == 1) {
        if (stimulus_responses_user[i,]$isResponseCorrect == 0) {
          print("Sanity error: misevaluation of the response")
          print(stimulus_responses_user[i,]$stimulusId)
          stop()
        }
      }
    }
  }
  
  
  stimulus_responses_user$submitDate <- submitDate
  
  
  
  # sanity check
  for (stimulusID  in stimulusIDs$stimulusId) {
    respRow <- subset(stimulus_responses_user, stimulusId==stimulusID)
    if (nrow(respRow) != 1) {
      print("Sanity error: more than 1 rows (or no rows at all) in posprocessed tag-pair events table, for stimulus")
      print(stimulusId)
      stop()
    }
  }
  
  
  user_responses <- rbind(user_responses, stimulus_responses_user)
  
  
  
  TP <- length(which(stimulus_responses_user$response == 1 & 
                       stimulus_responses_user$isFout == 1))
  
  FP <- length(which(stimulus_responses_user$response == 1 & 
                       stimulus_responses_user$isFout == 0))
  
  TN <- length(which(stimulus_responses_user$response == 0 & 
                       stimulus_responses_user$isFout == 0))
  
  FN <- length(which(stimulus_responses_user$response == 0 & 
                       stimulus_responses_user$isFout == 1))
  
  
  
  # sanity check 
  n_correct <- length(which(stimulus_responses_user$isResponseCorrect == 1))
  n_incorrect <- length(which(stimulus_responses_user$isResponseCorrect == 0))
  
  if (as.numeric(n_correct) + as.numeric(n_incorrect) != n_stimuli) {
    print("Sanity error: the sum  of correct and incorrect answers differs form the overall amount of stimuli")
    print(n_correct)
    print(n_incorrect)
    print(n_stimuli)
    stop()
  }
  
  if (as.numeric(TP) + as.numeric(FN) != as.numeric(n_incorrect_words) ) {
    print("Sanity error: the sum  of true positives and false negatives computed by R differs from the total amount of 'fout' words")
    print(TP)
    print(TN)
    print(scoreFrinex)
    stop()
  }
  
  if (as.numeric(TN) + as.numeric(FP) != as.numeric(n_correct_words) ) {
    print("Sanity error: the sum  of false positives and true negatives computed by R differs from the total amount of correct words")
    print(TN)
    print(FP)
    print(scoreFrinex)
    stop()
  }
  
  if (as.numeric(TP) + as.numeric(TN) != as.numeric(n_correct) ) {
    print("Sanity error: the sum  of true positives and negatives computed by R differs from the total amount of correct participant's answers")
    print(TP)
    print(TN)
    print(scoreFrinex)
    stop()
  }
  
  if (as.numeric(FP) + as.numeric(FN) != as.numeric(n_incorrect) ) {
    print("Sanity error: the sum  of false positives and negatives computed by R differs from ttotal amount of incorrect participant's answers")
    print(FP)
    print(FN)
    print(errorsFrinex)
    stop()
  }
  
  
  # Get test duration for participant from screenviews
  endTime <- strptime(substring(submitDate, 1, nchar(submitDate)-9), 
                      format = "%Y-%m-%dT%H:%M:%S")
  # start of the experiment is the last appearance of the stimuliScreen 
  screenviews_1user <- subset(screenviews_data, 
                              userId == user & screenName == "stimuliScreenSpellingCheck")
  screenviews_1user <- screenviews_1user[order(screenviews_1user[,"submitDate"]), ]
  
  startDate <- screenviews_1user[nrow(screenviews_1user),]$submitDate
  
  startTime <- strptime(substring(startDate, 1, nchar(startDate)-9), 
                        format = "%Y-%m-%dT%H:%M:%S")
  
  
  
  startTime_s <- as.numeric(startTime)
  endTime_s <- as.numeric(endTime)
  testDuration <- round((endTime_s - startTime_s)/60,2)
  
  print("startDate")
  print(startDate)
  print("endTime")
  print(submitDate)
  print("test duration")
  print(paste0(as.character(testDuration), " min"))
  
  new_user_scores <- data.frame(user, TP, FP, TN, FN, testDuration, startTime, endTime)
  names(new_user_scores) <- c("userId", 
                              "Hits (True Positives)",
                              "False Alarm (False Positives)",
                              "Correct Rejection (True Negative)",
                              "Missing (False Negative)",
                              "Test Duration min",
                             "Start time",
                             "End time"
  ) 
  user_scores <- rbind(user_scores, new_user_scores)
  
} 

if  (nrow(user_scores) != nrow(participants_uuids)) {
  print("sanity-check error: number of the participants for which the scores are counted, differs form the amount of the given participants")
  print(nrow(user_scores))
  print(nrow(participants_uuids))
  stop()
}



# Write scores to file
write.csv(user_scores, file=paste0(experiment_abr,".user_scores.csv"))

#user_responses <- user_responses[order(user_responses[,3], user_responses[,5]), ]
write.csv(user_responses, file=paste0(experiment_abr,".item_data.csv"))
