##--corsisession----------------------------------------------------------------##

library(jsonlite)
library(httr)
library(dplyr)
library(tidyr)
library(stringr)
source("../shared/BQ4_supportFunctions.R")

##--Configuration-------------------------------------------------------------##


experiment_abr <- "corsisession"
server_url <- "http://frinexproduction.mpi.nl"

admin_url <- paste(server_url, "/", experiment_abr, "-admin", sep = "")



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

##--Main----------------------------------------------------------------------##


stimulus_resp_columns <- c("userId", "screenName", "stimulusId", "response", "isCorrect")
stimulusresponses <- get_embedded("stimulusresponses", stimulus_resp_columns, admin_url, 1000)
stimulusresponses_data <- subset(stimulusresponses[["currentData"]])

participants_uuids <- read.csv("../shared/participants.csv", header = FALSE)

screenviews_columns <- c("userId", "screenName", "viewDate")
screenviews <- get_embedded("screenviews", screenviews_columns, admin_url)
screenviews_data <- unique(subset(screenviews[["currentData"]]))


responsesPracticeForward <- unique(subset(stimulusresponses_data, screenName=="RunTrialsPracticeForward" & !is.na(response) & nchar(str_trim(response))>0 ))
responsesPracticeBackward <- unique(subset(stimulusresponses_data,  screenName=="RunTrialsPracticeBackward" & !is.na(response) & nchar(str_trim(response))>0))
responsesForward <- unique(subset(stimulusresponses_data, screenName=="RunTrialsForward" & !is.na(response) & nchar(str_trim(response))>0))
responsesBackward <- unique(subset(stimulusresponses_data,  screenName=="RunTrialsBackward" & !is.na(response) & nchar(str_trim(response))>0))

## help function 

harvest_scores <- function(listUuids, responses, roundname, screenviews_info, startScreenName, endScreenName) {
  
  user_scores <- data.frame()
  user_responses <- data.frame()
  
  for (rawUUID in listUuids){
    
    
    user <- paste0("uuid-", rawUUID)
    print(user)
    print("Num events for user:")
    
    raw_user_responses <- subset(responses, userId == user)
   
    print(nrow(raw_user_responses))
    
    # order responses by time
    # raw_user_responses <- raw_user_responses[order(raw_user_responses[,2]), ]
    new_user_responses <- data.frame()
    
    for (stimulusID in unique(raw_user_responses$stimulusId)) {
      
       new_user_stimulus_response <- subset(raw_user_responses, stimulusId == stimulusID)
      
        # adding column representing stimulus sequence length
       length_from_id <-  ifelse(grepl("Practice", stimulusID), "Length2", strsplit(stimulusID, "_")[2])
       new_user_stimulus_response$stimulusLength <- substr(length_from_id, 7, nchar(length_from_id))
      
       
       # adding column representing response length
       new_user_stimulus_response$responseLength <-  nchar(new_user_stimulus_response$response)
       
        # selecting the row with the maxinal response length (this is the final response)
       max_resp_length <-  max(new_user_stimulus_response$responseLength)
       new_user_stimulus_response <- subset(new_user_stimulus_response,responseLength == max_resp_length)
       
       #evaluating response 
       new_user_stimulus_response$isCorrect <- 0
       new_user_stimulus_response$isCorrect <- ifelse(new_user_stimulus_response$stimulusLength == 2 && new_user_stimulus_response$response=="12", 1, new_user_stimulus_response$isCorrect )
       new_user_stimulus_response$isCorrect <- ifelse(new_user_stimulus_response$stimulusLength == 3 && new_user_stimulus_response$response=="123", 1, new_user_stimulus_response$isCorrect )
       new_user_stimulus_response$isCorrect <- ifelse(new_user_stimulus_response$stimulusLength == 4 && new_user_stimulus_response$response=="1234", 1, new_user_stimulus_response$isCorrect )
       new_user_stimulus_response$isCorrect <- ifelse(new_user_stimulus_response$stimulusLength == 5 && new_user_stimulus_response$response=="12345", 1, new_user_stimulus_response$isCorrect )
       new_user_stimulus_response$isCorrect <- ifelse(new_user_stimulus_response$stimulusLength == 6 && new_user_stimulus_response$response=="123456", 1, new_user_stimulus_response$isCorrect )
       new_user_stimulus_response$isCorrect <- ifelse(new_user_stimulus_response$stimulusLength == 7 && new_user_stimulus_response$response=="1234567", 1, new_user_stimulus_response$isCorrect )
       new_user_stimulus_response$isCorrect <- ifelse(new_user_stimulus_response$stimulusLength == 8 && new_user_stimulus_response$response=="12345678", 1, new_user_stimulus_response$isCorrect )
       new_user_stimulus_response$isCorrect <- ifelse(new_user_stimulus_response$stimulusLength == 9 && new_user_stimulus_response$response=="123456789", 1, new_user_stimulus_response$isCorrect )
       
       new_user_responses <- rbind(new_user_responses, new_user_stimulus_response)
       
    }
    
   
    
    
    user_responses <- rbind(user_responses, new_user_responses)
    
    
    # filling in scores
    n_correct <- length(which(new_user_responses$isCorrect == 1))
    n_incorrect <- length(which(new_user_responses$isCorrect == 0))
    
    
    if (n_correct > 0) {
      correct_rows <- subset(new_user_responses, isCorrect == 1)
      max_length <- max(correct_rows$stimulusLength)
    } else {
      max_length <- " " 
    }
    
    # Get test duration for participant from screenviews
    screenviews_1user <- subset(screenviews_info, userId == user)
    
    test_duration <- get_test_duration(screenviews_1user, 
                                       startScreenName, nchar(startScreenName), endScreenName, TRUE)
    
    startTime <- test_duration[["startTime"]]
    endTime <- test_duration[["endTime"]]
    testDuration <- test_duration[["testDuration"]]
    
    print("startTime")
    print(startTime)
    print("endTime")
    print(endTime)
    print("test duration")
    print(paste0(as.character(testDuration), " minutes"))
    
    new_user_scores <- data.frame(user, n_correct, n_incorrect, max_length, testDuration)
    names(new_user_scores) <- c("userId", 
                                paste0(roundname, "Correct"),
                                paste0(roundname,  "Incorrect"),
                                paste0(roundname, " max length achieved"),
                                paste0(roundname, " duration (sec)")
    ) 
    user_scores<- rbind(user_scores, new_user_scores)
    
  }
  
  return(list(user_responses=user_responses, user_scores=user_scores))
 
  
}

##--Get aggregates ----------------------------------------------##

results_practice_forward <- harvest_scores(participants_uuids$V1, responsesPracticeForward, "practiceForward", screenviews_data, "RunTrialsPracticeForward", "delayForward")
user_scores_practice_forward <- results_practice_forward$user_scores
user_responses_practice_forward <- results_practice_forward$user_responses

results_forward <- harvest_scores(participants_uuids$V1, responsesForward, "forward", screenviews_data, "RunTrialsForward", "instructionsBackward")
user_scores_forward <- results_forward$user_scores
user_responses_forward <- results_forward$user_responses



results_practice_backward <- harvest_scores(participants_uuids$V1, responsesPracticeBackward, "practiceBackwrard", screenviews_data, "RunTrialsBackwardPractice", "delayBackward")
user_scores_practice_backward <- results_practice_backward$user_scores
user_responses_practice_backward <- results_practice_backward$user_responses

results_backward <- harvest_scores(participants_uuids$V1, responsesBackward, "backward", screenviews_data, "RunTrialsBackward", "Admin")
user_scores_backward <- results_backward$user_scores
user_responses_backward <- results_backward$user_responses

user_practice_scores <- merge(user_scores_practice_forward, user_scores_practice_backward)
user_practice_scores$totalCorrect <- (user_practice_scores$forwardCorrect + user_practice_scores$backwardCorrect)
user_practice_scores$totalIncorrect <- (user_practice_scores$forwardIncorrect + user_practice_scores$backwardIncorrect)
write.csv(user_practice_scores, file=paste0(experiment_abr,".user_practice_scores.csv"))

user_scores <- merge(user_scores_forward, user_scores_backward)
user_scores$totalCorrect <- (user_scores$forwardCorrect + user_scores$backwardCorrect)
user_scores$totalIncorrect <- (user_scores$forwardIncorrect + user_scores$backwardIncorrect)
write.csv(user_scores, file=paste0(experiment_abr,".user_scores.csv"))

