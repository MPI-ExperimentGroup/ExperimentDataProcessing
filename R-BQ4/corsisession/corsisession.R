##--corsisession----------------------------------------------------------------##

library(jsonlite)
library(httr)
library(dplyr)
library(tidyr)
library(stringr)
source("../shared/BQ4_supportFunctions.R")

# Specification -------------------------------------------------------------##

n_bands <-  7
n_bands_practice <-  2
n_stimuli_per_length <- 2
max_n_allowed_errors <- 1
min_length <- 2
max_length <- 9

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


stimulus_resp_columns <- c("userId", "screenName", "stimulusId", "response", "isCorrect", "tagDate")
stimulusresponses <- get_embedded("stimulusresponses", stimulus_resp_columns, admin_url, 1000)
stimulusresponses_data <- stimulusresponses[["currentData"]]

responsesForwardCheck <- subset(stimulusresponses_data, screenName=="RunTrialsForward" & !is.na(isCorrect) )
responsesBackwardCheck <-subset(stimulusresponses_data,  screenName=="RunTrialsBackward" & !is.na(isCorrect) )


participants_uuids <- read.csv("../shared/participants.csv", header = FALSE, sep=",", fileEncoding = "UTF-8-BOM")

screenviews_columns <- c("userId", "screenName", "viewDate")
screenviews <- get_embedded("screenviews", screenviews_columns, admin_url)
screenviews_data <- unique(screenviews[["currentData"]])


responsesPracticeForward <- unique(subset(stimulusresponses_data, screenName=="RunTrialsPracticeForward" & !is.na(response)))
responsesPracticeBackward <- unique(subset(stimulusresponses_data,  screenName=="RunTrialsBackwardPractice" & !is.na(response)))

responsesForward <- unique(subset(stimulusresponses_data, screenName=="RunTrialsForward" & !is.na(response) &  is.na(isCorrect)))
responsesBackward <- unique(subset(stimulusresponses_data,  screenName=="RunTrialsBackward" & !is.na(response) & is.na(isCorrect)))


users_multiple_submission <- data.frame(userId=character(), reason=character(), details = character())




## help function 

harvest_scores <- function(listUuids, responses, roundname, screenviews_info, startScreenName, endScreenName, checkResponses = NULL) {
  
  user_scores <- data.frame()
  user_responses <- data.frame()
  
  for (rawUUID in listUuids){
    
    
    user <- paste0("uuid-", rawUUID)
    print("******")               
    print(user)
    
    raw_user_responses <- subset(responses, userId == user)
    raw_user_responses_nomultiples <- raw_user_responses[1,]
    # controlling multiple responses
    for (i in 2:nrow(raw_user_responses)) {
      isAlHere <- FALSE
      for (j in 1:nrow(raw_user_responses_nomultiples)) {
        if (raw_user_responses[i,]$stimulusId == raw_user_responses_nomultiples[j,]$stimulusId) {
          if (raw_user_responses[i,]$response == raw_user_responses_nomultiples[j,]$response) {
            # duplicated submission of the same response (the system is double-secure in the case of broken i-net connection)
            isAlHere <- TRUE
          } else {
            if (nchar(raw_user_responses[i,]$response) == nchar(raw_user_responses_nomultiples[j,]$response)) {
              users_multiple_submission <- add_row(users_multiple_submission, userId = user,
                                                   reason = paste0(raw_user_responses[i,]$stimulusId, " with the responses ", raw_user_responses[i,]$response, " and ", raw_user_responses_nomultiples[j,]$response),
                                                   details = "the latest of the submitted will be considered")
              isAlHere <- TRUE
              if (raw_user_responses[i,]$tagDate > raw_user_responses_nomultiples[j,]$tagDate) {
                raw_user_responses_nomultiples[j,] <- raw_user_responses[i,]
              }
            }
          }
        }
      }
      if (!isAlHere) {
        raw_user_responses_nomultiples <- rbind(raw_user_responses_nomultiples, raw_user_responses[i,])
      }
    }
   
    
    new_user_responses <- data.frame()
    
    for (stimulusID in unique(raw_user_responses$stimulusId)) {
      
       new_user_stimulus_response <- subset(raw_user_responses_nomultiples, stimulusId == stimulusID)
      
        # adding column representing stimulus sequence length
       if (grepl("Practice", stimulusID)) {
         length_from_id <- "Length2"
       } else {
         length_from_id <- strsplit(stimulusID, "_")[[1]][2]
       }
       
       new_user_stimulus_response$stimulusLength <- substr(length_from_id, 7, nchar(length_from_id))
      
       
       # adding column representing response length
       new_user_stimulus_response$responseLength <-  nchar(new_user_stimulus_response$response)
       
        # selecting the row with the maxinal response length (this is the final response)
       max_resp_length <-  max(new_user_stimulus_response$responseLength)
       new_user_stimulus_response <- subset(new_user_stimulus_response,responseLength == max_resp_length)
       
       #evaluating response 
       new_user_stimulus_response$isCorrect <- 0
       if (grepl("Forward", startScreenName)) {
         new_user_stimulus_response$isCorrect <- ifelse(new_user_stimulus_response$stimulusLength == 2 && new_user_stimulus_response$response=="12", 1, new_user_stimulus_response$isCorrect )
         new_user_stimulus_response$isCorrect <- ifelse(new_user_stimulus_response$stimulusLength == 3 && new_user_stimulus_response$response=="123", 1, new_user_stimulus_response$isCorrect )
         new_user_stimulus_response$isCorrect <- ifelse(new_user_stimulus_response$stimulusLength == 4 && new_user_stimulus_response$response=="1234", 1, new_user_stimulus_response$isCorrect )
         new_user_stimulus_response$isCorrect <- ifelse(new_user_stimulus_response$stimulusLength == 5 && new_user_stimulus_response$response=="12345", 1, new_user_stimulus_response$isCorrect )
         new_user_stimulus_response$isCorrect <- ifelse(new_user_stimulus_response$stimulusLength == 6 && new_user_stimulus_response$response=="123456", 1, new_user_stimulus_response$isCorrect )
         new_user_stimulus_response$isCorrect <- ifelse(new_user_stimulus_response$stimulusLength == 7 && new_user_stimulus_response$response=="1234567", 1, new_user_stimulus_response$isCorrect )
         new_user_stimulus_response$isCorrect <- ifelse(new_user_stimulus_response$stimulusLength == 8 && new_user_stimulus_response$response=="12345678", 1, new_user_stimulus_response$isCorrect )
         new_user_stimulus_response$isCorrect <- ifelse(new_user_stimulus_response$stimulusLength == 9 && new_user_stimulus_response$response=="123456789", 1, new_user_stimulus_response$isCorrect )
       }
       
       if (grepl("Backward", startScreenName)) {
         new_user_stimulus_response$isCorrect <- ifelse(new_user_stimulus_response$stimulusLength == 2 && new_user_stimulus_response$response=="21", 1, new_user_stimulus_response$isCorrect )
         new_user_stimulus_response$isCorrect <- ifelse(new_user_stimulus_response$stimulusLength == 3 && new_user_stimulus_response$response=="321", 1, new_user_stimulus_response$isCorrect )
         new_user_stimulus_response$isCorrect <- ifelse(new_user_stimulus_response$stimulusLength == 4 && new_user_stimulus_response$response=="4321", 1, new_user_stimulus_response$isCorrect )
         new_user_stimulus_response$isCorrect <- ifelse(new_user_stimulus_response$stimulusLength == 5 && new_user_stimulus_response$response=="54321", 1, new_user_stimulus_response$isCorrect )
         new_user_stimulus_response$isCorrect <- ifelse(new_user_stimulus_response$stimulusLength == 6 && new_user_stimulus_response$response=="654321", 1, new_user_stimulus_response$isCorrect )
         new_user_stimulus_response$isCorrect <- ifelse(new_user_stimulus_response$stimulusLength == 7 && new_user_stimulus_response$response=="7654321", 1, new_user_stimulus_response$isCorrect )
         new_user_stimulus_response$isCorrect <- ifelse(new_user_stimulus_response$stimulusLength == 8 && new_user_stimulus_response$response=="87654321", 1, new_user_stimulus_response$isCorrect )
         new_user_stimulus_response$isCorrect <- ifelse(new_user_stimulus_response$stimulusLength == 9 && new_user_stimulus_response$response=="987654321", 1, new_user_stimulus_response$isCorrect )
       }
       
       # check up for main rounds for a pair user-stimulus
       if (!grepl("Practice", stimulusID)) {
         checkResponseRow <- subset(checkResponses, userId==user & stimulusId == stimulusID & nchar(response) == max_resp_length)
         checkResp <- checkResponseRow[1,]$isCorrect
         if ((checkResp == TRUE && new_user_stimulus_response$isCorrect==0) || (checkResp == FALSE && new_user_stimulus_response$isCorrect==1)) {
           print("Check up fail for response, stimulus:")
           print(stimulusID)
           print("User:")
           print(user)
           print("Calculated by R-script response:")
           print(new_user_stimulus_response$isCorrect)
           print("Calculated Frinex response:")
           print(checkResp)
           stop()
         }
       }
       
       
       
       new_user_responses <- rbind(new_user_responses, new_user_stimulus_response)
       
    }
    
    n_correct <- as.numeric(length(which(new_user_responses$isCorrect == 1)))
    n_incorrect <- as.numeric(length(which(new_user_responses$isCorrect == 0)))
    
    if (startsWith(roundname, "practice")){
      nStimuli <-  n_stimuli_per_length * n_bands_practice
    } else {
      nStimuli <- n_stimuli_per_length * n_bands
    }
    if (n_correct + n_incorrect > nStimuli) {
      print("Sanity error: the sum of correctly and incorrectly answered stimuli is great than the overall possible amount of stimuli for this part")
      print(n_correct)
      print(n_incorrect)
      print(nStimuli)
      stop()
    }
    
    # max length achieved
    if (n_correct > 0) {
      correct_rows <- subset(new_user_responses, isCorrect == 1)
      max_length <- max(correct_rows$stimulusLength)
    } else {
      max_length <- 0 
    }
    
    max_length <- as.numeric(max_length)
   
    # check up for a main round for a particular user
    if (!grepl("practice", roundname)) {
      if (max_length > 0 ) {
        
        for (l in 3:max_length) {
          lengthRows <- subset(new_user_responses, new_user_responses$stimulusLength==l)
          
          if (nrow(lengthRows) != 2) {
            print("Error: The number of rows for a particulrar length is not 2.")
            print("User:")
            print(user)
            print("Length:")
            print(l)
            print("stimuli IDs:")
            print(new_user_responses$stimulusId)
            stop()
          } 
         
          if ((as.numeric(lengthRows[1,]$isCorrect) + as.numeric(lengthRows[2,]$isCorrect)) < max_n_allowed_errors) {
            print(paste0("Error: The number of correct responses for a not-completely failed length is less than ",  max_n_allowed_errors))
            print("The round should have stopped here")
            print("User:")
            print(user)
            print("Length:")
            print(l)
            print("stimuli IDs:")
            print(new_user_responses$stimulusId)
            stop()
          } 
        } 
      }
      
      # check if the experiment was stopped correctly 
      if (max_length < 9) {
        failedLength <- ifelse(max_length == 0, 3, max_length+1)
        lengthRows <- subset(new_user_responses, new_user_responses$stimulusLength == failedLength) # the failed length
        if (nrow(lengthRows) != n_stimuli_per_length) {
          print(paste0("Error: The number of rows for a particulrar length is not ", n_stimuli_per_length))
          print("User:")
          print(user)
          print("Length (failed):")
          print(failedLength)
          stop()
        } 
       
        if ((as.numeric(lengthRows[1,]$isCorrect) + as.numeric(lengthRows[2,]$isCorrect)) > 0) {
          print("Error: The number of correct responses for a particular stimulus length (for completely failed length) is not zero.")
          print("The experiment should have been continued.")
          print("User:")
          print(user)
          print("Length:")
          print(l)
          print("stimuli IDs:")
          print(new_user_responses$stimulusId)
          stop()
        }
      }
      
   
    }
   
    
    user_responses <- rbind(user_responses, new_user_responses)
    
    
    # filling in scores
    max_length <- ifelse(max_length == 0, " ", max_length)
    
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
    print(paste0(as.character(testDuration), " seconds"))
    
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


##--Get aggregates practice ----------------------------------------------##

results_practice_forward <- harvest_scores(participants_uuids$V1, responsesPracticeForward, "practiceForward", screenviews_data, "RunTrialsPracticeForward", "delayForward")
user_scores_practice_forward <- results_practice_forward$user_scores
user_responses_practice_forward <- results_practice_forward$user_responses

if (nrow(participants_uuids) != nrow(user_scores_practice_forward) ) {
  print("Practice Forward: there is a discrepancy between the number of rows in user-scores table and participant uuids, resp:")
  print(nrows(participants_uuids))
  print(nrows(user_scores_practice_forward))
}



results_practice_backward <- harvest_scores(participants_uuids$V1, responsesPracticeBackward, "practiceBackward", screenviews_data, "RunTrialsBackwardPractice", "delayBackward")
user_scores_practice_backward <- results_practice_backward$user_scores
user_responses_practice_backward <- results_practice_backward$user_responses

if (nrow(participants_uuids) != nrow(user_scores_practice_backward) ) {
  print("Practice Forward: there is a discrepancy between the number of rows in user-scores table and participant uuids, resp:")
  print(nrows(participants_uuids))
  print(nrows(user_scores_practice_backward))
}

user_practice_scores <- merge(user_scores_practice_forward, user_scores_practice_backward)
user_practice_scores$totalCorrect <- (user_practice_scores$practiceForwardCorrect + user_practice_scores$practiceBackwardCorrect)
user_practice_scores$totalIncorrect <- (user_practice_scores$practiceForwardIncorrect + user_practice_scores$practiceBackwardIncorrect)
write.csv(user_practice_scores, file=paste0(experiment_abr,".user_practice_scores.csv"))




##--Get aggregates main rounds ----------------------------------------------##

results_forward <- harvest_scores(participants_uuids$V1, responsesForward, "forward", screenviews_data, "RunTrialsForward", "instructionsBackward", responsesForwardCheck)
user_scores_forward <- results_forward$user_scores
user_responses_forward <- results_forward$user_responses

write.csv(user_responses_forward, file=paste0(experiment_abr,".forward_item_data_.csv"))

if (nrow(participants_uuids) != nrow(user_scores_forward) ) {
  print("Main Forward: there is a discrepancy between the number of rows in user-scores table and participant uuids, resp:")
  print(nrows(participants_uuids))
  print(nrows(user_scores_forward))
}



results_backward <- harvest_scores(participants_uuids$V1, responsesBackward, "backward", screenviews_data, "RunTrialsBackward", "Admin", responsesBackwardCheck)
user_scores_backward <- results_backward$user_scores
user_responses_backward <- results_backward$user_responses

write.csv(user_responses_backward, file=paste0(experiment_abr,".backward_item_data_.csv"))

if (nrow(participants_uuids) != nrow(user_scores_backward) ) {
  print("Practice Nackward: there is a discrepancy between the number of rows in user-scores table and participant uuids, resp:")
  print(nrows(participants_uuids))
  print(nrows(user_scores_backward))
}


user_scores <- merge(user_scores_forward, user_scores_backward)
user_scores$totalCorrect <- (user_scores$forwardCorrect + user_scores$backwardCorrect)
user_scores$totalIncorrect <- (user_scores$forwardIncorrect + user_scores$backwardIncorrect)
write.csv(user_scores, file=paste0(experiment_abr,".user_scores.csv"))

if (nrow(users_multiple_submission) == 0) {
  users_multiple_submission <- add_row(users_multiple_submission, userId = " ",
                                       reason = "no multiple response on the same stimulus were detected",
                                       details = " ")
}
write.csv(users_multiple_submission, file=paste0(experiment_abr,".duplicated_records.csv"))
