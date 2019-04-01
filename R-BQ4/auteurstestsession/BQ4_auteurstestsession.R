##--auteurstestsession ---------------------------------------------------------------##

library(jsonlite)
library(httr)
library(dplyr)
library(tidyr)
library(stringr)
source("../shared/BQ4_supportFunctions.R")


##--Configuration-------------------------------------------------------------##

experiment_abr <- "auteurstestsession"
server_url <- "https://frinexproduction.mpi.nl"
admin_url <- paste(server_url, "/", experiment_abr, "-admin", sep = "")

# Use this to skip users if users from earlier application versions with 
# incompatible data definitions are still in the database.
users_to_skip <- 0


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


tagpairevents_columns <- c("userId", "screenName", "eventTag", 
                           "tagValue1", "tagValue2")
tagpairevents <- get_embedded("tagpairevents", tagpairevents_columns, admin_url)

tagevents_columns <- c("userId", "screenName")
tagevents <- get_embedded("tagevents", tagevents_columns, admin_url)

screenviews_columns <- c("userId", "screenName", "submitDate")
screenviews <- get_embedded("screenviews", screenviews_columns, admin_url)
screenviews_data <- unique(screenviews[["currentData"]])

timestamps_columns <- c("userId", "screenName")
timestamps <- get_embedded("timeStamps", timestamps_columns, admin_url)

participants_data <- unique(subset(participants[["currentData"]], 
                                   staleCopy == FALSE))
tagpairevents_data <- unique(tagpairevents[["currentData"]])

tagevents_data <- unique(tagevents[["currentData"]])

participant_events <- left_join(tagpairevents[["currentData"]], 
                                participants_data, 
                                by = "userId")


user_scores <- data.frame()
user_responses <- data.frame()

for (rawUUID in participants_uuids$V1)  {
 
    user <- paste0("uuid-", rawUUID)
    print(user)
    print("Num events for user:")
    
    events <- subset(tagpairevents_data, 
                     userId == user & eventTag != "summary" & eventTag != "DataSubmission")
    
    print(nrow(events))
    
    if (nrow(events) == 0){
      next
    }
    
    submitDate <- subset(participants_data, userId == user)$submitDate
    userName <- subset(participants_data, userId == user)$workerId
    
    isAuthor <- 0
    user_response <- ""
    
    response_values <- c("true", "false")
    correctness_values <- c("correct", "incorrect")
    
    new_user_response <- subset(events, events$tagValue2 %in% response_values)
    #stimuli$isAuthor <- strsplit(stimuli$tagValue1, "_")[[1]][4]
    new_user_response$isAuthor <- as.character(lapply(strsplit(new_user_response$tagValue1,"_"), 
                               function(x) x[length(x)]))
    new_user_response$Stimulus <- lapply(strsplit(new_user_response$tagValue1,"_"), 
                               function(x) x[-length(x)])
    new_user_response$StimulusID <- as.character(lapply(strsplit(new_user_response$tagValue1,"_"), 
                                 function(x) x[1]))
    new_user_response$Author <- lapply(new_user_response$Stimulus, 
                             function(x) x[-1])
    new_user_response$Author <- as.character(lapply(new_user_response$Author, 
                             function(x) paste(x, collapse = " ")))
    new_user_response$userName <- userName
    new_user_response$submitDate <- submitDate
    
    new_user_response <- subset(new_user_response, select=c(userId, userName,
                                                            submitDate, 
                                                            Author, StimulusID, 
                                                            isAuthor, tagValue2))
    names(new_user_response)[7] <- 'userResponse'
    
    # Get values for user correctness
    
    user_correctness <- subset(events, events$tagValue2 %in% correctness_values)
    
    user_correctness$StimulusID <- as.character(lapply(strsplit(user_correctness$tagValue1,"_"), 
                                           function(x) x[1]))
    
    user_correctness <- subset(user_correctness, select=c(userId, StimulusID, 
                                                          tagValue2))
    
    names(user_correctness)[names(user_correctness) == 'tagValue2'] <- 'isUserCorrect'
    
    new_user_response <- left_join(new_user_response, user_correctness, 
                                   by = c("userId", "StimulusID"))
    
    # Change values for IsUserCorrect and UserAnswer from true/false to 1/0
    new_user_response$userResponse <- ifelse(new_user_response$userResponse == "true", 1, 0)
    new_user_response$isUserCorrect <- ifelse(new_user_response$isUserCorrect == "correct", 1, 0)
    new_user_response$isAuthor <- ifelse(new_user_response$isAuthor == "author", 1, 0)
    
    user_responses <- rbind(user_responses, new_user_response)
    
    # Scoring explanation (coding is counterintuitive)
    #
    # The users task is to select stimuli that are authors
    #
    # A tagValue1 that ends in "_author" indicates that stimulus was an author.
    # A tagValue1 that ends in "_not" indicates that stimulus was not an author.
    # tagValue2 indicates whether the user was correct or incorrect
    #
    # A False Positives is therefore a case where the user thought that the 
    # stimulus was an author, but the user was incorrect 
    # (tagValue2 == "incorrect") because the stimulus was not an author 
    # (tagValue1 ends in "_not")
    
    TP <- length(which(str_sub(events$tagValue1, -7) == "_author" & 
                      events$tagValue2 == "correct"))
    FP <- length(which(str_sub(events$tagValue1, -4) == "_not" & 
                         events$tagValue2 == "incorrect"))
    TN <- length(which(str_sub(events$tagValue1, -4) == "_not" & 
                         events$tagValue2 == "correct"))
    FN <- length(which(str_sub(events$tagValue1, -7) == "_author" & 
                         events$tagValue2 == "incorrect"))
    
    #print("True Positives:")
    #print(TP)
    #print("False Positives:")
    #print(FP)
    #print("True Negatives:")
    #print(TN)
    #print("False Negatives:")
    #print(FN)
    #print("Total:")
    #print(TP + FP + TN + FN)

    
    # Get test duration for participant from screenviews
    screenviews_1user <- subset(screenviews_data, 
                                  userId == user)
    
    test_duration <- get_test_duration(screenviews_1user)
    
    startTime <- test_duration[["startTime"]]
    endTime <- test_duration[["endTime"]]
    testDuration <- test_duration[["testDuration"]]
    
    
    print("startTime")
    print(startTime)
    print("endTime")
    print(endTime)
    print("test duration")
    print(paste0(as.character(testDuration), " minutes"))
    
    new_user_scores <- data.frame(user, userName, TP, FP, TN, FN, testDuration)
    names(new_user_scores) <- c("userId", "userName",
                            "Hits (True Positives)",
                            "False Alarm (False Positives)",
                            "Correct Rejection (True Negative)",
                            "Missing (False Negative)",
                            "Test Duration (mins)"
                            ) 
    user_scores <- rbind(user_scores, new_user_scores)
 
} 

# Write scores to file
write.csv(user_scores, file=paste0(experiment_abr,".user_scores.csv"))

#user_responses <- user_responses[order(user_responses[,3], user_responses[,5]), ]
write.csv(user_responses, file=paste0(experiment_abr,".item_data.csv"))
