##--Prescriptive Grammar------------------------------------------------------##

library(jsonlite)
library(httr)
library(dplyr)
library(tidyr)
source("../shared/BQ4_supportFunctions.R")

##--Configuration-------------------------------------------------------------##

experiment_abr <- "prescriptivegrammarsession"
server_url <- "http://frinexproduction.mpi.nl"
admin_url <- paste(server_url, "/", experiment_abr, "-admin", sep = "")#auth_file <- "../login.prescriptivegrammar_mante.csv"

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

###--Get required data from an experiment------------------------------------###

participants_uuids <- read.csv("../shared/participants.csv", header = FALSE)

participants_columns <- c("userId", "staleCopy", "submitDate")
participants <- get_embedded("participants", participants_columns, admin_url)
participants_data <- unique(subset(participants[["currentData"]], 
                                   staleCopy == FALSE))

tagpairevents_columns <- c("userId", "eventTag", 
                           "tagValue1", "tagValue2")
tagpairevents <- get_embedded("tagpairevents", tagpairevents_columns, admin_url)
tagpairevents_data <- unique(tagpairevents[["currentData"]])

screenviews_columns <- c("userId", "screenName", "submitDate")
screenviews <- get_embedded("screenviews", screenviews_columns, admin_url)
screenviews_data <- unique(screenviews[["currentData"]])

participant_events <- left_join(tagpairevents[["currentData"]], 
                                participants_data, 
                                by = "userId")



###--Process responses and aggregate scores----------------------------------###

user_scores <- data.frame()
user_responses <- data.frame()



eventsMainTest = subset(tagpairevents_data, eventTag != "summary" & eventTag !="DataSubmission" & !grepl("Practice", tagValue1) & !(tagValue1=="description"))

stimulus_resp_columns <- c("userId", "screenName", "stimulusId", "response", "isCorrect", "tagDate")
stimulusresponses <- get_embedded("stimulusresponses", stimulus_resp_columns, admin_url, 1000)
stimulusresponses_data <- unique(subset(stimulusresponses[["currentData"]], !grepl("Practice", stimulusId))) 



for (rawUUID in participants_uuids$V1)  {
  
  user <- paste0("uuid-", rawUUID)
  print(user)
    print("Num events for user:")
    
    events <- subset(eventsMainTest, userId == user)
    
    print(nrow(events))
    
    stimulusresponses_user <- subset(stimulusresponses_data, userId==user) 
    
    
    # Process subjects with complete data
    if (nrow(events) != 119){
      
      new_user_response <- events
      
      # add isUserCorrect column
      new_user_response <- new_user_response %>%
        mutate(eventTag = ifelse(eventTag %in% c("Correct", "Incorrect"), 
                                 "isUserCorrect", eventTag))
      
      # turn description into a unique tag
      new_user_response <- new_user_response %>%
        mutate(tagValue1 = ifelse(eventTag %in% c("stimulusInfo"), 
                                  tagValue2, tagValue1))
      
      new_user_response <- spread(new_user_response, eventTag, tagValue2)
      
      # Extract stimulus and isStimulusCorrect from StimulusAudioShown column
      # Example: 
      # stimulus = "mij2" in "./static/stimuli/mij2_1")
      # isStimulusCorrect ("1" from "./static/stimuli/mij2_1")
      new_user_response$Stimulus <- as.character(
        lapply(strsplit(new_user_response$StimulusAudioShown,"/"), 
               function(x) x[length(x)])
        )
      new_user_response$isStimulusCorrect <- lapply(
        strsplit(new_user_response$Stimulus,"_"), 
        function(x) x[length(x)]
        )
      new_user_response$Stimulus <- as.character(
        lapply(strsplit(new_user_response$Stimulus,"_"), 
        function(x) x[-length(x)])
        )
      
      # Remove StimulusAudioShown column
      new_user_response <- new_user_response[,-4]
      
      names(new_user_response)[names(new_user_response) == 'StimulusButton'] <- 'userResponse'
      
      for (stimulusID in stimulusresponses_user$stimulusId) {
        eventsStimulus <- subset(new_user_response, tagValue1 == stimulusID) 
        if (nrow(eventsStimulus) != 1) {
          print(stimulusID)
          print("Sanity error: stimulus has more than 1 response (or no responses at all)")
          print(nrow(eventsStimulus))
          stop()
        }
        responsesStimulus <- subset(stimulusresponses_user, stimulusId == stimulusID )
        if (eventsStimulus[1,]$userResponse != responsesStimulus[1,]$response) {
          print(stimulusID)
          print("Sanity error: user reaction reports differ in tagpair events and in stimulus response API request:")
          print(eventsStimulus[1,]$userResponse)
          print(responsesStimulus[1,]$response)
          stop()
        }
      }
      
      user_responses <- rbind(user_responses, new_user_response)    
    } else {
      print("Sanity check error: the amount of rows for this uesr does not coinside with the amount of stimuli (119):")
      print(nrow(events))
      stop()
    }
    
    # Scoring explanation (coding is different from auteurs- and spellingtest)
    #
    # The users task is to select whether stimuli are correct or incorrect
    # Scoring assumes Correct sentences to be positives and Incorrect sentences
    # to be negatives
    #
    # An eventTag of "Correct" indicates that stimulus was a correct 
    # sentence
    # An eventTag of "Incorrect" indicates that stimulus was not a correct 
    # sentence
    # tagValue2 indicates whether the user was correct or incorrect
    #
    # A False Positives is therefore a case where the user thought that the 
    # stimulus was a correct sentence (eventTag == "Correct"), but the user was 
    # incorrect (tagValue2 == "incorrect")
    
    TP <- length(which(events$eventTag == "Correct" & 
                         events$tagValue2 == "correct"))
    FP <- length(which(events$eventTag == "Correct" & 
                         events$tagValue2 == "incorrect"))
    TN <- length(which(events$eventTag == "Incorrect" & 
                         events$tagValue2 == "correct"))
    FN <- length(which(events$eventTag == "Incorrect" & 
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
    
    # sanity checks
    TPcheck <- length(which(stimulusresponses_user$response == "Correct" & 
                              stimulusresponses_user$isCorrect == TRUE))
    if (TP != TPcheck) {
      print("Sanity error: # of true positives obtained by tag-pair events and by stimulus-responses requests differ:")
      print(TP)
      print(TPcheck)
      stop()
    }
    FPcheck <- length(which(stimulusresponses_user$response == "Correct" & 
                              stimulusresponses_user$isCorrect == FALSE))
    if (FP != FPcheck) {
      print("Sanity error: # of false positives obtained by tag-pair events and by stimulus-responses requests differ:")
      print(FP)
      print(FPcheck)
      stop()
    }
    TNcheck <- length(which(stimulusresponses_user$response == "Incorrect" & 
                              stimulusresponses_user$isCorrect == TRUE))
    if (TN != TNcheck) {
      print("Sanity error: # of true negatives obtained by tag-pair events and by stimulus-responses requests differ:")
      print(TN)
      print(TNcheck)
      stop()
    }
    FNcheck <- length(which(stimulusresponses_user$response == "Incorrect" & 
                              stimulusresponses_user$isCorrect == FALSE))
    if (FN != FNcheck) {
      print("Sanity error: # of false negatives obtained by tag-pair events and by stimulus-responses requests differ:")
      print(FN)
      print(FNcheck)
      stop()
    }
    
    summary_user <- subset(tagpairevents_data, userId == user & grepl("score:", tagValue2))
    score <- lapply(strsplit(summary_user$tagValue2,":"), 
                    function(x) trimws(x[2]))
    if (score != as.numeric(TP) + as.numeric(TN) ) {
      print("Sanity error: # frinex-obtainesscore  differfrom the sumof true-positive and true-negative answers:")
      print(score)
      print(as.numeric(TP) + as.numeric(TN))
      stop()
    }
    
    # Get test duration for participant from screenviews
    screenviews_1user <- subset(screenviews_data, 
                                userId == user)
    
    test_duration <- get_test_duration(screenviews_1user, 
                                       "stimuliScreenGrammar", 20)
    
    startTime <- test_duration[["startTime"]]
    endTime <- test_duration[["endTime"]]
    testDuration <- test_duration[["testDuration"]]
    
    print("startTime")
    print(startTime)
    print("endTime")
    print(endTime)
    print("test duration")
    print(paste0(as.character(testDuration), " minutes"))
    
    new_user_scores <- data.frame(user, TP, FP, TN, FN, testDuration)
    names(new_user_scores) <- c("userId", 
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

user_responses[] <- lapply(user_responses, as.character)

#  Change values for IsUserCorrect and StimulusButton from true/false to 1/0
user_responses$userResponse <- ifelse(user_responses$userResponse == "Correct", 1, 0)
user_responses$isUserCorrect <- ifelse(user_responses$isUserCorrect == "correct", 1, 0)

# Re-sort by tagDate
user_responses <- user_responses[order(user_responses[,3]), ]
write.csv(user_responses, file=paste0(experiment_abr,".item_data.csv"))

