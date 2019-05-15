##--syntestsession----------------------------------------------------------------##

library(jsonlite)
library(httr)
library(dplyr)
library(tidyr)
library(stringr)
source("../shared/BQ4_supportFunctions.R")

## Sepcification ---------------------------------------------------------- ##

n_stimuli_pictures <- 35
n_stimuli_idioms <- 10

stimuli_origin <- read.csv("SynTest_4Olha.csv", header = TRUE, sep=";", fileEncoding="UTF-8-BOM")



##--Configuration-------------------------------------------------------------##


experiment_abr <- "syntestsession"
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

participants_uuids <- read.csv("../shared/participants.csv", header = FALSE, sep=",", fileEncoding="UTF-8-BOM")


stimulus_resp_columns <- c("userId", "screenName", "stimulusId", "response", "isCorrect", "tagDate")
stimulusresponses <- get_embedded("stimulusresponses", stimulus_resp_columns, admin_url, 1000)
stimulusresponses_data <- unique(subset(stimulusresponses[["currentData"]]))


screenviews_columns <- c("userId", "screenName", "viewDate")
screenviews <- get_embedded("screenviews", screenviews_columns, admin_url)
screenviews_data <- unique(subset(screenviews[["currentData"]]))

tagpairevents_columns <- c("userId", "tagDate", "eventTag", 
                           "tagValue1", "tagValue2")
tagpairevents <- get_embedded("tagpairevents", tagpairevents_columns, admin_url, 1000)
tagpairevents_data <- unique(tagpairevents[["currentData"]])
audio_events_syntest <- subset(tagpairevents_data, eventTag == "StimulusAudioShown" & grepl("SynTest", tagValue1) & !grepl("SynTest_Pract", tagValue1))
audio_events_idioms <- subset(tagpairevents_data, eventTag == "StimulusCodeAudioShown" & grepl("Idiom", tagValue1) & !grepl("example", tagValue1) &
                                !grepl("_A", tagValue2) & !grepl("_B", tagValue2) & !grepl("_C", tagValue2) & !grepl("_D", tagValue2))

responsesSyntest <- subset(stimulusresponses_data, screenName=="syntest" & !is.na(isCorrect))
responsesIdioms <- subset(stimulusresponses_data, screenName=="idioms" & !is.na(isCorrect))

frinexScores <- subset(tagpairevents_data, tagValue1=="gameSummary")


stimuli_origin_syntest <- subset(stimuli_origin, substr(Item_nr, 1, 7)=="SynTest" & Item_nr != "SynTest_Pract")
stimuli_origin_idioms <- subset(stimuli_origin, substr(Item_nr, 1, 5)=="Idiom")

## help function 

harvest_scores <- function(listUuids, responses, roundname, screenviews_info, startScreenName, endScreenName, nStimuli, frinexScores, stimuliOrigin, audioEvents) {
  
  user_scores <- data.frame()
  user_responses <- data.frame()
  users_multiple_submission <- data.frame(userId=character(), reason=character(), details = character())
  users_suspicious_audio <- data.frame(userId=character(), reason=character(), details = character())
  
  
  for (rawUUID in listUuids){
    
    
    user <- paste0("uuid-", rawUUID)
    print(user)
    
    raw_user_responses <- subset(responses, userId == user)
    raw_user_responses <- raw_user_responses[order(raw_user_responses$tagDate),]
    
    # filtering multiple answers
    new_user_responses <- raw_user_responses[1,]
    for (i in 2:nrow(raw_user_responses)) {
      isAlHere <- FALSE
      for (j in 1:nrow(new_user_responses)) {
        if (raw_user_responses[i,]$stimulusId == new_user_responses[j,]$stimulusId) {
          if (raw_user_responses[i,]$response == new_user_responses[j,]$response) {
            # duplicated submission of the same response (the system is double-secure in the case of broken i-net connection)
            isAlHere <- TRUE
          } else {
            users_multiple_submission <- add_row(users_multiple_submission, userId = user,
                                                   reason = paste0(raw_user_responses[i,]$stimulusId, " with the responses ", raw_user_responses[i,]$response, " and ",  new_user_responses[j,]$response),
                                                   details = "the latest of the submitted will be considered")
              isAlHere <- TRUE
              if (raw_user_responses[i,]$tagDate > new_user_responses[j,]$tagDate) {
                new_user_responses[j,] <- raw_user_responses[i,]
            }
          }
        }
      }
      if (!isAlHere) {
        new_user_responses <- rbind(new_user_responses, raw_user_responses[i,])
      }
    }
    
    new_user_responses <- new_user_responses[order(new_user_responses$tagDate),]
    
    # sanity check on amount of responses
    if (nrow(new_user_responses) != nrow(stimuliOrigin)) {
      print("Sanity error: the amount of answers is not equal to the amount of stimuli:")
      print(nrow(new_user_responses))
      print(nrow(stimuliOrigin))
      stop()
    }
    
    # checking if the sequence is correct and if the responses evaluated correctly
    for (i in 1:nrow(new_user_responses)) {
      origin_row <- subset(stimuliOrigin, Item_nr == new_user_responses[i,]$stimulusId)[1,]
      if (stimuliOrigin[i,]$Item_nr != new_user_responses[i,]$stimulusId){
        print("Sanity error: stimuli sequense is broken, there is stimulus")
        print(new_user_responses[i,]$stimulusId)
        print("But there must be a stimulus")
        print(stimuliOrigin[i,]$Item_nr)
        stop()
      }
      if (stimuliOrigin[i,]$Correct_picture == new_user_responses[i,]$response){
        if (!new_user_responses[i,]$isCorrect) {
          print("Sanity error: frinex stimuli evaluation error, the following stimulus was evaluated as incorrect whereas it is correct")
          print(new_user_responses[i,]$stimulusId)
          print(new_user_responses[i,]$response)
          stop()
        }
      } else {
        if (new_user_responses[i,]$isCorrect) {
          print("Sanity error: frinex stimuli evaluation error, the following stimulus was evaluated as correct whereas it is incorrect")
          print(new_user_responses[i,]$stimulusId)
          print(new_user_responses[i,]$response)
          stop()
        }
      }
    }
    
    
    new_user_responses$isCorrect <- ifelse(new_user_responses$isCorrect == TRUE, 1, 0)
    
    user_responses <- rbind(user_responses, new_user_responses)
    
    # checking audio events
    for (stimulusID in stimuliOrigin$Item_nr ) {
      audio_rows <- subset(audioEvents, userId == user & tagValue1 == stimulusID) 
      if (nrow(audio_rows) == 0) {
        users_suspicious_audio <- add_row(users_suspicious_audio, userId = user,
                                        reason = paste0("the audio for the stimulus ", stimulusID, " was not presented."),
                                        details = " ")
      }
      if (nrow(audio_rows) >1) {
        users_suspicious_audio <- add_row(users_suspicious_audio, userId = user,
                                             reason = paste0("the audio for the stimulus ", stimulusID, " was shown more than once."),
                                             details = paste0(nrow(audio_rows), " times"))
      }
    }
    
    
    # scoring
    n_correct <- length(which(new_user_responses$isCorrect == 1))
    n_incorrect <- length(which(new_user_responses$isCorrect == 0))
    
  
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
    print(paste0(as.character(testDuration), " sec"))
    
    
   
    # sanity check on the overall amount of stimuli 
    if (as.numeric(n_correct) + as.numeric(n_incorrect) != nStimuli ) {
      print("Sanity error: the number of evaluated stimuli is not the same as the amount of the actual test stimuli")
      print(" # of stimuli:")
      print(nStimuli)
      print("Evaluated (correct + incorrect):")
      print(as.numeric(n_correct) + as.numeric(n_incorrect))
      print("Where correct:")
      print(n_correct)
      print("and incorrect:")
      print(n_incorrect)
      stop()
    }
    
    # sanity check
    userFrinexScores <- subset(frinexScores, userId==user)
    userFrinexSyntestCorrect <- subset(userFrinexScores, eventTag == "totalScoreSyntest")
    if (nrow(userFrinexSyntestCorrect) != 1) {
      print("Weird amount of scores for tag totalScoreSyntest")
      print(nrow(userFrineSyntestxCorrect))
      stop()
    }
    userFrinexSyntestIncorrect <- subset(userFrinexScores, eventTag == "totalErrorsSyntest")
    if (nrow(userFrinexSyntestIncorrect) != 1) {
      print("Weird amount of scores for tag totalErrorsSyntest")
      print(nrow(userFrinexSyntestIncorrect))
      stop()
    }
    
    if (roundname == "syntest") {
      userFrinexCorrect <- userFrinexSyntestCorrect[1,]$tagValue2
      userFrinexIncorrect <- userFrinexSyntestIncorrect[1,]$tagValue2
    }
    
    if (roundname == "idioms") {
      userFrinexTotalCorrect <- subset(userFrinexScores, eventTag == "totalScoreSyntestAndIdioms")
      if (nrow(userFrinexTotalCorrect) != 1) {
        print("Weird amount of scores for tag totalScoreSyntestAndIdioms")
        print(nrow(userFrinexTotalCorrect))
        stop()
      }
      userFrinexTotalIncorrect <- subset(userFrinexScores, eventTag == "totalErrorsSyntestAndIdioms")
      if (nrow(userFrinexTotalIncorrect) != 1) {
        print("Weird amount of scores for tag totalErrorSyntestAndIdioms")
        print(nrow(userTotalFrinexIncorrect))
        stop()
      }
      
      userFrinexCorrect <- as.numeric(userFrinexTotalCorrect$tagValue2) -  as.numeric(userFrinexSyntestCorrect[1,]$tagValue2)
      userFrinexIncorrect <- as.numeric(userFrinexTotalIncorrect$tagValue2) - as.numeric(userFrinexSyntestIncorrect[1,]$tagValue2)
    }
    
   
    if (as.numeric(n_correct) != as.numeric(userFrinexCorrect)) {
      alert <- 1
    } else {
      alert <- 0
    }
    if (as.numeric(n_incorrect) != as.numeric(userFrinexIncorrect)) {
      alert <- 1
    } 
   
    
    
    new_user_scores <- data.frame(user, n_correct, n_incorrect, testDuration, userFrinexCorrect,  userFrinexIncorrect, alert)
    names(new_user_scores) <- c("userId", 
                                paste0(roundname, "Correct"),
                                paste0(roundname, "Incorrect"),
                                paste0(roundname, "Duration (sec)"),
                                paste0(roundname, "FrinexCorrect"),
                                paste0(roundname, "FrinexIncorrect"),
                                paste0(roundname, "Alert"))
    
    
    user_scores<- rbind(user_scores, new_user_scores)
    
  }
  
  return(list(user_responses=user_responses, user_scores=user_scores, mutiple_responses=users_multiple_submission, irregular_audio=users_suspicious_audio))
 
  
}


##--Get aggregates syntest ----------------------------------------------##


results_syntest <- harvest_scores(participants_uuids$V1, responsesSyntest, "syntest", screenviews_data, "syntest", "instructionsIdioms", n_stimuli_pictures, frinexScores, stimuli_origin_syntest, audio_events_syntest)

user_scores_syntest <- results_syntest$user_scores
user_responses_syntest <- results_syntest$user_responses
write.csv(user_responses_syntest, file=paste0(experiment_abr,".syntest_item_data.csv"))

if (nrow(participants_uuids) != nrow(user_scores_syntest) ) {
  print("Syntest: there is a discrepancy between the number of rows in user-scores table and participant uuids, resp:")
  print(nrows(participants_uuids))
  print(nrows(user_scores_syntest))
}

users_multiple_submission <- results_syntest$mutiple_responses
if (nrow(users_multiple_submission)==0) {
  users_multiple_submission <- add_row(users_multiple_submission, userId = " ", reason = "no multiple responses on the samestimuli were detected", details =" ")
}
write.csv(users_multiple_submission, file=paste0(experiment_abr,".syntest_multiple_responses.csv"))

users_suspicious_audio <- results_syntest$irregular_audio
if (nrow(users_suspicious_audio)==0) {
  users_suspicious_audio <- add_row(users_suspicious_audio, userId = " ", reason = "no irregularities were detected", details =" ")
}
write.csv(users_suspicious_audio, file=paste0(experiment_abr,".syntest_irregular_audios.csv"))

results_idioms <- harvest_scores(participants_uuids$V1, responsesIdioms, "idioms", screenviews_data, "idioms", "Admin", n_stimuli_idioms, frinexScores, stimuli_origin_idioms, audio_events_idioms)
user_scores_idioms <- results_idioms$user_scores
user_responses_idioms <- results_idioms$user_responses
write.csv(user_responses_idioms, file=paste0(experiment_abr,".idioms_item_data.csv"))

if (nrow(participants_uuids) != nrow(user_scores_idioms) ) {
  print("Idioms: there is a discrepancy between the number of rows in user-scores table and participant uuids, resp:")
  print(nrows(participants_uuids))
  print(nrows(user_idioms))
}

user_scores <- merge(user_scores_syntest, user_scores_idioms)
user_scores$totalCorrect <- (user_scores$syntestCorrect + user_scores$idiomsCorrect)
user_scores$totalIncorrect <- (user_scores$syntestIncorrect + user_scores$idiomsIncorrect)

write.csv(user_scores, file=paste0(experiment_abr,".user_scores.csv"))

users_multiple_submission <- results_idioms$mutiple_responses
if (nrow(users_multiple_submission)==0) {
  users_multiple_submission <- add_row(users_multiple_submission, userId = " ", reason = "no multiple responses on the samestimuli were detected", details =" ")
}
write.csv(users_multiple_submission, file=paste0(experiment_abr,".idioms_multiple_responses.csv"))

users_suspicious_audio <- results_idioms$irregular_audio
if (nrow(users_suspicious_audio)==0) {
  users_suspicious_audio <- add_row(users_suspicious_audio, userId = " ", reason = "no irregularities were detected", details =" ")
}
write.csv(users_suspicious_audio, file=paste0(experiment_abr,".idioms_irregular_audios.csv"))



