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

participants_uuids <- read.csv("../../participants_1.txt", header = FALSE, sep =';',  fileEncoding="UTF-8-BOM")
tagpairevents_columns <- c("userId", "screenName", "eventTag", "tagDate",
                           "tagValue1", "tagValue2")
screenviews_columns <- c("userId", "screenName", "submitDate", "viewDate")
stimulusresponses_columns <- c("userId", "screenName", "stimulusId", "response", "isCorrect", "tagDate", "eventMs")


stimuli_origin_syntest <- subset(stimuli_origin, substr(Item_nr, 1, 7)=="SynTest" & Item_nr != "SynTest_Pract")
stimuli_origin_idioms <- subset(stimuli_origin, substr(Item_nr, 1, 5)=="Idiom")

## help function 

harvest_scores <- function(listUuids, roundname, startScreenName, endScreenName, nStimuli, stimuliOrigin) {
  
  user_scores <- data.frame()
  user_responses <- data.frame()
  users_irregularities <- data.frame(userId=character(), message=character())
  
 
  
  for (rawUUID in listUuids){
    
    
    user <- paste0("uuid-", rawUUID)
    print(user)
    
    stimulusresponses_user_raw <- findByUserIdOrderByTagDateAsc(admin_url, user, "stimulusresponses", "findByUserIdOrderByTagDateAsc", stimulusresponses_columns)
    
    if (is.null(stimulusresponses_user_raw)) {
      message <- "No stimulusresponses record for this user"
      users_irregularities <-  add_row(users_irregularities, userId=user, message = message)
      next
    }
    
    events <- findByUserIdOrderByTagDateAsc(admin_url, user, "tagpairevents", "findByUserIdOrderByTagDateAsc", tagpairevents_columns)
    userFrinexScores <- subset(events, tagValue1=="gameSummary")
    
    if (roundname == "syntest") {
      stimulusresponses_user_raw <- subset(stimulusresponses_user_raw, screenName=="syntest" & !is.na(isCorrect))
      events <- subset(events, eventTag == "StimulusAudioShown" & grepl("SynTest", tagValue1) & !grepl("SynTest_Pract", tagValue1))
      
    } 
    
    if (roundname == "idioms") {
      stimulusresponses_user_raw<- subset(stimulusresponses_user_raw, screenName=="idioms" & !is.na(isCorrect))
      events <- subset(events, eventTag == "StimulusCodeAudioShown" & grepl("Idiom", tagValue1) & !grepl("example", tagValue1) &   !grepl("_A", tagValue2) & !grepl("_B", tagValue2) & !grepl("_C", tagValue2) & !grepl("_D", tagValue2))
    }
    
   
    # filtering multiple answers
    new_user_responses <- stimulusresponses_user_raw[1,]
    for (i in 2:nrow(stimulusresponses_user_raw)) {
      isAlHere <- FALSE
      for (j in 1:nrow(new_user_responses)) {
        if (stimulusresponses_user_raw[i,]$stimulusId == new_user_responses[j,]$stimulusId) {
          if (stimulusresponses_user_raw[i,]$response == new_user_responses[j,]$response) {
            # duplicated submission of the same response (the system is double-secure in the case of broken i-net connection)
            isAlHere <- TRUE
          } else {
            message <- paste(stimulusresponses_user_raw[i,]$stimulusId, "with the responses", 
                             stimulusresponses_user_raw[i,]$response, "and",  
                             new_user_responses[j,]$response, 
                             "the latest of the submitted will be considered", sep = " ")
             users_irregularities <- add_row(users_irregularities,userId = user, message = message)
           
              isAlHere <- TRUE
              if (stimulusresponses_user_raw[i,]$tagDate > new_user_responses[j,]$tagDate) {
                new_user_responses[j,] <- stimulusresponses_user_raw[i,]
            }
          }
        }
      }
      if (!isAlHere) {
        new_user_responses <- rbind(new_user_responses, stimulusresponses_user_raw[i,])
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
      audio_rows <- subset(events, tagValue1 == stimulusID) 
      if (nrow(audio_rows) == 0) {
        message <-paste0("the audio for the stimulus ", stimulusID, " was not presented.")
        users_irregularities <- add_row(users_irregularities, userId = user, message = message)
      }
      if (nrow(audio_rows) >1) {
        message <-paste("The audio for the stimulus", stimulusID, "was shown more than once.", nrow(audio_rows), "times", sep =" ")
        users_irregularities <- add_row(users_irregularities, userId = user, message = message)
      }
    }
    
    
    # scoring
    n_correct <- length(which(new_user_responses$isCorrect == 1))
    n_incorrect <- length(which(new_user_responses$isCorrect == 0))
    
  
    # Get test duration for participant from screenviews
    screenviews_1user <- findByUserIdOrderByTagDateAsc(admin_url, user, "screenviews", "findByUserIdOrderByViewDateAsc", screenviews_columns)
    
    test_duration <- get_duration_min_via_view_date(screenviews_1user, startScreenName, endScreenName)
    
    startTime <- test_duration[["start_time_s"]]
    endTime <- test_duration[["end_time_s"]]
    testDurationSec <- endTime - startTime
    testDuration <- testDurationSec
    
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
    
    
    userFrinexSyntestCorrect <- subset(userFrinexScores, eventTag == "totalScoreSyntest")
    if (nrow(userFrinexSyntestCorrect) != 1) {
      print("Weird amount of scores for tag totalScoreSyntest")
      print(nrow(userFrinexSyntestxCorrect))
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
  
  return(list(user_responses=user_responses, user_scores=user_scores, irregularities=users_irregularities))
 
  
}


##--Get aggregates syntest ----------------------------------------------##


results_syntest <- harvest_scores(participants_uuids$V1,"syntest", "syntest", "instructionsIdioms", n_stimuli_pictures, stimuli_origin_syntest)
scores_syntest <- results_syntest$user_scores
responses_syntest <- results_syntest$user_responses
irregularities_syntest <- results_syntest$irregularities
write.csv(responses_syntest, file=paste0(experiment_abr,".syntest_item_data.csv"))

if (nrow(participants_uuids) != nrow(scores_syntest) ) {
  message <- paste("Syntest: there is a discrepancy between the number of rows in user-scores table and participant uuids, resp:", 
  nrows(participants_uuids), 
  nrows(scores_syntest), sep = " ")
  irregularities_syntest < - add_row(irregularities_syntest, userId="summary", message = message)
}


write.csv(irregularities_syntest, file=paste0(experiment_abr,".syntest_irregularities.csv"))

results_idioms <- harvest_scores(participants_uuids$V1, "idioms", "idioms", "Admin", n_stimuli_idioms, stimuli_origin_idioms)
scores_idioms <- results_idioms$user_scores
responses_idioms <- results_idioms$user_responses
irregularities_idioms <- results_idioms$irregularities

write.csv(responses_idioms, file=paste0(experiment_abr,".idioms_item_data.csv"))

if (nrow(participants_uuids) != nrow(scores_idioms) ) {
  message <- paste("Idioms: there is a discrepancy between the number of rows in user-scores table and participant uuids, resp:", 
                   nrows(participants_uuids), 
                   nrows(user_idioms), sep = " ")
  irregularities_idioms < - add_row(irregularities_idiomst, userId="summary", message = message)
}

write.csv(irregularities_idioms, file=paste0(experiment_abr,".idioms_irregularities.csv"))


scores <- merge(scores_syntest, scores_idioms)
scores$totalCorrect <- (scores$syntestCorrect + scores$idiomsCorrect)
scores$totalIncorrect <- (scores$syntestIncorrect + scores$idiomsIncorrect)


write.csv(scores, file=paste0(experiment_abr,".scores.csv"))



