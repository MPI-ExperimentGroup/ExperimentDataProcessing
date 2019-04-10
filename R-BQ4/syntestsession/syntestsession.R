##--syntestsession----------------------------------------------------------------##

library(jsonlite)
library(httr)
library(dplyr)
library(tidyr)
library(stringr)
source("../shared/BQ4_supportFunctions.R")

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


stimulus_resp_columns <- c("userId", "screenName", "stimulusId", "response", "isCorrect", "tagDate")
stimulusresponses <- get_embedded("stimulusresponses", stimulus_resp_columns, admin_url, 1000)
stimulusresponses_data <- unique(subset(stimulusresponses[["currentData"]]))

participants_uuids <- read.csv("../shared/participants.csv", header = FALSE)

screenviews_columns <- c("userId", "screenName", "viewDate")
screenviews <- get_embedded("screenviews", screenviews_columns, admin_url)
screenviews_data <- unique(subset(screenviews[["currentData"]]))

tagpairevents_columns <- c("userId", "tagDate", "eventTag", 
                           "tagValue1", "tagValue2")
tagpairevents <- get_embedded("tagpairevents", tagpairevents_columns, admin_url, 1000)
tagpairevents_data <- unique(tagpairevents[["currentData"]])


responsesSyntest <- subset(stimulusresponses_data, screenName=="syntest" & !is.na(isCorrect))
responsesIdioms <- subset(stimulusresponses_data, screenName=="idioms" & !is.na(isCorrect))

frinexScores <- subset(tagpairevents_data, tagValue1=="gameSummary")

## help function 

harvest_scores <- function(listUuids, responses, roundname, screenviews_info, startScreenName, endScreenName, nStimuli, frinexScores) {
  
  user_scores <- data.frame()
  user_responses <- data.frame()
  
  for (rawUUID in listUuids){
    
    
    user <- paste0("uuid-", rawUUID)
    print(user)
    print("Num responses for user:")
    
    raw_user_responses <- subset(responses, userId == user)
   
    print(nrow(raw_user_responses))
    
    new_user_responses <- raw_user_responses
    new_user_responses <- new_user_responses[order(new_user_responses[,6]), ]
    new_user_responses$isCorrect <- ifelse(new_user_responses$isCorrect == TRUE, 1, 0)
    
  
  
    
    user_responses <- rbind(user_responses, new_user_responses)
    
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
  
  return(list(user_responses=user_responses, user_scores=user_scores))
 
  
}


##--Get aggregates syntest ----------------------------------------------##


results_syntest <- harvest_scores(participants_uuids$V1, responsesSyntest, "syntest", screenviews_data, "syntest", "instructionsIdioms", 35, frinexScores)
user_scores_syntest <- results_syntest$user_scores
user_responses_syntest <- results_syntest$user_responses

if (nrow(participants_uuids) != nrow(user_scores_syntest) ) {
  print("Syntest: there is a discrepancy between the number of rows in user-scores table and participant uuids, resp:")
  print(nrows(participants_uuids))
  print(nrows(user_scores_syntest))
}

results_idioms <- harvest_scores(participants_uuids$V1, responsesIdioms, "idioms", screenviews_data, "idioms", "Admin", 10, frinexScores)
user_scores_idioms <- results_idioms$user_scores
user_responses_idioms <- results_idioms$user_responses

if (nrow(participants_uuids) != nrow(user_scores_idioms) ) {
  print("Idioms: there is a discrepancy between the number of rows in user-scores table and participant uuids, resp:")
  print(nrows(participants_uuids))
  print(nrows(user_idioms))
}

user_scores <- merge(user_scores_syntest, user_scores_idioms)
user_scores$totalCorrect <- (user_scores$syntestCorrect + user_scores$idiomsCorrect)
user_scores$totalIncorrect <- (user_scores$syntestIncorrect + user_scores$idiomsIncorrect)
write.csv(user_scores, file=paste0(experiment_abr,".user_scores.csv"))


