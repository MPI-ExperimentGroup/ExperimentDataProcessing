##--ravensession----------------------------------------------------------------##

library(jsonlite)
library(httr)
library(dplyr)
library(tidyr)
library(stringr)
source("../shared/BQ4_supportFunctions.R")

##--Configuration-------------------------------------------------------------##


experiment_abr <- "ravensession"
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


responsesPractice <- unique(subset(stimulusresponses_data, screenName=="stimuliScreenRavenPractice" & !is.na(isCorrect) & nchar(str_trim(isCorrect))>0))
responsesMain <- unique(subset(stimulusresponses_data, screenName=="stimuliScreenRavenTest" & !is.na(isCorrect) & nchar(str_trim(isCorrect))>0))

## help function 

harvest_scores <- function(listUuids, responses, roundname, screenviews_info, startScreenName, endScreenName, nStimuli) {
  
  user_scores <- data.frame()
  user_responses <- data.frame()
  
  for (rawUUID in listUuids){
    
    
    user <- paste0("uuid-", rawUUID)
    print(user)
    print("Num events for user:")
    
    raw_user_responses <- subset(responses, userId == user)
   
    print(nrow(raw_user_responses))
    
    new_user_responses <- raw_user_responses
    new_user_responses$isCorrect <- ifelse(new_user_responses$isCorrect == TRUE, 1, 0)
    
    # make overslaan N/A
    new_user_responses$isCorrect <- ifelse(new_user_responses$response == "Overslaan", "NA", new_user_responses$isCorrect)
   
    
    n_overslaan <- length(which(new_user_responses$isCorrect == "NA"))
    n_correct <- length(which(new_user_responses$isCorrect == 1))
    n_incorrect <- length(which(new_user_responses$isCorrect == 0))
    n_dontknow <- length(which(new_user_responses$responses == "Ik weet het niet"))
   
    # sanity check on the overall amount of stimuli
    if (as.numeric(n_correct) + as.numeric(n_incorrect) != nStimuli ) {
      print("Sanity error: discrepance between the number of evaluated stimuli and the number of stimuli")
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
    
    #sanity check for main on overall amount of incorrect
    if (roundname == "main") {
      n_wronganswers <- length(which(new_user_responses$isCorrect == 0 && new_user_responses$responses != "Ik weet het niet"))
      if (as.numeric(n_wronganswers) + as.numeric(n_dontknow) != as.numeric(n_incorrect) ) {
        print("Sanity error for main test: discrepance between the number of incorrect stimuli and the sum of worng answers  + dontknows")
        print(" # of incorrect:")
        print(n_incorrect)
        print("Worng answers + dont knows:")
        print(as.numeric(n_wronganswers) + as.numeric(n_dontknow))
        print("Where wrongs:")
        print(n_wronganswers)
        print("and dontknows:")
        print(n_dontknow)
        stop()
      }
    }
    user_responses <- rbind(user_responses, new_user_responses)
    
    
    # Get test duration for participant from screenviews
    screenviews_1user <- subset(screenviews_info, userId == user)
    
    test_duration <- get_test_duration(screenviews_1user, 
                                       startScreenName, nchar(startScreenName), endScreenName)
    
    startTime <- test_duration[["startTime"]]
    endTime <- test_duration[["endTime"]]
    testDuration <- test_duration[["testDuration"]]
    
    print("startTime")
    print(startTime)
    print("endTime")
    print(endTime)
    print("test duration")
    print(paste0(as.character(testDuration), " seconds"))
    
    if (roundname == "practice") {
      new_user_scores <- data.frame(user, n_correct, n_incorrect, testDuration)
      names(new_user_scores) <- c("userId", 
                                  "Practice Correct",
                                  "Practice Incorrect",
                                  "Practice duration (min)")
       
    } else {
      new_user_scores <- data.frame(user, n_correct, n_incorrect, n_overslaan, n_dontknow, testDuration)
      names(new_user_scores) <- c("userId", 
                                  "Main Correct",
                                  "Main Incorrect incl. 'ik weet het niet'",
                                  "Overslaan",
                                  "DoNotKnow",
                                  "Mian duration (min)")
      
    }
    user_scores<- rbind(user_scores, new_user_scores)
    
  }
  
  return(list(user_responses=user_responses, user_scores=user_scores))
 
  
}


##--Get aggregates practice ----------------------------------------------##


results_practice <- harvest_scores(participants_uuids$V1, responsesPractice, "practice", screenviews_data, "stimuliScreenRavenPractice", "stimuliScreenRavenTest", 6)
user_scores_practice <- results_practice$user_scores
user_responses_practice <- results_practice$user_responses

if (nrow(participants_uuids) != nrow(user_scores_practice) ) {
  print("Practice: there is a discrepancy between the number of rows in user-scores table and participant uuids, resp:")
  print(nrows(participants_uuids))
  print(nrows(user_scores_practice))
}

##--Get aggregates main ----------------------------------------------##

results <- harvest_scores(participants_uuids$V1, responsesMain, "main", screenviews_data, "stimuliScreenRavenTest", "admin", 36)
user_scores <- results$user_scores
user_responses <- results$user_responses

if (nrow(participants_uuids) != nrow(user_scores) ) {
  print("Main: there is a discrepancy between the number of rows in user-scores table and participant uuids, resp:")
  print(nrows(participants_uuids))
  print(nrows(user_scores))
}


user_scores <- merge(user_scores_practice, user_scores)
user_scores$totalCorrect <- (user_scores$correct + user_scores_practice$correct)
user_scores$totalIncorrect <- (user_scores$incorrect + user_scores_practice$incorrect)
write.csv(user_scores, file=paste0(experiment_abr,".user_scores.csv"))


