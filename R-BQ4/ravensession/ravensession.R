##--ravensession----------------------------------------------------------------##

library(jsonlite)
library(httr)
library(dplyr)
library(tidyr)
library(stringr)
source("../shared/BQ4_supportFunctions.R")

## Sepcification ---------------------------------------------------------- ##

n_stimuli_practice <- 6
n_stimuli_main <- 36
stimuli_origin_main <- read.csv("AnswersAPMSet2.csv", header = TRUE, sep=";", fileEncoding="UTF-8-BOM")
stimuli_origin_practice <- read.csv("PracticeAPMSet2.csv", header = TRUE, sep=";", fileEncoding="UTF-8-BOM")


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

participants_uuids <- read.csv("../../participants.txt", header = FALSE, sep =';',  fileEncoding="UTF-8-BOM")
tagpairevents_columns <- c("userId", "screenName", "eventTag", "tagDate",
                           "tagValue1", "tagValue2")
screenviews_columns <- c("userId", "screenName", "submitDate", "viewDate")
stimulusresponses_columns <- c("userId", "screenName", "stimulusId", "response", "isCorrect", "tagDate", "eventMs")

l_ending_2 <- nchar(".gif")
stimuli_origin_practice$Picture <- as.character(stimuli_origin_practice$Picture)
stimuli_origin_main$Picture <- as.character(stimuli_origin_main$Picture)
stimuli_origin_practice$Picture <- substr(stimuli_origin_practice$Picture, 1, nchar(stimuli_origin_practice$Picture)-l_ending_2)
stimuli_origin_main$Picture <- substr(stimuli_origin_main$Picture, 1, nchar(stimuli_origin_main$Picture)-l_ending_2)




###--Process responses and aggregate scores----------------------------------###



## help function 

harvest_scores <- function(listUuids, roundname, startScreenName, endScreenName, nStimuli, originStimuli) {
  
  user_scores <- data.frame()
  user_responses <- data.frame()
  users_irregularities <- data.frame(userId=character(), message=character())
  stimuliId_Code <- NULL
  
  for (rawUUID in listUuids){
    
    
    user <- paste0("uuid-", rawUUID)
    print("******")
    print(user)
    
    stimulusresponses_user_raw <- findByUserIdOrderByTagDateAsc(admin_url, user, "stimulusresponses", "findByUserIdOrderByTagDateAsc", stimulusresponses_columns)
    
    if (is.null(stimulusresponses_user_raw)) {
      message <- "No stimulusresponses record for this user"
      users_irregularities <-  add_row(users_irregularities, userId=user, message = message)
      next
    }
    
    if (roundname == "practice") {
      responses <- subset(stimulusresponses_user_raw, screenName=="stimuliScreenRavenPractice" & !is.na(isCorrect) & nchar(str_trim(isCorrect))>0)
      
    } 
    
    if (roundname == "main") {
      responses<- subset(stimulusresponses_user_raw, screenName=="stimuliScreenRavenTest" & !is.na(isCorrect) & nchar(str_trim(isCorrect))>0)
    }
    
    
    
    events <- findByUserIdOrderByTagDateAsc(admin_url, user, "tagpairevents", "findByUserIdOrderByTagDateAsc", tagpairevents_columns)
    timeOutEvents <- subset(events, tagValue1=="timeOut")
    scoreEvents <- subset(events, eventTag=="summary" & tagValue1!="timeOut")
    
    # help list to create pairs stimuli ID <-> code 
    if (is.null(stimuliId_Code)) {
      shownEvents <- subset(events, eventTag=="StimulusCodeImageShown") 
      codeColumns <- c("tagValue1", "tagValue2")
      stimuliId_Code <- unique(shownEvents[codeColumns])
      l_prefix <- nchar("stimuli/")
      l_ending <- nchar(".png")
      stimuliId_Code$tagValue2 <- substr(stimuliId_Code$tagValue2, l_prefix+1, nchar(stimuliId_Code$tagValue2)-l_ending)
      names(stimuliId_Code)[names(stimuliId_Code)=="tagValue1"] <- "stimulusId"
      names(stimuliId_Code)[names(stimuliId_Code)=="tagValue2"] <- "code"
    }
    responses <- left_join(responses, stimuliId_Code, by="stimulusId")
    
    
    
    # filtering multiple answers
    new_user_responses <- responses[1,]
    for (i in 2:nrow(responses)) {
      isAlHere <- FALSE
      for (j in 1:nrow(new_user_responses)) {
        if (responses[i,]$stimulusId == new_user_responses[j,]$stimulusId) {
          if (responses[i,]$response == new_user_responses[j,]$response) {
            # duplicated submission of the same response (the system is double-secure in the case of broken i-net connection)
            isAlHere <- TRUE
          } else {
            if (new_user_responses[j,]$response != "Overslaan")  {
              message <- paste("Duplicated submission for ", responses[i,]$stimulusId, "with the responses", 
                               responses[i,]$response, 'and' ,  new_user_responses[j,]$response,
                               "the latest of the submitted will be considered", sep=" ")
              users_irregularities <- add_row(users_irregularities, userId = user,message = message)
              isAlHere <- TRUE
              if (responses[i,]$tagDate > new_user_responses[j,]$tagDate) {
                new_user_responses[j,] <- responses[i,]
              }
            }
          }
        }
      }
      if (!isAlHere) {
        new_user_responses <- rbind(new_user_responses, responses[i,])
      }
    }
    
    new_user_responses <- new_user_responses[order(new_user_responses$tagDate),]
    
   
    
    overslaans <- list()
    j <- 1
    m <- min(nStimuli, nrow(new_user_responses)) # in case of time out there may be less user responses than actual stimuli 
    for (i in 1:m) {
      if (new_user_responses[i,]$code != originStimuli[j,]$Picture) {
        print("Error in the sequence of stimuli, there is stimulus shown:")
        print(new_user_responses[i,]$code)
        print("But there must be ")
        print(originStimuli[j,]$Picture)
        stop()
      } else {
        if (new_user_responses[i,]$response == originStimuli[j,]$Correct_Answer) {
          if (!new_user_responses[i,]$isCorrect)  {
            print("Erroneous evalution of the response on stimulus ")
            print(new_user_responses[i,]$code)
            print(new_user_responses[i,]$response)
            stop()
          }
        } else {
          if (new_user_responses[i,]$isCorrect)  {
            print("Erroneous evalution of the response on stimulus ")
            print(new_user_responses[i,]$code)
            print(new_user_responses[i,]$response)
            stop()
          }
        }
      }
      j <- j+1
      if (new_user_responses[i,]$response == "Overslaan") {
        overslaans <- c(overslaans, new_user_responses[i,]$code) 
      }
    }
    
    if ((length(overslaans) > 0) && (nrow(new_user_responses)>nStimuli)) { 
      j <- 1
      for (i in (nStimuli+1):nrow(new_user_responses)) {
        if (new_user_responses[i,]$code != overslaans[j]) {
          print("Error in the sequence of overslaan stimuli, there is stimulus shown:")
          print(new_user_responses[i,]$code)
          print("But there must be ")
          print(overslaans[j])
          stop()
        } else {
          row <- subset(originStimuli, Picture == overslaans[j])
          correctAnswer <-  row[1,]$Correct_Answer           
          if (new_user_responses[i,]$response == correctAnswer) {
            if (!new_user_responses[i,]$isCorrect)  {
              print("Erroneous evalution of the response on stimulus ")
              print(new_user_responses[i,]$code)
              print(new_user_responses[i,]$response)
              stop()
            }
          } else {
            if (new_user_responses[i,]$isCorrect)  {
              print("Erroneous evalution of the response on stimulus ")
              print(new_user_responses[i,]$code)
              print(new_user_responses[i,]$response)
              stop()
            }
          }
        }
        j <- j+1
      }
    }
    
    new_user_responses$isCorrect <- ifelse(new_user_responses$isCorrect == TRUE, 1, 0)
    
  
    # set overslaan evaluation, which is later overwritten by some answer, to NA
    if (roundname == "main") {
      answered <- (subset(new_user_responses, response != "Overslaan"))$stimulusId
      overslaanAndAnswered <- (subset(new_user_responses, response == "Overslaan" & stimulusId %in% answered))$stimulusId
      new_user_responses$isCorrect <- ifelse(new_user_responses$response == "Overslaan" & new_user_responses$stimulusId %in% overslaanAndAnswered, "NA", new_user_responses$isCorrect)
      n_overslaan_no_answer <- length(which(new_user_responses$response == "Overslaan"  & !(new_user_responses$stimulusId %in% overslaanAndAnswered)))
      }
    
    user_responses <- rbind(user_responses, new_user_responses)
    
    # scoring
    # 1) Skipped and answered later will be counted and evaluated 1 time, because the overslaan answer for such stimuli is evaluated as NA 
    # 2) Skipped and not answered will be evaluated as incprrect
    n_correct <- length(which(new_user_responses$isCorrect == 1))
    n_incorrect <- length(which(new_user_responses$isCorrect == 0))
    n_dontknow <- length(which(new_user_responses$response == "Ik weet het niet"))
    
  
    # Get test duration for participant from screenviews
    screenviews_1user <- findByUserIdOrderByTagDateAsc(admin_url, user, "screenviews", "findByUserIdOrderByViewDateAsc", screenviews_columns)
    
    test_duration <- get_duration_min_via_view_date(screenviews_1user, startScreenName, endScreenName)
    
    startTime <- test_duration[["start_time_s"]]
    endTime <- test_duration[["end_time_s"]]
    testDurationSec <- endTime - startTime
    testDuration <- round(testDurationSec/60,2)
    
    print("startTime, sec")
    print(startTime)
    print("endTime, sec")
    print(endTime)
    print("test duration")
    print(paste0(as.character(testDuration), " minutes"))
    
    # define timeout
    timeOut <-0
    if (testDurationSec >= 1200) {
      timeOut <- 1
    }
   
    # sanity check on the overall amount of stimuli (and time out correction)
    if (as.numeric(n_correct) + as.numeric(n_incorrect) > nStimuli ) {
      print("Sanity error: the number of evaluated stimuli is greater than amount of the actual test stimuli")
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
    
    if (roundname == "main") {
      if (as.numeric(n_correct) + as.numeric(n_incorrect) < nStimuli) {
        if (timeOut == 0) {
          print("Sanity error: no time-out but the amount of evaluated stimuli is less than the overall stimuli")
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
      } 
    }
    
    
    
    
    if (roundname == "practice") {
      if (as.numeric(n_correct) + as.numeric(n_incorrect) < nStimuli)  {
        print("Sanity error: the amount of evaluated stimuli is less than the overall stimuli")
        print(" # of stimuli:")
        print(nStimuli)
        print("Evaluated (correct + incorrect):")
        print(as.numeric(n_correct) + as.numeric(n_incorrect))
        print("Where correct:")
        print(n_correct)
        print("and incorrect:")
        print(n_incorrect)
        stop()
        
      } else {
        print("Sanity check is passed: no time-out is detected and the amount of evaluated stimuli is the same as the amount of test stimuli")
      }
    }
    
    
    
    #sanity check for main on overall amount of incorrect
    if (roundname == "main") {
      n_wronganswers <- length(which(new_user_responses$isCorrect == 0 & new_user_responses$response != "Ik weet het niet"))
      if (as.numeric(n_wronganswers) + as.numeric(n_dontknow) != as.numeric(n_incorrect) ) {
        print("Sanity error for main test: discrepance between the number of incorrect stimuli and the sum of worng answers  + dontknows")
        print(" # of incorrect:")
        print(n_incorrect)
        print("Worng answers + dontknows:")
        print(n_wronganswers + as.numeric(n_dontknow))
        print("Where wrongs:")
        print(n_wronganswers)
        print("and dontknows:")
        print(n_dontknow)
        stop()
      } else {
        print("Sanity check on overall amount of incorrect passed")
        print(" # of incorrect:")
        print(n_incorrect)
        print("Worng answers + dontknows:")
        print(as.numeric(n_wronganswers) + as.numeric(n_dontknow))
      }
    }
    
    # sanity check frinex time-out
    if (roundname == "main") {
      frinexTimeOut <- nrow(timeOutEvents)
      if (as.numeric(frinexTimeOut) != 0 && as.numeric(frinexTimeOut) !=1) {
        print("Sanity error: the user appears more than once (ornegative amount of times ??? ) in the list of timeouts")
        print("Users' time outs:")
        print(as.numeric(frinexTimeOut))
        stop()
      }
      if (frinexTimeOut != timeOut) {
        frinexTimeOutAlert <- "Alert!"
      } else {
        frinexTimeOutAlert <- "ok"
      }
    }
    
    
    
    if (roundname == "practice") {
      new_user_scores <- data.frame(user, n_correct, n_incorrect, testDuration)
      names(new_user_scores) <- c("userId", 
                                  "practiceCorrect",
                                  "practiceIncorrect",
                                  "Practice duration (min)")
       
    } else {
      new_user_scores <- data.frame(user, n_correct, n_incorrect, n_overslaan_no_answer, timeOut, frinexTimeOut, frinexTimeOutAlert, testDuration)
      names(new_user_scores) <- c("userId", 
                                  "mainCorrect",
                                  "mainIncorrect",
                                  "in which overslaan not answered",
                                  "mainTimeOut",
                                  "main FrinexTimeOut  reported",
                                  "main FrinexTimeOutAlert",
                                  "main duration (sec)")
      
    }
    user_scores<- rbind(user_scores, new_user_scores)
    
  }
  
  return(list(user_responses=user_responses, user_scores=user_scores, irregularities=users_irregularities))
 
  
}


##--Get aggregates practice ----------------------------------------------##


results_practice <- harvest_scores(participants_uuids$V1, "practice", "stimuliScreenRavenPractice", "stimuliScreenRavenTest", n_stimuli_practice, stimuli_origin_practice)
scores_practice <- results_practice$user_scores
responses_practice <- results_practice$user_responses
irregularities_practice <- results_practice$irregularities

if (nrow(participants_uuids) != nrow(scores_practice) ) {
  message <- paste("Practice: there is a discrepancy between the number of rows in user-scores table and participant uuids, resp:", nrows(participants_uuids), nrows(scores_practice), sep =" ")
  irregularities_practice <- add_row(irregularities_practice, userId = user, message)
}


write.csv(irregularities_practice, file=paste0(experiment_abr,".practice_irregularities.csv"))


##--Get aggregates main ----------------------------------------------##

results <- harvest_scores(participants_uuids$V1, "main", "stimuliScreenRavenTest", "admin", n_stimuli_main, stimuli_origin_main)
scores <- results$user_scores
responses <- results$user_responses
irregularities <- results$irregularities

if (nrow(participants_uuids) != nrow(scores) ) {
  message <- paste("Practice: there is a discrepancy between the number of rows in user-scores table and participant uuids, resp:", nrows(participants_uuids), nrows(scores), sep =" ")
  irregularities <- add_row(irregularities, userId = user, message)
  
}


user_scores <- merge(scores_practice, scores)
scores$totalCorrect <- (scores$mainCorrect + scores_practice$practiceCorrect)
scores$totalIncorrect <- (scores$mainIncorrect + scores_practice$practiceIncorrect)

write.csv(scores, file=paste0(experiment_abr,".user_scores.csv"))
write.csv(responses, file=paste0(experiment_abr,".data_items.csv"))
write.csv(irregularities, file=paste0(experiment_abr,".irregularities.csv"))
