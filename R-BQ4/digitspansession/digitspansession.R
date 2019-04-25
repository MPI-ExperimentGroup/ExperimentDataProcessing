##--digitspansession----------------------------------------------------------------##

library(jsonlite)
library(httr)
library(dplyr)
library(tidyr)
library(stringr)
source("../shared/BQ4_supportFunctions.R")

## parameters (specification) --------------------------------------------------####

n_stimuli_forward <-  16
n_stimuli_backward <-  14
n_stimuli_per_length <- 2
max_n_allowed_errors <- 1
min_length <- 2
max_length_forward <- 9
max_length_forward <- 8


# read stimuli from the csv file

stimuli_csv_forward <- read.table("Digitspan_stimuli_forward.csv",  header=TRUE, sep=";")
stimuli_csv_backward <- read.table("Digitspan_stimuli_backward.csv",  header=TRUE, sep=";")

##--Configuration-------------------------------------------------------------##




experiment_abr <- "digitspansession"
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

participants_uuids <- read.csv("../shared/participants.csv", header = FALSE)

participants_columns <- c("userId", "staleCopy", "submitDate")

participants <- get_embedded("participants", participants_columns, admin_url)

participants_data <- subset(participants[["currentData"]], 
                            staleCopy == FALSE)

tagpairevents_columns <- c("userId", "tagDate", "eventTag",  "tagValue1", "tagValue2")
tagpairevents <- get_embedded("tagpairevents", tagpairevents_columns, admin_url, 1000)

tagpairevents_data <- unique(tagpairevents[["currentData"]])

screenviews_columns <- c("userId", "screenName", "viewDate")
screenviews <- get_embedded("screenviews", screenviews_columns, admin_url)
screenviews_data <- unique(subset(screenviews[["currentData"]]))

eventsForward = subset(tagpairevents_data, !grepl("summary", eventTag) & eventTag !="DataSubmission" &  (eventTag!="StimulusAudioShown") & grepl("forward", tagValue1) )
eventsBackward = subset(tagpairevents_data, !grepl("summary", eventTag) & eventTag !="DataSubmission" & (eventTag!="StimulusAudioShown") & grepl("backward", tagValue1) )


## help function 

harvest_scores <- function(listUuids, experimentEvents, roundname, startScreenName, startScreenMaxChars, endScreenName, stimuli_origin) {
  
  user_scores <- data.frame()
  user_responses <- data.frame()
  
  for (rawUUID in listUuids){
    
    
    user <- paste0("uuid-", rawUUID)
    print(user)
    print("Num events for user:")
    
    events <- subset(experimentEvents, userId == user)
   
    print(nrow(events))
    
  
    new_user_response <- events
    # replace "logTimeStamp" tag with with actual submit date-time of this tag
    new_user_response <- new_user_response %>%
      mutate(tagValue2 = ifelse(tagValue2 == "logTimeStamp", tagDate, tagValue2)) 
    
    # remve tagDate column 
    new_user_response <- new_user_response[,-2]
    
    # make freeText tag evaluating the response into isUserCorrect 
    new_user_response <- new_user_response %>%
      mutate(eventTag = ifelse(tagValue2 %in% c("correct", "incorrect"), 
                               "isUserCorrect", eventTag))
    
    new_user_response <- spread(new_user_response, eventTag, tagValue2) #eventTag content becomes a column name and tagValue2 becomes it corresponding value, given a pair uuid-tagValue1
    
    
    names(new_user_response)[names(new_user_response) == 'tagValue1'] <- 'stimulusSequence'
    
    names(new_user_response)[names(new_user_response) == 'freeText'] <- 'userResponse'
    
    names(new_user_response)[names(new_user_response) == 'freeText_ms'] <- 'Hit keys ms'
    
    # correcting typo
    names(new_user_response)[names(new_user_response) == 'Srart playing audio'] <-  'Start playing audio'
    
    
    # adding column representing stimulus sequence length
    seq <- new_user_response$stimulusSequence %>% str_replace(roundname, "")
    seq <- seq %>% str_replace_all("-", "")
    new_user_response$stimulusLength <-  nchar(seq)
    
    new_user_response <- new_user_response[ , c("userId", "stimulusSequence", "userResponse", "isUserCorrect", "stimulusLength", "Hit keys ms", "Fixation cross shown", "Start playing audio", "Button is pressed")]
    
    # Re-sort by fixation cross shown time-date
    new_user_response <- new_user_response[order(new_user_response[,7]), ]
    
    # sanity checks
    start <- nchar(roundname) + 1
    band_error_counter <- 0
    band_correct_counter <- 0
    band_stimuli_conter <- 0
    error_counter <- 0
    correct_counter <- 0
    
    # sanity check 
    for (i in 1:nrow(new_user_response)) {
      
      currentStimulusSequence <- new_user_response[i,]$stimulusSequence
      
      
      if (band_stimuli_conter == n_stimuli_per_length) {
        
        if (band_error_counter + band_correct_counter != n_stimuli_per_length) {
          print("Sanity error: the sum of band error counter and and correct counter does not equal to the given number of stimuli per band (length) ")
          print(currentStimulusSequence <- new_user_response[i,]$stimulusSequence)
          print(band_error_counter)
          print(band_correct_counter)
          print(n_stimuli_per_length)
          stop()
        }
        
        if (band_error_counter > max_n_allowed_errors) {
          print("Sanity error: the procedure should have been stopped before proposing this tsimulus")
          print(currentStimulusSequence <- new_user_response[i,]$stimulusSequence)
          stop()
        }
        
        band_error_counter <- 0
        band_correct_counter <- 0
      }
      
      
      l <- nchar(currentStimulusSequence)
      currentStimulusSequence <- substring(currentStimulusSequence, start, l)
      currentStimulusSequence <-currentStimulusSequence %>% str_replace_all("-", "")
      if (currentStimulusSequence  != stimuli_origin[i,]$string) {
        print("Sanity check error: incorrect sequence of stimuli, the following stimulus on the wrong position:")
        print(new_user_response[i,]$stimulusSequence)
        stop()
      }
      if (stimuli_origin[i,]$Correct.Answer == new_user_response[i,]$userResponse) {
        if (new_user_response[i,]$isUserCorrect != "correct") {
          print("Sanity check error: worong evaluation of the response")
          print(new_user_response[i,]$stimulusSequence)
          print(new_user_response[i,]$userResponse)
          print(new_user_response[i,]$isUserCorrect)
          print("Correct answer by secification")
          print(stimuli_origin[i,]$Correct.Answer)
          stop()
        } else {
          band_correct_counter <- band_correct_counter + 1
          correct_counter <- correct_counter + 1
        }
      } else {
        if (new_user_response[i,]$isUserCorrect != "incorrect") {
          print("Sanity check error: worong evaluation of the response")
          print(new_user_response[i,]$stimulusSequence)
          print(new_user_response[i,]$userResponse)
          print(new_user_response[i,]$isUserCorrect)
          stop()
        } else {
          band_error_counter <- band_error_counter + 1
          error_counter <- error_counter + 1
        }
      }
    } 
    
    # on finishingthe loop l will have the length of the last shown stimulus
    
    user_responses <- rbind(user_responses, new_user_response)
    
    
    # filling in scores
    n_correct <- length(which(new_user_response$isUserCorrect == "correct"))
    n_incorrect <- length(which(new_user_response$isUserCorrect == "incorrect"))
    
    # sanity checks  
    if (n_correct !=  correct_counter) {
      print("Sanity check error: frinex-calculated number of correct responses is not equal to the R-calculated 'manually'")
      print(n_correct)
      print(correct_counter)
      stop()
    }
    if (n_incorrect !=  error_counter) {
      print("Sanity check error: frinex-calculated number of incorrect responses is not equal to the R-calculated 'manually'")
      print(n_incorrect)
      print(error_counter)
      stop()
    }
    
    if (n_correct > 0) {
      correct_rows <- subset(new_user_response, isUserCorrect == "correct")
      max_length <- max(correct_rows$stimulusLength)
      if (l != max_length) {
        print("Sanity check error: the max length of the correct stimulus does not coinside with the length of the last shown stimulus (they must be in one length block)")
        print(max_length)
        print(l)
      }
    } else {
      max_length <- " " 
    }
    
    # Get test duration for participant from screenviews
    screenviews_1user <- subset(screenviews_data, userId == user)
    
    test_duration <- get_test_duration(screenviews_1user, 
                                       startScreenName, startScreenMaxChars, endScreenName)
    
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
                                paste0(roundname, " duration (min)")
    ) 
    user_scores<- rbind(user_scores, new_user_scores)
    
  }
  
  return(list(user_responses=user_responses, user_scores=user_scores))
  
}

##--Get aggregates ----------------------------------------------##

results_forward <- harvest_scores(participants_uuids$V1, eventsForward, "forward", "RunTrialsForward", 16, "InstructionsBackward", stimuli_csv_forward)
user_scores_forward <- results_forward$user_scores
user_responses_forward <- results_forward$user_responses

results_backward <- harvest_scores(participants_uuids$V1, eventsBackward, "backward", "RunTrialsBackward", 17, "Admin", stimuli_csv_backward)
user_scores_backward <- results_backward$user_scores
user_responses_backward <- results_backward$user_responses

# write.csv(user_scores_forward, file=paste0(experiment_abr,".forward.user_scores.csv"))
# write.csv(user_scores_backward, file=paste0(experiment_abr,".backward.user_scores.csv"))

user_scores <- merge(user_scores_forward, user_scores_backward)
user_scores$totalCorrect <- (user_scores$forwardCorrect + user_scores$backwardCorrect)
user_scores$totalIncorrect <- (user_scores$forwardIncorrect + user_scores$backwardIncorrect)
write.csv(user_scores, file=paste0(experiment_abr,".user_scores.csv"))

# user_responses_forward[] <- lapply(user_responses_forward, as.character)
#  Change values for IsUserCorrect from true/false to 1/0
# user_responses_forward$isUserCorrect <- ifelse(user_responses_forward$isUserCorrect == "correct", 1, 0)

# write.csv(user_responses_forward, file=paste0(experiment_abr,".forward.item_data.csv"))