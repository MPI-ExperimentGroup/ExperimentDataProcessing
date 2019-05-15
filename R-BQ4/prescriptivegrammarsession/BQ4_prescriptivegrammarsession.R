##--Prescriptive Grammar------------------------------------------------------##

library(jsonlite)
library(httr)
library(dplyr)
library(tidyr)
source("../shared/BQ4_supportFunctions.R")

## parameters (specification) --------------------------------------------------####

n_stimuli <-  40
n_correct_sentences <- 20
n_incorrect_sentences <- 20
# read stimuli from the csv file

stimuli_csv_raw <- read.csv("Stimuli_megapilot.csv",  header=TRUE, sep=";", fileEncoding = "UTF-8-BOM")
stimuli_csv <- subset(stimuli_csv_raw, !grepl("Practice", Soundfile))



##--Configuration-------------------------------------------------------------##

experiment_abr <- "prescriptivegrammarsession"
server_url <- "http://frinexproduction.mpi.nl"
admin_url <- paste(server_url, "/", experiment_abr, "-admin", sep = "")#auth_file <- "../login.prescriptivegrammar_mante.csv"



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

participants_uuids <- read.csv("../shared/participants.csv", header = FALSE, sep=",", fileEncoding = "UTF-8-BOM")

participants_columns <- c("userId", "staleCopy", "submitDate")
participants <- get_embedded("participants", participants_columns, admin_url)
participants_data <- unique(subset(participants[["currentData"]], 
                                   staleCopy == FALSE))

tagpairevents_columns <- c("userId", "eventTag", 
                           "tagValue1", "tagValue2", "tagDate")
tagpairevents <- get_embedded("tagpairevents", tagpairevents_columns, admin_url)
tagpairevents_data <- unique(tagpairevents[["currentData"]])

screenviews_columns <- c("userId", "screenName", "submitDate")
screenviews <- get_embedded("screenviews", screenviews_columns, admin_url)
screenviews_data <- unique(screenviews[["currentData"]])

participant_events <- left_join(tagpairevents[["currentData"]], 
                                participants_data, 
                                by = "userId")


stimulus_resp_columns <- c("userId", "screenName", "stimulusId", "response", "isCorrect", "tagDate")
stimulusresponses <- get_embedded("stimulusresponses", stimulus_resp_columns, admin_url, 1000)
stimulusresponses_data <- unique(subset(stimulusresponses[["currentData"]], !grepl("Practice", stimulusId))) 



###--Process responses and aggregate scores----------------------------------###
user_scores <- data.frame()
user_responses <- data.frame()

users_multiple_submission <- data.frame(userId=character(), reason=character(), details = character())


start_pos<- nchar("./static/stimuli/") +1

for (rawUUID in participants_uuids$V1)  {
  
  user <- paste0("uuid-", rawUUID)
  print(user)
  
  
  stimulusresponses_user_raw <- subset(stimulusresponses_data, userId==user)
  stimulusresponses_user_raw <- stimulusresponses_user_raw[order(stimulusresponses_user_raw$tagDate),]
  
  # filtering multiple answers
  stimulusresponses_user <- stimulusresponses_user_raw[1,]
  for (i in 2:nrow(stimulusresponses_user_raw)) {
    isAlHere <- FALSE
    for (j in 1:nrow(stimulusresponses_user)) {
      if (stimulusresponses_user_raw[i,]$stimulusId == stimulusresponses_user[j,]$stimulusId) {
        if (stimulusresponses_user_raw[i,]$response == stimulusresponses_user[j,]$response) {
          # duplicated submission of the same response (the system is double-secure in the case of broken i-net connection)
          isAlHere <- TRUE
        } else {
          users_multiple_submission <- add_row(users_multiple_submission, userId = user,
                                               reason = paste0(stimulusresponses_user_raw[i,]$stimulusId, " with the responses ", stimulusresponses_user_raw[i,]$response, " and ",  stimulusresponses_user[j,]$response),
                                               details = "the latest of the submitted will be considered")
          isAlHere <- TRUE
          if (stimulusresponses_user_raw[i,]$tagDate > stimulusresponses_user[j,]$tagDate) {
            stimulusresponses_user[j,] <- stimulusresponses_user_raw[i,]
          }
        }
      }
    }
    if (!isAlHere) {
      stimulusresponses_user <- rbind(stimulusresponses_user, stimulusresponses_user_raw[i,])
    }
  }
  
  stimulusresponses_user <- stimulusresponses_user[order(stimulusresponses_user$tagDate),]
  
  stimuli_shown_user<- subset(tagpairevents_data, userId==user & eventTag=="StimulusAudioShown")
  
  # The users task is to select whether stimuli are correct or incorrect
  # A False Positives is therefore a case where the user thought that the 
  # stimulus was a correct sentence
  TP <- 0
  FP <- 0
  TN <- 0
  FN <- 0
  
  colnames(stimulusresponses_user)[colnames(stimulusresponses_user)=="screenName"] <- "stimulusName"
  
  # sanity check & true/false positives/negatives counting
  for (i in 1:nrow(stimulusresponses_user)) {
    currentStimulusId <- stimulusresponses_user[i,]$stimulusId
    currentStimulusFile <- unique(subset(stimuli_shown_user, tagValue1 == currentStimulusId))[1,]$tagValue2
    currentStimulusName<- substr(currentStimulusFile, start_pos, nchar(currentStimulusFile))
    stimulusCsv <- subset(stimuli_csv, Soundfile == paste0(currentStimulusName,".wav"))[1,]
    if (stimulusCsv$Correct.response == 1) { # correct combination
      if (stimulusresponses_user[i,]$response == "Correct") { # answer with 'correct'
        if (stimulusresponses_user[i,]$isCorrect != TRUE) {
          print("Frinex sanity error: correct answer is evaluated as incorrect")
          print(currenStimulusName)
          stop()
        } else {
          TP <- TP + 1
        }
      } else { #  answer with 'incorrect'
        if (stimulusresponses_user[i,]$isCorrect != FALSE) {
          print("Frinex sanity error: incorrect answer is evaluated as correct")
          print(currenStimulusName)
          stop()
        } else {
          FN <- FN + 1
        } 
      }
    } else { # incorrect combination
      if (stimulusresponses_user[i,]$response == "Correct") { # answer with 'correct'
        if (stimulusresponses_user[i,]$isCorrect != FALSE) {
          print("Frinex sanity error: incorrect answer is evaluated as correct")
          print(currenStimulusName)
          stop()
        } else {
          FP <- FP + 1
        }
      } else { #  answer with 'incorrect'
        if (stimulusresponses_user[i,]$isCorrect != TRUE) {
          print("Frinex sanity error: correct answer is marked as incorrect")
          print(currenStimulusName)
          stop()
        } 
        else {
          TN <- TN + 1
        }
      } 
    }
    stimulusresponses_user[i,]$stimulusName <- currentStimulusName
  }
  
  
  stimulusresponses_user <- stimulusresponses_user[order(stimulusresponses_user$tagDate), ]
  
  # sanity check order
  for (i in 1:n_stimuli) {
    if (stimuli_csv[i,]$Soundfile != paste0(stimulusresponses_user[i,]$stimulusName, ".wav")) {
      print("Sanity error: The order of the stimuli is broken. The i-th stimulus must be ")
      print(i)
      print(stimuli_csv[i,]$Soundfile)
      print("But it is:")
      print(stimulusresponses_user[i,]$stimulusName)
      print("Check reloads and multiple responses.")
      stop()
    }
  }
  
  user_responses <- rbind(user_responses, stimulusresponses_user)
  
  # sanity checks
  
  if ((TP + TN +FP +FN) != n_stimuli) {
    print("Sanity error: the sum of true negatives, true positives, false negatives and false positives differs from the amount of stimuli: ") 
    print(TP + TN +FP +FN)
    print(n_stimuli)
    stop()
  }
  
  if ((TP + FN) != n_correct_sentences) {
    print("Sanity error: the sum of true positives and false positives differs from the amount of correct sentences: ") 
    print(TP + FN)
    print(n_correct_sentences)
    stop()
  }
  
  if ((FP + TN) != n_incorrect_sentences) {
    print("Sanity error: the sum of true negatives and  false positives differs from the amount of incorrect sentences: ") 
    print(FP + TN)
    print(n_incorrect_sentences)
    stop()
  }
  
  TPcheck <- length(which(stimulusresponses_user$response == "Correct" & 
                            stimulusresponses_user$isCorrect == TRUE))
  if (TP != TPcheck) {
    print("Sanity error: # of true positives obtained via  csv-table R-evaluation and by stimulus-responses requests differ:")
    print(TP)
    print(TPcheck)
    stop()
  }
  FPcheck <- length(which(stimulusresponses_user$response == "Correct" & 
                            stimulusresponses_user$isCorrect == FALSE))
  if (FP != FPcheck) {
    print("Sanity error: # of false positives obtained  via  csv-table R-evaluation and by stimulus-responses requests differ:")
    print(FP)
    print(FPcheck)
    stop()
  }
  TNcheck <- length(which(stimulusresponses_user$response == "Incorrect" & 
                            stimulusresponses_user$isCorrect == TRUE))
  if (TN != TNcheck) {
    print("Sanity error: # of true negatives obtained  via  csv-table R-evaluation and by stimulus-responses requests differ:")
    print(TN)
    print(TNcheck)
    stop()
  }
  FNcheck <- length(which(stimulusresponses_user$response == "Incorrect" & 
                            stimulusresponses_user$isCorrect == FALSE))
  if (FN != FNcheck) {
    print("Sanity error: # of false negatives obtained  via  csv-table R-evaluation and by stimulus-responses requests differ:")
    print(FN)
    print(FNcheck)
    stop()
  }
  
  summary_user <- subset(tagpairevents_data, userId == user & grepl("score:", tagValue2))
  score <- lapply(strsplit(summary_user$tagValue2,":"), 
                  function(x) trimws(x[2]))
  if (score != as.numeric(TP) + as.numeric(TN) ) {
    print("Sanity error: # frinex-obtained score  differsfrom the sum of true-positive and true-negative answers:")
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

user_responses$response <- ifelse(user_responses$response == "Correct", 1, 0)
user_responses$isCorrect <- ifelse(user_responses$isCorrect == TRUE, 1, 0)


write.csv(user_responses, file=paste0(experiment_abr,".item_data.csv"))

if (nrow(users_multiple_submission)==0) {
  users_multiple_submission <- add_row(users_multiple_submission, userId = " ", reason = "no multiple responses on the same stimuli were detected", details =" ")
}
write.csv(users_multiple_submission, file=paste0(experiment_abr,".multiple_responses.csv"))
