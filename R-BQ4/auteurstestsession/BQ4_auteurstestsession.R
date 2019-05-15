##--auteurstestsession ---------------------------------------------------------------##

library(jsonlite)
library(httr)
library(dplyr)
library(tidyr)
library(stringr)
source("../shared/BQ4_supportFunctions.R")

## parameters (specification) --------------------------------------------------####

n_stimuli <-  132
n_authors <- 90
n_nots <- 42

# read stimuli from the csv file
stimuli_csv <- read.csv("Auteurstest_definitief.csv",  header=TRUE, sep=";", fileEncoding="UTF-8-BOM")

# noncrtical: the list of the authors for which the internal stimuli id's are inconsistent (non-visible to the participant and theresearcher)
lecarre <- "John_le_Carr\u00E9"
enc2utf8(lecarre)

stimuli_inconsistent_ids <-  c("George_Eliot", "Miguel_de_Cervantes", lecarre)

not_saved_participant <- "3e9ac142-f85c-4508-9577-2bd9ddf50f72_2"
not_saved_participant_authors <- c("auth49_Thea_Beckman_author", 
                                   "auth58_Virginia_Woolf_author", 
                                   "auth64_Harry_Mulisch_author", 
                                   "auth67_J.D._Salinger_author", 
                                   "auth70_Annie_M.G._Schmidt_author", 
                                   "auth73_Karin_Slaughter_author",
                                   "auth82_James_Patterson_author", 
                                   "auth56_Marion_Pauw_author", 
                                   "auth68_A.F.Th._van_der_Heijden_author",
                                   "auth54_Chiara_Ricci_not",
                                   "auth63_Roald_Dahl_author",
                                   "auth69_Stephen_King_author",
                                   "auth72_Dan_Brown_author",
                                   "auth81_Herman_Koch_author",
                                   "auth84_J.K._Rowling_author", 
                                   "auth13_Simone_van_der_Vlugt_author",
                                   "auth25_Jane_Austen_author", 
                                   "auth31_J.R.R._Tolkien_author",
                                   "auth5_Donna_Tartt_author",
                                   "auth11_Mark_Twain_author",
                                   "auth17_David_Baldacci_author",
                                   "auth20_Stieg_Larsson_author",
                                   "auth18_John_Grisham_author",
                                   "auth36_Tom_Clancy_author",
                                   "auth110_Khaled_Hosseini_author",
                                   "auth128_Theresa_Ziegler_not",
                                   "auth105_Astrid_Lindgren_author", 
                                   "auth132_Nicci_French_author")




##--Configuration-------------------------------------------------------------##

experiment_abr <- "auteurstestsession"
server_url <- "https://frinexproduction.mpi.nl"
admin_url <- paste(server_url, "/", experiment_abr, "-admin", sep = "")


# Use this to skip users if users from earlier application versions with 
# incompatible data definitions are still in the database.


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

participants_uuids <- read.csv("../shared/participants.csv", header = FALSE, fileEncoding="UTF-8-BOM")

participants_columns <- c("userId", "workerId", "staleCopy", "submitDate")
participants <- get_embedded("participants", participants_columns, admin_url)


tagpairevents_columns <- c("userId", "screenName", "eventTag", "tagDate",
                           "tagValue1", "tagValue2")
tagpairevents <- get_embedded("tagpairevents", tagpairevents_columns, admin_url, 1000)
tagpairevents_data <- unique(tagpairevents[["currentData"]])


screenviews_columns <- c("userId", "screenName", "submitDate")
screenviews <- get_embedded("screenviews", screenviews_columns, admin_url)
screenviews_data <- screenviews[["currentData"]]


participants_data <- unique(subset(participants[["currentData"]], 
                                   staleCopy == FALSE))


participant_events <- left_join(tagpairevents[["currentData"]], 
                                participants_data, 
                                by = "userId")

stimulus_resp_columns <- c("userId", "screenName", "stimulusId", "response", "isCorrect", "tagDate")
stimulusresponses <- get_embedded("stimulusresponses", stimulus_resp_columns, admin_url, 1000)
stimulusresponses_data <- unique(stimulusresponses[["currentData"]])


user_scores <- data.frame()
user_responses <- data.frame()

users_multiple_submission <- data.frame()


for (rawUUID in participants_uuids$V1)  {
  
  
  user <- paste0("uuid-", rawUUID)
  print(user)
  
  
  
      
      stimulusresponses_user_raw <- subset(stimulusresponses_data, userId==user)
      
      if (rawUUID != not_saved_participant) {
        stimulusIDs <- unique(subset(stimulusresponses_user_raw, select=c(stimulusId)))
      } else { # complete the data with the screenshot data from Marjolijn
        #  re-use stimuli list from the previously succesfully saved user
        latestTagDate <- max(stimulusresponses_user_raw$tagDate)
        for (i in 1:nrow(stimulusIDs)) {
          stimulusID <- stimulusIDs[i,]
          rows <- subset(stimulusresponses_user_raw, stimulusId == stimulusID)
          if (nrow(rows)==0) { # correction using screenshots of Marjolijn
             if (stimulusID %in% not_saved_participant_authors) {
              response <- "true"
              if (endsWith(stimulusID, "_author")){
                isCorrect <- TRUE
              } else {
                isCorrect <- FALSE
              }
            } else {
              response <- "false"
              if (endsWith(stimulusID, "_not")){
                isCorrect <- TRUE
              } else {
                isCorrect <- FALSE
              }
            }
            new_stimulus_response <- data.frame(user, "stimuliScreenAuthorCheck", stimulusID, response, isCorrect, latestTagDate) 
            names(new_stimulus_response) <- c("userId", "screenName", "stimulusId", "response", "isCorrect", "tagDate")
            stimulusresponses_user_raw <- rbind(stimulusresponses_user_raw, new_stimulus_response)
          }
          
        }
        
      }  
      
      n_auth_check <- length(which(endsWith(stimulusIDs$stimulusId, "_author")))
      
      if (n_auth_check != n_authors) {
        print("Sanity error: number of author-marked stimuli differs of the number of authors given in the specification")
        print( n_auth_check)
        print(n_authors)
        stop()
      }
      
      n_not_check <- length(which(endsWith(stimulusIDs$stimulusId, "_not")))
      if (n_not_check != n_nots) {
        print("Sanity error: number of author-marked stimuli differs of the number of not-s given in the specification")
        print(n_not_check)
        print(n_nots)
        stop()
      }
      
      # checking coding of the stimuli  
      for (i in 1:nrow(stimuli_csv)) {
        
        if (!(stimuli_csv[i,]$Naam %in% stimuli_inconsistent_ids)) {
          current_stimulus_rows <- subset(stimulusIDs, grepl(stimuli_csv[i,]$Naam, gsub("\\.", "", stimulusId)))
          if (nrow(current_stimulus_rows) != 1) {
            print("Sanity error: there is an ambigous encoding for the stimuli corresponding to the person")
            print(stimuli_csv[i,]$Naam)
            stop()
          }
          current_stimulus <- current_stimulus_rows[1,]
          if((endsWith(current_stimulus, "_author") &&  stimuli_csv[i,]$Code == "NON") ||  
             (endsWith(current_stimulus, "_not") &&  stimuli_csv[i,]$Code == "AUT")) {
            print("Sanity error: incorect encoding of the stimuli corresponding to the person")
            print(stimuli_csv[i,]$Naam)
            stop()
          }
        }
        
      }
      
      
      # on the multiple submission the last attempt is taken as the end result
      # the staring time is the time stamp of the last apperanace of the stimulus screen
      # the ending time  is the timestamp of the last scoring attempt
      score_events <- subset(tagpairevents_data,  userId == user & startsWith(tagValue2, "totalScore:"))
      score_events <- score_events[order(score_events[,"tagDate"]), ]
      
      if (nrow(score_events) > 1) {
        new_multiple_submission <- data.frame("userId"=c(user))
        users_multiple_submission <- rbind(users_multiple_submission, new_multiple_submission)
      }
      
      if (nrow(score_events) < 1) {
        if (rawUUID != not_saved_participant){
          print("Error in the submission of the participant's results")
          stop()
        }
      }
      
      # last attempt to submit
      if (rawUUID != not_saved_participant) {
        submitDate <- score_events[nrow(score_events),]$tagDate
        # submitDate <- subset(participants_data, userId == user)$submitDate
      } else {
        submitDate <- latestTagDate
      }
      
      
      userName <- subset(participants_data, userId == user)$workerId
      
      
      stimulus_responses_user <- data.frame()
      
      for (stimulusID in  stimulusIDs$stimulusId) {
        stimulus_event <- subset(stimulusresponses_user_raw, stimulusId == stimulusID)
        last_time <- max(stimulus_event$tagDate)
        stimulus_event <- subset(stimulus_event, tagDate ==  last_time)
        stimulus_responses_user <- rbind(stimulus_responses_user, stimulus_event)
      }
      
      
      print("Num responses for user:")
      print(nrow(stimulus_responses_user))
      
      
      
      if (nrow(stimulus_responses_user) != n_stimuli) {
        print("Something went terribly wrong: the amount of rows of the post-processed table in the R-script still differs from the # of stimuli")
        print(nrow(stimulus_responses_user))
        print(n_stimuli)
        stop()
      }
      
      stimulus_responses_user$isAuthor <- ifelse(endsWith(stimulus_responses_user$stimulusId, "_author"), 1, 0)
      stimulus_responses_user$response <- ifelse(stimulus_responses_user$response == "true", 1, 0)
      stimulus_responses_user$isCorrect <- ifelse(stimulus_responses_user$isCorrect == TRUE, 1, 0)
      
      #sanity
      for (i in 1: nrow(stimulus_responses_user)) {
        
        if (!endsWith(stimulus_responses_user[i,]$stimulusId, "not") & !endsWith(stimulus_responses_user[i,]$stimulusId, "author")) {
          print("Sanity error: the following stimulus is not marked as an author or a not-author")
          print(stimulus_responses_user[i,]$stimulusId)
          stop()
        }
        if (endsWith(stimulus_responses_user[i,]$stimulusId, "not") && stimulus_responses_user[i,]$isAuthor == 1) {
          print("Sanity error: the following stimulus is marked as an author")
          print(stimulus_responses_user[i,]$stimulusId)
          stop()
        }
        
        if (endsWith(stimulus_responses_user[i,]$stimulusId, "author") && stimulus_responses_user[i,]$isAuthor == 0) {
          print("Sanity error: the following stimulus is marked as NOT an author")
          print(stimulus_responses_user[i,]$stimulusId)
          stop()
        }
        
        if (stimulus_responses_user[i,]$isAuthor == 0) {
          if  (stimulus_responses_user[i,]$response == 1) {
            if (stimulus_responses_user[i,]$isCorrect == 1) {
              print("Sanity error: misevaluation of the response")
              print(stimulus_responses_user[i,]$stimulusId)
              stop()
            }
          }
        }
        if (stimulus_responses_user[i,]$isAuthor == 0) {
          if  (stimulus_responses_user[i,]$response == 0) {
            if (stimulus_responses_user[i,]$isCorrect == 0) {
              print("Sanity error: misevaluation of the response")
              print(stimulus_responses_user[i,]$stimulusId)
              stop()
            }
          }
        }
        if (stimulus_responses_user[i,]$isAuthor == 1) {
          if  (stimulus_responses_user[i,]$response == 0) {
            if (stimulus_responses_user[i,]$isCorrect == 1) {
              print("Sanity error: misevaluation of the response")
              print(stimulus_responses_user[i,]$stimulusId)
              stop()
            }
          }
        }
        if (stimulus_responses_user[i,]$isAuthor == 1) {
          if  (stimulus_responses_user[i,]$response == 1) {
            if (stimulus_responses_user[i,]$isCorrect == 0) {
              print("Sanity error: misevaluation of the response")
              print(stimulus_responses_user[i,]$stimulusId)
              stop()
            }
          }
        }
      }
      
      
      stimulus_responses_user$submitDate <- submitDate
      
      
      
      # sanity check
      for (stimulusID  in stimulusIDs$stimulusId) {
        respRow <- subset(stimulus_responses_user, stimulusId==stimulusID)
        if (nrow(respRow) != 1) {
          print("Sanity error: more than 1 rows (or no rows at all) in posprocessed stimulus reasponces table for this user, for stimulus")
          print(stimulusId)
          stop()
        }
      }
      
      
      user_responses <- rbind(user_responses, stimulus_responses_user)
      
      
      
      TP <- length(which(stimulus_responses_user$response == 1 & 
                           stimulus_responses_user$isAuthor == 1))
      
      FP <- length(which(stimulus_responses_user$response == 1 & 
                           stimulus_responses_user$isAuthor == 0))
      
      TN <- length(which(stimulus_responses_user$response == 0 & 
                           stimulus_responses_user$isAuthor == 0))
      
      FN <- length(which(stimulus_responses_user$response == 0 & 
                           stimulus_responses_user$isAuthor == 1))
      
      
      
      # sanity check 
      n_correct <- length(which(stimulus_responses_user$isCorrect == 1))
      n_incorrect <- length(which(stimulus_responses_user$isCorrect == 0))
      
      if (as.numeric(n_correct) + as.numeric(n_incorrect) != n_stimuli) {
        print("Sanity error: the sum  of correct and incorrect answers differs form the overall amount of stimuli")
        print(n_correct)
        print(n_incorrect)
        print(n_stimuli)
        stop()
      }
      
      if (as.numeric(TP) + as.numeric(FN) != as.numeric(n_authors) ) {
        print("Sanity error: the sum  of true positives and false negatives computed by R differs from the total amount of authors")
        print(TP)
        print(TN)
        print(scoreFrinex)
        stop()
      }
      
      if (as.numeric(TN) + as.numeric(FP) != as.numeric(n_nots) ) {
        print("Sanity error: the sum  of false positives and true negatives computed by R differs from the total amount of notss")
        print(TN)
        print(FP)
        print(scoreFrinex)
        stop()
      }
      
      if (as.numeric(TP) + as.numeric(TN) != as.numeric(n_correct) ) {
        print("Sanity error: the sum  of true positives and negatives computed by R differs from the total amount of correct:")
        print(TP)
        print(TN)
        print(scoreFrinex)
        stop()
      }
      
      if (as.numeric(FP) + as.numeric(FN) != as.numeric(n_incorrect) ) {
        print("Sanity error: the sum  of false positives and negatives computed by R differs from ttotal amount of incorrect")
        print(FP)
        print(FN)
        print(errorsFrinex)
        stop()
      }
      
      
      # Get test duration for participant from screenviews
      endTime <- strptime(substring(submitDate, 1, nchar(submitDate)-9), 
                          format = "%Y-%m-%dT%H:%M:%S")
      # start of the experiment is the last appearance of the stimuliScreen (we count the last attempt)
      screenviews_1user <- subset(screenviews_data, 
                                  userId == user & screenName == "stimuliScreenAuthorCheck")
      screenviews_1user <- screenviews_1user[order(screenviews_1user[,"submitDate"]), ]
      
      startDate <- screenviews_1user[nrow(screenviews_1user),]$submitDate
      
      startTime <- strptime(substring(startDate, 1, nchar(startDate)-9), 
                            format = "%Y-%m-%dT%H:%M:%S")
      
      
      
      startTime_s <- as.numeric(startTime)
      endTime_s <- as.numeric(endTime)
      testDuration <- round((endTime_s - startTime_s)/60,2)
      
      print("startDate")
      print(startDate)
      print("endTime")
      print(submitDate)
      print("test duration")
      print(paste0(as.character(testDuration), " min"))
      
      new_user_scores <- data.frame(user, TP, FP, TN, FN, testDuration, startTime, endTime)
      names(new_user_scores) <- c("userId", 
                                  "Hits (True Positives)",
                                  "False Alarm (False Positives)",
                                  "Correct Rejection (True Negative)",
                                  "Missing (False Negative)",
                                  "Test Duration min",
                                  "Start time",
                                  "End time"
      ) 
      user_scores <- rbind(user_scores, new_user_scores)
    
 
} 



 if  (nrow(user_scores) != nrow(participants_uuids)) {
   print("sanity-check error: number of the participants for which the scores are counted, differs form the amount of the given participants")
   print(nrow(user_scores))
   print(nrow(participants_uuids))
   stop()
 }

write.csv(users_multiple_submission, file=paste0(experiment_abr,".multiple_submissions.csv"))

# Write scores to file
write.csv(user_scores, file=paste0(experiment_abr,".user_scores.csv"))

#user_responses <- user_responses[order(user_responses[,3], user_responses[,5]), ]
write.csv(user_responses, file=paste0(experiment_abr,".item_data.csv"))
