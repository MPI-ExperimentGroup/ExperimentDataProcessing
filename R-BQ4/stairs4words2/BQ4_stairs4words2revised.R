##--Stairs for words English (2 rounds)---------------------------------------##

library(jsonlite)
library(httr)
library(dplyr)
library(tidyr)
library(stringr)
source("../shared/BQ4_supportFunctions.R")


##--Configuration-------------------------------------------------------------##

experiment_abr <- "stairs4words2"
server_url <- "http://frinexproduction.mpi.nl"
admin_url <- paste(server_url, "/", experiment_abr, "-admin", sep = "")


# These variables have different values for the Dutch and English versions of
# Stairs4words
nonWordValue <- "0"
start_screenName <- "stimuliScreenV"

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

##--Get data from REST interface----------------------------------------------##

participants_uuids <- read.csv("../shared/participants.csv", header = FALSE)

participants_columns <- c("userId", "workerId")
participants <- get_embedded("participants", participants_columns, admin_url)
participants_data <- unique(subset(participants[["currentData"]]))

tagpairevents_columns <- c("userId", "screenName", "eventTag", 
                       "tagValue1", "tagValue2", "tagDate")
tagpairevents <- get_embedded("tagpairevents", tagpairevents_columns, admin_url, 1000)

tagpairevents_data <- unique(tagpairevents[["currentData"]])

screenviews_columns <- c("userId", "screenName", "viewDate")
screenviews <- get_embedded("screenviews", screenviews_columns, admin_url, 0)
screenviews_data <- unique(subset(screenviews[["currentData"]]))

stimulus_resp_columns <- c("userId", "screenName", "stimulusId", "response", "isCorrect", "tagDate")
stimulusresponses <- get_embedded("stimulusresponses", stimulus_resp_columns, admin_url, 1000)
stimulusresponses_data <- unique(subset(stimulusresponses[["currentData"]])) 
stimulusresponses_data$response <- str_replace_all(stimulusresponses_data$response, "&#44;", ",")
stimulusresponses_data$isCorrect <- ifelse(stimulusresponses_data$isCorrect == TRUE, 1, 0)

##--Pre-process fast track and finetuning-------------------------------------##

fast_track <- subset(tagpairevents_data, eventTag == "fast_track" &   tagValue2 != " ; ; ; ; ; ;" & !startsWith(TagValue2,"BandNumber;Label;UserAnswer;IsAnswerCorrect;"))


fast_track$label <- lapply(strsplit(fast_track$tagValue2,";"), 
                           function(x) x[1])

fast_track$BandNumber <- lapply(strsplit(fast_track$tagValue2,";"), 
                                function(x) x[2])

fast_track$UserAnswer <- lapply(strsplit(fast_track$tagValue2,";"), 
                                function(x) x[3])

fast_track$IsUserCorrect <- lapply(strsplit(fast_track$tagValue2,";"), 
                                   function(x) x[4])

fast_track$timestamp <- lapply(strsplit(fast_track$tagValue2,";"), 
                                   function(x) x[5])

fast_track$NonwordsFrequencyAtThisPoint <- lapply(strsplit(fast_track$tagValue2,";"), 
                                                  function(x) x[6])

fast_track$visitingNumber <- NA

fast_track$IsUserCorrect <- ifelse(fast_track$IsUserCorrect == "true", 1, 0)



fine_tuning <- subset(tagpairevents_data, eventTag == "fine_tuning"  &   tagValue2 != " ; ; ; ; ; ;"  & !startsWith(TagValue2,"BandNumber;Label;UserAnswer;IsAnswerCorrect;"))

fine_tuning$label <- lapply(strsplit(fine_tuning$tagValue2,";"), 
                            function(x) x[1])
fine_tuning$BandNumber <- lapply(strsplit(fine_tuning$tagValue2,";"), 
                                 function(x) x[2])
fine_tuning$UserAnswer <- lapply(strsplit(fine_tuning$tagValue2,";"), 
                                 function(x) x[3])
fine_tuning$IsUserCorrect <- lapply(strsplit(fine_tuning$tagValue2,";"), 
                                    function(x) x[4])

fine_tuning$timestamp <- lapply(strsplit(fine_tuning$tagValue2,";"), 
                               function(x) x[5])

fine_tuning$NonwordsFrequencyAtThisPoint <- NA

fine_tuning$visitingNumber <- lapply(strsplit(fine_tuning$tagValue2,";"), 
                               function(x) x[6])

fine_tuning$IsUserCorrect <- ifelse(fine_tuning$IsUserCorrect == "true", 1, 0)


rating_buttons <- subset(tagpairevents_data, eventTag == "RatingButton")

rating_buttons$label <- lapply(strsplit(rating_buttons$tagValue1,"_"), 
                               function(x) x[1])
names(rating_buttons)[names(rating_buttons) == 'tagValue2'] <- 'UserAnswer'

rating_buttons$UserAnswer <- str_replace_all(rating_buttons$UserAnswer, "&#44;", ",")

##--Write raw item data-------------------------------------------------------##

item_data  <- as.vector(c())

for (rawUUID in participants_uuids$V1)  {
  
  user <- paste0("uuid-", rawUUID)
  print(user)
  
  combined_2_rounds <- as.vector(c())
  
  # Skip participants without item data
  if (nrow(subset(fast_track, userId == user)) == 0){
    next
  }
  
  
  # Skip participants for whom start of fast track round 1 can't be determined
  fast_track_R1_start <- subset(screenviews_data, userId == user & 
           screenName == paste0(start_screenName,1))[,c("viewDate")]
  if (length(fast_track_R1_start) == 0){
    next
  }
  
  # Skip participants for whom start of fast track round 2 can't be determined
  fast_track_R2_start <- subset(screenviews_data, userId == user & 
            screenName == paste0(start_screenName,2))[,c("viewDate")]
  if (length(fast_track_R2_start) == 0){
    next
  }
  
  for(round in c("1","2")){
    
    round_screenName <- paste0(start_screenName,round)
    
    fast_track_1user <- subset(fast_track, userId == user & 
                                 screenName == round_screenName)
    # sanity check: two last fast track answers must be wrong and thirdfrom the end should be correct
    lastFastT <- nrow(fast_track_1user)
    if (fast_track_1user[lastFastT,]$IsUserCorrect != 0) {
      print("Sanity error: the last answer of the fast track must be 'false' but it is:")
      print(fast_track_1user[lastFastT,]$IsUserCorrect)
      stop()
    }
    if (fast_track_1user[lastFastT-1,]$IsUserCorrect != 0) {
      print("Sanity error: the pre-last answer of the fast track must be 'false' but it is:")
      print(fast_track_1user[lastFastT-1,]$IsUserCorrect)
      stop()
    }
    if (lastFastT >= 3) {
      if (fast_track_1user[lastFastT-2,]$IsUserCorrect != 1) {
        print("Sanity error: the pre-pre-last answer of the fast track must be 'true' but it is:")
        print(fast_track_1user[lastFastT-2,]$IsUserCorrect)
        print('Band Number:')
        print(fast_track_1user[lastFastT-2,]$BandNumber)
        stop()
      }
    }
    
    fastTrackLastBand <- ifelse(fast_track_1user[lastFastT,]$BandNumber != 0, fast_track_1user[lastFastT,]$BandNumber, fast_track_1user[lastFastT-1,]$BandNumber)
    if (fastTrackLastBand == 0) {
      print("Experiment omplementation flaw: bad word-nonword distribution on the fast track, two last stimuli in the fast track are nonwords.")
      stop()
    }
    
    # select raw data for one user
    fine_tuning_1user <- subset(fine_tuning, userId == user & 
                                  screenName == round_screenName)
    
    # Correctness of imlplementation check
    fineTuningFirstBand <- fine_tuning_1user[1,]$BandNumber
    if (fineTuningFirstBand > 0 ) { # not a non-word
      if (fastTrackLastBand > 1) {
        if (fastTrackLastBand < 54 || (fastTrackLastBand == 54 && fast_track_1user[lastFastT,]$IsUserCorrect == 0)) {
          if (as.numeric(fastTrackLastBand) - 1 != fineTuningFirstBand) {
            print("Experiment implementation flaw: the first band in fine tuning is not equal to the last band of fast track - 1")
            print(fineTuningFirstBand)
            print(fastTrackLastBand)
            stop()
          }
        }  else {
          if (fineTuningFirstBand != 54) {
            print("Experiment implementation flaw: the first band of the fine tuning must be 54 since the last band of the fast track is 54 and the user answer correctly on the last fast track stimulus")
            print(fineTuningFirstBand)
            print(fastTrackLastBand)
            stop()
          }
        }
      }else {
        if (fineTuningFirstBand != 1) {
          print("Experiment implementation flaw: the first band of the fine tuning must be 1 since the last band of the fast track is 1")
          print(fineTuningFirstBand)
          print(fastTrackLastBand)
          stop()
        }
      }
      }
     
   

    rating_buttons_1user <- subset(rating_buttons, userId == user & 
                                     screenName == round_screenName)
    
    # convert all columns to character type
    fine_tuning_1user[] <- lapply(fine_tuning_1user, as.character)
    fast_track_1user[] <- lapply(fast_track_1user, as.character)
    rating_buttons_1user[] <- lapply(rating_buttons_1user, as.character)
    
  
    # Concatenate fast track and fine tuning data
    item_data_1user <- rbind(fast_track_1user, fine_tuning_1user)
    item_data_1user <- item_data_1user[order(item_data_1user[,"timestamp"]), ]
    
 
    
    
    # Get start of experiment from screenViews
    fast_track_start <- max(subset(screenviews_data, 
                                   userId == user & 
                                     screenName == round_screenName)[,c("viewDate")])
    # Join concatenated item data to rating button data to get fine grained 
    # timestamps in miliseconds
    combined_1_user <- left_join(item_data_1user, 
                                 rating_buttons_1user[,c("userId", "label", "UserAnswer", "tagDate")], 
                                 by = c("userId", "label", "UserAnswer"))
    
    
    # sanity check: compare the R-derived stimuli sequence for 1 user with the stimuli sequence freom stimulusResponse
    responses_1user <- subset(stimulusresponses_data, userId == user & screenName == round_screenName)
    responses_1user <- responses_1user[order(responses_1user[,"tagDate"]), ]
    if (nrow(combined_1_user) != nrow(responses_1user)) {
      print("Sanity check error: there is a discrepance between the number of Frinex-registered responses and the number of R-calculated responses, resp.:")
      print(nrow(combined_1_user)) 
      print(nrow(responses_1user)) 
      stop()
    }
    for (i in 1:nrow(combined_1_user)) {
      combined_1_user_stimulus <- combined_1_user[i,]$label
      responses_1user_stimulus <- responses_1user[i,]$stimulusId
      if (combined_1_user_stimulus != responses_1user_stimulus) {
        print("Sanity check error: the i-th stimulus in the R-calculated response sequence and in the Frinex-registered one are different:")
        print(i)
        print(combined_1_user_stimulus) 
        print(responses_1user_stimulus) 
        stop()
      }
      combined_1_user_response <- combined_1_user[i,]$UserAnswer
      responses_1user_response <- responses_1user[i,]$response
      if (combined_1_user_response != responses_1user_response) {
        print("Sanity check error: the response on the i-th stimulus in the R-calculated response sequence and in the Frinex-registered one are different:")
        print(i)
        print(combined_1_user_response) 
        print(responses_1user_response) 
        stop()
      }
      combined_1_user_correctness <- combined_1_user[i,]$IsUserCorrect;
      responses_1user_correctness <- responses_1user[i,]$isCorrect
      if (combined_1_user_correctness != responses_1user_correctness) {
        print("Sanity check error: the evaluation of the i-th stimulus in the R-calculated response sequence and in the Frinex-registered one are different:")
        print(i)
        print(combined_1_user_correctness) 
        print(responses_1user_correctness) 
        stop()
      }
    }
    
    # Experiment implementation check: correctness of the fine tuning flow
    user_summary <- subset(tagpairevents_data, userId == user & 
                             eventTag == "user_summary" & 
                             tagValue1 == "row000001" & screenName == round_screenName)
    
    user_summary <- lapply(strsplit(user_summary$tagValue2,";"), 
                              function(x) x)
    
    user_score <- user_summary[[1]][1]
    BestFastTrack	<- user_summary[[1]][2]
    Cycle2oscillation	<- ifelse(user_summary[[1]][3]  == "true", 1, 0)
    EnoughFineTuningStimuli <- ifelse(user_summary[[1]][4]== "true", 1, 0)
    Champion <- ifelse(user_summary[[1]][5]== "true", 1, 0)
    Loser <- ifelse(user_summary[[1]][5]== "true", 1, 0)
   
    if (fastTrackLastBand == 1) {
      band <- c(1)
    } else {
      if (fastTrackLastBand == 54 && fast_track_1user[lastFastT,]$IsUserCorrect == 1) {
        band <- c(54)
      } else {
        band <- c(as.numeric(fastTrackLastBand) -1)
      }
    }
    
    if (band[1] != BestFastTrack) {
      print("Alert! The R-calculated and the Frinex calculated best fast track band differs:") 
      print(band[1])
      print(BestFastTrack)
      stop()
    }
    
    looserAttempt <- 0
    champAttempt <- 0
    i_band <- 1
    i_quad <- 1
    looser <- 0
    champion <- 0
    loop <- 0
    
    for (i in 1:nrow(fine_tuning_1user)){
      if (fine_tuning_1user[i,]$BandNumber > 0) {
        if (fine_tuning_1user[i,]$BandNumber != band[i_band]) {
          print("Alert for the implementation of the experiment: discrepance between the Frinex-detected and the R-calculated band, resp:")
          print(fine_tuning_1user[i,]$BandNumber)
          print(band[i_band])
          print("In row")
          print(i)
          stop()
        }
      }
      if (fine_tuning_1user[i,]$IsUserCorrect == 1) {
        if (i_quad == 4) { # the quadruple is finished correctly
          if (band[i_band] == 54)  { # champion candidate
            if (champAttempt == 1) { # champion, we should stop here
              if (i < nrow(fine_tuning_1user)) {
                print("Alert for the implementation of the experiment: champion is detected but the procedure does not stop")
                stop()
              } else {
                champion <- 1
              }
            } else {
              champAttempt <- 1 # one more session on the last band
              band <- c(band, 54)
              i_band<-  i_band + 1
              i_quad <- 1
            }
          } else { # one band uppe 
            band <- c(band, band[i_band]+1)
            i_band<-  i_band + 1
            i_quad <- 1
            champAttempt <- 0
            looserAttempt <- 0
          }
        } else { # not all the quadruple is checked
          i_quad <- i_quad + 1
        }
      } else { # incorrect answer
        # here we first must detect if there is a 2-loop of bands
        
        if (i_band>4 && band[i_band] == band[i_band-2] && band[i_band] == band[i_band-4] && band[i_band-1] == band[i_band-3])  {
          if (i < nrow(fine_tuning_1user)) {
            print("Alert for the implementation of the experiment: loopis detected but the procedure does not stop")
            stop()
          } else {
            loop <- 1
          }
        } else {
          if (band[i_band] == 1) { # looser candidate
            if (looserAttempt == 1) {
              if (i < nrow(fine_tuning_1user)) {
                print("Error in the implementation of the experiment: looser is detected but the procedure does not stop")
                stop()
              } else {
                looser <- 1
              }
            } else {
              looserAttempt <- 1 # onemore session on the first band 
              band <- c(band, 1)
              i_band<-  i_band + 1
              i_quad <- 1
            }
            
          } else {
            band <- c(band, band[i_band]-1)
            i_band<-  i_band + 1
            i_quad <- 1
            looserAttempt <- 0
            champAttempt <- 0
          }
        }
      }
    }
    
    if (looser != Loser) {
      print("Procedure omplementation alert: the R-calculated and Frinex detected looser-flags differ:")
      print(looser)
      print(Loser)
      stop()
    }
    if (champion != Champion) {
      print("Procedure omplementation alert: the R-calculated and Frinex detected champion-flags differ:")
      print(champion)
      print(Champion)
      stop()
    }
  
   if (loop != Cycle2oscillation) {
    print("Procedure omplementation alert: the R-calculated and Frinex detected oscillation-flags differ:")
    print(loop)
    print(Cycle2oscillation)
    stop()
   } else {
     if (loop == 1) {
       score <- min(band[i_band], band[i_band-1])
       if (score != user_score) {
         print("Procedure omplementation alert: the R-calculated and Frinex detected score differ:")
         print(score)
         print(user_score)
         stop()
       }
     }
  }
    
    # check EnoughFineTuningStimuli
    if (looser == 0 && champion == 0 && loop == 0) {
      if (EnoughFineTuningStimuli == 1) {
        print("Procedure omplementation alert, R-detection of looser, champ and loop are all FALSE")
        print("Which contradicts the TRUE detection of the EnoughFineTuningStimuli by FRINEX")
        stop()
      }
    }
    
    # Add start of experiment so duration of first item can be calculated
    combined_1_user$tagDate.y[1] <- fast_track_start
    
   
    # Calculate item durations
    combined_1_user$duration <- c(0, difftime(
      strptime(combined_1_user$tagDate.y[2:nrow(combined_1_user)], 
               format = "%Y-%m-%dT%H:%M:%OS+0000"),
      strptime(combined_1_user$tagDate.y[1:nrow(combined_1_user)-1], 
               format = "%Y-%m-%dT%H:%M:%OS+0000")
    ))
    combined_2_rounds <- rbind(combined_2_rounds, combined_1_user)
  }
  
  # Round duration to miliseconds
  combined_2_rounds$duration <- round(combined_2_rounds$duration, 3)
  
  
  # Change values for IsUserCorrect and UserAnswer from true/false to 1/0
  combined_2_rounds$UserAnswer <- ifelse(combined_2_rounds$UserAnswer == "JA, ik ken dit woord", 1, 0)
  
  
  # Select only required columns
  combined_2_rounds <- combined_2_rounds[,c("userId", "label", 
                                        "BandNumber", 
                                        "UserAnswer", "IsUserCorrect", 
                                        "duration", "eventTag",
                                        "NonwordsFrequencyAtThisPoint")]
  
  # Rename 'eventTag' column to 'phase'
  names(combined_2_rounds)[names(combined_2_rounds) == 'eventTag'] <- 'phase'
  
  # Add raw data for this user to complete item data
  item_data <- rbind(item_data, combined_2_rounds)
}

# Write item data to file
write.csv(item_data, file=paste0(experiment_abr,".item_data.csv"))


##--Get aggregates for each user----------------------------------------------##

user_scores  <- data.frame()

for (rawUUID in participants_uuids$V1)  {
  
  user <- paste0("uuid-", rawUUID)
  
  print(user)
  
  
  
  user_summary_r1 <- subset(tagpairevents_data, userId == user & 
                              eventTag == "user_summary" & 
                              tagValue1 == "row000001" & screenName =="stimuliScreenV1")
  user_summary_r2 <- subset(tagpairevents_data, userId == user & 
                              eventTag == "user_summary" & 
                              tagValue1 == "row000001" & screenName =="stimuliScreenV2")
  
  user_summary_r1 <- lapply(strsplit(user_summary_r1$tagValue2,";"), 
                            function(x) x)
  user_summary_r2 <- lapply(strsplit(user_summary_r2$tagValue2,";"), 
                            function(x) x)
  
  user_score_r1 <- user_summary_r1[[1]][1]
  BestFastTrack_r1	<- user_summary_r1[[1]][2]
  Cycle2oscillation_r1	<- user_summary_r1[[1]][3]
  EnoughFineTuningStimuli_r1 <- user_summary_r1[[1]][4]
  Champion_r1 <- user_summary_r1[[1]][5]
  Loser_r1 <- user_summary_r1[[1]][5]
                  
  user_score_r2 <- user_summary_r2[[1]][1]
  BestFastTrack_r2	<- user_summary_r2[[1]][2]
  Cycle2oscillation_r2	<- user_summary_r2[[1]][3]
  EnoughFineTuningStimuli_r2 <- user_summary_r2[[1]][4]
  Champion_r2 <- user_summary_r2[[1]][5]
  Loser_r2 <- user_summary_r2[[1]][5]
  
  fast_track_1user <- subset(fast_track, userId == user)
  
  
  fast_track_round1 <- subset(fast_track_1user, screenName == "stimuliScreenV1")
  fast_track_round2 <- subset(fast_track_1user, screenName == "stimuliScreenV2")
  
  last_fast_track_r1 <- fast_track_round1[nrow(fast_track_round1),]
  
  fast_track_r1_BandNumber <- lapply(strsplit(last_fast_track_r1$tagValue2,";"), 
                                     function(x) x[2])[[1]]
  
  switchOnNonWord_r1 <- ifelse(fast_track_r1_BandNumber == nonWordValue, 1, 0)
  
  last_fast_track_r2 <- fast_track_round2[nrow(fast_track_round2),]
  
  fast_track_r2_BandNumber <- lapply(strsplit(last_fast_track_r2$tagValue2,";"), 
                                     function(x) x[2])[[1]]
  
  switchOnNonWord_r2 <- ifelse(fast_track_r2_BandNumber == nonWordValue, 1, 0)
  
  new_user_scores <- data.frame(user, 
                                user_score_r1, 
                                BestFastTrack_r1,
                                Cycle2oscillation_r1, 
                                EnoughFineTuningStimuli_r1, 
                                Champion_r1, Loser_r1,
                                switchOnNonWord_r1,
                                user_score_r2, 
                                BestFastTrack_r2,
                                Cycle2oscillation_r2, 
                                EnoughFineTuningStimuli_r2, 
                                Champion_r2, Loser_r2,
                                switchOnNonWord_r2
  )
  
  names(new_user_scores) <- c("user",  
                              "score_R1", 
                              "BestFastTrack_R1",
                              "Cycle2oscillation_R1", 
                              "EnoughFineTuningStimuli_R1", 
                              "Champion_R1", "Loser_R1",
                              "switchOnNonWord_R1",
                              "score_R2", 
                              "BestFastTrack_R2",
                              "Cycle2oscillation_R2", 
                              "EnoughFineTuningStimuli_R2", 
                              "Champion_R2", "Loser_R2",
                              "switchOnNonWord_R2")
  
  user_scores <- rbind(user_scores, new_user_scores)  
}

user_scores$Cycle2oscillation_R1 <- ifelse(
  user_scores$Cycle2oscillation_R1 == "true", 1, 0)
user_scores$Cycle2oscillation_R2 <- ifelse(
  user_scores$Cycle2oscillation_R2 == "true", 1, 0)

user_scores$EnoughFineTuningStimuli_R1 <- ifelse(
  user_scores$EnoughFineTuningStimuli_R1 == "true", 1, 0)
user_scores$EnoughFineTuningStimuli_R2 <- ifelse(
  user_scores$EnoughFineTuningStimuli_R2 == "true", 1, 0)
user_scores$Champion_R1 <- ifelse(user_scores$Champion_R1 == "true", 1, 0)
user_scores$Champion_R2 <- ifelse(user_scores$Champion_R2 == "true", 1, 0)
user_scores$Loser_R1 <- ifelse(user_scores$Loser_R1 == "true", 1, 0)
user_scores$Loser_R2 <- ifelse(user_scores$Loser_R2 == "true", 1, 0)

write.csv(user_scores, file=paste0(experiment_abr,".user_scores.csv"))
