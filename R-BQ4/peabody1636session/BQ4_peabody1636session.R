##--Peabody NL----------------------------------------------------------------##

library(jsonlite)
library(httr)
library(dplyr)
library(tidyr)
source("../shared/BQ4_supportFunctions.R")


## parameters (specification) --------------------------------------------------####

start_band <- 13
next_band_after_base_is_done <- 14
min_band <- 1
max_band <- 17
pictures_per_band <-  12
max_admissible_errors <- 8
base_max_admissible_errors <- 4


# read stimuli from the csv file
stimuli_csv <- read.table("stimuli.csv",  header=TRUE, sep=";")



##--Configuration-------------------------------------------------------------##

start_screen <- "stimuliScreenBase13_13" 
end_screen <- "Admin"
experiment_abr <- "peabody1636session"
server_url <- "http://frinexproduction.mpi.nl"

user_birthday_url= "https://www.mpi.nl/dbmpi/plone/bigQuestion/nph-bq_show_birthday.pl"

admin_url <- paste(server_url, "/", experiment_abr, "-admin", sep = "")

peabody_dat = read.csv("Peabody.scores.csv", header = TRUE)
peabody_percent = read.csv("Peabody.percentiles.csv", header = TRUE)


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

participants_columns <- c("userId", "workerId", "staleCopy", "submitDate")

participants <- get_embedded("participants", participants_columns, admin_url)

participants_data <- subset(participants[["currentData"]], 
                            staleCopy == FALSE)

tagpairevents_columns <- c("userId", "screenName", "submitDate", "eventTag",   "tagValue1", "tagValue2", "dataChannel")
tagpairevents <- get_embedded("tagpairevents", tagpairevents_columns, admin_url, 1000)
tagpairevents_data <- unique(tagpairevents[["currentData"]])

screenviews_columns <- c("userId", "screenName", "viewDate")
screenviews <- get_embedded("screenviews", screenviews_columns, admin_url)
screenviews_data <- unique(subset(screenviews[["currentData"]]))

stimulus_resp_columns <- c("userId", "screenName", "stimulusId", "response", "isCorrect", "tagDate")
stimulusresponses <- get_embedded("stimulusresponses", stimulus_resp_columns, admin_url, 1000)
stimulusresponses_data <- unique(subset(stimulusresponses[["currentData"]], !is.na(isCorrect))) 


##--Get aggregates for each user----------------------------------------------##

## Loop through participants, and calculate age in half years, raw score, 
## wbq, and percentile based on age and raw score

user_scores <- data.frame()

for (rawUUID in participants_uuids$V1){
  
  user <- paste0("uuid-", rawUUID)
  print(user)
  
  stimulusresponses_user <- subset(stimulusresponses_data, userId==user)
  stimulusresponses_user <- stimulusresponses_user[order(stimulusresponses_user$tagDate), ]
  
  band <- start_band
  picture_counter <- 0
  n_errors <-0
  n_errors_current_band <- 0
  n_correct <-0
  n_correct_current_band <-0
  base <- TRUE
  
  for (i in 1:nrow(stimulusresponses_user)) {
    
    if (picture_counter == pictures_per_band) {  # start new band
      
      if (n_correct_current_band + n_errors_current_band != pictures_per_band) {
        print("Sanity Error: the sum of correct and erroenous answer for the band is not equal to the overall amount of pictures in the band.")
        print(stimulusresponses_user[i,]$screenName)
        print(n_correct_current_band)
        print(n_errors_current_band)  
        print("Check if there were duplicate submissions for this user")
        stop()
      }
      
      
      if (base == TRUE) { # base 
        
        if (n_errors_current_band <= base_max_admissible_errors) { # go to the main run
          band <- next_band_after_base_is_done
          base <- FALSE
        } else {
          if (band == min_band) {
            print("Sanity Error: the procedure should have stopped here because the critical amount of errors has been made")
            print(stimulusresponses_user[i,]$screenName)
            print(band)
            stop()
          } else {
            band <- band - 1 
          }
        }
       
      } else { # main run
        if (n_errors_current_band <= max_admissible_errors) { # go to the upper band
          if (band == max_band) {
            print("Sanity Error: the procedure should have stopped here because the maximal band have been reached")
            print(stimulusresponses_user[i,]$screenName)
            print(band)
            stop()
          } else {
            band <- band + 1 
          }
        } else { # stop
          print("Sanity Error: the procedure should have stopped here because the critical amount of errors has been made")
          print(stimulusresponses_user[i,]$screenName)
          print(band)
          stop()
        }
      }
    
      picture_counter <- 1 # current picture
      n_errors_current_band <- 0
      n_correct_current_band <- 0
    } else {
      picture_counter <- picture_counter+1 # current picture
    }
    
    
    if (base == TRUE) {
      tmp <- strsplit(stimulusresponses_user[i,]$screenName, "_")
      screenNumber  <- as.numeric(lapply(tmp, function (x) trimws(x[2])))
    } else {
      l <- nchar(stimulusresponses_user[i,]$screenName)
      screenNumber  <- as.numeric(substring(stimulusresponses_user[i,]$screenName, l-1, l))
    }
    if (screenNumber != band) {
      print("Sanity Error: presenter encoding does not correspond to the band number to be presented now:")
      print(stimulusresponses_user[i,]$screenName)
      print(band)
      stop()
    }
  
    currentStimulusId <- stimulusresponses_user[i,]$stimulusId
    intermediate_helper<-strsplit(currentStimulusId, "_")
    
    set <- lapply(intermediate_helper, function (x) trimws(x[1]))
    set<- as.numeric(substr(set,4, nchar(set)))
    
    if (set != band){
      print("Sanity Error: stimulus does not correspond to the band number to be presented now:")
      print(currentStimulusId)
      print(band)
      stop()
    }
    
     
    stimulusTableName <- lapply(intermediate_helper, function (x) trimws(x[1]))
    for (j in 2:4) {
      stimulusTableName <- paste0(stimulusTableName,"_", lapply(intermediate_helper, function (x) trimws(x[j])))
    }
   
    stimulusTable <- subset(stimuli_csv, Picture == paste0(stimulusTableName, ".png"))
    
    if (stimulusresponses_user[i,]$response == stimulusTable[1,]$Correct.Answer) {
      if (stimulusresponses_user[i,]$isCorrect == TRUE) {
        n_correct <-  n_correct  + 1
        n_correct_current_band <- n_correct_current_band + 1
      } else {
        print("Frinex sanity error: correct answer is marked as incorrect")
        print(currenStimulusId)
        stop()
      }
    } else {
      if (stimulusresponses_user[i,]$isCorrect == FALSE) {
        n_errors <- n_errors  + 1
        n_errors_current_band <- n_errors_current_band + 1
      } else {
        print("Frinex sanity error: incorrect answer is marked as correct")
        print(currenStimulusId)
        stop()
      }
    }
    
  }
  
  
  
  stimulusresponses_user$stimulus_nr <- lapply(strsplit(stimulusresponses_user$stimulusId,"_"), 
                                            function(x) x[5])
  
  summary_1user <- subset(tagpairevents_data, 
                          userId == user &
                          eventTag == "summary" & 
                            substring(tagValue2, 1, 11) == "totalErrors")
  
  
  # Get start and end of experiment from screenViews
  start_date <- max(subset(screenviews_data, 
                           userId == user & 
                             screenName == start_screen)[,c("viewDate")])
  
  end_date <- max(subset(screenviews_data, 
                         userId == user & 
                           screenName == end_screen)[,c("viewDate")])
  
  startTime <- strptime(substring(start_date, 1, nchar(start_date)-9), 
                        format = "%Y-%m-%dT%H:%M:%S")
  endTime <- strptime(substring(end_date, 1, nchar(end_date)-9), 
                      format = "%Y-%m-%dT%H:%M:%S")
  
  testDuration <- round(as.numeric(endTime -startTime), 1)
  
  print("startTime")
  print(startTime)
  print("endTime")
  print(endTime)
  print("test duration")
  print(paste0(as.character(testDuration), " minutes"))
  
  # Get highest stimulus number
  stimulus_nrs <- stimulusresponses_user$stimulus_nr
  
  highest_stimulus_nr <- as.numeric(sort(unlist(stimulus_nrs), 
                                         decreasing = TRUE)[1])
  print(paste0("highest stim nr: ",as.character(highest_stimulus_nr)))
  
  # Get total number of errors from summary
  totalErrors <- as.numeric(substring(summary_1user$tagValue2,
                                      nchar(summary_1user$tagValue2)-2+1))
  
  totalErrors_manual <- nrow(subset(stimulusresponses_user, isCorrect == FALSE))
  
  if (totalErrors_manual != n_errors) {
    print("Sanity Error: total amount of 'manual errors' different from total amount of errors counted by check-up R procedure")
    print(totalErrors_manual)
    print(n_errors)
    stop()
  }
  
  
  print("Total errors: ")
  print(totalErrors)
  
  print("Total errors manual: ")
  print(totalErrors_manual)
  
  # Calculate raw score from highest stimulus number and manual calculation of
  # total number of errors
  raw_score <- highest_stimulus_nr - totalErrors_manual
  print(paste0("Raw score: ",as.character(raw_score)))
  #print(raw_score)
  
  # Calculate age from date of birth and test date
  dateOfBirthRaw  <-  get_birthday(rawUUID, user_birthday_url)
  dateOfBirth <- as.Date(dateOfBirthRaw, format = "%Y-%m-%d")
  
  participant <- unique(subset(participants_data,  userId == user))
  
  submitDateRaw <- participant$submitDate 
  
  submitDate <- as.Date(substring(submitDateRaw, 1, 10), format = "%Y-%m-%d")
  
  age <- as.numeric(submitDate - dateOfBirth)
  age <- round(age/365.25, 1)
  print(paste0("Age: ",as.character(age)))
  #print(age)
  #print(age_to_column())
  
  # Get peabody score from table based on raw score
  peabody_row <- raw_score - 98
  print("Peabody row: ")
  print(peabody_row)
  age_converted <- age_to_column(as.numeric(age))
  peabody_column <- age_converted[1]
  age_half_rounded <- age_converted[2]
  print(paste0("Peabody column: ",peabody_column))
  #print(peabody_column)
  
  if(length(peabody_row) != 0){
    if(peabody_row > 0){
      temp <- quote(peabody_column)
      #temp <- quote("X16.0")
      #print(temp)
      peabody_score <- peabody_dat[peabody_row , eval(temp)]
      print("Peabody score:")
      print(peabody_score)
      #print(peabody_score)
    }else{
      # Can't get score with negative row number
      peabody_score <- NA
    }
  }else{
    # Can't get score without row number
    peabody_score <- NA
  }
  if(length(peabody_score) == 0){peabody_score <- NA}
  
  # Get percentile based on peabody_score (= WBQ)
  if(!is.na(peabody_score)){
    if(peabody_score < 61){
      percentile <- 0
    }else if(peabody_score > 139){
      percentile <- 100
    }else{
      percentile <- subset(peabody_percent, wbq == peabody_score)$percentile 
    }
  }else{
    percentile <- NA
  }
  
  # Gather scores for file output
  
  if(length(highest_stimulus_nr) == 0){highest_stimulus_nr <- NA}
  if(length(totalErrors) == 0){totalErrors <- NA}
  if(length(raw_score) == 0){raw_score <- NA}
  
  new_user_scores <- data.frame(rawUUID, highest_stimulus_nr, 
                                totalErrors, totalErrors_manual,
                                raw_score, age_half_rounded, peabody_score, 
                                percentile, testDuration)
  names(new_user_scores) <- c("userId", 
                              "Highest Stimulus Number",
                              "Total Errors",
                              "Total Errors manual",
                              "Raw Scores",
                              "Age Category",
                              "Peabody Score",
                              "Percentile",
                              "Test Duration (mins)")
  user_scores <- rbind(user_scores, new_user_scores)
  
}

# Write scores to file
write.csv(user_scores, file=paste0(experiment_abr,".peabody.user_scores.csv"))