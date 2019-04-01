##--Peabody NL----------------------------------------------------------------##

library(jsonlite)
library(httr)
library(dplyr)
library(tidyr)
source("shared/BQ4_supportFunctions.R")

##--Configuration-------------------------------------------------------------##


experiment_abr <- "peabody1636session"
server_url <- "http://frinexproduction.mpi.nl"

user_birthday_url= "https://www.mpi.nl/dbmpi/plone/bigQuestion/nph-bq_show_birthday.pl?"

admin_url <- paste(server_url, "/", experiment_abr, "-admin", sep = "")

peabody_dat = read.csv("peabody1636session/Peabody.scores.csv", header = TRUE)
peabody_percent = read.csv("peabody1636session/Peabody.percentiles.csv", header = TRUE)


##--Authentication------------------------------------------------------------##

## Read username/password from file and authenticate on Admin interface

auth_values <- read.table(paste("login.", experiment_abr, ".csv", sep = ""), 
                          stringsAsFactors = FALSE)

auth_body <-
  list(
    username=auth_values$username[1],
    password=auth_values$password[1]
  ) 

login_url <- paste(admin_url, "login", sep = "/")
req_auth <- httr::POST(login_url, body = auth_body)

##--Main----------------------------------------------------------------------##
participants_columns <- c("userId", "workerId", "staleCopy", "submitDate")

participants <- get_embedded("participants", participants_columns, admin_url)

participants_data <- subset(participants[["currentData"]], 
                            staleCopy == FALSE)

tagpairevents_columns <- c("userId", "screenName", "submitDate", "eventTag", 
                           "tagValue1", "tagValue2", "dataChannel")
tagpairevents <- get_embedded("tagpairevents", tagpairevents_columns, admin_url, 1000)

tagpairevents_data <- unique(tagpairevents[["currentData"]])

tagpairevents_data <- subset(tagpairevents_data, dataChannel == 2)

screenviews_columns <- c("userId", "screenName", "viewDate")
screenviews <- get_embedded("screenviews", screenviews_columns, admin_url)
screenviews_data <- unique(subset(screenviews[["currentData"]]))


##--Get aggregates for each user----------------------------------------------##

## Loop through participants, and calculate age in half years, raw score, 
## wbq, and percentile based on age and raw score

user_scores <- data.frame()

for (participant in 1:nrow(participants_data)){
  
  print(participants_data[participant,"userId"])
  
  # Get tagpairevents for participant
  tagpairevents_1user <- subset(tagpairevents_data, 
                                userId == participants_data[participant,"userId"] &
                                  eventTag == "StimulusImageShown")
  
  tagpairevents_1user$stimulus_nr <- lapply(strsplit(tagpairevents_1user$tagValue1,"_"), 
                                            function(x) x[5])
  summary_1user <- subset(tagpairevents_data, 
                          userId == participants_data[participant,"userId"] &
                            eventTag == "summary" & 
                            substring(tagValue2, 1, 11) == "totalErrors")
  
  
  # Get start and end of experiment from screenViews
  start_date <- max(subset(screenviews_data, 
                           userId == participants_data[participant,"userId"] & 
                             substring(screenName, 1, 24) == "stimuliScreenPeabodyBase")[,c("viewDate")])
  
  end_date <- max(subset(screenviews_data, 
                         userId == participants_data[participant,"userId"] & 
                           screenName == "Admin")[,c("viewDate")])
  
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
  stimulus_nrs <- tagpairevents_1user$stimulus_nr
  
  highest_stimulus_nr <- as.numeric(sort(unlist(stimulus_nrs), 
                                         decreasing = TRUE)[1])
  print(paste0("highest stim nr: ",as.character(highest_stimulus_nr)))
  
  # Get total number of errors from summary
  totalErrors <- as.numeric(substring(summary_1user$tagValue2,
                                      nchar(summary_1user$tagValue2)-2+1))
  
  totalErrors_manual <- nrow(unique(
    subset(tagpairevents_data, 
           userId == participants_data[participant,"userId"] & 
             tagValue2 == "incorrect")[,c("tagValue1", "tagValue2")]
  )
  )
  
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
  dateOfBirthRaw  =  screenviews <- get_birthday(user, user_birthday_url)
  
  dateOfBirth <- as.Date(dateOfBirthRaw, format = "%d/%m/%Y")
  
  submitDate <- as.Date(substring(participants_data[participant,
                                                    "submitDate"], 1, 10), 
                        format = "%Y-%m-%d")
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
  userId <- participants_data[participant,"userId"]
  userName <- participants_data[participant,"workerId"]
  if(length(highest_stimulus_nr) == 0){highest_stimulus_nr <- NA}
  if(length(totalErrors) == 0){totalErrors <- NA}
  if(length(raw_score) == 0){raw_score <- NA}
  
  new_user_scores <- data.frame(userId, userName, highest_stimulus_nr, 
                                totalErrors, totalErrors_manual,
                                raw_score, age_half_rounded, peabody_score, 
                                percentile, testDuration)
  names(new_user_scores) <- c("userId", "userName",
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