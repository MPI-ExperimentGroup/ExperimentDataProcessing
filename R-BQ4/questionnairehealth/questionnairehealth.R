##--Prescriptive Grammar------------------------------------------------------##


library(jsonlite)
library(httr)
library(dplyr)
library(tidyr)
source("shared/BQ4_supportFunctions.R")

##--Configuration-------------------------------------------------------------##

experiment_abr <- "questionnairehealth"
server_url <- "http://frinexproduction.mpi.nl"
admin_url <-
  paste(server_url, "/", experiment_abr, "-admin", sep = "")#auth_file <- "../login.questionnairehealth.csv"



##--Authentication------------------------------------------------------------##

## Read username/password from file and authenticate on Admin interface

auth_values <-
  read.table(paste("login.", experiment_abr, ".csv", sep = ""),
             stringsAsFactors = FALSE)

auth_body <-
  list(username = auth_values$username[1],
       password = auth_values$password[1])

login_url <- paste(admin_url, "login", sep = "/")
req_auth <- httr::POST(login_url, body = auth_body)

###--Get required data from an experiment------------------------------------###

participants_uuids <- read.csv("shared/participants.csv", header = FALSE)

participants_columns <-
  c("userId", "workerId", "staleCopy", "submitDate")
participants <-
  get_embedded("participants", participants_columns, admin_url)
participants_data <- unique(subset(participants[["currentData"]],
                                   staleCopy == FALSE))

tagpairevents_columns <- c("userId",
                           "screenName",
                           "eventTag",
                           "tagValue1",
                           "tagValue2",
                           "submitDate")
tagpairevents <-
  get_embedded("tagpairevents", tagpairevents_columns, admin_url)
tagpairevents_data <- unique(tagpairevents[["currentData"]])

screenviews_columns <- c("userId", "screenName", "submitDate")
screenviews <-
  get_embedded("screenviews", screenviews_columns, admin_url)
screenviews_data <- unique(screenviews[["currentData"]])

participant_events <- left_join(tagpairevents[["currentData"]],
                                participants_data,
                                by = "userId")


###--Process responses and aggregate scores----------------------------------###

user_scores <- data.frame()
user_responses <- data.frame()


for (user in participants_uuids$V1) {
   
    user <- paste0("uuid-", user)
    print(user)
    print("Num events for user:")
    
    events_tmp <- subset(tagpairevents_data,
                     userId == user & eventTag != "DataSubmission")
    
    events <- keep_latest(events_tmp, "contains_questhealth")
    
    events <- subset(events,  select=-c(submitDate, screenName, eventTag))
    
    print(nrow(events))
    
   
    new_user_response <- events
    
    new_user_response <-
      spread(new_user_response, tagValue1, tagValue2)
    
  
    user_responses <- rbind(user_responses, new_user_response)
    
  
}


user_responses[] <- lapply(user_responses, as.character)


write.csv(user_responses, file = paste0(experiment_abr, ".item_data.csv"))
