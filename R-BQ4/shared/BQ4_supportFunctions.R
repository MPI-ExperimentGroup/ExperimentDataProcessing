library(dplyr)


contains_prescriptivegrammar <- function(data_table, record) {
  if (nrow(data_table) == 0) {
    return(-1)
  }
  for (i in 1:nrow(data_table)) {
    row <- data_table[i,]
    if (row["tagValue1"] ==  record["tagValue1"]) {
      if (row["eventTag"] == record["eventTag"]) {
        # duplicated event with the same stimuli tagValue1 detected
        return(i)
      }
      if (((row["eventTag"] == "Correct") && (record["eventTag"] == "Incorrect")) || ((row["eventTag"] == "Incorrect") && (record["eventTag"] == "Correct"))){
        #  reaction evaluation events (but different sorts of reaction) also counts as duplicated row
        #  we will pick up the latermost reaction 
        return(i)
      }
    }
 
  }
  return(-1)
}

contains_questhealth <- function(data_table, record) {
  if (nrow(data_table) == 0) {
    return(-1)
  }
  for (i in 1:nrow(data_table)) {
    row <- data_table[i,]
    if (row["tagValue1"] ==  record["tagValue1"]) {
        return(i)
      }
  }
  return(-1)
}
        
keep_latest <- function(data_table, contains_instance) {
  if (nrow(data_table) == 0) {
    return(data_table)
  }
  contains = match.fun(contains_instance)
  
  retVal <- data.frame()
  for (i in 1:nrow(data_table)) {
    row <- data_table[i,]
    ind <- contains(retVal, row)
    if (ind<0) {
      retVal <- rbind(retVal, row)
    } else {
      if (row["submitDate"] > retVal[ind,]["submitDate"]) {
        retVal[ind,] <-  row
      } 
    }
  }
  return(retVal)
}

## get_embedded() gets experiment data from JSON response for a specific data 
## type

get_embedded <- function(data_type, columns, admin_url, size=0){
  admin_url <- paste(admin_url, data_type, sep = "/")
  url <- admin_url
  if (size > 0){
    url <- paste0(admin_url,"?size=",as.character(size))
  }
  
  print(url)
  req <- httr::GET(url)
  contentFirstPage <- fromJSON(httr::content(req, as = "text"))
  #print(contentFirstPage)
  #totalPages = 5
  totalPages <- contentFirstPage[["page"]][["totalPages"]]
  #columns <- c("userId", "tagValue1")
  currentData <- contentFirstPage[["_embedded"]][[data_type]][,columns]
  pagesRead <- 1
  if(!is.null(totalPages)){
    if(totalPages > 1){
      for(page in 1:totalPages-1){
        pageUrl <- paste(admin_url, "?page=", as.character(page), sep = "")
        if(size > 0){
          pageUrl <- paste0(pageUrl,"&size=",as.character(size))
        }
        print(pageUrl)
        req <- httr::GET(pageUrl)
        content <- fromJSON(httr::content(req, as = "text"))
        newData <- content[["_embedded"]][[data_type]][,columns]
        currentData <- bind_rows(currentData, newData)
        pagesRead <- pagesRead + 1
      }
    } 
  }  
  
  #return(content[["_embedded"]][[data_type]])
  return(list(currentData=currentData,contentFirstPage=contentFirstPage))
}



get_birthday <- function(userId, birthday_url){
  birthday_url <- paste(birthday_url, userId, sep = "?uuid=")
  print(birthday_url)
  req <- httr::GET(birthday_url)
  content <- fromJSON(httr::content(req, as = "text"))
  currentData <- content[["Birthday"]]
  return(currentData)
}


## get_two_oscillations() returns the number of times a sequence of two full
## oscillations occurs

get_two_oscillations <- function(bandnumbers){
  oscillation_low <- NA
  two_oscillations <- 0
  i <-  1
  previous <-  0
  for(bandnr in bandnumbers){
    if(i > 4){
      j <- i - 2
      #print(bandnr)
      #print(bandnumbers[j])
      #print("")
      
      # check for two oscillations:
      # (bandnr == bandnumbers[i-2] & bandnr == bandnumbers[i-4] & 
      # previous == bandnumbers[i-3]), 
      #
      # but not three:
      # !(bandnr == bandnumbers[i+2] & previous == bandnumbers[i+1]) &
      # !(bandnr == bandnumbers[i-6] & previous == bandnumbers[i-5]), 
      #
      # and don't count two and a half oscillations double:
      # !(previous == bandnumbers[i-3] & previous == bandnumbers[i-5] &
      # bandnr == bandnumbers[i-2] & bandnr == bandnumbers[i-4])
      
      if(bandnr == bandnumbers[i-2] && bandnr == bandnumbers[i-4] && #(
         previous == bandnumbers[i-3] #||
         #(previous == "-1" && as.numeric(bandnumbers[i-3]) == as.numeric(bandnr)+1) ||
         #(as.numeric(previous) == as.numeric(bandnr)+1 && bandnumbers[i-3] == "-1")
         #) 
         && 
         
         #!(bandnr == bandnumbers[i+2] & previous == bandnumbers[i+1]) &
         #!(bandnr == bandnumbers[i-6] & previous == bandnumbers[i-5]) &
         
         # check for double counting 2.5 oscillations, only if i > 6
         (i == 5 || (
           !(previous == bandnumbers[i-3] && previous == bandnumbers[i-5] &&
             bandnr == bandnumbers[i-2] && bandnr == bandnumbers[i-4])))
         #) #|
         # Check for oscillations with bands consisting of a single -1 band
         #(bandnr == bandnumbers[i-1] & bandnr == bandnumbers[i-3])
      )
      {
        print("Found minimum of two oscilations")
        if (is.na(oscillation_low)){
          oscillation_low <- min(bandnr, previous)
          print(paste0("Lowest bandnr in oscillation: ", as.character(oscillation_low)))
        }
        two_oscillations <- two_oscillations + 1
        print(paste(as.character(i), 
                    as.character(bandnr), as.character(previous), 
                    as.character(bandnumbers[i-2]), as.character(bandnumbers[i-3]), 
                    as.character(bandnumbers[i-4]),sep = ", "))
      }
    }
    # If (i > 5) but not (i > 6) than a check for double counting two and a half
    # oscillations is not possible/necessary
    
    previous = bandnr
    i <- i + 1
  }
  score <- ifelse(!is.na(oscillation_low),oscillation_low,bandnr)
  return(list(two_oscillations = two_oscillations, score = score))
}

## get_two_oscillations_with_error() returns the number of times a sequence of
## two full oscillations occurs, but only followed by an error

get_two_oscillations_with_error <- function(bandnumbers){
  
  oscillation_low <- NA
  two_oscillations <- 0
  i <-  1
  previous <-  0
  for(bandnr in bandnumbers[,1]){
    if(i > 4){
      j <- i - 2
      print(bandnr)
      #print(paste0("next: ",bandnumbers[i+1]))
      next_bandnr <- ifelse(i > nrow(bandnumbers), 1000, 
                            as.numeric(bandnumbers[i+1]))
      #print(bandnumbers[j])
      #print("")
      
      # check for two oscillations:
      # (bandnr == bandnumbers[i-2] & bandnr == bandnumbers[i-4] & 
      # previous == bandnumbers[i-3]), 
      #
      # but not three:
      # !(bandnr == bandnumbers[i+2] & previous == bandnumbers[i+1]) &
      # !(bandnr == bandnumbers[i-6] & previous == bandnumbers[i-5]), 
      #
      # and don't count two and a half oscillations double:
      # !(previous == bandnumbers[i-3] & previous == bandnumbers[i-5] &
      # bandnr == bandnumbers[i-2] & bandnr == bandnumbers[i-4])
      print(paste0("is user correct: ",bandnumbers[i,2]))
      print(paste0("next bandnr: ", next_bandnr))
      print(bandnumbers[i,2] == "false") # next_bandnr == as.numeric(bandnr)-1  || 
      if(bandnr == bandnumbers[i-2] && bandnr == bandnumbers[i-4] && #(
         previous == bandnumbers[i-3] && 
         (bandnumbers[i,2] == "false") # next_bandnr == as.numeric(bandnr)-1  || 
         #(previous == "-1" && as.numeric(bandnumbers[i-3]) == as.numeric(bandnr)+1) ||
         #(as.numeric(previous) == as.numeric(bandnr)+1 && bandnumbers[i-3] == "-1")
         #) 
         && 
         
         #!(bandnr == bandnumbers[i+2] & previous == bandnumbers[i+1]) &
         #!(bandnr == bandnumbers[i-6] & previous == bandnumbers[i-5]) &
         
         # check for double counting 2.5 oscillations, only if i > 6
         # 
         # Problem: This check is now too strict, because it also finds previous
         # oscillations that don't end in an error
         #
         (i == 5 || (
           !(previous == bandnumbers[i-3] && previous == bandnumbers[i-5] &&
             bandnr == bandnumbers[i-2] && bandnr == bandnumbers[i-4] &&
             bandnumbers[i-1,2] == "false")
           ))
         #) #|
         # Check for oscillations with bands consisting of a single -1 band
         #(bandnr == bandnumbers[i-1] & bandnr == bandnumbers[i-3])
      )
      {
        print("Found minimum of two oscilations")
        if (is.na(oscillation_low)){
          oscillation_low <- min(bandnr, previous)
          print(paste0("Lowest bandnr in oscillation: ", as.character(oscillation_low)))
        }
        two_oscillations <- two_oscillations + 1
        print(paste(as.character(i), 
                    as.character(bandnr), as.character(previous), 
                    as.character(bandnumbers[i-2]), as.character(bandnumbers[i-3]), 
                    as.character(bandnumbers[i-4]),sep = ", "))
      }
    }
    # If (i > 5) but not (i > 6) than a check for double counting two and a half
    # oscillations is not possible/necessary
    
    previous = bandnr
    i <- i + 1
  }
  score <- ifelse(!is.na(oscillation_low),oscillation_low,bandnr)
  return(list(two_oscillations = two_oscillations, score = score))
}

## remove second duplicate empty rows

remove_second_empty <- function(raw_bandnumbers){
  previous <- ""
  for(row in 1:nrow(raw_bandnumbers)){
    bandnr <- raw_bandnumbers[row,]$BandNumber[[1]]
    #print(bandnr)
    if(!is.null(bandnr) && bandnr == previous && bandnr == " "){
      #print(bandnr)
      #print(i)
      raw_bandnumbers <- raw_bandnumbers[-row,]
    }else{
      previous = bandnr
      #i = i + 1
    }  
  }
  return(raw_bandnumbers)
}

## reduce_to_single() reduces bandnrs to one value. Values of -1 for bands 
## consisting of a single non-word are corrected to their true bandnr.

reduce_to_single <- function(raw_bandnumbers, add_isUserCorrect = FALSE){
  bandnumbers <- as.vector(c())
  set <- c()
  previous_isUserCorrect <- ""
  last_isUserCorrect <- ""
  for(row in 1:nrow(raw_bandnumbers)){
    bandnr <- raw_bandnumbers[row,]$BandNumber[[1]]
    if(!bandnr %in% c(" ", "BandNumber")){
      set <- rbind(set, bandnr) 
      # remember last value for isUserCorrect in current band
      last_isUserCorrect <- raw_bandnumbers[row,]$IsUserCorrect[[1]]
    }else if (!is.null(set)){
      corrected_bandnr <- max(set)
      #print(corrected_bandnr)
      #print(previous_isUserCorrect)
      if(corrected_bandnr == "-1" && previous_isUserCorrect == "true"){
        #print("setting to higher correct bandnr")
        #print(as.numeric(bandnumbers[nrow(bandnumbers),1])+1)
        corrected_bandnr <- as.numeric(bandnumbers[nrow(bandnumbers),1])+1
      }else if (corrected_bandnr == "-1" && previous_isUserCorrect == "false"){
        #print("setting to lower correct bandnr")
        #print(as.numeric(bandnumbers[nrow(bandnumbers),1])-1)
        corrected_bandnr <- as.numeric(bandnumbers[nrow(bandnumbers),1])-1
      }
      # Special case for first band in fine tuning that consists of only a single
      # non-word.
      else if (corrected_bandnr == "-1" && previous_isUserCorrect == ""){
        # Set bandnr to next bandnr + 1
        #print("setting correct bandnr for first band in fine tuning")
        corrected_bandnr <- as.numeric(
          subset(fine_tuning_1user, as.numeric(BandNumber) > 0)[1,]$BandNumber[[1]]
        ) + 1
      }
      if(add_isUserCorrect){
        #print(raw_bandnumbers[row-1,2][[1]])
        bandnumbers <- rbind(bandnumbers, 
                             c(corrected_bandnr, 
                               raw_bandnumbers[row-1,2][[1]]))
      }else{
        bandnumbers <- rbind(bandnumbers, corrected_bandnr)
      }
      set <- as.vector(c())
      
      # Set last value for isUserCorrect in previous band
      previous_isUserCorrect <- last_isUserCorrect
    }
  }
  return(bandnumbers)
}




# Get test duration from screenviews of one user
# By default, the function assumes that the start date has a screenName that
# starts with "stimuliScreen". This works for the auteurs- and spellingtest,
# but not for the other test, which overrides these defaults.
# By default, the function assumes thta the end date has a screen name "Admin",
# but it is so for the test with practice rounds, when results of the practice rounds
# have to be taken in separate tables.

get_test_duration <- function(screenviews_1user, 
                              startScreenString="stimuliScreen",
                              startScreenMaxChars=13,
                              endScreen="Admin",
                              sec=FALSE){
  
  # Get test duration from submitDates in screenviews
  start_submit_date <- subset(screenviews_1user, 
                              substring(screenName, 1, startScreenMaxChars) == 
                                startScreenString)[3]
  start_submit_date <- start_submit_date[1,]
  
  end_submit_date <- subset(screenviews_1user, 
                            screenName == endScreen)  [3] 
  
  end_submit_date <- end_submit_date[1,]
  
  startTime <- strptime(substring(start_submit_date, 1, nchar(start_submit_date)-9), 
                        format = "%Y-%m-%dT%H:%M:%S")
  endTime <- strptime(substring(end_submit_date, 1, nchar(end_submit_date)-9), 
                      format = "%Y-%m-%dT%H:%M:%S")
  
  if (sec) {
    startTime_ms <- as.numeric(startTime)
    endTime_ms <- as.numeric(endTime)
    testDuration <- endTime_ms-startTime_ms
  } else {
    testDuration <- round(as.numeric(endTime-startTime), 2)
  }
  
  
  return(list(startTime=startTime, endTime=endTime, testDuration=testDuration))
}


# Calculate age based on date of birth and date of experiment

get_age <- function(dateOfBirth, date){
  age_days <- date - dateOfBirth 
  age 
  return(age)
}

# Convert an age to a column in the peabody_dat table

age_to_column <- function(age){
  age <- floor(age*2)/2
  if(age > 51){
    age = 51
  }
  age_column <- "X"
  if((age %% 1) == 0.5 && age < 40){
    age_column <- paste0(age_column,as.character(floor(age)),".6")
    age_half_round <- paste0(as.character(floor(age)),";6")
  }else{
    age_column <- paste0(age_column,as.character(floor(age)),".0")
    age_half_round <- paste0(as.character(floor(age)),";0")
  }
  return(c(age_column, age_half_round))
}