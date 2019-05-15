library(dplyr)

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

# Olha: why plus 1?
# what is the idea
# is it what florian needs?
get_two_oscillations_with_error <- function(bandnumbers){
  
  oscillation_low <- NA
  two_oscillations <- 0
  i <-  1
  previous <-  0
  for(bandnr in bandnumbers[,1]){
    if(i > 4){
      # j <- i - 2 (Olha: not used)
      print(paste0("current band in checking oscillations: ",  bandnr))
      # Olha (!): i+1 (not i) compared to nrow(bandnumbers) to prevent "out of bounds"
      # Olha (!): bandnumbers[i+1] is replaced with bandnumbers[i+1,1] to prevent coercion to NA
      if (i+1 <= nrow(bandnumbers)) {
        print(paste0("next bandnumber: ",bandnumbers[i+1,1]))
      } else {
        print("this is the last band")
      }
      next_bandnr <- ifelse(i+1> nrow(bandnumbers), 1000, 
                            as.numeric(bandnumbers[i+1,1]))
      
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
      # print(paste0("next bandnr: ", next_bandnr))
      # print(bandnumbers[i,2] == "false") # next_bandnr == as.numeric(bandnr)-1  || 
      if(bandnr == bandnumbers[i-2,1] && bandnr == bandnumbers[i-4,1] && #(
         previous == bandnumbers[i-3,1] && 
         (bandnumbers[i,2] == "false") 
         # next_bandnr == as.numeric(bandnr)-1  || 
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
         # Olha: if condition '(previous == bandnumbers[i-3,1] && previous == bandnumbers[i-5,1] &&
         # bandnr == bandnumbers[i-2,1] && bandnr == bandnumbers[i-4,1] &&
         # bandnumbers[i-1,2] == "false"' 
         # holds it meaans that we have already defined thecorrected score on that setp i-1
         (i == 5 || (
           !(previous == bandnumbers[i-3,1] && previous == bandnumbers[i-5,1] &&
             bandnr == bandnumbers[i-2,1] && bandnr == bandnumbers[i-4,1] &&
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
        print(paste0("Number in the sequence of the band numbers:", as.character(i)))
        print("Oscillating bands starting from the latest one: ")      
        print(paste(as.character(bandnr), as.character(previous), 
                    as.character(bandnumbers[i-2,1]), as.character(bandnumbers[i-3,1]), 
                    as.character(bandnumbers[i-4,1]),sep = ", "))
      }
    }
    # If (i > 5) but not (i > 6) than a check for double counting two and a half
    # oscillations is not possible/necessary
    
    previous = bandnr
    i <- i + 1
  }
  
  # Olha: if 'is.na(oscillation_low)' then there was no oscillation and we have stopped for other reasons
  # which can be one of the following: 
  # 2 times in a row visited the last band with the correct answers (chamipon), score is N (i.e. current bandnumber)
  # 2 times in a row visited the first band with the wrong answers (looser), score is 1 (i.e. current bandnumber)
  # not enought stimulifor the nextband, score is then current bandnumber
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

reduce_to_single <- function(raw_bandnumbers, fast_track_sequence, add_isUserCorrect = FALSE){
  bandnumbers <- as.vector(c())
  set <- c()
  previous_isUserCorrect <- ""
  last_isUserCorrect <- ""
  for(row in 1:nrow(raw_bandnumbers)){
    bandnr <- raw_bandnumbers[row,]$BandNumber[[1]]
    if(!bandnr %in% c(" ", "BandNumber")){ # Olha: " " denotes a border between quadruples
      set <- rbind(set, bandnr) 
      # remember last value for isUserCorrect in current band
      last_isUserCorrect <- raw_bandnumbers[row,]$IsUserCorrect[[1]]
    }else if (!is.null(set)){
      corrected_bandnr <- max(set) # Olha: here we define the band number for the whole quadruple
      #print(corrected_bandnr)
      #print(previous_isUserCorrect)
      if(corrected_bandnr == "-1" && previous_isUserCorrect == "true"){ # Olha: we have only 1 representative from the quadruple, and this one is a nonword; we have to derive the corrected bandnumber from the context, the previous band 
        #print("setting to higher correct bandnr")
        #print(as.numeric(bandnumbers[nrow(bandnumbers),1])+1)
        corrected_bandnr <- as.numeric(bandnumbers[nrow(bandnumbers),1])+1
      }else if (corrected_bandnr == "-1" && previous_isUserCorrect == "false"){
        #print("setting to lower correct bandnr")
        #print(as.numeric(bandnumbers[nrow(bandnumbers),1])-1)
        corrected_bandnr <- as.numeric(bandnumbers[nrow(bandnumbers),1])-1
      }
      # Special case for first band in fine tuning that consists of only a single
      # non-word. Olha: it also implies that the answer on this nonword was incorrect, and therefore the next band should be higher than the first detectable band in the fine tuning 
      else if (corrected_bandnr == "-1" && previous_isUserCorrect == ""){
        # Set bandnr to next bandnr + 1
        # print("setting correct bandnr for first band in fine tuning")
        # Olha (!): the subset below is taken over the fine_tuning_sequence,
        # which can be instantiated to fine_tuning_round1 or to fine_tuning_round2.
        # Earlier it was instantiated to fine_tuning_user1 (which keeps both rounds whereas we need round-depending data here)
        
        # Olha (!!!): I detect it via the last achieved band (with the correct answer) in the fast-track sentence 
        wordResponses <- subset(fast_track_sequence, as.numeric(BandNumber) > 0 && IsUserCorrect=="true")
        if (nrow(wordResponses)==0) { # all the words where answered wrongly, that it we ended up at the first band
          corrected_bandnr <- 1
        } else {
          corrected_bandnr <- as.numeric(wordResponses[nrow(wordResponses),]$BandNumber[[1]])
        }
     
      } 
      if(add_isUserCorrect){
        # Olha: earlier it was "raw_bandnumbers[row-1,2][[1]]" as a tuple correctness value.
        # But it may be a mistake if the quadruple's achievements is only 1 word/non-word, false answered, and the previous row-1 is actually a separator
        bandnumbers <- rbind(bandnumbers, c(corrected_bandnr, last_isUserCorrect))
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
# but not for the prescriptivegrammar test, which overrides these defaults.

get_test_duration <- function(screenviews_1user, 
                              startScreenString="stimuliScreen",
                              startScreenMaxChars=13){
  
  # Get test duration from submitDates in screenviews
  start_submit_date <- subset(screenviews_1user, 
                              substring(screenName, 1, startScreenMaxChars) == 
                                startScreenString)[3]
  
  end_submit_date <- subset(screenviews_1user, 
                            screenName == "Admin")  [3]  
  
  startTime <- strptime(substring(start_submit_date, 1, nchar(start_submit_date)-9), 
                        format = "%Y-%m-%dT%H:%M:%S")
  endTime <- strptime(substring(end_submit_date, 1, nchar(end_submit_date)-9), 
                      format = "%Y-%m-%dT%H:%M:%S")
  
  testDuration <- round(as.numeric(endTime -startTime), 1)
  
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