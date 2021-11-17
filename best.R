best <- function(state, outcome)
                 {
  ##Read outcome data
  data <- read.csv("outcome-of-care-measures.csv", na.strings = "Not Available", 
                   stringsAsFactors = FALSE)
  
  #Selecting columns
  names(data) <- 1:46
  outcomes <- c("heart attack" = 11, "heart failure" = 17, "pneumonia" = 23)
  list <- outcomes[outcome]
  
  my_data <- data[, c(2,7,list)]
  
  names(my_data) <- c("hospital", "state", outcome)
    
  stlist <- which(my_data[, 2] == state)
  stdata <- my_data[stlist, ]
  
  stdata <- na.omit(stdata)
  
  best <- which.min(stdata[, 3])
  hos <- stdata[best, 1]
  
  ##Check validity
  if (length(stlist) == 0) {
    stop("invalid state")
  }
  if (length(list) == 0) {
    stop("invalid outcome")
  }
  
  else
    
  return(hos)
  }