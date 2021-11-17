rankhospital <- function(state, outcome, num = "best") 
  {
  ## Read outcome data
  data <- read.csv("outcome-of-care-measures.csv", na.strings = "Not Available", 
                   stringsAsFactors = FALSE)
  
  names(data) <- 1:46
  outcomes <- c("heart attack" = 11, "heart failure" = 17, "pneumonia" = 23)
  my_data <- data[, c(2,7,outcomes[outcome])]
  names(my_data) <- c("hospital", "state", outcome)
  
  stlist <- which(my_data[, 2] == state)
  stdata <- my_data[stlist, ]
  stdata <- na.omit(stdata)
  ## Check that state and outcome are valid
  if (length(stlist) == 0) {
    stop("invalid state")
  }
  if (length(list) == 0) {
    stop("invalid outcome")
  }

  ## Return hospital name in that state with the given rank
  ind <- stdata[, 1]
  sorted <- sort.list(stdata[, 3])
  stdata <- stdata[sorted, ]
  
  if (num = "best") {
    return(stdata[1, 1])
  }
      
  if (num = "worst") {
    num == length(ind)
    return(stdata[1, num])
  }
  
  else
  stdata[1, num]

}
