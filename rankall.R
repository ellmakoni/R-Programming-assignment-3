rankall <- function(outcome, num = "best"){
  ## Re-use rankhospital
  ##source rankhospital
  ## Read outcome data
  states <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  states <- unique(select(states, State))
  states <- arrange(states,State)
  ctr <- 1
  end <- nrow(states)
  dataset <- data.frame("hospital"=character(0),"state"=character(0))
  
  while (ctr <= end){
    rh <- rankhospital(states[ctr,], outcome, num)
    
    if (length(rh[1]) == 0){
      rh <- "NA"
    }
    newDataSet <- data.frame("hospital" = rh[1], "state" = states[ctr,])
    dataset <- rbind(dataset, newDataSet)
    ctr <- ctr + 1
  }
  
  dataset
  
}

head(rankall("heart attack", 20), 10)

tail(rankall("pneumonia", "worst"), 3)

tail(rankall("heart failure"), 10)

