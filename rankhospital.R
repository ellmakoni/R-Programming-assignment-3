rankhospital <- function(state, outcome, num = "best"){
  library(dplyr)
  ## Read outcome data
  outcomeData <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  hospital <- NULL
  rate <- NULL
  range <- NULL
  
  ## Function for heart attack
  inHeartAttack <- function(s, x){
    x <- select(x, State, Hospital.Name, Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack)
    filtered <- x[x$State==s & x$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack != 'Not Available' ,c("Hospital.Name","Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack")]
    
    ## Sort by Hospital name
    sortedData <- arrange(filtered, Hospital.Name)
    
    ## Sort by Rate
    sortedData$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack <- as.numeric(sortedData$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack)
    sortedData <- arrange(sortedData, Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack)
    
    ## Return hospital name in that state with lowest 30-day death rate
    bestHosp <- sortedData
    
  }
  
  ## Function for heart failure
  inHeartFailure <- function(s, x){
    x <- select(x, State, Hospital.Name, Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure)
    filtered <- x[x$State==s & x$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure != 'Not Available' ,c("Hospital.Name","Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure")]
    
    ## Sort by Hospital name
    sortedData <- arrange(filtered, Hospital.Name)
    
    ## Sort by Rate
    sortedData$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure <- as.numeric(sortedData$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure)
    sortedData <- arrange(sortedData, Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure)
    
    ## Return hospital name in that state with lowest 30-day death rate
    bestHosp <- sortedData
    
  }
  
  ## Function for pneumonia
  inPneumonia <- function(s, x){
    x <- select(x, State, Hospital.Name, Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia)
    filtered <- x[x$State==s & x$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia != 'Not Available' ,c("Hospital.Name","Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia")]
    
    ## Sort by Hospital name
    sortedData <- arrange(filtered, Hospital.Name)
    
    ## Sort by Rate
    sortedData$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia <- as.numeric(sortedData$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia)
    sortedData <- arrange(sortedData, Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia)
    
    ## Return hospital name in that state with lowest 30-day death rate
    bestHosp <- sortedData
    
  }
  
  ## Finalise Result
  getHospital <- function(ds, range){
    if (range == "best")
      ds[1, ]
    else if (range == "worst")
      ds[nrow(ds),]
    else if (is.numeric(range))
      ds[range,]
  }
  
  ## Check if outcome is valid
  if (outcome == "heart attack"){
    ## Check if state is valid
    if (length(outcomeData[outcomeData$State == state,c("State")])>0){
      bh <- inHeartAttack(state, outcomeData)
      bh <- getHospital(bh, num)
      hospital <- bh[,c("Hospital.Name")]
      rate <- bh[,2]
    } else{
      hospital <- paste("Error in best(", state, ", ", outcome,") : invalid state", sep="")
    }
  } else if (outcome == "heart failure"){
    ## Check if state is valid
    if (length(outcomeData[outcomeData$State == state,c("State")])>0){
      bh <- inHeartFailure(state, outcomeData)
      bh <- getHospital(bh, num)
      hospital <- bh[,c("Hospital.Name")]
      rate <- bh[,2]
    } else{
      hospital <- paste("Error in best(", state, ", ", outcome,") : invalid state", sep="")
    }
  } else if (outcome == "pneumonia"){
    ## Check if state is valid
    if (length(outcomeData[outcomeData$State == state,c("State")])>0){
      bh <- inPneumonia(state, outcomeData)
      bh <- getHospital(bh, num)
      hospital <- bh[,c("Hospital.Name")]
      rate <- bh[,2]
    } else{
      hospital <- paste("Error in best(", state, ", ", outcome,") : invalid state", sep="")
    }
  } else{
    hospital <- paste("Error in best(", state, ", ", outcome,") : invalid outcome", sep="")
  }
  hospital
}

rankhospital("TX", "heart failure", 4)

rankhospital("MD", "heart attack", "worst") 

rankhospital("MN", "heart attack", 5000)
