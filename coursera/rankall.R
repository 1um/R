rankall <- function(outcome_name, num = "best") {
  setwd("~/Documents/prog/coursera/course2/pa3/rprog-data-ProgAssignment3-data")
  outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  if(outcome_name=="heart attack"){
    col = 11 
  }else if(outcome_name=="heart failure") {
    col = 17
  }else if(outcome_name=="pneumonia") {
    col = 23
  }else{
    stop("invalid outcome")
  }
  
  if(num=="best"){
    get <- function(arr){arr[1]} 
  }else if(num=="worst") {
    get <- function(arr){tail(arr,1)} 
  }else{
    get <- function(arr){arr[num]} 
  }
  
  
  outcome[, col] <- suppressWarnings(as.numeric(outcome[, col]))
  outcome = outcome[complete.cases(outcome[,col]),]
  outcome = outcome[order(outcome[,col],outcome$Hospital.Name),]
  r = tapply(outcome$Hospital.Name,outcome$State,get)
  data.frame(hospital = r, state = names(r))
}
