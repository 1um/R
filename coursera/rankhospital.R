# Нужно по больницам было вывести какой-то показатель(heart attack,heart failure...)  по конкретному штату(лучшее, i-тое, худшее)
rankhospital <- function(state, outcome_name, num = "best") {
  setwd("~/Documents/prog/coursera/course2/pa3/rprog-data-ProgAssignment3-data")
  outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  if(! state %in% unique(outcome$State)){
    stop("invalid state")
  }
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
  
  
  
  state_outcome = outcome[outcome$State == state,]
  state_outcome[, col] <- suppressWarnings(as.numeric(state_outcome[, col]))
  state_outcome = state_outcome[complete.cases(state_outcome[,col]),]
  state_outcome = state_outcome[order(state_outcome[,col],state_outcome$Hospital.Name),]
  get(state_outcome$Hospital.Name)
}
