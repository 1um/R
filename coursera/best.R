#Задача написать функцию, которая выберет больницы с найменьшей гибелью.
best <- function(state, outcome_name) {
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
  state_outcome = outcome[outcome$State == state,]
  state_outcome[, col] <- suppressWarnings(as.numeric(state_outcome[, col]))
  state_outcome = state_outcome[complete.cases(state_outcome[,col]),]
  min_outcome = min(state_outcome[,col])
  bests = state_outcome[state_outcome[,col]==min_outcome,]
  bests$Hospital.Name[order(bests$Hospital.Name)][1]
}