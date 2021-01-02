## best function takes 2 arguments (state and outcome) and rturns a chr
## vector with the name of the hospital that has the best 30-day mortality
## for the speicifed outcome in that state.

best<- function(state,outcome){
  ## Read outcome data
  full_data <- read.csv("outcome-of-care-measures.csv",colClasses = "character")
  ## Extract only pertinent data. Name, State, and all 3 rates.
  reduced <- full_data[,c(2,7,11,17,23)] 
  ## Check outcome and state is valid

  if(outcome == "heart attack"){
    if (state %in% reduced$State){
        reducedbyoutcome<-reduced[,c(1,2,3)]
        return(maxfinder(reducedbyoutcome,state))
    } else{
      stop("invalid state")
    }
  } else if(outcome == "pneumonia"){
    if (state %in% reduced$State){
        reducedbyoutcome<-reduced[,c(1,2,4)]
        return(maxfinder(reducedbyoutcome,state))
    } else{
      stop("invalid state")
    }
  } else if(outcome == "heart failure"){
    if (state %in% reduced$State){
        reducedbyoutcome<-reduced[,c(1,2,5)]
        return(maxfinder(reducedbyoutcome,state))
    } else{
      stop("invalid state")
    }
  } else{
    stop("invalid outcome")
      }
  
  ##Return hospital name in that state with lowest 30-day death
  ##rate
}

maxfinder <- function(data,state){
  outcome_data <- data[which(data$State==state),]
  best_rate <- colMax(outcome_data)
  output <- best_rate
  return(output)
}