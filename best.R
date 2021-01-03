## best function takes 2 arguments (state and outcome) and returns a chr
## vector with the name of the hospital that has the best 30-day mortality
## for the specified outcome in that state.

best<- function(state,outcome){
  ## Read outcome data
  full_data <- read.csv("outcome-of-care-measures.csv",colClasses = "character")
  ## Extract only pertinent data. Name, State, and all 3 rates.
  reduced <- full_data[,c(2,7,11,17,23)] 
  ## Check outcome and state is valid
  ## each if statement first checks if the outcome is valid
  ## then state is checked within each outcome check
  ## finally the appropriate outcome is pulled from reduced dataset
  ## this small dataset is that passed to maxfinder function

  if(outcome == "heart attack"){
    if (state %in% reduced$State){
        reducedbyoutcome<-reduced[,c(1,2,3)]
        return(maxfinder(reducedbyoutcome,state))
    } else{
      stop("invalid state")
    }
  } else if(outcome == "pneumonia"){
    if (state %in% reduced$State){
        reducedbyoutcome<-reduced[,c(1,2,5)]
        return(maxfinder(reducedbyoutcome,state))
    } else{
      stop("invalid state")
    }
  } else if(outcome == "heart failure"){
    if (state %in% reduced$State){
        reducedbyoutcome<-reduced[,c(1,2,4)]
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

## simplifies code by finding min hospital mortality and returning Name
maxfinder <- function(data,state_abbrv){
  ## filters out all states not passed to function
  outcome_data <- data[data$State == state_abbrv,]
  ##calculates min based on 3rd col which is form sorted data
  best_rate <- suppressWarnings(outcome_data[which.min(outcome_data[,3]),])
  output <- best_rate[order(best_rate$Hospital.Name),] ##sorts multiple outcomes
  ##grabs first line of sorted data
  output <- output[1,]
  return(output$Hospital.Name)
}