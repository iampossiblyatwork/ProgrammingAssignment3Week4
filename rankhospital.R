rankhospital <- function(state, outcome, num = "best") {
  
  ## Read outcome data
  full_data <- read.csv("outcome-of-care-measures.csv",colClasses = "character")
  ## Extract only pertinent data. Name, State, and all 3 rates.
  reduced <- full_data[,c(2,7,11,17,23)] 
  ## Check outcome and state is valid
  ## each if statement first checks if the outcome is valid
  ## then state is checked within each outcome check
  ## finally the appropriate outcome is pulled from reduced dataset
  ## this small dataset is that passed to maxfinder function
  ## if (integer check, positive check, b/w check)
  if(num == "best" | num=="worst"){
    ##do nothing
  }else if(num %% 1 == 0 & num > 0){
    ## do nothing
  }else{
    return(NA)
  }
  if(outcome == "heart attack"){
    if (state %in% reduced$State){
      reducedbyoutcome<-reduced[,c(1,2,3)]
      return(ranker(reducedbyoutcome,state,num))
    } else{
      stop("invalid state")
    }
  } else if(outcome == "pneumonia"){
    if (state %in% reduced$State){
      reducedbyoutcome<-reduced[,c(1,2,5)]
      return(ranker(reducedbyoutcome,state,num))
    } else{
      stop("invalid state")
    }
  } else if(outcome == "heart failure"){
    if (state %in% reduced$State){
      reducedbyoutcome<-reduced[,c(1,2,4)]
      return(ranker(reducedbyoutcome,state,num))
    } else{
      stop("invalid state")
    }
  } else{
    stop("invalid outcome")
  }
  ## Return hospital name in that state with the given rank
  
  ## 30-day death rate
  
  }
  
ranker <- function(data,state_abbrv,num){
  ## filters out all states not passed to function
  outcome_data <- data[data$State == state_abbrv,]
  ##calculates min based on 3rd col which is form sorted data
  ## sort first by rate (lowest to highest) then alphabetically by Hospital
  sorted_data <- suppressWarnings(outcome_data[order(as.numeric(outcome_data[,
    3]),outcome_data[,1]),])
  #counts all values that are NOT NA values after coercion
  hospital_count<-suppressWarnings(sum(!is.na(as.numeric(sorted_data[,3]))))
  if (num == "worst"){
    num <- hospital_count
  }else if(num == "best"){
    num <- 1
  }else if(num < hospital_count){
    ##do nothing!
  }else{
    return(NA)
  }
  num<-as.numeric(num)
  return(sorted_data[num,1])
}