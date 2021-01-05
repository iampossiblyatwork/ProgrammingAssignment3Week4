rankall <- function(outcome, num = "best") {
  
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
      reducedbyoutcome<-reduced[,c(1,2,3)]
      return(ranker(reducedbyoutcome,num))
  }else if(outcome == "pneumonia"){
      reducedbyoutcome<-reduced[,c(1,2,5)]
      return(ranker(reducedbyoutcome,num))
  }else if(outcome == "heart failure"){
      reducedbyoutcome<-reduced[,c(1,2,4)]
      return(ranker(reducedbyoutcome,num))
  }else{
      stop("invalid outcome")
  }
  ## Return hospital name in that state with the given rank
  
  ## 30-day death rate
  
}
  
  ## For each state, find the hospital of the given rank
  
  
  ## Return a data frame with the hospital names and the
  ## (abbreviated) state name}

ranker<- function (data,num){
  output<-data.frame(NULL)
  sorted<-suppressWarnings(data[order(
    data$State,as.numeric(data[,3]),data$Hospital.Name),])
  write.csv(sorted,"test.csv")
  count<-0
  split_sorted <- split.data.frame(sorted, f=sorted$State)
  for (state in split_sorted){
    count<-1+count
    value_check<- data.frame(state)
    state_hospital_total <- suppressWarnings(sum(!is.na(as.numeric(
      value_check[,3]))))
    if (num == "best"){
      rank<-1
    }else if (num == "worst"){
      rank <- state_hospital_total
    }else{
      rank <- num 
    }
    if (rank> state_hospital_total){
      na_overwrite<-value_check[1,1:2]
      na_overwrite <- replace(na_overwrite,1,NA)
      rownames(na_overwrite)<-value_check[1,2]
      hospital_rank_req<-na_overwrite
    }else{
      hospital_name<- value_check[rank,1:2]
      rownames(hospital_name)<-value_check[1,2]
      hospital_rank_req<-hospital_name
    }
    output<-rbind(output,hospital_rank_req)
  }
  return(output)
}