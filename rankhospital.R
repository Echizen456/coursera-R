rankhospital <- function(state, outcome, num = "best") {
  ## Read outcome data
  data <- read.csv("outcome-of-care-measures.csv",colClasses = 'character')
  ## Check that state and outcome are valid
  if (!(state %in% data$State))
  {
    stop(paste("Error in best(\"",state,"\",", "\"",outcome,"\") : invalid state",sep = ""))
  }
  ## Check that state and data are valid
  outNames <- c("heart attack", "heart failure", "pneumonia")
  outColumns <- c("Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"
                 ,"Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"
                 ,"Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia")
  if (!(outcome %in% outNames))
  {
    stop(paste("Error in best(\"",state,"\",", "\"",outcome,"\") : invalid outcome",sep = ""))
  }


  
  outColumn <- outColumns[which(outNames == outcome)]
  dataSta <- data[which(data$State == state),]
  dataSta <- dataSta[ !is.na(as.numeric(dataSta[,outColumn])), ]
  ## Return hospital name in that state with the given rank
  sortColumn <- order(as.numeric(dataSta[,outColumn]),dataSta[,"Hospital.Name"])

  if (num == "best")
  {
    num <- 1
  }
  if (num == "worst")
  {
    
    num <- length(sortColumn);
  }
  if(num > length(dataSta[,outColumn]))
  { NA}
  else
  {
    rank <- sortColumn[num]
    dataSta[rank,"Hospital.Name"]
    ## 30-day death rate
    
  }
 
}
