rankall <- function(outcome, num = "best") {
  ## Read outcome data
  data <- read.csv("outcome-of-care-measures.csv",colClasses = 'character')
  ## Check valid
  outNames <- c("heart attack", "heart failure", "pneumonia")
  outColumns <- c("Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"
                  ,"Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"
                  ,"Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia")
  if (!(outcome %in% outNames))
  {
    stop(paste("Error in best(\"",outcome,"\",", "\"",num,"\") : invalid outcome",sep = ""))
  }
  
  outColumn <- outColumns[which(outNames == outcome)]
  dataStas <- data[ !is.na(as.numeric(data[,outColumn])), ]
  states <- unique(dataStas$State)
  states <- sort(states[!is.na(states)])
  df <- data.frame(matrix(0,0,2))

  for(state in states)
  {
    index <- num
    dataSta <- dataStas[which(dataStas$State == state),]
    sortColumn <- order(as.numeric(dataSta[,outColumn]),dataSta[,"Hospital.Name"])
  
    if (num == "best")
    {
      index <- 1
    }
    if (num == "worst")
    {
      
      index <- length(sortColumn);
    }
    
   
    if(index > length(dataSta[,outColumn]))
    { hospital <- NA}
    else
    {
      
      rank <- sortColumn[index]
      hospital <- dataSta[rank,"Hospital.Name"]
      ## 30-day death rate
     
    }
    newDf <- data.frame(matrix(c(hospital,state),1,2))
    df <- rbind(df,newDf)
  }
  
  colnames(df) <- c("hospital","state")
  df
  
  ## For each state, find the hospital of the given rank
  
  ## Return a data frame with the hospital names and the
  
  ## (abbreviated) state name
}
