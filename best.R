best <- function(state, outcome) {
  ## Read outcome data
  data <- read.csv("outcome-of-care-measures.csv",colClasses = 'character')
  ## Check that state and outcome are valid
  if (!(state %in% data$State))
  {
    stop(paste("Error in best(\"",state,"\",", "\"",outcome,"\") : invalid state",sep = ""))
  }
  ## Check that state and data are valid
  outNames = c("heart attack", "heart failure", "pneumonia")
  outColumns = c("Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"
                 ,"Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"
                 ,"Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia")
  if (!(outcome %in% outNames))
  {
    stop(paste("Error in best(\"",state,"\",", "\"",outcome,"\") : invalid outcome",sep = ""))
  }

  
  outColumn = outColumns[which(outNames == outcome)]
  ## Return hospital name in that state with lowest 30-day death
  dataSta = data[which(data$State == state) ,]
  dataSta <- dataSta[ !is.na(as.numeric(dataSta[,outColumn])), ]
  bb = summary(as.numeric(dataSta[,outColumn]))
  column = which(as.numeric(dataSta[,outColumn]) == bb["Min."])
  sort(dataSta[column,"Hospital.Name"])[1]
  ## rate
}
