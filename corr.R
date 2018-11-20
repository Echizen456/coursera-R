corr <- function(directory,threshold = 0){
  files <- file.path(directory ,list.files(directory))
  n <- length(files)
  
  if (n > 0){
      merge_data <- read.csv(files[1],header = T,sep = ",", comment.char = "")
    
      for ( index  in 2:n){
        new_data = read.csv(files[index],header = T,sep = ",", comment.char = "")
        merge_data = rbind(merge_data,new_data)
      
      }
    
     com <- complete(directory ,id = 1:332)
     satisIDs <- com[com$nobs>threshold,"id"]
     ids_data <- merge_data[complete.cases(merge_data),]
     
     {
       cnames <- names(ids_data)
       #ids_data <- ids_data[ids_data$ID %in% satisIDs,]
       out <- vector(mode = "numeric", length = 0)
       for (index in satisIDs){
         sulfate <- ids_data[ids_data$ID %in% index,"sulfate"]
         nitrate <- ids_data[ids_data$ID %in% index,"nitrate"]
         new_ma <- cor(sulfate,nitrate)
         out <- cbind(out,new_ma)
         
       }
       as.vector(out)
       
     }
  }
  
  
}