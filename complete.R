complete <- function(directory,id = 1:332){
  # read all files
  files <- file.path(directory ,list.files(directory))
  n <- length(files)
  
  if (n > 0){
    merge_data <- read.csv(files[1],header = T,sep = ",", comment.char = "")
    
    for ( index  in 2:n){
      new_data = read.csv(files[index],header = T,sep = ",", comment.char = "")
      merge_data = rbind(merge_data,new_data)
      
    }
    
    ids_data <- merge_data[complete.cases(merge_data),]
    cnames <- c("id","nobs")
    out <- data.frame(matrix(nrow = 0,ncol = length(cnames)))
    
    
    for (index in id) 
    {
      nob <- nrow(ids_data[ids_data$ID %in% index,])
      df <- data.frame(matrix(c(index,nob),1,2))
      out <- rbind(out,df)
    }
    names(out) <- cnames
    out  
    
    
    #ids_data
  }
  
}