f <- function(x) {
  g <- function(y) {
    y + z
  }
  z <- 4
  x + g(x)
}
pollutantmean <- function(directory,pollutant,id = 1:332){
  # read all files
  files <- file.path(directory ,list.files(directory))
  n <- length(files)
  
  if (n > 0){
      merge_data <- read.csv(files[1],header = T,sep = ",", comment.char = "")
      
      for ( index  in 2:n){
          new_data = read.csv(files[index],header = T,sep = ",", comment.char = "")
          merge_data = rbind(merge_data,new_data)
          
      }

      ids_data <- merge_data[!is.na(merge_data[pollutant]),]
      #print((ids_data))
      # choose ids
      
      #ids_data[ids_data$ID %in%  1:2,]
      
      
      if (sum(ids_data$ID %in% id) ==0 )
      {
        0
        
      }
      else{
        ids_data <- ids_data[ids_data$ID %in% id,pollutant]
        mean(ids_data)
      }
      
       #ids_data
    }
  
}
  