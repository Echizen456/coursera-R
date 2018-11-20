first <- function(){
  x <- rnorm(100)
  mean(x)
  
}



test <- function(){
  mydata <- read.table(text="Country_cde    Flag1     Flag2
IE               A         X
                       IE               B         X
                       US               A         X
                       US               A         Y
                       IE               C         Z",header=T,stringsAsFactors=F)
  
  freq_matrix <- table( unlist( unname(mydata) ) ) # Other way to count the occurrences
  print(freq_matrix["A"])
  mydata[,"Score"] <- apply( mydata,1, function(x) { paste0( sum(freq_matrix[x]) ,"/", length(x) )}) # Do the sum, paste with number of cols (should be computed outside to avoid cache miss)
  
}
