# Author: Jorrel Khalil S. Aunario, jaunario@gmail.com
# Date :  21 February 2012
# Version 0.0.1
# Licence GPL v3

strToChar <- function(str){
  return(unlist(strsplit(str,"")))
}

serialn <- function(x, width=2){    
  # Just use sprintf
  return(sprintf(paste("%0",width,"d",sep=""),x))
}

