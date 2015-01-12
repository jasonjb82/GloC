# Author: Jorrel Khalil S. Aunario, jaunario@gmail.com
# Date :  21 February 2012
# Version 0.0.1
# Licence GPL v3

rescale <- function(x, oldmin, oldmax, newmin, newmax){
  return((x-oldmin)*(newmax-newmin)/(oldmax-oldmin) + newmin)
}


in.range <- function(x, minofrange, maxofrange, min.inc=TRUE, max.inc=TRUE){
  if(minofrange>maxofrange){
    temp <- minofrange
    minofrange <- maxofrange
    maxofrange <- temp
  }
  uop <- ifelse(max.inc, "<=", "<")
  lop <- ifelse(min.inc, ">=", ">")
  
  return(get(lop)(x,minofrange) & get(uop)(x, maxofrange))
  
}

dominant <- function(x, na.rm=TRUE){
  if (!is.numeric(x)) stop("x should be numeric.")
  if (na.rm==TRUE) x <- x[!is.na(x)]
  
  if (length(x)<1) result <- NA else {
    frq <- table(x)
    result <- as.numeric(names(frq)[which(frq==max(frq))[1]]) # in case of tie get the first value
  }
  return(result)
}
