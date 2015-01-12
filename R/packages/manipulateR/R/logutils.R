# Author: Jorrel Khalil S. Aunario, jaunario@gmail.com
# Date :  21 February 2012
# Version 0.0.1
# Licence GPL v3

show.message <- function(..., EL=FALSE, delay=0){
  # Real-time console messages
  if (EL){
    message(rep(" ", options("width")$width-1),"\r", appendLF=FALSE)
  } 
  message(...)
  Sys.sleep(delay)
  flush.console()  
}

timer.message <- function(text, time){
  if(is.numeric(time)){
    for (i in time:1){
      show.message(text, i,"\r", EL=TRUE, delay=1, appendLF=FALSE)
    }    
  }
}

progress.bar <- function(step, width){
 #TODO	
}