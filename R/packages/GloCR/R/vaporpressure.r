

svp <- function(temp){
	return(6.1078 * exp(17.2694 * temp / (temp + 237.3)))
}

mvp <- function(tavg,rhavg){    
	return(svp(tavg) * rhavg / 100)
}


