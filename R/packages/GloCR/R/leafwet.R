# Author: Robert J. Hijmans, r.hijmans@gmail.com
# License GPL3
# Version 0.1  January 2009

eLW <- function(rhmin, rhmax, tmin) {
# emperical leaf wetness estimation according to Hijmans, Forbes and Walker, 2001
    ewhr <- exp(-8.093137318+0.11636662*rhmax-0.03715678*rhmin+0.000358713*rhmin*rhmin)
    if (rhmin < 52) {
      ewhr52 <- exp(-8.093137318+0.11636662*rhmax-0.03715678*52+0.000358713*52*52);
      ewhr <- ewhr52 - (ewhr - ewhr52);
	}
    ewhr <- max(0, min(ewhr, 24))
    if (tmin < 0) {ewhr <- 0}
	return(ewhr)
}


leafWet <- function(wth, simple=TRUE) {
	lw <- vector(length=dim(wth@w)[1])
	wth@w$rhavg <- (wth@w$rhmin + wth@w$rhmax) / 2
	for (d in 1:length(lw)) {
		rh <- diurnalRH(wth@lat, wth@w$date[d], wth@w$rhavg[d], wth@w$tmin[d], wth@w$tmax[d], wth@w$tavg[d])
		if (simple) {
			lw[d] <- length(rh[rh>=90])
		} else {
			w <- rh
			x <- (rh - 80) / (95 - 80)
			w[rh > 95] <- 1
			w[rh < 95] <- x[rh < 95]
			w[rh < 80] <- 0
			lw[d] <- sum(w)
		}
	}
	return(lw)
}


leafWetWithRain <- function(wth, simple=TRUE) {
	lw <- leafWet(wth, simple=simple)
	prec[is.na(wth@w$prec)] <- 0 
	prhrs <- pmin(12, wth@w$prec / 5)
	return(lw + (1 - lw/24) * prhrs)
}

