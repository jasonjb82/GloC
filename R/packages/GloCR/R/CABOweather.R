# Author: Robert J. Hijmans, r.hijmans@gmail.com
# License GPL3
# Version 0.1  January 2009

#write.fse <- function(wth, year, country, stn, author="", src="", target=".", na.val=-99, y2ksafe=1, sradkJ=TRUE, ...){    
#stop('see function writeCABOwth')
#}


writeCABOwth <- function(wth, country='AAA', station=1, wind=2.5,  path=getwd(), print=TRUE, boundlat=TRUE, rainfact=1, tempfact=0) {
	
	wth@w$srad <- wth@w$srad * 1000
	wth@w$tmin <- wth@w$tmin + tempfact
	wth@w$tmax <- wth@w$tmax + tempfact
	wth@w$prec <- wth@w$prec * rainfact

	years <- unique(wth@w$year)
	for (yr in years) {
		fname <- paste(path, '/', country, station, '.', substr(yr, 2, 4), sep="")
		if (print) {
			cat(fname, '\n')
			flush.console()
		}
		thefile <- file(fname, "w")
		
		cat("*-----------------------------------------------------------", "\n", file = thefile)
		cat("*  Created by the R package 'weather'\n", file = thefile)
		cat("*", "\n", file = thefile)
		cat("*  Column    Daily Value\n", file = thefile)
		cat("*     1      Station number\n", file = thefile)
		cat("*     2      Year\n", file = thefile)
		cat("*     3      Day\n", file = thefile)
		cat("*     4      irradiance         KJ m-2 d-1\n", file = thefile)
		cat("*     5      min temperature            oC\n", file = thefile)
		cat("*     6      max temperature            oC\n", file = thefile)
		cat("*     7      vapor pressure            kPa\n", file = thefile)
		cat("*     8      mean wind speed         m s-1\n", file = thefile)
		cat("*     9      precipitation          mm d-1\n", file = thefile)
		cat("*\n", file = thefile)
		cat("** WCCDESCRIPTION=gizmo\n", file = thefile)
		cat("** WCCFORMAT=2\n", file = thefile)
		cat("** WCCYEARNR=", yr, "\n", file = thefile)
		cat("*-----------------------------------------------------------", "\n", file = thefile)
		if (boundlat) {
			if ( wth@lat > 60) { wth@lat <- 59 }
			if ( wth@lat < -60 ) { wth@lat <- -59 }
		}
		cat(wth@lon, wth@lat, wth@alt, '  0.00  0.00 \n', file = thefile)

		yw <- subset(wth@w, wth@w$year==yr)
		yw[is.na(yw)] <- -9999
		for (d in 1:length(yw[,1])) {
			cat("1  ", sprintf("%6.0f", yr), sprintf("%5.0f", d), sprintf("%10.0f", yw$srad[d]), sprintf("%8.1f", yw$tmin[d]), sprintf("%8.1f", yw$tmax[d]), sprintf("%8.1f", yw$vapr[d]), sprintf("%8.1f", wind), sprintf("%8.1f", yw$prec[d]), "\n", file=thefile)
		}
		close(thefile)
    }
	return(invisible(TRUE))
}		
