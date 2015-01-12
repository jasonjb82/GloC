# Author: Robert J. Hijmans, r.hijmans@gmail.com
# License GPL3
# Version 0.1  January 2009

getWthFile <- function(filename, type='NASA') {
	if (type=='NASA') {
		return(.getWthFileNASA(filename))
	}

}

getWthXY <- function(lon, lat, start="1993-1-1", end="2009-12-31") {
	sday <- dayFromDate(start)
	smon <- monthFromDate(start)
	syr <- yearFromDate(start)
	eday <- dayFromDate(end)
	emon <- monthFromDate(end)
	eyr <- yearFromDate(end)
	raster <- raster()
	cell <- cellFromXY(raster, c(lon, lat))
	if (is.na(cell)) {
		stop("invalid coordinates")
	} 
	filename <- paste("daily_weather_", cell, ".nasa", sep="")

	vars <- c("swv_dwn", "T2M", "T2MN", "T2MX", "RH2M", "RAIN")

	xy <- xyFromCell(raster, cell)
	lon <- xy[1]
	lat <- xy[2]
	if (!file.exists(filename)) {
		part1 <- "http://earth-www.larc.nasa.gov/cgi-bin/cgiwrap/solar/agro.cgi?email=agroclim%40larc.nasa.gov&step=1&lat="
		part2 <- paste(lat, "&lon=", lon, "&sitelev=&ms=", smon, "&ds=", sday, "&ys=", syr, "&me=", emon, "&de=", eday, "&ye=", eyr, sep="")
		part3 <- ''
		for (i in 1:length(vars)) {
			part3 <- paste(part3, "&p=", vars[i], sep="")
		}
		part3 <- paste(part3, "&submit=Submit", sep="")
		theurl <- paste(part1, part2, part3, sep="")
		download.file(url=theurl, destfile=filename, method="auto", quiet = FALSE, mode = "wb", cacheOK = TRUE)
	}
	return(.getWthFileNASA(filename))
}

.ICASAstyle <- function(lns) {
	h1 <- lns[7]
#@ INSI   WTHLAT   WTHLONG  WELEV   TAV   AMP  REFHT  WNDHT
#  NASA   59.500  -167.500      5                        10
	
	y <- as.numeric(substr(h1, 9, 15))
	x <- as.numeric(substr(h1, 18, 25))
	alt <- as.numeric(substr(h1, 27, 32))
	h2 <- lns[9]
#@ WEYR WEDAY  SRAD   TMAX   TMIN   RAIN   WIND   TDEW    T2M   RH2M
	h2 <- strsplit ( gsub("[[:space:]]+", " ", gsub("[[:space:]]+$", "", h2))  , " ")

	lns <- lns[10:(length(lns)-1)]
	lns <- strsplit ( gsub("[[:space:]]+", " ", gsub("[[:space:]]+$", "", lns))  , " ")
	lns <- matrix(as.numeric(unlist(lns)), ncol=length(lns[[1]]), byrow=T)
	#colnames(lns) <- h2[[1]]
	lns <- lns[,-1]
	colnames(lns) <- c("year", "doy", "srad", "tmax", "tmin", "prec", "wind", "tdew", "tavg", "relh")

	rhnx <- rhMinMax(lns[,'relh'], lns[,'tmin'], lns[,'tmax'], lns[,'tavg']) 
	vapr <- lns[,'relh'] * SVP(lns[,'tavg']) / 1000     # 100 for % and 10 to go from hPa to kPa
	lns <- cbind(lns, rhnx, vapr)
	date <- dateFromDoy(lns[,'doy'], lns[,'year'])
	lns <- cbind(as.data.frame(date), lns)
	wth <- new('weather')
	wth@lon <- x
	wth@lat <- y
	wth@alt <- alt
	wth@w <- lns
	return(wth)

	#@ WEYR WEDAY  SRAD   TMAX   TMIN   RAIN   WIND   TDEW    T2M   RH2M

}

.getWthFileNASA <- function(filename) {
	lns <- readLines(filename)
	
	if (substr(lns[6],1,1) == '@') {
		return(.ICASAstyle(lns))
	}
	
	alt <- NA
	try(alt <- as.numeric(substr(lns[4],60,66)), silent=TRUE)
	x <- 0
	y <- 0
	try(x <- as.numeric(substr(lns[3],36,41)), silent=TRUE)
	try(y <- as.numeric(substr(lns[3],19,24)), silent=TRUE)
	
	hdr <- strsplit ( gsub("[[:space:]]+", " ", gsub("[[:space:]]+$", "", lns[14]))  , " ")[[1]]
	if (hdr[1] != "YEAR") { stop("Something (not so) funny is going on") }
	lns <- lns[15:length(lns)]
	lns <- strsplit ( gsub("[[:space:]]+", " ", gsub("[[:space:]]+$", "", lns))  , " ")
	lns <- matrix(as.numeric(unlist(lns)), ncol=length(lns[[1]]), byrow=T)

	nicevars <- c("year", "doy", "srad", "tavg", "tmin", "tmax", "relh", "prec")
	colnames(lns) <- nicevars
	rhnx <- rhMinMax(lns[,'relh'], lns[,'tmin'], lns[,'tmax'], lns[,'tavg']) 
	
	vapr <- lns[,'relh'] * SVP(lns[,'tavg']) / 1000     # 100 for % and 10 to go from hPa to kPa

	lns <- cbind(lns, rhnx, vapr)
	
	date <- dateFromDoy(lns[,'doy'], lns[,'year'])
	lns <- cbind(as.data.frame(date), lns)
	wth <- new('weather')
	wth@lon <- x
	wth@lat <- y
	wth@alt <- alt
	wth@w <- lns
	return(wth)
}

