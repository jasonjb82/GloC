# Author: Jorrel Khalil S. Aunario, jaunario@gmail.com
# Date :  7 May 2012
# Version 0.0.1
# Licence GPL v3
# Read and Write FSE weather files

read.fse <- function(fsefile, datacols=c("station_id", "year", "doy", "srad", "tmin", "tmax", "vapr","wind","prec"), delim=" ", skip.hdr=FALSE, std.vals=TRUE){

	fsewth <-  new("weather")
	is.sunshine <- FALSE
	if (length(which(datacols %in% c("year", "doy")))!=2) stop("Required columns year and doy (day of year) not found.")

	if(file.exists(fsefile) & file.info(fsefile)$size!=0){
		
		dlines <- readLines(fsefile)
		dlines <- gsub("\t", delim, dlines)
		
		# get headers
		ihdr <- grep("\\*", dlines)

		if(!skip.hdr){
			hdr <- gsub("\\*", " ", dlines[min(ihdr):max(ihdr)])
			hdr <- trim(gsub("\\?", " ", hdr))
			hdr <- hdr[hdr!=""]
			
			icol <- grep("1[[:space:]]+Station", hdr, ignore.case=TRUE)
			if (length(icol)>1 & length(grep("--", hdr))>0){
				colinfo <- hdr[icol:(length(hdr)-1)]
			} else {
				colinfo <- hdr[icol:length(hdr)]
			}
			hdr <- hdr[1:(icol-1)]
			
			# get station name
			i <- grep("station", hdr, ignore.case=TRUE)
			if (length(i)==0) {
				i <- grep("location", hdr, ignore.case=TRUE)
			} 
			fsewth@stn <- ifelse(!is.na(i[1]), trim(gsub("\\*", "", unlist(strsplit(hdr[i],":"))[2])),"Unknown")
			
			# get source
			i <- grep("source", hdr, ignore.case=TRUE)
			fsewth@rmk <- ifelse(length(i)==1, trim(unlist(strsplit(hdr[i],":"))[2]),"")
			
			# get station name
			#i <- grep("source", hdr, ignore.case=TRUE)
			#fsewth@rmk <- ifelse(length(i)==1, trim(unlist(strsplit(dlines[i],":"))[2]),"")			
		}
			
		
		# get coordinates
		coords <- as.numeric(unlist(strsplit(trim(dlines[max(ihdr)+1]),delim)))
		coords <- coords[!is.na(coords)]
		
		rm(dlines)
		gc(verbose=FALSE)
		
		fsewth@lon <- coords[1]
		fsewth@lat <- coords[2]
		fsewth@alt <- coords[3]
		
		#dmatrix <- matrix(as.numeric(unlist(strsplit(trim(dlines[(max(ihdr)+2):length(dlines)]), "[[:space:]]+"))), ncol=length(colinfo), byrow=TRUE)
		#dmatrix[dmatrix==-9999] <- NA
		#dmatrix <- as.data.frame(dmatrix)
		
		if(delim==" " | delim==""){
			dmatrix <- read.table(fsefile, skip=max(ihdr)+1, stringsAsFactors=FALSE)	
		} else {
			dmatrix <- read.table(fsefile, skip=max(ihdr)+1, stringsAsFactors=FALSE, sep=delim)
		}
		
		colnames(dmatrix) <- datacols
				
		if(!skip.hdr & std.vals){
			# CHECK RADIATION UNITS THEN CONVERT TO MEGAJOULE/SQM/DAY IF NECESSARY
			
			# Check if sunshine hours/duration
			rad_var <- grep("sunshine[[:print:]]*", tolower(colinfo), ignore.case=TRUE)		
			if (length(rad_var)!=0){
				dmatrix[,rad_var] <- round(sunhoursToSRad(dmatrix[,rad_var],dmatrix[,3],fsewth@lat, coords[4], coords[5]),2)
				show.message("Sunshine duration", appendLF=TRUE)
				
			} else {
				rad_var <- grep("[[:print:]]*rad[[:print:]]*", tolower(colinfo), ignore.case=TRUE)
				if(length(rad_var)!=0 & grepl("kj", colinfo[rad_var],ignore.case=TRUE)) {
					dmatrix[,rad_var] <- round(dmatrix[,rad_var]/1000,2)
				} 
				
			}			
		}
		
		wdate <- dateFromDoy(dmatrix[,"doy"],dmatrix[,"year"])
		fsewth@w <- cbind(wdate,as.data.frame(dmatrix[,4:length(datacols)]))		
		#fsewth@rmk <- ifelse(length(i)==1, trim(unlist(strsplit(dlines[i],":"))[2]),"")
		
	} else {
		stop(fsefile, " not found.")
	}
	return(fsewth)
}


.toFSEFile <- function(wthdat, country="WORLD", station="", author="Geoclimate (IRRI-GIS Climate Data Package)", format="csv", comments="", savepath=getwd()){
	# standard checks
	if (class(wthdat)!="weather"){
		stop("Unsupported data format. Should of class \"weather\"")
	}
	vars <- c("srad", "tmin", "tmax", "vapr", "wind", "prec")
	if(sum(vars %in% colnames(wthdat@w))<length(vars)){
		stop("Incomplete data. ", paste(vars[!vars %in% colnames(wthdat@w)],collapse=", "), " not found.")
	}
	
	#Override Station
	if (station!="") wthdat@stn <- station	
	
	hdrspec <- c(  paste("*  Author      :", author, "   -99.: nil value"),
			paste("*  Source      :", wthdat@rmk),
			"*",
			paste("*  Comments    :", comments))
	
	hdrvars <- c(  "*  Column    Daily Value",
			"*     1      Station number",
			"*     2      Year",
			"*     3      Day",
			"*     4      irradiance         KJ m-2 d-1",
			"*     5      min temperature            oC",
			"*     6      max temperature            oC",
			"*     7      vapor pressure            kPa",
			"*     8      mean wind speed         m s-1",
			"*     9      precipitation          mm d-1")
	
	hdrstn <- c(paste("*  Station Name: Geoclimate Pixel", wthdat@stn),
				paste("*  Longitude:  ", sprintf("%.2f", wthdat@lon), "    Latitude:", sprintf("%.2f", wthdat@lat), "    Altitude:  ", wthdat@alt ,"m"))
	
	hdrbar <- paste("*", paste(rep("-",max(nchar(c(hdrspec,hdrvars, hdrstn)))), collapse=""),sep="")
	
	wthdat@w$year <- as.numeric(format(wthdat@w$wdate, "%Y"))
	wthdat@w$doy <- as.numeric(format(wthdat@w$wdate, "%j"))
	
	
	
	if (format=="csv"){
		locstr <- paste(sprintf("%.2f", wthdat@lon), sprintf("%.2f", wthdat@lat),format(sprintf("%.1f", wthdat@alt), width=5), "0.00", "0.00", sep=", ")
		dat <- paste(wthdat@stn, wthdat@w$year, wthdat@w$doy, wthdat@w$srad, wthdat@w$tmin, wthdat@w$tmax, wthdat@w$vapr, wthdat@w$wind, wthdat@w$prec, sep=", ")		
	} else if (format=="fixed"){
		locstr <- paste(sprintf("%.2f", wthdat@lon), sprintf("%.2f", wthdat@lat),format(sprintf("%.1f", wthdat@alt), width=5), "0.00", "0.00")
		dat <- paste(wthdat@stn, sprintf("%6.0d", wthdat@w$year), format(wthdat@w$wdate, " %j", width=6), sprintf("%10.0f", wthdat$srad[d]), sprintf("%8.1f", wthdat$tmin[d]), sprintf("%8.1f", wthdat$tmax[d]), sprintf("%8.1f", wthdat$vapr[d]), sprintf("%8.1f", wind), sprintf("%8.1f", wthdat$prec[d]))		
	} 
	
	dat <- gsub("NA", "-99.", dat)
	
	years <- unique(wthdat@w$year)	
	wthstrs <- list()
	files <- vector()
	for (yy in years){
		fname <- paste(savepath, "/", country, wthdat@stn, ".", substr(yy, 2,4),sep="")
		files <- c(files,fname)
		writeLines(c(hdrbar,hdrspec,hdrstn,hdrvars,hdrbar,locstr, dat[wthdat@w$year==yy]),fname)
	}
	
	return(files)	
	
}


if ( !isGeneric("write.fse") ) {
	setGeneric("write.fse", function(wth, writeto, ...)
				standardGeneric("write.fse"))
}


setMethod("write.fse", signature(wth="weather", writeto="character"),
	function(wth, writeto, ...){
		return(.toFSEFile(wthdat=wth, savepath=writeto, ...))
	}
)

setMethod("write.fse", signature(wth="list", writeto="character"),	
	function(wth, writeto, ...){
		files <- vector()
		for (i in 1:length(wth)){
			if (class(wth[[i]])!="weather") {
				warning("Class ", class(wth[[i]]), " cannot be written as FSE weather file. Skipped.")				
			} else {
				files <- c(files,.toFSEFile(wthdat=wth[[i]], savepath=writeto, ...))				
			}
		}			
		return(files)
	}
)
