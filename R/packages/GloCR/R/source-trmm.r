# Author: Jorrel Khalil S. Aunario, jaunario@gmail.com
# Date :  20 February 2012
# Version 0.0.2
# Licence GPL v3
TRMM.ftp <- "ftp://disc2.nascom.nasa.gov/data/TRMM/Gridded/Derived_Products"

get.trmm <- function(ver="v7", wdate="1998-1-1", savepath=getwd(), rm.existing=FALSE,...){
	if(!require(RCurl)) stop("package RCurl not found.")
	
	if (!is.na(savepath)) force.directories(savepath, recursive=TRUE)
	versions <- c("3B42_V6", "3B42_V7", "3B42RT")
	
	ip <- grep(ver, versions, ignore.case=TRUE)
	if (is.na(ip) | length(ip)>1){
		stop("Invalid version. Should be v6, v7 or rt.")
	} else {
		product <- ifelse(toupper(ver) %in% c("V6", "V7"), paste("3B42_",toupper(ver), sep=""),paste("3B42", toupper(ver), sep=""))
	}

	wdate <- as.Date(wdate)
	if ((ip==3) & (wdate < as.Date("2008-10-1"))){
		stop("Date ", wdate," is earlier than start of specified version. ", versions[ip], " started 2008-10-1.")
		#wdate <- as.Date("1998-1-1")
	} else if (wdate < as.Date("1998-1-1")){
		show.message("Date ", wdate," is earlier than start of specified version. ", versions[ip], " started 1998-1-1.")
		#wdate <- as.Date("1998-1-1")
	}
	
	switch(ip,
	{ 	fname <- paste("3B42_daily.", format(wdate, "%Y.%m.%d"),".6.bin",sep="" )
		prod.ftp <- paste(TRMM.ftp, product, "Daily", yearFromDate(wdate), fname,sep="/")
		},
	{ 	fname <- paste("3B42_daily.", format(wdate, "%Y.%m.%d"),".7.bin",sep="" )
		prod.ftp <- paste(TRMM.ftp, product, "Daily", yearFromDate(wdate), fname,sep="/")
		},
	{ 	fname <- paste("3B42RT_daily.", format(wdate, "%Y.%m.%d"),".bin",sep="" )
		prod.ftp <- paste(TRMM.ftp, product, "Daily", yearFromDate(wdate), fname,sep="/")
		}
	)
    
	if (file.exists(paste(savepath,fname,sep="/")) & rm.existing){
		file.remove(paste(savepath,fname,sep="/"))
		rawtrmm <- withRetry(getBinaryURL(prod.ftp),...)
	} else if (file.exists(paste(savepath,fname,sep="/"))){
		rawtrmm <- getBinaryURL(paste("file://localhost", normalizePath(savepath), fname, sep="/"))			
	} else {
		rawtrmm <- withRetry(getBinaryURL(prod.ftp),...)
	}
	
	wth <- new("weather")
	wth@stn <- "Tropical Rainfall Measuring Mission"
	wth@lon <- c(-180,180)
	wth@lat <- c(-50,50)
	
	if (class(rawtrmm)=="try-error") {
		wth@rmk <- c(prod.ftp,rawtrmm)
		wth@w <- data.frame(cell=numeric(0), wdate=character(0), prec=numeric(0), stringsAsFactors=FALSE)
		warning(rawtrmm)
	} else {
		if (class(savepath)=="character" & !file.exists(paste(savepath,fname,sep="/"))) writeBin(rawtrmm, paste(savepath,fname,sep="/"))
		
		wth@rmk <- prod.ftp
		trmmraster <- raster(extent(0,360,-50,50))
		baseraster <- raster()
		res(trmmraster) <- res(baseraster) <- 0.25
		txy <- as.data.frame(xyFromCell(trmmraster, 1:ncell(trmmraster)))
		txy$x[txy$x>180] <- txy$x[txy$x>180]-360
		cell <- cellFromXY(baseraster, txy)		
		
		prec <- matrix(readBin(rawtrmm, double(), endian="big", size=4, n=ncell(trmmraster)), ncol=ncol(trmmraster), nrow=nrow(trmmraster), byrow=TRUE)
		prec[prec<0] <- NA
		# TODO: review why this was done
		prec <- prec[nrow(prec):1,]
		
		trmmraster[] <- prec

		wth@w <- as.data.frame(cell)
		wth@w$wdate <- as.character(wdate)
		wth@w$prec <- values(trmmraster)
		wth@w <- wth@w[order(cell),]
		rm(trmmraster, baseraster, prec, cell)
		gc(verbose=FALSE)		
	}	
	
	return(wth)
}

trmm.monthly <- function(month=1,year=1998, savepath=getwd(), rm.old=FALSE){
    if (!require(ncdf)) stop("Package ncdf not found.")
	
	if (!force.directories(savepath)) stop("Can not create di") 
    doy <- doyFromDate(paste(year,month,1,sep="-"))
    if (year<2007){
        fname <- paste("3B43.",substr(year,3,4),sprintf("%02d",month),"01.6",sep="")
        src <- paste("http://disc2.nascom.nasa.gov/daac-bin/OTF/HTTP_services.cgi?FILENAME=%2Fdata%2Fs4pa%2FTRMM_L3%2FTRMM_3B43%2F", year,"%2F",sprintf("%03d",doy),"%2F",fname,".HDF&LABEL=",fname,".nc&SHORTNAME=TRMM_3B43&SERVICE=HDF_TO_NetCDF&VERSION=1.02", sep="")    
    } else {
        fname <- ifelse(year==2007, paste("3B43.",substr(year,3,4),sprintf("%02d",month),"01.6",sep=""),paste("3B43.",substr(year,3,4),sprintf("%02d",month),"01.6A",sep="")) 
        src <- paste("http://disc2.nascom.nasa.gov/daac-bin/OTF/HTTP_services.cgi?FILENAME=%2Fftp%2Fdata%2Fs4pa%2FTRMM_L3%2FTRMM_3B43%2F", year,"%2F",sprintf("%03d",doy),"%2F",fname,".HDF&LABEL=",fname,".nc&SHORTNAME=TRMM_3B43&SERVICE=HDF_TO_NetCDF&VERSION=1.02", sep="")        
    }
    
	outfile <- ifelse(outfile=="",paste(fname,".nc",sep=""), outfile)
	if (!file.exists(outfile)){
		withRetry(download.file(src, outfile, method="internal", mode="wb"))
	} else if (rm.old){
		file.remove(outfile)
		withRetry(download.file(src, outfile, method="internal", mode="wb"))
	} 
    
    if (month %in% c(1,3,5,7,8,10,12)){
        multiplier <- 24*31
    } else if (month==2){
        multiplier <- ifelse(isLeapYear(year),24*29,24*28)
    } else {
        multiplier <- 24*30
    }
	
    traster <- try(raster(outfile, varname="pcp"),silent=TRUE)
    if(class(traster)!="try-error"){
    	xy <- xyFromCell(traster,1:ncell(traster))
    	prec <- values(traster)
    	result <- cbind(xy,prec)
    } else {
        show.message(traster, appendLF=TRUE)
        result <- vector()
    }
    
	return(result)
}
