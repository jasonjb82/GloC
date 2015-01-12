# Author: Jorrel Khalil S. Aunario, jaunario@gmail.com
# Date :  20 January 2012
# Version 0.0.1
# Licence GPL v3

# Current ftp site
GSOD.ftp <- "ftp://ftp.ncdc.noaa.gov/pub/data/gsod"
# Reference to var values for parsing downloaded data
GSOD.varrefs <- read.csv(system.file("GSOD.varrefs.csv", package="geoclimate"), stringsAsFactors=FALSE)

GSOD.readStations <- function(stationfile=system.file("GSOD.stations.csv", package="geoclimate"), rm.nodata=FALSE, rm.nocoords=TRUE){
    show.message("Reading GSOD station info file.", appendLF=TRUE)					
    stations <- read.csv(stationfile, stringsAsFactors=FALSE)
    if(rm.nodata) stations <- stations[-which(is.na(stations$BEGIN)),]
    if(rm.nocoords) {
		stations <- stations[-which(stations$LAT==-99999|is.na(stations$LAT)),]
		stations <- stations[-which(stations$LON==-99999|is.na(stations$LON)),]
	}
    stationid <- paste(sprintf("%06d",stations$USAF), sprintf("%05d", stations$WBAN), sep="-")
    stations <- cbind(stationid,stations, stringsAsFactors=FALSE)
    
    # Change to float
    stations$LAT <- stations$LAT
    stations$LON <- stations$LON
    stations$ELEV..1M.[stations$ELEV.M.==-99999] <- NA
    stations$ELEV..1M. <- stations$ELEV.M.
    # Rename elevation fieldname 
    colnames(stations)[colnames(stations)=="ELEV.M."] <- "ELEV1M"
    # Change to date 
    stations$BEGIN <- as.Date(as.character(stations$BEGIN), "%Y%m%d")
    stations$END <- as.Date(as.character(stations$END), "%Y%m%d")
	return(stations)
}

GSOD.stations <- GSOD.readStations()

GSOD.updateStations <- function(){
	success <- FALSE
	if(!require(RCurl)){
		show.message("Error: RCurl package not found.", appendLF=TRUE)		
	} else {
		show.message("Checking file differences.", appendLF=TRUE)
		online <-  unlist(strsplit(getURL("ftp://ftp.ncdc.noaa.gov/pub/data/noaa/"),ifelse(Sys.info()["sysname"]=="Windows","\r\n","\n")))
		oinfo <- unlist(strsplit(online[grep("ISD-HISTORY.CSV$", online, ignore.case=TRUE)],"[[:space:]]+"))
		
		age <- difftime(as.Date(paste(oinfo[6:7], collapse=" "), "%b %d"),file.info(system.file("GSOD.stations.csv", package="geoclimate"))$ctime, units="weeks")
		size <- as.numeric(oinfo[5])-file.info(system.file("GSOD.stations.csv", package="geoclimate"))$size

		if (age>2 | size!=0){
			if(!file.copy(system.file("GSOD.stations.csv", package="geoclimate"),paste(system.file("GSOD.stations.csv", package="geoclimate"),".bck",sep=""),overwrite=TRUE)){
				show.message("Unable to create station data backup file. GSOD update process aborted.", appendLF=TRUE)
			} else {
				show.message("Downloading station info file from GSOD FTP site.", EL=TRUE, appendLF=FALSE)
				dl.success <- withRetry(download.file(paste("ftp://ftp.ncdc.noaa.gov/pub/data/noaa/",oinfo[9],sep=""),system.file("GSOD.stations.csv", package="geoclimate"),mode="wb"))
				if (dl.success!=0){
					show.message("Failed to connect GSOD FTP site.", appendLF=TRUE)
					file.copy(system.file("GSOD.stations.csv.bck", package="geoclimate"),system.file("GSOD.stations.csv", package="geoclimate"),overwrite=TRUE)
				} 
				show.message("GSOD Stations info file update complete.", EL=TRUE, appendLF=TRUE)
				success <- TRUE
			}		
			
		} else {		
			show.message("GSOD station file is upto date.", appendLF=TRUE)
			success <- TRUE
		}
	}
}

#gsod.download <- function(gsodurl,fname=basename(gsodurl), ...){
#	dl.success <- FALSE
#	gsodzip <- withRetry(getBinaryURL(gsodurl), ...)
#	if (class(gsodzip)!="try-error") {
#		writeBin(gsodzip, fname)
#		dl.success <- TRUE
#	}
#	return(dl.success)
#}
get.gsod <- function(station, year=as.numeric(format(Sys.Date(),"%Y")), savepath=getwd(), rm.existing=FALSE,...){

    # check for RCurl package
    if(!require(RCurl)){
		stop("RCurl package not found.")
	}

    # check for write permissions
	if(!force.directories(savepath, recursive=TRUE)){
		stop("Can't create download path.")
	} 

	sindex <- grep(station, GSOD.stations$stationid)
    #check if station exists
    if (length(sindex)!=1){
        stop("Can't find station ", station,"\nTry updating GSOD stations file.")
    } 
	
	# prepare weather object
	result <- new("weather")
	result@stn <- paste(GSOD.stations$STATION.NAME[sindex], " (", station, ")", sep="")
    result@lon <- GSOD.stations$LON[sindex]
	result@lat <- GSOD.stations$LAT[sindex]
	result@alt <- GSOD.stations$ELEV1M[sindex]    

	fname <- paste(station,"-",year,".op.gz", sep="")
	ftpurl <- paste(GSOD.ftp, year, fname, sep="/")
	
	# Download gsod file if necessary
	if (!file.exists(paste(savepath,fname, sep="/"))){
		dl <- withRetry(download.file(ftpurl, destfile = paste(savepath, fname, sep = "/"), mode = "wb", quiet = TRUE), ...)				
	} else if (rm.existing | file.info(paste(savepath, fname, sep="/"))$size==0){
		# Remove existing downloaded file
		file.remove(paste(savepath,fname, sep="/"))
		dl <- withRetry(download.file(ftpurl, destfile = paste(savepath, fname, sep = "/"), mode = "wb", quiet = TRUE), ...)	
	}
	
	gz <- gzfile(paste(savepath,fname,sep="/"))
	dlines <- readLines(gz)
	gz <- close(gz)			

	# Parse the gsod file if successfully downloaded
	if (length(dlines)>0){
		
		dat <- vector()
		#Parsing the GSOD file
		for (i in 1:14){
			tmp <- trim(substr(dlines[-1], GSOD.varrefs$stpos[i], GSOD.varrefs$enpos[i]))
			if(!is.na(GSOD.varrefs$missing[i])) {
				tmp[tmp==as.character(GSOD.varrefs$missing[i])] <- NA				
			}
			dat <- cbind(dat,tmp)
		}
		
		colnames(dat) <- GSOD.varrefs$variable[1:14]
		dat <- as.data.frame(dat, stringsAsFactors=FALSE)
		gsod <- data.frame(tavg=numeric(nrow(dat)),slpressure=numeric(nrow(dat)),stpressure=numeric(nrow(dat)),tdew=numeric(nrow(dat)),visibility=numeric(nrow(dat)),wind=numeric(nrow(dat)),maxwind=numeric(nrow(dat)),gust=numeric(nrow(dat)),tmax=numeric(nrow(dat)),tmin=numeric(nrow(dat)),prec=numeric(nrow(dat)),snowdepth=numeric(nrow(dat)))
		# CLEAN UP CLIMATE DATA
		gsod$tavg 		<- round(FtoC(as.numeric(dat$TEMP)),1)*10 # MEAN TEMP
		gsod$slpressure <- as.numeric(dat$SLP)*10  # SEA LEVEL PRESSURE
		gsod$stpressure <- as.numeric(dat$STP)*10  # STATION PRESSURE
		gsod$tdew 		<- round(FtoC(as.numeric(dat$DEWP)),1)*10  # MEAN DEW POINT
		gsod$visibility <- round((as.numeric(dat$VISIB) * 1.609344),1)*10 # VISIBILITY
		
		##############################################
		# WINDSPEED NEEDED IN ORYZA2k
		gsod$wind  		<- round(as.numeric(dat$WDSP) * 0.514444444,1)*10 # WIND SPEED
		gsod$maxwind 	<- round(as.numeric(dat$MXSPD) * 0.514444444,1)*10  # MAX SUSTAINED SPEED
		gsod$gust  		<- round(as.numeric(dat$GUST) * 0.514444444,1)*10  # MAX GUST
	  
		##############################################
		# MAX T NEEDED IN ORYZA2k
		gsod$tmax   <- round(FtoC(as.numeric(dat$MAX)),1)*10  # MAX T
	  
		##############################################
		# MIN 2 NEEDED IN ORYZA2k
		gsod$tmin   <- round(FtoC(as.numeric(dat$MIN)),1)*10  # MIN T
	
		##############################################
		# RAINFALL NEEDED IN ORYZA2k
		gsod$prec   <- round(as.numeric(dat$PRCP)*25.4,1)*10  # RAINFALL
		
		##############################################
		# SNOW DEPTH
		gsod$snowdepth   <- round(as.numeric(dat$SNDP)*25.4,1)*10  # convert to mm
		
		indicators <- matrix(as.numeric(unlist(strsplit(dat$FRSHTT,""))),byrow=TRUE, ncol=6)
		colnames(indicators) <- c("ifog","irain","isnow","ihail","ithunder","itornado") 
		
		wdate <- as.Date(dat$YEARMODA,"%Y%m%d")
		
		gsod <- cbind(wdate, gsod, indicators, stringsAsFactors=FALSE)
		# TODO: get from database?
		result@w <- gsod
	} else {
        # result@rmk <- paste("Download failed for", year)  
		result@rmk <- as.character(dl)  
    }	
	return(result)
}
