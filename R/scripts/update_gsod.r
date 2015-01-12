# Author: Jorrel Khalil S. Aunario, jaunario@gmail.com
# Date :  8 March 2012
# Version 0.0.1
# Licence GPL v3
# Update GSOD table

#Load libraries
library(geoclimate)
library(RCurl)
library(mailR)
library(RODBC)

schema <- commandArgs(trailing=TRUE)[1]
wset <- commandArgs(trailing=TRUE)[2]
stations <- commandArgs(trailing=TRUE)[3]
year <- commandArgs(trailing=TRUE)[4]
resume <- commandArgs(trailing=TRUE)[5]


# Location of downloaded GSOD files
if(Sys.info()[1]=="Linux"){
	sdir <- "/home/gisadmin/Projects/gneo/temp/GSOD"
  if(!file.exists(sdir)) force.directories(sdir)
} 

# connect to database
if(Sys.info()[1]=="Linux"){
	con <- odbcDriverConnect(paste("DRIVER=MySQL;SERVER=localhost;DATABASE=", schema, ";USER=geoclim_admin;PASSWORD=cL!m8_2013A;OPTION=27;"))
} else {
	#con <- odbcConnect("geoclimate")
	con <- odbcConnect(schema)
}

year <- ifelse(is.na(year),yearFromDate(Sys.Date()),as.numeric(year))

yfile <- paste("gsod_",year,".tar",sep="")

if (is.na(resume)){
  sqlQuery(con, paste("DELETE FROM", wset, "WHERE wdate>", paste("'",year-1,"-12-31'",sep="")))
  sqlQuery(con, paste("UPDATE ", stations, "SET end=NULL WHERE YEAR(end)=", year))
  
  localsize <- file.info(paste(sdir,yfile,sep="/"))$size
  if (is.na(localsize)){
    # Download archive if file does not exist locally
    message("Y:", year, " > Downloading from GSOD server.", appendLF=TRUE)
    withRetry(download.file(paste(GSOD.ftp,"/",year, "/gsod_", year, ".tar", sep=""), destfile=paste(sdir,yfile,sep="/"), mode='wb'))
  } else {    
    message("Y:", year, " > Retrieving archive info from website.\r", appendLF=FALSE)
    online <-  withRetry(unlist(strsplit(getURL(paste(GSOD.ftp,"/",year,"/",sep="")),"\r\n")), retries=2, delay=30) # get list of files available for specified year
    websize <- as.numeric(unlist(strsplit(online[grep(yfile,online)],"[[:space:]]+"))[5])
    if (websize!=localsize) { # Download archive if file size on the web is different from the local
      message("Y:", year, " > Downloading from GSOD server.", appendLF=TRUE)
      unlink(paste(sdir,yfile,sep="/"))
      unlink(paste(sdir,year,sep="/"), recursive=TRUE)
      withRetry(download.file(paste(GSOD.ftp,"/",year, "/gsod_", year, ".tar", sep=""), destfile=paste(sdir,yfile,sep="/"), mode='wb'))
    }
  }
  
  message("Y:", year, " > Decompressing archive file.", appendLF=TRUE)
  untar(paste(sdir,yfile,sep="/"),verbose=FALSE, exdir=paste(sdir,year,sep="/"), extras="--no-same-owner")  
}

success <- vector()
failed <- vector()
procst <- Sys.time()
	
for (j in 1:nrow(GSOD.stations)){
#for (j in 24981:nrow(GSOD.stations)){    
	stnst <- Sys.time()
	stn <- GSOD.stations$stationid[j]
	stfile <- paste(sdir,year,paste(stn,"-",year,".op.gz",sep=""),sep="/")
	
	if (!file.exists(paste(sdir,year,paste(stn,"-",year,".op.gz",sep=""),sep="/"))){
		message("Y:", year, "| S: ", stn," > not available.", appendLF=TRUE)
		next # skip if no available download
	}
	
	# Check if station in db
	add <- FALSE
	message("Y:", year, "| S: ", stn," > Checking records in server.\r", appendLF=FALSE)
	db.station <- sqlQuery(con, paste("SELECT * FROM", stations, "where station_code =",shQuote(stn)), stringsAsFactors=FALSE)
	#db.station$begin <- as.Date(db.station$begin)
	#db.station$end <- as.Date(db.station$end)

	
	if (nrow(db.station)==0) { # If no record in local server
		add <- TRUE				
		dbid <- sqlQuery(con, "SELECT IF(MAX(station_id) IS NULL,1,MAX(station_id)+1) station_id FROM gsod_stations")$station_id
		db.station[1,] <- NA
		db.station[1,] <- cbind(dbid,cellFromXY(raster(),GSOD.stations[j,c("LON", "LAT")]),GSOD.stations[j,c("stationid", "STATION.NAME", "CTRY", "FIPS", "STATE", "CALL", "LAT", "LON","ELEV1M")], NA, NA)
        if (class(db.station$call)!="character") db.station$call <- as.character(db.station$call)				
	} else dbid <- db.station$station_id[1] 
	
	if (!is.na(db.station$end[1]) & year<yearFromDate(db.station$end[1])){ # Check if current year already uploaded
		message("Y:", year, "| S: ", stn," > Already uploaded.", appendLF=TRUE)
		next
	} 
				
	# Download data from GSOD.ftp
	message("Y:", year, "| S: ", stn," > Connecting to GSOD website.\r", appendLF=FALSE)
	#TODO debug further
	dat <- get.gsod(stn, year=year, savepath=paste(sdir,year,sep="/"), rm.existing=FALSE)
	
	# TODO get index of start of data for upload based on date
	if (length(dat@w)==0){
		message("Y:", year, "| S: ", stn," > ", dat@rmk)
		if(class(failed)=="logical"){
			failed <- data.frame(GSOD.stations$stationid[j], dat@rmk)
		} else {
			failed <- rbind(failed,c(GSOD.stations$stationid[j], dat@rmk))
		}
		next
	}
	# Remove records falling behind the latest existing record
	if (!is.na(db.station$end[1])) dat@w <- dat@w[dat@w$wdate>db.station$end[1],]
	
	if (nrow(dat@w)==0){
		message("Y:", year, "| S: ", stn," > Already uploaded.", appendLF=TRUE)
		next
	}
	
	# Use database integer id for station
	dat@stn <- as.character(dbid)
	
	tries <- 0
	repeat {
		message("Y:", year, "| S: ", stn," > Uploading new data. (", dat@w$wdate[1], " to " , dat@w$wdate[nrow(dat@w)], ")\r", appendLF=FALSE)				
		successful <- suppressMessages(upload.gsod(con, dat, setname=wset))
		if(!successful){
			message("Y:", year, "| S: ", stn," > Update failed.\r", appendLF=TRUE)
			message("Y:", year, "| S: ", stn," > Trying to resolve database issues. \r", appendLF=FALSE)
            if (tries==0){
                sqlQuery(con, "flush tables;")    
            } else if (tries==1){
                con <- odbcClose(con)
                con <- odbcDriverConnect("DRIVER=MySQL;SERVER=localhost;DATABASE=geoclim_pro;USER=geoclim_admin;PASSWORD=cL!m8_2013A;OPTION=27;")            
            }
            message("Y:", year, "| S: ", stn," > Performing clean-up before retry.\r", appendLF=FALSE)
			delq <- paste("DELETE FROM", wset, "WHERE station_id =", dbid, "AND wdate>=", format(dat@w$wdate[1],"'%Y-%m-%d'"))
			sqlQuery(con, delq)
			tries <- tries + 1
		} else {
			stnen <- Sys.time()
			if (class(success)=="logical") {
				success <- data.frame(GSOD.stations$stationid[j], as.character(dat@w$wdate[1]), as.character(dat@w$wdate[nrow(dat@w)]), nrow(dat@w),as.character(stnst),as.character(stnen), format(stnen-stnst), stringsAsFactors=FALSE)
			} else {
				success <- rbind(success, c(GSOD.stations$stationid[j], as.character(dat@w$wdate[1]), as.character(dat@w$wdate[nrow(dat@w)]), nrow(dat@w),as.character(stnst),as.character(stnen), format(stnen-stnst)))
			}
			message("Y:", year, "| S: ", stn," > Update Done.", appendLF=TRUE)
			db.station$begin <- min(as.Date(db.station$begin), dat@w$wdate, na.rm=TRUE)
			db.station$end <- max(as.Date(db.station$end), dat@w$wdate, na.rm=TRUE)

			if (add) {
                sqlSave(con, db.station, tablename=stations, append=TRUE, rownames=FALSE)
            } else {
                db.station <- db.station[,c("station_id","begin","end")]
                sqlUpdate(con, db.station, tablename=stations, index="station_id")
            }
			break
		}
		if (tries>3){
			message("Y:", year, "| S: ", stn," > Attempt futile. Try next time.", appendLF=TRUE)
			if(class(failed)=="logical"){
				failed <- data.frame(GSOD.stations$stationid[j], "Upload failed")
			} else {
				failed <- rbind(failed,c(GSOD.stations$stationid[j], "Upload failed"))
			}				
			break
		}		
	}
    gc(verbose=FALSE)
}
con <- odbcClose(con)
unlink(paste(sdir,yfile,sep="/"))
procen <- Sys.time()

eto <- "j.aunario@irri.org"
efrom <- "irrigis@gmail.com"
esubject <- paste("GSOD Update for", year)
ebody <- c("Update GSOD completed","",
paste("Dataset:", wset),
paste("Start:",format(procst, "%d %b %Y %X")),
paste("End:", format(procen, "%d %b %Y %X")),
paste("Time Elapsed:",format(procen-procst)),"",
paste("Last stn run:", stn), 
paste("No. of stations Scheduled:", nrow(GSOD.stations)),
paste("No. processed stations:", ifelse(is.null(nrow(success)),0,nrow(success))),
paste("No. failed stations:", ifelse(is.null(nrow(failed)),0,nrow(failed))),
paste("Failed stations:", paste(failed, collapse=", ")),
"\n\n\n---------------------------",
"System generated message. DO NOT REPLY."
)

send.mail(from=efrom, to=eto, subject=esubject, body=paste(ebody,collapse="\n"), smtp=list(host.name="smtp.gmail.com", port=587, user.name="irrigis", passwd="Ragnarok09", tls=TRUE), authenticate=TRUE, send=TRUE)

if(Sys.info()[1]=="Linux"){
  writeLines(body,paste("/home/gisadmin/logs/",schema, "_", wset ,"_",year, "_", format(Sys.time(),"%Y-%m-%d"), ".txt",sep=""))
} else {
  writeLines(body,paste(sdir, "/", schema, "_", wset ,"_",year, "_", format(Sys.time(),"%Y-%m-%d"), ".txt",sep=""))  
}

