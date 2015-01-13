# Author: Jorrel Khalil S. Aunario, jaunario@gmail.com
# Date :  8 March 2012
# Version 0.0.1
# Licence GPL v3
# Update NASA table

#Load libraries
library(GloC)
library(mailR)
library(RODBC)

getCols <- function(log, columns){
  ch.cols <- columns[log]
  toproc <- ch.cols[ch.cols %in% c("tmin", "tmax", "tdew", # Bias-correction and downscaling
                                   "srad", "rh2m", "wind"  # downscaling
                                   )]
  return(paste(toproc, collapse=", "))
}

# Location of downloaded NASA files
# sdir <- paste(getwd(),"NASA",sep="/")
sdir <- "/home/gisadmin/Projects/gneo/temp/NASA"

# connect to database schema <- "geoclim_raw"
schema <- commandArgs(trailing=TRUE)[1]
if(Sys.info()[1]=="Linux"){
	#con <- odbcDriverConnect(paste("DRIVER=MySQL;SERVER=localhost;DATABASE=", schema, ";USER=root;PASSWORD=G!5admIn_2012;OPTION=27;",sep=""))
	con <- odbcDriverConnect(paste("DRIVER=MySQL;SERVER=localhost;DATABASE=", schema, ";USER=geoclim_admin;PASSWORD=cL!m8_2013A;OPTION=27;", sep=""))            
} else {
	con <- odbcConnect("geoclimate")
}

wset <- commandArgs(TRUE)[2]
# check if valid weather set
gtables <- sqlTables(con)$TABLE_NAME
if(!wset %in% gtables) {
    stop("Invalid wset argument. ", wset, " Not a valid NASA table. Choose from ", paste(gtables[grep('nasa',gtables)], collapse=", "))
} 

part <- try(as.numeric(commandArgs(TRUE)[3]))
# check part argument
if(class(part)=="try-error") {
    stop("Invalid part argument. Not a number. Should be a number 1..5.")
} else if (part>5){
    stop("Invalid part argument. Should be a number 1..5.")
}

year <- try(as.numeric(commandArgs(TRUE)[4]))
# check part argument
if(class(year)=="try-error") {
  stop("Invalid part argument. Not a number. Should be a number 1..5.")
} else if (is.na(year)){
  year <- as.numeric(format(Sys.Date(),"%Y"))
  st <- as.Date(paste(year,"-1-1",sep=""))
  uen <- Sys.Date()-as.numeric(format(Sys.Date(),"%d"))-as.numeric(format(Sys.Date()-as.numeric(format(Sys.Date(),"%d")),"%d"))
} else if (year<1983 | year>as.numeric(format(Sys.Date(),"%Y"))){
  stop("Invalid year argument. Should be between 1983 and ", as.numeric(format(Sys.Date(),"%Y")), ".")
} else {
  st <- as.Date(paste(year,"-1-1",sep=""))  
  uen <- min(as.Date(paste(year,"-12-31",sep="")),Sys.Date()-as.numeric(format(Sys.Date(),"%d"))-as.numeric(format(Sys.Date()-as.numeric(format(Sys.Date(),"%d")),"%d")))
}



for (i in 0:1){
  #if(i==0 & part==1) sqlClear(con,wset)
	batch <- (part-1)+(i*5)
	cells <- 1:6480+(6480*batch)

	success <- failed <- proc <- vector()
  attachments <- vector()
	procst <- Sys.time()
  # Update til two months prior to the current month
	
	#uen <- as.Date("2013-7-31")	
	base <- raster()
	for (cell in cells){
		cellst <- Sys.time()
		xy <- xyFromCell(base,cell)
		
		show.message("Batch ", batch+1, " Cell #", cell,": Checking existing records from", st, " to ", uen, ".\r", appendLF=FALSE, EL=TRUE)	  
	  exist <- fetch.daily(xy,srcvars=list(nasacomp_1d="all"),con, stdate=st, endate=uen)[[1]]
    
		show.message("Batch ", batch+1, " Cell #", cell,": Connecting to NASA-POWER website.\r", appendLF=FALSE, EL=TRUE)
		dat <- withRetry(get.nasa(xy[1,'x'],xy[1,'y'], stdate=st, endate=uen, savepath=sdir, rm.existing=FALSE))

  	if (length(dat@w)==0){
			show.message("Batch ", batch+1, " Cell #", cell,": ", dat@rmk, EL=TRUE)
			failed <- rbind(failed,c(cell, dat@rmk))
			next
		}
		
		show.message("Batch ", batch+1, " Cell #", cell,": Comparing local and web data.\r", appendLF=FALSE, EL=TRUE)

    # New records
    newd <- dat@w[which(!(as.Date(dat@w$date) %in% exist@w$wdate)),]
		if(nrow(newd)==0) {
		  show.message("Batch ", batch+1, " Cell #", cell,": No new records to upload.", appendLF=TRUE, EL=TRUE)
		} else {
      dat2 <- dat
      dat2@w <- newd
      
      dat2@w[,-1] <- dat2@w[,-1]*100
      upcols <- c("wdate","toa_dwn", "srad", "lwv_dwn", "tavg", "tmin", "tmax", "rh2m", "tdew", "prec", "wind")
      
      tries <- 0
      repeat {
        show.message("Batch ", batch+1, " Cell #", cell,": Uploading ", nrow(newd), " records from the web.\r", appendLF=FALSE, EL=TRUE)
        successful <- suppressMessages(upload.nasa(con, dat2, setname=wset, cols=upcols))
        if(!successful){
          show.message("Batch ", batch+1, " Cell #", cell,": Update failed.", EL=TRUE)
          show.message("Batch ", batch+1, " Cell #", cell,": Trying to resolve database issues. \r", appendLF=FALSE, EL=TRUE)
          if (retry==0){
            sqlQuery(con, "flush tables;")    
          } else if (retry==1){
            con <- odbcClose(con)
            con <- odbcDriverConnect(paste("DRIVER=MySQL;SERVER=localhost;DATABASE=", schema, ";USER=geoclim_admin;PASSWORD=cL!m8_2013A;OPTION=27;", sep=""))            
          }
          show.message("Batch ", batch+1, " Cell #", cell,": Performing clean-up before retry.\r", appendLF=FALSE, EL=TRUE)
          delq <- paste("DELETE FROM", wset, "WHERE cell =", cell, "AND wdate BETWEEN ", newd$date[1], " AND ", newd$date[nrow(newd)])
          sqlQuery(con, delq)
          tries <- tries + 1
        } else {
          cellen <- Sys.time()
          success <- rbind(success, c(cell, newd$date[1], newd$date[nrow(newd)], nrow(dat@w),as.character(cellst),as.character(cellen), format(cellen-cellst)))
          show.message("Batch ", batch+1, " Cell #", cell,": Update Done.", EL=TRUE)
          break
        }
        if (tries>3){
          show.message("Batch ", batch+1, " Cell #", cell,": Attempt futile. Try next time.")
          failed <- rbind(failed,c(cell, "Upload failed"))
          break
        }		
      }		  
		}
		
    # Update old
    # Check dates first
		
    exd <- exist@w[order(exist@w$wdate),] # ensure dates are in order
    upd <- dat@w[which(as.Date(dat@w$date) %in% exist@w$wdate),]
		
    ed <- as.matrix(exd[,-1])
		ud <- as.matrix(upd[,-1])
    
    up.log <- xor(is.na(ed), is.na(ud)) # Checks if new values are available for once missing data
    up.log[which(ud!=ed)] <- TRUE # Checks if new values are available for existing data		
    up.cols <- which(colSums(up.log, na.rm=TRUE)>0)
            
    if(length(up.cols)>0){
      up.rows <- which(rowSums(up.log, na.rm=TRUE)>0)      
      show.message("Batch ", batch+1, " Cell #", cell,": Found new values for ", length(up.rows)," records on ", length(up.cols), " variables.", EL=TRUE)
      ed[up.log] <- ud[up.log]      
      exd[,-1] <- ed*100
      exd <- cbind(cell,exd[up.rows,c(1,up.cols+1)])
      
      sqlUpdate(con, exd, tablename=wset, index=c("cell","wdate"))
      vars <- unlist(apply(up.log[up.rows,],1,getCols,colnames(up.log)))
      toproc.inc <- which(vars!="")
      if(length(vars)>0){
        cell.proc <- data.frame(wdate=exd$wdate[toproc.inc],vars[toproc.inc])
        proc <- rbind(proc, cell.proc)
      } 
    }
	}

  proc <- unique(proc)
  
  if(length(failed)!=0) attachments <- c(attachments, "failed")
  if(length(proc)!=0) attachmnts <- c(attachments, "proc")
  if(length(success)!=0) attachments <- c(attachments, "success")
  save(list=attachments, file=paste(paste("~/logs/geoclimate/nasa_1d_batch", batch+1,format(Sys.Date(),"%F"),sep="_"),".Rdata",sep=""))
	procen <- Sys.time()

	eto <- "j.aunario@irri.org"
	efrom <- "irrigis@gmail.com"
	esubject <- paste("NASA Update Part", batch+1, "of 10")
	ebody <- c("Update NASA POWER completed","",
	paste("Dataset:", wset),
	paste("Start:",format(procst, "%d %b %Y %X")),
	paste("End:", format(procen, "%d %b %Y %X")),
	paste("Time Elapsed:",format(procen-procst)),"",
	paste("Last cell run:", cell), 
	paste("No. of Cells Scheduled:", length(cells)),
	paste("No. processed cells:", ifelse(is.null(nrow(success)),0,nrow(success))),
	paste("No. failed cells:", length(failed)),
	paste("Failed cells:", paste(failed, collapse=", ")),
	"\n\n\n---------------------------",
	paste("See ", paste("~/logs/geoclimate/nasa_1d_batch", batch+1,format(Sys.Date(),"%F"),sep="_"),".Rdata for log details",sep=""),
	"System generated message. DO NOT REPLY."
	)

	#writeLines(body,paste("/home/gisadmin/logs/",schema, "_", wset , "_", batch, "_", format(Sys.time(),"%Y-%m-%d"), ".txt",sep=""))
  
#   if(class(success)=="matrix" | class(success)=="data.frame"){
# 		colnames(success) <- c("cell", "update start", "update end", "records", "start", "end", "proc length")
#     attachment <- mime_part(as.data.frame(success), "uploaded")		
# 	} else attachment <- NULL	
# 	
# 	if(class(failed)=="matrix" | class(failed)=="data.frame"){
# 		colnames(failed) <- c("cell", "remarks")
# 		attachment2 <- mime_part(as.data.frame(failed), "failed")		
# 	} else attachment2 <- NULL
	
	#body <- list(paste(ebody,collapse="\n"), attachment, attachment2)
	send.mail(from=efrom, to=eto, subject=esubject, body=paste(ebody,collapse="\n"), smtp=list(host.name="smtp.gmail.com", port=587, user.name="irrigis", passwd="Ragnarok09", tls=TRUE), authenticate=TRUE, send=TRUE)
	#sqlQuery(con, "flush tables;")
	gc(verbose=FALSE)
}

con <- odbcClose(con)
