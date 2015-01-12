# Author: Jorrel Khalil S. Aunario, jaunario@gmail.com
# Date :  8 March 2012
# Version 0.0.1
# Licence GPL v3
# Update GSOD table
message("TRMM Upload Started.", appendLF=FALSE)
upst <- Sys.time()
#Load libraries
library(geoclimate)
library(RCurl)
#library(sendmailR)
library(RODBC)

cargs <- commandArgs(trailing=TRUE)
schema <- cargs[1]
ver <- cargs[2]
stdate <- cargs[3]
endate <- ifelse(is.na(cargs[4]),as.character(Sys.Date()-as.numeric(format(Sys.Date(),"%d"))), cargs[4])

if (ver=="rt") stdate <- ifelse(is.na(stdate),"2008-10-1", stdate) else stdate <- ifelse(is.na(stdate),"1998-1-1", stdate)


#ver <- "v7"
dataset <- paste("trmm", ver, "_15m", sep="")
#stdate <- "1998-1-1"
#endate <- "2012-12-31"
#endate <- "2012-4-30"
#rm.existing <- FALSE

world_15m <- raster()
trmm_15m <- raster(extent(0,360,-50,50))
res(trmm_15m) <- res(world_15m) <- 0.25

trmm_cover <- extent(-180,180,-50,50) 
tcells_std <- cellsFromExtent(world_15m,trmm_cover) 
procdates <- seq(from=as.Date(stdate), to=as.Date(endate), by="day")

txy <- as.data.frame(xyFromCell(trmm_15m, 1:ncell(trmm_15m)))
txy$xadj <- txy$x
txy$xadj[txy$xadj>180] <- txy$xadj[txy$xadj>180] - 360
txy$tc <- cellsFromExtent(world_15m,trmm_cover)
txy$wc <- cellFromXY(world_15m,txy[,c("xadj","y")])


# Location of downloaded GSOD files
if(Sys.info()[1]=="Linux"){
	sdir <- "/home/gisadmin/Projects/geoclimate/TRMM"
} else {
	sdir <- "D:/Projects/GeoClimate/Data/TRMM"
}
sdir <- paste(sdir, ver, sep="/")

# connect to database
if(Sys.info()[1]=="Linux"){
	con <- odbcDriverConnect("DRIVER=MySQL;SERVER=localhost;DATABASE=geoclim_pro;USER=geoclim_admin;PASSWORD=cL!m8_2013A;OPTION=27;")
} else {
	#con <- odbcConnect("geoclimate")
	con <- odbcConnect("local_geoclim")
}
failed <- vector()
success <- vector()
for (i in 1:length(procdates)){
    procst <- Sys.time()
    sqlQuery(con, "flush tables;")
	recs <- sqlQuery(con, paste("SELECT count(*) AS records FROM ", dataset," WHERE wdate = ",shQuote(procdates[i]), sep=""))$records
    if (recs >0 & recs<576000) {
        message(as.character(procdates[i]),": Incomplete upload detected. Removing old records.\r", appendLF=FALSE)
        sqlQuery(con, paste("DELETE FROM ", dataset, " WHERE wdate=", shQuote(procdates[i]),sep=""))
    } else if (recs==576000){
        message(as.character(procdates[i]),": Already updated.\r", appendLF=TRUE)
        next
    } 
	message(as.character(procdates[i]),": Downloading from TRMM FTP. \r", appendLF=FALSE)
    trmm <- get.trmm(ver=ver, wdate=procdates[i], savepath=sdir, rm.existing=FALSE, retries=3, delay=10)
	trmm@w$cell <- txy$wc
	trmm@w$prec <- round(trmm@w$prec,3)
  trmm@w <- trmm@w[order(trmm@w$cell),]
    retry <- 0
	repeat{
      message(as.character(procdates[i]),": Uploading to server. \r", appendLF=FALSE)
      if (!upload.trmm(con,trmm,setname=dataset)){
        message(as.character(procdates[i]),": Uploading failed.", appendLF=FALSE)
        message(as.character(procdates[i]),": Trying to resolve database issues. \r", appendLF=FALSE)
        if (retry==0){
            sqlQuery(con, "flush tables;")    
        } else if (retry==1){
            con <- odbcClose(con)
            con <- odbcDriverConnect("DRIVER=MySQL;SERVER=localhost;DATABASE=geoclim_pro;USER=geoclim_admin;PASSWORD=cL!m8_2013A;OPTION=27;")            
        }
        message(as.character(procdates[i]),": Cleaning up before retry. \r", appendLF=FALSE)
        sqlQuery(con, paste("DELETE FROM ", dataset, " WHERE wdate=", shQuote(procdates[i]),sep=""))
      } else if(retry==2) {
            message(as.character(procdates[i]),": Attempt futile. \r", appendLF=FALSE)
            failed <- c(failed,procdates[i])
            break
      } else {
            message(as.character(procdates[i]),": Done.\r", appendLF=TRUE)
            break
      }
      retry <- retry+1  
    } 
	procen <- Sys.time()
    success <- rbind(success,c(procdates[i],procst,procen))    
}
con <- odbcClose(con)
upen <- Sys.time()

body <- c("Upload TRMM completed","",
paste("Dataset:", dataset),
paste("Period:", stdate, "-", endate),
paste("Start:",format(procst, "%d %b %Y %X")),
paste("End:", format(procen, "%d %b %Y %X")),
paste("Time Elapsed:",format(procen-procst)),"",
paste("Last date run:", procdates[i]), 
paste("No. of stations Scheduled:", length(procdates)),
paste("No. processed dates:", ifelse(is.null(nrow(success)),0,nrow(success))),
paste("No. failed stations:", ifelse(is.null(length(failed)),0,length(failed))),
paste("Failed stations:", paste(failed, collapse=", ")),
"\n\n\n---------------------------",
"System generated message. DO NOT REPLY."
)

if(Sys.info()[1]=="Linux"){
  writeLines(body,paste("/home/gisadmin/logs/", schema, "_", dataset ,"_", stdate, "_", endate, "_", format(Sys.time(),"%Y-%m-%d"), ".txt", sep=""))
} else {
  writeLines(body,paste(sdir, "/", schema, "_", dataset ,"_", stdate, "_", endate, "_", format(Sys.time(),"%Y-%m-%d"), ".txt", sep=""))
}

