##############################################################################
# title         : NASA_GSOD_Station_Correction.R;
# purpose       : Correct NASA/POWER data using GSOD station data;
# producer      : prepared by A. Sparks;
# editor        : J. Aunario
# last update   : in Los Banos, IRRI, August 2013;
# inputs        : NASA/POWER and GSOD weather data from IRRI geoclimate db;
# outputs       : Corrected NASA/POWER weather data;
# remarks       : ;
##############################################################################
#setwd("/home/gisadmin/Projects/geoclimate/NASA Correction Meta")

library(raster)
library(fields)
library(RODBC)
library(plyr)
library(compiler)
library(genutils)

#create global raster object to get id values from for database matching
#x <- raster()
#vals <- 1:ncell(x)
#x[] <- vals
ldlm <- Sys.Date() - as.numeric(format(Sys.Date(),"%d")) # last day last month
schema <- ifelse(is.na(commandArgs(trailing=TRUE)[1]),"geoclimate",commandArgs(trailing=TRUE)[1])

if (schema=="geoclim_pro"){
  tab <- "nasacorr_1d"
  totalrecs <- 18969
  zval <- 100
  sig <- 0
} else {
  tab <- "nasacorr_2014"
  totalrecs <- 18969
  zval <- 1
  sig <- 2
}

if(is.na(commandArgs(trailing=TRUE)[3])){    
    maxdate <- ldlm - as.numeric(format(ldlm,"%d")) # last day last 2 months 
    mindate <- as.Date(format(maxdate,"%Y-%m-1")) 
} else {
    mindate <- as.Date(commandArgs(trailing=TRUE)[3])
    maxdate <- as.Date(ifelse(is.na(commandArgs(trailing=TRUE)[4]),format(ldlm - as.numeric(format(ldlm,"%d")),"%x"),commandArgs(trailing=TRUE)[4]))    
}

vv <- ifelse(is.na(commandArgs(trailing=TRUE)[2]),"tdew",commandArgs(trailing=TRUE)[2])

alt.dem <- raster("/home/gisadmin/Projects/geoclimate/alt_1deg/alt_1deg.asc")

# FOR DEBUGGING
# con.out <- conxn
# dem <- alt.dem
# min.date <- mindate
# max.date <- maxdate
# variable <- vv


#fun queries the database and downscales the weather variable
fun <- function(variable, dem, min.date, max.date, con.out){
  
  dates <- seq(from = min.date, to = max.date, by = "day")
  con.in <- odbcDriverConnect(paste("DRIVER=MySQL;SERVER=localhost;DATABASE=geowarehouse;USER=root;PASSWORD=G!5admIn_2012;OPTION=27;",sep="")) #connect to MySQL server	
  # gsod station location and elevations
  sqlQuery(con.in, "flush tables")
  gsod.loc <- sqlQuery(con.in, "SELECT station_id, pixel_1d AS cell, lon, lat, elev1m AS dem FROM geoclim_pro.gsod_stations")
  log <- vector()
  for(i in 2:length(dates)){
    st <- Sys.time()
    sqlQuery(con.in, "flush tables")
    show.message(shQuote(dates[i]), ": Querying GSOD table. \r", appendLF=FALSE, EL=TRUE)
    gsod <- sqlQuery(con.in, paste("SELECT station_id, ", variable, " FROM geoclim_pro.gsod_xd WHERE wdate = ", shQuote(dates[i]), sep = ""))
    gsod[, 2] <- gsod[, 2]/10 # Convert back from integer
    
    ###Join with gsod.loc to get xyz data###
    gsod <- join(gsod.loc, gsod, by = 'station_id', type = 'full')
    gsod <- na.omit(gsod)
    
    ###Query Server for NASA Data##
    show.message(shQuote(dates[i]), ": Querying NASA-POWER table. \r", appendLF=FALSE, EL=TRUE)
    sqlQuery(con.in, "flush tables")
    nasa.var <- sqlQuery(con.in, paste("SELECT cell, ", variable, "/100 AS variable FROM geoclim_raw.nasacomp_1d WHERE wdate = ", shQuote(dates[i]), " ORDER BY cell", sep = ""))
    
    ###Remove stations delta outside of 2*sigma+-mean
    gsod$delta <- gsod[,variable]-nasa.var$variable[(match(gsod$cell,nasa.var$cell))]
    llim <- mean(gsod$delta,na.rm=TRUE)-2*sd(gsod$delta,na.rm=TRUE)
    ulim <- mean(gsod$delta,na.rm=TRUE)+2*sd(gsod$delta,na.rm=TRUE)
    gsod <- gsod[gsod$delta>=llim & gsod$delta<=ulim,]
    
    ###Remove outliers###
    bxs <- boxplot.stats(gsod[, variable])
    vari <- gsod[!gsod[, 5] %in% bxs$out, ] # remove outliers
    
    ###Create Correction Datasets###
    var.id.stations <- paste(vari[, 1], collapse = ',')
    var.vals <- vari[, variable]
    var.xyz <- vari[, c("lon","lat","dem")]
    rm(vari,gsod)
    gc(reset=TRUE,verbose=FALSE)
    
    ###TPS Section### Thin-plate spline
    show.message(shQuote(dates[i]), ": Running TPS for ", variable, ". \r", appendLF=FALSE, EL=TRUE)
    tps.var <- Tps(var.xyz, var.vals, lon.lat = TRUE)
    
    
    ###TPS Predictions Section###		
    show.message(shQuote(dates[i]), ": Interpolating", variable, ". \r", appendLF=FALSE, EL=TRUE)
    tps.pred.var <- interpolate(dem, tps.var, xyOnly = FALSE, progress = 'text')
    
    ###Correction Section###
    show.message(shQuote(dates[i]), ": Correcting ", variable, ". \r", appendLF=FALSE, EL=TRUE)
    nasa.var.org <- raster()
    nasa.var.org[nasa.var$cell] <- nasa.var$variable
    nasa.var.org <- crop(nasa.var.org, dem)
    var.cor <- tps.pred.var - nasa.var.org
    nasa.var.cor <- nasa.var.org + var.cor
    
    ###Write to the database!###		
    dat <- data.frame(1:ncell(nasa.var.cor), dates[i], round(values(nasa.var.cor)*zval,sig)) 
    colnames(dat) <- c("cell", "wdate", variable)
    dat <- na.omit(dat)
    # TODO: add to database
    # Calculate tdew irrigated such that RHMAX=100 (always). Pepijn van Oort & Jorrel Khalil 2013/09/09
    # if (variable=="tmin"){
    #   tdewirr <- dat[, variable]
    # }
    
    
    if (schema=="geoclim_pro") dat <- dat[!is.na(dat[,variable]),]
    
    sqlQuery(con.in, "flush tables")
    xst <- sqlQuery(con.out, paste("SELECT count(*) recs FROM ", schema, ".", tab," WHERE wdate = ", shQuote(dates[i]), " ORDER BY cell", sep = ""))
    
    if(xst$recs[1]==totalrecs) {
      show.message(shQuote(dates[i]), ": Updating ", variable, " on ", schema, ".", tab, ". \r", appendLF=FALSE, EL=TRUE)
      sqlUpdate(con.out, dat, tablename = tab, index=c("wdate", "cell"))
    } else {
      # Remove old uploads			
      if (xst$recs[1]>0 & xst$recs[1]<totalrecs){
        show.message(shQuote(dates[i]), ": Removing old corrections on ", schema, ".", tab, ". \r", appendLF=FALSE, EL=TRUE)
        sqlQuery(con.out, "flush tables")
        sqlQuery(con.out, paste("DELETE FROM ", schema, ".", tab,"  WHERE wdate = ", shQuote(dates[i]), sep = ""))		    
      }
      
      vars <- c("tmin", "tmax", "tdew")      
      dat[,vars[vars!=variable]] <- NA		  
      dat <- dat[,c("cell","wdate", "tmin", "tmax", "tdew")]
      
      if(tab=="nasa_correction"){
        xy <- xyFromCell(raster(),dat$cell)
        dat$lon <- xy[,"x"]
        dat$lat <- xy[,"y"]				
        dat <- dat[,c("cell","wdate", "lon", "lat", "tmin", "tmax", "tdew")]
      } 
      
      show.message(shQuote(dates[i]), ": Uploading to ", schema, ".", tab, ". \r", appendLF=FALSE,EL=TRUE)
      sqlQuery(con.out, "flush tables")
      sqlSave(con.out, dat, tablename=tab, append=TRUE, rownames=FALSE)			
      #upload new values
    } 
    
    rm(var.cor, nasa.var.org, nasa.var.cor, nasa.var, tps.pred.var, tps.var, bxs, var.vals,
       var.id.stations, var.xyz)
    gc(verbose=FALSE,reset=TRUE) # reclaim some memory
    en <- Sys.time()
    
    show.message(shQuote(dates[i]), ": Done.", appendLF=TRUE)
    log <- rbind(log, c(shQuote(dates[i]), shQuote(st), shQuote(en), (en-st)/3600))
  }
  names(log) <- c("date", "proc_st", "proc_en", "time_elapsed")
  write.csv(log,file=paste("~/logs/nasa_correct_", variable,"_",as.character(min.date),"_",as.character(max.date),".csv",sep=""),row.names=FALSE)
}

#speed things up a bit by using the compiler library
fun.cmp <- cmpfun(fun) 
conxn <- odbcDriverConnect(paste("DRIVER=MySQL;SERVER=localhost;DATABASE=", schema, ";USER=root;PASSWORD=G!5admIn_2012;OPTION=27;",sep="")) #connect to MySQL server

# FOR DEBUGGING
# con.out <- conxn
# dem <- alt.dem
# min.date <- mindate
# max.date <- maxdate
# variable <- vv

# Choose fun when debugging
#fun(variable=vv, dem=alt.dem, min.date=mindate, max.date=maxdate, con.out=conxn)
fun.cmp(variable=vv, dem=alt.dem, min.date=mindate, max.date=maxdate, con.out=conxn)
conxn <- odbcClose(conxn) # close database connection when complete

#sqlClear(conxn,tab)#eos
#conxn
