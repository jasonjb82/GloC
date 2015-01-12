# Author: Jorrel Khalil S. Aunario, jaunario@gmail.com
# Date :  7 March 2012
# Version 0.0.1
# Licence GPL v3

# Setup GSOD tables on Climate Schema
setup.GSOD <- function(connectionstring, setname="gsod_xd", stationtable="stations"){
    # read station inventory
    show.message("Reading station information from GSOD.",eol="\n")
    stations <-  read.csv(paste(GSOD.ftp,"ish-history.csv",sep=""), stringsAsFactors=FALSE)
    show.message("Parsing information.",eol="\n")
    stations <- recodeMissing(stations,colnames(stations),old="")
    stations <- recodeMissing(stations,colnames(stations),old="??")
    
    station_id <- 1:nrow(stations)
    station_code <- paste(sprintf("%06d",stations$USAF),sprintf("%05d",stations$WBAN),sep="-")
    
    stations$LAT <- ifelse(stations$LAT > 90.0*1000|stations$LAT < -90.0*1000, NA, stations$LAT/1000)
    stations$LON <- ifelse(stations$LON > 180*1000|stations$LON < -180*1000, NA, stations$LON/1000)
    stations$ELEV..1M. <- ifelse(stations$ELEV..1M.==-99999|stations$ELEV..1M.==-999.999, NA, stations$ELEV..1M./10)
    #stations$BEGIN[!is.na(stations$BEGIN)] <- paste(substr(stations$BEGIN[!is.na(stations$BEGIN)],1,4),substr(stations$BEGIN[!is.na(stations$BEGIN)],5,6),substr(stations$BEGIN[!is.na(stations$BEGIN)],7,8),sep="-")
    #stations$BEGIN <- as.Date(stations$BEGIN)
    stations$BEGIN <- NA
    #stations$END[!is.na(stations$END)] <- paste(substr(stations$END[!is.na(stations$END)],1,4),substr(stations$END[!is.na(stations$END)],5,6),substr(stations$END[!is.na(stations$END)],7,8),sep="-")
    #stations$END <- as.Date(stations$END)
    stations$END  <- NA
    stations <- cbind(station_id, station_code, stations[,-which(colnames(stations) %in% c("USAF","WBAN"))],stringsAsFactors=FALSE)
    stations$nasa_pixel <- cellFromXY(raster(),stations[, c("LON","LAT")])
    
    show.message("Connecting to geoclimate server.",eol="\n")
    con <- odbcConnect(connectionstring)
    show.message("Creating stations table.",eol="\n")
    sqlQuery(con, paste("DROP TABLE IF EXISTS `",stationtable,"`",sep=""))
    sqlQuery(con, paste(
        "CREATE TABLE `",stationtable,"` (",
          "`station_id` int(11) NOT NULL,",
          "`station_code` char(12) NOT NULL COMMENT 'USAF-WBAN',",          
          "`stationname` varchar(50) DEFAULT NULL,",
          "`ctry` char(2) DEFAULT NULL,",
          "`fips` char(2) DEFAULT NULL,",
          "`state` char(2) DEFAULT NULL,",
          "`call` varchar(15) DEFAULT NULL,",
          "`lat` DECIMAL(6,3) DEFAULT NULL,",
          "`lon` DECIMAL(6,3) DEFAULT NULL,",
          "`elev1m` DECIMAL(10,3) DEFAULT NULL,",
          "`begin` DATE DEFAULT NULL,",
          "`end` DATE DEFAULT NULL,",
          "`nasa_pixel` INT(5) DEFAULT NULL,",
          "PRIMARY KEY (`station_id`)",
        ") ENGINE=MyISAM"))
    show.message("Sending station info to server.",eol="\n")
    sqlSave(con, stations,tablename=stationtable, rownames=FALSE, append=TRUE)
    show.message("Creating `",setname,"` datatable.",eol="\n")
    sqlQuery(con, paste("DROP TABLE IF EXISTS `",setname,"`"))
    sqlQuery(con, paste("CREATE TABLE `",setname,"` (",
          "`station_id` INT(11) NOT NULL,",
          "`wdate` DATE NOT NULL,",
          "`tavg` INT DEFAULT NULL,",
          "`slpressure` INT DEFAULT NULL,",
          "`stpressure` INT DEFAULT NULL,",
          "`tdew` INT DEFAULT NULL,",
          "`visibility` INT DEFAULT NULL,",
          "`wind` INT DEFAULT NULL,",
          "`maxwind` INT DEFAULT NULL,",
          "`gust` INT DEFAULT NULL,",
          "`tmax` INT DEFAULT NULL,",
          "`tmin` INT DEFAULT NULL,",
          "`prec` INT DEFAULT NULL,",
          "`snowdepth` INT DEFAULT NULL,",
          "`ifog` BOOLEAN DEFAULT NULL,",
          "`irain` BOOLEAN DEFAULT NULL,",
          "`isnow` BOOLEAN DEFAULT NULL,",
          "`ihail` BOOLEAN DEFAULT NULL,",
          "`ithunder` BOOLEAN DEFAULT NULL,",
          "`itornado` BOOLEAN DEFAULT NULL",
        ") ENGINE=MyIsam DEFAULT CHARSET=latin1", sep=""))
    con <- odbcClose(con)
    show.message("Ready for GSOD scraping",eol="\n")
}
