##############################################################################
# title         : 02.SQL_Query.R;
# purpose       : query IRRI GIS NASAPOWER database server for data downscaling;
# producer      : prepared by A. Sparks;
# last update   : in Los Banos, IRRI, June 2012;
# inputs        : SQL queries from NASA/POWER database hosted in IRRI GIS lab; 
# outputs       : Downscaled GeoTiff files of NASA/POWER weather data;
# remarks       : ;
##############################################################################

source('scripts/R/03.Interpolate-TPS.R') #load the function that does the interpolation

#con <- odbcConnect(dsn = 'climate_SRV2A') #connect to MySQL server

#con <- odbcConnect(dsn = 'climate_SRV3A') #connect to MySQL server linux
con <- odbcDriverConnect("DRIVER=MySQL;SERVER=localhost;DATABASE=geoclim_raw;USER=root;PASSWORD=G!5admIn_2012;OPTION=27;") #connect to MySQL server

#check whether downscaled output files exist
#if not, then the max/min dates entered are used
#if so, start with next date in sequence based on files found in dir

#fun queries the database and downscales the weather variable
fun <- function(a.dem, dem.pred, z, output.directory, min.date, max.date){
  
  if(length(list.files(path = output.directory)) != 0){ 
      file.pattern  <- paste(weather.var, "[[:graph:]]{11}.tif$", sep = "")
      e <- list.files(path = output.directory, pattern = file.pattern)
      f <- e[length(e)]
      f <- substr(f, 6, 15)
        if(f < max.date) {
          dates <- seq(from = as.Date(f) + 1, to = as.Date(max.date), by = 1)
        } else if(f >= max.date) {
          stop("Files already exist, nothing to do")
        }
      } else {
        dates <- seq(from = as.Date(min.date), to = as.Date(max.date), by = 1)
  }

  
  for(r in 1:length(dates)){
    l <- shQuote(dates[[r]])
    
    i <- getValues(z)
    
    if(weather.var == 'tmin' | weather.var == 'tmax' | weather.var == 'tdew'){#select corrected weather data table for tmin, tmax, tdew, NASA otherwise
      
      #       df.out <- sqlQuery(con, paste("SELECT wdate, ", noquote(weather.var), " FROM geoclimate.nasa_correction WHERE cell IN(", paste(i, collapse = ','), ") AND wdate =", l, sep = ''))
      df.out <- sqlQuery(con, paste("SELECT wdate, cell, ", weather.var, " FROM geoclimate.nasa_correction WHERE cell IN(", paste(i, collapse = ','), ") AND wdate =", l, sep = ''))
    } else {  
      df.out <- sqlQuery(con, paste("SELECT wdate, cell, ", weather.var, "/100 ", weather.var, " FROM geoclim_raw.nasacomp_1d WHERE cell IN(", paste(i, collapse = ','), ") AND wdate =", l, sep = ''))
    }
    df2 <- as.data.frame(i)
    df2[,weather.var] <- NA
    df2[match(df.out$cell,i),weather.var] <- df.out[,weather.var]
    df2[,1] <- df.out$wdate[1]
    colnames(df2)[1] <- "wdate"
    df.out <- df2
    rm(df2)
    gc()
    
    if (sum(!is.na(df.out[,2]))<1) {
      message("No data found for ", weather.var, " on ", dates[[r]], appendLF=TRUE)
      next
    }
    tps.fun(df.out, a.dem, dem.pred, xyz)
    }         
  gc() #try to reclaim some memory
}

 
#run function to query database and downscale weather data
fun(dem.a, dem.pred, z, output.directory, min.date, max.date)

#eos
