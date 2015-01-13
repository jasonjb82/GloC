##############################################################################
# title         : 01.Downscale_NASA_POWER.R;
# purpose       : spatial downscaling of NASA/POWER weather data to 0.25 degrees;
# producer      : prepared by A. Sparks;
# last update   : in Los Banos, IRRI, June 2012;
# inputs        : NASA/POWER database, files as called in lib/load_data.R;
# outputs       : downscaled weather data in the format of R Raster 
#                 GeoTiff files for tmin, tmax, rh2m, tdew, wind;
##############################################################################

st <- Sys.time()

library(raster)

#set the bounding box area of interest for downscaling
#format: extent(xmin, xmax, ymin, ymax)
boundingbox <- extent(60, 147, -12, 55) #Asia only

#load more libraries and other files
#setwd("C:/users/yourusername")
source("load_libraries.R")

#NOTE!
#The script will check to see if files exist in the output directory.
#If files exist for the variable being downscaled it will begin where the last
#run left off, or stop with an error message.

#select weather variable to downscale, select only one per R session!
weather.var <- commandArgs(trailing=TRUE)[1]
if (!weather.var %in% c("rh2m","srad","tdew","tmin","tmax","wind")){
  stop("Invalid weather variable ", weather.var)
}
#specify directory for saving downscaled grid files, always include trailing "/"
#there is a 'cache' folder already in this zip file with 'tmin', 'tmax', 'tdew', 'wind' and 'rh2m' subfolders
#output.directory <- "cache/wind/"
output.directory <- paste("cache/downscale/", weather.var, "/",sep="")

ldlm <- Sys.Date() - as.numeric(format(Sys.Date(),"%d")) # last day last month
if(is.na(commandArgs(trailing=TRUE)[2])){    
  max.date <- ldlm - as.numeric(format(ldlm,"%d")) # last day last 2 months 
  min.date <- as.Date(format(max.date,"%Y-%m-1")) 
} else {
  min.date <- as.Date(commandArgs(trailing=TRUE)[2])
  max.date <- as.Date(ifelse(is.na(commandArgs(trailing=TRUE)[3]),format(ldlm - as.numeric(format(ldlm,"%d")),"%Y-%m-%d"),commandArgs(trailing=TRUE)[3]))    
}


#run it!
#There will be error messages about the Krig function, these are OK to ignore.
source('scripts/R/02.SQL-Query.R') 

en <- Sys.time()

#writeLines(c(paste("Time start:", st),paste("Time end:", en),paste("Time Elapsed:", en-st)),paste(paste("timelog", weather.var, min.date, max.date, sep="_"),".txt", sep=""))
#eos
