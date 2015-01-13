##############################################################################
# title         : 03.Interpolate-TPS.R;
# purpose       : downscale NASA/POWER from 1deg to to 0.25deg using TPS;
# producer      : prepared by A. Sparks;
# last update   : in Los Banos, IRRI, June 2012;
# inputs        : digital elevation and NASA/POWER data;
# outputs       : none the real work is done in 02.SQL-Query.R;
# remarks 1     : just a function called to downscale the weather data and write the file;
##############################################################################

tps.fun <- function(df.out, a.dem, dem.pred, xyz){
  vals <- unlist(df.out[, 2:ncol(df.out)])
  names(vals) <- NULL
 
  #####TPS Section#####
  tps <- Tps(xyz[, c('lon', 'lat', 'dem')], vals)

  #####TPS Predictions Section#####
  tps.pred <- interpolate(dem.pred, tps, xyOnly = FALSE)
  tps.pred <- mask(tps.pred, dem) #Set water values back to NA
  tps.pred <- tps.pred*10
  file.date <- df.out[1, 1]
  file.path <- paste(output.directory, weather.var, sep = '')
  tps.out <- paste(file.path, file.date, sep = '-')
  crs_string <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")
  writeRaster(tps.pred, tps.out, 
              format = 'GTiff', 
              NAflag = -9999,
              datatype = 'INT2S',
              options = 'COMPRESS = LZW',
              crs = crs_string)
}

#eos