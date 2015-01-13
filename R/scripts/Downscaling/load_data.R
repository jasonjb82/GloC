##############################################################################
# title         : load_data.R;
# purpose       : load data to downscale NASA/POWER weather data to 0.25 degrees;
# producer      : prepared by A. Sparks;
# last update   : in Los Banos, IRRI, August 2011;
##############################################################################

#load GIS data files, these are all 0.25degree
dem <- raster('data/dem.grd') #altitude from worldclim.org, aggregated from 10*

#create global raster object to get id values from for database matching
x <- raster()
vals <- 1:ncell(x)
x[] <- vals

#crop the raster object by the bounding box specified
z <- crop(x, boundingbox)

#load dem, aggregate, and remove NA for TPS
dem <- crop(dem, boundingbox) #crop to desired geographic area
dem.pred <- dem
dem.pred[is.na(dem)] <- 0 #set water NA to 0 for TPS, won't work with NA
dem.a <- aggregate(dem.pred, 4) #aggregate to 1deg

#create dataframe of values for TPS
xyz <- data.frame(xyFromCell(dem.a, 1:ncell(dem.a)), getValues(dem.a))
names(xyz) <- c('lon', 'lat', 'dem')
