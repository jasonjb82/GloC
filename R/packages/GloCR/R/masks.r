# Author: Jorrel Khalil S. Aunario, jaunario@gmail.com
# Date :  13 August 2012
# Version 0.0.1
# Licence GPL v3

#library(RODBC)
#library(raster)
#con <- odbcConnect("geoclimadmin")


setup.mask <- function(con, maskset_name, maskset_desc=NA, targetres=NA,...){  
	
	# Load source data
	iso <- raster("D:/Projects/SiRiUS/Data/ISO/GADM_ISO.tif")
	attribs <- iso@data@attributes[[1]]
	dem <- raster("D:/Projects/SiRiUS/Data/DEM/SRTM/srtm_5m.tif")
	soil <- raster("D:/Projects/SiRiUS/Data/Soil/Data/WISE5by5min/Grid/WISE_smw5by5min.tif")
	
	# Specify output specs
	if (!is.na(targetres)){
	  # resample/aggregate
	  iso <- aggregate(iso,fact=targetres/xres(iso),...) 
	  dem <- aggregate(dem,fact=targetres/xres(dem))
	  soil <- aggregate(soil,fact=targetres/xres(soil))
	  
	}
	cell <- 1:ncell(iso) 
	
	masktable <- as.data.frame(cell)
	masktable$iso3 <- attribs$ISO[match(iso[],attribs$Value)]
	masktable$elevation <- NA   
	masktable$elevation[cellsFromExtent(iso,dem)] <- round(dem[],2)
	masktable$land <- as.numeric(!is.na(masktable$iso3))
	masktable$soil_wise <- NA
	masktable$soil_wise[cellsFromExtent(iso,soil)] <- soil[]
	
	maskinfo <- c("default", maskset_name, maskset_desc, xmin(iso), xmax(iso), ncol(iso), ymin(iso), ymax(iso), nrow(iso))
	
	
	sqlSave(con, maskinfo, tablename="masksets", append=TRUE, rownames=FALSE)
	sqlSave(con, masktable, tablename=maskset_name, append=TRUE, rownames=FALSE)
  return(TRUE)
}
