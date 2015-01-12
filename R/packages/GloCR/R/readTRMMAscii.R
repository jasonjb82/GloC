# Author: Angelo Carlo D. Pacheco and Jorrel Khalil Aunario
# Date : September 9, 2009
# Version 0.1
# Licence GPL v3

# Function for converting TRMM 3B43 v6 ascii data to raster 
#  Indicate the number of header lines to remove (except column names)

readTRMMAscii <- function(inputfile, outputfile, hdrs=5, ...){
    tf <- tempfile()

	write.table(readLines(inputfile)[-(1:hdrs)], tf, row.names=F, col.names=F, quote=F)
	coor <- read.table(tf, header=T)
	
	file.remove(tf)

	y = max(coor[,1]) - min(coor[,1]) 
	y = y*4

	x = max(coor[,2]) - min(coor[,2]) 
	x = x*4

	rast <- raster(nrow=y, ncol=x, xmn=min(coor[,2]), xmx=max(coor[,2]), ymn=min(coor[,1]), ymx=max(coor[,1]) )
	rast[] <- -9999

	vec <- 1:length(rast[])
	vec[] <- -9999

	c <- cellFromXY(rast, cbind(coor[,2],coor[,1]) )
	vec[c] <- coor[,3]

	rast <- setValues(rast, vec)

	filename(rast) <- outputfile
	if (filename(rast) != "") {
		rast <- writeRaster(rast, ...)
	}
	
	return(rast)
}