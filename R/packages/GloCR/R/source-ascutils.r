# Author: Jorrel Khalil S. Aunario, jaunario@gmail.com
# Date :  30 April 2010
# Version 0.0.1
# Licence GPL v3

fillMissing <- function(ascii, xllcorner,yllcorner,ncols,nrows,cellsize){
    asclines <- readLines(ascii)
    cols <- as.numeric(trim(sub("ncols","",asclines[1])))
    rows <- as.numeric(trim(sub("nrows","",asclines[2])))
    xll <- as.numeric(trim(sub("xllcorner","",asclines[3])))
    yll <- as.numeric(trim(sub("yllcorner","",asclines[4])))
    res<- as.numeric(trim(sub("cellsize","",asclines[5])))

}

asciiDataFrame <- function(ascfile, nodata.na=TRUE, verbose=FALSE){
    asclines <- readLines(ascfile)
    cols <- as.numeric(trim(sub("ncols","",asclines[grep("ncols", asclines)[1]])))
    rows <- as.numeric(trim(sub("nrows","",asclines[grep("nrows", asclines)[1]])))
    xll <- as.numeric(trim(sub("xllcorner","",asclines[grep("xllcorner", asclines)[1]])))
    yll <- as.numeric(trim(sub("yllcorner","",asclines[grep("yllcorner", asclines)[1]])))
    res<- as.numeric(trim(sub("cellsize","",asclines[grep("cellsize", asclines)[1]])))
    nodata<- as.numeric(trim(sub("NODATA_value","",asclines[grep("NODATA_value", asclines)[1]])))
    cell <- 1:(cols*rows)-1
    nlayers <- length(asclines)/(rows+6)
    dat <- numeric(0)
    for (i in 1:nlayers){
        #cat(1:(rows+6)+((rows+6)*(i-1)), "\n")
        #flush.console()
        dat <- cbind(dat,as.numeric(unlist(strsplit(asclines[1:rows+6+((rows+6)*(i-1))]," "))))
    }
    if(nodata.na){
        dat[dat==nodata] <- NA
    }
    colnames(dat) <- 1:nlayers
    dat <- as.data.frame(cbind(cell,dat),stringsAsFactors=FALSE)
    return(dat)
}
