#library(RNetCDF)

#library(raster)
#library(RODBC)
#library(weather)
#library(genutils)

cccma.files <- function(..., type="nc"){
    if (!require(ncdf)) stop("Package ncdf not found.")
    files <- list.files(...)
#    info <- matrix(unlist(strsplit(basename(files))),ncol=8)
    
#    cccmadir <- "D:/projects/Climate/Database/Source/CCCMA"
#    cccmafiles <- list.files(cccmadir, full.names=TRUE)
#    climvars <- vector()
#    for (cccfile in cccmafiles){
        #cccfile <- cccmafiles[1]
#        nc <- open.nc(cccfile)
#        climvar <- var.inq.nc(nc,file.inq.nc(nc)$nvars-1)$name
#        climvars <- c(climvars,climvar)
#    }
#    y <- 2001
#    d <- 0
#    for (i in 1:36500){
#        daydata <- vector()
#        if(d<365 | isLeapYear(y)){
#            d <- d + 1                  
#        } else {
#            y <- y+1
#            d <- 0
#        }
#        dt <- dateFromDoy(d,y)
#        for (i in 1:length(climvars)){
#           assign(climvar, raster(cccmafiles[i], varname=climvars[i], band=i))        
#        }
#        colnames(daydata) <- climvars
#               
#    }

}
