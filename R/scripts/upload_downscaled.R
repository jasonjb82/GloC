library(raster)
library(RODBC)

myMean <- function(x,...){
  x[x==-1000] <- NA  
  return(mean(x, ...))
}

focalFill <- function(xx,...){
  
  x <- xx
  tosub <- which(is.na(x[]))
  ii <- 0  
  prev <- length(tosub)
  stagnation <- 0
  repeat{
    ii <- ii+1
    x <- focal(x,NAonly=TRUE,...)
    
    if(ii>1000) {
      break
    } else {			    
      tofill <- sum(is.na(x[]))
      if (tofill==0){
        message("Raster completely filled.", appendLF=TRUE)
        break 
      } else if(tofill < prev){
        message("Filled ", prev-tofill, " pixels.", appendLF=TRUE) 
        prev <- tofill
      } else if (tofill == prev){
        stagnation <- stagnation+1
        message("Stagnation point ", stagnation, ". ", tofill, " pixels left unfilled.", appendLF=TRUE) 
      } else if (tofill > prev){
        stop("Imposible outcome. Recompute variables.", appendLF=TRUE) 
      }
    }
    if (stagnation==3) {      
      break			 
    }        
  }
  return(x)
}

schema <- ifelse(is.na(commandArgs(trailing=TRUE)[1]),"geoclim_pro",commandArgs(trailing=TRUE)[1])

ldlm <- Sys.Date() - as.numeric(format(Sys.Date(),"%d")) # last day last month
if(is.na(commandArgs(trailing=TRUE)[2])){    
  maxdate <- ldlm - as.numeric(format(ldlm,"%d")) # last day last 2 months 
  mindate <- as.Date(format(maxdate,"%Y-%m-1"))    
} else {
  mindate <- as.Date(commandArgs(trailing=TRUE)[2])
  maxdate <- as.Date(ifelse(is.na(commandArgs(trailing=TRUE)[3]),format(ldlm - as.numeric(format(ldlm,"%d")),"%Y-%m-%d"),commandArgs(trailing=TRUE)[3]))    
}


con <- odbcDriverConnect(paste("DRIVER=MySQL;SERVER=localhost;DATABASE=", schema, ";USER=root;PASSWORD=G!5admIn_2012;OPTION=27;",sep="")) #connect to MySQL server
dataset <- "nsdsas_15m"
columns <- sqlColumns(con,dataset)$COLUMN_NAME

dsdir <- "cache/downscale"
setwd("/home/gisadmin/Projects/gneo")

years <- 2014


world <- raster()
res(world) <- 0.25
asia <- raster(dir(path="cache/downscale/tmin", pattern="tmin", full.names=TRUE)[1])
acells <- which(!is.na(asia[]))
xy <- xyFromCell(asia,acells)
cell <- cellFromXY(world,xy)

dates <- format(seq(from=mindate, to=maxdate, by="day"),"%Y-%m-%d")
  for (d in dates) {
    #d <- dates[1]
    ul <- sqlQuery(con, paste("SELECT count(*) recs FROM", dataset, "WHERE wdate =",shQuote(d)))$recs
    if(length(acells)==ul) {
      message(d, ": Already uploaded")
      flush.console()
      next
    } else if (length(acells)>0 & length(acells)<ul){
      ul <- sqlQuery(con, paste("DELETE FROM", dataset, "WHERE wdate =",shQuote(d)))
    }
    
    message(d, ": Reading Tiff files.")
    #flush.console()
    
    files <- dir(dsdir,pattern=d,recursive=TRUE, full.names=TRUE)
      
    if(length(files)<1) {stop("No files found for date ", d)}
    asiastack <- stack(files)
    # Test tmin<tmax
    tmin <- raster(asiastack,5)
    tmax <- raster(asiastack,4)
    diff.nx <- tmax-tmin
    rm.cells <- which(diff.nx[acells]<0)
    if (length(rm.cells)>0){
      wind <- raster(asiastack,6) # backup wind data when old tmin and tmax are removed
      tmin[is.na(tmin[])] <- -1000
      tmin[rm.cells] <- NA 
      tmin <- focalFill(tmin,fun=myMean,w=matrix(1,3,3))
      tmin[tmin[]==-1000] <- NA
      names(tmin) <- names(asiastack)[5]
      asiastack <- dropLayer(asiastack,5)
      
      tmax[is.na(tmax[])] <- -1000
      tmax[rm.cells] <- NA 
      tmax <- focalFill(tmax,fun=myMean,w=matrix(1,3,3))
      tmax[tmax[]==-1000] <- NA
      names(tmax) <- names(asiastack)[4]
      asiastack <- dropLayer(asiastack,4)      
      
      asiastack <- addLayer(asiastack,tmax)
      asiastack <- addLayer(asiastack,tmin)
    }
    dsdata <- as.data.frame(asiastack[acells])
    colnames(dsdata) <- substr(colnames(dsdata),1,4)
    wdate <- d
    data <- data.frame(cell, wdate)
    for (cc in 3:length(columns)){
      if (columns[cc] %in% colnames(dsdata)){
        data[,columns[cc]] <- round(dsdata[,columns[cc]],0)
      } else data[,columns[cc]] <- NA
    }
    message(d, ": Uploading to server.")
    flush.console()    
    sqlSave(con, data, tablename=dataset, rownames=FALSE,append=TRUE)
    message(d, ": Done.")
    flush.console()    
  }
con <- odbcClose(con)

#world[data$cell] <- data$srad
#levelplot(world)