library(geoclimate)

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

# TODO centralize location of data to be loaded
setwd("/home/gisadmin/Projects/gneo")

dem <- raster("/home/gisadmin/Projects/geoclimate/alt_1deg/alt_1deg.asc")
con <- odbcDriverConnect(paste("DRIVER=MySQL;SERVER=localhost;DATABASE=geoclimate;USER=root;PASSWORD=G!5admIn_2012;OPTION=27;",sep="")) #connect to MySQL server

schema <- ifelse(is.na(commandArgs(trailing=TRUE)[1]),"geoclim_pro",commandArgs(trailing=TRUE)[1])
if (schema=="geoclim_pro"){
  tab <- "nasacorr_1d"
  zval <- 100
  sig <- 0  
} else {
  tab <- "nasacorr_2014"
  zval <- 1
  sig <- 2
}

ldlm <- Sys.Date() - as.numeric(format(Sys.Date(),"%d")) # last day last month
if(is.na(commandArgs(trailing=TRUE)[2])){    
    maxdate <- ldlm - as.numeric(format(ldlm,"%d")) # last day last 2 months 
    mindate <- as.Date(format(maxdate,"%Y-%m-1"))    
} else {
    mindate <- as.Date(commandArgs(trailing=TRUE)[2])
    maxdate <- as.Date(ifelse(is.na(commandArgs(trailing=TRUE)[3]),format(ldlm - as.numeric(format(ldlm,"%d")),"%Y-%m-%d"),commandArgs(trailing=TRUE)[3]))    
}

load("data/cellxy.Rdata")
#jobs <- 
done <- vector()
repeat {
  message("Getting jobs. ")  
  dates <- get.jobs(initjobs=seq(from=as.Date(mindate),to=as.Date(maxdate),by="day"), workload=60)	
  if(length(dates)<1) {
    message("No jobs available. Hurray!", appendLF=TRUE)
    file.remove(c("jobs.Rdata", "files.csv"))
    break
  } else {
    message("Got ", length(dates), " jobs to work on.", appendLF=TRUE)
  }
  for(i in 1:length(dates)){
    
    d <- dates[i]
    message(format(d,"%Y-%m-%d"),": Retrieving corrected data.",appendLF=TRUE)
    tab <- ifelse(d>as.Date("2011-12-31"),"nasacorr_2014","nasa_correction")
    dat <- sqlQuery(con, paste("SELECT * FROM",tab,"WHERE wdate =",shQuote(d),"AND (tmin IS NOT NULL OR tmax IS NOT NULL OR tdew IS NOT NULL)"))
    dat <- merge(dat,cellxy)
    dat <- dat[,c("cell","wdate","lon","lat","tmin","tmax","tdew")]

    errcell <- which(dat$tmin>dat$tmax)
    if(length(errcell)<1){
      message(format(d,"%Y-%m-%d"),": Clean.",appendLF=TRUE)    
      sqlSave(con,dat,tablename="nasa_correction",append=TRUE,rownames=FALSE)
      done <- c(done,d)
      next
      #nasacorr_clean <- dat
    } else {
      message(format(d,"%Y-%m-%d"),": Cleaning tmin.",appendLF=TRUE)    
      base <- raster(dem)
      base[dat$cell] <- dat$tmin  
      base[is.na(base[])] <- -1000
      base[dat$cell[errcell]] <- NA 
      rtmin <- focalFill(base,fun=myMean,w=matrix(1,3,3))
      
      message(format(d,"%Y-%m-%d"),": Cleaning tmax.",appendLF=TRUE)    
      base <- raster(dem)
      base[dat$cell] <- dat$tmax	
      base[is.na(base[])] <- -1000
      base[dat$cell[errcell]] <- NA 
      rtmax <- focalFill(base,fun=myMean,w=matrix(1,3,3))
      
      rtmax[rtmax[]==-1000] <- NA
      rtmin[rtmin[]==-1000] <- NA
      
      if (tab=="nasa_correction" | tab=="nasacorr_1d"){
        nasacorr_clean <- data.frame(cell=dat$cell,wdate=d,tmin=rtmin[dat$cell],tmax=rtmax[dat$cell])
      } else {
        nasacorr_clean <- data.frame(cell=dat$cell,wdate=d,lon=cellxy$lon[match(dat$cell,cellxy$cell)],lat=cellxy$lat[match(dat$cell,cellxy$cell)],tmin=rtmin[dat$cell],tmax=rtmax[dat$cell],tdew=dat$tdew)
      }
      
      #nasacorr_clean <- data.frame(cell,wdate=d,tmin=rtmin[cell],tmax=rtmax[cell],tdew=dat$tdew)   # FOR DEBUGGING ONLY
      
      # Additional Quality control
      #redflag <- which(!(nasacorr_clean$tdew>=nasacorr_clean$tmin & nasacorr_clean$tdew<=nasacorr_clean$tmax))
      #if(length(redflag>0)) warning("Tdew outside of temperature range. This is another problem!")
    }
    
    rm(dat,rtmax,rtmin,base)
    gc(verbose=FALSE,reset=TRUE)
    
    message(format(d,"%Y-%m-%d"),": Updating database.",appendLF=TRUE)
    if(tab=="nasa_correction" | tab=="nasacorr_1d"){
      sqlUpdate(con, nasacorr_clean, tablename=tab, index=c("cell","wdate"))
    } else {
      sqlSave(con, nasacorr_clean, tablename="nasa_correction", append=TRUE, rownames=FALSE)
    }
    #sqlSave(con, nasacorr_clean, append=TRUE, rownames=FALSE) # FOR DEBUGGING ONLY
    done <- c(done,d)
  }	
}

con <- odbcClose(con)