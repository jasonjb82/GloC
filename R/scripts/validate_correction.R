# Title: NASA-POWER Bias Correction Validation script
# Author: Jorrel Khalil S. Aunario
# Description: This validation script is used for windows-based R sessions
#

library(geoclimate)
library(ggplot2)
library(RODBC)
library(DiceEval)

# Settings
setwd("X:/Projects/gneo")
# Inputs
adm <- shapefile("E:/General/Vector/Administrative Boundaries/GADM_L0.shp")
base <- raster()

# Retrieve data from database
if (!file.exists("validation.RData")){
  con <- odbcConnect("climate_SRV3A")

  
  vstns <- sqlFetch(con,"geowarehouse.weather_stations", stringsAsFactors=FALSE)
  vstns$pixel_1d <- cellFromXY(base, vstns[,c("lon", "lat")])
  
  astns <- sqlFetch(con,"geoclimate.africarice_stations", stringsAsFactors=FALSE)
  astns$table_name <- "geoclimate.africarice_xd"
  astns$pixel_1d <- cellFromXY(base, astns[,c("lon", "lat")])
  istns <- sqlFetch(con,"geoclimate.irriclim_stations", stringsAsFactors=FALSE)
  istns$table_name <- "geoclimate.irriclim_xd"
  
  stns <- rbind(istns[, c("station_id","lat","lon", "pixel_1d", "table_name")], astns[, c("station_id","lat","lon", "pixel_1d", "table_name")], vstns[, c("station_id","lat","lon", "pixel_1d", "table_name")])
  
  cells <- sort(unique(stns$pixel_1d))
  alldata <- vector()
  vars <- c("tmin", "tmax", "tdew")
  
  for (i in 1: length(cells)){
    cell <- cells[i]
    pxstns <- stns[stns$pixel_1d==cell,]
    # Retrieve validation data
    tables <- unique(pxstns$table_name)
    obs <- vector()
    message(cell, ": Retrieving available station data.\r")
    for (tb in tables){
      stdata <- sqlQuery(con, paste("SELECT station_id, wdate, tmin, tmax FROM", tb, "WHERE station_id in (", paste(pxstns$station_id[pxstns$table_name==tb],collapse=", "), ") AND tmin IS NOT NULL AND tmax IS NOT NULL;"))
      stdata$cell <- cell
      stdata$table_name <- tb
      colnames(stdata) <- c("station_id", "wdate", "obs.tmin", "obs.tmax", "cell", "table_name")
      if (grepl("validation_xd",tb)) stdata[, c("obs.tmin", "obs.tmax")] <- stdata[, c("obs.tmin", "obs.tmax")]/100
      obs <- rbind(obs,stdata[ ,c("cell","wdate","station_id","table_name","obs.tmin", "obs.tmax")])
      rm(stdata)
      gc(reset=TRUE)
    }
    dts <- unique(obs$wdate)
    message(cell, ": Retrieving uncorrected NASA-POWER.\r")
    uncorr <- sqlQuery(con, paste("SELECT cell, wdate, tmin/100 `uncor.tmin`, tmax/100 `uncor.tmax` FROM geoclim_raw.nasacomp_1d WHERE cell=", cell,  " AND wdate in (", paste(shQuote(dts),collapse=", "), ");"))
    message(cell, ": Retrieving corrected NASA-POWER.\r")
    corr <- sqlQuery(con, paste("SELECT cell, wdate, tmin `cor.tmin`, tmax `cor.tmax` FROM geoclimate.nasa_correction WHERE cell=", cell,  " AND wdate in (", paste(shQuote(dts),collapse=", "), ");"))
    pixdata  <- merge(merge(obs,uncorr, by=c("cell","wdate")),corr, by=c("cell","wdate"))
    #pixdata <- na.omit(pixdata)
    message(cell, ": Merging into main data frame.\r")
    alldata <- rbind(alldata,pixdata)  
    message(cell, ": Cleaning garbage.\r")
    rm(obs,dts,uncorr,corr,pixdata)
    gc(reset=TRUE)  
    message(cell, ": Done. (", i, " of ", length(cells), ")", appendLF=TRUE)
  }
  con <- odbcClose(con)
  alldata$year <- yearFromDate(alldata$wdate)
  alldata$stn_code <- paste(alldata$table_name,alldata$station_id,sep="_")  
} else {
  load("X:/Projects/gneo/validation.RData")
}

# for RMSE Corrected
cortmax <- alldata[,c("wdate","obs.tmax","cor.tmax")]
cortmax <- na.omit(cortmax)
cortmin <- alldata[,c("wdate","obs.tmin","cor.tmin")]
cortmin <- na.omit(cortmin)

# for RMSE Uncorrected
uncortmax <- alldata[,c("wdate","obs.tmax","uncor.tmax")]
uncortmax <- na.omit(uncortmax)
uncortmin <- alldata[,c("wdate","obs.tmin","uncor.tmin")]
uncortmin <- na.omit(uncortmin)

ostat <- data.frame(nstns=length(unique(alldata$stn_code)), npixels=length(unique(alldata$cell)),
                    tmin_corr_n=nrow(cortmin), tmin_corr_rmse=RMSE(cortmin$obs.tmin, cortmin$cor.tmin),
                    tmin_uncorr_n=nrow(uncortmin), tmin_uncorr_rmse=RMSE(uncortmin$obs.tmin, uncortmin$uncor.tmin),
                    tmax_corr_n=nrow(cortmax), tmax_corr_rmse=RMSE(cortmax$obs.tmax, cortmax$cor.tmax),
                    tmax_uncorr_n=nrow(uncortmax), tmax_uncorr_rmse=RMSE(uncortmax$obs.tmax, uncortmax$uncor.tmax))
rm(uncortmax, uncortmin, cortmin, cortmax)
gc(reset=TRUE)

ggplot(alldata, aes(x=obs.tmin,y=cor.tmin, colour=factor(cell)))+geom_point()+facet_wrap( ~ year)
ggplot(alldata, aes(x=obs.tmin,y=uncor.tmin, colour=factor(cell)))+geom_point()+facet_wrap( ~ year)


# Compute RMSE by pixel (for mapping)
astat <- vector()
for (i in 1:length(cells)){
  cell <- cells[i]
  celldt <- alldata[alldata$cell==cell,]
  stns <- unique(paste(celldt$table_name, celldt$station_id, sep="_"))
  nvs <- length(stns)
  # for RMSE Corrected
  cortmax <- celldt[,c("wdate","obs.tmax","cor.tmax")]
  cortmax <- na.omit(cortmax)
  cortmin <- celldt[,c("wdate","obs.tmin","cor.tmin")]
  cortmin <- na.omit(cortmin)
  
  # for RMSE Uncorrected
  uncortmax <- celldt[,c("wdate","obs.tmax","uncor.tmax")]
  uncortmax <- na.omit(uncortmax)
  uncortmin <- celldt[,c("wdate","obs.tmin","uncor.tmin")]
  uncortmin <- na.omit(uncortmin)
  
  cstat <- data.frame(cell,nvalistns=nvs, 
                      tmin_corr_n=nrow(cortmin), tmin_corr_rmse=RMSE(cortmin$obs.tmin, cortmin$cor.tmin),
                      tmin_uncorr_n=nrow(uncortmin), tmin_uncorr_rmse=RMSE(uncortmin$obs.tmin, uncortmin$uncor.tmin),
                      tmax_corr_n=nrow(cortmax), tmax_corr_rmse=RMSE(cortmax$obs.tmax, cortmax$cor.tmax),
                      tmax_uncorr_n=nrow(uncortmax), tmax_uncorr_rmse=RMSE(uncortmax$obs.tmax, uncortmax$uncor.tmax))
  astat <- rbind(astat, cstat)
  rm(uncortmax, uncortmin, cortmin, cortmax,celldt,stns,cstat,nvs)
  gc(reset=TRUE)
}

# Map pixel RMSEs

tmin_rmse_cor <- raster()
tmin_rmse_cor[astat$cell] <- astat$tmin_corr_rmse
tmin_rmse_uncor <- raster()
tmin_rmse_uncor[astat$cell] <- astat$tmin_uncorr_rmse
tmin_rmse_diff <- tmin_rmse_uncor-tmin_rmse_cor

tmin_max_rmse <- ceiling(max(tmin_rmse_diff[],na.rm=TRUE))
tmin_min_rmse <- floor(min(-tmin_max_rmse, tmin_rmse_diff[],na.rm=TRUE))
dev.set(2)
plot(tmin_rmse_diff, col=colorRampPalette(c("red", "white", "blue"))(15), zlim=c(tmin_min_rmse,tmin_max_rmse))
plot(adm, border="lightgrey", add=TRUE)

#x11()

tmax_rmse_cor <- raster()
tmax_rmse_cor[astat$cell] <- astat$tmax_corr_rmse
tmax_rmse_uncor <- raster()
tmax_rmse_uncor[astat$cell] <- astat$tmax_uncorr_rmse
tmax_rmse_diff <- tmax_rmse_uncor-tmax_rmse_cor

tmax_max_rmse <- ceiling(max(tmax_rmse_diff[],na.rm=TRUE))
tmax_min_rmse <- floor(min(-tmax_max_rmse, tmax_rmse_diff[],na.rm=TRUE))
x11()
plot(tmax_rmse_diff, col=colorRampPalette(c("red", "white", "blue"))(15), zlim=c(tmax_min_rmse,tmax_max_rmse))
plot(adm, border="lightgrey", add=TRUE)

nvalistns <- raster()
nvalistns[astat$cell] <- astat$nvalistns

plot(nvalistns, col=colorRampPalette(c("grey", "black"))(15))
plot(adm, border="lightgrey", add=TRUE)

for(j in 2:ncol(astat)){
  base <- raster()
  base[astat$cell] <- astat[,j]
  if (grepl("tmin",colnames(astat)[j]) & grepl("rmse",colnames(astat)[j])){
    llim <- tmin_min_rmse  
    ulim <- tmin_max_rmse
    colcodes <- colorRampPalette(c("red", "white", "blue"))(25)
  } else if (grepl("tmax",colnames(astat)[j]) & grepl("rmse",colnames(astat)[j])){
    llim <- tmin_min_rmse  
    ulim <- tmin_max_rmse
    colcodes <- colorRampPalette(c("red", "white", "blue"))(25)
  } else {
    next
  }
  
  
  plot(base, col=colcodes)
  plot(adm, add=TRUE, border="grey")
  writeRaster(base, filename=paste("cache/validation/", colnames(astat)[j],"_1983-2013.tif",sep=""), format="GTiff", options=c("COMPRESS=LZW", "TFW=TRUE"), overwrite=TRUE)
  rm(base)
  gc(reset=TRUE)  
}


for (y in 1983:2013){
  vstat <- vector()
  for (i in 1:length(cells)){
    cell <- cells[i]
    celldt <- alldata[alldata$cell==cell & alldata$year==y,]
    stns <- unique(paste(celldt$table_name, celldt$station_id, sep="_"))
    nvs <- length(stns)
    # for RMSE Corrected
    cortmax <- celldt[,c("wdate","obs.tmax","cor.tmax")]
    cortmax <- na.omit(cortmax)
    cortmin <- celldt[,c("wdate","obs.tmin","cor.tmin")]
    cortmin <- na.omit(cortmin)
    
    # for RMSE Uncorrected
    uncortmax <- celldt[,c("wdate","obs.tmax","uncor.tmax")]
    uncortmax <- na.omit(uncortmax)
    uncortmin <- celldt[,c("wdate","obs.tmin","uncor.tmin")]
    uncortmin <- na.omit(uncortmin)
    
    cstat <- data.frame(cell,nvalistns=nvs, 
                        tmin_corr_n=nrow(cortmin), tmin_corr_rmse=RMSE(cortmin$obs.tmin, cortmin$cor.tmin),
                        tmin_uncorr_n=nrow(uncortmin), tmin_uncorr_rmse=RMSE(uncortmin$obs.tmin, uncortmin$uncor.tmin),
                        tmax_corr_n=nrow(cortmax), tmax_corr_rmse=RMSE(cortmax$obs.tmax, cortmax$cor.tmax),
                        tmax_uncorr_n=nrow(uncortmax), tmax_uncorr_rmse=RMSE(uncortmax$obs.tmax, uncortmax$uncor.tmax))
    vstat <- rbind(vstat, cstat)
    rm(uncortmax, uncortmin, cortmin, cortmax,celldt,stns,cstat,nvs)
    gc(reset=TRUE)
  }
    
  for(j in 2:ncol(vstat)){
    base <- raster()
    base[vstat$cell] <- vstat[,j]
    writeRaster(base, filename=paste("cache/validation/", colnames(vstat)[j],"_",y,".tif",sep=""), format="GTiff", options=c("COMPRESS=LZW", "TFW=TRUE"), overwrite=TRUE)
    rm(base)
    gc(reset=TRUE)  
  }
}

ss <- colnames(astat)[-1]
sbyyr <- vector()
for (j in 2:length(ss)){
  fls <- dir(path="cache/validation", pattern=paste(ss[j],".*.tif$",sep=""), full.names=TRUE)
  sstk <- stack(fls)
  ssdat <- cbind(cells,sstk[cells])
  sbyyr <- cbind(sbyyr, as.numeric(colSums(ssdat[,-1], na.rm=TRUE)))
}
colnames(sbyyr) <- ss[-1]

# RMSEs by station, by year
stcodes <- unique(alldata$stn_code)
sstat <- vector()
for (y in 1983:2013){
  for (i in 1:length(stcodes)){
    stcode <- stcodes[i]
    stdt <- alldata[alldata$stn_code==stcode & alldata$year==y,]
    if (nrow(stdt)<1) next
    #stns <- unique(paste(celldt$table_name, celldt$station_id, sep="_"))
    #nvs <- length(stns)
    # for RMSE Corrected
    cortmax <- stdt[,c("wdate","obs.tmax","cor.tmax")]
    cortmax <- na.omit(cortmax)
    cortmin <- stdt[,c("wdate","obs.tmin","cor.tmin")]
    cortmin <- na.omit(cortmin)
    
    # for RMSE Uncorrected
    uncortmax <- stdt[,c("wdate","obs.tmax","uncor.tmax")]
    uncortmax <- na.omit(uncortmax)
    uncortmin <- stdt[,c("wdate","obs.tmin","uncor.tmin")]
    uncortmin <- na.omit(uncortmin)
    
    ststat <- data.frame(stcode,year=y, 
                        tmin_corr_n=nrow(cortmin), tmin_corr_rmse=RMSE(cortmin$obs.tmin, cortmin$cor.tmin),
                        tmin_uncorr_n=nrow(uncortmin), tmin_uncorr_rmse=RMSE(uncortmin$obs.tmin, uncortmin$uncor.tmin),
                        tmax_corr_n=nrow(cortmax), tmax_corr_rmse=RMSE(cortmax$obs.tmax, cortmax$cor.tmax),
                        tmax_uncorr_n=nrow(uncortmax), tmax_uncorr_rmse=RMSE(uncortmax$obs.tmax, uncortmax$uncor.tmax))
    sstat <- rbind(sstat, ststat)
    rm(uncortmax, uncortmin, cortmin, cortmax,stdt,ststat)
    gc(reset=TRUE)
  }
}
#toplot <- c("tmin_corr_rmse","tmin_uncorr_rmse","tmax_corr_rmse","tmax_uncorr_rmse")
  sstat$year <- factor(sstat$year)

pplot <- ggplot(sstat,aes(x=year, y=tmin_corr_rmse))+geom_boxplot()+geom_jitter()
ggsave(filename="tmin_corr_rmse.png",plot=pplot)
pplot <- ggplot(sstat,aes(x=year, y=tmin_uncorr_rmse))+geom_boxplot()+geom_jitter()
ggsave(filename="tmin_uncorr_rmse.png",plot=pplot)
pplot <- ggplot(sstat,aes(x=year, y=tmin_rmse_diff))+geom_boxplot()+geom_jitter()
ggsave(filename="tmin_rmse_diff_boxplot.png")
pplot <- ggplot(sstat,aes(x=year, y=tmax_corr_rmse))+geom_boxplot()+geom_jitter()
ggsave(filename="tmax_corr_rmse.png",plot=pplot)
pplot <- ggplot(sstat,aes(x=year, y=tmax_uncorr_rmse))+geom_boxplot()+geom_jitter()
ggsave(filename="tmax_uncorr_rmse.png",plot=pplot)
ggplot(sstat,aes(x=year, y=tmax_rmse_diff))+geom_boxplot()+geom_jitter()
ggsave(filename="tmax_rmse_diff_boxplot.png")


# RMSEs by year
ystat <- vector()
for (y in 1983:2013){  
    ydt <- alldata[alldata$year==y,]
    #stns <- unique(paste(celldt$table_name, celldt$station_id, sep="_"))
    #nvs <- length(stns)
    # for RMSE Corrected
    cortmax <- ydt[,c("wdate","obs.tmax","cor.tmax")]
    cortmax <- na.omit(cortmax)
    cortmin <- ydt[,c("wdate","obs.tmin","cor.tmin")]
    cortmin <- na.omit(cortmin)
    
    # for RMSE Uncorrected
    uncortmax <- ydt[,c("wdate","obs.tmax","uncor.tmax")]
    uncortmax <- na.omit(uncortmax)
    uncortmin <- ydt[,c("wdate","obs.tmin","uncor.tmin")]
    uncortmin <- na.omit(uncortmin)
    
    ststat <- data.frame(year=y,nstns=length(unique(ydt$stn_code)), npixels=length(unique(ydt$cell)),
                         tmin_corr_n=nrow(cortmin), tmin_corr_rmse=RMSE(cortmin$obs.tmin, cortmin$cor.tmin),
                         tmin_uncorr_n=nrow(uncortmin), tmin_uncorr_rmse=RMSE(uncortmin$obs.tmin, uncortmin$uncor.tmin),
                         tmax_corr_n=nrow(cortmax), tmax_corr_rmse=RMSE(cortmax$obs.tmax, cortmax$cor.tmax),
                         tmax_uncorr_n=nrow(uncortmax), tmax_uncorr_rmse=RMSE(uncortmax$obs.tmax, uncortmax$uncor.tmax))
    ystat <- rbind(ystat, ststat)
    rm(uncortmax, uncortmin, cortmin, cortmax,ydt,ststat)
}
write.csv(ystat, "yearly_RMSE.csv", row.names=FALSE)

for(j in 2:ncol(vstat)){
  base <- raster()
  base[vstat$cell] <- vstat[,j]
  writeRaster(base, filename=paste("cache/validation/", colnames(vstat)[j],"_",y,".tif",sep=""), format="GTiff", options=c("COMPRESS=LZW", "TFW=TRUE"), overwrite=TRUE)
  rm(base)
  gc(reset=TRUE)  
}
