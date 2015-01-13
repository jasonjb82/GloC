# Author: Jorrel Khalil S. Aunario
# Title: Importing weather data to R weather object

wth.csv <- function (txtfile){
	setwd("D:/Google Drive/Projects/Geoclimate/Validation Station Data/Latin America")
	csvfiles <- dir(pattern=".csv$", full.names=TRUE)
	
	con <- odbcConnect("climate_SRV3A")
	cols <-data.frame(col=c("data", "radiacao", "tempMinima", "tempMaxima", "dewPoint", "precipitacao", "windSpeed"),
			std.col=c("wdate", "srad", "tmin", "tmax", "tdew", "prec", "wind"), stringsAsFactors=FALSE)
	stns <- sqlFetch(con, "geowarehouse.weather_stations")
	
	
	for (i in 1:length(csvfiles)){
		csvfile <- csvfiles[i]
		dat <- read.csv(csvfile, stringsAsFactors=FALSE)
		stn_id <- match(dat$id_estacao[1],stns$station_name)
		# Parse date
		dtsep <- ifelse(sum(grepl("-",dat$data))==nrow(dat),"-","/")
		mdy <- matrix(unlist(strsplit(dat$data,dtsep)), ncol=3, byrow=TRUE)
		mdcols <- which(colMeans(nchar(mdy))==2)    
		ycol <- which(colMeans(nchar(mdy))==4)
		for(cc in mdcols){      
			if(sum(as.numeric(mdy[,cc])>12)>0) dcol <- cc else mcol <- cc
		}
		dat$data <- as.Date(paste(mdy[,ycol],mdy[,mcol],mdy[,dcol],sep="/"))
		dat[dat==-99] <- NA
		geoclim_pro.validation_xd <- dat[, cols$col]
		geoclim_pro.validation_xd[, -1] <- round(geoclim_pro.validation_xd[, -1],2)*100
		colnames(geoclim_pro.validation_xd) <- cols$std.col
		geoclim_pro.validation_xd$vapr <- NA 
		geoclim_pro.validation_xd <- data.frame(station_id=ifelse(is.na(stn_id),nrow(stns)+1,stn_id), geoclim_pro.validation_xd)
		
		geowarehouse.weather_stations <- data.frame(station_id=ifelse(is.na(stn_id),nrow(stns)+1,stn_id), station_name=dat$id_estacao[1],
				lat=dat$latitude[1], lon=dat$longitude[1], source="Embrapa", start_date=min(dat$data), 
				end_date=max(dat$data), remarks="c/o Alenxandre Heinemann", last_update=Sys.Date(),
				table_name="geoclim_pro.validation_xd")
		if (is.na(stn_id)){
			sqlSave(con,geowarehouse.weather_stations, rownames=FALSE, append=TRUE) 
		} else sqlUpdate(con,geowarehouse.weather_stations, index="station_id")
		stns <- sqlFetch(con, "geowarehouse.weather_stations")
		sqlSave(con, geoclim_pro.validation_xd, append=TRUE, rownames=FALSE)
	}	
}
