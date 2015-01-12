# Author: Robert J. Hijmans and Jorell Aunario, r.hijmans@gmail.com
# License GPL3
# Version 0.1  January 2009

# TODO use setMethod (cell or point)
DBgetWthXY <- function(con, tablename, lon, lat, alt=2, rst=raster(), ...) {
	cell <- cellFromXY(rst, c(lon, lat))
	wc <- DBgetWthCell(con, tablename, cell, ...)
	wc@lon <- lon
	wc@lat <- lat
	wc@alt <- 0	
  return(wth)
}

DBgetWthCell <- function(con, tablename, cell, verbose=FALSE, year="all") {
    
    #wthset <- sqlQuery(con, paste("SELECT * FROM datasets WHERE table_name=", shQuote(tablename), sep="")) 
    #maskset <- sqlQuery(con, paste("SELECT * FROM masksets WHERE maskset_id=", wthset$maskset_id, sep=""))
    
    #baseraster <- raster()
    #baseraster <- crop(baseraster, extent(maskset$xmin, maskset$xmax, maskset$ymin, maskset$ymax))       
    #res(baseraster) <- c(maskset$xres, maskset$yres)

    #if(!require(RODBC)) stop("Required package RODBC not found. Please install RODBC.")
	# use rodbcExt to be able to retry connections automatically
    #db <- odbcConnect(database)
	
	if (year=='all'){
        query <- paste("SELECT * FROM", tablename, "WHERE cell =", cell)    
    } else {
        query <- paste("SELECT * FROM", tablename, "WHERE cell =", cell, "AND YEAR(wdate) IN (",paste(year, collapse=","),")")
    }
	w <- sqlQuery(con, query)
	#odbcClose(db)
	year <- yearFromDate(w$wdate)
	doy <- doyFromDate(w$wdate)
	vars <- colnames(w)[-which(colnames(w) %in% c("cell","wdate"))]
	w <- cbind(w$wdate,year,doy,w[,vars])
    
    colnames(w) <- c("date", "year", "doy", vars)
	
        
    wth <- new('weather')
	wth@lon <- -9999
    wth@lat <- -9999
	wth@alt <- -9999
	wth@w <- w
	
	return(wth)     
}

DBgetWthCellNoDSN <- function(tablename, cell, user, pwd, driver="MySQL ODBC 5.1 Driver", server="geo.irri.org", database="nasa") {
	require(RODBC)
	connString <- paste("'DRIVER={",driver,"};SERVER=",server,";DATABASE=",database,";USER=",user,";PASSWORD=", pwd,";OPTION=27;",sep="'")
	cnt <-0
	repeat {
		cnt<-cnt+1
		db <- odbcDriverConnect(connString)
		if (db!=-1){
			break
		}
		else if (cnt > 4) {
			cat("Unable to connect to server (cell=", cell,") \n", sep="")
			stop();
		}
		rm(db)
		cat("Retrying to connect. (cell=",cell,", retries=",cnt, ") \n", sep="")
	}
	query <- paste("SELECT * FROM", tablename, "WHERE cell =", cell)
	data <- sqlQuery(db, query)
	odbcClose(db)
	rm(db)
	colnames(data) <- c("cell", "day", "srad", "tmax", "tmin", "prec", "tdew", "temp", "relh")
	return(data[,-1])     
}

DBgetWthLWXY <- function(database, tablename, lon, lat, rst=raster()) {
	cell <- cellFromXY(rst, c(lon, lat))
	return(DBgetWthLWCell(database, tablename, cell, lat))
}	

DBgetWthLWCell <- function(database, tablename, cell, latitude) {
	w <- DBgetWthCell(database, tablename, cell)
	w$lfwt <- leafWetWithRain(w, FALSE)
	return(w)
}	

DBgetWthLWCellNoDSN <- function(tablename, cell, latitude, user, pwd, ...) {
	w <- DBgetWthCellNoDSN(tablename, cell, user, pwd)
	w$lfwt <- leafWetWithRain(w, FALSE)
	return(w)
}

AccessGetWthXY <- function(database, tablename, lon, lat, rst=raster()) {
	cell <- cellFromXY(rst, c(lon, lat))
	return(AccessGetWthCell(database, tablename, cell))   }	

	
AccessGetWthCell <- function(database, tablename, cell) {
	require(RODBC)
	query <- paste("SELECT * FROM", tablename, "WHERE cell =", cell)
	db <- odbcConnectAccess(database)
	w <- sqlQuery(db, query)
	odbcClose(db)
	colnames(w) <- c("cell", "day", "prec", "relh", "srad", "tmax", "tmin")
	rhnx <- rhMinMax(w$relh, w$tmin, w$tmax) 
	w$rhmn <- rhnx[,1]
	w$rhmx <- rhnx[,2]
	return(w[,-1])     
}	

AccessGetCellNumbers <- function(database, tablename) {
	require(RODBC)
	query <- paste("SELECT cell FROM", tablename, "GROUP BY cell") 
	db <- odbcConnectAccess(database)
	data <- sqlQuery(db, query)
	odbcClose(db)
	return(as.vector(data[,1]))
}
