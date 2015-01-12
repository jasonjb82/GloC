# Author: Jorrel Khalil S. Aunario, jaunario@gmail.com
# Date :  14 March 2013
# Version 0.0.2
# Licence GPL v3

.fetch <- function(cells, con, wset, stdate=as.Date("1983-1-1"), endate=Sys.Date(), vars=NULL, timestep=TS.daily, ...){
	#function(cells, con, wset, stdate=Sys.Date()-as.numeric(format(Sys.Date(),"%j"))+1, endate=Sys.Date(), vars=NULL, ...){
		
	#INPUT CLEANUP
	# remove invalid (NAs) cells
	cells <- cells[!is.na(cells)]
	
	# parameter vars general checks and query component construction 
	if (length(vars)>1){
		invalids <- which(is.na(vars))
		if (length(invalids)>0) stop ("Invalid vars specification detected.")		
	} else if (length(vars)==0){
		vars <- "*"
	} else if ((length(vars)==1 & (is.na(vars) | tolower(vars)=="all" | vars=="*"))){
		vars <- "*"
	} 
	
	# Date check
	if(stdate>endate){
		warning(format(stdate,"%x"), " > ",format(endate,"%x"))
		dd <- stdate
		stdate <- endate
		endate <- dd
	}
	
	# Preventive measures for known MySQL-born issues (i.e cannot open table, disconnected RODBC object) 
	preventivem <- try(sqlQuery(con, "flush tables"))	
	if (class(preventivem)=="try-error") {
		con <- odbcReConnect(con)
	}
	
	# QUERY CONSTRUCTION
	# Column list
	if (length(vars)>1){
		ts <- switch(timestep, "wdate as date", "wdate as date","yr as year, mo as month", "yr as year")
		vars <- paste("cell", ts, paste(vars, collapse=", "), sep= ", ")	
	} else if ((length(vars)==1 & vars!="*")){
		ts <- switch(timestep, "wdate as date", "wdate as date","yr as year, mo as month", "yr as year")
		vars <- paste("cell", ts, paste(vars, collapse=", "), sep= ", ")	
	}
	
	# time filter clause	
	timefilter <- switch(timestep, paste("wdate BETWEEN", shQuote(stdate), "AND", shQuote(endate)),
			paste("wdate BETWEEN", shQuote(stdate), "AND", shQuote(endate)),
			ifelse(yearFromDate(stdate)!=yearFromDate(endate),paste("(yr=",yearFromDate(stdate)," AND mo>=", monthFromDate(stdate),") OR (yr=",yearFromDate(endate)," AND mo<=", monthFromDate(endate),")",sep=""),paste("yr=", yearFromDate(stdate), " AND (mo BETWEEN ", monthFromDate(stdate), " AND ", monthFromDate(endate), ")",sep="")),
			paste("yr BETWEEN ", yearFromDate(stdate), " AND ", yearFromDate(endate), sep=""))
	
	# order clause
	orderclause <- switch(timestep, "cell, wdate", 
			"cell, wdate", 
			"cell, yr, mo",
			"cell, yr",)
	
	query <- paste("SELECT ", vars, " FROM ", wset, " WHERE (", timefilter , ") AND (cell IN (",paste(cells, collapse=", ") ,")) ORDER BY ", orderclause, sep="")
	
	# QUERY RUN
	data <- sqlQuery(con, query, ...)
	
	return(data)
}

if ( !isGeneric("geoclimate.fetch") ) {
	setGeneric("geoclimate.fetch", function(xy, srcvars, connection, ...)
				standardGeneric("geoclimate.fetch"))
}



#setMethod("geoclimate.fetch", signature(xy="matrix", srcvars="list", connection="RODBC"),
fetch.monthly <-	function(xy, srcvars, connection, warehouse="geowarehouse",...){
			
			# Connect to database
			#connection <- odbcConnect(connection)
			
			# Get dataset meta data for location matching
			srcmeta <- sqlQuery(connection,paste("SELECT * FROM ", warehouse,".climate_data WHERE timestep='daily' AND table_name in (", paste(shQuote(unique(names(srcvars))),collapse=", "),")", sep=""), stringsAsFactors=FALSE)
			maxres <- NA
			for (i in 1:length(srcvars)){
				srcm <- srcmeta[srcmeta$table_name==names(srcvars)[i],]
				if (srcm$type=="grid"){
					srcraster <- raster(xmn=srcm$xmin, xmx=srcm$xmax, ymn=srcm$ymin, ymx=srcm$ymax, nrow=srcm$nrow, ncol=srcm$ncol)
					baseraster <- raster()
					res(baseraster) <- res(srcraster)

					# determine psudo-station number (basegrid + basegridcell)
					if (is.na(maxres)|maxres>res(srcraster)[1]){
						maxres <- res(srcraster)[1]						
					} 
					
					cells <- cellFromXY(srcraster,xy)
					
					stdcells <- cellFromXY(baseraster,xy)
					tmp <- .fetch(cells=stdcells, con=connection, wset=paste(srcm$schema_name,srcm$table_name, sep=".") , vars=srcvars[[i]], timestep=TS.monthly, ...)
					if(length(srcvars[[i]])==1 & (is.na(srcvars[[i]]) | tolower(srcvars[[i]])=="all" | srcvars[[i]]=="*")) srcvars[[i]] <- colnames(tmp)[!colnames(tmp) %in% c("cell", "wdate", "idx")]
						
				    #tmp <- fetch(cells=stdcells, con=connection, wset=paste(srcm$schema_name,srcm$table_name, sep=".") , vars=srcvars[[i]]) 
					tmp$idx <- match(tmp$cell, stdcells) 
					tmp[,srcvars[[i]]] <- tmp[,srcvars[[i]]]/srcm$zval
					tmp <- tmp[,-grep("cell", colnames(tmp))]
					
				} else {
					warning("Non-grid type dataset not yet supported. Skipping.")
					# TODO support point type 
					next
				}
				if (!exists("outdat")) outdat <- tmp else outdat <- merge(outdat, tmp, by=c("idx","date"), all=TRUE)
			}
			
			
			basegrid <- raster() 
			res(basegrid) <- maxres			
			cells <- cellFromXY(basegrid,xy)	
			
			#Generate Psudo-station ID based on maximumresolution. If resolution <.1 multiply by 3600 (seconds in 1 degree) else multiply by 60 (mins in 1 degree)
			stn <- ifelse(length(gregexpr("0",unlist(strsplit(as.character(maxres),"\\."))[2])[[1]])>1,maxres*3600,maxres*60) 
			station <- paste(stn, sprintf(paste("%0",nchar(ncell(basegrid)),"d",sep=""),cells),sep="")

			# Construct source string (srcstr) for remarks on weather object
			srcstr <- vector()
			for (i in 1:length(srcvars)){
				srcstr <- c(srcstr, paste(names(srcvars)[i],": ", paste(srcvars[[i]], collapse=", "), sep=""))
			}
			srcstr <- paste(srcstr, collapse="; ")
			
			#Disaggregate into sets by point
			outlist <- list()
			for (i in 1:nrow(xy)){
				wth <- new ("weather")
				wth@stn <- station[i]
				wth@rmk <- srcstr
				wth@lon <- xy[i,1]
				wth@lat <- xy[i,2]
				wth@alt <- -99
				wth@w <- outdat[outdat$idx==i,-(grep("idx", colnames(outdat)))]					 
				outlist[[i]] <- wth
			}
			return(outlist)				
		}
#)

fetch.daily <-	function(xy, srcvars, connection, warehouse="geowarehouse",...){
	
	# Connect to database
	#connection <- odbcConnect(connection)
	
	# Get dataset meta data for location matching
	srcmeta <- sqlQuery(connection,paste("SELECT * FROM ", warehouse,".climate_data WHERE timestep='daily' AND table_name in (", paste(shQuote(unique(names(srcvars))),collapse=", "),")", sep=""), stringsAsFactors=FALSE)
	maxres <- NA
	for (i in 1:length(srcvars)){
		srcm <- srcmeta[srcmeta$table_name==names(srcvars)[i],]
		if (srcm$type=="grid"){
			srcraster <- raster(xmn=srcm$xmin, xmx=srcm$xmax, ymn=srcm$ymin, ymx=srcm$ymax, nrow=srcm$nrow, ncol=srcm$ncol)
			baseraster <- raster()
			res(baseraster) <- res(srcraster)
			
			# determine psudo-station number (basegrid + basegridcell)
			if (is.na(maxres)|maxres>res(srcraster)[1]){
				maxres <- res(srcraster)[1]						
			} 
			
			cells <- cellFromXY(srcraster,xy)
			
			stdcells <- cellFromXY(baseraster,xy)
			tmp <- .fetch(cells=stdcells, con=connection, wset=paste(srcm$schema_name,srcm$table_name, sep=".") , vars=srcvars[[i]], timestep=TS.daily, ...)
			if(length(srcvars[[i]])==1 & (is.na(srcvars[[i]]) | tolower(srcvars[[i]])=="all" | srcvars[[i]]=="*")) srcvars[[i]] <- colnames(tmp)[!colnames(tmp) %in% c("cell", "wdate", "idx")]
			#tmp <- fetch(cells=stdcells, con=connection, wset=paste(srcm$schema_name,srcm$table_name, sep=".") , vars=srcvars[[i]]) 
			tmp$idx <- match(tmp$cell, stdcells) 
			tmp[,srcvars[[i]]] <- tmp[,srcvars[[i]]]/srcm$zval
			tmp <- tmp[,-grep("cell", colnames(tmp))]
			
		} else {
			warning("Non-grid type dataset not yet supported. Skipping.")
			# TODO support point type 
			next
		}
		if (!exists("outdat")) outdat <- tmp else outdat <- merge(outdat, tmp, by=c("idx","date"), all=TRUE)
	}
	
	
	basegrid <- raster() 
	res(basegrid) <- maxres			
	cells <- cellFromXY(basegrid,xy)	
	
	#Generate Psudo-station ID based on maximumresolution. If resolution <.1 multiply by 3600 (seconds in 1 degree) else multiply by 60 (mins in 1 degree)
	stn <- ifelse(length(gregexpr("0",unlist(strsplit(as.character(maxres),"\\."))[2])[[1]])>1,maxres*3600,maxres*60) 
	station <- paste(stn, sprintf(paste("%0",nchar(ncell(basegrid)),"d",sep=""),cells),sep="")
	
	# Construct source string (srcstr) for remarks on weather object
	srcstr <- vector()
	for (i in 1:length(srcvars)){
		srcstr <- c(srcstr, paste(names(srcvars)[i],": ", paste(srcvars[[i]], collapse=", "), sep=""))
	}
	srcstr <- paste(srcstr, collapse="; ")
	
	#Disaggregate into sets by point
	outlist <- list()
	for (i in 1:nrow(xy)){
		wth <- new ("weather")
		wth@stn <- station[i]
		wth@rmk <- srcstr
		wth@lon <- xy[i,1]
		wth@lat <- xy[i,2]
		wth@alt <- -99
		wth@w <- outdat[outdat$idx==i,-(grep("idx", colnames(outdat)))]					 
		outlist[[i]] <- wth
	}
	return(outlist)				
}

#setMethod("geoclimate.fetch", signature(cell="numeric"),
#		function(cell, ...){
#			return(.fetch(cell=cell,...))				
#		}
#)
#
#setMethod("geoclimate.fetch", signature(cell="matrix"),
#	function(cell, ...){
#	
#	}
#)
#
#setMethod("geoclimate.fetch", signature(cell="data.frame"),
#		function(cell, ...){
#			
#})
#
#setMethod("geoclimate.fetch", signature(cell="RasterLayer"),
#		function(cell, ...){
#			
#})
#
