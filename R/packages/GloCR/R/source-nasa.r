# Author: Jorrel Khalil S. Aunario, jaunario@gmail.com
# Date :  22 February 2011
# Version 0.0.1
# Licence GPL v3

get.nasa <- function(x, y, vars=c("toa_dwn","swv_dwn","lwv_dwn","T2M", "T2MN","T2MX", "RH2M", "DFP2M","RAIN", "WS10M"),stdate="1983-1-1", endate=Sys.Date(), savepath=getwd(), rm.existing=FALSE){
	if(!require(RCurl)){
		stop("Package RCurl not found.")
	}
	result <- new("weather")
	src <- ""
	if(length(x)!=1|length(y)!=1){
		show.message("Warning: Either x or y has length > 1. Using first only.", appendLF=TRUE)
		x <- x[1]
		y <- y[1]
	}
	result@lon <- x
	result@lat <- y
	
	# check if downloaded file can be saved to disk
	savepath[is.na(savepath)] <- NULL
	proceedwrite <- ifelse(is.character(savepath),force.directories(savepath),FALSE)
	
	cell <- cellFromXY(raster(),t(c(x,y)))
	result@stn <- as.character(cell)
	

	stdate <- as.Date(stdate)
	endate <- as.Date(endate)
	
	fname <- paste(paste("nasa",sprintf("%06i",cell),sprintf("%05.1f",x),sprintf("%05.1f",y),format(stdate,"%Y.%m.%d"),format(endate,"%Y.%m.%d"), sep="_"), ".txt",sep="")
	#dlurl <- paste("http://earth-www.larc.nasa.gov/cgi-bin/cgiwrap/solar/agro.cgi?email=agroclim%40larc.nasa.gov&step=1&lat=",y,"&lon=",x,"&ms=",format(stdate,"%m"),"&ds=",format(stdate,"%d"),"&ys=",format(stdate,"%Y"),"&me=",format(endate,"%m"),"&de=",format(endate,"%d"),"&ye=",format(endate,"%Y"),"&p=swv_dwn&p=T2M&p=T2MN&p=T2MX&p=RH2M&p=DFP2M&p=RAIN&p=WS10M&submit=Submit", sep="")
	dlurl <- paste("http://power.larc.nasa.gov/cgi-bin/cgiwrap/solar/agro.cgi?email=agroclim%40larc.nasa.gov&step=1&lat=",y,"&lon=",x,"&ms=",monthFromDate(stdate),"&ds=",dayFromDate(stdate),"&ys=",yearFromDate(stdate),"&me=",monthFromDate(endate),"&de=",dayFromDate(endate),"&ye=",yearFromDate(endate),"&p=", paste(vars,collapse="&p=",sep=""),"&submit=Submit", sep="")
	
	show.message("Reading ", appendLF=FALSE)
	if (!file.exists(paste(savepath, fname, sep="/"))){
		show.message(dlurl, appendLF=TRUE)
		dlines <- unlist(strsplit(getURL(url=dlurl), "\n"))
		if(!is.null(savepath)) writeLines(dlines, paste(savepath, fname, sep="/"))
		src <- dlurl		
	} else if (rm.existing | file.info(paste(savepath, fname, sep="/"))$size==0){
		show.message(dlurl, appendLF=TRUE)
		file.remove(paste(savepath, fname, sep="/"))
		dlines <- unlist(strsplit(getURL(url=dlurl), "\n"))
		writeLines(dlines, paste(savepath, fname, sep="/"))
		src <- dlurl
	} else {
		show.message(paste(savepath, fname, sep="/"), appendLF=TRUE)
		dlines <- readLines(paste(savepath, fname, sep="/"))
		src <- paste(savepath, fname, sep="/")		
	}
	
	if (class(dlines)=="try-error"){
		msg <- as.character(dlines)
	} else {
		# Check download integrity
		stline <- grep(paste(format(stdate,"%Y"),format(as.numeric(format(stdate,"%j")),width=3)), dlines)
		endline <- grep(paste(format(endate,"%Y"),format(as.numeric(format(endate,"%j")),width=3)), dlines)
		 
		if (length(stline)!=1|length(endline)!=1){
			msg <- paste("Incomplete or No data found on file. If file", fname, "is on disk, remove the file then rerun this program.")
		} else if(length(unlist(strsplit(dlines[endline], "[[:space:]]+")))!=(length(vars)+2)){
			msg <- paste("Incomplete download detected. If file", fname, "is on disk, remove the file then rerun this program.")
		} else {
			msg <- paste("Read from", src)
			if (proceedwrite) writeLines(dlines, paste(savepath, fname, sep="/"))
			alt <- as.numeric(unlist(strsplit(dlines[grep("Elevation", dlines)],"="))[2])
			dlines <- dlines[stline:endline]
			dvector <- unlist(strsplit(dlines, "[[:space:]]+"))
			dvector[dvector=="-"] <- NA
			nasadata <- as.data.frame(matrix(as.numeric(dvector), ncol=(length(vars)+2), byrow=TRUE))
			colnames(nasadata) <- c("yr", "doy", vars)
			
			date <- format(as.Date(paste(nasadata$yr,nasadata$doy),"%Y %j"),"%Y-%m-%d")
			nasadata <- cbind(date, nasadata[,-(1:2)], stringsAsFactors=FALSE)
			
			result@alt <- alt
			result@w <- nasadata
			rm(dlines,dvector,nasadata)
			gc(verbose=FALSE)
		}
	}
	show.message(msg)
	result@rmk <- msg
	return(result)
}
 
#get.nasa(-179.5, 89.5)

