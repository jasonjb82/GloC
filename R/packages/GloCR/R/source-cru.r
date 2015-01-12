# Author: Jorrel Khalil S. Aunario, jaunario@gmail.com
# Date :  13 January 2014
# Version 0.0.1
# Licence GPL v3

cru.inventory <- function(crudir=".", vars=c("cld","dtr","frs","pet","pre","tmn","tmp","tmx","vap","wet")){
	years <- 1901:2012
	months <- 1:12
	
	cru.inv <- expand.grid(months,years)[,2:1]
	colnames(cru.inv) <- c("year","month")
	
	
	for (i in 1:length(vars)) {
		show.message("Scanning CRU directory. (",crudir,"/",vars[i],")", EL=TRUE, appendLF=FALSE)
		cru.inv[,vars[i]] <-NA 
		vardir <- paste(crudir,vars[i],sep="/")
		filename <- dir(path=vardir, pattern=paste("^",vars[i],"_",sep=""),full.names=TRUE)
		year <- as.numeric(substr(basename(filename),5,8))
		month <- as.numeric(substr(basename(filename),10,nchar(basename(filename))))
		cru.inv[match(paste(year,month,sep="-"),paste(cru.inv$year,cru.inv$month,sep="-")),vars[i]] <- filename 
	}
	show.message("CRU Inventory Created.", EL=TRUE, appendLF=TRUE)
	return(cru.inv)
	
} 

get.cru <- function(vars=c("cld","dtr","frs","pet","pre","tmn","tmp","tmx","vap","wet"), year=1901, month=1,...){
	if(!exists("cru.inv")) cru.inv <<- cru.inventory(...)
	crustack <- stack(unlist(cru.inv[which(cru.inv$year==year&cru.inv$month==month),vars]))
	cells <- which(!is.na(crustack[[1]][]))
	wth <- new("weather")
	wth@stn <- "Climate Research Unit - British Atmospheric Data Centre"
	wth@lon <- c(xmin(crustack),xmax(crustack))
	wth@lat <- c(ymin(crustack),ymax(crustack))
	wth@w <- as.data.frame(cbind(1:ncell(crustack),rep(year,ncell(crustack)), rep(month,ncell(crustack)), values(crustack)))
	wth@w <- wth@w[which(rowSums(!is.na(wth@w[,vars]))>0),]
	colnames(wth@w) <- c("cell","year","month",vars)
	wth@w[,c("cld","dtr","pet","pre","tmn","tmp","tmx","vap")] <- wth@w[,c("cld","dtr","pet","pre","tmn","tmp","tmx","vap")]*10
	return(wth)
}
