# Author: Jorrel Khalil S. Aunario, jaunario@gmail.com
# Date :  22 September 2010
# Version 0.0.1
# Licence GPL v3

dd2DMS <- function(dd, lat=T){
    aDD <- abs(dd)
    deg <- trunc(aDD)
    dec <- aDD - deg
    mn <- round(dec*60)
    DMS <- paste(deg, mn)
    if (lat & dd>0){                                
        DMS <- paste(DMS, "N")
    } else if (lat & dd<0){
        DMS <- paste(DMS, "S")
    } else if (!lat & dd>0){
        DMS <- paste(DMS, "E")
    } else {
        DMS <- paste(DMS, "W")
    }
    return(DMS)
}

dms2DD <- function(dms, deg="d",minute="'", sec='"'){
    dms <- trim(dms)
    directions <- substr(dms, nchar(dms), nchar(dms))
    #d <- try(substr(dms,1,regexpr(deg,dms)-1),...) 
    d <- try(as.numeric(trim(substr(dms,1,regexpr(deg,dms)-1))))
    d[which(is.na(d))] <- as.numeric(trim(substr(dms[which(is.na(d))],1,nchar(dms[which(is.na(d))])-1)))
    m <- try(as.numeric(trim(substr(dms,regexpr(deg,dms)+1,regexpr(minute,dms)-1))))
    m[is.na(m)] <- 0 
    s <- try(as.numeric(trim(substr(dms,regexpr(minute,dms)+1,regexpr(sec,dms)-1))))
    s[is.na(s)] <- 0
    md <- s/60
    dd <- d+((m+md)/60)
    dd[!directions %in% c("N","E")] <- -dd[!directions %in% c("N","E")]
    #if (!directions %in% c("N","E")) dd <- -dd  
    return(dd)
}

#dd2UTM <- function(lat,lon){
#    
#}

getISO2 <- function(lat, lon,retries=5){
    cnt <- 0
    iso2fetch <- FALSE
    svcurl <- paste("http://ws.geonames.org/countryCode?lat=",lat,"&lng=",lon,"&username=demo&style=full",sep="")
    #if (is.na(countries1[i])) next
    #if (countries1[i] != i2[167]) next
    while((class(iso2fetch)=="try-error" | class(iso2fetch)=="logical") & cnt<retries){
        cat(svcurl,"\n")
        flush.console()
        iso2fetch <- try(scan(svcurl, what='character', quiet=TRUE),silent=TRUE)
        if (class(iso2fetch)=="try-error"){
            cnt <- cnt+1
            cat("Webservice failure on ",svcurl ,".\n Retries ", cnt,". (Will skip after 5th try) \n", sep="")
            flush.console();
        } else if (class(iso2fetch)=="character"){
            if(length(iso2fetch)>1){
                iso2fetch <- ""
            } else {
                if (nchar(iso2fetch)>1){
                    cnt <- cnt+1
                    cat("Webservice failure on ",svcurl ,".\n Retries ", cnt,". (Will skip after 5th try) \n", sep="")
                    flush.console();
                    iso2fetch <- FALSE
                }
            }
        }   
    }
    if (cnt>=retries) iso2fetch <- NA
    return(iso2fetch)         
}
