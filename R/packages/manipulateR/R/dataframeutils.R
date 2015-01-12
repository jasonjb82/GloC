# Author: Jorrel Khalil S. Aunario, jaunario@gmail.com
# Date :  30 April 2010
# Version 0.0.1
# Licence GPL v3

clean.dataframe <- function(dat, cols=colnames(dat), addcols=TRUE, rmOtherCols=TRUE){
    miss <- cols[!cols %in% colnames(dat)]
    if (addcols & length(miss)>0){
        for (m in miss){
            dat[,m] <- NA
        }
    } else if(!addcols & length(miss)>0){
        return(FALSE)
        stop("Missing Columns")
    } 
    dchk <- is.na(dat[,cols])
    
    if (nrow(dat)>0){
        
        if (length(cols)>1){
            if(rmOtherCols){
                dat <- dat[!rowSums(dchk)==length(cols),cols]
            }else dat <- dat[!rowSums(dchk)==length(cols),]    
        } else {
            if(rmOtherCols){
                dat <- dat[!dchk,cols]
            }else dat <- dat[!dchk,]
        }
                
    }    
    return(dat)        
}

recodeMissing <- function(dat, cols, old, new=NA){
    for (i in 1:length(cols)){
        change <- which(dat[,cols[i]]==old)
        if(length(change)>0) dat[change,cols[i]] <- new
    }
    return(dat)
}

