# Author: Jorrel Khalil S. Aunario, jaunario@gmail.com
# Date :  27 March 2014
# Version 0.0.1
# Licence GPL v3

get.jobs <- function(initjobs, jobfile="jobs.Rdata",workload=500,delay=10, maxtries=100){
	myjob <- vector()
	worker.id <- Sys.getpid()
	
	if(!file.exists("files.csv")){
		filelock <- data.frame(filename=character(0),worker=numeric(0), stringsAsFactors=FALSE)		
	} else {
		filelock <- read.csv("files.csv",stringsAsFactors=FALSE)
	}
	
	if(is.na(match(jobfile,filelock$filename))){
		jobs <- initjobs
		save(jobs, file=jobfile)		
		new.id <- nrow(filelock)+1
		filelock[new.id,] <- NA		
		filelock$filename[new.id] <- jobfile   
		write.csv(filelock,"files.csv",row.names=FALSE)		 
	} 
	
	tries <- 0
	repeat{
		filelock <- read.csv("files.csv",stringsAsFactors=FALSE)
		if(is.na(filelock$worker[match(jobfile,filelock$filename)])){
			filelock$worker[match(jobfile,filelock$filename)] <- worker.id
			write.csv(filelock,"files.csv",row.names=FALSE)
			load(jobfile)
			if(length(jobs)>0){
				myjob <- jobs[1:min(workload,length(jobs))] 
				jobs <- jobs[!jobs %in% myjob]
				save(jobs, file=jobfile)				
			}
			filelock$worker[filelock$filename==jobfile] <- NA
			write.csv(filelock,"files.csv",row.names=FALSE)
			break
		} else if (tries<maxtries){
			message("Job directory currently in use. Waiting.", appendLF=TRUE)
			Sys.sleep(delay)	
			tries <- tries+1
		} else {
			message("Too many workers queued. Try again next time.", appendLF=TRUE)
			break
		}	
	}
	return(myjob)
}

combine.output <- function(newdata, out.rdata="output.Rdata",delay=10){
	success <- FALSE
	
	if(!file.exists("files.csv")){
		filelock <- data.frame(filename=character(0),worker=numeric(0), stringsAsFactors=FALSE)		
	} else {
		filelock <- read.csv("files.csv",stringsAsFactors=FALSE)
	}
	
	if(!file.exists(out.rdata)){
		filelock <- read.csv("files.csv",stringsAsFactors=FALSE)		
	}
	success <- TRUE
	return(success)	
}
