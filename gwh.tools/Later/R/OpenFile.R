OpenFile <-
function(path="", file.name.base, silent=FALSE, my.warning=""){
	if(file.exists(paste(path, file.name.base, ".RDS", sep=""))){
	  if(!silent)
	    log.start.seconds <- 
			  BeginTimedLog("Loading file ", paste(file.name.base, ".RDS", sep=""))
		loaded.object <- readRDS(file=paste(path, file.name.base, ".RDS", sep=""))
	}else if(file.exists(paste(path, file.name.base, ".rds", sep=""))){
	  if(!silent)
	    log.start.seconds <- 
			  BeginTimedLog("Loading file ", paste(file.name.base, ".rds", sep=""))
		loaded.object <- readRDS(file=paste(path, file.name.base, ".rds", sep=""))
	}else if(file.exists(paste(path, file.name.base, ".csv", sep=""))){
	  if(!silent)
	    log.start.seconds <- 
			  BeginTimedLog("Loading file ", paste(file.name.base, ".csv", sep=""))
		loaded.object <- read.csv(file=paste(path, file.name.base, ".csv", sep=""))
	}else{
    if(my.warning=="")
		stop("Couldn't find file with base name ", file.name.base, " at path ",  path)
    else{
      if(!silent)
        LogLine(my.warning)
      loaded.object="" 
      silent <- TRUE
    }
	}
	if(!silent)
	  EndTimedLog(log.start.seconds)
	return(loaded.object)
}
