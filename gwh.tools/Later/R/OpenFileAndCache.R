OpenFileAndCache <-
function(path, file.name.base, silent=FALSE, my.warning=""){
  if(file.exists(paste(path, file.name.base, ".RDS", sep=""))){
    if(file.exists(paste(path, file.name.base, ".csv", sep=""))){
      t <- as.numeric(difftime(time1=file.info(paste(path, file.name.base, ".csv", sep=""))$mtime,
                               time2=file.info(paste(path, file.name.base, ".RDS", sep=""))$mtime))
      if(t<0){
        if(!silent)
          log.start.seconds <- 
          BeginTimedLog("Loading file ", paste(file.name.base, ".RDS", sep=""))
        loaded.object <- readRDS(file=paste(path, file.name.base, ".RDS", sep=""))
      }else{
        if(!silent)
          log.start.seconds <- 
          BeginTimedLog("Loading file ", paste(file.name.base, ".csv", sep=""))
        loaded.object <- read.csv(file=paste(path, file.name.base, ".csv", sep=""))
        if(!silent){
          EndTimedLog(log.start.seconds)
          BeginTimedLog("Saving file ", paste(file.name.base, ".RDS", sep=""))      
        }
        saveRDS(loaded.object, file=paste(path, file.name.base, ".RDS", sep=""))
        
      }        
    }else{
      if(!silent)
        log.start.seconds <- 
        BeginTimedLog("Loading file ", paste(file.name.base, ".RDS", sep=""))
      loaded.object <- readRDS(file=paste(path, file.name.base, ".RDS", sep=""))
    }
  }else{      
    if(!silent)
      log.start.seconds <- 
      BeginTimedLog("Loading file ", paste(file.name.base, ".csv", sep=""))
    loaded.object <- read.csv(file=paste(path, file.name.base, ".csv", sep=""))
    if(!silent){
      EndTimedLog(log.start.seconds)
      BeginTimedLog("Saving file ", paste(file.name.base, ".RDS", sep=""))      
    }
      saveRDS(loaded.object, file=paste(path, file.name.base, ".RDS", sep=""))
  }
  if(!silent)
    EndTimedLog(log.start.seconds)
  return(loaded.object)
}
