require(gwh.tools)
require(data.table)
require(snow)
require(doParallel)
require(data.table)
require(Rcpp)
require(dplyr)
require(Matrix)

s3.data.path <- "s3://middleware-research/useq/EGH-NEW3/3-EGHwVALIDwLIQwFFE2/"
s3.input.folder <- "EGHoriginal_PCADD"

working.directory <- "~/data/"
setwd(working.directory)

s3.input.path <- paste(s3.data.path, s3.input.folder, "/", sep="")
s3.output.path <- paste(s3.data.path, s3.input.folder, "_Meta/", sep="")

#s3.put(file = "~/Config.R", s3.path = paste(s3.output.path, "Config.R", sep=""))
s3.source(s3.path = paste(s3.output.path, "Config.R", sep = ""))


part.file <- s3.ls(s3.path = s3.input.path)
part.file <- strsplit(part.file, split="/")
part.file <- unlist(lapply(part.file, FUN = function(x)x[length(x)]))
part.file <- part.file[grep(s3.file.pattern, part.file)]
num.part <- length(part.file)
LogLine("Discovered ", num.part, " partition files.")

file.name <- "EventRate.csv"
event.table <- s3.fread(paste(s3.profile.path, file.name, sep=""))
# event.id=-1 means event.id populated but not matched
# event.id=0  means event.id not populated
event.id <- c(-1L, 0L, event.table$val.event.id)
event.rate <- c(0, 0, event.table$rate)
num.event <- length(event.id)

file.name <- "gll.csv"
gll <- s3.fread(paste(s3.profile.path, file.name, sep=""))
grid.id <- c(0L, gll$grid.id)
num.grid <- length(grid.id)

file.name <- "BucketThresholds.csv"
threshold.table <- s3.fread(paste(s3.profile.path, file.name, sep=""))
num.threshold <- nrow(threshold.table)
haz.id.vec <- unique(threshold.table$haz.id)
bucket.table <- rbind(
  data.frame(haz.id = threshold.table$haz.id, 
             bucket.id = threshold.table$haz.thresh + 1L,
             bucket.from = threshold.table$haz.val
  ),
  data.frame(haz.id = haz.id.vec, bucket.id = 1L, bucket.from = NA))
bucket.table <- 
  bucket.table[order(bucket.table$haz.id, 
                     bucket.table$bucket.id),]
num.bucket <- nrow(bucket.table)
bucket.table <- cbind(bucket.table, bucket.to = c(bucket.table$bucket.from[2:num.bucket], NA))
bucket.names <- paste("haz", bucket.table$haz.id, 
                      "-buc", bucket.table$bucket.id, sep = "")

working.directory <- getwd()

log.start.seconds <- BeginTimedLog("Creating cluster.")
host.vector <- rep("localhost", 30)
cl <- makeSOCKcluster(names=host.vector)
registerDoParallel(cl)
EndTimedLog(log.start.seconds)



#s3.input.path <- "s3://middleware-research/useq/EGH_FINAL_WLIQ_wFFE/EGH_FINAL_WLIQ_wFFE2.txt/"
# partition.range <- 0L:999L
#for(i in 1:num.part){
tmp.out <- foreach(i=1:num.part) %dopar% {
  setwd(working.directory)
  require(data.table)
  require(Matrix)
  require(dplyr)
  out.file.name <- sprintf("Output-%05.f.RDS", i)
  if(!file.exists(out.file.name)){
    #    log.start.seconds <- BeginTimedLog("Getting file ", s3.file.name)
    system(paste("s3cmd get ", s3.input.path, part.file[i], sep="")) 
    #    EndTimedLog(log.start.seconds)    
    #    log.start.seconds <- BeginTimedLog("Loading file ", s3.file.name)
    egh <- fread(part.file[i], header = FALSE)
    #    tmp.filter <- egh$V1 <= 2481006
    #    if(sum(tmp.filter)>0){
    #      warning("Removing events with RMS eventID.")
    #      egh <- egh[!tmp.filter]
    #    }
    #    EndTimedLog(log.start.seconds)
    na.filter <- is.na(egh)
    egh[na.filter] <- 0
    
    event.index <- match(egh$V1, event.id)
    tmp.filter <- is.na(event.index)
    unmatched.event.id <- unique(egh$V1[tmp.filter])
    if(sum(tmp.filter) > 0L){
      warning("Some eventIDs couldn't be matched.")
      # This will assign event.id=-1 meaning 
      # "event.id populated but not matched"
      event.index[tmp.filter] <- 1L
    }
    
    grid.index <- match(egh$V2, grid.id)
    tmp.filter <- is.na(grid.index)
    if(sum(tmp.filter)>0){
      warning("Some gridIDs couldn't be matched.")
      grid.index[tmp.filter] <- 1L
    }
    
    cum.grid.matrix <- sparseMatrix(dims = c(num.grid, length(bucket.names)),
                                    dimnames = list(grid.id, bucket.names),
                                    i=integer(0),
                                    j=integer(0),
                                    x=numeric(0))
    cum.event.matrix <- sparseMatrix(dims = c(num.event, length(bucket.names)),
                                     dimnames = list(event.id, bucket.names),
                                     i=integer(0),
                                     j=integer(0),
                                     x=numeric(0))
    
    for(haz.id in haz.id.vec){
      threhold.index <- which(threshold.table$haz.id==haz.id)
      threshold.vec <- threshold.table$haz.val[threhold.index]
      j.offset <- min(which(bucket.table$haz.id==haz.id))
      wc <- findInterval(x = egh[[2L+haz.id]], vec = threshold.vec)
      
      
      grid.group.table <- group_by(data.table(grid.index, 
                                              hazard.bucket = wc, 
                                              rate = event.rate[event.index]), grid.index, hazard.bucket)
      grid.agg.table <- summarise(grid.group.table, sum(rate))
      setnames(x = grid.agg.table, old = "sum(rate)", "rate")
      tmp.grid.matrix <- sparseMatrix(dims = c(num.grid, length(bucket.names)),
                                      dimnames = list(grid.id, bucket.names),
                                      i = grid.agg.table$grid.index, 
                                      j = grid.agg.table$hazard.bucket + j.offset,
                                      x = grid.agg.table$rate)
      cum.grid.matrix <- cum.grid.matrix + tmp.grid.matrix
      
      
      event.group.table <- group_by(data.table(event.index, 
                                               hazard.bucket = wc, 
                                               count = 1), event.index, hazard.bucket)
      event.agg.table <- summarise(event.group.table, sum(count))
      setnames(x = event.agg.table, old = "sum(count)", "count")
      tmp.event.matrix <- sparseMatrix(dims = c(num.event, length(bucket.names)),
                                       dimnames = list(event.id, bucket.names),
                                       i = event.agg.table$event.index, 
                                       j = event.agg.table$hazard.bucket + j.offset,
                                       x = event.agg.table$count)
      cum.event.matrix <- cum.event.matrix + tmp.event.matrix
    }
    saveRDS(list(grid.matrix=cum.grid.matrix, 
                 event.matrix=cum.event.matrix,
                 unmatched.event.id=unmatched.event.id), out.file.name)
    file.remove(part.file[i])
  }
}

LogLine("Big Data Process completed.")

LogLine("Files created: ", length(list.files(pattern = "Output-.*")))

stopCluster(cl)

log.start.seconds <- BeginTimedLog("Creating cluster.")
host.vector <- rep("localhost", 10)
cl <- makeSOCKcluster(names=host.vector)
registerDoParallel(cl)
EndTimedLog(log.start.seconds)

reduce.method <- c("add", "add", "unique")

AggFiles <- function(file.in, file.out, num.out, working.directory, reduce.method){
  file.vec = list.files(pattern = file.in)
  num.in <- length(file.vec)
  threshold.vec <- seq(from = 1, to = num.in + 1L, length.out = num.out + 1L)
  partition.vec <- findInterval(x = 1:num.in, threshold.vec)
  tmp.out <- foreach(i=1:num.out) %dopar% {
    setwd(working.directory)
    require(Matrix)
    is.first=TRUE
    for(ii in which(partition.vec==i)){
      if(is.first){
        cum.mat.list <- readRDS(file.vec[ii])
        is.first <- FALSE
      }else{
        mat.list  <- readRDS(file.vec[ii])
        for(iii in 1:length(mat.list)){
          if(reduce.method[iii]=="add"){
            cum.mat.list[[iii]] <- cum.mat.list[[iii]] + mat.list[[iii]]
          }else if(reduce.method[iii]=="unique"){
            cum.mat.list[[iii]] <- unique(c(cum.mat.list[[iii]], mat.list[[iii]]))            
          }
        }
      }      
    }
    saveRDS(cum.mat.list, paste(file.out, sprintf("%05.f.RDS", i - 1L), sep=""))
  }
}

AggFiles(file.in = "Output-.*", file.out = "Out1-", num.out = 100, 
         working.directory, reduce.method = reduce.method)
LogLine("Agg level 1 completed.")
AggFiles(file.in = "Out1-.*", file.out = "Out2-", num.out = 5, 
         working.directory, reduce.method = reduce.method)
LogLine("Agg level 2 completed.")
AggFiles(file.in = "Out2-.*", file.out = "OutputFinal", num.out = 1, 
         working.directory, reduce.method = reduce.method)
LogLine("Agg level 3 completed.")

stopCluster(cl)

file.name <- "OutputFinal00000.RDS"
s3.put(file = file.name, 
       s3.path = paste(s3.output.path, "ResultBin.RDS", sep=""), 
       options = "-m binary/octet-stream")

mat.list <- readRDS("OutputFinal00000.RDS")

sum.mat <- sparseMatrix(dim = c(num.bucket, length(haz.id.vec)),
                        dimnames = list(bucket.names,
                                        paste("haz", haz.id.vec, sep="")),
                        i = 1:num.bucket, 
                        j = bucket.table$haz.id, 
                        x = 1)


event.mat <- mat.list[[2]]


event.sum.mat <- event.mat %*% sum.mat
event.rec.count <- event.sum.mat[,1]
file.name <- "EventRecCount.csv"
write.csv(data.frame(event.id = event.id, 
                     grid.count = as.vector(event.rec.count)), file.name, row.names=FALSE)
s3.put(file = file.name, s3.path = s3.output.path)

file.name <- "EventHazCount.csv"
write.csv(as.matrix(event.mat), file.name, row.names=TRUE)
s3.put(file = file.name, s3.path = s3.output.path)

grid.mat <- mat.list[[1]]


grid.sum.mat <- grid.mat %*% sum.mat
grid.rate <- as.vector(grid.sum.mat[,1])
file.name <- "GridRate.csv"
write.csv(data.frame(grid.id, grid.rate), file.name, row.names=FALSE)
s3.put(file = file.name, s3.path = s3.output.path)

file.name <- "GridHazRate.csv"
write.csv(as.matrix(grid.mat), file.name, row.names=TRUE)
s3.put(file = file.name, s3.path = s3.output.path)


file.name <- "EventMeta.csv"
write.csv(data.frame(event.id, rate=event.rate), file.name, row.names=FALSE)
s3.put(file = file.name, s3.path = s3.output.path)

file.name <- "GridMeta.csv"
write.csv(data.frame(grid.id, lat=c(NA, gll$lat), lon=c(NA, gll$lon)), file.name, row.names=FALSE)
s3.put(file = file.name, s3.path = s3.output.path)

file.name <- "BucketMeta.csv"
write.csv(bucket.table, file.name, row.names=FALSE)
s3.put(file = file.name, s3.path = s3.output.path)

file.name <- "HazardMeta.csv"
write.csv(unique(data.frame(haz.id=threshold.table$haz.id,
                            haz.name=threshold.table$haz.name)), 
          file.name, row.names=FALSE)
s3.put(file = file.name, s3.path = s3.output.path)

#write.csv(as.matrix(fin.mat), "resultAll.csv")
#system("s3cmd put resultAll.csv s3://middleware-research/TEMP_Georg/") 



stopCluster(cl)