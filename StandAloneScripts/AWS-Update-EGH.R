require(gwh.tools)
require(data.table)
require(snow)
require(doParallel)
require(data.table)
require(Rcpp)
require(dplyr)
require(Matrix)

s3.data.path <- "s3://middleware-research/useq/EGH-NEW3/4-EGHwVALIDwLIQwFFE2wUSONLY/"
s3.input.folder <- "EGHoriginal_PCADD_noNulls_HazardTrunc"

s3.update.path <- "s3://middleware-research/Georg/Data/USEQ-EGH-Update/Factors/"

working.directory <- "~/data/"
setwd(working.directory)

s3.input.path <- paste(s3.data.path, s3.input.folder, "/", sep="")
s3.output.path <- paste(s3.data.path, s3.input.folder, "_Adjusted/", sep="")

#s3.get(out.file = "~/Config.R", s3.path = paste(s3.output.path, "Config.R", sep=""))
#s3.put(file = "~/Config.R", s3.path = paste(s3.output.path, "Config.R", sep=""))
s3.source(s3.path = paste(s3.output.path, "Config.R", sep = ""))


part.file <- s3.ls(s3.path = s3.input.path)
part.file <- strsplit(part.file, split="/")
part.file <- unlist(lapply(part.file, FUN = function(x)x[length(x)]))
part.file <- part.file[grep(s3.file.pattern, part.file)]
num.part <- length(part.file)
part.id <- unlist(as.numeric(
  lapply(strsplit(part.file, split = "-"), FUN = function(x) x[length(x)])))
LogLine("Discovered ", num.part, " partition files.")
LogLine("Partition identifier ranges from ", min(part.id),
        " to ", max(part.id))

file.name <- "gll.csv"
gll <- s3.fread(paste(s3.profile.path, file.name, sep=""))
grid.id <- c(0L, gll$grid.id)
num.grid <- length(grid.id)

factor.list <- list()
for(i in 1:6){
  file.name <- 
    paste("Factor-Period", sprintf("%01.0f", i),
          "Mat.RDS", sep="")
  factor.list[[i]] <- s3.readRDS(s3.path=paste(s3.update.path, file.name, sep=""))
}

file.name <- "VGridUGrid.csv"
grid.lookup <- s3.read.csv(s3.path=paste(s3.update.path, file.name, sep=""))

# Remove Alaska and Hawaii:
f <- grid.lookup$dist > 10
LogLine("Removing ", sum(f), " grid.ids for Alaska and Hawaii.")
grid.lookup <- grid.lookup[!f,]


# Remove county level grid points:
f <- grid.lookup$grid.id1 %in% gll$grid.id[gll$grid.match.id==3]
LogLine("Removing ", sum(f), " county level grid.ids.")
grid.lookup <- grid.lookup[!f,]

log.start.seconds <- BeginTimedLog("Creating cluster.")
host.vector <- rep("localhost", 20)
cl <- makeSOCKcluster(names=host.vector)
registerDoParallel(cl)
EndTimedLog(log.start.seconds)


# partition.range <- 0L:999L
#for(i in 1:5){
#for(i in 1:num.part){
tmp.out <- foreach(i=1:num.part) %dopar% {
  setwd(working.directory)
  require(data.table)
  require(gwh.tools)
  egh <- s3.fread(s3.path=paste(s3.input.path, part.file[i], sep="")) 
  egh.class <- unlist(lapply(egh, class))
  for(ii in which(egh.class=="character")){
    egh[[ii]] <- as.numeric(egh[[ii]])
  }
  na.filter <- is.na(egh)
  egh[na.filter] <- 0
    
  usgs.grid.index <- match(egh$V2, grid.lookup$grid.id1)
  tmp.filter <- is.na(usgs.grid.index)
  filtered.grid.index <- usgs.grid.index[tmp.filter]

  for(factor.i in 1L:length(factor.list)){
    col.i <- factor.i + 3L # (Skip event.id, grid.id, MMI)
    egh[[col.i]][tmp.filter] <- egh[[col.i]][tmp.filter] *
      factor.list[[factor.i]][filtered.grid.index]
  }
  
  s3.write.table(s3.path=paste(s3.output.path, part.file[i], sep=""), 
               object = egh, row.names=FALSE, col.names=FALSE)
  remove("egh")
  gc()
}

LogLine("Big Data Process completed.")


stopCluster(cl)

