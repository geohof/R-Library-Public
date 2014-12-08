require(maps) # install.packages("maps")
require(gwh.tools)
require(Matrix)

options(stringsAsFactors = FALSE)

s3.data.path <- "s3://middleware-research/useq/EGH-NEW3/4-EGHwVALIDwLIQwFFE2wUSONLY/EGHoriginal_PCADD_noNulls_HazardTrunc_Adjusted/"
s3.input.folder <- "Data"

working.directory <- "~/data/"
setwd(working.directory)

s3.input.path <- paste(s3.data.path, s3.input.folder, "_Meta/", sep="")
s3.output.path <- paste(s3.data.path, s3.input.folder, "_Vis/", sep="")

#s3.put(file = "~/Config.R", s3.path = paste(s3.output.path, "Config.R", sep=""))
#s3.source(s3.path = paste(s3.output.path, "Config.R", sep = ""))


result.list <- s3.readRDS(s3.path = paste(s3.input.path, "ResultBin.RDS", sep=""))
grid.matrix <- result.list$grid.matrix
grid.meta <- s3.read.csv(s3.path = paste(s3.input.path, "GridMeta.csv", sep=""))
grid.filter <- grid.meta$grid.id>0
grid.meta <- grid.meta[grid.filter,] 
grid.matrix <- grid.matrix[grid.filter,] 
grid.id <- grid.meta$grid.id

hazard.meta <- s3.read.csv(s3.path = paste(s3.input.path, "HazardMeta.csv", sep=""))

exc.rate.vector <- c(.0001, .0002, .0005, .001, .002, .005, .01, .02, .05, .1, .2, .5, 1)

map.rp.vec <- c(2475, 475)
#  p = 1- exp(-r)
#  r = -ln(1-p)
map.er.vector <- -log(1 - 1 / map.rp.vec)


bucket.table <- s3.read.csv(s3.path = paste(s3.input.path, "BucketMeta.csv", sep=""))

num.records <- nrow(grid.matrix)

log.start.seconds <- BeginTimedLog("Creating cluster.")
host.vector <- rep("localhost", 10)
cl <- makeSOCKcluster(names=host.vector)
registerDoParallel(cl)
EndTimedLog(log.start.seconds)

#for(haz.i in 1:nrow(hazard.meta)){
foreach(haz.i=1:nrow(hazard.meta)) %dopar% {
  working.directory <- "~/data/"
  
  require(maps) # install.packages("maps")
  require(gwh.tools)
  require(Matrix)
  
  col.filter <- which(bucket.table$haz.id==hazard.meta$haz.id[haz.i])
  exc.rate.mat <- as.matrix(grid.matrix[,col.filter])
  for(i in (ncol(exc.rate.mat) - 1L):1L){
    exc.rate.mat[,i] <- exc.rate.mat[,i] + exc.rate.mat[,i + 1]
  }  
  haz.threshold <- bucket.table$bucket.from[col.filter]
  haz.threshold[1] <- 0
  haz.mat <- matrix(data=0, nrow=num.records, ncol=length(exc.rate.vector))
  rev.col.filter <- col.filter[order(col.filter, decreasing = TRUE)]
  log.start.seconds <- BeginTimedLog("Interpolating data for ", hazard.meta$haz.name[haz.i])
  for(i in 1: num.records){
    x <- c(0, as.vector(exc.rate.mat[i, ncol(exc.rate.mat):1]))
    # Note that we assume that rate has 
    # decayed twice the max hazard value:
    y <- c(max(haz.threshold) * 2, haz.threshold[ncol(exc.rate.mat):1])
    haz.mat[i,] <- 
      approx(yright = 0, ties = "ordered",
             x=x, 
             y=y,
             xout=exc.rate.vector
      )$y
    UpdateLogPercent(100 * i / num.records)
  }
  
  colnames(haz.mat) <- exc.rate.vector
  rownames(haz.mat) <- grid.id
  s3.write.csv(object = haz.mat, 
               s3.path = paste(s3.output.path, hazard.meta$haz.name[haz.i], "_ER.csv", sep=""))
  EndTimedLog(log.start.seconds)

  color.threshold <- haz.threshold[-1]
  round.col.thr <- round(color.threshold, 3 - ceiling(log(x = color.threshold, base = 10)))
  color.pal <- colorRampPalette(c("gray", "yellow", "red"))(length(color.threshold) + 1L)
  #rainbow(n=length(color.threshold) + 1L)
  
  if(!file.exists("RP-Maps")){
    dir.create("RP-Maps")
  }
  for(er.i in 1:length(map.rp.vec)){
    filename <- paste("RP-Maps/", hazard.meta$haz.name[haz.i], "_", 
                      sprintf("%05.0f", map.rp.vec[er.i]), ".png", sep="")     
    png(filename, width = 2400, height = 1600, pointsize=48)
    grid.index <- match(grid.id, grid.meta$grid.id)
    lat <- grid.meta$lat[grid.index]
    lon <- grid.meta$lon[grid.index]
    filter <- !is.na(grid.index)
    lat <- lat[filter]
    lon <- lon[filter]
    below.i <- sum(map.er.vector[er.i]>exc.rate.vector)
    above.i <- below.i + 1L
    below.er <- exc.rate.vector[below.i]
    above.er <- exc.rate.vector[above.i]
    below.val <- haz.mat[,below.i][filter]
    above.val <- haz.mat[,above.i][filter]
    val <- below.val + (above.val - below.val) *
      (map.er.vector[er.i] - below.er) / (above.er - below.er)
    o <- order(val)
    lat <- lat[o]
    lon <- lon[o]
    val <- val[o]
    color.index <- findInterval(val, color.threshold)
    color.id <- color.pal[color.index + 1L]
    map(database = "world", 
        xlim = c(min(lon), max(lon) + 20), 
        ylim = c(min(lat), max(lat)))
    title(main = paste("VRISC ", hazard.meta$haz.name[haz.i], " at return period ", map.rp.vec[er.i],sep =""))
    filter <- 1:length(lat)
    # filter <- sample(x = length(lat), size = length(lat), replace = FALSE)
    #  filter <- sample(x = length(lat), size = 10000, replace = FALSE)
    points(lon[filter], lat[filter]
           , col=color.id[filter]
           , pch=19
           , cex=.2)
    map(database = "world", add=TRUE) 
    legend(
      x="bottomright",
      legend = paste(c("", round.col.thr), "-", c(round.col.thr, "")),
      fill = color.pal
    )
    dev.off()
    s3.put(file = filename,
             s3.path = paste(s3.output.path, filename, sep=""))
  }
}

stopCluster(cl)
