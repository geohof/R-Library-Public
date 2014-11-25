require(maps) # install.packages("maps")
require(gwh.tools)
require(Matrix)

options(stringsAsFactors = FALSE)

s3.data.path <- "s3://middleware-research/useq/EGH-NEW3/4-EGHwVALIDwLIQwFFE2wUSONLY/"
s3.input.folder <- "EGHoriginal_PCADD"

working.directory <- "~/data/"
setwd(working.directory)

s3.input.path <- paste(s3.data.path, s3.input.folder, "_Meta/", sep="")
s3.output.path <- paste(s3.data.path, s3.input.folder, "_Vis/", sep="")

#s3.put(file = "~/Config.R", s3.path = paste(s3.output.path, "Config.R", sep=""))
#s3.source(s3.path = paste(s3.output.path, "Config.R", sep = ""))


l <- s3.readRDS(s3.path = paste(s3.input.path, "ResultBin.RDS", sep=""))


haz.id <- l$hazard.meta$haz.id[l$hazard.meta$haz.name=="SD1"]

bucket.index <- max(which(l$bucket.meta$haz.id==haz.id))

grid.data <- l$grid.matrix[,bucket.index]

grid.data <- grid.data[order(grid.data, decreasing = TRUE)]

grid.data <- grid.data[1:1000]

which.grid <- match(names(grid.data), l$grid.meta$grid.id)
lat <- l$grid.meta$lat[which.grid]
lon <- l$grid.meta$lon[which.grid]
MapDots(lon,lat,val = 1, val.bucket = c(1,1))

l$bucket.meta[bucket.index,]
s3.write.csv(s3.path = paste(s3.output.path, "TopRiskySD1Grids.csv", sep=""),
             object = grid.data)

head(grid.data)
