require(maps) # install.packages("maps")
require(gwh.tools)
require(Matrix)
require(data.table)

options(stringsAsFactors = FALSE)

event.id <- 10080081
#s3.data.path <- "s3://middleware-research/useq/EGH-NEW3/4-EGHwVALIDwLIQwFFE2wUSONLY/EGHoriginal_PCADD_noNulls_HazardTrunc"
#s3.input.folder <- ""
s3.data.path <- "s3://middleware-research/useq/EGH-NEW3/4-EGHwVALIDwLIQwFFE2wUSONLY/EGHoriginal_PCADD_noNulls_HazardTrunc_Adjusted/"
s3.input.folder <- "Data"

working.directory <- "~/data/"
setwd(working.directory)

s3.input.path <- paste(s3.data.path, s3.input.folder, "/", sep="")
s3.meta.path <- paste(s3.data.path, s3.input.folder, "_Meta/", sep="")
s3.output.path <- paste(s3.data.path, s3.input.folder, "_Vis/", sep="")

#s3.put(file = "~/Config.R", s3.path = paste(s3.output.path, "Config.R", sep=""))
#s3.source(s3.path = paste(s3.output.path, "Config.R", sep = ""))

part.event <- s3.read.csv(s3.path = paste(s3.meta.path, "PartEvent.csv", sep=""))

which.part <- which(event.id==part.event$event.id)
if(length(which.part)!=1)stp("Trouble finding single partition.")
which.part.id <- part.event$part.id[which.part]

egh <- s3.fread(s3.path=
  paste(s3.input.path, "part-", sprintf("%05.f", which.part.id), sep="")) 
egh.class <- unlist(lapply(egh, class))
for(ii in which(egh.class=="character")){
  egh[[ii]] <- as.numeric(egh[[ii]])
}
na.filter <- is.na(egh)
egh[na.filter] <- 0

egh <- egh[egh$V1==event.id]

grid.meta <- s3.read.csv(s3.path = paste(s3.meta.path, "GridMeta.csv", sep=""))

m <- match(egh$V2, grid.meta$grid.id)
if(sum(is.na(m))>0)stop("Grid mismatches.")
na.filter <- is.na(egh)
egh[na.filter] <- 0

egh <- egh[egh$V1==event.id]


grid.meta <- s3.read.csv(s3.path = paste(s3.meta.path, "GridMeta.csv", sep=""))

m <- match(egh$V2, grid.meta$grid.id)

egh <- egh[grid.meta$grid.match.id[m]==0]

m <- match(egh$V2, grid.meta$grid.id)


if(sum(is.na(m))>0)stop("Grid mismatches.")
MapDots(lon = grid.meta$lon[m], lat = grid.meta$lat[m],
        val.bucket = c(seq(from = .25, to = 2.5, by = 0.25), 1/0),
        val = egh$V4, leg.dec = 2, point.cex=.05,
        file="map1.png"
        )

