MapDots <-
function(lon, lat, val,
           val.bucket=seq(from = min(val), to = max(val), length.out = 6),
           col.ramp.fun = colorRampPalette(c("gray", "yellow", "red")),
           col = col.ramp.fun(length(val.bucket) - 1L),
           leg.dec = as.integer(2 - log(min(val), base=10)),
           leg.bucket = round(val.bucket, leg.dec),
           leg.txt = paste(leg.bucket[1:(length(val.bucket) - 1L)], "-", leg.bucket[2:length(val.bucket)]),
           file.name, title.str
           ){
  if(!missing("file.name")){
    png(file.name, width = 2400, height = 1600, pointsize=48)
  }

  num.bucket <- length(val.bucket) - 1L
  map(database = "world", 
      xlim = c(min(lon), max(lon)), 
      ylim = c(min(lat), max(lat)))
  
  val.col <- col[pmax(1L, pmin(findInterval(val, val.bucket), num.bucket))]
  points(lon, lat
         , col=val.col
         , pch=19
         , cex=.5)
  map(database = "world", add=TRUE) 
  map(database = "state", add=TRUE) 
  if(!missing("title.str")){
    title(main = title.str)
  }
  legend(
    x="bottomright",
    legend = leg.txt,
    fill = col)
  if(!missing("file.name")){
    dev.off()
    }
}
