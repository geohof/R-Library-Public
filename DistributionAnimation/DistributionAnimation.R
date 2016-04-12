require(animation)
saveGIF(ani.width=800, ani.height=600, interval=.4, 
    		movie.name="RPAnimation.gif",
				outdir=paste(getwd(),"/Animation", sep=""), 
  {
  log.obj <- BeginTimedLog("Creating maps for period ", 
                                     period.vec[period.i])
  file.name <- paste("Factors/Factor-Period", period.i, 
                     ".RDS", sep="")
  factor.matrix <- readRDS(file.name)
  for(er.i in num.er:1){
    file.name.base <- 
      paste("Factor-Period", period.i,
            "ExcRate", sprintf("%01.4f", exc.rate.vector[er.i]),
            sep="")
    file.name.base <- gsub(pattern = "[.]", replacement = "_",
                           file.name.base)
    val.bucket <- c(0.5, .7, .85, .95, 1.05, 1.15, 1.3, 1.5, 1/0)
    par(mar=c(0,0,0,0))
    layout(mat = matrix(c(1,2), ncol=1), heights = c(5,1))
    par(mar=c(0,0,0,0))
    MapDots(lon=lon, lat=lat, val=factor.matrix[,er.i], leg.dec=2, 
            val.bucket=val.bucket,
            col.ramp.fun = colorRampPalette(c("blue", "yellow", "red")),
#            file.name = paste("Maps/", file.name.base, ".png", sep=""),
            map.database="state", title.str = "")
    par(mar=c(3,2,0,2))
    barplot(horiz = T, 1 / exc.rate.vector[er.i], log="x", xlim = c(1,10000),             
            xlab = "Return period"
            )

#     MapDots(lon=lon, lat=lat, val=factor.matrix[,er.i], leg.dec=2, 
#             val.bucket=val.bucket,
#             col.ramp.fun = colorRampPalette(c("blue", "yellow", "red")),
#             file.name = paste("Maps/California/", file.name.base, ".png", sep=""),
#             title.str = "", leg.x="topright", map.database="state", 
#             map.regions="California")
    log.obj <- UpdateLogPercent(log.obj, 100 * er.i / num.er)
  }
  EndTimedLog(log.obj)
}
)
