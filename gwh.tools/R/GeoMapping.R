#' A suite of logging functions
#' 
#' %% ~~ A concise (1-5 lines) description of what the function does. ~~
#' 
#' %% ~~ If necessary, more details than the description above ~~
#' 
#' @name Geographic Mapping
#' @param \dots %% ~~Describe \code{\dots} here~~
#' @note %% ~~further notes~~
#' @author %% ~~who you are~~
#' @seealso %% ~~objects to See Also as \code{\link{help}}, ~~~
#' @references %% ~put references to the literature/web site here ~
#' @keywords ~kwd1 ~kwd2
#' @examples
#' 
#' ##---- Should be DIRECTLY executable !! ----
#' ##-- ==>  Define data, use random,
#' ##--	or do  help(data=index)  for the standard data sets.
#' 
#' ## The function is currently defined as
#' function (...) 
#' {
#'     cat(format(Sys.time(), "%H-%M-%S"), ": ", ..., "  ...", sep = "")
#'     return(unclass(Sys.time()))
#'   }
#' 
#' 


#' @export 
#' @rdname geoMapping
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


#' @export 
#' @rdname geoMapping
CreateKMZ <- function(grid.matrix, grid.col, 
                      val.min=min(grid.matrix), 
                      val.max=max(grid.matrix), 
                      file.path="", file.name.base, overlay.name,
                      lon.min, lon.max, lat.min, lat.max){
  num.lon <- ncol(grid.matrix)
  num.lat <- nrow(grid.matrix)
  
  lon.increment <- (lon.max - lon.min) / (num.lon - 1L)
  lat.increment <- (lat.max - lat.min) / (num.lat - 1L)
  
  #  bmp(filename=paste(file.path, file.name.base, ".bmp", sep=""),
  png(filename=paste(file.path, file.name.base, ".png", sep=""),
      #    width=num.lon + 1L, height=num.lat + 1L, bg="transparent")
      height=num.lon + 1L, width=num.lat + 1L, bg="transparent")
  par(mar=c(0,0,0,0))
  image(grid.matrix, 
        col=grid.col,
        zlim=c(val.min, val.max),  
        bg="transparent")
  dev.off()
  
  png(filename=paste(file.path, file.name.base, "_legend.png", sep=""),
      width=100, height=200, 
      bg="white")
  par(mar=c(0,0,0,0))
  plot.new()
  legend(
    x="center",
    legend=round(val.min + (0:9) / 9 * (val.max - val.min), 0), 
    fill = grid.col[(0:9) / 9 * (length(grid.col) - 1L) + 1L])
  dev.off()
  
  
  
  png.name <- paste(file.name.base, ".png", sep="")
  bmp.name <- paste(file.name.base, ".bmp", sep="")
  legend.name <- paste(file.name.base, "_legend.png", sep="")
  kml.str <- paste("<?xml version=\"1.0\" encoding=\"UTF-8\"?>", 
                   "\n<kml xmlns=\"http://www.opengis.net/kml/2.2\" ", 
                   "xmlns:gx=\"http://www.google.com/kml/ext/2.2\" ",
                   "xmlns:kml=\"http://www.opengis.net/kml/2.2\" ",
                   "xmlns:atom=\"http://www.w3.org/2005/Atom\">", 
                   "\n<Folder><name>",
                   overlay.name,
                   "</name>",
                   "\n  <GroundOverlay>\n    <name>", 
                   "Overlay",
                   "</name>",
                   #                   "\n    <description>",
                   #                   "<![CDATA[<img src=\"",
                   #                   png.name,
                   #                   bmp.name,
                   #                   "\"/>]]></description>",
                   "\n    <color>ddffffff</color>",
                   #"\n    <gx:balloonVisibility>1</gx:balloonVisibility>", 
                   "\n    <Icon><href>",
                   png.name,
                   #                   bmp.name,
                   "</href><viewBoundScale>0.75</viewBoundScale></Icon>",
                   "\n    <LatLonBox>\n      <north>",
                   lat.max,
                   "</north>\n      <south>",
                   lat.min - lon.increment,
                   "</south>\n      <east>",
                   lon.max + lon.increment,
                   "</east>\n      <west>",
                   lon.min,
                   "</west>\n   </LatLonBox>\n  </GroundOverlay>",
                   "\n  <ScreenOverlay>", 
                   "<name> Legend</name>",
                   "\n    <Icon><href>",
                   legend.name, "</href></Icon>",
                   "\n    <overlayXY x=\"0\" y=\"1\" xunits=\"fraction\" yunits=\"fraction\"/>", 
                   "\n    <screenXY x=\"0\" y=\"1\" xunits=\"fraction\" yunits=\"fraction\"/>",
                   "\n    <rotationXY x=\"0\" y=\"0\" xunits=\"fraction\" yunits=\"fraction\"/>",
                   "\n    <size x=\"0\" y=\"0\" xunits=\"fraction\" yunits=\"fraction\"/>",
                   "\n  </ScreenOverlay>",
                   "\n</Folder>\n</kml>",
                   sep="")
  write(x=kml.str, file=paste(file.path, file.name.base, ".kml", sep=""))
  system(paste("\"C:/Users/georg.hofmann/Downloads/7za.exe\" a \"",
               getwd(), "/", file.path, file.name.base, ".zip",
               "\" \"",
               getwd(), "/", file.path, file.name.base, ".png",
               #               getwd(), "/", file.path, file.name.base, ".bmp",
               "\" \"",
               getwd(), "/", file.path, file.name.base, "_legend.png",
               "\" \"",
               getwd(), "/", file.path, file.name.base, ".kml",
               sep=""))
  file.rename(from=paste(file.path, file.name.base, ".zip", sep=""), 
              to=paste(file.path, file.name.base, ".kmz", sep=""))
  file.remove(paste(file.path, file.name.base, ".kml", sep=""))
  file.remove(paste(file.path, file.name.base, ".png", sep=""))
  file.remove(paste(file.path, file.name.base, "_legend.png", sep=""))
  
  
  file.copy(from=paste(file.path, file.name.base, ".kmz", sep=""),
            to=paste("C:/Users/georg.hofmann/Google Drive/Tmp/kml/", 
                     file.name.base, ".kmz", sep=""))
}


