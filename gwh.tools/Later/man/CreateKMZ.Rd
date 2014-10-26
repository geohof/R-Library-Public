\name{CreateKMZ}
\alias{CreateKMZ}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
CreateKMZ(grid.matrix, grid.col, val.min = min(grid.matrix), val.max = max(grid.matrix), file.path = "", file.name.base, overlay.name, lon.min, lon.max, lat.min, lat.max)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{grid.matrix}{
%%     ~~Describe \code{grid.matrix} here~~
}
  \item{grid.col}{
%%     ~~Describe \code{grid.col} here~~
}
  \item{val.min}{
%%     ~~Describe \code{val.min} here~~
}
  \item{val.max}{
%%     ~~Describe \code{val.max} here~~
}
  \item{file.path}{
%%     ~~Describe \code{file.path} here~~
}
  \item{file.name.base}{
%%     ~~Describe \code{file.name.base} here~~
}
  \item{overlay.name}{
%%     ~~Describe \code{overlay.name} here~~
}
  \item{lon.min}{
%%     ~~Describe \code{lon.min} here~~
}
  \item{lon.max}{
%%     ~~Describe \code{lon.max} here~~
}
  \item{lat.min}{
%%     ~~Describe \code{lat.min} here~~
}
  \item{lat.max}{
%%     ~~Describe \code{lat.max} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (grid.matrix, grid.col, val.min = min(grid.matrix), 
    val.max = max(grid.matrix), file.path = "", file.name.base, 
    overlay.name, lon.min, lon.max, lat.min, lat.max) 
{
    num.lon <- ncol(grid.matrix)
    num.lat <- nrow(grid.matrix)
    lon.increment <- (lon.max - lon.min)/(num.lon - 1L)
    lat.increment <- (lat.max - lat.min)/(num.lat - 1L)
    png(filename = paste(file.path, file.name.base, ".png", sep = ""), 
        height = num.lon + 1L, width = num.lat + 1L, bg = "transparent")
    par(mar = c(0, 0, 0, 0))
    image(grid.matrix, col = grid.col, zlim = c(val.min, val.max), 
        bg = "transparent")
    dev.off()
    png(filename = paste(file.path, file.name.base, "_legend.png", 
        sep = ""), width = 100, height = 200, bg = "white")
    par(mar = c(0, 0, 0, 0))
    plot.new()
    legend(x = "center", legend = round(val.min + (0:9)/9 * (val.max - 
        val.min), 0), fill = grid.col[(0:9)/9 * (length(grid.col) - 
        1L) + 1L])
    dev.off()
    png.name <- paste(file.name.base, ".png", sep = "")
    bmp.name <- paste(file.name.base, ".bmp", sep = "")
    legend.name <- paste(file.name.base, "_legend.png", sep = "")
    kml.str <- paste("<?xml version=\"1.0\" encoding=\"UTF-8\"?>", 
        "\n<kml xmlns=\"http://www.opengis.net/kml/2.2\" ", "xmlns:gx=\"http://www.google.com/kml/ext/2.2\" ", 
        "xmlns:kml=\"http://www.opengis.net/kml/2.2\" ", "xmlns:atom=\"http://www.w3.org/2005/Atom\">", 
        "\n<Folder><name>", overlay.name, "</name>", "\n  <GroundOverlay>\n    <name>", 
        "Overlay", "</name>", "\n    <color>ddffffff</color>", 
        "\n    <Icon><href>", png.name, "</href><viewBoundScale>0.75</viewBoundScale></Icon>", 
        "\n    <LatLonBox>\n      <north>", lat.max, "</north>\n      <south>", 
        lat.min - lon.increment, "</south>\n      <east>", lon.max + 
            lon.increment, "</east>\n      <west>", lon.min, 
        "</west>\n   </LatLonBox>\n  </GroundOverlay>", "\n  <ScreenOverlay>", 
        "<name> Legend</name>", "\n    <Icon><href>", legend.name, 
        "</href></Icon>", "\n    <overlayXY x=\"0\" y=\"1\" xunits=\"fraction\" yunits=\"fraction\"/>", 
        "\n    <screenXY x=\"0\" y=\"1\" xunits=\"fraction\" yunits=\"fraction\"/>", 
        "\n    <rotationXY x=\"0\" y=\"0\" xunits=\"fraction\" yunits=\"fraction\"/>", 
        "\n    <size x=\"0\" y=\"0\" xunits=\"fraction\" yunits=\"fraction\"/>", 
        "\n  </ScreenOverlay>", "\n</Folder>\n</kml>", sep = "")
    write(x = kml.str, file = paste(file.path, file.name.base, 
        ".kml", sep = ""))
    system(paste("\"C:/Users/georg.hofmann/Downloads/7za.exe\" a \"", 
        getwd(), "/", file.path, file.name.base, ".zip", "\" \"", 
        getwd(), "/", file.path, file.name.base, ".png", "\" \"", 
        getwd(), "/", file.path, file.name.base, "_legend.png", 
        "\" \"", getwd(), "/", file.path, file.name.base, ".kml", 
        sep = ""))
    file.rename(from = paste(file.path, file.name.base, ".zip", 
        sep = ""), to = paste(file.path, file.name.base, ".kmz", 
        sep = ""))
    file.remove(paste(file.path, file.name.base, ".kml", sep = ""))
    file.remove(paste(file.path, file.name.base, ".png", sep = ""))
    file.remove(paste(file.path, file.name.base, "_legend.png", 
        sep = ""))
    file.copy(from = paste(file.path, file.name.base, ".kmz", 
        sep = ""), to = paste("C:/Users/georg.hofmann/Google Drive/Tmp/kml/", 
        file.name.base, ".kmz", sep = ""))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
