\name{MapDots}
\alias{MapDots}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
MapDots(lon, lat, val, val.bucket = seq(from = min(val), to = max(val), length.out = 6), col.ramp.fun = colorRampPalette(c("gray", "yellow", "red")), col = col.ramp.fun(length(val.bucket) - 1L), leg.dec = as.integer(2 - log(min(val), base = 10)), leg.bucket = round(val.bucket, leg.dec), leg.txt = paste(leg.bucket[1:(length(val.bucket) - 1L)], "-", leg.bucket[2:length(val.bucket)]), file.name, title.str)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{lon}{
%%     ~~Describe \code{lon} here~~
}
  \item{lat}{
%%     ~~Describe \code{lat} here~~
}
  \item{val}{
%%     ~~Describe \code{val} here~~
}
  \item{val.bucket}{
%%     ~~Describe \code{val.bucket} here~~
}
  \item{col.ramp.fun}{
%%     ~~Describe \code{col.ramp.fun} here~~
}
  \item{col}{
%%     ~~Describe \code{col} here~~
}
  \item{leg.dec}{
%%     ~~Describe \code{leg.dec} here~~
}
  \item{leg.bucket}{
%%     ~~Describe \code{leg.bucket} here~~
}
  \item{leg.txt}{
%%     ~~Describe \code{leg.txt} here~~
}
  \item{file.name}{
%%     ~~Describe \code{file.name} here~~
}
  \item{title.str}{
%%     ~~Describe \code{title.str} here~~
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
function (lon, lat, val, val.bucket = seq(from = min(val), to = max(val), 
    length.out = 6), col.ramp.fun = colorRampPalette(c("gray", 
    "yellow", "red")), col = col.ramp.fun(length(val.bucket) - 
    1L), leg.dec = as.integer(2 - log(min(val), base = 10)), 
    leg.bucket = round(val.bucket, leg.dec), leg.txt = paste(leg.bucket[1:(length(val.bucket) - 
        1L)], "-", leg.bucket[2:length(val.bucket)]), file.name, 
    title.str) 
{
    if (!missing("file.name")) {
        png(file.name, width = 2400, height = 1600, pointsize = 48)
    }
    num.bucket <- length(val.bucket) - 1L
    map(database = "world", xlim = c(min(lon), max(lon)), ylim = c(min(lat), 
        max(lat)))
    val.col <- col[pmax(1L, pmin(findInterval(val, val.bucket), 
        num.bucket))]
    points(lon, lat, col = val.col, pch = 19, cex = 0.5)
    map(database = "world", add = TRUE)
    map(database = "state", add = TRUE)
    if (!missing("title.str")) {
        title(main = title.str)
    }
    legend(x = "bottomright", legend = leg.txt, fill = col)
    if (!missing("file.name")) {
        dev.off()
    }
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
