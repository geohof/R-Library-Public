\name{OpenFile}
\alias{OpenFile}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
OpenFile(path = "", file.name.base, silent = FALSE, my.warning = "")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{path}{
%%     ~~Describe \code{path} here~~
}
  \item{file.name.base}{
%%     ~~Describe \code{file.name.base} here~~
}
  \item{silent}{
%%     ~~Describe \code{silent} here~~
}
  \item{my.warning}{
%%     ~~Describe \code{my.warning} here~~
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
function (path = "", file.name.base, silent = FALSE, my.warning = "") 
{
    if (file.exists(paste(path, file.name.base, ".RDS", sep = ""))) {
        if (!silent) 
            log.start.seconds <- BeginTimedLog("Loading file ", 
                paste(file.name.base, ".RDS", sep = ""))
        loaded.object <- readRDS(file = paste(path, file.name.base, 
            ".RDS", sep = ""))
    }
    else if (file.exists(paste(path, file.name.base, ".rds", 
        sep = ""))) {
        if (!silent) 
            log.start.seconds <- BeginTimedLog("Loading file ", 
                paste(file.name.base, ".rds", sep = ""))
        loaded.object <- readRDS(file = paste(path, file.name.base, 
            ".rds", sep = ""))
    }
    else if (file.exists(paste(path, file.name.base, ".csv", 
        sep = ""))) {
        if (!silent) 
            log.start.seconds <- BeginTimedLog("Loading file ", 
                paste(file.name.base, ".csv", sep = ""))
        loaded.object <- read.csv(file = paste(path, file.name.base, 
            ".csv", sep = ""))
    }
    else {
        if (my.warning == "") 
            stop("Couldn't find file with base name ", file.name.base, 
                " at path ", path)
        else {
            if (!silent) 
                LogLine(my.warning)
            loaded.object = ""
            silent <- TRUE
        }
    }
    if (!silent) 
        EndTimedLog(log.start.seconds)
    return(loaded.object)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
