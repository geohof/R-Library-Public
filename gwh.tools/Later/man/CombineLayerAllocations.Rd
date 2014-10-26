\name{CombineLayerAllocations}
\alias{CombineLayerAllocations}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
CombineLayerAllocations(allocationFilenameList, allocationColNameList, outputFilename)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{allocationFilenameList}{
%%     ~~Describe \code{allocationFilenameList} here~~
}
  \item{allocationColNameList}{
%%     ~~Describe \code{allocationColNameList} here~~
}
  \item{outputFilename}{
%%     ~~Describe \code{outputFilename} here~~
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
function (allocationFilenameList, allocationColNameList, outputFilename) 
{
    numAllocationFiles <- length(allocationFilenameList)
    MyLogBegin("Loading data")
    dataTableList <- list()
    layerIds <- c()
    totalNumCols <- 0
    for (i in 1:numAllocationFiles) {
        dataTableList[[i]] <- read.csv(allocationFilenameList[i], 
            header = TRUE)
        layerIds <- unique(c(layerIds, dataTableList[[i]][, 1]))
        totalNumCols <- totalNumCols + ncol(dataTableList[[i]]) - 
            1
    }
    layerIds <- layerIds[order(layerIds)]
    numLayers <- length(layerIds)
    allAllocations <- matrix(0, nrow = numLayers, ncol = totalNumCols)
    colnames(allAllocations) <- 1:totalNumCols
    totalNumCols <- 0
    for (i in 1:numAllocationFiles) {
        numCols <- ncol(dataTableList[[i]]) - 1
        allAllocations[match(dataTableList[[i]][, 1], layerIds), 
            (totalNumCols + 1):(totalNumCols + numCols)] <- data.matrix(dataTableList[[i]][, 
            2:(numCols + 1)])
        colnames(allAllocations)[(totalNumCols + 1):(totalNumCols + 
            numCols)] <- paste(allocationColNameList[i], colnames(dataTableList[[i]])[2:(numCols + 
            1)])
        totalNumCols <- totalNumCols + numCols
    }
    MyLogEnd()
    MyLogBegin("Writing data")
    output <- data.frame(layerIds, allAllocations)
    write.csv(output, file = outputFilename, row.names = FALSE)
    MyLogEnd()
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
