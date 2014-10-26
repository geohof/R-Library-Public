\name{WriteTVaRELLayerAllocation}
\alias{WriteTVaRELLayerAllocation}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
WriteTVaRELLayerAllocation(inputFileName, outputFileName, tVaRReturnPeriodVector)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{inputFileName}{
%%     ~~Describe \code{inputFileName} here~~
}
  \item{outputFileName}{
%%     ~~Describe \code{outputFileName} here~~
}
  \item{tVaRReturnPeriodVector}{
%%     ~~Describe \code{tVaRReturnPeriodVector} here~~
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
function (inputFileName, outputFileName, tVaRReturnPeriodVector) 
{
    layerIdName <- "LayerId"
    trialName <- "TrialId"
    lossName <- "Loss"
    MyLogLine("Working directory: ", getwd())
    numTVaRs <- length(tVaRReturnPeriodVector)
    MyLogBegin("Loading losses from file ", inputFileName)
    dataTable <- read.csv(inputFileName, header = TRUE)
    LayerIds <- unique(dataTable[, layerIdName])
    numLayers <- length(LayerIds)
    numTrials <- max(dataTable[, trialName])
    MyLogEnd()
    MyLogLine("Detected ", numTrials, " Trials and ", numLayers, 
        " Layers.")
    MyLogLine("Loaded mean loss: ", sum(dataTable[, "Loss"])/numTrials)
    MyLogBegin("Reorganizing sparse loss data into dense matrix")
    loss <- matrix(data = 0, nrow = numTrials, ncol = numLayers)
    colnames(loss) <- LayerIds
    for (i in 1:numLayers) {
        whichLines <- which(dataTable[, layerIdName] == LayerIds[i])
        loss[dataTable[whichLines, trialName], i] <- dataTable[whichLines, 
            "Loss"]
    }
    MyLogEnd()
    MyLogLine("Calculating EL and TVaRs:")
    output <- matrix(0, numLayers, numTVaRs + 2)
    colnames(output) <- (1:(numTVaRs + 2))
    output[, 1] <- LayerIds
    colnames(output)[1] <- "LayerId"
    output[, 2] <- data.matrix(colSums(loss)/numTrials)
    colnames(output)[2] <- "EL"
    for (i in (1:numTVaRs)) {
        tVaRReturnPeriod <- tVaRReturnPeriodVector[i]
        allocatedTVaR <- data.matrix(AllocateTVaR(trialMatrix = loss, 
            returnPeriod = tVaRReturnPeriod))
        output[, i + 2] <- allocatedTVaR
        colnames(output)[i + 2] <- paste("TVaR", tVaRReturnPeriod, 
            sep = "")
        MyLogLine("Total TVaR", tVaRReturnPeriod, ": ", sum(allocatedTVaR))
    }
    MyLogBegin("Writing results to file ", outputFileName)
    write.csv(output, file = outputFileName, row.names = FALSE)
    MyLogEnd()
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
