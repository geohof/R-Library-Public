\name{AllocateTVaRs}
\alias{AllocateTVaRs}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
AllocateTVaRs(trialMatrix, returnPeriod)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{trialMatrix}{
%%     ~~Describe \code{trialMatrix} here~~
}
  \item{returnPeriod}{
%%     ~~Describe \code{returnPeriod} here~~
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
function (trialMatrix, returnPeriod) 
{
    numTVaRs <- length(Returnperiod)
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
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
