\name{ReRankSamples}
\alias{ReRankSamples}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
ReRankSamples(Samples, RankTable, AlreadyInOrder = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Samples}{
%%     ~~Describe \code{Samples} here~~
}
  \item{RankTable}{
%%     ~~Describe \code{RankTable} here~~
}
  \item{AlreadyInOrder}{
%%     ~~Describe \code{AlreadyInOrder} here~~
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
function (Samples, RankTable, AlreadyInOrder = FALSE) 
{
    NumSamples <- NCOL(Samples)
    SampleSize <- nrow(Samples)
    if (AlreadyInOrder) {
        OrderedSamples <- Samples
    }
    else {
        OrderedSamples <- matrix(data = 0, nrow = NumTrials, 
            ncol = NumSamples)
        for (i in 1:NumSamples) {
            OrderedSamples[, i] <- Samples[order(Samples[, i]), 
                i]
        }
    }
    CorrelatedSamples <- matrix(data = 0, nrow = SampleSize, 
        ncol = NumSamples)
    for (i in 1:NumSamples) {
        CorrelatedSamples[, i] <- OrderedSamples[RankTable[, 
            i], i]
    }
    colnames(CorrelatedSamples) <- colnames(Samples)
    return(CorrelatedSamples)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
