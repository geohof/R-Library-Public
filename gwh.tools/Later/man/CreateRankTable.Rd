\name{CreateRankTable}
\alias{CreateRankTable}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
CreateRankTable(n = NumTrials, CorMat)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{n}{
%%     ~~Describe \code{n} here~~
}
  \item{CorMat}{
%%     ~~Describe \code{CorMat} here~~
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
function (n = NumTrials, CorMat) 
{
    NumSamples <- nrow(CorMat)
    Samples <- mvrnorm(n = n, mu = rep(0, NumSamples), Sigma = CorMat)
    rankTable <- matrix(data = as.integer(0), nrow = n, ncol = NumSamples)
    colnames(rankTable) <- colnames(CorMat)
    for (i in 1:NumSamples) {
        rankTable[, i] <- rank(Samples[, i], ties.method = "random")
    }
    return(rankTable)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
