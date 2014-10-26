\name{CalculateEPPoisson}
\alias{CalculateEPPoisson}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
CalculateEPPoisson(Rate, Loss, Probability)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Rate}{
%%     ~~Describe \code{Rate} here~~
}
  \item{Loss}{
%%     ~~Describe \code{Loss} here~~
}
  \item{Probability}{
%%     ~~Describe \code{Probability} here~~
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
function (Rate, Loss, Probability) 
{
    NumEvents = NROW(Loss)
    o <- order(Loss, decreasing = TRUE)
    Rate <- Rate[o]
    Loss <- Loss[o]
    if (Probability < Rate[1]) {
        PML <- Loss[1]
    }
    else {
        i <- 2
        SumRate <- Rate[1]
        CumProb <- 1 - exp(-SumRate)
        repeat {
            NewSumRate <- SumRate + Rate[i]
            NewCumProb <- 1 - exp(-NewSumRate)
            if (Probability < NewCumProb) {
                PML <- Loss[i - 1] + (Loss[i] - Loss[i - 1]) * 
                  (Probability - CumProb)/(NewCumProb - CumProb)
                break
            }
            SumRate <- NewSumRate
            CumProb <- NewCumProb
            i <- i + 1
            if (i > NumEvents) {
                PML <- 0
                break
            }
        }
    }
    return(PML)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
