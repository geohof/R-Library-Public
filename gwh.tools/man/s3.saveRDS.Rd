\name{s3.saveRDS}
\alias{s3.saveRDS}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{SaveRDS to s3}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
s3.saveRDS(s3.path, object)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{s3.path}{
%%     ~~Describe \code{s3.path} here~~
}
  \item{object}{
%%     ~~Describe \code{object} here~~
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
function (s3.path, object) 
s3.apply.put(s3.path, object, saveRDS, options = "-m binary/octet-stream")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
