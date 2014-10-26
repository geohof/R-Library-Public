\name{s3.get}
\alias{s3.get}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Get from s3}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
s3.get(s3.path, out.file = "", options = "")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{s3.path}{
%%     ~~Describe \code{s3.path} here~~
}
  \item{out.file}{
%%     ~~Describe \code{out.file} here~~
}
  \item{options}{
%%     ~~Describe \code{options} here~~
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
function (s3.path, out.file = "", options = "") 
{
    s3.cmd <- paste("s3cmd get", s3.path, out.file, options)
    if (out.file == "") {
        out.file <- strsplit(x = file, split = "/")[[1]]
        out.file <- out.file[length(out.file)]
    }
    res <- system(s3.cmd, intern = TRUE)
    return(out.file)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
