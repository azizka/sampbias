\name{summary.sampbias}
\alias{summary.sampbias}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Summary Method for Class sampbias~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
summary.sampbias(object, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{
%%     ~~Describe \code{object} here~~
}
  \item{\dots}{
%%     ~~Describe \code{\dots} here~~
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
function (object, ...) 
{
    cat("Number of occurences: ", object$summa$total_occ, "\n")
    cat("Number of species: ", object$summa$total_sp, "\n")
    cat("Raster resolution: ", object$summa$res, "\n")
    cat("Distance binsize: ", object$summa$binsize, "\n")
    cat("Convexhull: ", object$summa$convexhull, "\n")
    cat("Geographic extent:\n")
    print(object$summa$extent)
    cat("Bias effect at distance:\n")
    print(object$biastable)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
