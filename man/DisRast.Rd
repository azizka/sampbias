\name{DisRast}
\alias{DisRast}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Distance rasters from a list of geographic gazeteers
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
DisRast(gaz, ras, buffer, ncores)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{gaz}{
%%     ~~Describe \code{gaz} here~~
}
  \item{ras}{
%%     ~~Describe \code{ras} here~~
}
  \item{buffer}{
%%     ~~Describe \code{buffer} here~~
}
  \item{ncores}{
%%     ~~Describe \code{ncores} here~~
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
function (gaz, ras, buffer, ncores) 
{
    if (ncores > 1) {
        if (!requireNamespace("geosphere", quietly = TRUE)) {
            stop("parallel needed for multithreading. Please install the package.", 
                call. = FALSE)
        }
        else {
            cl <- parallel::makeCluster(ncores)
        }
    }
    e <- raster::extent(ras)
    cut.off <- e + buffer
    gaz.crop <- lapply(gaz, function(k) raster::crop(k, cut.off))
    check <- sapply(gaz.crop, is.null)
    if (any(check)) {
        gaz.crop <- gaz.crop[check]
        warning(paste("no reference found for ", names(gaz.crop)[!check], 
            " Increase buffer."))
    }
    if (all(check)) {
        warning("no references found within study boundaries. Increase buffer")
        warning("falling back to species and occurence raster")
        return(FALSE)
    }
    check2 <- lapply(gaz.crop, function(k) sum(is.na(k@data[, 
        1])))
    if (any(check2 > 0)) {
        warnings("gazeteers have NA values in first column, the corresponding entries will eb ignored")
    }
    r <- raster(cut.off)
    res(r) <- res
    r[] <- 0
    if (ncores == 1) {
        dist.r <- lapply(gaz.crop, function(k) raster::rasterize(x = k, 
            y = r, field = 1, fun = "count"))
        dist.d <- lapply(dist.r, function(k) raster::distance(k))
    }
    else {
        parallel::clusterExport(cl, "gaz.crop")
        parallel::clusterExport(cl, "r")
        parallel::clusterEvalQ(cl, library(raster))
        dist.r <- parallel::parLapply(cl, gaz.crop, function(k) raster::rasterize(x = k, 
            y = r, field = 1, fun = "count"))
        dist.d <- parallel::parLapply(cl, dist.r, function(k) raster::distance(k))
        parallel::stopCluster(cl)
    }
    dist.out <- lapply(dist.d, function(k) raster::crop(k, extent(ras)))
    return(dist.out)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
