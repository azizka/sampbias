\name{SamplingBias}
\alias{SamplingBias}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Evaluating Sampling Bias in Species Distribution Data
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
SamplingBias(x, gaz = NULL, res = 1, buffer = 1, convexhull = F, binsize = NULL, biasdist = c(0, 10), terrestrial = T, ncores = 1, plotextra = F, plotextrafile =
                 "samp_bias_extra_plots.pdf", verbose = T)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
%%     ~~Describe \code{x} here~~
}
  \item{gaz}{
%%     ~~Describe \code{gaz} here~~
}
  \item{res}{
%%     ~~Describe \code{res} here~~
}
  \item{buffer}{
%%     ~~Describe \code{buff} here~~
}
  \item{convexhull}{
%%     ~~Describe \code{convexhull} here~~
}
  \item{binsize}{
%%     ~~Describe \code{binsize} here~~
}
  \item{biasdist}{
%%     ~~Describe \code{biasdist} here~~
}
  \item{terrestrial}{
%%     ~~Describe \code{terrestrial} here~~
}
  \item{ncores}{
%%     ~~Describe \code{ncores} here~~
}
  \item{plotextra}{
%%     ~~Describe \code{plotextra} here~~
}
  \item{plotextrafile}{
%%     ~~Describe \code{plotextra} here~~
}
  \item{verbose}{
%%     ~~Describe \code{verbose} here~~
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
function (x, gaz = NULL, res = 1, buff = 1, convexhull = F, binsize = NULL, 
    biasdist = c(0, 10), terrestrial = T, ncores = 1, plotextra = F, 
    verbose = T) 
{
    dat.pts <- sp::SpatialPoints(x[, c("decimallongitude", "decimallatitude")])
    dum.ras <- raster::raster(extent(dat.pts))
    res(dum.ras) <- res
    if (ncell(dum.ras) > 1e+06) {
        warning("huge raster size")
    }
    if (verbose) {
        cat("Creating occurrence raster...")
    }
    occ.out <- .OccRast(x = dat.pts, ras = dum.ras)
    if (verbose) {
        cat(" Done\n")
    }
    if (verbose) {
        cat("Creating species raster...")
    }
    spe.out <- .SpeRast(x, ras = dum.ras, ncores = ncores)
    if (verbose) {
        cat(" Done\n")
    }
    if (verbose) {
        cat("Calculating distance raster...")
    }
    if (is.null(gaz)) {
        warnings("gaz not found, using standard gazeteers")
        gaz <- list(airports = sampbias::airports, cities = sampbias::cities, 
            rivers = sampbias::waterbodies, roads = sampbias::roads)
    }
    check <- all(sapply(gaz, is.raster))
    if (all(check)) {
        dis.ras <- gaz
    }
    else {
        dis.ras <- DisRast(gaz = gaz, ras = dum.ras, buffer = buffer, 
            ncores = ncores)
        if (terrestrial) {
            wrld <- crop(SampBias::landmass, extent(dum.ras))
            wrld <- rasterize(wrld, dum.ras)
            dis.ras <- lapply(dis.ras, function(k) mask(k, wrld))
        }
        dis.mat <- lapply(dis.ras, "as.matrix")
    }
    if (verbose) {
        cat(" Done\n")
    }
    if (is.logical(dis.ras)) {
        out <- c(Occurences = occ.out, Species = spe.out, results)
        class(out) <- append("sampbias", class(out))
        return(out)
    }
    else {
        if (is.null(binsize)) {
            binsize = res * 1e+05 * 1.1
        }
        if (verbose) {
            cat("Extracting values...")
        }
        dis.vec <- .DisVect(x = dat.pts, dist = dis.ras, convexhull = convexhull, 
            binsize = binsize)
        if (verbose) {
            cat(" Done\n")
        }
        if (verbose) {
            cat("Calculating likelihood...")
        }
        results <- .RunSampBias(dists = dis.vec, distmats = dis.mat, 
            plotextra = plotextra, biasdist = biasdist)
        for (i in 1:length(results)) {
            results[[i]]$bias_matrix <- raster(results[[i]]$bias_matrix)
            extent(results[[i]]$bias_matrix) <- extent(dis.ras[[1]])
            results[[i]]$bias_matrix <- mask(results[[i]]$bias_matrix, 
                dis.ras[[1]])
        }
        biastable <- lapply(results, "[", "bias_vector")
        biastable <- lapply(biastable, "data.frame")
        biastable <- lapply(biastable, function(k) t(k))
        biastable <- biastable[!is.na(biastable)]
        biastable <- do.call("rbind.data.frame", biastable)
        if (verbose) {
            cat(" Done\n")
        }
        if (verbose) {
            cat("Preparing output...")
        }
        out <- list(summa = list(total_occ = nrow(x), total_sp = length(unique(x$species)), 
            extent = extent(dat.pts), res = res, binsize = binsize, 
            convexhull = convexhull), occurrences = occ.out, 
            species = spe.out, biasmaps = results, biastable = biastable)
        class(out) <- append("sampbias", class(out))
        if (verbose) {
            cat(" Done\n")
        }
        return(out)
    }
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
