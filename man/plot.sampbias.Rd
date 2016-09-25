\name{plot.sampbias}
\alias{plot.sampbias}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Plot Method for Class sampbias
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
plot.sampbias(x, gaz = NULL, sealine = T, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
%%     ~~Describe \code{x} here~~
}
  \item{gaz}{
%%     ~~Describe \code{gaz} here~~
}
  \item{sealine}{
%%     ~~Describe \code{sealine} here~~
}
  \item{...}{
%%     ~~Describe \code{sealine} here~~
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
function (x, gaz = NULL, sealine = T) 
{
    if (!is.null(gaz)) {
        gaz.plo <- lapply(gaz, function(k) raster::crop(x = k, 
            y = x$occurrences))
        list.condition <- sapply(gaz.plo, function(x) class(x) == 
            "SpatialPointsDataFrame")
        point.gaz <- gaz.plo[list.condition]
        point.gaz <- lapply(point.gaz, "data.frame")
        for (i in 1:length(point.gaz)) {
            point.gaz[[i]] <- data.frame(long = point.gaz[[i]]$longitude, 
                lat = point.gaz[[i]]$latitude, bias = rep(names(point.gaz)[i], 
                  nrow(point.gaz[[i]])))
        }
        point.gaz <- do.call("rbind.data.frame", point.gaz)
        line.gaz <- gaz.plo[!list.condition]
        line.gaz <- lapply(line.gaz, "fortify")
        for (i in 1:length(line.gaz)) {
            line.gaz[[i]] <- data.frame(line.gaz[[i]], bias = rep(names(line.gaz)[i], 
                nrow(line.gaz[[i]])))
        }
        line.gaz <- do.call("rbind.data.frame", line.gaz)
    }
    rast <- data.frame(coordinates(x$occurrences), as.data.frame(x$occurrences))
    colnames(rast) <- c("Longitude", "Latitude", "val")
    occ.plo <- ggplot() + geom_raster(data = rast, aes(y = Latitude, 
        x = Longitude, fill = val)) + coord_fixed() + theme_bw() + 
        scale_fill_viridis(na.value = "transparent", option = "viridis", 
            direction = -1, discrete = F) + ggtitle("Occurrences")
    theme(legend.title = element_blank())
    rast <- data.frame(coordinates(x$species), as.data.frame(x$species))
    colnames(rast) <- c("Longitude", "Latitude", "val")
    spe.plo <- ggplot() + geom_raster(data = rast, aes(y = Latitude, 
        x = Longitude, fill = val)) + coord_fixed() + theme_bw() + 
        scale_fill_viridis(na.value = "transparent", option = "viridis", 
            direction = -1, name = "Species\nnumber", discrete = F) + 
        ggtitle("Species")
    theme(legend.title = element_blank())
    plo.biasras <- lapply(x$biasmaps, function(k) {
        out <- data.frame(coordinates(k$bias_matrix), as.data.frame(k$bias_matrix))
        names(out) <- c("Longitude", "Latitude", "Val")
        return(out)
    })
    plo.biasras.out <- list()
    for (i in 1:length(plo.biasras)) {
        plo.biasras.out[[i]] <- ggplot() + geom_raster(data = plo.biasras[[i]], 
            aes(y = Latitude, x = Longitude, fill = Val)) + coord_fixed() + 
            theme_bw() + scale_fill_viridis(na.value = "transparent", 
            option = "viridis", direction = 1, name = "Bias effect", 
            discrete = F, limits = c(0, 1)) + ggtitle(names(x$biasmaps)[i])
    }
    out <- c(list(occ.plo, spe.plo), plo.biasras.out)
    if (sealine == T) {
        wrld <- crop(SampBias::landmass, extent(x$occurrences))
        wrld <- fortify(wrld)
        out <- lapply(out, function(k) {
            k + geom_polygon(data = wrld, aes(x = long, y = lat, 
                group = group), lwd = 0.5, col = "grey40", fill = "transparent")
        })
    }
    if (!is.null(gaz)) {
        out <- lapply(out, function(k) {
            k + geom_path(mapping = aes(x = long, y = lat, linetype = bias, 
                group = group), data = line.gaz, col = "grey40") + 
                geom_point(mapping = aes(x = long, y = lat, shape = bias), 
                  data = point.gaz, col = "grey10")
        })
    }
    for (i in 1:length(out)) {
        print(out[[i]])
    }
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
