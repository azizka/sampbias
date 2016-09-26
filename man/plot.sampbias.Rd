\name{plot.sampbias}
\alias{plot.sampbias}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Mapping Occurrence and Bias Rasters in an Object of the Class Sampbias
}
\description{
Plotting method for class \code{sampbias}, generating a rasterized plots showing the number of occurrences and the number of species per gridcell, one bias raster indicating the efect strenth for each gazetteer provided to \code{SamplingBias}, and a average bias raster indicating the average bias for each gridcell.
}
\usage{
\method{plot}{sampbias}(x, gaz = NULL, sealine = T, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
an object of the class \code{sampbias}.
}
  \item{gaz}{
a list of SpatialObjects, to be printed on the maps.  Should be the same objects provided to \code{SamplingBias} when creating the Object. If \code{gaz} is not supplied, the sampbias package standard gazetteers are used.
}
  \item{sealine}{
logical. Should the sealine be added to the plots? Default is to TRUE.
}
  \item{...}{
Additional arguments to be passed to \code{\link{plot}}.
}
}

\value{
A series of R plots based on ggplot2.
}

\note{
Check https://github.com/azizka/sampbias/wiki for a tutorial on sampbias.
}

\seealso{
\code{\link{SamplingBias}}, \code{\link{summary.sampbias}}
}
\examples{
#simulate data
occ <- data.frame(species = rep(sample(x = LETTERS, size = 5), times = 10),
                  decimallongitude = runif(n = 50, min = -5, max = 5),
                  decimallatitude = runif(n = 50, min = -4, max = 4))


#create point gazetteer
pts <- data.frame(long = runif(n = 5, min = -5, max = 5),
                  lat = runif(n = 5, min = -4, max = 4),
                  dat = rep("A", 5))
pts <- SpatialPointsDataFrame(coords = pts[,1:2], data = data.frame(pts[,3]))

lin <- data.frame(long = seq(-5, 5, by = 1),
                  lat = rep(2, times = 11))
lin <- SpatialLinesDataFrame(sl = SpatialLines(list(Lines(Line(lin), ID="B1"))),
                             data = data.frame("B", row.names = "B1"))

gaz <- list(lines.strucutre = lin, point.structure = pts)

out <- SamplingBias(x = occ, gaz = gaz, terrestrial = FALSE)
plot(out)
}

\keyword{ methods }

