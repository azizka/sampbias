\name{is.sampbias}
\alias{is.sampbias}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Is Method for Class sampbias
}
\description{
Check class of \code{sampbias} objects.
}
\usage{
\method{is}{sampbias}(object, class2)
}

\arguments{
  \item{object}{
an object of the class \code{sampbias}
}
  \item{class2}{
the names of the class to which is relations are to be examined defined, or (more efficiently) the class definition objects for the classes.
}
}
\details{
With two arguments, tests whether object can be treated as from class2.  With one argument, returns all the super-classes of this object's class.}

\seealso{
Check https://github.com/azizka/sampbias/wiki for a tutorial on sampbias.
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
is(out)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ methods }
