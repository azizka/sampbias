\name{cities}
\alias{cities}
\docType{data}
\title{
Global Major Populated Places
}
\description{
A gazetteer of global country and province capitals, as well as major cities and smaller cities in less populated areas.
}
\usage{data("cities")}
\format{
  A data frame with 7322 observations on the following 4 variables.
  \describe{
    \item{\code{nameascii}}{a factor; city name.}
    \item{\code{adm0_a3}}{a factor; ISO-3 code of country name.}
    \item{\code{longitude}}{a numeric vector.}
    \item{\code{latitude}}{a numeric vector.}
  }
}
\source{
\url{http://www.naturalearthdata.com/downloads/10m-cultural-vectors/}
}

\examples{
data(cities)
str(cities)
}
\keyword{datasets}
