\name{airports}
\alias{airports}
\docType{data}
\title{
Global Airport Coordinates
}
\description{
A gazetteer of major global airport locations. 
}
\usage{data("airports")}
\format{
  A data frame with 891 observations on the following 4 variables.
  \describe{
    \item{\code{code}}{a factor, IATA-3 code.}
    \item{\code{name}}{a character vector, airport names.}
    \item{\code{longitude}}{a numeric vector.}
    \item{\code{latitude}}{a numeric vector.}
  }
}
\source{
\url{http://www.naturalearthdata.com/}
}

\examples{
data(airports)
str(airports)
}
\keyword{datasets}
