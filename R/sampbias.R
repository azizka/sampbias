

#' Global Airport Coordinates
#' 
#' A gazetteer of major global airport locations.
#' 
#' 
#' @name airports
#' @docType data
#' @format A data frame with 891 observations on the following 4 variables.
#' \describe{ \item{list("code")}{a factor, IATA-3 code.} \item{list("name")}{a
#' character vector, airport names.} \item{list("longitude")}{a numeric
#' vector.} \item{list("latitude")}{a numeric vector.} }
#' @source \url{http://www.naturalearthdata.com/}
#' @keywords datasets
#' @examples
#' 
#' data(airports)
#' str(airports)
#' 
NULL





#' Global Major Populated Places
#' 
#' A gazetteer of global country and province capitals, as well as major cities
#' and smaller cities in less populated areas.
#' 
#' 
#' @name cities
#' @docType data
#' @format A data frame with 7322 observations on the following 4 variables.
#' \describe{ \item{list("nameascii")}{a factor; city name.}
#' \item{list("adm0_a3")}{a factor; ISO-3 code of country name.}
#' \item{list("longitude")}{a numeric vector.} \item{list("latitude")}{a
#' numeric vector.} }
#' @source
#' \url{http://www.naturalearthdata.com/downloads/10m-cultural-vectors/}
#' @keywords datasets
#' @examples
#' 
#' data(cities)
#' str(cities)
#' 
NULL





#' Global Coastlines
#' 
#' A \code{SpatialPolygonsDataFrame} with global coastlines.
#' 
#' 
#' @name landmass
#' @docType data
#' @source
#' \url{http://www.naturalearthdata.com/downloads/10m-physical-vectors/}
#' @keywords datasets
#' @examples
#' 
#' data("landmass")
#' plot(landmass)
#' 
NULL





#' Global Network of Major Roads
#' 
#' A gazetteer of major global roads as \code{SpatialPolygonsDataFrame}.
#' 
#' 
#' @name roads
#' @docType data
#' @source
#' \url{http://www.naturalearthdata.com/downloads/10m-cultural-vectors/}
#' @keywords datasets
#' @examples
#' 
#' data(roads)
#' 
NULL





#' Global Major Waterbodies
#' 
#' A \code{SpatialPolygonsDataFrame} with the centerlines of major global
#' waterbodies.
#' 
#' 
#' @name waterbodies
#' @docType data
#' @source
#' \url{http://www.naturalearthdata.com/downloads/10m-physical-vectors/}
#' @keywords datasets
#' @examples
#' 
#' data(waterbodies)
#' plot(waterbodies)
#' 
NULL



