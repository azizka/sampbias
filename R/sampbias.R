
#' Example Dataset for a Custom Study Area
#'
#' An example of the format needed to provide custom areas for \code{\link{calculate_bias}}
#'
#' @name area_example
#' @docType data
#' @keywords datasets
#' @examples
#'
#' data(area_example)
#'
"area_example"

#' Detailed Example for a Custom Study Area
#'
#' An example of the format needed to provide custom areas for \code{\link{calculate_bias}}
#' based on a publicly available set of global ecoregions.
#'
#' @name ecoregion_example
#' @docType data
#' @source
#' \url{https://www.worldwildlife.org/publications/terrestrial-ecoregions-of-the-world}
#' @keywords datasets
#' @examples
#'
#' data(ecoregion_example)
#'
"ecoregion_example"

#' Equal Area Raster
#'
#' An example for an global equal area raster (in Behrmann projection) for the
#' format needed for a custom grid provided to \code{\link{calculate_bias}}.
#'
#' @name ea_raster
#' @docType data
#' @keywords datasets
#' @examples
#'
#' data(ea_raster)
#' ea_raster <- terra::unwrap(ea_raster)
#' @importFrom terra unwrap
"ea_raster"

#' Borneo
#'
#' The outline of Borneo, as example data for the user-defined study area option
#' of \code{\link{calculate_bias}}. From \url{https://www.naturalearthdata.com}.
#'
#' @name borneo
#' @docType data
#' @keywords datasets
#' @examples
#'
#' data(borneo)
#'
"borneo"

