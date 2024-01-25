# Distance raster for all gazeteers get empirical distance distribution from
# gazetteers, raster based

#' Distance Rasters from a List of Geographic Gazetteers
#'
#' Creates a list of distances rasters based on a list of geographic
#' gazetteers, as SpatVector objects, and a template SpatRaster,
#' indicating the desired extent and resolution.
#'
#'
#' @param gaz an object of the class \code{list}, including one or more
#' geographic gazetteers of the class \code{SpatVector}.
#' @param ras an object of the class \code{SpatRaster}. Defining the extent and
#' resolution of the distances rasters.
#' @param buffer numerical.  The size of the geographic buffer around the
#' extent of \code{ras} for the distance calculations in degrees, to account for
#' geographic structures neighbouring the study area (such as a road right
#' outside the study area) Default is to the resolution of \code{ras}.
#' @return a \code{list} of \code{SpatRaster} objects of the same length as
#' \code{gaz}. The values in each raster correspond to the planar geographic
#' distance to the next feature in \code{gaz}, given the resolution of
#' \code{ras}
#' @note Check https://github.com/azizka/sampbias/wiki for a tutorial on
#' sampbias.
#' @seealso \code{\link{calculate_bias}}
#' @keywords spatial
#' @examples
#'
#' #create raster for resolution and extent
#' ras <- terra::rast(terra::ext(-5,5,-4,4), res = 1)
#'
#' #create point gazeteer
#' pts <- data.frame(long = runif(n = 5, min = -5, max = 5),
#'                   lat = runif(n = 5, min = -4, max = 4),
#'                   dat = rep("A", 5))
#'
#' pts <- terra::vect(pts, geom = c("long", "lat"))
#'
#' lin <- as.matrix(data.frame(long = seq(-5, 5, by = 1),
#'                   lat = rep(2, times = 11)))
#' lin <- terra::vect(lin, type = "line")
#'
#' gaz <- list(point.structure = pts, lines.strucutre = lin)
#'
#' out <- dis_rast(gaz, ras)
#'
#' \dontrun{plot(out[[1]])}
#'
#'@export
#'@importFrom terra res ext rast crop vect rasterize distance ext<-
#'
dis_rast <- function(gaz, ras, buffer = NULL) {

  # create buffer, if none is supplied
  if (is.null(buffer)) {
    buffer <- terra::res(ras)[1] * 10
  }

  # if x and y resolution are different, set buffer to 0 and return warning
  # In a later version this should also be enabled for resolutions with uneven

  if (terra::res(ras)[1] != terra::res(ras)[2]) {
    buffer <- 0
    warning("Buffer not supported for CRS with unequal resolution. Setting buffer to 0. Mind edge effects!")
  }

  #adapt buffer to resolution, buffer always has to be a multiple of resolution
  decs <- .DecimalPlaces(terra::res(ras)[1])
  if (.DecimalPlaces(buffer) > decs) {
    buffer <- round(buffer, decs)
    warning(sprintf("Adapting buffer precision to resolution. Buffer set to %s", buffer))
  }

  #make buffer even in case it is odd, by setting to the closes even multiple of the resolution
  if ((buffer * 10 ^ decs) %% 2 != 0  ) {

    ref <- seq(0, terra::res(ras)[1] * 100, by = terra::res(ras)[1])
    ref <- ref[which((ref * 10 ^ decs) %% 2 == 0 )]

    buffer <- ref[which.min(abs(ref - buffer))]

    # buffer  <- buffer + 1/10^decs
    warning(sprintf("Evening buffer. Buffer set to %s", buffer))
  }

  # create large extent for distance calculations
  e <- terra::ext(ras)
  cut.off <- e + buffer

  # crop gazetteers, to larger extent
  gaz.crop <- lapply(gaz, function(k) terra::crop(k, cut.off))

  # check if something was found for all gazetteers and remove the remainders
  check <- sapply(gaz.crop, is.null)

  if (any(check)) {
    gaz.crop <- gaz.crop[check]
    warning(paste("No reference found for ", names(gaz.crop)[!check], " Increase 'buffer'."))
  }

  if (all(check)) {
    warning("No references found within study boundaries. Increase buffer. Falling back to species and occurrence raster")
    return(FALSE)
  }

  # check if gazeteers have na values in the first column, which will cause
  check2 <- lapply(gaz.crop, function(k) sum(is.na(k[, 1])))
  if (any(check2 > 0)) {
    warning("Gazetteers have NA values in first column, the corresponding entries will be ignored")
  }

  # create dummy raster
  r <- terra::rast(cut.off)
  terra::res(r) <- terra::res(ras)
  r[] <- 0

  # Transform sf to SpatVect
  for (i in seq_along(gaz.crop)) {
    if (inherits(gaz.crop[[i]], "sf")) {
      gaz.crop[[i]] <- terra::vect(gaz.crop[[i]])
    }
  }
  ## rasterize gazetteers
  dist.r <-
    suppressWarnings(lapply(gaz.crop, function(k)
      terra::rasterize(
        x = k,
        y = r,
        field = 1,
        fun = "count"
      )))
  for (i in seq_along(dist.r)) {
    if (all(is.na(terra::values(dist.r[[i]])))) {
      dist.r[[i]] <- terra::rasterize(
        x = gaz.crop[[i]],
        y = r,
        field = 1,
        touches = TRUE
      )
    }
  }
  
  # calculate distance for all gazeteers
  dist.d <- lapply(dist.r, function(k) suppressWarnings(terra::distance(k)))

  ## crop resulting distance raster to study area
  temp <- buffer / as.numeric(as.character(terra::res(ras)[1]))
  bool <- .DecimalPlaces(temp) == 0
    if (bool) {
      dist.out <- lapply(dist.d, function(k) terra::crop(k, terra::ext(ras)))
    } else {
      ref <- seq(0, res(ras)[1] * 100, by = res(ras)[1])
      ref <- ref[which((ref * 10 ^ decs) %% 2 == 0 )]

      buffer <- ref[which.min(abs(ref - buffer))]

      warning(sprintf("'Buffer' was not a multiple of res. Set to %s", buffer))

      dist.out <- lapply(dist.d, function(k) terra::crop(k, terra::ext(ras)))
    }

  return(dist.out)
}
