# Distance raster for all gazeteers get empirical distace distribution from
# gazetteers, rasterbased


#' Distance Rasters from a List of Geographic Gazetteers
#'
#' Creates a list of distances rasters based on a list of geographic
#' gazetteers, as SpatialPoints or SpatialLines objects, and a template raster,
#' indicating the desired extent and resolution
#'
#'
#' @param gaz an object of the class \code{list}, including one or more
#' geographic gazetteers of the class \code{SpatialPointsDataFrame} or
#' \code{SpatialLinesDataFrame}.
#' @param ras an object of the class \code{raster}. Defining the extent and
#' resolution of the distances rasters.
#' @param buffer numerical.  The size of the geographic buffer around the
#' extent of ras for the distance calculations in degrees, to account for
#' geographic structures neighbouring the study area (such as a road right
#' outside the study area) Default is to the resolution of \code{ras}.
#' @param ncores numerical.  The number of cores used for parallel computing.
#' Must be lower than the available number of cores. Not finally implemented in
#' version 0.1.0.
#' @return a \code{list} of \code{raster} objects of the same length as
#' \code{gaz}. The values in each raster correspond to the planar geographic
#' distance to the next feature in \code{gaz}, given the resolution of
#' \code{ras}
#' @note Check https://github.com/azizka/sampbias/wiki for a tutorial on
#' sampbias.
#' @seealso \code{\link{SamplingBias}}
#' @keywords spatial
#' @examples
#'
#' #create raster for resolution and extent
#' ras <- raster(extent(-5,5,-4,4), res = 1)
#'
#' #create point gazeteer
#' pts <- data.frame(long = runif(n = 5, min = -5, max = 5),
#'                   lat = runif(n = 5, min = -4, max = 4),
#'                   dat = rep("A", 5))
#'
#' pts <- SpatialPointsDataFrame(coords = pts[,1:2], data = data.frame(pts[,3]))
#'
#' lin <- data.frame(long = seq(-5, 5, by = 1),
#'                   lat = rep(2, times = 11))
#' lin <- SpatialLinesDataFrame(sl = SpatialLines(list(Lines(Line(lin), ID="B1"))),
#'                              data = data.frame("B", row.names = "B1"))
#'
#' gaz <- list(point.structure = pts, lines.strucutre = lin)
#'
#' out <- dis_rast(gaz, ras)
#'
#' \dontrun{plot(out[[1]])}
#'
#'@export
#'@importFrom raster crop extent raster res
#'
dis_rast <- function(gaz, ras, buffer = NULL, ncores = 1) {

  # create buffer, if none is supplied
  if (is.null(buffer)) {
    buffer <- res(ras)[1]
  }
  #adapt buffer to resolution, buffer always has to be a multiple of resolution
  decs <- .DecimalPlaces(raster::res(ras)[1])
  if(.DecimalPlaces(buffer) > decs){
    buffer <- round(buffer, decs)
    warning(sprintf("Adapting buffer precision to resolution. Buffer set to %s", buffer))
  }

  #make buffer even in case it is odd
  if((buffer * 10^decs) %%2 != 0  ){
    buffer  <- buffer + 1/10^decs
    warning(sprintf("Evening buffer. Buffer set to %s", buffer))
  }

  # intiate cluster if multiple cores are used
  if (ncores > 1) {
    if (!requireNamespace("parallel", quietly = TRUE)) {
      stop("parallel needed for multithreading. Please install the package.",
           call. = FALSE)
    } else {
      cl <- parallel::makeCluster(ncores)
    }
  }

  # create large extent for distance calculations
  e <- raster::extent(ras)
  cut.off <- e + buffer

  # crop gazeteers, to larger extent
  gaz.crop <- lapply(gaz, function(k) raster::crop(k, cut.off))

  # check if something was found for all gazeteers and remove the remainders
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
  check2 <- lapply(gaz.crop, function(k) sum(is.na(k@data[, 1])))
  if (any(check2 > 0)) {
    warning("Gazetteers have NA values in first column, the corresponding entries will be ignored")
  }

  # create dummy raster
  r <- raster(cut.off)
  res(r) <- raster::res(ras)
  r[] <- 0

  ## rasterize gazeteers, parallelized if desired
  if (ncores == 1) {
    dist.r <- lapply(gaz.crop, function(k) raster::rasterize(x = k, y = r,
                                                             field = 1, fun = "count"))
    # calculate distance for all gazeteers
    dist.d <- lapply(dist.r, function(k) suppressWarnings(raster::distance(k)))
  } else {
    parallel::clusterExport(cl, "gaz.crop")
    parallel::clusterExport(cl, "r")
    parallel::clusterEvalQ(cl, library(raster))

    dist.r <- parallel::parLapply(cl, gaz.crop, function(k) raster::rasterize(x = k,
                                                                              y = r, field = 1, fun = "count"))

    # calculate distance for all gazeteers
    dist.d <- parallel::parLapply(cl, dist.r, function(k) suppressWarnings(raster::distance(k)))
    parallel::stopCluster(cl)
  }

  ## crop resulting distance raster to study area
    if (buffer%%as.numeric(as.character(res(ras)[1])) == 0) {
      dist.out <- lapply(dist.d, function(k) raster::crop(k, extent(ras)))
    } else {
      stop("'Buffer' is not a multiple of res. Set 'buffer' to a multiple of res")
    }

  return(dist.out)
}
