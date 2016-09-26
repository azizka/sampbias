# Distance raster for all gazeteers get empirical distace distribution from
# gazetteers, rasterbased
DisRast <- function(gaz, ras, buffer = NULL, ncores = 1) {

  # create buffer, if none is supplied
  if (is.null(buffer)) {
    buffer <- res(ras)[1] * 2
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
    warning(paste("no reference found for ", names(gaz.crop)[!check], " Increase buffer."))
  }

  if (all(check)) {
    warning("no references found within study boundaries. Increase buffer")
    warning("falling back to species and occurence raster")
    return(FALSE)
  }

  # check if gazeteers have na values in the first column, which will cause
  check2 <- lapply(gaz.crop, function(k) sum(is.na(k@data[, 1])))
  if (any(check2 > 0)) {
    warnings("gazeteers have NA values in first column, the corresponding entries will be ignored")
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
    dist.d <- lapply(dist.r, function(k) raster::distance(k))
  } else {
    parallel::clusterExport(cl, "gaz.crop")
    parallel::clusterExport(cl, "r")
    parallel::clusterEvalQ(cl, library(raster))

    dist.r <- parallel::parLapply(cl, gaz.crop, function(k) raster::rasterize(x = k,
                                                                              y = r, field = 1, fun = "count"))

    # calculate distance for all gazeteers
    dist.d <- parallel::parLapply(cl, dist.r, function(k) raster::distance(k))
    parallel::stopCluster(cl)
  }

  ## crop resulting distance raster to study area
  if (buffer%%res(ras)[1] == 0) {
    dist.out <- lapply(dist.d, function(k) raster::crop(k, extent(ras)))
  } else {
    warning("buffer is not a multiple of res, rasters resampled. Results will be imprecise. Set buffer to multiple of res.")
    dist.out <- lapply(dist.d, function(k) raster::resample(k, ras))
  }

  return(dist.out)
}
