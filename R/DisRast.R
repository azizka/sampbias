#Distance raster for all gazeteers
DisRast <- function(gaz, ras, buffer, ncores){ #get empirical distace distribution from gazetteers, rasterbased

  #intiate cluster if multiple cores are used
  if(ncores > 1){
    if (!requireNamespace("geosphere", quietly = TRUE)) {
      stop("parallel needed for multithreading. Please install the package.",
           call. = FALSE)
    }else{
      cl <- parallel::makeCluster(ncores)
    }
  }

  #create large extent for distance calculations
  e <- raster::extent(ras)
  cut.off <- e + buffer

  #crop gazeteers, to larger extent
  gaz.crop <- lapply(gaz, function(k) raster::crop(k, cut.off))

  # check if something was found for all gazeteers and remove the remainders
  check <- sapply(gaz.crop, is.null)

  if(any(check)){
    gaz.crop <- gaz.crop[check]
    warning(paste("no reference found for ", names(gaz.crop)[!check], " Increase buffer."))
  }

  if(all(check)){
    warning("no references found within study boundaries. Increase buffer")
    warning("falling back to species and occurence raster")
    return(FALSE)
  }

  #check if gazeteers have na values in the first column, which will cause
  check2 <-   lapply(gaz.crop, function(k) sum(is.na(k@data[,1])))
  if(any(check2 >0)){
    warnings("gazeteers have NA values in first column, the corresponding entries will eb ignored")
  }

  #create dummy raster
  r <- raster(cut.off)
  res(r) <- raster::res(ras)
  r[] <- 0

  ##rasterize gazeteers, parallelized if desired
  if(ncores == 1){
    dist.r <- lapply(gaz.crop, function(k) raster::rasterize(x = k, y = r, field = 1, fun = "count"))
    #calculate distance for all gazeteers
    dist.d <- lapply(dist.r, function(k) raster::distance(k))
  }else{
    parallel::clusterExport(cl, "gaz.crop")
    parallel::clusterExport(cl, "r")
    parallel::clusterEvalQ(cl, library(raster))

    dist.r <- parallel::parLapply(cl,
                                  gaz.crop,
                                  function(k) raster::rasterize(x = k, y = r, field = 1, fun = "count"))

    #calculate distance for all gazeteers
    dist.d <- parallel::parLapply(cl,
                                  dist.r,
                                  function(k) raster::distance(k))
    parallel::stopCluster(cl)
  }

  ##crop resulting distance raster to study area
  dist.out <- lapply(dist.d, function(k) raster::crop(k, extent(ras)))

  return(dist.out)
}
