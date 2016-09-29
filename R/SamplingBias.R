SamplingBias <- function(x, gaz = NULL, res = 1, buffer = NULL, convexhull = F, terrestrial = T,
                         binsize = NULL, biasdist = c(0, 10000), ncores = 1, plotextra = F,
                         plotextrafile = "samp_bias_extra_plots.pdf", verbose = T) {

  #convert x to SpatialPoints
  dat.pts <- sp::SpatialPoints(x[, c("decimallongitude", "decimallatitude")])
  # create dummy raster
  dum.ras <- raster::raster(round(extent(dat.pts), .DecimalPlaces(res)))
  res(dum.ras) <- res

  #exclude occurrences in the sea and recreate dummy raster
  if(terrestrial){
    if(verbose){cat("Adjusting to terrestrial surface...")}
    wrld <- raster::crop(sampbias::landmass, extent(dum.ras))
    wrld <- raster::rasterize(wrld, dum.ras)

    excl <- raster::extract(wrld, x[,c("decimallongitude", "decimallatitude")])
    x <- x[!is.na(excl), c("species", "decimallongitude", "decimallatitude")]
    rownames(x) <- NULL
    x$species <- as.factor(as.character(x$species))

    dat.pts <- sp::SpatialPoints(x[, c("decimallongitude", "decimallatitude")])

    dum.ras <- raster::raster(round(extent(dat.pts) + res, .DecimalPlaces(res)))
    res(dum.ras) <- res

    wrld <- raster::crop(sampbias::landmass, extent(dum.ras))
    wrld <- raster::rasterize(wrld, dum.ras)
    if(verbose){cat(" Done.\n")}
  }

  # warning if combination of resolution and extent exceed 1mio gridcells
  if (raster::ncell(dum.ras) > 1e+06) {
    warning("Huge raster size")
  }

  # occurrence raster
  if (verbose) {
    cat("Creating occurrence raster...")
  }
  occ.out <- .OccRast(x = dat.pts, ras = dum.ras)
  if (verbose) {
    cat(" Done\n")
  }

  # species raster
  if (verbose) {
    cat("Creating species raster...")
  }
  spe.out <- .SpeRast(x, ras = dum.ras, ncores = ncores)
  if (verbose) {
    cat(" Done\n")
  }

  # Distance raster calculation check if gazeteers are provided, replace by
  # standard gazeteers if necessary
  if (verbose) {
    cat("Calculating distance raster...")
  }
  if (is.null(gaz)) {
    warning("'gaz' not found, using standard gazetteers")

    gaz <- list(airports = sampbias::airports, cities = sampbias::cities,
                rivers = sampbias::waterbodies, roads = sampbias::roads)
  }

  # check if gaz are provided as distance rasters, if so, this function will
  # run much faster
  check <- all(sapply(gaz, is.raster))

  if (all(check)) {
    #'use input directly as distance raster
    dis.ras <- gaz
  } else {
    ## create distance raster for all gazeteers
    dis.ras <- DisRast(gaz = gaz, ras = dum.ras, buffer = buffer, ncores = ncores)

    if (terrestrial) {
      dis.ras <- lapply(dis.ras, function(k) mask(k, wrld))
    }

    # check if there are values in the distance raster
    if (all(is.na(values(dis.ras[[1]])))) {
      stop("No valid distances found. Consider setting terrestrial = F")
    }

    dis.mat <- lapply(dis.ras, "as.matrix")
  }
  if (verbose) {
    cat(" Done\n")
  }

  ## Check if a distance was found for any gazeteer, if not, only return
  ## species and occurrence raster
  if (is.logical(dis.ras)) {
    out <- c(Occurences = occ.out, Species = spe.out, results)
    class(out) <- append("sampbias", class(out))

    return(out)
  } else {
    # empirical and observed values in the input format for the likelihood
    # optimization set binsize based on raster resolution, if no binsize is
    # provided, roughly assuming 1 deg = 100 km
    if (is.null(binsize)) {
      binsize <- res * 1e+05
    }
    if (verbose) {
      cat("Extracting values...")
    }
    dis.vec <- .DisVect(x = dat.pts, dist = dis.ras, convexhull = convexhull,
                        binsize = binsize)
    if (verbose) {
      cat(" Done\n")
    }

    # run likelihood calculation
    if (verbose) {
      cat("Calculating likelihood...")
    }
    biasdist2 <- biasdist/1000

    results <- .RunSampBias(dists = dis.vec, distmats = dis.mat, plotextra = plotextra,
                            biasdist = biasdist2, plotextrafile = plotextrafile)

    # crop output rasters to landmass by using the distance rasters
    for (i in 1:length(results)) {
      results[[i]]$bias_matrix <- raster::raster(results[[i]]$bias_matrix)
      extent(results[[i]]$bias_matrix) <- raster::extent(dis.ras[[1]])
      results[[i]]$bias_matrix <- mask(results[[i]]$bias_matrix, dis.ras[[1]])
    }

    # create biastable with the bias per distance
    biastable <- lapply(results, "[", "bias_vector")
    biastable <- lapply(biastable, "data.frame")
    biastable <- lapply(biastable, function(k) t(k))
    biastable <- biastable[!is.na(biastable)]
    biastable <- do.call("rbind.data.frame", biastable)
    names(biastable) <- as.numeric(names(biastable)) * 1000

    if (verbose) {
      cat(" Done\n")
    }

    # create output file, a list of the class sampbias
    if (verbose) {
      cat("Preparing output...")
    }
    out <- list(summa = list(total_occ = nrow(x), total_sp = length(unique(x$species)),
                             extent = extent(dum.ras), res = res, binsize = binsize, convexhull = convexhull),
                occurrences = occ.out, species = spe.out, biasmaps = results, biastable = biastable)
    class(out) <- append("sampbias", class(out))
    if (verbose) {
      cat(" Done\n")
    }

    return(out)
  }
}
