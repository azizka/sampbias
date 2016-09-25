SamplingBias <- function(x, gaz = NULL, res = 1, buffer = 1, convexhull = F, binsize = NULL,
                         biasdist = c(0,10), terrestrial = T, ncores = 1, plotextra = F,
                         plotextrafile = "samp_bias_extra_plots.pdf", verbose = T){
  # x = a table with occurrence as downloaded from gbif, but possibly also a data.frame with only three columns: species, decimallongitude, decimallatitude
  #res = raster resolution in degree,
  #gaz = a list of gazeteers, as spatial points or spatial Polygons
  #buff = the buffer around the occurrence point extent for the distance calculation in degree
  #convexhull = logical, shall the empirical distribution be calculated based on a convex hull, or alternatively from the extent of the datapoints
  #binsize = size of the histogramm bins for the empirical distirbution in meters. Should not be smaller than res!
  #biasdist = vector indicating the distances for which bias values are tracked
  #ncores = the number of cores to parallelize the distance calculation using the parallel package
  #plotextra = logical.  Indicating if the statisitcal distributions and likelihood surfcae should be plotted in an exatra file called "samp_bias_extra_plots.pdf"
  #verbose = if true, reports the progress to std.out
  #terrestrial = cut all rasters, also for empirical distribution to land area, using the landmass gazetteer?
  #plotextrafile = file path and name for the extraplots

  #helpers that will be used throughout the function
  ##create dummy raster
  dat.pts <- sp::SpatialPoints(x[,c("decimallongitude", "decimallatitude")])
  dum.ras <- raster::raster(extent(dat.pts))
  res(dum.ras) <- res

  #warning if combination of resolution and extent exceed 1mio gridcells
  if(raster::ncell(dum.ras) > 1000000){
    warning("huge raster size")
  }

  #occurrence raster
  if(verbose){cat("Creating occurrence raster...")}
  occ.out <- .OccRast(x = dat.pts, ras = dum.ras)
  if(verbose){cat(" Done\n")}

  #species raster
  if(verbose){cat("Creating species raster...")}
  spe.out <- .SpeRast(x, ras = dum.ras, ncores = ncores)
  if(verbose){cat(" Done\n")}

  #Distance raster calculation
  ##check if gazeteers are provided, replace by standard gazeteers if necessary
  if(verbose){cat("Calculating distance raster...")}
  if(is.null(gaz)){
    warnings("gaz not found, using standard gazeteers")

    gaz <- list(airports = sampbias::airports,
                cities = sampbias::cities,
                rivers = sampbias::waterbodies,
                roads = sampbias::roads)
  }


  ##check if gaz are provided as distance rasters, if so, this functoin will run much faster
  check <- all(sapply(gaz, is.raster))

  if(all(check)){
    #"use input directly as distance raster
    dis.ras <- gaz
  }else{
    ##create distance raster for all gazeteers
    dis.ras <- DisRast(gaz = gaz, ras = dum.ras, buffer = buffer, ncores = ncores)

    if(terrestrial){
      wrld <- raster::crop(sampbias::landmass, extent(dum.ras))
      wrld <- raster::rasterize(wrld, dum.ras)
      dis.ras <- lapply(dis.ras, function(k) mask(k, wrld))
    }

    dis.mat <- lapply(dis.ras, "as.matrix")
  }
  if(verbose){cat(" Done\n")}

  ## Check if a distance was found for any gazeteer, if not, only return species and occurrence raster
  if(is.logical(dis.ras)){
    out <- c(Occurences = occ.out,
             Species = spe.out,
             results)
    class(out) <- append("sampbias", class(out))

    return(out)
  }else{
    #empirical and observed values in the input format for the likelihood optimization
    ##set binsize based on raster resolution, if no binsize is provided, roughly assuming 1 deg = 100 km
    if(is.null(binsize)){
      binsize = res * 100000 * 1.1
    }
    if(verbose){cat("Extracting values...")}
    dis.vec <- .DisVect(x = dat.pts, dist = dis.ras, convexhull = convexhull, binsize = binsize)
    if(verbose){cat(" Done\n")}

    #run Danieles shit
    if(verbose){cat("Calculating likelihood...")}

    results <- .RunSampBias(dists = dis.vec, distmats = dis.mat, plotextra = plotextra, biasdist = biasdist, plotextrafile = plotextrafile)

    #crop output rasters to landmass by using the distance rasters
    for(i in 1:length(results)){
      results[[i]]$bias_matrix <- raster::raster(results[[i]]$bias_matrix)
      extent(results[[i]]$bias_matrix) <- raster::extent(dis.ras[[1]])
      results[[i]]$bias_matrix <- mask(results[[i]]$bias_matrix, dis.ras[[1]])
    }

    #create biastable with the bias per distance
    biastable <- lapply(results, "[", "bias_vector")
    biastable <- lapply(biastable, "data.frame")
    biastable <- lapply(biastable,function(k) t(k))
    biastable <- biastable[!is.na(biastable)]
    biastable <- do.call("rbind.data.frame", biastable)

    if(verbose){cat(" Done\n")}


    #create output file, a list of the class sampbias


    if(verbose){cat("Preparing output...")}
    out <- list(summa = list(total_occ = nrow(x),
                          total_sp = length(unique(x$species)),
                          extent = extent(dat.pts),
                          res = res,
                          binsize = binsize,
                          convexhull = convexhull),
             occurrences = occ.out,
             species = spe.out,
             biasmaps = results,
             biastable = biastable)
    class(out) <- append("sampbias", class(out))
    if(verbose){cat(" Done\n")}


    return(out)
  }
}
