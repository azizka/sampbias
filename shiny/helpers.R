
PlotOccRast <- function(x, res, gaz = NULL){
    r <- raster(extent(x))
    res(r) <- res
    
    rast <- rasterize(x, r, fun = "count")
    rast <- data.frame(coordinates(rast), as.data.frame(rast))
    colnames(rast) <- c("Longitude", "Latitude", "val")
    
    if(!is.null(gaz)){
    gaz.plo <- lapply(gaz, function(k) raster::crop(x = k, y = extent(x)))
    gaz.plo[[1]] <- data.frame(coordinates(gaz.plo[[1]]))
    gaz.plo[[2]] <- data.frame(coordinates(gaz.plo[[2]]))
    gaz.plo[3:5] <- lapply(gaz.plo[3:5], "fortify")
    }

    out <- ggplot()+
      geom_raster(data=rast, aes(y=Latitude, x=Longitude, fill=val))+
      coord_fixed()+
      theme_bw()+
      scale_fill_viridis(na.value = "transparent", 
                         option = "viridis", 
                         direction = -1, name = "Occurrence\nnumber", discrete = F)+
      
      theme(panel.border = element_rect(fill = NA))
    
    if(!is.null(gaz)){
      out <- out+
        ggplot2::geom_path(mapping = aes_string(x = "long", y = "lat", group = "group"),
                           linetype = 1,
                           data = gaz.plo$landmass, col = "grey40")+
        ggplot2::geom_path(mapping = aes_string(x = "long", y = "lat", group = "group"),
                           linetype = 1, data = gaz.plo$roads, col = "grey40")+
        ggplot2::geom_path(mapping = aes_string(x = "long", y = "lat", group = "group"),
                           linetype = 2.5,
                           data = gaz.plo$rivers, col = "grey40")+
        ggplot2::geom_point(mapping = aes_string(x = "longitude", y = "latitude", colour = "Airports"),
                            shape = 16, size = 2.5, data = gaz.plo$airports, col = "grey10")+
        ggplot2::geom_point(mapping = aes_string(x = "longitude", y = "latitude", colour = "Cities"),
                            shape = 17, size = 1.5, data = gaz.plo$cities, col = "grey10")+
        scale_color_manual(name = "Biases", values = c(Airports = 'black', Cities = 'black'))
        # scale_shape_manual(name = "Biases", values = c('Airports' = 16, 'Cities' = 17))
      # ggplot2::labs(shape = "", color = "Bias Effect", linetype = "")
    }

   return(out)
  
}


PlotSpRast <- function(x, res, wrld){
  r <- raster(extent(SpatialPoints(x[,c("decimallongitude", "decimallatitude")])))
  res(r) <- res
  
  wrld <- crop(wrld, r)
  wrld <- fortify(wrld)
  
  inp <- split(x, f = x$species)
  rast <- lapply(inp, function(x){
    po <- SpatialPoints(x[,c("decimallongitude", "decimallatitude")], CRS("+proj=longlat +datum=WGS84"))
    ras_sub <- rasterize(po, r, fun = "count")
    ras_sub[ras_sub >= 1] <- 1
    ras_sub[is.na(ras_sub)] <- 0
    return(ras_sub)})
  
  rast <- Reduce("+", rast)
  rast[rast == 0] <- NA
  
  #plot
  rast <- data.frame(coordinates(rast), as.data.frame(rast))
  colnames(rast) <- c("Longitude", "Latitude", "val")
  out <- ggplot()+
    geom_raster(data=rast, aes(y=Latitude, x=Longitude, fill=val))+
    coord_fixed()+
    theme_bw()+
    scale_fill_viridis(na.value = "transparent", 
                       option = "viridis", 
                       direction = -1, name = "Species\nnumber", discrete = F)+
    geom_polygon(data=wrld, aes(x=long, y=lat, group = group),
                 lwd = 0.5, col = "grey20", fill = "transparent")+
    theme(panel.border = element_rect(fill = NA))
  return(out)
}
#####################################################################################################
#sampbias functions for likelihood calculation
####################################################################################################

.RunSampBias <- function(dists, distmats, biasdist = c(0, 10), plotextra = F, plotextrafile = "samp_bias_extra_plots.pdf") {
  
  names.dists <- names(dists)
  if(is.null(names.dists)){
    names(dists) <- paste("Unnamed", seq(1, length(dists)), sep = "")
    names.dists <- names(dists)
  }
  
  if (plotextra == T) {
    pdf(file = plotextrafile, width = 5 * 4 * 0.75, height = 5 * 4 * 0.75)
    par(mfrow = c(length(names.dists), 4))
  }
  
  res <- list()
  for (i in 1:length(dists)) {
    res[[i]] <- .CalcExpBias(dists = dists[[i]], Dmatrix = distmats[[i]], names.dists[i],
                             rescale = 0, distvec = biasdist, plotextra = plotextra)
  }
  
  for (i in 1:length(names.dists)) {
    b.matrix <- res[[i]][[3]]
    if (i == 1) {
      average.bias <- b.matrix
    } else {
      average.bias <- average.bias + b.matrix
    }
  }
  
  average.bias.matrix <- average.bias/length(names.dists)
  
  res[[length(dists) + 1]] <- list(bias_matrix = average.bias.matrix)
  names(res) <- c(names.dists, "average_bias_matrix")
  
  
  if (plotextra == T) {
    n <- dev.off()
  }
  return(res)
  
}

# EXPONENTIAL MODEL
# construct probability vector for all poits based on the empirical probaility distribution
.ConstructPxVector <- function(x, X0, nP0, rescale, Px, plotextra) {
  x <- sort(x)
  if (plotextra == T) {
    h.obs <- hist(x, breaks = c(0, X0), main = "Sampled distribution", xlab = "Rescaled distance")
  } else {
    h.obs <- hist(x, breaks = c(0, X0), plot = F)
  }
  obs.counts <- h.obs$counts  # length(obs.counts) = length(P0) = length(X0)
  Px <- c()
  for (i in 1:length(obs.counts)) {
    Px <- c(Px, rep(nP0[i], obs.counts[i]))
  }  # length(Px) = length(x)
  Px[Px == 0] <- min(nP0[nP0 > 0])
  return(Px)
}

.bEXP <- function(d, l = 1) {
  return(dexp(d, rate = l, log = F))
}

.CalcProbSampleEXP <- function(x, X0, nP0, rescale, Px, l) {
  num <- Px * .bEXP(x, l)
  max.dist <- max(X0)
  int.discrete <- X0
  step <- max.dist/length(X0)
  den <- sum((nP0 * .bEXP(int.discrete, l)) * step)
  return(c(sum(log(num) - log(den))))
}

.PlotLikSurfaceEXP <- function(x, X0, nP0, rescale, Px, m = 0.01, M = 5, step = 100,
                               plotextra) {
  # PLOT LIKELIHOOD SURFACE
  plot(nP0 ~ X0, type = "l", ylab = "Probability", xlab = "Rescaled distance",
       main = "Empirical distribution")
  z <- c()
  lambda <- seq(m, M, length.out = step)
  ML <- c(-Inf, 0)
  for (i in 1:length(lambda)) {
    lik <- .CalcProbSampleEXP(x, X0, nP0, rescale, Px, lambda[i])
    z[i] <- lik
  }
  print(c(max(z), 1/lambda[which(z == max(z))]))
  ML.scale <- 1/lambda[which(z == max(z))]
  ML.rate <- lambda[which(z == max(z))]
  plot(z ~ lambda, type = "l", ylab = "Likelihood", main = "Likelihood surface",
       #main=paste("scale:",round(ML.scale,4),"rate:",round(ML.rate,4)),
       xlab = "Rate")
  return(ML.rate)
}


.CalcExpBias <- function(dists, Dmatrix, biasname = "bias", rescale = 0, plotextra = T,
                         distvec) {
  if (rescale == 0) {
    rescale <- 5 * max(dists$X0)
    cat(c("\nrescale factor:", rescale, max(dists$X0), "\n"))
  }
  X0 <- dists$X0/rescale  # binned distances - unit is set to 10,000 Km
  cell_size <- min(X0)
  X0 <- X0 + cell_size
  P0 <- dists$P0  # prob of binned distances # THEY SHOULD SUM UP TO 1 (maybe instead of $density we should take $counts/sum($counts))
  # normalized P0
  nP0 <- P0/sum(P0)
  x <- dists$X/rescale  # samples (observed distances) - unit is set to 10,000 Km
  x <- x[!is.na(x)]
  #x <- sort(x) + cell_size
  #x[x<min(X0)] <- min(X0) # to avoid x values < than min X0
  
  # NOTES: SMALLER RATE (=GREATER SCALE) -> SMALLER BIAS
  Px <- .ConstructPxVector(x = x, X0 = X0, nP0 = nP0, rescale = rescale, Px = Px, plotextra = plotextra)
  if (plotextra == T) {
    ML.rate <- .PlotLikSurfaceEXP(x, X0, nP0, rescale, Px)
  } else {
    ML.rate <- 1
  }
  
  .CalcProbSampleEXPOptim <- function(sc) {
    l <- 1/sc
    num <- Px * .bEXP(x, l)
    max.dist <- max(X0)
    int.discrete <- X0
    step <- max.dist/length(X0)
    den <- sum((nP0 * .bEXP(int.discrete, l)) * step)
    return(-sum(log(num) - log(den)))
  }
  
  OPT <- stats::optim(par = ML.rate, fn = .CalcProbSampleEXPOptim, method = "Brent",
                      lower = 1e-02, upper = 10000)
  ML.rate <- 1/OPT$par
  print(ML.rate)
  bias1 <- stats::dexp(0, rate = ML.rate)
  bias2 <- stats::dexp(median(X0), rate = ML.rate)
  bias3 <- stats::dexp(max(X0), rate = ML.rate)
  # rescaled bias curve (at 0 distance -> 0 bians, hence sampling prob = 1)
  dExp <- stats::dexp(X0, rate = ML.rate)/bias1
  bias.vec <- 1 - dExp
  true.dist <- dists$X0/1000
  if (plotextra == T) {
    graphics::plot(bias.vec ~ true.dist, main = paste("Effect of", biasname), xlab = "Distance (Km)",
                   ylab = "Sampling bias", type = "l", ylim = c(0, 1))
  }
  d1 <- Dmatrix
  bias.matrix <- 1 - stats::dexp(d1/rescale, rate = ML.rate)/bias1
  
  dist.vec.bias <- 1 - stats::dexp(distvec/rescale, rate = ML.rate)/bias1
  names(dist.vec.bias) <- distvec
  
  res <- list(dists$X0, bias.vec, bias.matrix, dist.vec.bias)
  names(res) <- c("distances", "bias", "bias_matrix", "bias_vector")
  
  return(res)
  
}
#####################################################################################################
#sampbias functions not related to likelihood calculations
####################################################################################################
DisRast <- function(gaz, ras, buffer = NULL, ncores = 1) {
  
  # create buffer, if none is supplied
  if (is.null(buffer)) {
    buffer <- res(ras)[1]
  }
  #adapt buffer to resolution, buffer always has to be a multiple of resolution
  decs <- .DecimalPlaces(res)
  if(.DecimalPlaces(buffer) != decs){
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
  if (buffer%%res(ras)[1] == 0) {
    dist.out <- lapply(dist.d, function(k) raster::crop(k, extent(ras)))
  } else {
    warning("buffer is not a multiple of res, rasters resampled. Results will be imprecise. Set buffer to multiple of res.")
    dist.out <- lapply(dist.d, function(k) raster::resample(k, ras))
  }
  
  return(dist.out)
}

#get vector with empirical distribution and observed distribution from occurrence points and distance rasters
.DisVect <- function(x, dist, convexhull = c(F,T), binsize){
  #x = the occurence points as spatial points object
  #dis.ras = the distnace rasters, as 1 in the output of getDistance
  
  #get observed distances
  obs.out <- lapply(dist, function(k) raster::extract(k, x))
  obs.out <- lapply(obs.out, "na.omit")
  obs.out <- lapply(obs.out, "as.numeric")
  
  #get empirical distances
  ##crop by data extent or convex hull depending on the convexhull option
  if(convexhull){
    ch <- grDevices::chull(x@coords)
    coords <- x@coords[c(ch, ch[1]), ]
    poly <- sp::SpatialPolygons(list(sp::Polygons(list(sp::Polygon(coords)),
                                                  ID = "convhl")),
                                proj4string = sp::CRS(sp::proj4string(x)))
    
    emp.out <- lapply(dist, function(k) raster::rasterize(poly, k, mask = T))
  }else{
    e <- extent(x)
    emp.out <- lapply(dist, function(k) raster::crop(k, e))
  }
  
  
  ##check if the number of raster cells for the empirical distribution is high enough (> 10,000) if not, sample randomly
  
  ##get empirical distribution
  if(binsize == "default"){
    emp.out <- lapply(emp.out, function(k){hist(na.omit(values(k)), plot = F)})
  }else{
    emp.out <- lapply(emp.out, function(k){hist(na.omit(values(k)),
                                                breaks = seq(0, max(values(k), na.rm = T) + binsize, by = binsize),
                                                plot = F, right = F)})
  }
  #output data
  out <- list()
  
  for(i in 1:length(dist)){
    out[[i]] <- list(X0 = emp.out[[i]]$mids, P0 = emp.out[[i]]$density, X = obs.out[[i]])
  }
  
  names(out) <- names(dist)
  
  check <- unlist(lapply(emp.out, function(k) length(k$mids)))
  if(any(check) < 100){warning("Low resolution, increase resolution or switch to absolute distance calculation")  }
  
  return(out)
}

#get number of decimal places
.DecimalPlaces <- function(x) {
  if ((x %% 1) != 0) {
    nchar(strsplit(sub('0+$', '', as.character(x)), ".", fixed=TRUE)[[1]][[2]])
  } else {
    return(0)
  }
}


SamplingBias <- function(x, gaz = NULL, res = 1, buffer = NULL, convexhull = F, terrestrial = T,
                         terrestrial.cut, 
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
    wrld <- raster::crop(terrestrial.cut, extent(dum.ras))
    wrld <- raster::rasterize(wrld, dum.ras)
    
    excl <- raster::extract(wrld, x[,c("decimallongitude", "decimallatitude")])
    x <- x[!is.na(excl), c("species", "decimallongitude", "decimallatitude")]
    rownames(x) <- NULL
    x$species <- as.factor(as.character(x$species))
    
    dat.pts <- sp::SpatialPoints(x[, c("decimallongitude", "decimallatitude")])
    
    dum.ras <- raster::raster(round(extent(dat.pts), .DecimalPlaces(res)))
    res(dum.ras) <- res
    
    wrld <- raster::crop(terrestrial.cut, extent(dum.ras))
    wrld <- raster::rasterize(wrld, dum.ras)
    if(verbose){cat(" Done.\n")}
  }
  
  # warning if combination of resolution and extent exceed 1mio gridcells
  if (raster::ncell(dum.ras) > 1e+06) {
    warning("huge raster size")
  }
  
  # occurrence raster
  # if (verbose) {
  #   cat("Creating occurrence raster...")
  # }
  # occ.out <- .OccRast(x = dat.pts, ras = dum.ras)
  # if (verbose) {
  #   cat(" Done\n")
  # }
  # 
  # # species raster
  # if (verbose) {
  #   cat("Creating species raster...")
  # }
  # spe.out <- .SpeRast(x, ras = dum.ras, ncores = ncores)
  # if (verbose) {
  #   cat(" Done\n")
  # }
  # 
  # Distance raster calculation check if gazeteers are provided, replace by
  # standard gazeteers if necessary
  if (verbose) {
    cat("Calculating distance raster...")
  }
  # if (is.null(gaz)) {
  #   warning("gaz not found, using standard gazeteers")
  #   
  #   gaz <- list(airports = sampbias::airports, cities = sampbias::cities,
  #               rivers = sampbias::waterbodies, roads = sampbias::roads)
  # }
  
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
                biasmaps = results, biastable = biastable)
    class(out) <- append("sampbias", class(out))
    if (verbose) {
      cat(" Done\n")
    }
    
    return(out)
  }
}

plot.sampbias <- function(x, gaz = NULL, sealine = T, terrestrial.cut,  ...) {
  
  # prepare gazetteers to be included for plotting
  if (!is.null(gaz)) {
    gaz.plo <- lapply(gaz, function(k) raster::crop(x = k, y = x$summa$extent))
    
    # find which gazetteers are points and which are list
    list.condition <- sapply(gaz.plo, function(x) class(x) == "SpatialPointsDataFrame")
    
    point.gaz <- gaz.plo[list.condition]
    if(length(point.gaz) > 0){
    point.gaz <- lapply(point.gaz, "data.frame")
    
    for (i in 1:length(point.gaz)) {
      point.gaz[[i]] <- data.frame(long = point.gaz[[i]]$longitude, lat = point.gaz[[i]]$latitude,
                                   bias = rep(names(point.gaz)[i], nrow(point.gaz[[i]])))
    }
    
    point.gaz <- do.call("rbind.data.frame", point.gaz)
    }
    
    line.gaz <- gaz.plo[!list.condition]
    if(length(line.gaz) > 0){
    
    line.gaz <- lapply(line.gaz, "fortify")
    
    for (i in 1:length(line.gaz)) {
      line.gaz[[i]] <- data.frame(line.gaz[[i]], bias = rep(names(line.gaz)[i],
                                                            nrow(line.gaz[[i]])))
    }
    line.gaz <- do.call("rbind.data.frame", line.gaz)
    }
  }
  
  # # Occurrence raster
  # rast <- data.frame(sp::coordinates(x$occurrences), as.data.frame(x$occurrences))
  # colnames(rast) <- c("Longitude", "Latitude", "val")
  # 
  # occ.plo <- ggplot2::ggplot()+
  #   ggplot2::geom_raster(data = rast, aes_string(y = "Latitude", x = "Longitude", fill = "val"))+
  #   ggplot2::coord_fixed()+
  #   ggplot2::theme_bw()+
  #   viridis::scale_fill_viridis(na.value = "transparent", option = "viridis",
  #                               direction = -1, discrete = F)+
  #   ggplot2::ggtitle("Occurrences")+
  #   ggplot2::theme(legend.title = element_blank())
  # 
  # # Species raster
  # rast <- data.frame(sp::coordinates(x$species), as.data.frame(x$species))
  # colnames(rast) <- c("Longitude", "Latitude", "val")
  # 
  # spe.plo <- ggplot2::ggplot()+
  #   ggplot2::geom_raster(data = rast, aes_string(y = "Latitude", x = "Longitude", fill = "val"))+
  #   ggplot2::coord_fixed()+
  #   ggplot2::theme_bw()+
  #   viridis::scale_fill_viridis(na.value = "transparent", option = "viridis",
  #                               direction = -1, name = "Species\nnumber", discrete = F)+
  #   ggplot2::ggtitle("Species")+
  #   ggplot2::theme(legend.title = element_blank())
  
  # Bias rasters
  plo.biasras <- lapply(x$biasmaps, function(k) {
    # ra <- raster(k$bias_matrix)
    out <- data.frame(sp::coordinates(k$bias_matrix), as.data.frame(k$bias_matrix))  # use species as all rasters have the same extent, to get the coordinates back
    names(out) <- c("Longitude", "Latitude", "Val")
    return(out)
  })
  
  plo.biasras.out <- list()
  
  for (i in 1:length(plo.biasras)) {
    plo.biasras.out[[i]] <- ggplot2::ggplot()+
      ggplot2::geom_raster(data = plo.biasras[[i]], aes_string(y = "Latitude", x = "Longitude", fill = "Val"))+
      ggplot2::coord_fixed()+
      ggplot2::theme_bw()+
      viridis::scale_fill_viridis(na.value = "transparent",option = "viridis",
                                  direction = 1, name = "Bias effect", discrete = F,
                                  limits = c(0, 1))+
      ggplot2::ggtitle(names(x$biasmaps)[i])
    
  }
  
  out <- plo.biasras.out
  
  if (sealine == T) {
    wrld <- raster::crop(terrestrial.cut, extent(x$summa$extent))
    wrld <- ggplot2::fortify(wrld)
    out <- lapply(out, function(k) {
      k + ggplot2::geom_polygon(data = wrld, aes_string(x = "long", y = "lat",group = "group"),
                                lwd = 0.5, col = "grey40", fill = "transparent")
    })
  }
  
  # add gazetteers to plots
  if (!is.null(gaz)) {
    out <- lapply(out, function(k) {
      if(length(line.gaz) > 0){
      k + ggplot2::geom_path(mapping = aes_string(x = "long", y = "lat",
                                                  linetype = "bias", group = "group"),
                             data = line.gaz, col = "grey40")
      }
      if(length(point.gaz) > 0){
       k + ggplot2::geom_point(mapping = aes_string(x = "long", y = "lat",
                                                 shape = "bias"), data = point.gaz, col = "grey10")
      }
      k +ggplot2::labs(shape = "", color = "Bias Effect", linetype = "")
    })
  }
  
  #print plots
  for (i in 1:length(out)) {
    print(out[[i]])
  }
}


BiasPlot <- function(x, gaz, res, terrestrial.cut){
  
  result <- SamplingBias(x = x, gaz = gaz, res = res, buffer = 1, convexhull = F, 
                      terrestrial = T, binsize = NULL, verbose = F, terrestrial.cut = terrestrial.cut)
  
  plot.sampbias(result, gaz = gaz, sealine = F, terrestrial.cut = terrestrial.cut)
}








