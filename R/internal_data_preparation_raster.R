
#'@importFrom stats na.omit
#'@importFrom graphics hist
#'@importFrom sp CRS Polygons SpatialPoints
#'@importFrom raster extract rasterize


#Occurrence raster
.OccRast <- function(x, ras){
  rast <- raster::rasterize(x, ras, fun = "count")
  return(rast)
}

#Species Number raster
.SpeRast <- function(x, ras, ncores){
  inp <- split(x, f = x$species)

  if(ncores > 1){
    if (!requireNamespace("parallel", quietly = TRUE)) {
      stop("parallel needed for multithreading. Please install the package.",
           call. = FALSE)
    }else{
      cl <- parallel::makeCluster(ncores)
    }
  }

  if(ncores == 1){
    rast <- lapply(inp, function(k){
      po <- sp::SpatialPoints(k[,c("decimallongitude", "decimallatitude")], CRS("+proj=longlat +datum=WGS84"))
      ras_sub <- raster::rasterize(po, ras, fun = "count")
      ras_sub[ras_sub >= 1] <- 1
      ras_sub[is.na(ras_sub)] <- 0
      return(ras_sub)})
  }else{
    parallel::clusterExport(cl, "inp")
    parallel::clusterExport(cl, "ras")
    parallel::clusterEvalQ(cl, library(raster))
    parallel::clusterEvalQ(cl, library(sp))

    rast <- parallel::parLapply(cl,
                                  inp,
                                  function(k){
                                    po <- sp::SpatialPoints(k[,c("decimallongitude", "decimallatitude")],
                                                        CRS("+proj=longlat +datum=WGS84"))
                                    ras_sub <- raster::rasterize(po, ras, fun = "count")
                                    ras_sub[ras_sub >= 1] <- 1
                                    ras_sub[is.na(ras_sub)] <- 0
                                    return(ras_sub)})
    parallel::stopCluster(cl)
  }

  rast <- Reduce("+", rast)
  rast[rast == 0] <- NA

  return(rast)
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
    e <- extent(dist[[1]])
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
    out[[i]] <- list(X0 = emp.out[[i]]$mids, X0counts = emp.out[[i]]$counts, P0 = emp.out[[i]]$density, X = obs.out[[i]])
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

