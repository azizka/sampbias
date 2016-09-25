plot.sampbias <- function(x, gaz = NULL, sealine = T, ...){
#maps = switch the spatial visualization of sampling bias on and off
  #gaz = the gazetteers used to evaluate the sampling bias

  #prepare gazetteers to be included for plotting
  if(!is.null(gaz)){
    gaz.plo <- lapply(gaz, function(k) raster::crop(x = k, y = x$occurrences))

    #find which gazetteers are points and which are list
    list.condition <- sapply(gaz.plo, function(x) class(x)=="SpatialPointsDataFrame")

    point.gaz  <- gaz.plo[list.condition]
    point.gaz <- lapply(point.gaz, "data.frame")

    for(i in 1:length(point.gaz)){
      point.gaz[[i]] <- data.frame(long= point.gaz[[i]]$longitude,
                                   lat = point.gaz[[i]]$latitude,
                                   bias = rep(names(point.gaz)[i], nrow(point.gaz[[i]])))
    }


    point.gaz <- do.call("rbind.data.frame", point.gaz)

    line.gaz  <- gaz.plo[!list.condition]
    line.gaz <- lapply(line.gaz, "fortify")

    for(i in 1:length(line.gaz)){
      line.gaz[[i]] <- data.frame(line.gaz[[i]],
                                   bias = rep(names(line.gaz)[i], nrow(line.gaz[[i]])))
    }
    line.gaz <- do.call("rbind.data.frame", line.gaz)
  }

  #Occurrence raster
  rast <- data.frame(sp::coordinates(x$occurrences), as.data.frame(x$occurrences))
  colnames(rast) <- c("Longitude", "Latitude", "val")

  occ.plo <- ggplot2::ggplot()+
    ggplot2::geom_raster(data=rast, aes_string(y="Latitude", x="Longitude", fill="val"))+
    ggplot2::coord_fixed()+
    ggplot2::theme_bw()+
    viridis::scale_fill_viridis(na.value = "transparent",
                       option = "viridis",
                       direction = -1, discrete = F)+
    ggplot2::ggtitle("Occurrences")
  ggplot2::theme(legend.title = element_blank())

  #Species raster
  rast <- data.frame(sp::coordinates(x$species), as.data.frame(x$species))
  colnames(rast) <- c("Longitude", "Latitude", "val")

  spe.plo <- ggplot2::ggplot()+
    ggplot2::geom_raster(data=rast, aes_string(y="Latitude", x="Longitude", fill="val"))+
    ggplot2::coord_fixed()+
    ggplot2::theme_bw()+
    viridis::scale_fill_viridis(na.value = "transparent",
                       option = "viridis",
                       direction = -1, name = "Species\nnumber", discrete = F)+
    ggplot2::ggtitle("Species")
  ggplot2::theme(legend.title = element_blank())



    #Bias rasters

  plo.biasras <- lapply(x$biasmaps, function(k){
    # ra <- raster(k$bias_matrix)
    out <- data.frame(sp::coordinates(k$bias_matrix), as.data.frame(k$bias_matrix))# use species as all rasters have the same extent, to get the coordinates back
    names(out) <- c("Longitude", "Latitude", "Val")
    return(out)
  })

  plo.biasras.out <- list()

  for(i in 1:length(plo.biasras)){
    plo.biasras.out[[i]] <- ggplot()+
      ggplot2::geom_raster(data=plo.biasras[[i]], aes_string(y="Latitude", x="Longitude", fill="Val"))+
      ggplot2::coord_fixed()+
      ggplot2::theme_bw()+
      viridis::scale_fill_viridis(na.value = "transparent",
                         option = "viridis",
                         direction = 1, name = "Bias effect", discrete = F,
                         limits = c(0,1))+
      ggplot2::ggtitle(names(x$biasmaps)[i])

  }

  out <- c(list(occ.plo, spe.plo), plo.biasras.out)

  if(sealine == T){
    wrld <- raster::crop(SampBias::landmass, extent(x$occurrences))
    wrld <- ggplot2::fortify(wrld)
    out <- lapply(out, function(k){
      k + ggplot2::geom_polygon(data=wrld, aes_string(x="long", y="lat", group = "group"),
                       lwd = 0.5, col = "grey40", fill = "transparent")
    })
  }

  #add gazetteers to plots
  if(!is.null(gaz)){
    out <- lapply(out, function(k) {
      k+
        ggplot2::geom_path(mapping = aes_string(x = "long", y = "lat", linetype = "bias", group = "group"), data = line.gaz,  col = "grey40")+
        ggplot2::geom_point(mapping = aes_string(x = "long", y = "lat", shape = "bias"), data = point.gaz,  col = "grey10")+
        ggplot::labs(shape = "", color = "Bias Effect", linetype = "")
      })
  }

  #load world outlines


  for(i in 1:length(out)){
    print(out[[i]])
  }
}
