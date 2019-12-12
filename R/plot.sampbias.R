#' Mapping Occurrence and Bias Rasters in an Object of the Class Sampbias
#'
#' Plotting method for class \code{sampbias}, generating a rasterized plots
#' showing the number of occurrences and the number of species per gridcell,
#' one bias raster indicating the effect strength for each gazetteer provided to
#' \code{SamplingBias}, and a average bias raster indicating the average bias
#' for each gridcell.
#'
#'
#' @param x an object of the class \code{sampbias}.
#' @param gaz a list of SpatialObjects, to be printed on the maps.  Should be
#' the same objects provided to \code{SamplingBias} when creating the Object.
#' If \code{gaz} is not supplied, the sampbias package standard gazetteers are
#' used.
#' @param sealine logical. Should the sealine be added to the plots? Default is
#' to TRUE.
#' @param ...  Additional arguments to be passed to \code{\link{plot}}.
#' @return A series of R plots based on ggplot2.
#' @note Check https://github.com/azizka/sampbias/wiki for a tutorial on
#' sampbias.
#' @seealso \code{\link{SamplingBias}}, \code{\link{summary.sampbias}}
#' @keywords methods
#' @examples
#'
#' #simulate data
#' occ <- data.frame(species = rep(sample(x = LETTERS, size = 5), times = 10),
#'                   decimallongitude = runif(n = 50, min = -5, max = 5),
#'                   decimallatitude = runif(n = 50, min = -4, max = 4))
#'
#'
#' #create point gazetteer
#' pts <- data.frame(long = runif(n = 5, min = -5, max = 5),
#'                   lat = runif(n = 5, min = -4, max = 4),
#'                   dat = rep("A", 5))
#' pts <- SpatialPointsDataFrame(coords = pts[,1:2], data = data.frame(pts[,3]))
#'
#' lin <- data.frame(long = seq(-5, 5, by = 1),
#'                   lat = rep(2, times = 11))
#' lin <- SpatialLinesDataFrame(sl = SpatialLines(list(Lines(Line(lin), ID="B1"))),
#'                              data = data.frame("B", row.names = "B1"))
#'
#' gaz <- list(lines.strucutre = lin, point.structure = pts)
#'
#' out <- sampling_bias(x = occ, gaz = gaz, terrestrial = FALSE)
#' plot(out)
#'
#'@export
#'@importFrom ggplot2 aes_string coord_fixed element_blank fortify geom_path geom_point geom_polygon geom_raster ggplot ggtitle labs theme theme_bw
#'@importFrom raster as.data.frame crop extent
#'@importFrom sp coordinates
#'@importFrom viridis scale_fill_viridis
#'
plot.sampbias <- function(x, gaz = NULL, sealine = T, ...) {

  # prepare gazetteers to be included for plotting
  if (!is.null(gaz)) {
    gaz.plo <- lapply(gaz, function(k) raster::crop(x = k, y = x$occurrences))

    # find which gazetteers are points and which are list
    list.condition <- sapply(gaz.plo, function(x) class(x) == "SpatialPointsDataFrame")

    point.gaz <- gaz.plo[list.condition]
    point.gaz <- lapply(point.gaz, "data.frame")

    for (i in 1:length(point.gaz)) {
      point.gaz[[i]] <- data.frame(long = point.gaz[[i]]$longitude, lat = point.gaz[[i]]$latitude,
                                   bias = rep(names(point.gaz)[i], nrow(point.gaz[[i]])))
    }

    point.gaz <- do.call("rbind.data.frame", point.gaz)

    line.gaz <- gaz.plo[!list.condition]
    line.gaz <- lapply(line.gaz, "fortify")

    for (i in 1:length(line.gaz)) {
      line.gaz[[i]] <- data.frame(line.gaz[[i]], bias = rep(names(line.gaz)[i],
                                                            nrow(line.gaz[[i]])))
    }
    line.gaz <- do.call("rbind.data.frame", line.gaz)
  }

  # Occurrence raster
  rast <- data.frame(sp::coordinates(x$occurrences), raster::as.data.frame(x$occurrences))
  colnames(rast) <- c("Longitude", "Latitude", "val")

  occ.plo <- ggplot2::ggplot()+
    ggplot2::geom_raster(data = rast, ggplot2::aes_string(y = "Latitude", x = "Longitude", fill = "val"))+
    ggplot2::coord_fixed()+
    ggplot2::theme_bw()+
    viridis::scale_fill_viridis(na.value = "transparent", option = "viridis",
                                direction = -1, discrete = F)+
    ggplot2::ggtitle("Occurrences")+
    ggplot2::theme(legend.title = element_blank())

  # Species raster
  rast <- data.frame(sp::coordinates(x$species), raster::as.data.frame(x$species))
  colnames(rast) <- c("Longitude", "Latitude", "val")

  spe.plo <- ggplot2::ggplot()+
    ggplot2::geom_raster(data = rast, aes_string(y = "Latitude", x = "Longitude", fill = "val"))+
    ggplot2::coord_fixed()+
    ggplot2::theme_bw()+
    viridis::scale_fill_viridis(na.value = "transparent", option = "viridis",
                                direction = -1, name = "Species\nnumber", discrete = F)+
    ggplot2::ggtitle("Species")+
    ggplot2::theme(legend.title = element_blank())

  # Bias rasters
  plo.biasras <- lapply(x$biasmaps, function(k) {
    # ra <- raster(k$bias_matrix)
    out <- data.frame(sp::coordinates(k$bias_matrix),
                      raster::as.data.frame(k$bias_matrix))  # use species as all rasters have the same extent, to get the coordinates back
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

  out <- c(list(occ.plo, spe.plo), plo.biasras.out)

  if (sealine == T) {
    wrld <- raster::crop(sampbias::landmass,
                         raster::extent(x$occurrences))
    wrld <- ggplot2::fortify(wrld)
    out <- lapply(out, function(k) {
      k + ggplot2::geom_polygon(data = wrld,
                                aes_string(x = "long",
                                           y = "lat",
                                           group = "group"),
                                lwd = 0.5,
                                col = "grey40", fill = "transparent")
    })
  }

  # add gazetteers to plots
  if (!is.null(gaz)) {
    out <- lapply(out, function(k) {
      k + ggplot2::geom_path(mapping = aes_string(x = "long", y = "lat",
                                                  linetype = "bias", group = "group"),
                             data = line.gaz, col = "grey40")+
        ggplot2::geom_point(mapping = aes_string(x = "long", y = "lat",
                                                 shape = "bias"), data = point.gaz, col = "grey10")+
        ggplot2::labs(shape = "", color = "Bias Effect", linetype = "")
    })
  }

  #print plots
  for (i in 1:length(out)) {
    print(out[[i]])
  }
}
