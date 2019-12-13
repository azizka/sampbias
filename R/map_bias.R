#' Mapping Projected Bias Effects
#'
#'A plotting function to visualize the bias effects calculated using \code{\link{calculate_bias}}
#'and projected using \code{\link{project_bias}}.
#'
#'
#' @param x a raster stack as generate by \code{\link{project_bias}}
#' @param gaz a list of SpatialObjects, to be printed on the maps.  Should be
#' the same objects provided to \code{calculate_bias} when creating the Object.
#' If \code{gaz} is not supplied, the sampbias package standard gazetteers are
#' used.
#' @param sealine logical. Should the sealine be added to the plots? Default is
#' to TRUE.
#' @param logtransform logical. IF TRUE, the bias projections are logtransformed. Default is TRUE.
#' @return A series of R plots based on ggplot2.

#' @seealso \code{\link{calculate_bias}}, \code{\link{project_bias}}
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
#' out <- calculate_bias(x = occ, gaz = gaz, terrestrial = FALSE)
#' proj <- project_bias(out)
#' map_bias(proj)
#'
#'@export
#'@importFrom dplyr rename bind_cols filter
#'@importFrom ggplot2 aes coord_fixed element_blank facet_wrap fortify geom_path geom_point geom_polygon geom_raster ggplot ggtitle labs theme theme_bw
#'@importFrom raster as.data.frame coordinates
#'@importFrom rlang .data
#'@importFrom viridis scale_fill_viridis
#'
map_bias <- function(x,
                     gaz = NULL,
                     sealine = TRUE,
                     logtransform = TRUE) {

  # prepare gazetteers to be included for plotting
  if (!is.null(gaz)) {
    gaz.plo <- lapply(gaz, function(k) raster::crop(x = k, y = x$occurrences))

    # find which gazetteers are points and which are list
    list.condition <- sapply(gaz.plo, function(x) class(x) == "SpatialPointsDataFrame")

    point.gaz <- gaz.plo[list.condition]
    point.gaz <- lapply(point.gaz, "data.frame")

    for (i in 1:length(point.gaz)) {
      point.gaz[[i]] <- data.frame(long = point.gaz[[i]]$longitude,
                                   lat = point.gaz[[i]]$latitude,
                                   bias = rep(names(point.gaz)[i],
                                              nrow(point.gaz[[i]])))
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

  # Bias rasters
  message("Plotting bias rasters")
  coords <- x %>%
    coordinates() %>%
    as.data.frame() %>%
    rename(geo_lon = .data$x, geo_lat = .data$y)


  plo <-as.data.frame(x) %>%
    bind_cols(coords) %>%
    pivot_longer(cols = -contains("geo"), values_to = "val", names_to = "split") %>%
    filter(split != "occurrences")

  if(logtransform){
    out <- ggplot(plo)+
      geom_raster(aes(x = .data$geo_lon, y = .data$geo_lat, fill = log10(.data$val)))
  }else{
    out <- ggplot(plo)+
      geom_raster(aes(x = .data$geo_lon, y = .data$geo_lat, fill = .data$val))
  }



  out <- out+
    facet_wrap(split~ .)+
    coord_fixed()+
    theme_bw()+
    scale_fill_viridis(na.value = "transparent",
                       option = "viridis",
                       direction = 1,
                       name = "Bias effect [log]",
                       discrete = F)+
    theme(axis.title = element_blank())+
    facet_wrap(split~ .)


  if (sealine == T) {
    message("Adding sealine")
    wrld <- raster::crop(sampbias::landmass,
                         raster::extent(x$occurrences))
    wrld <- fortify(wrld)
    out <- out +
      geom_polygon(data = wrld,
                   aes(x = .data$long,
                       y = .data$lat,
                       group = .data$group),
                   lwd = 0.5,
                   col = "grey40",
                   fill = "transparent")
  }

  # add gazetteers to plots
  if (!is.null(gaz)) {
    message("Adding gazetteers")
    out <- out +
      geom_path(mapping = aes(x = .data$long, y = .data$lat,
                              linetype = .data$bias, group = .data$group),
                data = line.gaz, col = "grey40")+
      geom_point(mapping = aes(x = .data$long,
                               y = .data$lat,
                               shape = .data$bias),
                 data = point.gaz,
                 col = "grey10")+
      labs(shape = "", color = "Bias Effect", linetype = "")
  }

  #return plots
  print(out)
}
