#' Mapping Projected Bias Effects
#'
#'A plotting function to visualize the effect of accessibility bias caused by different biasing factors
#'in space.
#'
#'
#' @param x a raster stack as generate by \code{\link{project_bias}}
#' @param gaz a list of SpatialObjects, to be printed on the maps.  Should be
#' the same objects provided to \code{calculate_bias} when creating the Object.
#' If \code{gaz} is not supplied, the sampbias package standard gazetteers are
#' used.
#' @param sealine logical. Should the coastline be added to the plots? Default is
#' to TRUE.
#' @param type character vector. One of c("sampling_rate", "log_sampling_rate", "diff_to_max"). If "sampling_rate".
#' the plot shows the raw projected sampling rate depending on the
#' biasing factors, if "log_sampling_rate", the plot shows the log10 transformed sampling rate, and if
#' "diff_to_max", the relative deviation of sampling rate from the maximum
#' rate as calculated using \code{\link{calculate_bias}}
#'and projected using \code{\link{project_bias}}. For instance, a value of -25 indicates
#' a drop of 25% compared to the highest rate
#' (e.g. in a road on river flowing through the city airport).
#' @return A series of R plots based on ggplot2.

#' @seealso \code{\link{calculate_bias}}, \code{\link{project_bias}}
#' @keywords methods
#' @examples
#' \dontrun{
#'   #simulate data
#'   occ <- data.frame(species = rep(sample(x = LETTERS, size = 5), times = 10),
#'                    decimalLongitude = runif(n = 50, min = 12, max = 20),
#'                    decimalLatitude = runif(n = 50, min = -4, max = 4))
#'
#'   out <- calculate_bias(x = occ, terrestrial = TRUE)
#'   proj <- project_bias(out)
#'   map_bias(proj)
#' }
#'
#'@export
#'@importFrom dplyr rename bind_cols filter
#'@importFrom ggplot2 aes coord_fixed element_blank facet_wrap fortify geom_path
#'  geom_point geom_polygon geom_raster ggplot ggtitle labs theme theme_bw
#'  geom_sf
#'@importFrom terra as.data.frame crds
#'@importFrom rlang .data
#'@importFrom viridis scale_fill_viridis
#'@importFrom sf st_agr st_crop
#'
map_bias <- function(x,
                     gaz = NULL,
                     sealine = TRUE,
                     type = "sampling_rate") {

  match.arg(type, choices = c("sampling_rate", "log_sampling_rate", "diff_to_max"))

  # prepare gazetteers to be included for plotting
  if (!is.null(gaz)) {
    gaz.plo <- lapply(gaz, function(k) terra::crop(x = k, y = x$occurrences))

    # find which gazetteers are points and which are list
    list.condition <- sapply(gaz.plo, function(x) class(x) == "SpatialPointsDataFrame")

    point.gaz <- gaz.plo[list.condition]
    point.gaz <- lapply(point.gaz, "data.frame")

    for (i in seq_along(point.gaz)) {
      point.gaz[[i]] <- data.frame(long = point.gaz[[i]]$longitude,
                                   lat = point.gaz[[i]]$latitude,
                                   bias = rep(names(point.gaz)[i],
                                              nrow(point.gaz[[i]])))
    }

    point.gaz <- do.call("rbind.data.frame", point.gaz)

    line.gaz <- gaz.plo[!list.condition]
    line.gaz <- lapply(line.gaz, "fortify")

    for (i in seq_along(line.gaz)) {
      line.gaz[[i]] <- data.frame(line.gaz[[i]], bias = rep(names(line.gaz)[i],
                                                            nrow(line.gaz[[i]])))
    }
    line.gaz <- do.call("rbind.data.frame", line.gaz)
  }

  # Bias rasters
  message("Plotting bias rasters")
  coords <- x %>%
    terra::crds() %>%
    as.data.frame() %>%
    rename(geo_lon = .data$x, geo_lat = .data$y)


  plo <- terra::as.data.frame(x) %>%
    bind_cols(coords) %>%
    pivot_longer(cols = -contains("geo"), values_to = "val", names_to = "split") %>%
    filter(split != "occurrences")


  if (type == "sampling_rate") {
    plo <- plo %>%
      filter(split != "Total_percentage")

    out <- ggplot(plo)+
      geom_raster(aes(x = .data$geo_lon, y = .data$geo_lat, fill = .data$val))+
      facet_wrap(split~ .)+
      coord_fixed()+
      theme_bw()+
      scale_fill_viridis(na.value = "transparent",
                         option = "viridis",
                         direction = 1,
                         name = "Estimated\nsampling rate",
                         discrete = FALSE)+
      theme(axis.title = element_blank())+
      facet_wrap(split~ .)
  }

  if (type == "log_sampling_rate") {
    plo <- plo %>%
      filter(split != "Total_percentage")

    out <- ggplot(plo)+
      geom_raster(aes(x = .data$geo_lon, y = .data$geo_lat, fill = log10(.data$val)))+
      facet_wrap(split~ .)+
      coord_fixed()+
      theme_bw()+
      scale_fill_viridis(na.value = "transparent",
                         option = "viridis",
                         direction = 1,
                         name = "Estimated\nsampling\nrate [log10]",
                         discrete = FALSE)+
      theme(axis.title = element_blank())+
      facet_wrap(split~ .)
  }

  if (type == "diff_to_max") {
    plo <- plo %>%
      filter(split == "Total_percentage")

    out <- ggplot(plo) +
      geom_raster(aes(x = .data$geo_lon, y = .data$geo_lat, fill = .data$val)) +
      coord_fixed() +
      theme_bw() +
      scale_fill_viridis(na.value = "transparent",
                         option = "viridis",
                         direction = 1,
                         name = "Relative change\nto the mean\n[%]",
                         discrete = FALSE) +
      theme(axis.title = element_blank())
  }

  if (sealine == TRUE) {
    message("Adding sealine")
    landmass <-
      try(suppressWarnings(
        rnaturalearth::ne_coastline(returnclass = "sf")
      ), silent = TRUE)
    if (inherits(landmass, "try-error")) { 
      stop(paste("landmass could not be downloaded using rnaturalearth."),
           "Please, check your internet connection.")
    }
    sf::st_agr(landmass) <- "constant"
    wrld <- sf::st_crop(landmass,
                        terra::ext(x$occurrences))
    out <- out +
      geom_sf(data = wrld,
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
                data = line.gaz, col = "grey40") +
      geom_point(mapping = aes(x = .data$long,
                               y = .data$lat,
                               shape = .data$bias),
                 data = point.gaz,
                 col = "grey10") +
      labs(shape = "", color = "Bias Effect", linetype = "")
  }

  #return plots
  print(out)
}
