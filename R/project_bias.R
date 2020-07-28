#' Projecting Bias Effects in Space
#'
#'
#' Uses the the estimated bias weights from a \code{sampbias} object to project the
#' bias through space, using the same raster as used for the distance calculation.#'
#'
#' @param x an object of the class \code{sampbias}.
#' @param factors a character vector indicating which biasing factors to project
#' @return A raster stack, with the same length as the number of biasing factors used
#' in \code{\link{calculate_bias}}. The names indicate the factors included for each layer.
#' @seealso \code{\link{calculate_bias}}, \code{\link{summary.sampbias}}
#' @keywords methods
#' @examples
#'
#' #simulate data
#' occ <- data.frame(species = rep(sample(x = LETTERS, size = 5), times = 10),
#'                   decimalLongitude = runif(n = 50, min = -5, max = 5),
#'                   decimalLatitude = runif(n = 50, min = -4, max = 4))
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
#' out <- project_bias(out)
#'
#'@export
#'@importFrom magrittr %>%
#'@importFrom raster stack as.matrix
#'@importFrom rlang .data


project_bias <- function(x, factors = NULL) {

  # get raster stack
  # ras <- as.matrix(x$distance_rasters) %>% t() %>% data.frame()
  ras <- as.matrix(x$distance_rasters) %>% data.frame()

  # rescale as in the bias estimation
  ras <- ras/x$summa$rescale_distances

  # get mean posterior weights and sort according to importance
  mean_w <- colMeans(x$bias_estimate)[-c(1:4)] %>% sort() %>% rev()

  # sort ras as mean_w
  ord <- match(gsub("w_", "", names(mean_w)), colnames(ras))
  ras <- ras[, ord]

  # use factors if provided to select certain biasing effects for projection
  if (!is.null(factors)) {
    mean_w <- mean_w[gsub("w_", "", names(mean_w)) %in% factors]
    ras <- ras[,colnames(ras) %in% factors]
  }

  # calculate the values for each raster cell
  lambdas <- list()

  ## Get the mean estimate for the mean rate
  mean_q <- colMeans(x$bias_estimate)[4]

  ## calculate the sampling rate when increasingly adding biasing factors
  for (i in seq_along(mean_w)) {
    if(is.numeric(ras)){
      test <- ras
    }else{
      test <- ras[,1:i]
    }

    lambdas[[i]] <- get_lambda_ij(q = mean_q,
                                  w = mean_w[1:i],
                                  X = test) %>%
      as.matrix()
  }

  ## add a plot for the relative variation
  perc <- round((lambdas[[length(lambdas)]] - mean_q) / mean_q * 100, 2)
  lambdas[[length(lambdas) + 1]] <- perc

  # re-transfor to a spatial raster
  out <- lapply(lambdas, "raster",
                template = x$distance_rasters[[1]]) %>%
    c(x$occurrences) %>% stack()

  # define the names
  nam <- vector()
  for (i in seq_along(mean_w)) {
    nam <- c(nam, paste(gsub("w_", "",
                             names(mean_w)[1:i]),
                        collapse = "+"))
  }

  # add the total percentage change and the occurrences, which are also  part of the stack
  nam <- c(nam, "Total_percentage", "occurrences")

  names(out) <- nam

  # retun output object
  return(out)
}
