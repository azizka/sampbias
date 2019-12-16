#' Plotting the Posterior Estimates of the Bias Weights
#'
#' Plotting method for class \code{sampbias}, generating a box-whiskers-plot
#' showing the bias weights for all biasing factors
#' indicating the effect strength for each gazetteer provided to
#' \code{calculate_bias}.
#'
#'
#' @param x an object of the class \code{sampbias}.
#' @param \dots Additional arguments passed to summary.
#' @return A plot
#' @seealso \code{\link{calculate_bias}}, \code{\link{summary.sampbias}}
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
#' plot(out)
#'
#'@method plot sampbias
#'@export
#'@importFrom forcats fct_reorder
#'@importFrom ggplot2 ggplot geom_boxplot xlab ylab coord_flip theme_bw theme
#'@importFrom graphics plot
#'@importFrom magrittr %>%
#'@importFrom dplyr contains mutate select
#'@importFrom rlang .data
#'@importFrom stats median
#'@importFrom tidyr pivot_longer
#'
plot.sampbias <- function(x, ...) {

  plo <-  x$bias_estimate %>%
    pivot_longer(cols = contains("w_"), names_to = "bias",
                 values_to = "posterior_estimate") %>%
    mutate(bias = gsub("w_", "", .data$bias)) %>%
    mutate(bias = fct_reorder(.data$bias, .data$posterior_estimate, .fun = median, .desc = FALSE))

  out <- ggplot(plo)+
    geom_boxplot(aes(x = .data$bias, y = .data$posterior_estimate))+
    xlab("Biasing factor")+
    ylab("Posterior weight")+
    coord_flip()+
    theme_bw()+
    theme(panel.grid.minor.x = element_blank(),
          panel.grid.major.y = element_blank())

  print(out)
}
