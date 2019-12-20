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
#' plot(out)
#'
#'@method plot sampbias
#'@export
#'@importFrom forcats fct_reorder
#'@importFrom cowplot plot_grid
#'@importFrom ggplot2 ggplot geom_boxplot geom_point xlab ylab coord_flip theme_bw theme
#'@importFrom graphics plot
#'@importFrom magrittr %>%
#'@importFrom dplyr contains mutate select
#'@importFrom rlang .data
#'@importFrom stats median
#'@importFrom tidyr pivot_longer
#'@importFrom viridis scale_color_viridis
#'
plot.sampbias <- function(x, ...) {

  plo1 <-  x$bias_estimate %>%
    pivot_longer(cols = contains("w_"), names_to = "bias",
                 values_to = "posterior_estimate") %>%
    mutate(bias = gsub("w_", "", .data$bias)) %>%
    mutate(bias = fct_reorder(.data$bias, .data$posterior_estimate, .fun = median, .desc = FALSE))

  plo2_w <-colMeans(x$bias_estimate)
  plo2_dist <- seq(1,1000,length.out=1000)
  plo2 <- data.frame(dist = plo2_dist,
                     rate = plo2_w[4] * exp(-plo2_w[5:length(plo2_w)]*plo2_dist),
                     id = names(x$bias_estimate)[-c(1:4)]) %>%
    mutate(id = gsub("w_", "", .data$id)) %>%
    mutate(id = factor(.data$id, levels = levels(plo1$bias)))

  p1 <- ggplot(plo1)+
    geom_boxplot(aes(x = .data$bias, y = .data$posterior_estimate, fill = .data$bias))+
    scale_fill_viridis(discrete = TRUE)+
    xlab("Biasing factor")+
    ylab("Posterior weight")+
    coord_flip()+
    theme_bw()+
    theme(panel.grid.minor.x = element_blank(),
          panel.grid.major.y = element_blank(),
          legend.position = "none")

  p2 <- ggplot(plo2)+
    geom_point(aes(x = .data$dist, y = .data$rate, color = .data$id))+
    scale_color_viridis(discrete = TRUE)+
    xlab("Distance to the bias [km]")+
    ylab("Sampling rate")+
    theme_bw()+
    theme(panel.grid.minor.x = element_blank(),
          panel.grid.major.y = element_blank(),
          legend.title = element_blank(),
          legend.position = "bottom")

  out <- plot_grid(p1, p2, labels = c('A', 'B'), ncol = 1)

  print(out)
}
