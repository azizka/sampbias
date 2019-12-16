#' Summary Method for Class sampbias
#'
#' Summary method for objects of the class \code{sampbias}.
#'
#'
#' @param object An object of the class \code{sampbias}
#' @param \dots Additional arguments passed to summary.
#' @return Summary printed to screen.
#' @seealso \code{\link{calculate_bias}} \code{\link{is.sampbias}}
#' \code{\link{plot.sampbias}}
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
#' summary(out)
#' @method summary sampbias
#' @export
#'
summary.sampbias <- function(object, ...) {
  cat("Number of occurences: ", object$summa$total_occ, "\n")
  cat("Raster resolution: ", object$summa$res, "\n")
  cat("Convexhull: ", object$summa$convexhull, "\n")
  cat("Geographic extent:\n")
  print(object$summa$extent)
  cat("Bias weights:\n")
  print(data.frame(bias_weight = colMeans(object$bias_estimate[,-c(1:4)]),
                   std_dev = apply(object$bias_estimate[,-c(1:4)], 2, "sd")))
}
