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
#' \dontrun{
#'   #simulate data
#'   occ <- data.frame(species = rep(sample(x = LETTERS, size = 5), times = 10),
#'                    decimalLongitude = runif(n = 50, min = 12, max = 20),
#'                    decimalLatitude = runif(n = 50, min = -4, max = 4))
#'
#'   out <- calculate_bias(x = occ, terrestrial = TRUE)
#'   summary(out)
#' }
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
