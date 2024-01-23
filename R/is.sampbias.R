#' Is Method for Class sampbias
#'
#' Check class of \code{sampbias} objects.
#'
#' With two arguments, tests whether object can be treated as from class2.
#' With one argument, returns all the super-classes of this object's class.
#'
#' @param object an object of the class \code{sampbias}
#' @param class2 the names of the class to which is relations are to be
#' examined defined, or (more efficiently) the class definition objects for the
#' classes.
#' @keywords methods
#' @examples
#' \dontrun{
#'   #simulate data
#'   occ <- data.frame(species = rep(sample(x = LETTERS, size = 5), times = 10),
#'                    decimalLongitude = runif(n = 50, min = 12, max = 20),
#'                    decimalLatitude = runif(n = 50, min = -4, max = 4))
#'
#'   out <- calculate_bias(x = occ, terrestrial = TRUE)
#'   is(out)
#' }
#'@method is sampbias
#'@importFrom methods is
#'@export
#'
is.sampbias <- function(object, class2){
  inherits(object, "sampbias")}
