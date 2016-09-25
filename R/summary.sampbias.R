summary.sampbias <- function(object, ...){
  cat("Number of occurences: ", object$summa$total_occ, "\n")
  cat("Number of species: ", object$summa$total_sp, "\n")
  cat("Raster resolution: ", object$summa$res, "\n")
  cat("Distance binsize: ", object$summa$binsize, "\n")
  cat("Convexhull: ", object$summa$convexhull, "\n")
  cat("Geographic extent:\n")
  print(object$summa$extent)
  cat("Bias effect at distance:\n")
  print(object$biastable)
}
