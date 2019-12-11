#' Evaluating Sampling Bias in Species Distribution Data
#'
#' The major function of the package, calculating the bias effect of sampling
#' bias due to geographic structures, such as the vicinity to cities, airports,
#' rivers and roads. Results are projected to space, and can be compared
#' numerically.
#'
#' The default gazetteers delivered with the package are simplified from
#' http://www.naturalearthdata.com/downloads/. They include major features, and
#' for small scale analyses custom gazetteers should be used.
#'
#' For computational convenience, the gazetteers are cropped to the extent of
#' the point occurrence data sets. To account for the fact, that, relevant
#' structures might lay directly outside this extent, but still influencing the
#' distribution of samples in the study area, the buffer option, gives the
#' area, around the extent that should be included in the distance calculation.
#'
#' The \code{biasdist} options defines at which distance the bias effect should
#' be recorded. These values are comparable between different biasing factors
#' and give an estimate of bias severity. For instance, if \code{biasdist} =
#' c(0, 10000), the \code{biastable} will indicate the bias effect at distances
#' 0 and 10000 meters from all sources of bias. The bias effect is the fraction
#' of samples expected to be missed at a given distance relative to distance 0.
#' A high bias effect means a high fraction of missed records. The fractions
#' are comparable between sources of bias and different datasets and the bias
#' at any given distance can thus inform on the relative severity of a biasing
#' source.
#'
#' Visit \url{https://github.com/azizka/sampbias/wiki} for more information on
#' distance calculation and the algorithm behind sampbias.
#'
#' @param x an object of the class \code{data.frame}, with one species
#' occurrence record per line, and at least three columns, named
#' \sQuote{species}, \sQuote{decimallongitude}, and \sQuote{decimallatitude}.
#' @param gaz a list of geographic gazetteers as \code{SpatialPointDataFrame}
#' or \code{SpatialLinesDataFrame}.  If NULL, a set of default gazetteers,
#' representing large scale occurrence of airports, cities, rivers and roads is
#' used.  See Details.
#' @param res numerical.  The raster resolution for the distance calculation to
#' the geographic features and the data visualization, in decimal degrees. The
#' default is to one degree, but higher resolution will be desirable for most
#' analyses. Res together with the extent of the input data determine
#' computation time and memory requirements.
#' @param buffer numerical.  The size of the geographic buffer around the
#' extent of \code{ras} for the distance calculations in degrees, to account
#' for geographic structures neighbouring the study area (such as a road right
#' outside the study area). Should be a multiple of res.  Default is to res.
#' See Details.
#' @param convexhull logical.  If TRUE, the empirical distribution (and the
#' output maps) is restricted to cells within a convex hull polygon around
#' \code{x}. If FALSE a rectangle around \code{x} is used.  Default = FALSE.
#' @param terrestrial logical.  If TRUE, the empirical distribution (and the
#' output maps) are restricted to terrestrial areas.  Uses the
#' \code{\link{landmass}} to define what is terrestrial.  Default = TRUE.
#' @param binsize numerical.  Indicating the distance bin size (in meters!) for
#' approximating the empirical and observed sample distributions. Should be
#' slightly larger than res (which is in degrees). Default = res * 100000 *
#' 1.1.
#' @param biasdist numerical.  A vector indicating the distance at which the
#' average bias should be calculated for the output table (in meters). Can also
#' be a single number.  See details.  Default = c(0, 10000).
#' @param ncores numerical.  The number of cores used for parallel computing.
#' Must be lower than the available number of cores. Not finally implemented in
#' version 0.1.0.
#' @param plotextra logical. logical. If TRUE, creates a .pdf file with plots
#' of the statistical distance distributions and bias effects
#' @param plotextrafile character.  The full path where the extra plot should
#' be saved.
#' @param verbose logical.  If TRUE, progress is reported.  Default = TRUE.
#' @return An object of the S3-class \sQuote{sampbias}, which is a list
#' including the following objects: \item{summa}{A list of summary statistics
#' for the sampbias analyses, including the total number of occurrence points
#' in \code{x}, the total number of species in \code{x}, the extent of the
#' output rasters as well as the settings for \code{res}, \code{binsize}, and
#' \code{convexhull} used in the analyses.} \item{occurrences}{a \code{raster}
#' indicating occurrence records per grid cell, with resolution res.}
#' \item{species}{a \code{raster} with indicating the number of species per
#' grid cell, with resolution res.} \item{biasmaps}{a list of \code{rasters},
#' with the same length as gaz. Each element is the spatial projection of the
#' bias effect for a sources of bias in \code{gaz}. The last raster in the list
#' is the average over all bias sources.} \item{biastable}{a \code{data.frame},
#' with the estimated bias effect for each bias source in \code{gaz}, at the
#' distances specified by \code{biasdist}.}
#' @note Check \url{https://github.com/azizka/sampbias/wiki} for a tutorial on
#' sampbias.
#' @seealso \code{\link{summary.sampbias}} \code{\link{is.sampbias}}
#' \code{\link{plot.sampbias}}
#' @keywords maths spatial
#' @examples
#'
#' #simulate data
#' occ <- data.frame(species = rep(sample(x = LETTERS, size = 5), times = 10),
#'                   decimallongitude = runif(n = 50, min = -5, max = 5),
#'                   decimallatitude = runif(n = 50, min = -4, max = 4))
#'
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
#' out <- SamplingBias(x = occ, gaz = gaz, terrestrial = FALSE)
#' summary(out)
#'
#' @export
#' @importFrom raster as.raster extent extent<- mask res<- values
#' @importFrom grDevices is.raster
#'
SamplingBias <- function(x, gaz = NULL, res = 1, buffer = NULL, convexhull = F, terrestrial = T,
                         binsize = NULL, biasdist = c(0, 10000), ncores = 1, plotextra = F,
                         plotextrafile = "samp_bias_extra_plots.pdf", verbose = T) {

  #convert x to SpatialPoints
  dat.pts <- sp::SpatialPoints(x[, c("decimallongitude", "decimallatitude")])
  # create dummy raster
  dum.ras <- raster::raster(round(extent(dat.pts), .DecimalPlaces(res)))
  res(dum.ras) <- res

  #exclude occurrences in the sea and recreate dummy raster
  if(terrestrial){
    if(verbose){cat("Adjusting to terrestrial surface...")}
    wrld <- raster::crop(sampbias::landmass, extent(dum.ras))
    wrld <- raster::rasterize(wrld, dum.ras)

    excl <- raster::extract(wrld, x[,c("decimallongitude", "decimallatitude")])
    x <- x[!is.na(excl), c("species", "decimallongitude", "decimallatitude")]
    rownames(x) <- NULL
    x$species <- as.factor(as.character(x$species))

    dat.pts <- sp::SpatialPoints(x[, c("decimallongitude", "decimallatitude")])

    dum.ras <- raster::raster(round(extent(dat.pts) + res, .DecimalPlaces(res)))
    res(dum.ras) <- res

    wrld <- raster::crop(sampbias::landmass, extent(dum.ras))
    wrld <- raster::rasterize(wrld, dum.ras)
    if(verbose){cat(" Done.\n")}
  }

  # warning if combination of resolution and extent exceed 1mio gridcells
  if (raster::ncell(dum.ras) > 1e+06) {
    warning("Huge raster size")
  }

  # occurrence raster
  if (verbose) {
    cat("Creating occurrence raster...")
  }
  occ.out <- .OccRast(x = dat.pts, ras = dum.ras)

  if (terrestrial) {
    occ.out <-  mask(occ.out, wrld)
  }

  occ.out[is.na(occ.out)] <- 0

  if (verbose) {
    cat(" Done\n")
  }

  # # species raster
  if (verbose) {
    cat("Creating species raster...")
  }
  spe.out <- .SpeRast(x, ras = dum.ras, ncores = ncores)
  if (verbose) {
    cat(" Done\n")
  }

  # Distance raster calculation check if gazeteers are provided, replace by
  # standard gazeteers if necessary
  if (verbose) {
    cat("Calculating distance raster...")
  }
  if (is.null(gaz)) {
    warning("'gaz' not found, using standard gazetteers")

    gaz <- list(airports = sampbias::airports, cities = sampbias::cities,
                rivers = sampbias::waterbodies, roads = sampbias::roads)
  }

  # check if gaz are provided as distance rasters, if so, this function will
  # run much faster
  check <- all(sapply(gaz, is.raster))

  if (all(check)) {
    #use input directly as distance raster
    dis.ras <- gaz
  } else {
    ## create distance raster for all gazeteers
    dis.ras <- DisRast(gaz = gaz, ras = occ.out, buffer = buffer, ncores = ncores)
    if (terrestrial) {
      dis.ras <- lapply(dis.ras, function(k) mask(k, wrld))
    }

    # check if there are values in the distance raster
    if (all(is.na(values(dis.ras[[1]])))) {
      stop("No valid distances found. Consider setting terrestrial = F")
    }

    #dis.mat <- lapply(dis.ras, function(k){out <- raster::as.matrix(k); return(out)})
  }
  if (verbose) {
    cat(" Done\n")
  }

  ## Check if a distance was found for any gazeteer, if not, only return
  ## species and occurrence raster
  if (is.logical(dis.ras)) {
    out <- c(Occurences = occ.out, Species = spe.out, results)
    class(out) <- append("sampbias", class(out))
    return(out)
  } else {

    # Generate the data.frame with the counts and distances
    dis.vec <- lapply(dis.ras, "getValues")
    dis.vec <- as.data.frame(do.call(cbind, dis.vec))
    names(dis.vec) <- names(dis.ras)

    dis.vec <- data.frame(cell_id = 1:nrow(dis.vec),
                          record_count = getValues(occ.out),
                          dis.vec)
    dis.vec <- dis.vec[complete.cases(dis.vec),]
    save(dis.vec, file = "record_count_and_Distances_dataframe.rda")


    # FROM HERE ON ALL NEEDS TO BE REPLACED!

    # empirical and observed values in the input format for the likelihood
    # optimization set binsize based on raster resolution, if no binsize is
    # provided, roughly assuming 1 deg = 100 km
    if (is.null(binsize)) {
      binsize <- res * 1e+05
    }
    if (verbose) {
      cat("Extracting values...")
    }
    dis.vec <- .DisVect(x = dat.pts, dist = dis.ras, convexhull = convexhull,
                        binsize = binsize)
    if (verbose) {
      cat(" Done\n")
    }

    # run likelihood calculation
    if (verbose) {
      cat("Calculating likelihood...")
    }
    biasdist2 <- biasdist/1000

    results <- .RunSampBias(dists = dis.vec, distmats = dis.mat, plotextra = plotextra,
                            biasdist = biasdist2, plotextrafile = plotextrafile)

    # crop output rasters to landmass by using the distance rasters
    for (i in 1:length(results)) {
      results[[i]]$bias_matrix <- raster::raster(results[[i]]$bias_matrix)
      extent(results[[i]]$bias_matrix) <- raster::extent(dis.ras[[1]])
      results[[i]]$bias_matrix <- mask(results[[i]]$bias_matrix, dis.ras[[1]])
    }

    # create biastable with the bias per distance
    biastable <- lapply(results, "[", "bias_vector")
    biastable <- lapply(biastable, "data.frame")
    biastable <- lapply(biastable, function(k) t(k))
    biastable <- biastable[!is.na(biastable)]
    biastable <- do.call("rbind.data.frame", biastable)
    names(biastable) <- as.numeric(names(biastable)) * 1000

    if (verbose) {
      cat(" Done\n")
    }

    # create output file, a list of the class sampbias
    if (verbose) {
      cat("Preparing output...")
    }
    out <- list(summa = list(total_occ = nrow(x), total_sp = length(unique(x$species)),
                             extent = extent(dum.ras), res = res, binsize = binsize, convexhull = convexhull),
                occurrences = occ.out, species = spe.out, biasmaps = results, biastable = biastable)
    class(out) <- append("sampbias", class(out))
    if (verbose) {
      cat(" Done\n")
    }

    return(out)
  }
}
