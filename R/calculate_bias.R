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
#' Visit \url{https://github.com/azizka/sampbias/wiki} for more information on
#' distance calculation and the algorithm behind sampbias.
#'
#' @param x an object of the class \code{data.frame}, with one species
#' occurrence record per line, and at least three columns, named
#' \sQuote{species}, \sQuote{decimalLongitude}, and \sQuote{decimalLatitude}.
#' @param gaz a list of geographic gazetteers as \code{SpatialPointDataFrame}
#' or \code{SpatialLinesDataFrame}.  If NULL, a set of default gazetteers,
#' representing large scale occurrence of airports, cities, rivers and roads is
#' used.  See Details.
#' @param res numerical.  The raster resolution for the distance calculation to
#' the geographic features and the data visualization, in decimal degrees. The
#' default is to one degree, but higher resolution will be desirable for most
#' analyses. \code{Res} together with the extent of the input data determine
#' computation time and memory requirements.
#' @param buffer numerical.  The size of the geographic buffer around the
#' extent of \code{ras} for the distance calculations in degrees, to account
#' for geographic structures neighbouring the study area (such as a road right
#' outside the study area). Should be a multiple of \code{res}.  Default is to \code{res}.
#' See Details.
#' @param restrict_sample a SpatialPolygons or SpatialPolygonDataframe.
#' If provided the area for the bias test will be restricted to raster cells
#' within these polygons (and the extent of the sampled points in x).
#' Make sure to use adequate values for \code{res}. Default = NULL.
#' @param terrestrial logical.  If TRUE, the empirical distribution (and the
#' output maps) are restricted to terrestrial areas.  Uses the
#' \code{\link{landmass}} to define what is terrestrial.  Default = TRUE.
#' @param inp_raster an object of class raster. A template raster for the
#' counts and distance calculation. Can be used to provide a special resolution, or
#' for different coordinate reference systems. See vignette.
#' @param mcmc_rescale_distances numerical. rescaling factor for the
#' distance calculation
#' @param mcmc_iterations numerical. the number of iterations for the MCMC,
#' by default 100,000
#' @param mcmc_burnin numerical. the burn-in for the MCMC, default is to 20,000
#' @param mcmc_outfile character string. the path on where to write
#' the results of the MCMC, optional.
#' @param prior_q the gamma prior for the sampling rate $q$,
#' which represents the expected number of occurrences per cell
#' in the absence of biases. In the format c(shape,rate).
#' @param prior_w the gamma prior for the steepness of the Poisson rate decline,
#' such that w approximating 0 results in a
#' null model of uniform sampling rate q across cells.
#' In the format c(shape,rate).
#' @param plot_raster logical. If TRUE, a plot of the occurrence raster is shown
#' for diagnostic purposes. Default = FALSE
#' @param verbose logical.  If TRUE, progress is reported.  Default = TRUE.
#' @param run_null_model logical. Run a null model with bias weights set to zero.
#' @param use_hyperprior logical. If TRUE a hyperprior on the bias weights is used for
#'  regularization to avoid over-parametrization.
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
#'                   decimalLongitude = runif(n = 50, min = -5, max = 5),
#'                   decimalLatitude = runif(n = 50, min = -4, max = 4))
#'
#'
#'
#' #create point gazetteer
#' pts <- data.frame(long = runif(n = 5, min = -5, max = 5),
#'                   lat = runif(n = 5, min = -4, max = 4),
#'                   dat = rep("A", 5))
#' pts <- sp::SpatialPointsDataFrame(coords = pts[,1:2], data = data.frame(pts[,3]))
#'
#' lin <- data.frame(long = seq(-5, 5, by = 1),
#'                   lat = rep(2, times = 11))
#' lin <- sp::SpatialLinesDataFrame(sl = sp::SpatialLines(list(sp::Lines(sp::Line(lin), ID="B1"))),
#'                              data = data.frame("B", row.names = "B1"))
#'
#' gaz <- list(lines.strucutre = lin, point.structure = pts)
#'
#' out <- calculate_bias(x = occ, gaz = gaz, terrestrial = FALSE)
#' summary(out)
#'
#' @export
#' @importFrom raster as.raster extent extent<- getValues mask res<- values rasterize trim stack plot
#' @importFrom grDevices is.raster
#' @importFrom sp SpatialPointsDataFrame SpatialLinesDataFrame SpatialLines Lines Line spTransform proj4string
#' @importFrom rlang .data
#' @importFrom stats complete.cases
#' @importFrom rgeos gBuffer
#'
calculate_bias <- function(x,
                          gaz = NULL,
                          res = 1,
                          buffer = NULL,
                          restrict_sample = NULL,
                          terrestrial = TRUE,
                          inp_raster = NULL,
                          mcmc_rescale_distances = 1000,
                          mcmc_iterations = 1e+05,
                          mcmc_burnin = 2e+04,
                          mcmc_outfile = NULL,
                          prior_q = c(1, 0.01),
                          prior_w = c(1, 1),
                          plot_raster = FALSE,
                          verbose = TRUE,
                          run_null_model = FALSE,
                          use_hyperprior = TRUE) {

  #convert x to SpatialPoints
  # dat.pts <- sp::SpatialPoints(x[, c("decimalLongitude", "decimalLatitude")])
  dat.pts <- sp::SpatialPoints(x[, c("decimalLatitude", "decimalLongitude")])

  # create dummy raster if no raster is supplied
  if(!is.null(inp_raster)){
    dum.ras <- inp_raster
	raster::projection(dat.pts) <- raster::projection(dum.ras)
  }else{
    dum.ras <- raster::raster(round(extent(dat.pts), .DecimalPlaces(res)))
    res(dum.ras) <- res
  }

  # warning if combination of resolution and extent exceed 1mio grid cells
  if (raster::ncell(dum.ras) > 1e+06) {
    warning("Huge raster size")
  }

  # occurrence raster
  if (verbose) {
    message("Creating occurrence raster...")
  }
  occ.out <- .OccRast(x = dat.pts, ras = dum.ras)
  occ.out <-  raster::trim(occ.out)

  occ.out[is.na(occ.out)] <- 0

  ## adapt occurrence raster to terrestrial surface
  if (terrestrial) {
    if(verbose){message("Adjusting to terrestrial surface...")}

    ## Get landmass
    if(!is.null(inp_raster)){
      wrld <- spTransform(sampbias::landmass, CRSobj = proj4string(inp_raster))
      wrld <- raster::crop(wrld, extent(occ.out))
      wrld <- raster::rasterize(wrld, occ.out)
    }else{
      wrld <- raster::crop(sampbias::landmass, extent(occ.out))
      wrld <- raster::rasterize(wrld, occ.out)
    }
    ## adjust raster
    occ.out <-  raster::mask(occ.out, wrld)
    occ.out <-  raster::trim(occ.out)
    }

  ## Adapt occurrence raster to custom study extent if provided
  if(!is.null(restrict_sample)){
    if(verbose){message("Adjusting to custom study extent...")}
    ## Get landmass
    restrict_sample <- raster::crop(restrict_sample, extent(occ.out))
    cust <- raster::rasterize(restrict_sample, occ.out)

    occ.out <-  raster::mask(occ.out, cust)
    occ.out <-  raster::trim(occ.out)
  }

  # Distance raster calculation
  ## check if gazetteers are provided, replace by
  ## standard gazetteers if necessary
  if (verbose) {
    message("Calculating distance raster...")
  }
  if (is.null(gaz)) {
    warning("'gaz' not found, using standard gazetteers")

    gaz <- list(airports = sampbias::airports, cities = sampbias::cities,
                rivers = sampbias::waterbodies, roads = sampbias::roads)
  }

  ## check if gaz are provided as distance rasters, if so, this function will
  ## run much faster
  check <- all(sapply(gaz, is.raster))

  if (all(check)) {
    ## use input directly as distance raster
    dis.ras <- gaz
  } else {
    ## create distance raster for all gazeteers
    dis.ras <- dis_rast(gaz = gaz,
                        ras = occ.out,
                        buffer = buffer)
    dis.ras <- lapply(dis.ras, function(k) raster::crop(k, occ.out))
    dis.ras <- lapply(dis.ras, function(k) raster::mask(k, occ.out))

    ##  check if there are values in the distance raster
    if (all(is.na(values(dis.ras[[1]])))) {
      stop("No valid distances found. Consider setting terrestrial = F")
    }
  }

  # Plot the occurrence and distance rasters for diagnostics
  if(plot_raster){
   # plot(stack(dis.ras))
    plot(occ.out, main = "Occurrence raster")
    if(!is.null(restrict_sample)){
      plot(restrict_sample, add = TRUE)
    }
  }

  ## Check if a distance was found for any gazetteer, if not, only return
  ## occurrence raster
  if (verbose) {
    message("Estimating bias...")
  }
  if (is.logical(dis.ras)) {
    out <- c(Occurences = occ.out)
    class(out) <- append("sampbias", class(out))
    return(out)
  } else {
    # Generate the data.frame with the counts and distances
    dis.vec <- lapply(dis.ras, "getValues")
    dis.vec <- as.data.frame(do.call(cbind, dis.vec))
    names(dis.vec) <- names(dis.ras)

    dis.vec <- data.frame(cell_id = seq_len(nrow(dis.vec)),
                          record_count = getValues(occ.out),
                          dis.vec)
    dis.vec <- dis.vec[complete.cases(dis.vec),]

    rec_count <- c(sum(dis.vec$record_count == 0), sum(dis.vec$record_count > 0))

    out <- .RunSampBias(x = dis.vec,
                        rescale_distances = mcmc_rescale_distances,
                        iterations = mcmc_iterations,
                        burnin = mcmc_burnin,
                        prior_q = prior_q,
                        prior_w = prior_w,
                        outfile = mcmc_outfile,
                        run_null_model = run_null_model,
                        use_hyperprior = use_hyperprior)

    # create output file, a list of the class sampbias
    if (verbose) {
      message("Preparing output...")
    }

    # Generate the output object
    out <- list(summa = list(total_occ = nrow(x),
                             total_sp = length(unique(x$species)),
                             extent = extent(dum.ras),
                             res = res,
                             restrict_sample = restrict_sample,
                             rescale_distances = mcmc_rescale_distances,
                             data_availability = rec_count),
                occurrences = occ.out,
                bias_estimate = out,
                distance_rasters = stack(dis.ras))
    class(out) <- append("sampbias", class(out))

    if (verbose) {
      message(" Done\n")
    }

    return(out)
  }
}
