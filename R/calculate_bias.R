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
#'   occurrence record per line, and at least three columns, named
#'   \sQuote{species}, \sQuote{decimalLongitude}, and \sQuote{decimalLatitude}.
#' @param gaz a list of geographic gazetteers as \code{SpatVector} or \code{sf}.
#'   If NULL, a set of default gazetteers, representing large scale occurrence
#'   of airports, cities, rivers, and roads is used. See Details.
#' @param res numerical.  The raster resolution for the distance calculation to
#'   the geographic features and the data visualization, in decimal degrees. The
#'   default is to one degree, but higher resolution will be desirable for most
#'   analyses. \code{res} together with the extent of the input data determine
#'   computation time and memory requirements.
#' @param buffer numerical.  The size of the geographic buffer around the extent
#'   of \code{ras} for the distance calculations in degrees, to account for
#'   geographic structures neighbouring the study area (such as a road right
#'   outside the study area). Should be a multiple of \code{res}.  Default is to
#'   \code{res} * 10. See Details.
#' @param restrict_sample a \code{SpatVector} object. If provided the area for
#'   the bias test will be restricted to raster cells within these polygons (and
#'   the extent of the sampled points in x). Make sure to use adequate values
#'   for \code{res}. Default = NULL.
#' @param terrestrial logical.  If TRUE, the empirical distribution (and the
#'   output maps) are restricted to terrestrial areas.  Uses the
#'   \code{rnaturalearth:::ne_countries} to define what is
#'   terrestrial.  Default = TRUE.
#' @param inp_raster an object of class \code{SpatRaster}. A template raster for
#'   the counts and distance calculation. Can be used to provide a special
#'   resolution, or for different coordinate reference systems. See vignette.
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
#' @param verbose logical.  If TRUE, progress is reported.  Default = FALSE.
#' @param run_null_model logical. Run a null model with bias weights set to
#'   zero.
#' @param use_hyperprior logical. If TRUE a hyperprior on the bias weights is
#'   used for regularization to avoid over-parametrization.
#' @return An object of the S3-class \sQuote{sampbias}, which is a list
#' including the following objects: \item{summa}{A list of summary statistics
#' for the sampbias analyses, including the total number of occurrence points
#' in \code{x}, the total number of species in \code{x}, the extent of the
#' output rasters as well as the settings for \code{res}, \code{binsize}, and
#' \code{convexhull} used in the analyses.} \item{occurrences}{a \code{SpatRaster}
#' indicating occurrence records per grid cell, with resolution res.}
#' \item{species}{a \code{SpatRaster} with indicating the number of species per
#' grid cell, with resolution res.} \item{biasmaps}{a list of \code{SpatRaster},
#' with the same length as gaz. Each element is the spatial projection of the
#' bias effect for a sources of bias in \code{gaz}. The last raster in the list
#' is the average over all bias sources.} \item{biastable}{a \code{data.frame},
#' with the estimated bias effect for each bias source in \code{gaz}, at the
#' distances specified by \code{biasdist}.}
#' @note Check \url{https://github.com/azizka/sampbias/wiki} for a tutorial on
#'   sampbias.
#' @seealso \code{\link{summary.sampbias}} \code{\link{is.sampbias}}
#' \code{\link{plot.sampbias}}
#' @keywords maths spatial
#' @examples
#' \dontrun{
#'   #simulate data
#'   x <- data.frame(species = rep(sample(x = LETTERS, size = 5), times = 20),
#'                    decimalLongitude = runif(n = 100, min = 0, max = 20),
#'                    decimalLatitude = runif(n = 100, min = -4, max = 4))
#'
#'   out <- calculate_bias(x, terrestrial = TRUE, buffer = 0)
#'   summary(out)
#'   plot(out)
#'   
#'   
#'   
#' }
#' @export
#' @importFrom terra vect rast ext crs<- values mask res<- rasterize trim plot
#'   crop ncell crs
#' @importFrom rlang .data
#' @importFrom stats complete.cases
#' @importFrom sf st_transform st_crop
#' @importFrom rnaturalearth ne_download ne_file_name ne_countries ne_coastline
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
                          verbose = FALSE,
                          run_null_model = FALSE,
                          use_hyperprior = TRUE) {

  #convert x to SpatialPoints
  coords <- c("decimalLongitude", "decimalLatitude")
  dat.pts <- terra::vect(x[, coords], geom = coords)

  # create dummy raster if no raster is supplied
  if (!is.null(inp_raster)) {
    dum.ras <- inp_raster
  } else {
    dum.ras <- terra::rast(round(terra::ext(dat.pts),
                                 .DecimalPlaces(res)))
    res(dum.ras) <- res
  }

  # warning if combination of resolution and extent exceed 1mio grid cells
  if (terra::ncell(dum.ras) > 1e+06) {
    warning("Huge raster size")
  }

  # occurrence raster
  if (verbose) {
    message("Creating occurrence raster...")
  }
  occ.out <- .OccRast(x = dat.pts, ras = dum.ras)

  # Trim raster to remove outer NA cells
  if (is.null(inp_raster)) {
    occ.out <- terra::trim(occ.out)
  }

  #Replace NAs
  occ.out[is.na(occ.out)] <- 0

  ## adapt occurrence raster to terrestrial surface
  if (terrestrial) {
    if (verbose) {
      message("Adjusting to terrestrial surface...")
    }
    
    ## Get landmass
    if (verbose) {
      message("Downloadinf landmass shapefile...")
    }
    
    landmass <-
      try(suppressWarnings(rnaturalearth::ne_countries(returnclass = "sf")),
          silent = TRUE)
    if (inherits(landmass, "try-error")) { 
      stop(paste("landmass could not be downloaded using rnaturalearth."),
           "Please, check your internet connection.")
      }
    landmass <- terra::vect(landmass)
    if (!is.null(inp_raster)) {
      wrld <- terra::project(landmass, inp_raster)
      wrld <- terra::crop(wrld, inp_raster)
      wrld <- terra::rasterize(wrld, occ.out)
      
    }else{
      wrld <- terra::crop(landmass, occ.out)
      wrld <- terra::rasterize(wrld, occ.out)
    }
    ## adjust raster
    occ.out <-  terra::mask(occ.out, wrld)
    occ.out <-  terra::trim(occ.out)
    if (all(is.na(terra::values(occ.out)))) {
      stop("No points left after keeping ")
    }
    occ.out[is.na(occ.out)] <- 0
  }

  ## Adapt occurrence raster to custom study extent if provided
  if (!is.null(restrict_sample)) {
    restrict_sample <- terra::vect(restrict_sample)
    if (verbose) {
      message("Adjusting to custom study extent...")
    }

    restrict_sample <- terra::crop(restrict_sample, terra::ext(occ.out))
    cust <- terra::rasterize(restrict_sample, occ.out)

    occ.out <-  terra::mask(occ.out, cust)
    occ.out <-  terra::trim(occ.out)
  }

  # Distance raster calculation
  ## check if gazetteers are provided, replace by
  ## standard gazetteers if necessary
  if (verbose) {
    message("Calculating distance raster...")
  }
  if (is.null(gaz)) {
    if (verbose) {
      message(
          "'gaz' not found, using standard gazetteers.",
          "Gazetteers will be downloaded using rnaturalearth package."
      )
    }
    
    gaz_type <- c("roads",
                  "urban_areas", 
                  "rivers_lake_centerlines",
                  "airports")
    category <- c("cultural", "physical")[c(1, 1, 2, 1)]
    scales <- "large"
    names_gaz <- c("roads", "cities", "waterbodies", "airports")
    gaz <- list()
    counter <- 0
    for (i in seq_along(gaz_type)) {
      ref <-
        try(suppressWarnings(terra::vect(
          rnaturalearth::ne_download(
            scale = scales,
            type = gaz_type[i],
            category = category[i],
            returnclass = "sf"
          )
        )),
        silent = TRUE)
      if (inherits(ref, "try-error")) {
        a <- paste("Gazetteer for", gaz_type[i], "not found at\n%s")
        warning(sprintf(
          a,
          rnaturalearth::ne_file_name(
            scale = scales,
            type = gaz_type[i],
            full_url = TRUE
          )
        ))
        warning(paste("Skipping", gaz_type[i], "from bias analysis"))
      } else {
        counter <- counter + 1
        gaz[[counter]] <- ref
        names(gaz)[[counter]] <- names_gaz[[counter]]
      }
    }
    if (length(gaz) == 0) {
      stop(
        paste(
          "Gazetteers could not be downloaded using rnaturalearth package.",
          "Check your internet connection or input the gazetteers using",
          "the argument gaz."
        )
      )
    }
    
  }

  ## check if gaz are provided as distance rasters, if so, this function will
  ## run much faster
  check <- all(sapply(gaz, function(x){inherits(x, "SpatRaster")}))

  if (all(check)) {
    ## use input directly as distance raster
    dis.ras <- gaz
  } else {
    ## create distance raster for all gazeteers
    dis.ras <- dis_rast(gaz = gaz,
                        ras = occ.out,
                        buffer = buffer)
    dis.ras <-
      lapply(dis.ras, function(k) {
        terra::crs(k) <- terra::crs(occ.out)
        k
      })
    dis.ras <- lapply(dis.ras, function(k) terra::crop(k, occ.out))
    dis.ras <- lapply(dis.ras, function(k) terra::mask(k, occ.out))

    ##  check if there are values in the distance raster
    if (all(is.na(values(dis.ras[[1]])))) {
      stop("No valid distances found. Consider setting terrestrial = F")
    }
  }
  ## Name rasters
  if (is.null(names(dis.ras))) {
    names(dis.ras) <- paste0("X", seq_along(dis.ras))
  }
  
  # Plot the occurrence and distance rasters for diagnostics
  if (plot_raster) {
    terra::plot(occ.out, main = "Occurrence raster")
    if (!is.null(restrict_sample)) {
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
    dis.vec <- lapply(dis.ras, terra::values)
    dis.vec <- as.data.frame(do.call(cbind, dis.vec))
    names(dis.vec) <- names(dis.ras)

    dis.vec <- data.frame(cell_id = seq_len(nrow(dis.vec)),
                          record_count = terra::values(occ.out)[, 1],
                          dis.vec)
    dis.vec <- dis.vec[stats::complete.cases(dis.vec),]

    rec_count <- c(sum(dis.vec$record_count == 0), 
                   sum(dis.vec$record_count > 0))

    out <- .RunSampBias(x = dis.vec,
                        rescale_distances = mcmc_rescale_distances,
                        iterations = mcmc_iterations,
                        burnin = mcmc_burnin,
                        prior_q = prior_q,
                        prior_w = prior_w,
                        outfile = mcmc_outfile,
                        run_null_model = run_null_model,
                        use_hyperprior = use_hyperprior,
                        verbose = verbose)

    # create output file, a list of the class sampbias
    if (verbose) {
      message("Preparing output...")
    }

    # Generate the output object
    out <- list(summa = list(total_occ = nrow(x),
                             total_sp = length(unique(x$species)),
                             extent = terra::ext(dum.ras),
                             res = res,
                             restrict_sample = restrict_sample,
                             rescale_distances = mcmc_rescale_distances,
                             data_availability = rec_count),
                occurrences = occ.out,
                bias_estimate = out,
                distance_rasters = terra::rast(dis.ras))
    class(out) <- append("sampbias", class(out))

    if (verbose) {
      message(" Done\n")
    }

    return(out)
  }
}
