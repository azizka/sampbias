## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE, 
                      warning = FALSE, 
                      message = FALSE, 
                      fig.pos = 'H',
                      eval = TRUE)

## ---- eval = FALSE------------------------------------------------------------
#  require(devtools)
#  install_github(repo = "azizka/sampbias")

## ----load_packages------------------------------------------------------------
library(sampbias)
library(terra)

## -----------------------------------------------------------------------------
#loading a text file to R
occ <- read.csv(system.file("extdata", 
                           "mammals_borneo.csv", 
                           package = "sampbias"),
               sep = "\t")

## ---- warning = FALSE, message = FALSE----------------------------------------
cit <- terra::vect(system.file("extdata/Borneo_major_cities.shp",
                               package = "sampbias"))
roa <- terra::vect(system.file("extdata/Borneo_major_roads.shp",
                               package = "sampbias"))

gazetteers <- list(cities = cit,
                   roads = roa)

## ---- message=FALSE-----------------------------------------------------------
bias.out <- calculate_bias(x = occ, gaz = gazetteers)

## ---- fig.height=7, fig.cap = "Output of a sampbias analysis. A) Posterior estimates of the bias weights for cities and roads, B) the decay of the sampling rate with increasing distance from cities and roads."----
summary(bias.out)
plot(bias.out)

## ---- fig.cap = "Sampling bias effect projected in space. Note the coarse grid resolution which should be increased."----
proj <- project_bias(bias.out)
map_bias(proj, type = "log_sampling_rate")

## ---- fig.cap="Example of a customized study area which can be provided to the calculate_bias function via the restrict_sample option. Only grid cells within the red areas are included in the sampling bias calculation."----
data(area_example)
data(borneo)

plot(borneo)
plot(area_example, col = "red", add = TRUE)


## ---- message = FALSE---------------------------------------------------------
bias.out <- calculate_bias(x = occ, 
                           gaz = gazetteers, 
                           restrict_sample = area_example)


## ---- fig.cap="Example of a more complex customized study area representing the Borneo montane rain forests ecoregion from Olson et al. (2001). Only grid cells within the red area are included in the sampling bias calculation."----
data(ecoregion_example)

plot(borneo)
plot(ecoregion_example, col = "red", add = TRUE)


## ---- message = FALSE---------------------------------------------------------
bias.out <- calculate_bias(x = occ, 
                           gaz = gazetteers, 
                           restrict_sample = ecoregion_example)

## ---- eval = FALSE------------------------------------------------------------
#  #projection
#  wgs84 <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
#  
#  # an example for an equal area raster
#  data(ea_raster)
#  ea_raster <- terra::unwrap(ea_raster)
#  
#  # reproject the occurrence coordinates
#  ## select coordinates from the occ data.frame and create spatial object
#  ea_occ <- terra::vect(occ, geom = c("decimalLongitude", "decimalLatitude"),
#                        crs="+proj=longlat +datum=WGS84")
#  ## transform to the same CRS as the equal area grid
#  ea_occ <- terra::project(ea_occ, terra::crs(ea_raster))
#  
#  ## retransform into a data.frame
#  ea_occ <- data.frame(species = occ[, 1],
#                       terra::crds(ea_occ))
#  
#  # reproject gazetteers
#  ## set the CRS in case it is not defined. Make sure to know the correct CRS.
#  terra::crs(gazetteers[[1]]) <-
#    terra::crs(gazetteers[[2]]) <-
#    wgs84
#  
#  #transform to the new CRS
#  ea_gaz <- lapply(gazetteers, terra::project,
#                   y = terra::crds(ea_raster))
#  
#  # run sampbias
#  ea_bias <- calculate_bias(x = ea_occ,
#                             gaz = ea_gaz,
#                             inp_raster = ea_raster)

