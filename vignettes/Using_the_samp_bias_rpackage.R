## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE, eval = TRUE, warning = FALSE, message = FALSE, fig.pos = 'H')

## ---- eval = FALSE------------------------------------------------------------
#  require(devtools)
#  install_github(repo = "azizka/sampbias")
#  library(sampbias)
#  library(maptools)
#  library(rgdal)
#  library(raster)

## ---- echo = FALSE------------------------------------------------------------
library(sampbias)
library(maptools)
library(rgdal)
library(raster)

## -----------------------------------------------------------------------------
#loading a text file to R
occ <-read.csv(system.file("extdata", 
                           "mammals_borneo.csv", 
                           package="sampbias"),
               sep = "\t")

## ---- warning = FALSE, message = FALSE----------------------------------------
cit <- readOGR(dsn = system.file("extdata", package="sampbias"), 
               layer = "Borneo_major_cities", verbose = FALSE)
roa <- readOGR(dsn = system.file("extdata", package="sampbias"), 
               layer = "Borneo_major_roads", verbose = FALSE)

gazetteers <- list(cities = cit,
                   roads = roa)

## -----------------------------------------------------------------------------
bias.out <- calculate_bias(x = occ, gaz = gazetteers)

## ---- eval = TRUE, fig.height=7, fig.cap = "Output of a sampbias analysis. A) Posterior estimates of the bias weights for cities and roads, B) the decay of the sampling rate with increasing distance from cities and roads."----
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

bias.out <- calculate_bias(x = occ, 
                           gaz = gazetteers, 
                           restrict_sample = area_example)

## ---- fig.cap="Example of a more complex customized study area representing the Borneo montane rain forests ecoregion from Olson et al. (2001). Only grid cells within the red area are included in the sampling bias calculation."----
data(ecoregion_example)

plot(borneo)
plot(ecoregion_example, col = "red", add = TRUE)

bias.out <- calculate_bias(x = occ, 
                           gaz = gazetteers, 
                           restrict_sample = ecoregion_example)

## ---- eval = FALSE------------------------------------------------------------
#  #projection
#  wgs84 <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
#  
#  # an example for an equal area raster
#  data(ea_raster)
#  
#  # reproject the occurrence coordinates
#  ## select coordinates from the occ data.frame and create spatial object
#  ea_occ <- SpatialPoints(occ[,c(3,2)],
#                          proj4string = wgs84)
#  ## transform to the same CRS as the equal area grid
#  ea_occ <- spTransform(ea_occ, CRSobj = proj4string(ea_raster))
#  ## retransform into a data.frame
#  ea_occ <- data.frame(species = occ[,1], coordinates(ea_occ))
#  
#  # reproject gazetteers
#  ## set the CRS in case it is not defined. Make sure to know the correct CRS.
#  proj4string(gazetteers[[1]]) <-
#    proj4string(gazetteers[[2]]) <-
#    wgs84
#  
#  #transform to the new CRS
#  ea_gaz <- lapply(gazetteers, "spTransform", CRSobj = proj4string(ea_raster))
#  
#  # run sampbias
#  ea_bias <- calculate_bias(x = ea_occ,
#                             gaz = ea_gaz,
#                             inp_raster = ea_raster)

