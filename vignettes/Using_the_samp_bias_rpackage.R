## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE, eval = TRUE, warning = FALSE, message = FALSE)

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

## -----------------------------------------------------------------------------
cit <- readOGR(dsn = system.file("extdata", package="sampbias"), 
               layer = "Borneo_major_cities")
roa <- readOGR(dsn = system.file("extdata", package="sampbias"), 
               layer = "Borneo_major_roads")

gazetteers <- list(cities = cit,
                   roads = roa)

## -----------------------------------------------------------------------------
bias.out <- calculate_bias(x = occ, gaz = gazetteers)

## ---- eval = FALSE------------------------------------------------------------
#  summary(bias.out)
#  plot(bias.out)

## -----------------------------------------------------------------------------
proj <- project_bias(bias.out)
map_bias(proj)

## -----------------------------------------------------------------------------
data(area_example)
borneo <- crop(sampbias::landmass, extent(108, 120, -5, 7))

plot(borneo)
plot(area_example, col = "red", add = TRUE)

bias.out <- calculate_bias(x = occ, 
                           gaz = gazetteers, 
                           restrict_sample = area_example)


## -----------------------------------------------------------------------------
data(ecoregion_example)

plot(borneo)
plot(ecoregion_example, col = "red", add = TRUE)

bias.out <- calculate_bias(x = occ, 
                           gaz = gazetteers, 
                           restrict_sample = ecoregion_example,
                           plot_raster = TRUE)

## -----------------------------------------------------------------------------
# an example for an equal area raster
data(ea_raster)

# reproject the occurrence coordinate
## select coordinates from the occ data.frame and create spatial object
ea_occ <- SpatialPoints(occ[,c(3,2)], 
                        proj4string = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))
## transform to the same CRS as the raster
ea_occ <- spTransform(ea_occ, CRSobj = proj4string(ea_raster))
## retransform into a data.frame
ea_occ <- data.frame(species = occ[,1], coordinates(ea_occ))

# reproject gazetteers
## set the CRS in case it is not defined. Make sure to know the correct CRS.
proj4string(gazetteers[[1]]) <- proj4string(gazetteers[[2]]) <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")

#transform to the new crs
ea_gaz <- lapply(gazetteers, "spTransform", CRSobj = proj4string(ea_raster))

# run sampbias
ea_bias <- calculate_bias(x = ea_occ, 
                           gaz = ea_gaz, 
                           inp_raster = ea_raster)
summary(ea_bias)


