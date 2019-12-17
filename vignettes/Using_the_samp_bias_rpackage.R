## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE, eval = TRUE, warning = FALSE, message = FALSE)

## ---- eval = FALSE-------------------------------------------------------
#  require(devtools)
#  install_github(repo = "azizka/sampbias")
#  library(sampbias)
#  library(maptools)

## ---- echo = FALSE-------------------------------------------------------
library(sampbias)
library(maptools)

## ------------------------------------------------------------------------
#loading a text file to R
occ <-read.csv(system.file("extdata", 
                           "mammals_borneo.csv", 
                           package="sampbias"),
               sep = "\t")

## ------------------------------------------------------------------------
cit <- readShapeSpatial(system.file("extdata", 
                                    "Borneo_major_cities.shp", 
                                    package="sampbias"))
roa <- readShapeSpatial(system.file("extdata", 
                                    "Borneo_major_roads.shp", 
                                    package="sampbias"))

gazetteers <- list(cities = cit,
                   roads = roa)

## ------------------------------------------------------------------------
bias.out <- calculate_bias(x = occ, gaz = gazetteers)

## ---- eval = FALSE-------------------------------------------------------
#  summary(bias.out)
#  plot(bias.out)

## ------------------------------------------------------------------------
proj <- project_bias(bias.out)
map_bias(proj)

