library(sampbias)
library(cowplot)
library(ggplot2)
library(sp)
library(sf)
library(rgdal)

occ <-read.csv(system.file("extdata", "mammals_borneo.csv", package="sampbias"), sep = "\t")

## get a polygon of Borneo
data(landmass)

born2 <- st_read("empirical_analyses/Borneo.kml")
born2 <- sf:::st_zm(born2$geom)
born2 <- as(born2, 'Spatial')
born <- intersect(landmass, born2)


# occ <- read.csv("original_data/mammals_borneo.csv", sep = "\t")
#
# occ <- select(occ, species, decimallatitude, decimallongitude, class)
#
# write.table(occ, file = "inst/extdata/mammals_borneo.csv", sep = "\t")
# occ <-read.csv("inst/extdata/mammals_borneo.csv", sep = "\t")

out <- calculate_bias(occ, res = 0.05, buffer = 0.5, restrict_sample = born)

p1 <- plot(out)
summary(out)

ggsave(p1, filename = "ms_figures/figure_empirical_results.jpg", height = 8, width = 8)

proj <- project_bias(out)
p2 <- map_bias(proj)


ggsave(p2, filename = "ms_figures/figure_empirical_results_spatial_projection.pdf", height = 16, width = 16)







