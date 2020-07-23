
library(sampbias)
library(cowplot)
library(ggplot2)
library(sp)
library(rgdal)
library(sf)
library(raster)


# rarefaction steps:
rar <- c(1, 0.5, 0.25, 0.1, 0.01, 0.001, 0.0001)
ID <- 1
res <- 0.05

#rarefaction of a randomly sampled dataset across the study area without bias

## get a polygon of borneo
data(landmass)

born2 <- st_read("empirical_analyses/Borneo.kml")
born2 <- sf:::st_zm(born2$geom)
born2 <- as(born2, 'Spatial')
born <- intersect(born, born2)


## randomly sample points across teh island
occ <- sp::spsample(x = born, n = 100000, type = "random")
occ <- data.frame(species = letters[1:25],
                  coordinates(occ))
names(occ) = c("species", "decimalLongitude", "decimalLatitude")

# rarefaction loop for the bias calculation, write out the resulting paramter estimates and a map
for(i in 1:length(rar)){
  print(i)
  # subset the simulated data
  sub <- occ[base::sample(x = 1:nrow(occ), size = round(nrow(occ) * rar[i], 0)), ]

  # calculate sampling bias
  out <- calculate_bias(occ, res = res, buffer = 0.5)

  # plot of bias projection in space
  proj <- project_bias(out)
  p2 <- map_bias(proj)

  ggsave(p2, filename = paste("empirical_analyses/simulations/figure_empirical_results_spatial_projection_simulated_",
                              ID, "_", rar[i], ".pdf", sep = ""),
         height = 16, width = 16)


  #preapre output files
  out <- out$bias_estimate
  out$ID <- ID
  out$rar <- rar[i]
  out$res <- res
  out$type <- "simulated"

  # write to disk
  write_csv(out, "empirical_analyses/weight_estimates.csv", append = TRUE)
}


