
library(sampbias)
library(cowplot)
library(ggplot2)
library(sp)
library(rgdal)
library(sf)
library(raster)
library(readr)

# rarefaction steps:
rar <- c(1, 0.5, 0.25, 0.1, 0.01, 0.001)
rar <- rar[6]
ID <- 5
res <- 0.05

#rarefaction of a randomly sampled data set across the study area without bias

## get a polygon of Borneo
data(landmass)

born2 <- st_read("empirical_analyses/Borneo.kml")
born2 <- sf:::st_zm(born2$geom)
born2 <- as(born2, 'Spatial')
born <- intersect(landmass, born2)


## randomly sample points across the island
occ <- sp::spsample(x = born, n = 100000, type = "random")
occ <- data.frame(species = letters[1:25],
                  coordinates(occ))
names(occ) = c("species", "decimalLongitude", "decimalLatitude")

# rarefaction loop for the bias calculation, write out the resulting paramter estimates and a map
for(i in 1:length(ID)){
  print(i)
  # subset the simulated data
  sub <- occ[base::sample(x = 1:nrow(occ), size = round(nrow(occ) * rar, 0)), ]

  # calculate sampling bias
  out <- calculate_bias(sub, res = res, buffer = 0.5, restrict_sample = born)

  # plot of bias projection in space
  proj <- project_bias(out)
  p2 <- map_bias(proj)

  ggsave(p2, filename = paste("empirical_analyses/simulations/figure_empirical_results_spatial_projection_simulated_",
                              ID[i], "_", rar, ".pdf", sep = ""),
         height = 16, width = 16)


  #prepare output files
  out <- out$bias_estimate
  out$ID <- ID[i]
  out$rar <- rar
  out$res <- res
  out$type <- "simulated"

  # write to disk
  write_csv(out, "empirical_analyses/simulations/weight_estimates.csv", append = TRUE)
}


