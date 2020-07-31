
library(sampbias)
library(cowplot)
library(ggplot2)
library(sp)
library(rgdal)
library(sf)
library(raster)
library(readr)

# rarefaction steps:
rar <- c(100000,
         25000,
         6262,
         6262 * 0.5,
         round(6262 * 0.25, 0),
         round(6262 * 0.1, 0),
         round(6262 * 0.01, 0))
rar <- rar[7]
ID <- 1
res <- 0.1#0.05

#rarefaction of a randomly sampled data set across the study area without bias

## get a polygon of Borneo
data(landmass)

born2 <- st_read("empirical_analyses/Borneo.kml")
born2 <- sf:::st_zm(born2$geom)
born2 <- as(born2, 'Spatial')
born <- intersect(landmass, born2)

# rarefaction loop for the bias calculation, write out the resulting parameter estimates and a map
for(i in 1:length(ID)){
  print(i)
  # randomly sample points across the island
  occ <- sp::spsample(x = born, n = rar, type = "random")
  occ <- data.frame(species = "A",
                    coordinates(occ))
  names(occ) = c("species", "decimalLongitude", "decimalLatitude")

  # calculate sampling bias
  out <- calculate_bias(occ, res = res, buffer = 0.5, restrict_sample = born, mcmc_rescale_distances = 1000)

  # plot of bias projection in space
  proj <- project_bias(out)
  p2 <- map_bias(proj)

  ggsave(p2, filename = paste("empirical_analyses/simulations/rescale1000_figure_empirical_results_spatial_projection_simulated_",
                              res, "_", rar, "_", ID[i], ".pdf", sep = ""),
         height = 16, width = 16)


  p3 <- map_bias(proj, sampling_rate = TRUE)

  ggsave(p3, filename = paste("empirical_analyses/simulations/rescale1000_figure_empirical_results_spatial_projection_simulated_",
                              res, "_", rar, "_", ID[i], "_sampling_rate.pdf", sep = ""),
         height = 16, width = 16)

  #prepare output files
  out <- out$bias_estimate
  out$ID <- ID[i]
  out$rar <- rar
  out$res <- res
  out$type <- "simulated"
  out$model <- "full"

  # write to disk
  write_csv(out, paste("empirical_analyses/simulations/rescale1000_weight_estimates_simulated_",
                       res, "_", rar, "_", ID[i], ".csv", sep = ""))


################rescale change
  # calculate sampling bias
  out <- calculate_bias(occ, res = res, buffer = 0.5, restrict_sample = born, mcmc_rescale_distances = 100000)

  # plot of bias projection in space
  proj <- project_bias(out)
  p2 <- map_bias(proj)

  ggsave(p2, filename = paste("empirical_analyses/simulations/rescale100000_figure_empirical_results_spatial_projection_simulated_",
                              res, "_", rar, "_", ID[i], ".pdf", sep = ""),
         height = 16, width = 16)


  p3 <- map_bias(proj, sampling_rate = TRUE)

  ggsave(p3, filename = paste("empirical_analyses/simulations/rescale100000_figure_empirical_results_spatial_projection_simulated_",
                              res, "_", rar, "_", ID[i], "_sampling_rate.pdf", sep = ""),
         height = 16, width = 16)

  #prepare output files
  out <- out$bias_estimate
  out$ID <- ID[i]
  out$rar <- rar
  out$res <- res
  out$type <- "simulated"
  out$model <- "full"

  # write to disk
  write_csv(out, paste("empirical_analyses/simulations/rescale100000_weight_estimates_simulated_",
                       res, "_", rar, "_", ID[i], ".csv", sep = ""))

  # run with null model
#
#   out <- calculate_bias(occ, res = res, buffer = 0.5, restrict_sample = born, run_null_model = TRUE)
#
#   # plot of bias projection in space
#   proj <- project_bias(out)
#   p2 <- map_bias(proj)
#
#   ggsave(p2, filename = paste("empirical_analyses/simulations/figure_empirical_results_spatial_projection_simulated_",
#                               res, "_", rar, "_", ID[i], "_null.pdf", sep = ""),
#          height = 16, width = 16)
#
#
#   p3 <- map_bias(proj, sampling_rate = TRUE)
#
#   ggsave(p3, filename = paste("empirical_analyses/simulations/figure_empirical_results_spatial_projection_simulated_",
#                               res, "_", rar, "_", ID[i], "_null_sampling_rate.pdf", sep = ""),
#          height = 16, width = 16)
#
#   #prepare output files
#   out <- out$bias_estimate
#   out$ID <- ID[i]
#   out$rar <- rar
#   out$res <- res
#   out$type <- "simulated"
#   out$model <- "null"
#
#   # write to disk
#   write_csv(out, paste("empirical_analyses/simulations/weight_estimates_simulated_",
#                        res, "_", rar, "_", ID[i], "_null.csv", sep = ""))

}


