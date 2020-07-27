library(sampbias)
library(cowplot)
library(ggplot2)
library(sp)
library(rgdal)
library(readr)
library(sf)
library(raster)


# rarefaction steps:
rar <- c(1, 0.5, 0.25, 0.1, 0.01)
rar <- rar[5]
ID <- 1:5
res <- 0.05

## get a polygon of Borneo
data(landmass)

born2 <- st_read("empirical_analyses/Borneo.kml")
born2 <- sf:::st_zm(born2$geom)
born2 <- as(born2, 'Spatial')
born <- intersect(landmass, born2)

# rarefaction of the empirical data
occ <-read.csv(system.file("extdata", "mammals_borneo.csv", package="sampbias"), sep = "\t")

# rarefaction and bias calculation loop, write out a map and the bias estimates

for(i in 1:length(ID)){
  print(i)
  sub <- occ[base::sample(x = 1:nrow(occ), size = round(nrow(occ) * rar, 0)), ]
  out <- calculate_bias(sub, res = res, buffer = 2, restrict_sample = born)

  # plot of bias projection in space
  proj <- project_bias(out)
  p2 <- map_bias(proj)

  ggsave(p2, filename = paste("empirical_analyses/simulations/figure_empirical_results_spatial_projection_empirical_",
                              rar, "_", ID[i], ".pdf", sep = ""),
         height = 16, width = 16)


  # prepare bias estimate output
  out <- out$bias_estimate
  out$ID <- ID[i]
  out$rar <- rar
  out$res <- res
  out$type <- "empirical"

  # write to disk
  if(ID[i] == 1 & rar == 0.001){
    write_csv(out, "empirical_analyses/simulations/weight_estimates.csv", append = FALSE)
  }else{
    write_csv(out, "empirical_analyses/simulations/weight_estimates.csv", append = TRUE)
  }
}
