library(sampbias)
library(cowplot)
library(ggplot2)
library(sp)
library(rgdal)


# rarefaction steps:
rar <- c(1, 0.5, 0.25, 0.1, 0.01, 0.001)
ID <- 1
res <- 0.05


# rarefaction of the empirical data
occ <-read.csv(system.file("extdata", "mammals_borneo.csv", package="sampbias"), sep = "\t")

# rarefaction and bias calculation loop, write out a map and the bias estiamtes

for(i in 1:length(rar)){
  print(i)
  sub <- occ[base::sample(x = 1:nrow(occ), size = round(nrow(occ) * rar[i], 0)), ]
  out <- calculate_bias(occ, res = res, buffer = 0.5)
  out$ID <- ID
  out$rar <- rar[i]
  out$res <- res
  out$type <- "empirical"

  if(ID == 1 & i == 1){
    write_csv(out, "empirical_analyses/weight_estimates.csv", append = FALSE)
  }else{
    write_csv(out, "empirical_analyses/weight_estimates.csv", append = TRUE)
  }

  proj <- project_bias(out)
  p2 <- map_bias(proj)

  ggsave(p2, filename = paste("empirical_analyses/figure_empirical_results_spatial_projection_empirical_", ID, "_", rar[i], ".pdf", sep = ""), height = 16, width = 16)

  write
}


#rarefaction of a randomly sampled dataset across the study area without bias

## get a polygon of borneo
born <- readOGR(dsn = "empirical_analyses", layer = "ne_110m_land")

STILL NEED TO CROP

## randomly sample points across teh island
occ <- sp::spsample(x = born, n = 100000)

# rarefaction loop for the bias calculation, write out the resulting paramter estimates and a map

for(i in 1:length(rar)){
  print(i)
  sub <- occ[base::sample(x = 1:nrow(occ), size = round(nrow(occ) * rar[i], 0)), ]
  out <- calculate_bias(occ, res = res, buffer = 0.5)
  out$ID <- ID
  out$rar <- rar[i]
  out$res <- res
  out$type <- "simulated"

  if(ID == 1 & i == 1){
    write_csv(out, "empirical_analyses/weight_estimates.csv", append = FALSE)
  }else{
    write_csv(out, "empirical_analyses/weight_estimates.csv", append = TRUE)
  }

  proj <- project_bias(out)
  p2 <- map_bias(proj)

  ggsave(p2, filename = paste("empirical_analyses/figure_empirical_results_spatial_projection_empirical_", ID, "_", rar[i], ".pdf", sep = ""), height = 16, width = 16)
}


