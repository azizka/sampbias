library(sampbias)
library(cowplot)
library(ggplot2)
library(sp)
library(rgdal)
library(readr)
library(sf)
library(raster)


# rarefaction steps:
rar <- c(1, 0.5, 0.1, 0.01)
rar <- rar[2]
ID <- 1:3
res <- 0.05

## get a polygon of Borneo
data(landmass)
data(borneo)

# rarefaction of the empirical data
occ <-read.csv(system.file("extdata", "mammals_borneo.csv", package="sampbias"), sep = "\t")

# rarefaction and bias calculation loop, write out a map and the bias estimates

for(i in 1:length(ID)){
  print(i)
  sub <- occ[base::sample(x = 1:nrow(occ), size = round(nrow(occ) * rar, 0)), ]
  out <- calculate_bias(sub, res = res, buffer = 2, restrict_sample = borneo)

  save(out, file = paste("empirical_analyses/simulations/empirical_rarefaction_results_",
                         round(nrow(occ) * rar, 0), "_", ID[i], ".rda", sep = ""))
  # plot of bias projection in space
  proj <- project_bias(out)
  p2 <- map_bias(proj)

  ggsave(p2, filename = paste("empirical_analyses/simulations/empirical_rarefaction_figure_rate_",
                              round(nrow(occ) * rar, 0), "_", ID[i], ".pdf", sep = ""),
         height = 16, width = 16)

  p3 <- map_bias(proj, type = "log_sampling_rate")

  ggsave(p3, filename = paste("empirical_analyses/simulations/empirical_rarefaction_figure_lograte_",
                              round(nrow(occ) * rar, 0), "_", ID[i], ".pdf", sep = ""),
         height = 16, width = 16)

  p4 <- map_bias(proj, type = "diff_to_max")

  ggsave(p4, filename = paste("empirical_analyses/simulations/empirical_rarefaction_figure_difftomax_",
                              round(nrow(occ) * rar, 0), "_", ID[i], ".pdf", sep = ""),
         height = 16, width = 16)


  # prepare bias estimate output
  out <- out$bias_estimate
  out$ID <- ID[i]
  out$rar <- round(nrow(occ) * rar, 0)
  out$res <- res
  out$type <- "empirical"

  # write to disk
    write_csv(out,
              paste("empirical_analyses/simulations/empirical_rarefaction_weightestimates_",
                    round(nrow(occ) * rar, 0), "_", ID[i], ".csv", sep = ""))
}
