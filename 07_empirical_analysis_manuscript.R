library(sampbias)
library(cowplot)
library(ggplot2)
library(sp)
library(sf)
library(rgdal)

# generate input example data
# occ <-read.csv(system.file("extdata", "mammals_borneo.csv", package="sampbias"), sep = "\t")

# occ <- read.csv("original_data/mammals_borneo.csv", sep = "\t")
#
# occ <- select(occ, species, decimallatitude, decimallongitude, class)
#
# write.table(occ, file = "inst/extdata/mammals_borneo.csv", sep = "\t")
# occ <-read.csv("inst/extdata/mammals_borneo.csv", sep = "\t")

## get a polygon of Borneo
data(landmass)
data(borneo)

occ<- read.csv(system.file("extdata",
                                   "mammals_borneo.csv",
                                   package="sampbias"),
                       sep = "\t")

out <- calculate_bias(occ, res = 0.05,
                      buffer = 0.5,
                      restrict_sample = borneo)

p1 <- plot(out)
summary(out)

ggsave(p1, filename = "ms_figures/figure_empirical_results.jpg", height = 8, width = 8)

proj <- project_bias(out)
p2 <- map_bias(proj, type = "diff_to_max")


ggsave(p2, filename = "ms_figures/figure_empirical_projection_difftomax.pdf", height = 16, width = 16)

p3 <- map_bias(proj, type= "sampling_rate")

ggsave(p3, filename = "ms_figures/figure_empirical_projection_samplingrate.pdf", height = 16, width = 16)


p4 <- map_bias(proj, type= "log_sampling_rate")

ggsave(p4, filename = "ms_figures/figure_empirical_projection_logsamplingrate.pdf", height = 16, width = 16)





