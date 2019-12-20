library(sampbias)
library(cowplot)

occ <-read.csv(system.file("extdata", "mammals_borneo.csv", package="sampbias"), sep = "\t")


# occ <- read.csv("original_data/mammals_borneo.csv", sep = "\t")
#
# occ <- select(occ, species, decimallatitude, decimallongitude, class)
#
# write.table(occ, file = "inst/extdata/mammals_borneo.csv", sep = "\t")
# occ <-read.csv("inst/extdata/mammals_borneo.csv", sep = "\t")

out <- calculate_bias(occ, res = 0.05, buffer = 0.5)

p1 <- plot(out)
summary(out)

ggsave(p1, filename = "ms_figures/figure_empirical_results.jpg", height = 8, width = 8)

proj <- project_bias(out)
p2 <- map_bias(proj)


ggsave(p1, filename = "ms_figures/figure_empirical_results.jpg", height = 8, width = 8)







