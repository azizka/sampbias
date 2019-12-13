library(sampbias)
library(cowplot)

occ <-read.csv(system.file("extdata", "mammals_borneo.csv", package="sampbias"), sep = "\t")

out <- calculate_bias(occ, res = 0.1)

p1 <- plot(out)
summary(out)

proj <- project_bias(out)
p2 <- map_bias(proj)


plot_out <- plot_grid(p1, p2, labels = c('A', 'B'), ncol = 1, rel_heights = c(1, 2))

ggsave(plot_out, filename = "ms_figures/figure_empirical_results.jpg", height = 10, width = 8)
