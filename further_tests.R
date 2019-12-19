# additional tests


# a dataset of occurrence from plants of Burkina Faso

library(sampbias)
library(cowplot)
library(magrittr)
library(dplyr)
library(raster)
library(ggplot2)
library(magrittr)
library(sp)
library(dplyr)
library(viridis)

occ <-read.csv("original_data/bfa_0026301-191105090559680.csv", sep = "\t")

occ <- occ %>%
  filter(!is.na(decimalLongitude)) %>%
  filter(decimalLongitude > -8 & decimalLongitude < 5) %>%
  filter(decimalLatitude > 9)


out <- calculate_bias(occ, res = 0.1, buffer = 1)

p1 <- plot(out)
summary(out)

proj <- project_bias(out)
p2 <- map_bias(proj)


plot_out <- plot_grid(p1, p2, labels = c('A', 'B'), ncol = 1, rel_heights = c(1, 2))

ggsave(plot_out, filename = "ms_figures/figure_empirical_results.jpg", height = 10, width = 8)



data(airports)
data(waterbodies)
data(cities)
data(roads)
data(landmass)

# prepare for plotting
airp <- airports %>% crop(SpatialPoints(occ[,23:22])) %>%  coordinates() %>%  data.frame() %>% mutate(type = "Airport")
cit <- cities %>% crop(SpatialPoints(occ[,23:22])) %>%  coordinates() %>%  data.frame() %>%  mutate(type = "City")
pts <- bind_rows(cit, airp)


riv <- waterbodies %>% crop(SpatialPoints(occ[,23:22])) %>%  fortify() %>%  mutate(type = "River")
road <- roads %>% crop(SpatialPoints(occ[,23:22])) %>%  fortify() %>%  mutate(type = "Road")
lin <- bind_rows(riv, road)


lma <- landmass %>% crop(SpatialPoints(occ[,23:22])) %>%  fortify()

# plot
ggplot()+
  geom_polygon(data = lma,
               mapping = aes(x = long, y = lat, group = group), lwd = 0.5, col = "grey40", fill = "transparent")+
  geom_path(mapping = aes(x = long, y = .data$lat, group = .data$group, shape = type, color = type),
            data = lin)+
  geom_point(data = pts, mapping = aes(x = longitude, y = latitude, linetype = type, color = type), size = 3)+
  geom_point(data = occ, mapping = aes(x = decimalLongitude, y = decimalLatitude), size = 1)+
  scale_color_viridis(discrete = TRUE)+
  xlab("Longitude")+
  ylab("Latitude")+
  coord_fixed()+
  theme_bw()+
  theme(
    legend.position = "bottom"
  )
