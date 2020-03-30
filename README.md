# sampbias 1.0.3
[![Project Status: Active – The project has reached a stable, usable state and is being actively developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![Build Status](https://travis-ci.org/azizka/sampbias.svg?branch=master)](https://travis-ci.org/azizka/sampbias)
[![codecov.io](https://codecov.io/github/azizka/sampbias/graphs/badge.svg?branch=master)](https://codecov.io/github/azizka/sampbias)

*Sampbias* is a statistical method to evaluate and visualise geographic sampling biases in species distribution datasets, implemented as an R package.

# Description
Species occurrence datasets derived from biological collections or human observations are widely used in biological sciences, including ecology, conservation, systematics and evolution. However, such data are often geographically biased, with remote areas being strongly under sampled. Although spatial and taxonomic biases are widely recognised by the scientific community, few attempts have been made to quantify their strength and to discern among different sources of biases. The implications of not considering biases in biodiversity research have not yet been thoroughly assessed, but are likely to be substantial. Therefore, it is advisable that any study dealing with species occurrence data - either carefully validated or directly downloaded - should assess the biases covered by this package.     

*Sampbias* is a method and tool to 1) visualize the distribution of occurrence records and species in any user-provided dataset, 2) quantify the biasing effect of geographic features related to human accessibility, such as proximity to cities, rivers or roads, and 3) create publication-level graphs of these biasing effects in space.

The results of *sampbias* can be used to identify priority for further collection or digitalisation efforts, provide bias surfaces for species distribution modelling, or assess the reliability of scientific results based on publicly available species distribution data.

# Examples
Example datasets for *sampbias* and a tutorial on how to use it are provided with the package.

# For the impatient
```{r}
#installing the package
install.packages("devtools")
require("devtools")
install_github("azizka/sampbias")
library(sampbias)

#reading a csv file as downloaded from GBIF and provided in the example data folder
example.in <- read.csv(system.file("extdata", "mammals_borneo.csv", package="sampbias"), sep = "\t")

#running sampbias
example.out <- calculate_bias(x = example.in, res = 0.1)

summary(example.out)
plot(example.out)
```
