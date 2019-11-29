# sampbias
[![Project Status: Active – The project has reached a stable, usable state and is being actively developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![Build Status](https://travis-ci.org/azizka/sampbias.svg?branch=master)](https://travis-ci.org/azizka/sampbias)
[![codecov.io](https://codecov.io/github/azizka/sampbias/graphs/badge.svg?branch=master)](https://codecov.io/github/azizka/sampbias)

*Sampbias* is a statistical method to evaluate and visualise geographic sampling biases in species distribution datasets, implemented as an R package and graphical-user-interface shiny app.

# Description
Species occurrence datasets derived from biological collections or human observations are widely used in biological sciences, including ecology, conservation, systematics and evolution. However, such data are often geographically biased, with remote areas being strongly under sampled. Although spatial and taxonomic biases are widely recognised by the scientific community, few attempts have been made to quantify their strength and to discern among different sources of biases. The implications of not considering biases in biodiversity research have not yet been thoroughly assessed, but are likely to be substantial. Therefore, it is advisable that any study dealing with species occurrence data - either carefully validated or directly downloaded - should assess the biases covered by this package.     

*Sampbias* is a method and tool to 1) visualize the distribution of occurrence records and species in any user-provided dataset, 2) quantify the biasing effect of geographic features related to human accessibility, such as proximity to cities, rivers or roads, and 3) create publication-level graphs of these biasing effects in space.

*Sampbias* evaluates the biasing effect of geographic features by comparing the statistical distance distribution observed in a user-provided dataset to a simulated distribution expected under random sampling. The method is scale independent, and any multi-species occurrence records can be tested against any set of geographic gazetteers (reliability increases with increasing dataset size). Default large-scale gazetteers for airports, cities, rivers and roads are provided with the package. Species Occurrence data as downloaded from the data portal of the Global Biodiversity Information Facility (GBIF) can be directly used as input data for *sampbias*. The output of the package includes measures of bias effect, comparison between different gazetteers (e.g. comparing biasing effect of roads and rivers), different taxa (e.g. birds vs. flowering plants) and different datasets (e.g. specimens vs human observations).

The results of *sampbias* can be used to identify priority for further collection or digitalisation efforts, provide bias surfaces for species distribution modelling, or assess the reliability of scientific results based on publicly available species distribution data.

*Sampbias* thus offers an efficient, largely automated means for biodiversity scientists and non-specialists alike to further explore species occurrence data. 

# Examples
Example datasets for the R package are provided in the example_data folder. Further explanations on how to use *sampbias* and the theoretical background can be found on the gitHub wiki page. A build version of the package can be downloaded form the repository, or directly from R using the code shown in the wiki and below. The graphical-user-interface-based shiny app can be accessed at https://azizka.shinyapps.io/sampbias/.

# For the impatient
```{r}
#installing the package
install.packages("devtools")
require("devtools")
install_github("azizka/sampbias")
library(sampbias)

#reading a csv file as downloaded from GBIF and provided in the example data folder
example.in <- read.csv("example_data/mammals_borneo.csv",sep = "\t")

#running sampbias
example.out <- SamplingBias(x = example.in, res = 0.1)

summary(example.out)
plot(example.out)

#writing plots to a .pdf file
pdf("Your_file_name.pdf")
plot(example.out)
dev.off()

```
