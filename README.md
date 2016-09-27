#What is sampbias?

A statistical method to evaluate and visualize geographic sampling biases in species distribution datasets, implemented as R package and graphical-user-interface shiny app.

#Description

Species distribution datasets from biological collections or human observations widely used in biological sciences and conservation, are often geographically biased, with remote areas being strongly under sampled. *Sampbias* is a method and tool to 1) visualize the distribution of occurrence records and species in any user-provided dataset, 2) quantify the biasing effect of geographic features related to accessibility, such as airports, cities, rivers or roads and 3) create publication-level graphs of these biasing effects in space.

*Sampbias* evaluates the biasing effect of geographic features by comparing the statistical distance distribution observed in the userprovided dataset to an empirical distribution expected under random sampling. The methods is scale independent, and any multi-species occurrence records can be tested against any set of geographic gazetteers (reliability increases with increasing dataset size). Default large-scale gazetteers for airports, cities, rivers and roads are provided with the package. Species Occurrence data as downloaded from the data portal of the Global Biodiversity Information Facility (GBIF) can be directly used as input data for *sampbias*. The output of the package includes measures of bias effect, comparable between different gazetteers (e.g. comparing biasing effect of roads and rivers), different taxa (e.g. birds vs. flowering plants)and different datasets (e.g. specimens vs human observations).

The results of *sampbias* can, be used to identify gaps for further collection or digitalization efforts, provide bias surfaces for distribution modelling or assess the reliability of scientific results for conservation.

#Examples

Example datasets for the R package are provided in the example_data folder. Further explanations on how to use *sampbias* and the theoretical background can be found on the gitHub wiki page. A build version of the package can be downloaded form the repository, or directly from R using the code shown in the wiki and below. The graphical-user-interface-based shiny app can be accessed at (REFERENCE TO SHINY APP).


#For the impatient

```{r}
#install the package
install.packages("devtools")
require("devtools")
install_github("azizka/sampbias")

#reading a csv file as downloaded from GBIF and provided in the example data folder
example.in <- read.csv("example_data/mammals_borneo_0008817-160822134323880.csv",sep = "\t")

#using the default gazetteers
example.out <- SamplingBias(x = example.in)

summary(example.out)
plot(example.out)

```

