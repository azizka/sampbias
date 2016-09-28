library(shiny)
library(maptools)
library(raster)
library(ggplot2)
library(viridis)
library(rgeos)

data("wrld_simpl")


# Define UI for application 
ui <- shinyUI(fluidPage(
   
   # Application title
   titlePanel("Sampling bias in species distribution records"),
   
    
   sidebarLayout(
      sidebarPanel(

        fileInput("file1", label = h3("Occurence File")),

        radioButtons("type", label = h3("Bias Type"),
                     choices = list("None" = "non",
                                    "Species" = "spe",
                                    "Cities" = "cit", "Airports" = "air",
                                    "Rivers" = "riv", "Roads" = "roa",
                                    "Combined" = "comb"), selected = "non"),

        numericInput("num", label = h3("Raster resolution [deg]"), value = 1)
      ),
      
      # Show plots
      mainPanel(
         plotOutput("plo1"),
         plotOutput("plo2")
      )
   )
))

# Define server logic required to draw a histogram
source("helpers.R")

server <- shinyServer(function(input, output) {

  options(shiny.maxRequestSize=35*1024^2) # change maximum upload size
  
  #load the input data
  dataInput <- reactive({
    inFile <- input$file1

    read.csv(inFile$datapath, sep = "\t")
  })

    #create a spatialPOints data.frame version of the input dataset
  datPts <- reactive({
   SpatialPoints(dataInput()[,c("decimallongitude", "decimallatitude")])
  })

  #this is the first plot
   output$plo1 <- renderPlot({
     
     #do not display anything if no file is selected
     if (is.null(input$file1)){
       return(NULL)
       }else{
         load("default_gazetteers.R")
         print(PlotOccRast(datPts(), res = input$num, gaz = gazetteers))
     }
   })
   
   output$plo2 <- renderPlot({
     #do not display anything if no file is selected
     if (is.null(input$file1)){return(NULL)
       }else{
     load("default_gazetteers.R")
     dist <- switch(input$type,
                    non = "NULL",
                    spe = PlotSpRast(dataInput(), res = input$num, wrld = gazetteers$landmass),
                    cit = BiasPlot(x = dataInput(), gaz = list(cities = gazetteers$cities), 
                                   res = input$num, terrestrial.cut = gazetteers$landmass),
                    air = BiasPlot(x = dataInput(), gaz = list(airports = gazetteers$airports), 
                                   res = input$num, terrestrial.cut = gazetteers$landmass),
                    riv = BiasPlot(x = dataInput(), gaz = list(rivers = gazetteers$rivers), 
                                   res = input$num, terrestrial.cut = gazetteers$landmass),
                    roa = BiasPlot(x = dataInput(), gaz = list(roads = gazetteers$roads), 
                                   res = input$num, terrestrial.cut = gazetteers$landmass),
                    comb = BiasPlot(x = dataInput(), gaz = gazetteers[1:4], 
                                    res = input$num, terrestrial.cut = gazetteers$landmass))
     
     print(dist)
           }
   })
})

# Run the application 
shinyApp(ui = ui, server = server)

