library(shiny)
library(maptools)
library(raster)
library(ggplot2)
library(viridis)

data("wrld_simpl")


# Define UI for application 
ui <- shinyUI(fluidPage(
   
   # Application title
   titlePanel("Sampling bias in species distribution records"),
   
    
   sidebarLayout(
      sidebarPanel(

        fileInput("file1", label = h3("File input")),
        
        fileInput("file2", label = h3("Gazetteer Input")),
        
        radioButtons("type", label = h3("Bias Type"),
                     choices = list("Species" = "spe",
                                    "Cities" = "cit", "Airports" = "air",
                                    "Rivers" = "riv", "Roads" = "roa",
                                    "Combined" = "comb"), selected = "spe"),
       
        # selectInput("select", label = h3("Calculation method"), 
        #             choices = list("Raster" = 1, "Distance" = 2)),
       
        numericInput("num", label = h3("Raster resolution [deg]"), value = 1)
        
        
        # checkboxGroupInput("checkGroup", label = h3("Show"), 
        #                    choices = list("Airports" = "air", "Cities" = "cit", "Rivers" = "riv", "Roads" = "roa"),
        #                    selected = 1)

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
  
  # gazInput <- reactive({
  #   inFile2 <- input$file2
  # 
  #   load(inFile2$datapath, envir = environment())
  # })

    #create a spatialPOints data.frame version of the input dataset
  datPts <- reactive({
   SpatialPoints(dataInput()[,c("decimallongitude", "decimallatitude")])
  })
  
  #load and crop backround country border map
  # wrldMap <- reactive({
  #   wrld <- crop(wrld_simpl, extent(datPts()))
  #   fortify(wrld)
  # })

  
  #this is the first plot
   output$plo1 <- renderPlot({
     
     #do not display anything if no file is selected
     if (is.null(input$file1)){
       return(NULL)
       }else{
       if (is.null(input$file2)){ 
         print(PlotOccRast(datPts(), res = input$num, gaz = NULL))
       }else{
         load(input$file2$datapath, envir = environment())
         print(PlotOccRast(datPts(), res = input$num, gaz = gazetteers))
       }
     }


     

   })
   
   output$plo2 <- renderPlot({
     #do not display anything if no file is selected
     if (is.null(input$file1)){return(NULL)
       }else{
         if(is.null(input$file2)){return(NULL)
           }else{
     # load(input$file2$datapath, envir = environment())
     
     
     dist <- switch(input$type,
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
       }
   })

})

# Run the application 
shinyApp(ui = ui, server = server)

