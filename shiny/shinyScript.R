library(reproducible)
library(SpaDES.shiny)
destPath <- file.path(getwd(), "shiny") %>%
  checkPath(create = TRUE)
mySimOut <- reproducible::prepInputs(url = "https://drive.google.com/open?id=1qIqZ0tHAsQUdfNMZji5jsmV6zZorsa6I",
                                    targetFile = "resultsShiny.rds", destinationPath = destPath)
shine(mySimOut)
source('~/Documents/GitHub/borealBirdsAndForestry/shiny/fitModel.R')
pathToData <- file.path(destPath, "data")

# Do this for 1999 and 2011 too.
predictions <- lapply(X = c(1985, 1999, 2011), FUN = function(year){
  predictions <- predictDensities(birdSpecies = mySimOut$birdSpecies, 
                                  disturbanceRas = raster::raster(
                                    file.path(pathToData, paste0("mergedFocal",year,"-100Res250m.tif"))), 
                                  birdDensityRasters = mySimOut$birdDensityRasters, currentTime = year, 
                                  modelList = mySimOut$models$localTransitional, pathData = pathToData)
return(predictions)  
})
names(predictions) <- c(1985, 1999, 2011)

# This is a Shiny web application to display the BAM densities with offsets.

# Data: https://drive.google.com/open?id=18TeNsWmtNwe3CR39b28G042Q9RgrhQxJ

# Outputs: 1. Where are the point counts (Sp + Years)
#          2. Histogram of data distribution per BCR and per province; (Sp + Years)
#          3. Alberto's Model resuls (species + scale + disturbance type)
#          4. Predictions + uncertainty for the whole boreal using the data selected (species)
# Inputs: species, survey years, scale, disturbance type

# Need: 
# 1. data: cache -> run on 388 and cache, add the shiny app as a module 
# 2. Still need to work on the prediction module. Only for one year!
# 3. Canada's shapefile

# 1. Construct the structure of the models;
# 3. Add raster layers: https://rstudio.github.io/leaflet/raster.html
# 4. Add tables

library(shiny)
library(shinythemes)
library(leaflet)
library(RColorBrewer)
library(ggmap)
library(raster)

options(browser = "C:/Program Files (x86)/Google/Chrome/Application/chrome.exe")

dt <- reproducible::prepInputs(url = "https://drive.google.com/open?id=18TeNsWmtNwe3CR39b28G042Q9RgrhQxJ", 
                               fun = data.table::fread)
r_colors <- rgb(t(col2rgb(colors()) / 255))
names(r_colors) <- colors()

maptypes <- c("MapQuestOpen.Aerial", # nope
              "Stamen.TerrainBackground", # Nice
              "Esri.WorldImagery", # real map
              "OpenStreetMap", # nope
              "Stamen.Watercolor", # really cool!
              "Stamen.TonerLite")

# Set colours for rasters # RColorBrewer
pal <- colorNumeric(c("#0C2C84", "#41B6C4", "#FFFFCC"), values(r),
                    na.color = "transparent")


# Define UI for application that draws a histogram
ui <- fillPage(
  theme = shinytheme("slate"),
  # leafletOutput("mymap"),
  
  br(),
  leafletOutput("mymap", height="350px"),
  absolutePanel(top=20, left=70, textInput("target_zone", "" , "Canada")),
  br(),
  
  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      sliderInput(inputId = "year", label = "Year", min = 1985, max = 2011,
                  value = 1985, step = 13),
      selectInput(multiple = FALSE, inputId = "species", label = "Choose Species", 
                  choices = list("CMWA", "BOCH", "BBWA", "BTNW")),
      radioButtons(inputId = "scale", label = "Spatial scale", 
                   choices = list("Local", "Neighborhood")),
      checkboxGroupInput(inputId = "typeDisturbance", label = "Type of disturbance", 
                         choices = c("Alienating", "Successional")),
      submitButton(text = "Calculate", icon = icon("refresh"))
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("modelSummary")
    ),
    position = "left")
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  
  output$mymap <- renderLeaflet({
    if(input$target_zone=="Canada"){
      ZOOM=3.7
      LAT=57.304897
      LONG=-102.262423
    }else{
      target_pos=geocode(input$target_zone)
      LAT=target_pos$lat
      LONG=target_pos$lon
      ZOOM=12
    }
    leaflet() %>% addTiles() %>%
      # The ras will depend on the species/type/scale choices
      addRasterImage(raster::plot(predictions[[as.character(input$year)]][[input$species]]), opacity = 0.8) %>%
      addLegend(pal = colorNumeric(), values = values(predictions[[as.character(input$year)]][[input$species]]),
                 title = "Predicted abundance") %>%
      setView(lng=LONG, lat=LAT, zoom=ZOOM ) %>%
      addProviderTiles(maptypes[5],
                       options = providerTileOptions(noWrap = TRUE)
      )
  })
  
  output$modelSummary <- renderPlot({
    
    # THIS WILL BE A TABLE OF summary(model)
    summary(mySimOut$models$localTransitional[[input$species]])
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
