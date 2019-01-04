# Please install the development version of reproducible from github if you don't have it
devtools::install_github("PredictiveEcology/reproducible", ref = "development")
library(reproducible)

Require(SpaDES.shiny)
Require(raster)
Require(viridisLite)
Require(lme4)

destPath <- file.path(getwd(), "shiny") %>%
  checkPath(create = TRUE)
pathToData <- file.path(destPath, "data") %>%
  checkPath(create = TRUE)
url1985 <- "https://drive.google.com/open?id=13ll_52ktGGCbn_1a_oz2zZN8--vdkZOl"
url1999 <- "https://drive.google.com/open?id=1P71cQmUUHNX-WFAAeHxF99-ggMiYo0tA"
url2011 <- "https://drive.google.com/open?id=17w8NmnGJSQNhxRxH_Vgr_qAM_3uVcCQZ"
yearsToShow <- c(1985, 1999, 2011)

mySimOut <- reproducible::prepInputs(url = "https://drive.google.com/open?id=1qIqZ0tHAsQUdfNMZji5jsmV6zZorsa6I",
                                    targetFile = "resultsShiny.rds", destinationPath = pathToData)

rastersList <- lapply(X = yearsToShow, FUN = function(year){
  url <- get(paste0("url", year))
  assign(paste0("year", year), value = preProcess(url = url, 
                                                  destinationPath = pathToData, 
                                                  targetFile = paste0("mergedFocal",year,"-100Res250m.tif")))
})
names(rastersList) <- paste0("year", yearsToShow)

listFiles <- lapply(X = names(rastersList), FUN = function(year){
  filePath <- rastersList[[year]]$targetFilePath
  return(filePath)
})
names(listFiles) <- yearsToShow

# shine(mySimOut) # Should be running mySim, not mySimOut!
source(file.path(getwd(), 'shiny/fitModel.R'))
source(file.path(getwd(), 'shiny/predictDensities.R'))

predictions <- lapply(X = c(1985, 1999, 2011), FUN = function(year){
  predictions <- Cache(predictDensities, birdSpecies = mySimOut$birdSpecies, 
                                  disturbanceRas = raster::raster(listFiles[[as.character(year)]]), 
                                  birdDensityRasters = mySimOut$birdDensityRasters, currentTime = year, 
                                  modelList = mySimOut$models$localTransitional, pathData = pathToData)
return(predictions)  
})
names(predictions) <- c(1985, 1998, 2011)

repPredictions <- lapply(X = names(predictions), FUN = function(year){
  predSp <- lapply(X = names(predictions[[year]]), FUN = function(species){
    ras <- projectInputs(x = predictions[[year]][[species]], 
                         targetCRS = "+proj=merc +a=6378137 +b=6378137 +lat_ts=0.0 +lon_0=0.0 +x_0=0.0 +y_0=0 +k=1.0 +units=m +nadgrids=@null +wktext  +no_defs",
                         method = "bilinear")
    return(ras)
  })
  names(predSp) <- names(predictions[[year]])
  return(predSp)
})
names(repPredictions) <- names(predictions)
#"EPSG:3857" = "+proj=merc +a=6378137 +b=6378137 +lat_ts=0.0 +lon_0=0.0 +x_0=0.0 +y_0=0 +k=1.0 +units=m +nadgrids=@null +wktext  +no_defs"

# This is a Shiny web application to display the BAM densities with offsets.

# Data: https://drive.google.com/open?id=18TeNsWmtNwe3CR39b28G042Q9RgrhQxJ

# Outputs: 1. Where are the point counts (Sp + Years)
#          2. Histogram of data distribution per BCR and per province; (Sp + Years)
#          3. Alberto's Model resuls (species + scale + disturbance type)
#          4. Predictions + uncertainty for the whole boreal using the data selected (species)
# Inputs: species, survey years, scale, disturbance type

# Need:
# 2. Fix scrolldown for webpage
# 3. Fix the raster layer on top of the map: https://rstudio.github.io/leaflet/raster.html

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

allStack <- raster::stack(repPredictions[['1985']][['BBWA']], repPredictions[['1998']][['BBWA']], repPredictions[['2011']][['BBWA']],
                          repPredictions[['1985']][['BTNW']], repPredictions[['1998']][['BTNW']], repPredictions[['2011']][['BTNW']],
                          repPredictions[['1985']][['CAWA']], repPredictions[['1998']][['CAWA']], repPredictions[['2011']][['CAWA']],
                          repPredictions[['1985']][['CMWA']], repPredictions[['1998']][['CMWA']], repPredictions[['2011']][['CMWA']])

# Define UI for application that draws a histogram
ui <- fillPage(
  
  theme = shinytheme("slate"),

  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      sliderInput(inputId = "year", label = "Year", min = 1985, max = 2011,
                  value = 1985, step = 13),
      hr(),
      selectInput(multiple = FALSE, inputId = "species", label = "Choose Species", 
                  choices = list("CMWA", "CAWA", "BBWA", "BTNW"), selected = "BBWA"),
      hr(),
      radioButtons(inputId = "scale", label = "Spatial scale", 
                   choices = list("Local", "Neighborhood"), selected = "Neighborhood"),
      hr(),
      checkboxGroupInput(inputId = "typeDisturbance", label = "Type of disturbance", 
                         choices = c("Alienating", "Successional"), selected = "Successional")
     ),
    
    # Show a plot of the generated distribution
    mainPanel(tabsetPanel(
      tabPanel(
        title = "Abundance predictions", leafletOutput(outputId = "mymap", height = 600)
      ),
      tabPanel(
        title = "Model summary", verbatimTextOutput("modelSummary")
      )
    ),
    position = "left")
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

  output$mymap <- renderLeaflet({
    ras <- repPredictions[[as.character(input$year)]][[input$species]]
    # Set colours for rasters # RColorBrewer
    pal <- colorNumeric(palette = "inferno", domain = ras[], na.color = "#00000000")
      ZOOM=3.7
      LAT=57.2
      LONG=-95
    leaflet() %>% addTiles() %>%
      setView(lng=LONG, lat=LAT, zoom=ZOOM ) %>%
      addProviderTiles(maptypes[5],
                       options = providerTileOptions(noWrap = TRUE))%>%
      addRasterImage(x = ras, colors = pal, opacity = 1, project = FALSE) %>%
      addLegend(pal = pal, values = ras[],
                title = paste0("Predicted abundance <br>in ", input$year, " for ", input$species), bins = 10)
  })
  
  output$modelSummary <- renderPrint({
    # This is a table of the model summary
    summary(mySimOut$models$localTransitional[[input$species]])
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
