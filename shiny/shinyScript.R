# Please install the development version of reproducible from github if you don't have it
devtools::install_github("PredictiveEcology/reproducible", ref = "development")
library(reproducible)

Require("SpaDES.shiny")
Require("raster")
Require("viridisLite")
Require("lme4")

destPath <- file.path(getwd(), "shiny") %>%
  checkPath(create = TRUE)
pathToData <- file.path(destPath, "data") %>%
  checkPath(create = TRUE)
predictedFolder <- file.path(destPath, "predicted") %>%
  checkPath(create = TRUE)

# Download model results
mySimOut <- reproducible::prepInputs(url = "https://drive.google.com/open?id=1qIqZ0tHAsQUdfNMZji5jsmV6zZorsa6I",
                                    targetFile = "resultsShiny.rds", destinationPath = pathToData)

# Download bird density rasters
urlBBWA <- "https://drive.google.com/open?id=1ajv5eVhGyE2WQXhfB0Pf6fz33W8lEBBk"
urlCAWA <- "https://drive.google.com/open?id=16RCWfGauWIgjh_9HjyvyfsPjpCdVcobs"
urlCMWA <- "https://drive.google.com/open?id=1Rj0Nx8idK7rnnJYh2RAQ18gLBaWyiYnO"
urlBTNW <- "https://drive.google.com/open?id=1V7JDtkDglaxk6BSFBgAdqFsTJWd6yAI_"
densityRasList <- lapply(mySimOut$birdSpecies, FUN = function(sp){
  rasSp <- reproducible::prepInputs(url = get(paste0("url", sp)),
                                          targetFile = paste0("density", sp,".tif"), 
                                          destinationPath = pathToData)
  return(rasSp)
})
names(densityRasList) <- mySimOut$birdSpecies

# Download focal rasters
url1985 <- "https://drive.google.com/open?id=13ll_52ktGGCbn_1a_oz2zZN8--vdkZOl"
url1999 <- "https://drive.google.com/open?id=1P71cQmUUHNX-WFAAeHxF99-ggMiYo0tA"
url2011 <- "https://drive.google.com/open?id=17w8NmnGJSQNhxRxH_Vgr_qAM_3uVcCQZ"
yearsToShow <- c(1985, 1999, 2011)
rastersList <- lapply(X = yearsToShow, FUN = function(year){
  url <- get(paste0("url", year))
  assign(paste0("year", year), value = preProcess(url = url, 
                                                  destinationPath = pathToData, 
                                                  targetFile = paste0("mergedFocal",year,"-100Res250m.tif")))
})
names(rastersList) <- paste0("year", yearsToShow)
focalRas <- lapply(X = names(rastersList), FUN = function(year){
  filePath <- rastersList[[year]]$targetFilePath
  return(filePath)
})
names(focalRas) <- yearsToShow

# shine(mySimOut) # Should be running mySim, not mySimOut!
source(file.path(getwd(), 'shiny/fitModel.R'))
source(file.path(getwd(), 'shiny/predictDensities.R'))

predictions <- lapply(X = c(1985, 1999, 2011), FUN = function(year){
  predictions <- Cache(predictDensities, birdSpecies = mySimOut$birdSpecies, 
                                  disturbanceRas = raster::raster(focalRas[[as.character(year)]]), 
                                  birdDensityRasters = densityRasList, currentTime = year, 
                                  modelList = mySimOut$models$localTransitional, pathData = pathToData)
return(predictions)  
})
names(predictions) <- c(1985, 1998, 2011)

repPredictions <- lapply(X = names(predictions), FUN = function(year){
  predSp <- lapply(X = names(predictions[[year]]), FUN = function(species){
    EPSG.3857 <- "+proj=merc +a=6378137 +b=6378137 +lat_ts=0.0 +lon_0=0.0 +x_0=0.0 +y_0=0 +k=1.0 +units=m +nadgrids=@null +wktext  +no_defs"
    ras <- projectInputs(x = predictions[[year]][[species]], 
                         targetCRS = EPSG.3857,
                         method = "bilinear")
    return(ras)
  })
  names(predSp) <- names(predictions[[year]])
  return(predSp)
})
names(repPredictions) <- names(predictions)

# This is a Shiny web application to display the BAM predicted densities based on Suarez et al. (in review) models.

# Data: https://drive.google.com/open?id=18TeNsWmtNwe3CR39b28G042Q9RgrhQxJ

# Inputs: species, survey years, scale, disturbance type

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
                   choices = list("Local", "Neighborhood"), selected = "Local"),
      hr()
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
    
    # scale <- if (input$scale == "Local") "localTransitional" else "neighborhoodTransitional" # When all neighborhood results are ready
    # summary(mySimOut$models[[scale]][[input$species]]) # When neigh ready, uncomment these lines and delete the next if statement
    
    if (input$scale == "Local") {
      summary(mySimOut$models[["localTransitional"]][[input$species]])
    } else {
      print("Neighborhood models not yet implemented")
    }
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
