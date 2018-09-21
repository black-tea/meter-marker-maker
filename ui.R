##############################
# Meter Marker Maker UI Code #
##############################

### Libraries
library(shiny)
library(DT)
library(leaflet)
library(sf)
library(dplyr)

appCSS <- ".mandatory_star { color: red; }"

### User Interface
ui <- fluidPage(
  titlePanel("Meter Marker Maker"),
  
  div(
    id = "form",
    fluidRow(
      
      # First UI Bin
      column(4,
             uiOutput("int_select"),
             numericInput("distance", label = "Distance from Intersection (ft)", value = 0)
      ),
      
      # Second UI Bin
      column(4, uiOutput("treatment_info1")),
      
      # Third UI Bin
      column(4, uiOutput("treatment_info2"))
    ),
    
    hr(),
    h6(htmlOutput("message"), align="center"),
    hr(),
    
    # Map Output
    leafletOutput("map")
    
  )

)