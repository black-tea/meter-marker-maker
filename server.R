##################################
# Meter Marker Maker Server Code #
##################################

library(shiny)
library(sf) 

#### Functions
ClearAll <- function() {
  # Clears map and RV objects
  #
  # Args: None
  #
  # Returns: Clears map and RV objects
  #
  proxy <- leafletProxy("map") %>%
    clearMarkers() %>%
    clearShapes()
  #locationRV$Segment <- NULL
  #locationRV$Intersection <- NULL
}

CalcSegFraction <- function(startPt, seg, distFt) {
  # Ensure interpolation along line returns correct point
  # using PostGIS function ST_LineLocatePoint for verification
  #
  # Args:
  #   startPt: A point value at one end of the segment, used to establish origin for distance
  #   seg: A polyline segment to interpolate the distance along
  #   segFraction: Fractional value of the segment where the point is located
  #
  # Returns:
  #   Fractional value along the line for generating the point
  #
  #print(seg)
  segDistFt <- st_length(seg)
  segFraction <- distFt/segDistFt
  units(segFraction) <- NULL
  print(segFraction)
  if(CheckLineStart(startPt, seg) == TRUE) {
    return(segFraction) 
  } else if (CheckLineStart(startPt, seg) == FALSE) {
    return(1 - segFraction)
  }
}

CheckLineStart <- function(startPt, seg) {
  # Ensure interpolation along line returns correct point
  # using PostGIS function ST_LineLocatePoint for verification
  #
  # Args:
  #   startPt: intersection point used to establish origin for distance
  #   seg: A polyline segment to interpolate the distance along
  #
  # Returns:
  #   True/False indicating whether the intersection point
  #   at the start of the line or not
  #
  Pt1 <- st_line_sample(x = seg, n = 1, sample = 0) %>% st_cast("POINT")
  Pt2 <- st_line_sample(x = seg, n = 1, sample = 1) %>% st_cast("POINT")
  #print(Pt1)
  #print(Pt2)
  if(st_distance(startPt, Pt1) < st_distance(startPt, Pt2)){
    return(TRUE)
  } else {
    return(FALSE)
  }
  
}

GenerateLinePoint <- function(startPt, seg, distFt) {
  # Generate a point along a line
  #
  # Args:
  #   startPt: A point value at one end of the segment, used to establish origin for distance
  #   seg: A polyline segment to interpolate the distance along
  #   distFt: Distance value for interpolation along the polyline segment
  #
  # Returns:
  #   A sf point at the precise distance specified along the line
  #
  # Need some error handling if distance > length of line
  seg <- seg %>%
    st_cast("LINESTRING") %>%
    st_set_crs(4326) %>%
    st_transform(2229)
  startPt <- startPt %>%
    st_set_crs(4326) %>%
    st_transform(2229)
  #segDistFt <- st_length(seg)
  #segFraction <- distFt/segDistFt
  segFraction <- CalcSegFraction(startPt, seg, distFt)
  infrastructurePt <- st_line_sample(x = seg, n = 1, sample = segFraction) %>%
    st_cast("POINT") %>%
    st_transform(4326)
  return(infrastructurePt)
  # Error handling
  #if distFt > segDist
}

SelectSegment <- function(segID) {
  # Select a polyline based on the row ID and draw on map
  #
  # Args:
  #   segID: Row ID of the line
  #
  # Returns:
  #   Adds Selected Line to the Map
  if(!is.null(xstreetR())){
    # Filter polylines based on the click
    polylineSelected <- xstreetR() %>%
      filter(rownames(.) == segID )
    # Add selected line information to RV
    locationRV$Segment <- polylineSelected
    # Add selected line on top as another color
    proxy <- leafletProxy("map") %>%
      # Add selected line shape to map
      addPolylines(
        data = polylineSelected,
        layerId = "selected",
        color = "#0066a1",
        opacity = 1
      )
    # Once user has selected the street segment, becomes NULL
    msgRV$msg <- c(' ')
  } 
}

CreateIcon <- function(color) {
  # Create icon for mapping, using the awesomeIcons library
  #
  # Args:
  #   color: desired color for the map marker
  #
  # Returns:
  #   Map marker with the 'circle-o' icon in the desired color
  custom_icon <- awesomeIcons(
    icon = 'circle-o',
    iconColor = '#ffffff',
    library = 'fa',
    # The markercolor is from a fixed set of color choices
    markerColor = color
  )
  return(custom_icon)
}

#### Prep Code
# Load intersections
intersections <- read_sf('data/Intersections/Intersections.shp')
streets <- read_sf('data/Centerline/Streets.shp')
# https://stackoverflow.com/questions/50844693/simple-feature-linestring-collection-not-visible-in-r-leaflet
names(streets$geometry) <- NULL
councilDistricts <- read_sf('data/CouncilDistricts/CnclDist_July2012_wgs84.shp')
dotDistricts <- read_sf('data/DOTDistricts/LADOT_District_wgs84.shp')

#### Server Code
server <- function(input, output, session) {
  
  ### UI Elements
  # Intersection Selection
  output$int_select <- renderUI({
    
    # Selection Input
    selectizeInput(inputId = "int",
                   label = "Intersection",
                   choices = intersections$TOOLTIP,
                   selected = NULL,
                   multiple = FALSE)
  })
  
  
  # Message Object
  output$message <- renderText({rv_msg$msg})
  # Render a list for each item in the msg
  output$message <- renderUI({
    HTML(paste(rv_msg$msg, collapse ='<br/>'))
  })
  
  ### Reactive Objects
  # RV storing UI message variable
  rv_msg <- reactiveValues()
  
  # RV for location objects
  locationRV <- reactiveValues(Intersection=list(), Segment=list())
  
  # Reactive expression to grab intersection data based on user selection
  intersectionR <- reactive({
    if(!is.null(input$int) && input$int != "" && length(input$int) > 0){
      # int_query <- paste0("SELECT * FROM intersections WHERE tooltip=","'",toString(input$int),"'")
      # intersectionR <- sqlQuery(int_query, type = 'spatial')
      intersectionR <- intersections %>%
        filter(TOOLTIP == toString(input$int))

    } else {return(NULL)}
  })
  
  # Reactive expression to grab cross streets from selected intersection
  xstreetR <- reactive({
    if(!is.null(input$int) && input$int != "" && length(input$int) > 0){
      # Grab selected intersection information
      intersectionR <- intersectionR()
      # Query for streets related to the intersection
      xstreet <- streets %>%
        filter(INT_ID_FRO == intersectionR$CL_NODE_ID | INT_ID_TO == intersectionR$CL_NODE_ID)
      return(xstreet)
      
    } else {return(NULL)}
  })
  
  # Reactive expression to grab the council district based on the point
  cdR <- reactive({
    if(!is.null(locationRV$Intersection)){
      cdR <- sf::st_join(locationRV$Intersection, councilDistricts)
      return(cdR$DISTRICT)
    } else {
      return(NULL)
    }
  })
  
  # Reactive expression to grab the DOT district based on the point
  dotR <- reactive({
    if(!is.null(locationRV$Intersection)){
      dotR <- sf::st_join(locationRV$Intersection, dotDistricts)
      return(dotR$DOT_DIST)
    } else {
      return(NULL)
    }
  })
  
  # # Reactive expression capturing distance for SFS to nearest intersection
  # sfsDistR <- reactive({
  #   if(input$treatment_type == 'sfs') {
  #     if(!any(sapply(list(locationRV$Intersection, locationRV$Segment, input$sfs_distance), is.null)) && input$treatment_type == 'sfs'){
  #       sfsPt <- DistToPt(locationRV$Intersection, locationRV$Segment, input$sfs_distance)}
  #     return(input$sfs_distance)
  #   } else {
  #     return(NULL)
  #   }
  # })
  
  # # temporary observer to test sfsDistR
  # observeEvent(input$int, {
  #   print(sfsDistPtR())
  # })
  
  ### Map
  output$map <- renderLeaflet({
    # Map object
    map <- leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron,
                       options = providerTileOptions(minZoom = 10, maxZoom = 18)) %>%
      setView(lng = -118.329327,
              lat = 34.0546143,
              zoom = 11) 
  })
  
  # Map observer that updates based on the intersection
  observeEvent(input$int, {
    if(!is.null(input$int) && input$int != "" && length(input$int) > 0){
      # Get intersection reactive var, clear markers, clear RV
      intersectionR <- intersectionR()
      locationRV$Segment <- NULL
      proxy <- leafletProxy("map") %>%
        clearMarkers() %>%
        clearShapes()
      #ClearAll()
      
      # If there is one marker in the query, it is blue
      if(nrow(intersectionR) == 1 && length(intersectionR) > 0) {
        # Add intersection to RV object
        locationRV$Intersection <- intersectionR
        # Add intersection marker to map
        proxy %>% addAwesomeMarkers(
          data = intersectionR,
          icon = CreateIcon('darkblue')
        )
        
        # if(input$treatment_type == "Intersection Tightening") {
        #   rv_msg$msg <- c(paste0("Coordinates: ", toString(intersectionR$geometry)),
        #                   paste0("Council District: ",toString(cdR())),
        #                   paste0("DOT District: ", toString(dotR())))


        # Get cross streets
        xstreetR <- xstreetR()

        # Update message to choose a street
        rv_msg$msg <- c('Select a Cross Street')

        # If there is at least one related segment, add it
        if(nrow(xstreetR) > 0) {
          print("hey you!")
          print(xstreetR)
          proxy %>% addPolylines(
            data = xstreetR,
            layerId = as.numeric(rownames(xstreetR)),
            color = "gray"
          )
        }

        # If there is > 1 marker, gray initially
      } else if(nrow(intersectionR) > 1) {
        proxy %>% addAwesomeMarkers(
          data = intersectionR,
          layerId = as.numeric(rownames(intersectionR)),
          icon = CreateIcon("gray")
        )
        rv_msg$msg <- c('Select One Intersection Node')
        
      }
      # Update the map zoom bounds
      proxy %>% fitBounds(lng1 = as.double(st_bbox(intersectionR)[1]),
                          lat1 = as.double(st_bbox(intersectionR)[2]),
                          lng2 = as.double(st_bbox(intersectionR)[3]),
                          lat2 = as.double(st_bbox(intersectionR)[4]))
      
      }
  })
  
  # Map Observer based on int selection
  observeEvent(input$map_marker_click, {
    if(nrow(intersectionR()) > 1) {
      # Grab ID of the shape that was clicked
      click_id <- input$map_marker_click$id
      # Filter intersections based on the click
      intS <- intersectionR() %>%
        filter(rownames(.) == click_id)
      locationRV$Intersection <- intS
      # HERE I NEED TO UPDATE locationRV$point
      # HERE I NEED TO ADD SELECTED LINE
      proxy <- leafletProxy("map") %>%
        # Add selected intersection to map
        addAwesomeMarkers(
          data = intS,
          layerId = "intselected",
          icon = CreateIcon('darkblue')
        )
      
      rv_msg$msg <- c(paste0("Coordinates: ", toString(locationRV$Intersection$geometry)),
                      paste0("Council District: ",toString(cdR())),
                      paste0("DOT District: ", toString(dotR())))
    }
  })
  
  # Map Observer based on the polyline selection
  observeEvent(input$map_shape_click, {
    if(!is.null(xstreetR())){
      # Grab ID of the shape that was clicked
      click_id <- input$map_shape_click$id
      # Filter polylines based on the click
      polyline_s <- xstreetR() %>%
        filter(rownames(.) == click_id )
      locationRV$Segment <- polyline_s
      # Add selected line on top as another color
      proxy <- leafletProxy("map") %>%
        # Add selected line shape to map
        addPolylines(
          data = polyline_s,
          layerId = "selected",
          color = "#0066a1",
          opacity = 1
        )
      # Once user has selected the street segment, becomes NULL
      rv_msg$msg <- c('.')
    }
  })
  
  # Observer to add point along the line
  observe({
    if(!any(sapply(list(locationRV$Intersection, locationRV$Segment, input$distance), function(x) length(x) < 1))){
    #if(length(locationRV$Intersection) > 0){
      print(locationRV$Intersection)
      print(locationRV$Segment)
      print(input$distance)
      infrastructurePt <- GenerateLinePoint(locationRV$Intersection, locationRV$Segment, input$distance) 
      # Update Map
      leafletProxy("map") %>%
        removeMarker(layerId = "infrastructurePt") %>%
        addMarkers(data = infrastructurePt, layerId = "infrastructurePt")
    }
  })

}

