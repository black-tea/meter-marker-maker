##################################
# Meter Marker Maker Server Code #
##################################

library(shiny)
library(sf) 


#### Functions
ChoosePt <- function(startPt, seg, segFraction) {
  # Ensure interpolation along line returns correct point
  # using PostGIS function ST_LineLocatePoint for verification
  #
  # Args:
  #   startPt: A point value at one end of the segment, used to establish origin for distance
  #   seg: A polyline segment to interpolate the distance along
  #   segFraction: Fractional value of the segment where the point is located
  #
  # Returns:
  #   The sf point that matches the distance specified in DistToPt
  #
  Pt1 <- st_line_sample(x = seg, n = 1, sample = segFraction) %>% st_cast("POINT")
  Pt2 <- st_line_sample(x = seg, n = 1, sample = (1 - segFraction)) %>% st_cast("POINT")
  
}

DistToPt <- function(startPt, seg, distFt) {
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
  segDistFt <- st_length(seg)
  segFraction <- distFt/segDistFt
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
    proxy <- leafletProxy("infrastructureManagerMap") %>%
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
  #RV for location objects
  rv_location <- reactiveValues(Intersection=list(),
                                Segment=list())
  #RV storing UI message variable
  rv_msg <- reactiveValues()
  
  # RV for location objects
  locationRV <- reactiveValues(Intersection=list(), Segment=list())
  
  # Reactive expression to grab intersection data based on user selection
  intersection_r <- reactive({
    if(!is.null(input$int) && input$int != "" && length(input$int) > 0){
      # int_query <- paste0("SELECT * FROM intersections WHERE tooltip=","'",toString(input$int),"'")
      # intersection_r <- sqlQuery(int_query, type = 'spatial')
      intersection_r <- intersections %>%
        filter(TOOLTIP == toString(input$int))

    } else {return(NULL)}
  })
  
  # Reactive expression to grab the council district based on the point
  cd_r <- reactive({
    if(!is.null(rv_location$Intersection)){
      cd_r <- sf::st_join(rv_location$Intersection, councilDistricts)
      return(cd_r$DISTRICT)
    } else {
      return(NULL)
    }
  })
  
  
  
  # Reactive expression to grab the DOT district based on the point
  dotR <- reactive({
    if(!is.null(rv_location$Intersection)){
      dotR <- sf::st_join(rv_location$Intersection, dotDistricts)
      return(dotR$DOT_DIST)
    } else {
      return(NULL)
    }
  })
  
  # Reactive expression to grab cross streets from selected intersection
  xstreet_r <- reactive({
    if(!is.null(input$int) && input$int != "" && length(input$int) > 0){
      # Grab selected intersection information
      intersection_r <- intersection_r()
      # Query for streets related to the intersection
      xstreet <- streets %>%
        filter(INT_ID_FRO == intersection_r$CL_NODE_ID | INT_ID_TO == intersection_r$CL_NODE_ID)
      return(xstreet)
      
    } else {return(NULL)}
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
      intersection_r <- intersection_r()
      rv_location$Segment <- NULL
      proxy <- leafletProxy("map") %>%
        clearMarkers() %>%
        clearShapes()
      
      # If there is one marker in the query, it is blue
      if(nrow(intersection_r) == 1 && length(intersection_r) > 0) {
        # Add intersection to RV object
        rv_location$Intersection <- intersection_r
        # Add intersection marker to map
        proxy %>% addAwesomeMarkers(
          data = intersection_r,
          icon = CreateIcon('darkblue')
        )
        
        # if(input$treatment_type == "Intersection Tightening") {
        #   rv_msg$msg <- c(paste0("Coordinates: ", toString(intersection_r$geometry)),
        #                   paste0("Council District: ",toString(cd_r())),
        #                   paste0("DOT District: ", toString(dotR())))


        # Get cross streets
        xstreet_r <- xstreet_r()

        # Update message to choose a street
        rv_msg$msg <- c('Select a Cross Street')

        # If there is at least one related segment, add it
        if(nrow(xstreet_r) > 0) {
          print("hey you!")
          print(xstreet_r)
          proxy %>% addPolylines(
            data = xstreet_r,
            layerId = as.numeric(rownames(xstreet_r)),
            color = "gray"
          )
        }

        # If there is > 1 marker, gray initially
      } else if(nrow(intersection_r) > 1) {
        proxy %>% addAwesomeMarkers(
          data = intersection_r,
          layerId = as.numeric(rownames(intersection_r)),
          icon = CreateIcon("gray")
        )
        rv_msg$msg <- c('Select One Intersection Node')
        
      }
      # Update the map zoom bounds
      proxy %>% fitBounds(lng1 = as.double(st_bbox(intersection_r)[1]),
                          lat1 = as.double(st_bbox(intersection_r)[2]),
                          lng2 = as.double(st_bbox(intersection_r)[3]),
                          lat2 = as.double(st_bbox(intersection_r)[4]))
      
      }
  })
  
  # Map Observer based on int selection
  observeEvent(input$map_marker_click, {
    if(nrow(intersection_r()) > 1) {
      # Grab ID of the shape that was clicked
      click_id <- input$map_marker_click$id
      # Filter intersections based on the click
      intS <- intersection_r() %>%
        filter(rownames(.) == click_id)
      rv_location$Intersection <- intS
      # Add selected line on top
      proxy <- leafletProxy("map") %>%
        # Add selected intersection to map
        addAwesomeMarkers(
          data = intS,
          layerId = "intselected",
          icon = CreateIcon('darkblue')
        )
      
      rv_msg$msg <- c(paste0("Coordinates: ", toString(rv_location$Intersection$geometry)),
                      paste0("Council District: ",toString(cd_r())),
                      paste0("DOT District: ", toString(dotR())))
    }
  })
  
  # Map Observer based on the polyline selection
  observeEvent(input$map_shape_click, {
    if(!is.null(xstreet_r())){
      # Grab ID of the shape that was clicked
      click_id <- input$map_shape_click$id
      # Filter polylines based on the click
      polyline_s <- xstreet_r() %>%
        filter(rownames(.) == click_id )
      rv_location$Segment <- polyline_s
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
    #if(!any(sapply(list(locationRV$Intersection, locationRV$Segment, input$distance), is.null))){
    if(length(locationRV$Intersection) > 0){
      print(locationRV$Intersection)
      print(locationRV$Segment)
      print(input$distance)
      infrastructurePt <- DistToPt(locationRV$Intersection, locationRV$Segment, input$distance) 
      # Update Map
      leafletProxy("map") %>%
        removeMarker(layerId = "infrastructurePt") %>%
        addMarkers(data = infrastructurePt, layerId = "infrastructurePt")
    }
  })

}

