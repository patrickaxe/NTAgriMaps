library(shiny)
library(leaflet)
library(sf)
library(tidyverse)
library(DT)

# Input Data

sf <- read_sf(dsn=file.path(rprojroot::find_rstudio_root_file(), "Shapefiles","Study Area - Overall.shp"))

landunits<-read_sf(dsn=file.path(rprojroot::find_rstudio_root_file(), "Shapefiles","Summary14.shp"))


# Transform shapefile to WGS84
sf <- st_transform(sf, crs = 4326)
landunits<-st_transform(landunits, crs=4326)

# Add Data
df <- data.frame(CODE = c("BARKN_25", "BARKN_50", "BESWI_25", "DUNMA_50", "EARNH_50",  "FRE15_50", "GUNNP_25", "KARLS_50",  "KRP18_25",  "LAR18_25",  "LARRI_25",  "NANGU_25", "ORACK_25", "ROPLU_100", "TTREE_25",  "WEA18_25",  "WILDM_25",  "ALICU_25"),
                 ALT = c("NA", "NA", "Beswick ALT", "Jingaloo AC", "Arnhem ALT", " Kurnturlpara & Warumungu ALTs", "NA", "Karlantijpa South ALT",  "NA",  "NA",  "Mangarrayi & Wubalawun ALTs",  "Daly River/Port Keats ALT", "NA", "NA", "Ahakeye ALT",  "NA",  "NA",  "Warrabri & Iliyarne ALTs" ),
                 CROPS = c("Rainfed/Irrigated", "Rainfed/Irrigated", "Rainfed/Irrigated", "Irrigated", "NA",  "NA", "NA", "Irrigated",  "NA",  "Rainfed/Irrigated",  "Irrigated",  "Rainfed/Irrigated", "Irrigated", "Limited Suitability", "Irrigated",  "NA",  "NA",  "Irrigated"))

# Merge Dataframes
study_shapes<-merge(sf,df)


study_shapes$SURVEY_ID <- substr(study_shapes$CODE, 1, 5) 
study_shapes<-study_shapes %>%
  relocate(SURVEY_ID, .after = SURVEYTYPE)


# Combine into a named list
shapefiles_list <- list(
  study_shapes = study_shapes,
  landunits = landunits
)

# Define UI
ui <- fluidPage(
  titlePanel("Interactive Map for Agricultural Land Suitability Study"),
  sidebarLayout(
    sidebarPanel(
      width = 3,
      selectInput("layer", "Select Layer:", choices = names(shapefiles_list), selected = "study_shapes"),
      uiOutput("popup_field_ui"),
      br(),
      tags$p("Use `SURVEY_ID` before changing the layer selection.", style = "colour: #555; font-style: italic;")
    ), 
    
    mainPanel(
      leafletOutput("mymap", width="100%", height=800), 
      br(),
      DT::DTOutput("table")

    )
  )
)

# Define server
server <- function(input, output, session) {
  
  # Reactive expression to get the selected shapefile
  selected_shapefile <- reactive({
    shapefiles_list[[input$layer]]
  })
  
  # Dynamically update popup field choices based on selected layer
  
  output$popup_field_ui <- renderUI({
    req(selected_shapefile())
    choices <- names(selected_shapefile())
    default_choice <- if ("SURVEY_ID" %in% choices) "SURVEY_ID" else choices[1]
    
    selectInput("popup_field", "Select Popup Options:",
                choices = choices, selected = default_choice)
  })
  
  
  # Render base map
  output$mymap <- renderLeaflet({
    leaflet() %>% 
      addTiles() %>%   
      addProviderTiles(
        providers$Esri.WorldImagery,
        options = providerTileOptions(opacity = 0.5)
      ) %>%
      setView(133.2, -19.23, zoom = 5)
  })
  
  
  
  output$table <- DT::renderDT({
    req(selected_shapefile())
    
    # Drop geometry column
    data <- sf::st_drop_geometry(selected_shapefile())
    
    # Show all columns
    DT::datatable(data)
  })

  
  # Observe and update map when layer or popup field changes
  observe({
    req(selected_shapefile(), input$popup_field)
    leafletProxy("mymap") %>%
      clearShapes() %>%
      addPolygons(data = selected_shapefile(),
                  popup = ~as.character(get(input$popup_field)),
                  label = ~as.character(get(input$popup_field)),
                  color = "red",
                  weight = 1,
                  fillOpacity = 0.5)
  })
}

# Run the app
shinyApp(ui = ui, server = server)

