library(shiny)
library(leaflet)
library(shinyFiles)

rast_picker <- 
  shinyFilesButton(
    'raster_file', 
    label = 'Choose local raster', 
    title = 'Please select a file',
    multiple = F
    )

wv_picker <- 
  radioButtons(
    "wv_labels", "How are the bands labelled?", 
    choiceNames = c("Original PRISMA band names", "Numeric Wavelengths"),
    choiceValues = c("Sequential", "Numeric")
  )



ui <- fluidPage(
  
  titlePanel("PRISMA Pixel View"),

  wellPanel(
    fluidRow(
      column(2, actionButton("reset", "Reset View")),
      column(2, rast_picker),
      column(4), # TODO placeholder
      column(4, wv_picker)
    )
  ),

  fluidRow(
    column(6, leafletOutput("map", height = "60vh")),
    column(6, plotOutput("plot", height = "60vh"))
  ),
  
  br(),
  
  uiOutput("selected_point")
  

)

