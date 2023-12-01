library(shiny)
library(leaflet)
library(shinyFiles)

rast_picker <- 
  shinyFilesButton(
    'raster_file', 
    label = 'Choose local raster', 
    title = 'Select a raster',
    multiple = F
    )

vect_picker <- 
  shinyFilesButton(
    'vector_file', 
    label = 'Choose local points or polygons', 
    title = 'Select vector file(s)',
    multiple = T
  )

wv_picker <- 
  radioButtons(
    "wv_labels", "How are the bands labelled?", 
    choiceNames = c("Original PRISMA band names", "Numeric Wavelengths"),
    choiceValues = c("Sequential", "Numeric")
  )


plot_panel <- 
  plotOutput(
    "plot", height = "60vh",
    dblclick = "plot_dblclick",
    brush = brushOpts(
      id = "plot_brush",
      resetOnNew = T
    )
  )


ui <- fluidPage(
  
  titlePanel("PRISMA Pixel View"),

  wellPanel(
    fluidRow(
      column(4, rast_picker, vect_picker),
      column(4, uiOutput("vector_selections")), 
      column(4, wv_picker)
    )
  ),

  fluidRow(
    column(6, leafletOutput("map", height = "60vh")),
    column(6, plot_panel, uiOutput("plot_helper_text"))
  ),
  
  br(),
  
  uiOutput("selected_point")
  

)

