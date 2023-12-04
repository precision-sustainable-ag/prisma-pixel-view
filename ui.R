library(shiny)
library(leaflet)
library(shinyFiles)

rast_picker <- 
  shinyFilesButton(
    'raster_file', 
    label = 'Choose local raster', 
    title = 'Select a raster',
    multiple = F
    ) %>% 
  div(style = "padding-bottom: 1em;")

vect_picker <- 
  shinyFilesButton(
    'vector_file', 
    label = 'Choose local points or polygons', 
    title = 'Select vector file(s)',
    multiple = T
  ) %>% 
  div(style = "padding-bottom: 1em;")


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

jump_box <- 
  textInput(
    "jump_text", 
    "Paste coords to jump",
    placeholder = "e.g. 39.03,-76.93 (any order)"
    )


ui <- fluidPage(
  
  titlePanel("PRISMA Pixel View"),
  #actionButton("browser", "browser"),
  wellPanel(
    fluidRow(
      column(4, rast_picker, jump_box),
      column(4, vect_picker, uiOutput("vector_selections")), 
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

