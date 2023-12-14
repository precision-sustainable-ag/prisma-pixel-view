library(shiny)
library(leaflet)
library(shinyFiles)

manifest <- 
  tags$head(
    tags$link(rel="apple-touch-icon", href="apple-touch-icon.png"),
    tags$link(rel="icon", type="image/png", sizes="32x32", href="favicon-32x32.png"),
    tags$link(rel="icon", type="image/png", sizes="16x16", href="favicon-16x16.png"),
    tags$link(rel="manifest", href="site.webmanifest")
  )

title_component <- 
  span(
    tags$img(
      src = "PSAlogo-text.png", 
      style = "height: 1.5em; vertical-align: middle;"
    ) %>% 
      tags$a(
        href = "https://www.precisionsustainableag.org",
        target="_blank", rel="noopener noreferrer"
      ),
    "PRISMA Pixel View",
    tags$a(
      icon("github"), 
      href = "https://github.com/precision-sustainable-ag/prisma-pixel-view", 
      class = "button", 
      target="_blank", rel="noopener noreferrer", 
      style = "float: right;"
      )
  ) %>% 
  titlePanel(windowTitle = "PRISMA Pixel View")

  

rast_picker <- 
  shinyFilesButton(
    'raster_file', 
    label = 'Raster image', 
    title = 'Select a raster',
    multiple = F
    ) %>% 
  div(style = "padding-bottom: 1em;")

rast_comp_picker <- 
  shinyFilesButton(
    'raster_comp_file', 
    label = 'Optional ref. image', 
    title = 'Select a raster',
    multiple = F
  ) %>% 
  div(style = "padding-bottom: 1em") %>% 
  column(8, .) %>% 
  fluidRow(uiOutput("comp_show_hide"))

vect_picker <- 
  shinyFilesButton(
    'vector_file', 
    label = 'Points and/or polygons', 
    title = 'Select vector file(s)',
    multiple = T
  ) 


wv_picker <- 
  radioButtons(
    "wv_labels", "How are the bands labelled?", 
    choiceNames = c("Original PRISMA band names", "Numeric Wavelengths"),
    choiceValues = c("Sequential", "Numeric"),
    selected = character(0)
  )

wv_comp_picker <- 
  radioButtons(
    "wv_comp_labels", "How are the bands labelled?", 
    choiceNames = c("Original PRISMA band names", "Numeric Wavelengths"),
    choiceValues = c("Sequential", "Numeric"),
    selected = character(0)
  )

band_legend_component <- 
  fluidRow(
    column(8), 
    uiOutput("legend_show_hide")
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
    "Paste coords or cell # to jump",
    placeholder = "e.g. 39.03,-76.93 (any order)"
    )

map_component <- 
  leafletOutput("map", height = "60vh") %>% 
  shinycssloaders::withSpinner(type = 7, size = 2)
  

ui <- fluidPage(
  manifest,
  
  title_component,
  # actionButton("browser", "browser"),
  wellPanel(
    fluidRow(
      column(4, rast_picker, wv_picker, band_legend_component),
      column(4, rast_comp_picker, wv_comp_picker),
      column(4, vect_picker, uiOutput("vector_selections"), jump_box), 
    )
  ),

  fluidRow(
    column(6, map_component),
    column(6, plot_panel, uiOutput("plot_helper_text"))
  ),
  
  br(),
  
  uiOutput("selected_point")
  

)

