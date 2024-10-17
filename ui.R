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

rast_comp0_picker <- 
  shinyFilesButton(
    'raster_comp0_file', 
    label = 'Optional ref. image', 
    title = 'Select a raster',
    multiple = F
  ) %>% 
  div(style = "padding-bottom: 1em") %>% 
  column(8, .) %>% 
  fluidRow(uiOutput("comp0_show_hide"))

rast_comp1_picker <- 
  shinyFilesButton(
    'raster_comp1_file', 
    label = 'Optional ref. image', 
    title = 'Select a raster',
    multiple = F
  ) %>% 
  div(style = "padding-bottom: 1em") %>% 
  column(8, .) %>% 
  fluidRow(uiOutput("comp1_show_hide"))

rast_comp2_picker <- 
  shinyFilesButton(
    'raster_comp2_file', 
    label = 'Optional ref. image', 
    title = 'Select a raster',
    multiple = F
  ) %>% 
  div(style = "padding-bottom: 1em") %>% 
  column(8, .) %>% 
  fluidRow(uiOutput("comp2_show_hide"))

rast_comp3_picker <- 
  shinyFilesButton(
    'raster_comp3_file', 
    label = 'Optional ref. image', 
    title = 'Select a raster',
    multiple = F
  ) %>% 
  div(style = "padding-bottom: 1em") %>% 
  column(8, .) %>% 
  fluidRow(uiOutput("comp3_show_hide"))

vect_picker <- 
  shinyFilesButton(
    'vector_file', 
    label = 'Points and/or polygons', 
    title = 'Select vector file(s)',
    multiple = T
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
      column(4, rast_picker, band_legend_component),
      column(4, rast_comp0_picker),
      column(4, vect_picker, uiOutput("vector_selections"), jump_box), 
    ),
    tags$hr(),
    fluidRow(
      column(4, rast_comp1_picker), #TODO display name and color for plot next to file button
      column(4, rast_comp2_picker),
      column(4, rast_comp3_picker),
    )
  ),

  fluidRow(
    column(6, map_component),
    column(6, plot_panel, uiOutput("plot_helper_text"))
  ),
  
  br(),
  
  uiOutput("selected_point")
  

)

