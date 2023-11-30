library(shiny)
library(leaflet)

ui <- fluidPage(
  
  titlePanel("PRISMA Pixel View"),
  

  wellPanel(
    actionButton("reset", "Reset View"),
    "Some interactive tools for selecting things..."
  ),
  
  fluidRow(
    column(6, leafletOutput("map", height = "60vh")),
    column(6, plotOutput("plot", height = "60vh"))
  ),
  
  br(),
  
  uiOutput("selected_point")
  

)

