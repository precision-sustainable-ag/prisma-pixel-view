library(shiny)
library(leaflet)

ui <- fluidPage(
  
  titlePanel("PRISMA Pixel View"),
  

  wellPanel(
    actionButton("reset", "Reset View"),
    radioButtons(
      inputId = "wv_labels", "How are the bands labelled?", 
      choiceNames = c("Original PRISMA band names", "Numeric Wavelengths"),
      choiceValues = c("Sequential", "Numeric")
      )
  ),
  
  fluidRow(
    column(6, leafletOutput("map", height = "60vh")),
    column(6, plotOutput("plot", height = "60vh"))
  ),
  
  br(),
  
  uiOutput("selected_point")
  

)

