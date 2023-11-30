library(shiny)
library(terra)
library(sf)
library(dplyr)
library(ggplot2)



server <- function(input, output) {
  path <- "rasters/PRS_L2D_STD_20221210_HCO_VNIR_SWIR_mosaic_updated_georegistration.tif"
  
  raster <- rast(path)
  r_crs <- crs(raster, describe = T)$code
  
  #raster_stars <- stars::read_stars(path)
  
  output$map <- renderLeaflet({
    input$reset
    
    leaflet() %>% 
      addProviderTiles(
        "CartoDB.Positron",
        options = providerTileOptions(opacity = 1, attribution = "")
      ) %>% 
      leafem::addGeotiff(
        file = path, bands = 40, opacity = 0.6, 
        colorOptions = leafem::colorOptions(
          palette = c("#00000000", rev(viridis::magma(255)))
        ),
        resolution = 72
      ) %>% 
      addProviderTiles(
        "CartoDB.PositronOnlyLabels",
        options = providerTileOptions(opacity = 0.75, attribution = "")
      )
  })
  
  clicked_coords <- reactive({
    st_point(
      c(input$map_click[["lng"]], input$map_click[["lat"]])
    ) %>% 
      st_sfc(crs = 4326) %>% 
      st_transform(as.numeric(r_crs)) %>% 
      st_coordinates()
  })
  
  output$plot <- renderPlot({
    
    req(is.numeric(input$map_click[["lng"]]))
    req(input$wv_labels)
    
    loc <- clicked_coords()

    vals <- 
      extract(raster, loc, cell = T) %>% 
      dplyr::select(cell, matches("[.0-9]+$")) %>%
      rename_all(~stringr::str_extract(., "cell$|[.0-9]+$")) %>% 
      tidyr::pivot_longer(cols = -cell) %>% 
      rename(band = name, reflectance = value) 
      
    if (input$wv_labels == "Sequential") {
      vals <- mutate(vals, 
        band = as.numeric(band),
        wv = wavelengths[band],
        src = wv_src[band]
        )
    } else if (input$wv_labels == "Numeric") {
      vals <- mutate(vals, 
        band = as.numeric(band),
        wv = band,
        src = 1
      )
    }
    
    title <- unique(vals$cell)
    
    ggplot(vals, aes(wv, reflectance)) +
      geom_line(aes(group = src)) +
      scale_y_continuous(
        #breaks = function(lmts) {seq(0, max(lmts) + 0.05, by = 0.05)}
        labels = function(brk) {sprintf("%4.2f", brk)}
      ) +
      scale_x_continuous(
        breaks = seq(400, 2400, by = 200),
      ) +
      labs(
        title = paste("Cell number:", title, collapse = ""),
        subtitle = basename(path),
        x = "wavelength (nm)",
        y = "reflectance"
      ) +
      theme_bw() +
      theme(
        title = element_text(size = 14),
        axis.text = element_text(size = 14),
        plot.subtitle = element_text(size = 11)
        )
  })
  
  output$selected_point <-  renderUI({
    
    req(is.numeric(input$map_click[["lng"]]))
    
    gj <- make_geojson(
      input$map_click[["lng"]], 
      input$map_click[["lat"]]
      ) 
    
    loc <- cellFromXY(raster, clicked_coords())
    
    wellPanel(
      h4(
        "Copy cell into R, or use ", 
        code(glue::glue("raster[{loc}]"))
        ),
      pre(glue::glue("st_read('{gj}') %>%\n  st_transform({r_crs})"))
    )
  })
  
}
