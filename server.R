library(shiny)
library(terra)
library(sf)
library(dplyr)
library(ggplot2)
library(leafem)



server <- function(input, output, session) {

  observeEvent(input$browser,{
    browser()
  })
  
  shinyFileChoose(
    input, 'raster_file', 
    roots = c(wd = '.'), 
    filetypes = c('tif')
    )
  
  shinyFileChoose(
    input, 'vector_file', 
    roots = c(wd = '.'), 
    filetypes = c('shp', 'geojson')
  )
  
  path <- reactive({
    parseFilePaths(c(wd = "."), input$raster_file)[["datapath"]]
  })
  
  raster <- reactive({ rast(path()) })
  r_crs <- reactive({ as.numeric(crs(raster(), describe = T)[["code"]]) })
  

  vector_path <- reactive({
    parseFilePaths(c(wd = "."), input$vector_file)[["datapath"]]
  })
  
  v <- reactiveValues(names = character(0))
  observeEvent(
    vector_path(), {
      v$names <- 
        c(v$names, vector_path()) %>% 
        unique()
    }
  )
  
  
  output$vector_selections <- renderUI({
    selectizeInput(
      "vector_show_hide", HTML("&nbsp;"),
      choices = basename(v$names),
      selected = basename(v$names),
      multiple = T,
      options = list(
        plugins = list("remove_button")
      )
    )
  })
  
  output$map <- renderLeaflet({
    req(path())
    
    input$reset

    bb <- 
      as.polygons(raster(), extent = T) %>% 
      st_as_sf() %>% 
      st_transform(4326) %>% 
      st_bbox()
    
    leaflet() %>%
      addProviderTiles(
        "CartoDB.Positron",
        options = providerTileOptions(opacity = 1, attribution = "")
      ) %>%
      addGeotiff(
        file = path(), bands = 1, opacity = 0.6, # TODO update bands with picker
        colorOptions = colorOptions(
          palette = c("#00000000", rev(viridis::magma(255)))
        ),
        resolution = 72
      ) %>%
      addProviderTiles(
        "CartoDB.PositronOnlyLabels",
        options = providerTileOptions(opacity = 0.75, attribution = "")
      ) %>% 
      addHomeButton(
        ext = bb, position = "topleft",
        group = "↺ Reset"
      )
  })
  
  
  # what a silly hack to make this fire when the list becomes empty
  observeEvent(
    c(input$vector_show_hide, "___placeholder___"), {
      req(length(v$names) > 0)
      
      to_show <- basename(v$names) %in% input$vector_show_hide

      purrr::map(
        v$names[to_show],
        ~{
          obj <- sf::read_sf(.x)
          
          obj_point <- obj %>% 
            filter(sf::st_is(geometry, c("POINT", "MULTIPOINT")))
          
          obj_poly <- obj %>% 
            filter(sf::st_is(geometry, c("POLYGON", "MULTIPOLYGON")))
          
          
          leafletProxy("map") %>% 
            addPolygons(
              data = obj_poly, 
              fillOpacity = 0.1, weight = 2, opacity = 0.3,
              group = paste0(basename(.x), "_poly")#,
              #color = "#00B3FF"
              ) %>% 
            addCircleMarkers(
              data = obj_point, 
              fillOpacity = 0.1, weight = 3, opacity = 0.3, radius = 5,
              group = paste0(basename(.x), "_point")#,
              #color = "#4C00FF"
              )
        }
      )
      
      purrr::map(
        v$names[!to_show],
        ~leafletProxy("map") %>%
          clearGroup(paste0(basename(.x), c("_point", "_poly")))
      )
    }
  )
  
  typed_coords <- reactiveValues(pt = NULL)
  
  observeEvent(
    input$jump_text, {
      
    jump_c <- extract_coords_from_string(input$jump_text)

    if (length(jump_c) == 0) {
      leafletProxy("map") %>%
        removeMarker("typed_point")
    }
    
    req(length(jump_c) == 2)

    if (any(abs(jump_c) > 180)) {
      # reproject 
      #   (move this logic into put_ll_in_order, add crs arg)
    }

    mc <- input$map_center[c("lng", "lat")] %>% unlist()
    coords <- put_ll_in_order(jump_c, mc)
    
    leafletProxy("map") %>%
      addCircleMarkers(
        lng = coords[1], lat = coords[2],
        layerId = "typed_point",
        color = "black"
      )

    typed_coords$pt <- reproject_coords(coords, 4326, r_crs())
  })
  
  observeEvent(
    input$map_click, {
      updateTextInput(
        inputId = "jump_text",
        value = ""
      )
      
      typed_coords$pt <- NULL
    }
  )
  
  # TODO: If something is pasted before the map is clicked on init,
  #   the graph doesn't fire. But behavior is normal after map click.
  clicked_coords <- reactive({
    if (is.null(typed_coords$pt)) {
      reproject_coords(
        c(input$map_click[["lng"]], input$map_click[["lat"]]),
        4326, r_crs()
      )
    } else {
      typed_coords$pt
    }
  })
  
  reflectance_at_point <- reactive({
    req(is.numeric(input$map_click[["lng"]]))
    req(input$wv_labels)
    
    vals <- 
      extract(raster(), clicked_coords(), cell = T) %>% 
      dplyr::select(cell, matches("[.0-9]+$")) %>%
      rename_all(~stringr::str_extract(., "cell$|[.0-9]+$")) %>% 
      tidyr::pivot_longer(cols = -cell) %>% 
      rename(band = name, reflectance = value) 

    if (input$wv_labels == "Sequential") {
      vals <- mutate(
        vals, 
        band = as.numeric(band),
        wv = wavelengths[band],
        src = wv_src[band]
      )
    } else if (input$wv_labels == "Numeric") {
      vals <- mutate(
        vals, 
        band = as.numeric(band),
        wv = band,
        src = 1
      )
    }
    
    vals
  })
  
  
  plot_ranges <- reactiveValues(x = NULL, y = NULL)
  
  # When a double-click happens, check if there's a brush on the plot.
  # If so, zoom to the brush bounds; if not, reset the zoom.
  observeEvent(
    input$plot_dblclick, {
      brush <- input$plot_brush
      # sets these to NULL when `brush` is NULL
      plot_ranges$x <- c(brush$xmin, brush$xmax)
      plot_ranges$y <- c(brush$ymin, brush$ymax)
    }
  )
  
  
  output$plot <- renderPlot({
    req(path())
    req(is.numeric(input$map_click[["lng"]]))
    req(input$wv_labels)
    
    focus_tag <- if (!is.null(plot_ranges$x)) { ", subset of values" }
    cell_id <- unique(reflectance_at_point()$cell)
    title <- paste0("Cell number: ", cell_id, focus_tag, collapse = "")
    
    ggplot(reflectance_at_point(), aes(wv, reflectance)) +
      geom_line(aes(group = src)) +
      scale_y_continuous(labels = y_labels) +
      scale_x_continuous(breaks = x_breaks) +
      labs(
        title = title,
        subtitle = basename(path()),
        x = "wavelength (nm)",
        y = "reflectance"
      ) +
      coord_cartesian(xlim = plot_ranges$x, ylim = plot_ranges$y) +
      theme_bw() +
      theme(
        title = element_text(size = 14),
        axis.text = element_text(size = 14),
        plot.subtitle = element_text(size = 11)
        )
  })
  
  output$plot_helper_text <- renderUI({
    req(path())
    req(is.numeric(input$map_click[["lng"]]))
    req(input$wv_labels)
    
    div("Click-and-drag to brush a region, double-click to set/reset selection.")
  })
  
  output$selected_point <-  renderUI({
    
    req(is.numeric(input$map_click[["lng"]]))
    
    gj <- make_geojson(
      input$map_click[["lng"]], 
      input$map_click[["lat"]]
      ) 
    
    loc <- cellFromXY(raster(), clicked_coords())
    
    wellPanel(
      h4(
        "Copy cell into R, or use ", 
        code(glue::glue("raster[{loc}]"))
        ),
      pre(glue::glue("st_read('{gj}') %>%\n  st_transform({r_crs()})"))
    )
  })
  
}
