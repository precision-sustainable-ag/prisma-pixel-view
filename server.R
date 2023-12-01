library(shiny)
library(terra)
library(sf)
library(dplyr)
library(ggplot2)
library(leafem)



server <- function(input, output, session) {
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
      "vector_show_hide", NULL,
      choices = basename(v$names),
      selected = basename(v$names),
      multiple = T
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
        ~leafletProxy("map") %>% 
          addGeoJSON(
            geojson = readr::read_lines(.x) %>% paste(collapse = "\n"),
            layerId = basename(.x),
            fillOpacity = 0.1,
            weight = 2,
            opacity = 0.3
          )
      )
      
      purrr::map(
        v$names[!to_show],
        ~leafletProxy("map") %>%
          removeGeoJSON(basename(.x))
      )
    }
  )
  
  clicked_coords <- reactive({
    st_point(
      c(input$map_click[["lng"]], input$map_click[["lat"]])
    ) %>% 
      st_sfc(crs = 4326) %>% 
      st_transform(r_crs()) %>% 
      st_coordinates()
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
      scale_y_continuous(
        #breaks = function(lmts) {seq(0, max(lmts) + 0.05, by = 0.05)}
        labels = function(brk) {sprintf("%4.2f", brk)}
      ) +
      scale_x_continuous(
        breaks = seq(400, 2400, by = 200),
      ) +
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
