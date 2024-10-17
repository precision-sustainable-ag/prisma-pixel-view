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
  
  
  
  ## File choosers ----
  shinyFileChoose(
    input, 'raster_file', 
    roots = c(wd = '.'), 
    filetypes = c('tif', '', 'envi')
    )
  
  shinyFileChoose(
    input, 'raster_comp0_file', 
    roots = c(wd = '.'), 
    filetypes = c('tif', '', 'envi')
  )
  
  shinyFileChoose(
    input, 'raster_comp1_file', 
    roots = c(wd = '.'), 
    filetypes = c('tif', '', 'envi')
  )
  
  shinyFileChoose(
    input, 'raster_comp2_file', 
    roots = c(wd = '.'), 
    filetypes = c('tif', '', 'envi')
  )
  
  shinyFileChoose(
    input, 'raster_comp3_file', 
    roots = c(wd = '.'), 
    filetypes = c('tif', '', 'envi')
  )
  
  shinyFileChoose(
    input, 'vector_file', 
    roots = c(wd = '.'), 
    filetypes = c('shp', 'geojson')
  )
  
  
  
  # ref Path, raster, name ----
  path <- reactive({
    parseFilePaths(c(wd = "."), input$raster_file)[["datapath"]]
  })
  
  raster <- reactive({     
    if (!length(path())) { return(NULL) }
    rast(path()) 
  })
  r_crs <- reactive({ as.numeric(crs(raster(), describe = T)[["code"]]) })

  output$raster_name = renderUI({
    req(path())
    name_label(basename(path()), cols[1])
    })

  
  
  # 0 Path, raster, name ----
  path_comp0 <- reactive({
    if (!is.list(input$raster_comp0_file)) { return(NULL) }
    parseFilePaths(c(wd = "."), input$raster_comp0_file)[["datapath"]]
  })
  
  raster_comp0 <- reactive({ 
    if (!length(path_comp0())) { return(NULL) }
    rast(path_comp0()) 
    })
  
  output$comp0_name = renderUI({
    req(path_comp0())
    name_label(basename(path_comp0()), cols[2])
  })
  
  
  
  # 1 Path, raster, name ----
  path_comp1 <- reactive({
    if (!is.list(input$raster_comp1_file)) { return(NULL) }
    parseFilePaths(c(wd = "."), input$raster_comp1_file)[["datapath"]]
  })
  
  raster_comp1 <- reactive({ 
    if (!length(path_comp1())) { return(NULL) }
    rast(path_comp1()) 
  })
  
  output$comp1_name = renderUI({
    req(path_comp1())
    name_label(basename(path_comp1()), cols[3])
  })
  
  
  
  # 2 Path, raster, name ----
  path_comp2 <- reactive({
    if (!is.list(input$raster_comp2_file)) { return(NULL) }
    parseFilePaths(c(wd = "."), input$raster_comp2_file)[["datapath"]]
  })
  
  raster_comp2 <- reactive({ 
    if (!length(path_comp2())) { return(NULL) }
    rast(path_comp2()) 
  })
  
  output$comp2_name = renderUI({
    req(path_comp2())
    name_label(basename(path_comp2()), cols[4])
  })
  
  
  
  # 3 Path, raster, name ----
  path_comp3 <- reactive({
    if (!is.list(input$raster_comp3_file)) { return(NULL) }
    parseFilePaths(c(wd = "."), input$raster_comp3_file)[["datapath"]]
  })
  
  raster_comp3 <- reactive({ 
    if (!length(path_comp3())) { return(NULL) }
    rast(path_comp3()) 
  })
  
  output$comp3_name = renderUI({
    req(path_comp3())
    name_label(basename(path_comp3()), cols[5])
  })
  
  
  
  # 0 Show/hide ----
  output$comp0_show_hide <- renderUI({
    req(input$raster_comp0_file)
    div(
      actionButton(
        "comp0_show_hide",
        label = "",
        icon = icon("chart-line")
      ),
      actionButton(
        "comp0_bn",
        label = "",
        icon = icon("down-left-and-up-right-to-center")
      ),
      title = "Show/hide/normalize ref. spectrum"
    ) %>% 
      column(4, .)
  })
  
  observeEvent(
    input$comp0_show_hide, 
    updateActionButton(
      inputId = "comp0_show_hide",
      icon = list(
        icon("chart-line", style = "filter: invert(80%);"), 
        icon("chart-line")
      )[[(input$comp0_show_hide %% 2) + 1]]
    )
  )
  
  observeEvent(
    input$comp0_bn, 
    updateActionButton(
      inputId = "comp0_bn",
      icon = list(
        icon("down-left-and-up-right-to-center"),
        icon("up-right-and-down-left-from-center", style = "filter: invert(80%);")
      )[[(input$comp0_bn %% 2) + 1]]
    )
  )
  
  
  # 1 Show/hide ----
  output$comp1_show_hide <- renderUI({
    req(input$raster_comp1_file)
    div(
      actionButton(
        "comp1_show_hide",
        label = "",
        icon = icon("chart-line")
      ),
      actionButton(
        "comp1_bn",
        label = "",
        icon = icon("down-left-and-up-right-to-center")
      ),
      title = "Show/hide/normalize ref. spectrum"
    ) %>% 
      column(4, .)
  })
  
  observeEvent(
    input$comp1_show_hide, 
    updateActionButton(
      inputId = "comp1_show_hide",
      icon = list(
        icon("chart-line", style = "filter: invert(80%);"), 
        icon("chart-line")
      )[[(input$comp1_show_hide %% 2) + 1]]
    )
  )
  
  observeEvent(
    input$comp1_bn, 
    updateActionButton(
      inputId = "comp1_bn",
      icon = list(
        icon("down-left-and-up-right-to-center"),
        icon("up-right-and-down-left-from-center", style = "filter: invert(80%);")
      )[[(input$comp1_bn %% 2) + 1]]
    )
  )
  
  
  
  # 2 Show/hide ----
  output$comp2_show_hide <- renderUI({
    req(input$raster_comp2_file)
    div(
      actionButton(
        "comp2_show_hide",
        label = "",
        icon = icon("chart-line")
      ),
      actionButton(
        "comp2_bn",
        label = "",
        icon = icon("down-left-and-up-right-to-center")
      ),
      title = "Show/hide/normalize ref. spectrum"
    ) %>% 
      column(4, .)
  })
  
  observeEvent(
    input$comp2_show_hide, 
    updateActionButton(
      inputId = "comp2_show_hide",
      icon = list(
        icon("chart-line", style = "filter: invert(80%);"), 
        icon("chart-line")
      )[[(input$comp2_show_hide %% 2) + 1]]
    )
  )
  
  observeEvent(
    input$comp2_bn, 
    updateActionButton(
      inputId = "comp2_bn",
      icon = list(
        icon("down-left-and-up-right-to-center"),
        icon("up-right-and-down-left-from-center", style = "filter: invert(80%);")
      )[[(input$comp2_bn %% 2) + 1]]
    )
  )
  
  
  # 3 Show/hide ----
  output$comp3_show_hide <- renderUI({
    req(input$raster_comp3_file)
    div(
      actionButton(
        "comp3_show_hide",
        label = "",
        icon = icon("chart-line")
      ),
      actionButton(
        "comp3_bn",
        label = "",
        icon = icon("down-left-and-up-right-to-center")
      ),
      title = "Show/hide/normalize ref. spectrum"
    ) %>% 
      column(4, .)
  })
  
  observeEvent(
    input$comp3_show_hide, 
    updateActionButton(
      inputId = "comp3_show_hide",
      icon = list(
        icon("chart-line", style = "filter: invert(80%);"), 
        icon("chart-line")
      )[[(input$comp3_show_hide %% 2) + 1]]
    )
  )
  
  observeEvent(
    input$comp3_bn, 
    updateActionButton(
      inputId = "comp3_bn",
      icon = list(
        icon("down-left-and-up-right-to-center"),
        icon("up-right-and-down-left-from-center", style = "filter: invert(80%);")
      )[[(input$comp3_bn %% 2) + 1]]
    )
  )
  
  
  
  # ref Show/hide ----
  output$legend_show_hide <- renderUI({
    req(input$raster_file)
    div(
      actionButton(
        "legend_show_hide",
        label = "",
        icon = icon("list")
      ), 
      actionButton(
        "raster_bn",
        label = "",
        icon = icon("down-left-and-up-right-to-center")
      ),
      title = "Show/hide legend, normalize brightness"
    ) %>% 
      column(4, .)
  })
  

  
  observeEvent(
    input$legend_show_hide, 
    updateActionButton(
      inputId = "legend_show_hide",
      icon = list(
        icon("list"), 
        icon("list", style = "filter: invert(80%);")
      )[[(input$legend_show_hide %% 2) + 1]]
    )
  )
  
  observeEvent(
    input$raster_bn, 
    updateActionButton(
      inputId = "raster_bn",
      icon = list(
        icon("down-left-and-up-right-to-center"),
        icon("up-right-and-down-left-from-center", style = "filter: invert(80%);")
      )[[(input$raster_bn %% 2) + 1]]
    )
  )
  
  
  # Vectors ----
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
      "vector_show_hide", NULL, #HTML("&nbsp;"),
      choices = basename(v$names),
      selected = basename(v$names),
      multiple = T,
      options = list(
        plugins = list("remove_button")
      )
    )
  })
  
  
  
  # Map ----
  output$map <- renderLeaflet({
    req(path())
    
    bb <- 
      as.polygons(raster(), extent = T) %>% 
      st_as_sf() %>% 
      st_transform(4326) %>% 
      st_bbox()
    
    band = if(length(names(raster())) >= 40) { 40 } else { 1 }
    
    leaflet() %>%
      addProviderTiles(
        "CartoDB.Positron",
        options = providerTileOptions(opacity = 1, attribution = "")
      ) %>%
      addGeoRaster(
        stars::st_as_stars(raster())[,,,band], # TODO update bands with picker
        colorOptions = colorOptions(
          palette = c("#00000000", rev(viridis::magma(255)))
        ),
        resolution = 72, opacity = 0.6
      ) %>% 
      addProviderTiles(
        "CartoDB.PositronOnlyLabels",
        options = providerTileOptions(opacity = 0.75, attribution = "")
      ) %>% 
      addHomeButton(
        ext = bb, position = "topleft",
        group = "↺ Reset"
      ) %>% 
      fitBounds(bb[[1]], bb[[2]], bb[[3]], bb[[4]])
  })
  
  
  
  # Legend ----
  observeEvent(
    input$legend_show_hide, {
      req(raster())
      #browser()
      rv <- terra::minmax(raster()[[1]]) %>% as.numeric() # TODO update band
    
      if (input$legend_show_hide %% 2) {
        leafletProxy("map") %>% 
          addLegend(
            bins = scales::pretty_breaks()(rv) %>% 
              scales::rescale(rev(rv), rv) %>% 
              rev(),
            values = rv,
            pal = leaflet::colorNumeric((viridis::magma(255)), rv),
            labFormat = labelFormat(
              transform = function(x) {scales::rescale(x, rev(rv), rv)}
              ),
            layerId = "legend",
            opacity = 0.8
          ) 
      } else {
        leafletProxy("map") %>% 
          removeControl("legend")
      }
    }
  )

  
  
  # Vectors show/hide ----
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
  
  
  
  # Jump coords ----
  typed_coords <- reactiveValues(pt = NULL)
  
  observeEvent(
    input$jump_text, {
      
    jump_c <- extract_coords_from_string(input$jump_text)
    coords <- NaN

    if (length(jump_c) == 0) {
      leafletProxy("map") %>%
        removeMarker("typed_point")
    }
    
    if (length(jump_c) == 1) {
      coords <- 
        terra::xyFromCell(raster(), jump_c) %>% 
        reproject_coords(r_crs(), 4326)
    }
    
    if (length(jump_c) == 2) {
      mc <- input$map_center[c("lng", "lat")] %>% unlist()
      coords <- put_ll_in_order(jump_c, mc, r_crs())
    } 
    
    req(all(is.finite(coords)))
    
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
  
  
  
  # Click coords ----
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
  
  
  
  # ref Reflectance ----
  reflectance_at_point <- reactive({
    req(is.numeric(input$map_click[["lng"]]))

    vals <- 
      extract(raster(), clicked_coords(), cells = T)
    
    new_names = c("cell", seq.int(length(names(vals)) - 1))
    
    vals <- 
      vals %>% 
      purrr::set_names(new_names) %>% 
      tidyr::pivot_longer(cols = -cell) %>% 
      rename(band = name, reflectance = value) %>% 
      mutate(
        band = as.numeric(band),
        wv = wavelengths[band],
        src = wv_src[band],
        reflectance = bnorm(reflectance, input$raster_bn %% 2)
      )
    
    vals
  })
  
  
  
  
  # 0 Reflectance ----
  comp0_geom_line <- reactive({
    req(is.numeric(input$map_click[["lng"]]))

    if (
      !length(raster_comp0()) || 
        (input$comp0_show_hide %% 2)
      ) { return(NULL) }
    
    vals <- 
      extract(raster_comp0(), clicked_coords(), cells = T)
    
    new_names = c("cell", seq.int(length(names(vals)) - 1))
    
    vals <- 
      vals %>% 
      purrr::set_names(new_names) %>% 
      tidyr::pivot_longer(cols = -cell) %>% 
      rename(band = name, reflectance = value) %>% 
      mutate(
        band = as.numeric(band),
        wv = wavelengths[band],
        src = wv_src[band],
        reflectance = bnorm(reflectance, input$comp0_bn %% 2)
      )
    
    
    geom_line(data = vals, color = cols[2])
  })
  
  
  
  # 1 Reflectance ----
  comp1_geom_line <- reactive({
    req(is.numeric(input$map_click[["lng"]]))
    
    if (
      !length(raster_comp1()) || 
      (input$comp1_show_hide %% 2)
    ) { return(NULL) }
    
    vals <- 
      extract(raster_comp1(), clicked_coords(), cells = T)
    
    new_names = c("cell", seq.int(length(names(vals)) - 1))
    
    vals <- 
      vals %>% 
      purrr::set_names(new_names) %>% 
      tidyr::pivot_longer(cols = -cell) %>% 
      rename(band = name, reflectance = value) %>% 
      mutate(
        band = as.numeric(band),
        wv = wavelengths[band],
        src = wv_src[band],
        reflectance = bnorm(reflectance, input$comp1_bn %% 2)
      )
    
    geom_line(data = vals, color = cols[3])
  })
  
  
  
  # 2 Reflectance ----
  comp2_geom_line <- reactive({
    req(is.numeric(input$map_click[["lng"]]))
    
    if (
      !length(raster_comp2()) || 
      (input$comp2_show_hide %% 2)
    ) { return(NULL) }
    
    vals <- 
      extract(raster_comp2(), clicked_coords(), cells = T)
    
    new_names = c("cell", seq.int(length(names(vals)) - 1))
    
    vals <- 
      vals %>% 
      purrr::set_names(new_names) %>% 
      tidyr::pivot_longer(cols = -cell) %>% 
      rename(band = name, reflectance = value) %>% 
      mutate(
        band = as.numeric(band),
        wv = wavelengths[band],
        src = wv_src[band],
        reflectance = bnorm(reflectance, input$comp2_bn %% 2)
      )
    
    geom_line(data = vals, color = cols[4])
  })
  
  
  
  # 3 Reflectance ----
  comp3_geom_line <- reactive({
    req(is.numeric(input$map_click[["lng"]]))
    
    if (
      !length(raster_comp3()) || 
      (input$comp3_show_hide %% 2) 
    ) { return(NULL) }
    
    vals <- 
      extract(raster_comp3(), clicked_coords(), cells = T)
    
    new_names = c("cell", seq.int(length(names(vals)) - 1))
    
    vals <- 
      vals %>% 
      purrr::set_names(new_names) %>% 
      tidyr::pivot_longer(cols = -cell) %>% 
      rename(band = name, reflectance = value) %>% 
      mutate(
        band = as.numeric(band),
        wv = wavelengths[band],
        src = wv_src[band],
        reflectance = bnorm(reflectance, input$comp3_bn %% 2)
      )
    
    geom_line(data = vals, color = cols[5])
  })
  
  
  
  # Plot zoom ----
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
  
  
  
  # Plot ----
  output$plot <- renderPlot({
    req(path())
    req(is.numeric(input$map_click[["lng"]]))

  #  browser()
    
    focus_tag <- if (!is.null(plot_ranges$x)) { ", subset of values" }
    cell_id <- unique(reflectance_at_point()$cell)
    title <- paste0("Cell number: ", cell_id, focus_tag, collapse = "")
    
    ggplot(reflectance_at_point(), aes(wv, reflectance)) +
      comp0_geom_line() +
      comp1_geom_line() +
      comp2_geom_line() +
      comp3_geom_line() +
      geom_line(aes(group = src), color = cols[1]) +
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

    div("Click-and-drag to brush a region, double-click to set/reset selection.")
  })
  
  
  
  # Selection display ----
  output$selected_point <-  renderUI({
    
    req(is.numeric(input$map_click[["lng"]]))
    req(raster())
    
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
