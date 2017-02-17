#' Launch visualization
#' 
#' @param prompt whether or not to prompt the default browser to open the app. 
#' By default, a browser is prompted when R is being used interactively.
#' @export
#' @author Carson Sievert
#' @examples 
#' 
#' launch()

launch <- function(prompt = interactive()) {
  
  # geographic data
  data("geoDevelopments", package = "bcviz")
  data("geoDistricts", package = "bcviz")
  data("geoMunicipals", package = "bcviz")
  
  # population estimates
  data("popDevelopments", package = "bcviz")
  data("popDistricts", package = "bcviz")
  
  # property tax transfer
  data("ptt", package = "bcviz")
  
  bb <- st_bbox(geoDistricts)
  
  # user interface
  ui <- fluidPage(fluidRow(
    column(
      4, leafletOutput("map", height = 450),
      # TODO: should this be a conditional panel (based on the statistic)?
      selectInput(
        "regionType", "Choose a resolution:",
        c("Development Regions" = "developments", 
          "Regional Districts" = "districts")
      )
    ),
    column(
      8, plotlyOutput("pop", height = 650)
    )
  ))
  
  # server-side logic
  server <- function(input, output, session, ...) {
    
    regions <- reactiveValues(
      selected = NULL
    )
    
    output$map <- renderLeaflet({
      leaflet() %>%
        addTiles() %>%
        # TODO: why isn't fitBounds() consistent? Why does setView() lead to console errors?
        fitBounds(bb[["xmin"]], bb[["ymin"]], bb[["xmax"]], bb[["ymax"]])
    })
    
    # update reactive values upon clicking the map and modify opacity sensibly
    observeEvent(input$map_shape_click, {
      
      # TODO: clicking on an already clicked region should remove it...
      regions$selected <- c(regions$selected, input$map_shape_click)
      
      d <- switch(
        input$regionType,
        developments = geoDevelopments,
        districts = geoDistricts,
        municipals = geoMunicipals
      )
      
      d <- d[d$label %in% input$map_shape_click$id, ]
      
      
      leafletProxy("map", session) %>%
        removeShape(layerId = input$map_shape_click$id) %>%
        addPolygons(
          data = d,
          color = "black",
          fillOpacity = 1,
          weight = 1,
          highlightOptions = highlightOptions(fillOpacity = 0.2),
          label = ~label,
          layerId = ~label
        )
      
    })
    
    
    output$pop <- renderPlotly({
      
      d <- switch(
        input$regionType,
        developments = popDevelopments,
        districts = popDistricts
      )
      
      keyVar <- sub("s$", "", input$regionType)
      
      # always show overall BC population
      d <- d[d[[keyVar]] %in% c(regions$selected, "British Columbia"), ]
      
      p <- ggplot(d, aes(Age, Population, color = Gender)) +
        geom_line(aes(group = Year), alpha = 0.1) +
        geom_line(aes(frame = Year)) + 
        facet_wrap(as.formula(paste0("~", keyVar)), ncol = 1, scales = "free_y") + 
        theme_BCStats() + labs(y = NULL) +
        ggtitle("Population by age and gender from 1986 to 2016")
      
      ggplotly(p, dynamicTicks = TRUE, tooltip = "Gender") %>%
        hide_legend() %>%
        animation_opts(300)
    })
    
    # redraw polygons upon changing the region type
    observeEvent(input$regionType, {
      
      regions$selected <- NULL
      
      d <- switch(
        input$regionType,
        developments = geoDevelopments,
        districts = geoDistricts,
        municipals = geoMunicipals
      )
      
      leafletProxy("map", session) %>%
        clearShapes() %>%
        addPolygons(
          data = d,
          color = "black",
          weight = 1,
          highlightOptions = highlightOptions(fillOpacity = 1),
          label = ~label,
          layerId = ~label
        )
    })
    
  }
  
  shinyApp(
    ui, server, 
    options = list(launch.browser = prompt)
  )
}
