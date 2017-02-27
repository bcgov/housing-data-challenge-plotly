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
  
  # geographic data, ordered from largest to smallest 
  data("geoDevelopments", package = "bcviz")
  data("geoDistricts", package = "bcviz")
  data("geoMunicipals", package = "bcviz")
  data("geoCensusTracts", package = "bcviz")
  
  # population estimates
  data("popDevelopments", package = "bcviz")
  data("popDistricts", package = "bcviz")
  
  # property tax transfer
  data("ptt", package = "bcviz")
  
  bb <- st_bbox(geoDistricts)
  
  # user interface
  ui <- fluidPage(fluidRow(
    
    include_css("region.css"),
    column(
      4, leafletOutput("map", height = 450),
      uiOutput("regionTypes")
    ),
    column(
      8, tabsetPanel(
        id = "currentTab",
        tabPanel(
          "Population", plotlyOutput("pop", height = 650), value = "population"
        ),
        tabPanel(
          "Debug", verbatimTextOutput("debug"), value = "debug"
        )
      )
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
      
      validateRegionType(input$regionType)
      
      d <- switch(
        input$regionType,
        developments = geoDevelopments,
        districts = geoDistricts,
        municipals = geoMunicipals,
        tracts = geoCensusTracts
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
      
      validateRegionType(input$regionType)
      
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
      
      validateRegionType(input$regionType)
      
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
    
    output$regionTypes <- renderUI({
      selectInput(
        "regionType", "Choose a resolution:", geoByStat(input$currentTab)
      )
    })
    
    output$debug <- renderPrint({
      ls(input)
    })
    
  }
  
  shinyApp(
    ui, server, 
    options = list(launch.browser = prompt)
  )
}




validateRegionType <- function(type) {
  validate(
    need(type, "Loading..."),
    errorClass = "region"
  )
}

include_css <- function(file) {
  path <- system.file("css", package = "bcviz")
  includeCSS(file.path(path, file))
}
