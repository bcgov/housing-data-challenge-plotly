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
  data("popCensusTracts", package = "bcviz")
  
  # property tax transfer
  data("ptt", package = "bcviz")
  
  # base theme for ggplot2 derived plots
  theme_set(theme_BCStats())
  
  # determine some global bounds
  bb <- st_bbox(geoDistricts)
  
  # let leaflet know about persistent selection
  options(persistent = TRUE)
  
  # user interface
  ui <- fluidPage(fluidRow(
    
    # NOTE TO SELF: can use shinythemes::themeSelector() to select a theme
    # shinythemes::shinytheme("sandstone"),
    
    # this css styles the 'error' message shown while output is generated...
    include_css("region.css"),
      
    column(
      4, leafletOutput("map", height = 450),
      # available region types depend on the current panel...
      uiOutput("regionTypes")
    ),
    
    # TODO: provide an icon in the title which links to help videos?
    column(
      8, navbarPage(title = "BC Housing Market", id = "currentTab",
      tabPanel(
        # IDEA: dynamically populate (i.e., uiOutput()) a list of selectInput()s for changing x/y/group/color defaults
        "Population", plotlyOutput("pop", height = 600), value = "population"
        #,icon = tags$icon("question-circle")
      ),
      tabPanel(
        # IDEA: dynamically populate (i.e., uiOutput()) a list of selectInput()s for changing x/y/group/color defaults
        "Dwelling", plotlyOutput("dwell", height = 600), value = "dwelling"
      )
    ))
    
  ))
  
  # server-side logic
  server <- function(input, output, session, ...) {
    
    rv <- reactiveValues(
      # the geography being shown (important for removing 'old' geography)
      geo = NULL,
      # the current type of geography (e.g., developments, municipalities, etc)
      regionType = NULL,
      # currently selected regions (currently only used for direct manip of map)
      regions = NULL
    )
    
    output$regionTypes <- renderUI({
      selectInput(
        "regionType", "Choose a resolution:", geoByTab(input$currentTab)
      )
    })
    
    getGeoData <- reactive({
      validateRegionType(input$regionType)
      d <- switch(
        input$regionType,
        developments = geoDevelopments,
        districts = geoDistricts,
        municipals = geoMunicipals,
        tracts = geoCensusTracts
      )
      shared_data(d)
    })
    
    output$map <- renderLeaflet({
      leaflet() %>%
        addTiles('http://stamen-tiles-{s}.a.ssl.fastly.net/toner-lite/{z}/{x}/{y}.png') %>%
        # TODO: Why does setView() lead to console errors?
        fitBounds(bb[["xmin"]], bb[["ymin"]], bb[["xmax"]], bb[["ymax"]]) %>%
        setMaxBounds(bb[["xmin"]], bb[["ymin"]], bb[["xmax"]], bb[["ymax"]])
    })
    
    # update reactive values upon clicking the map and modify opacity sensibly
    observeEvent(input$map_shape_click, {
      
      # don't do anything if this is the dwelling tab
      if (!identical(input$currentTab, "dwelling")) {
        # TODO: clicking on an already clicked region should remove it...
        rv$regions <- c(rv$regions, input$map_shape_click)
        
        d <- getGeoData()$origData()
        d <- d[d$label %in% input$map_shape_click$id, ]
        
        leafletProxy("map", session) %>%
          removeShape(layerId = input$map_shape_click$id) %>%
          addPolygons(
            data = shared_data(d),
            color = "black",
            fillOpacity = 1,
            weight = 1,
            highlightOptions = highlightOptions(fillOpacity = 0.2),
            label = ~label,
            layerId = ~label,
            group = "foo"
          )
      }
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
      d <- d[d[[keyVar]] %in% c(rv$regions, "British Columbia"), ]
      
      p <- ggplot(d, aes(Age, Population, color = Gender)) +
        geom_line(aes(group = Year), alpha = 0.1) +
        geom_line(aes(frame = Year)) + 
        facet_wrap(as.formula(paste0("~", keyVar)), ncol = 1, scales = "free_y") + 
        labs(y = NULL, title = "Population by age and gender from 1986 to 2016")
      
      ggplotly(p, dynamicTicks = TRUE, tooltip = "Gender") %>%
        hide_legend() %>%
        animation_opts(300)
    })
    
    output$dwell <- renderPlotly({
      
      pd <- shared_data(popCensusTracts)
      
      p <- ggplot(pd, aes(x = pop16 / area, y = pop16 / dwell16)) + 
        geom_point(aes(text = txt), alpha = 0.2) + 
        labs(x = "People per square km", y = "People per dwelling")
        
      ggplotly(p, tooltip = "text") %>% 
        layout(
          dragmode = "select",
          annotations = list(
            text = "Brush points to \n highlight tracts \n (double-click to reset)",
            x = 0.5, y = 0.5, xref = "paper", yref = "paper",
            ax = 100, ay = -100
          )
        ) %>%
        highlight(off = "plotly_deselect", dynamic = TRUE, persistent = TRUE)
    })
    
    # out with the old polygons, in with the new
    observeEvent(input$regionType, {
      
      # selected regions can't persist when changing resolution...
      # well, maybe when increasing the resolution?
      rv$regions <- NULL
    
      # dwelling vis has different opacity settings...
      isDwelling <- identical(input$currentTab, "dwelling")
      
      d <- getGeoData()
      bb <- st_bbox(d$origData())
      
      leafletProxy("map", session) %>%
        clearGroup("foo") %>%
        addPolygons(
          data = getGeoData(),
          color = "black",
          weight = 1,
          opacity = 1,
          fillOpacity = if (isDwelling) 0.5 else 0.2,
          highlightOptions = if (!isDwelling) highlightOptions(fillOpacity = 1),
          label = ~label,
          layerId = ~label,
          group = "foo"
        ) %>% fitBounds(bb[["xmin"]], bb[["ymin"]], bb[["xmax"]], bb[["ymax"]])
    })
    
  }
  
  shinyApp(
    ui, server, options = list(launch.browser = prompt)
  )
}


validateRegionType <- function(type) {
  validate(
    need(type, "Loading..."),
    errorClass = "region"
  )
}
