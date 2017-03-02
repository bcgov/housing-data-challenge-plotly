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
  # TODO: data is available for other region types
  data("pttMunicipals", package = "bcviz")
  pttVars <- unique(pttMunicipals$variable)
  
  # base theme for ggplot2 derived plots
  theme_set(theme_BCStats())
  
  # determine some global bounds
  bb <- st_bbox(geoDistricts)
  
  # let leaflet know about persistent selection
  options(persistent = TRUE)
  
  # user interface
  ui <- fluidPage(fluidRow(
    
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
        "Population", value = "population", #icon = tags$icon("question-circle"),
        plotlyOutput("pop", height = 600) 
      ),
      tabPanel(
        "Dwelling", value = "dwelling", 
        plotlyOutput("dwell", height = 600)
      ),
      tabPanel(
        title = "Property Transfer", value = "ptt",
        selectizeInput(
          "pttVars", 
          label = "Choose variables:", 
          choices = pttVars, 
          selected = pttVars[6:9], 
          multiple = TRUE,
          width = "100%",
          options = list(maxItems = 5)
        ),
        plotlyOutput("ptt", height = 600)
      )
    ))
    
  ))
  
  # server-side logic
  server <- function(input, output, session, ...) {
    
    rv <- reactiveValues(
      # currently selected regions (currently only used for direct manip of map)
      regions = NULL,
      # the (non-geo) data currently being viewed --
      # important to know since we don't want to draw regions that we don't have...
      # (pttMunicipals is a good example why)
      data = NULL,
      regionType = NULL
    )
    
    # rv$regionType depends on both input$regionType _and_ input$currentTab
    observeEvent(input$regionType, {
      rv$regionType <- input$regionType
    })
    
    observeEvent(input$currentTab, {
      type <- switch(
        input$currentTab,
        population = "developments",
        dwelling = "tracts",
        ptt = "municipals"
      )
      rv$regionType <- type %||% input$regionType
    })
    
    output$regionTypes <- renderUI({
      selectInput(
        "regionType", "Choose a resolution:", geoByTab(input$currentTab)
      )
    })
    
    getGeoData <- reactive({
      validateRegionType(rv$regionType)
      d <- switch(
        rv$regionType,
        developments = geoDevelopments,
        districts = geoDistricts,
        municipals = geoMunicipals,
        tracts = geoCensusTracts
      )
      # keep only the polygons that reside in the data we're visualizing
      if (!is.null(rv$data) && "label" %in% intersect(names(d), names(rv$data))) {
        d <- semi_join(d, rv$data, by = "label")
      }
      shared_data(d)
    })
    
    getPopData <- reactive({
      validateRegionType(rv$regionType)
      d <- switch(
        rv$regionType,
        developments = popDevelopments,
        districts = popDistricts
      )
      # notify world "this is the data" before subsetting
      rv$data <- d
      # always show overall BC population
      d[d$label %in% c(rv$regions, "British Columbia"), ]
    })
    
    getPttData <- reactive({
      validateRegionType(rv$regionType)
      # TODO: more region types!
      d <- switch(
        rv$regionType,
        municipals = pttMunicipals
      )
      # we can only realistically show a subset of the variables
      d2 <- d[d$variable %in% input$pttVars, ]
      rv$data <- d2
      shared_data(d2)
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
        
        # subset down to the clicked region
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
      
      d <- getPopData()
      
      p <- ggplot(d, aes(Age, Population, color = Gender)) +
        geom_line(aes(group = Year), alpha = 0.1) +
        geom_line(aes(frame = Year)) + 
        facet_wrap(~label, ncol = 1, scales = "free_y") + 
        labs(y = NULL, title = "Population by age and gender from 1986 to 2016")
      
      ggplotly(p, dynamicTicks = TRUE, tooltip = "Gender") %>%
        hide_legend() %>%
        animation_opts(300)
    })
    
    output$dwell <- renderPlotly({
      
      # TODO: more region types?
      rv$data <- popCensusTracts
      pd <- shared_data(popCensusTracts)
      
      p <- ggplot(pd, aes(x = pop16 / area, y = pop16 / dwell16)) + 
        geom_point(aes(text = txt), alpha = 0.2) + 
        labs(x = "People per square km", y = "People per dwelling")
        
      # TODO: color by difference in population (11 to 16)!?!
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
    
    output$ptt <- renderPlotly({
      
      d <- getPttData()
      
      p <- ggplot(d, aes(trans_period, value, group = label)) +
        geom_line() +
        facet_wrap(~variable, scales = "free_y", ncol = 1) + 
        theme(legend.position = "none") + 
        labs(x = NULL, y = NULL)
      
      ggplotly(p, height = 600, tooltip = c("x", "group"), dynamicTicks = T) %>% 
        highlight("plotly_click", "plotly_doubleclick", dynamic = TRUE) %>%
        layout(
          dragmode = "zoom", 
          margin = list(l = 100, b = 50),
          annotations = list(
            text = "Click to select a region\n(double-click to reset)",
            x = 0.6, y = 0.9, xref = "paper", yref = "paper",
            ax = 150, ay = -50
          )
        )
    })
    
    # out with the old polygons, in with the new
    observeEvent(rv$regionType, {
      
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
