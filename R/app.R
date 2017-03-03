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
  geoDevelopments$label <- toupper(geoDevelopments$label)
  geoDistricts$label <- toupper(geoDistricts$label)
  geoMunicipals$label <- toupper(geoMunicipals$label)
  geoCensusTracts$label <- toupper(geoCensusTracts$label)
  
  # population estimates
  data("popDevelopments", package = "bcviz")
  data("popDistricts", package = "bcviz")
  popDevelopments$label <- toupper(popDevelopments$label)
  popDistricts$label <- toupper(popDistricts$label)
  
  # dwelling and population in 2011/2016
  data("dwellTracts", package = "bcviz")
  dwellTracts$label <- toupper(dwellTracts$label)
  
  # property tax transfer
  data("pttDevelopments", package = "bcviz")
  data("pttDistricts", package = "bcviz")
  data("pttMunicipals", package = "bcviz")
  pttDevelopments$label <- toupper(pttDevelopments$label)
  pttDistricts$label <- toupper(pttDistricts$label)
  pttMunicipals$label <- toupper(pttMunicipals$label)
  
  
  pttVars <- unique(
    c(pttMunicipals$variable, pttDistricts$variable, pttMunicipals$variable)
  )
  
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
      uiOutput("regionTypes"),
      conditionalPanel(
        "input.currentTab == 'ptt'",
        selectizeInput(
          "pttVars", 
          label = "Choose variables:", 
          choices = pttVars, 
          selected = defaultPttVars(), 
          multiple = TRUE,
          width = "100%",
          options = list(maxItems = 8)
        )
      ),
      conditionalPanel(
        "input.currentTab == 'create'",
        uiOutput("dataType"),
        fluidRow(
          actionButton("buttonCreate", "Create", icon = icon("line-chart")),
          downloadButton("downloadData", "Download", icon = icon("download"))
        )
      )
    ),
    
    # TODO: provide an icon in the title which links to help videos?
    column(
      8, navbarPage(title = "BC Housing Market", id = "currentTab", selected = "ptt",
      tabPanel(
        "Population", value = "pop", #icon = tags$icon("question-circle"),
        plotlyOutput("pop", height = 600) 
      ),
      tabPanel(
        "Dwelling", value = "dwell", 
        plotlyOutput("dwell", height = 500)
      ),
      tabPanel(
        title = "Property Transfer", value = "ptt",
        plotlyOutput("ptt", height = 600)
      ),
      tabPanel(
        title = "Create", value = "create",
        dataTableOutput("createDataTable")
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
      data = NULL
    )
    
    output$regionTypes <- renderUI({
      selectInput(
        "regionType", 
        label = "Choose a resolution:", 
        choices = geoByTab(input$currentTab),
        selected = switch(
          input$currentTab,
          population = "developments",
          dwelling = "tracts",
          ptt = "developments"
        )
      )
    })
    
    getGeoData <- reactive({
      validateInput(input$regionType)
      geoDat <- switch(
        input$regionType,
        developments = geoDevelopments,
        districts = geoDistricts,
        municipals = geoMunicipals,
        tracts = geoCensusTracts
      )
      visDat <- get(paste0(input$currentTab, simpleCap(input$regionType)))
      # keep only the polygons that reside in the data we're visualizing
      if (!is.null(visDat) && "label" %in% intersect(names(geoDat), names(visDat))) {
        geoDat <- semi_join(geoDat, visDat, by = "label")
      }
      # TODO: join with visDat to populate informative tooltips!
      shared_data(geoDat)
    })
    
    getPopData <- reactive({
      validateInput(input$regionType)
      d <- switch(
        input$regionType,
        developments = popDevelopments,
        districts = popDistricts
      )
      # always show overall BC population
      d[d$label %in% c(rv$regions, "British Columbia"), ]
    })
    
    getPttData <- reactive({
      validateInput(input$regionType)
      d <- switch(
        input$regionType,
        developments = pttDevelopments,
        districts = pttDistricts,
        municipals = pttMunicipals
      )
      validateInput(input$pttVars)
      # we can only realistically show a subset of the variables
      d2 <- d[d$variable %in% input$pttVars, ]
      # ensure the ordering of panels reflects the input ordering
      d2$variable <- factor(d2$variable, levels = input$pttVars)
      shared_data(d2)
    })
    
    output$map <- renderLeaflet({
      leaflet() %>%
        addTiles('http://stamen-tiles-{s}.a.ssl.fastly.net/toner-lite/{z}/{x}/{y}.png') %>%
        # TODO: Why does setView() lead to console errors?
        fitBounds(bb[["xmin"]], bb[["ymin"]], bb[["xmax"]], bb[["ymax"]])
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
      
      # not completely sure why this is necessary...
      validateInput(d$label)
      
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
      pd <- shared_data(dwellTracts)
      
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
      
      # not completely sure why this is necessary...
      #validateInput(d$variable)
      
      p <- ggplot(d, aes(trans_period, value, group = label)) +
        geom_line() +
        facet_wrap(~variable, scales = "free_y", ncol = 1) + 
        theme(legend.position = "none", axis.text.x = element_text(angle = 30)) + 
        labs(x = NULL, y = NULL)
      
      ggplotly(p, height = 600, tooltip = c("x", "group"), dynamicTicks = T) %>% 
        highlight("plotly_click", "plotly_doubleclick", dynamic = TRUE) %>%
        layout(
          dragmode = "zoom", 
          margin = list(l = 100, b = 100, t = 40),
          annotations = list(
            text = "Click to select a region (double-click to reset)",
            x = 0.15, y = 0.97, xref = "paper", yref = "paper",
            ax = -10, ay = -43
          )
        )
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
        ) %>% 
        fitBounds(bb[["xmin"]], bb[["ymin"]], bb[["xmax"]], bb[["ymax"]])
    })
    
    
    output$dataType <- renderUI({
      selectInput(
        "dataType", "Choose a dataset (depends on current resolution)", 
        choices = dataByResolution(input$regionType)
      )
    })
    
    # ---------------------------------------------------------------------
    # Create Tab
    # ---------------------------------------------------------------------
    
    getCreateData <- reactive({
      # TODO: how to provide wide forms of data?
      validateInput(input$dataType)
      d <- get(paste0(input$dataType, simpleCap(input$regionType)))
      # if this data has been "melted", spread it back
      if (all(c("value", "variable") %in% names(d))) {
        d <- tidyr::spread(d, variable, value)
      }
      d
    })
    
    # post to plotly's create page
    observeEvent(input$buttonCreate, {
      print(upload_grid(getCreateData(), filename = new_id()))
    })
    
    output$createDataTable <- renderDataTable({
      getCreateData()
    })
    
    output$downloadData <- downloadHandler(
      filename = function() { paste0(input$dataType, simpleCap(input$regionType), '.csv') },
      content = function(file) {
        write.csv(getCreateData(), file, row.names = FALSE)
      }
    )
    
  } # end of server() function
  
  shinyApp(
    ui, server, options = list(launch.browser = prompt)
  )
}


validateInput <- function(type) {
  validate(
    need(type, "Loading..."),
    errorClass = "region"
  )
}
