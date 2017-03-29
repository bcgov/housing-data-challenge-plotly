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
  
  # dwelling and population in 2011/2016
  data("dwellTracts", package = "bcviz")
  
  # property tax transfer
  data("pttDevelopments", package = "bcviz")
  data("pttDistricts", package = "bcviz")
  data("pttMunicipals", package = "bcviz")
  
  # links to plotly grids (for creation)
  data("gridLinks", package = "bcviz")
  
  # add hovertext info for census tracts
  geoCensusTracts <- dplyr::left_join(
    geoCensusTracts, dwellTracts[c("label", "txt")], by = "label"
  )
  
  # collect all unique ptt vars and provide a sensible ordering
  pttVars <- unique(
    c(pttMunicipals$variable, pttDistricts$variable, pttMunicipals$variable)
  )
  
  # base theme for ggplot2 derived plots
  theme_set(theme_BCStats())
  
  # determine some global bounds
  bb <- st_bbox(geoDistricts)
  
  # inform leaflet about persistent selection
  options(persistent = TRUE)
  
  # widget to change the height of the plot
  heightInput <- conditionalPanel(
    "input.currentTab != 'create'",
    sliderInput(
      "height", "Height of plot", 
      value = 625, min = 100, max = 3000, step = 25
    )
  )
  
  # user interface
  ui <- fluidPage(fluidRow(
    
    # this css styles the 'error' message shown while output is generated...
    include_css("region.css"),
      
    column(
      4, div(
        style = "color:#777;position:relative;top:70%;transform:translateY(-50%);", 
        h2("BC Housing Market Explorer")
      ),
      leafletOutput("map", height = 450),
      fluidRow(
        column(
          # Assuming property transfer is the default tab
          6, selectInput(
            "regionType", "Choose a resolution", 
            choices = geoByTab("ptt"), selected = "developments"
          )
        ),  
        column(6, heightInput)
      ),
      conditionalPanel(
        "input.currentTab == 'ptt'",
        radioButtons(
          "pttVisType", "Pre-choosen groups:", 
          choices = c(
            "Foreign Involvement" = "foreign",
            "Overall Transactions" = "overall",
            "Commercial Transactions" = "commercial",
            "Residential Transactions" = "residential"
          ),
          selected = "foreign",
          inline = TRUE
        ),
        selectizeInput(
          "pttVars", 
          label = "Add/remove variables:", 
          choices = c(defaultPttVars("foreign"), setdiff(pttVars, defaultPttVars("foreign"))), 
          selected = defaultPttVars("foreign"), 
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
      8, navbarPage(
        id = "currentTab", selected = "ptt", 
        windowTitle = "BC Housing Market Explorer",
        title = a(
          target = "_blank", 
          href = "https://vimeo.com/207379729", 
          icon("question-circle")
        ),
        tabPanel(
          "Population", value = "population", plotlyOutput("pop", height = 600) 
        ),
        tabPanel(
          "Dwelling", value = "dwelling", plotlyOutput("dwell", height = 500)
        ),
        tabPanel(
          title = "Property Transfer", value = "ptt", plotlyOutput("ptt", height = 600)
        ),
        tabPanel(
          title = "Create", value = "create", dataTableOutput("createDataTable")
        )
      ))
    
  ))
  
  # server-side logic
  server <- function(input, output, session, ...) {
    
    rv <- reactiveValues(
      # currently selected regions (currently only used for direct manip of map)
      regions = NULL
    )
    
    getGeoData <- reactive({
      validateInput(input$regionType)
      geoDat <- switch(
        input$regionType,
        developments = geoDevelopments,
        districts = geoDistricts,
        municipals = geoMunicipals,
        tracts = geoCensusTracts
      )
      if (is.null(geoDat)) return(NULL)
      validateInput(input$currentTab)
      # ugh, these two developments are merged in the ptt data...
      if (identical(input$currentTab, "ptt")) {
        geoDat <- geoDat[!geoDat$label %in% c("NECHAKO", "NORTH COAST"), ]
      } else {
        geoDat <- geoDat[!geoDat$label %in% c("NECHAKO & NORTH COAST"), ]
      }
      visDat <- tryCatch(
        get(paste0(input$currentTab, simpleCap(input$regionType))),
        error = function(e) data.frame()
      )
      # keep only the polygons that reside in the data we're visualizing
      if (!is.null(visDat) && "label" %in% intersect(names(geoDat), names(visDat))) {
        geoDat <- semi_join(geoDat, visDat, by = "label")
      }
      # fallback on "standard" key if no informative tooltip is available
      # NOTE: this appears to only work for list-columns?
      geoDat[["txt"]] <- lapply(geoDat[["txt"]] %||% geoDat[["label"]], HTML)
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
      if (is.null(d)) return(NULL)
      # always show overall BC population
      d[d$label %in% c(rv$regions, "BRITISH COLUMBIA"), ]
    })
    
    getDwellData <- reactive({
      # TODO: more region types?
      shared_data(dwellTracts)
    })
    
    getPttData <- reactive({
      validateInput(input$regionType)
      d <- switch(
        input$regionType,
        developments = pttDevelopments,
        districts = pttDistricts,
        municipals = pttMunicipals
      )
      # it could be that we don't have a valid region type yet...
      if (is.null(d)) return(NULL)
      validateInput(input$pttVars)
      # we can only realistically show a subset of the variables
      d2 <- d[d$variable %in% input$pttVars, ]
      # ensure the ordering of panels reflects the input ordering
      d2$variable <- factor(d2$variable, levels = input$pttVars)
      shared_data(d2)
    })
    
    # change region type options when switching tabs
    observeEvent(input$currentTab, {
      
      updateSelectInput(
        session = session, inputId = "regionType",
        choices = geoByTab(input$currentTab),
        selected = switch(
          input$currentTab,
          population = "developments",
          dwelling = "tracts",
          ptt = "developments"
        )
      )
      
    })
    
    # modify the items defining the ptt variables to visualize 
    observeEvent(input$pttVisType, {
      
      redrawRegions()
      
      vars <- defaultPttVars(input$pttVisType)
      updateSelectizeInput(
        session = session, inputId = "pttVars",
        # ensure these variables are listed first, in a sensible order
        choices = c(vars, setdiff(pttVars, vars)),
        selected = vars
      )
      
    })
    
    
    output$map <- renderLeaflet({
      leaflet() %>%
        addTiles(
          'http://stamen-tiles-{s}.a.ssl.fastly.net/toner-lite/{z}/{x}/{y}.png'
        ) %>%
        # TODO: Why does setView() lead to console errors?
        fitBounds(bb[["xmin"]], bb[["ymin"]], bb[["xmax"]], bb[["ymax"]])
    })
    
    # update reactive values upon clicking the map and modify opacity sensibly
    observeEvent(input$map_shape_click, {
      
      # "clickable regions" only on the population page
      if (identical(input$currentTab, "population")) {
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
      
      input$currentTab
      
      # get and validate that data is ready
      d <- getPopData()
      validateInput(d)
      
      p <- ggplot(d, aes(Age, Population, color = Gender)) +
        geom_line(aes(group = Year), alpha = 0.1) +
        geom_line(aes(frame = Year)) + 
        facet_wrap(~label, ncol = 1, scales = "free_y") + 
        labs(y = NULL, title = "Population by age and gender from 1986 to 2016")
      
      ggplotly(p, height = input$height, dynamicTicks = TRUE, tooltip = c("Gender", "Year")) %>%
        hide_legend() %>%
        animation_opts(300)
    })
    
    output$dwell <- renderPlotly({
      
      # clear any previous "crosstalk selections"
      input$currentTab
      
      # get and validate that data is ready
      d <- getDwellData()
      validateInput(d)
      
      p1 <- ggplot(d, aes(x = pop16 / area, y = pop16 / dwell16)) + 
        geom_point(aes(text = txt), alpha = 0.2) + 
        labs(x = "People per square km", y = "People per dwelling")
      
      gg1 <- ggplotly(p1, height = input$height, tooltip = "text")
      
      p2 <- ggplot(d) +
        geom_segment(
          alpha = 0.2,
          aes(x = "2011", xend = "2016", y = pop11, yend = pop16, group = label),
        ) +
        geom_point(aes(x = "2011", y = pop11, text = txt), size = .5, alpha = 0.2) +
        geom_point(aes(x = "2016", y = pop16, text = txt), size = 1, alpha = 0.2) +
        labs(x = NULL, y = "Population")
      
      gg2 <- ggplotly(p2, tooltip = "text")
        
      subplot(
        gg1, gg2, titleX = TRUE, titleY = TRUE, nrows = 2, margin = c(0, 0, .05, .05)
      ) %>% 
        layout(
          dragmode = "select",
          annotations = list(
            text = "Brush points to \n highlight tracts \n (double-click to reset)",
            x = 0.5, y = 0.75, xref = "paper", yref = "paper",
            ax = 100, ay = - (1/11 * input$height)
          )
        ) %>%
        highlight(off = "plotly_deselect", dynamic = TRUE, persistent = TRUE)
    })
    
    output$ptt <- renderPlotly({
      
      # clear any previous "crosstalk selections"
      input$currentTab
      
      # get and validate that data is ready
      d <- getPttData()
      validateInput(d)
      
      p <- ggplot(d, aes(trans_period, value, group = label, text = txt)) +
        geom_line() +
        facet_wrap(~variable, scales = "free_y", ncol = 1) + 
        theme(legend.position = "none", axis.text.x = element_text(angle = 15)) + 
        labs(x = NULL, y = NULL)
      
      ggplotly(p, height = input$height, tooltip = "text", dynamicTicks = TRUE) %>% 
        highlight("plotly_click", "plotly_doubleclick", dynamic = TRUE) %>%
        layout(
          dragmode = "zoom", 
          margin = list(l = 100, b = 100, t = 45),
          annotations = list(
            text = "Click to select a region (double-click to reset)",
            x = 0.15, y = 0.97, xref = "paper", yref = "paper",
            ax = 20, ay = - (1/13 * input$height)
          )
        )
    })
    
    observeEvent(input$regionType, {
      redrawRegions()
    })
    
    observeEvent(input$currentTab, {
      redrawRegions()
      
      # draw visual clue letting user know they may click regions 
      if (identical(input$currentTab, "population")) {
        leafletProxy("map", session) %>%
          addLabelOnlyMarkers(
            -120, 56.89769, group = "label",
            label = HTML("Click on a region <br /> to select it"),
            labelOptions = labelOptions(noHide = TRUE, textsize = '15px')
          )
      } else {
        leafletProxy("map", session) %>%
          clearGroup("label")
      }

    })
    
    # out with the old polygons, in with the new
    redrawRegions <- function() {
      # selected regions can't persist when changing resolution...
      # well, maybe when increasing the resolution?
      rv$regions <- NULL
      
      # dwelling vis has different opacity settings...
      isDwelling <- identical(input$currentTab, "dwelling")
      
      SD <- getGeoData()
      d <- SD$origData()
      bb <- st_bbox(st_as_sf(d))
      
      leafletProxy("map", session) %>%
        clearGroup("foo") %>%
        addPolygons(
          data = SD,
          color = "black",
          weight = 1,
          opacity = 1,
          fillOpacity = if (isDwelling) 0.5 else 0.2,
          highlightOptions = if (!isDwelling) highlightOptions(fillOpacity = 1),
          label = ~txt,
          layerId = ~label,
          group = "foo"
        ) %>% 
        fitBounds(bb[["xmin"]], bb[["ymin"]], bb[["xmax"]], bb[["ymax"]])
    }
    
    # TODO: use updateSelectInput()?
    output$dataType <- renderUI({
      validateInput(input$regionType)
      selectInput(
        "dataType", "Choose a dataset (depends on current resolution)", 
        choices = dataByResolution(input$regionType)
      )
    })
    
    # ---------------------------------------------------------------------
    # Create Tab
    # ---------------------------------------------------------------------
    
    getCreateData <- reactive({
      validateInput(input$dataType)
      d <- tryCatch(
        get(paste0(input$dataType, simpleCap(input$regionType))),
        error = function(e) data.frame()
      )
      # if this data has been "melted", spread it back
      if (all(c("value", "variable") %in% names(d))) {
        d <- tidyr::spread(d, variable, value)
      }
      d
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
    
    # post to plotly's create page
    observeEvent(input$buttonCreate, {
      datName <- paste0(input$dataType, simpleCap(input$regionType))
      browseURL(paste0("https://plot.ly/create/?fid=", gridLinks[[datName]]))
    })
    
    
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
