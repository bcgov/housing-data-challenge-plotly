# ------------------------------------------------------------------------
# Population/dwelling at the census tract level
# ------------------------------------------------------------------------

d <- readr::read_csv(
  "http://www12.statcan.gc.ca/census-recensement/2016/dp-pd/hlt-fst/pd-pl/Tables/CompFile.cfm?Lang=Eng&T=1601&OFT=FULLCSV"
)
dbc <- d[ d[[2]] %in% "British Columbia", ]

# sensible naming and downsizing
dwellTracts <- dbc %>%
  rename(
    label = `Geographic code`,
    pop16 = `Population, 2016`,
    pop11 = `Population, 2011`,
    dwell16 = `Total private dwellings, 2016`,
    #dwell11 = `Total private dwellings, 2011`,
    area = `Land area in square kilometres, 2016`
  ) %>%
  select(label, pop16, pop11, dwell16, area) %>%
  mutate(txt = paste(
    "Census tract:", label, "<br>", 
    "Population '16:", pop16, "<br>",
    "Population '11:", pop11, "<br>",
    "Dwellings '16:", dwell16, "<br>",
    "Area (square km):", area
  ))


devtools::use_data(dwellTracts, overwrite = TRUE)



# link 
library(crosstalk)
library(leaflet)
library(plotly)

pd <- SharedData$new(dwellTracts, ~label, "A")

p <- ggplot(pd, aes(x = pop16 / area, y = pop16 / dwell16)) + 
  geom_point(aes(text = txt), alpha = 0.2) + 
  labs(x = "People per square km", y = "People per dwelling")

gg <- ggplotly(p, tooltip = "text") %>% 
  layout(dragmode = "select") %>%
  highlight(persistent = T, dynamic = T)

md <- geoCensusTracts %>%
  dplyr::left_join(popCensusTracts) %>%
  sf::st_as_sf() %>%
  #sf::st_centroid() %>%
  SharedData$new(~label, "A")

map <- leaflet(md) %>%
  addTiles() %>%
  #addCircleMarkers()
  addPolygons(
    weight = 1, 
    color = "black",
    fillOpacity = 1,
    opacity = 1,
    popup = ~txt
  )

bscols(gg, map)


# ------------------------------------------------------------------------
# TODO: 
#  Do we even have these geographic codes? I think we may have download 
# Statistics Canada's geo
# ------------------------------------------------------------------------

d <- readr::read_csv(
  "http://www12.statcan.gc.ca/census-recensement/2016/dp-pd/hlt-fst/pd-pl/Tables/CompFile.cfm?Lang=Eng&T=701&OFT=FULLCSV"
)
# get rid of note at the bottom
d <- d[-seq.int(which(d[[1]] %in% "Note:"), nrow(d)), ]

dbc <- d[ d[[2]] %in% "British Columbia", ]


d <- readr::read_csv(
  "http://www12.statcan.gc.ca/census-recensement/2016/dp-pd/hlt-fst/pd-pl/Tables/CompFile.cfm?Lang=Eng&T=301&OFT=FULLCSV"
)

