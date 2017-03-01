include_css <- function(file) {
  path <- system.file("css", package = "bcviz")
  includeCSS(file.path(path, file))
}

# what levels are available for a given set of statistics?
# useful for determing what to show in the 'region level' dropdown
geoByTab <- function(stat = c("population", "dwelling")) {
  stat <- match.arg(stat, stat)
  geos <- switch(
    stat,
    population = c("developments", "districts"),
    dwelling = c("tracts"),
    ptt = c("districts")
  )
  geoAll()[geoAll() %in% geos]
}

# query all available geographic levels
geoAll <- function() {
  # listed from least to most granular
  c(
    "Development Regions" = "developments", 
    "Regional Districts" = "districts",
    "Municipalities" = "municipals",
    "Census Tracts" = "tracts"
  )
}

shared_data <- function(d) {
  SharedData$new(d, ~label, "Selected region")
}