# what levels are available for a given set of statistics?
# useful for determing what to show in the 'region level' dropdown
geoByStat <- function(stat = c("population", "housing")) {
  stat <- match.arg(stat, stat)
  geos <- switch(
    stat,
    population = c("developments", "districts"),
    housing = c("developments", "districts"),
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