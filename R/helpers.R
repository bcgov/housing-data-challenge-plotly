load_data <- function(name) {
  e <- environment()
  data(name, package = "bcviz", envir = e)
  d <- get(name, envir = e)
  if ("label" %in% names(d)) {
    d$label <- toupper(d$label)
  }
  d
}


include_css <- function(file) {
  path <- system.file("css", package = "bcviz")
  includeCSS(file.path(path, file))
}

# what levels are available for a given set of statistics?
# useful for determing what to show in the 'region level' dropdown
geoByTab <- function(stat = c("pop", "dwell", "ptt")) {
  stat <- match.arg(stat, stat)
  geos <- switch(
    stat,
    population = c("developments", "districts"),
    dwell = c("tracts"),
    ptt = c("developments", "districts", "municipals")
  )
  geos <- geos %||% as.character(geoAll())
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

dataByResolution <- function(type = c("developments", "districts", "municipals", "tracts")) {
  type <- match.arg(type, type)
  dat <- switch(
    type,
    developments = c("pop", "ptt"),
    districts = c("pop", "ptt"),
    municipals = c("ptt"),
    tracts = c("dwell")
  )
  dataAll()[dataAll() %in% dat]
}

dataAll <- function() {
  c(
    "Property Transfer Tax" = "ptt",
    "Population" = "pop",
    "Dwellings" = "dwell"
  )
}

defaultPttVars <- function() {
  c(
    "Total Market Transactions",
    "PTT Paid ($ sum)",
    "Foreign Involvement Transactions",
    "FMV sum of Foreign Involvement Transactions ($ sum)",
    "Additional Tax Paid ($ sum)"
  )
}

# set a sensible group name default....
shared_data <- function(d, var = ~label) {
  SharedData$new(d, var, "Selected region")
}

# captializes every word in a sting -- see ?toupper
simpleCap <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1,1)), substring(s, 2),
        sep="", collapse=" ")
}

"%||%" <- function(x, y) {
  if (!length(x)) y else x
}

new_id <- function() {
  basename(tempfile(""))
}