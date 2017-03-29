# upload grids to plotly so we can link to them...
# devtools::install_github("ropensci/plotly@feature/APIv2")

upload <- function(nm) {
  # if this data has been "melted", spread it back
  data(list = nm, package = "bcviz")
  d <- get(nm)
  if (all(c("value", "variable") %in% names(d))) {
    d <- tidyr::spread(d, variable, value)
  }
  plotly::upload_grid(d, filename = plotly:::new_id())
}

dats <- c(
  "pttDevelopments",
  "pttDistricts",
  "pttMunicipals",
  "popDevelopments",
  "popDistricts",
  "dwellTracts"
)

gridLinks <- character()
for (i in dats) {
  gridLinks[[i]] <- upload(i)$file$fid
}
devtools::use_data(gridLinks, overwrite = TRUE)
