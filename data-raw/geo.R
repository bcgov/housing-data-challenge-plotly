# NOTE: this page has links to shape files for the different region types
# http://www.bcstats.gov.bc.ca/statisticsbysubject/geography/TranslationsDataSets.aspx

library(sf)
library(curl)

# ---------------------------------------------------------------------------
# obtain/simplify shape files for the 8 Development regions
# ---------------------------------------------------------------------------

district <- "http://www.bcstats.gov.bc.ca/Files/8057f3fc-ea90-4764-8684-db3031c0bd38/Boundaries-DevelopmentRegions.zip"
target <- file.path("data-raw",  basename(district))
if (!file.exists(target)) {
  curl_download(district, target)
}
unzip(target, exdir = "data-raw/tmp")
d <- st_read("data-raw/tmp/DR.shp")
d2 <- st_simplify(d, dTolerance = 350)
geoDevelopments <- mutate(st_transform(d2, 4326), label = DR_NAME)
devtools::use_data(geoDevelopments, overwrite = TRUE)

# ---------------------------------------------------------------------------
# obtain/simplify shape files for the 28 regional districts
# ---------------------------------------------------------------------------

district <- "http://www.bcstats.gov.bc.ca/Files/18885d4f-e4cf-443b-bb3b-d169651be62d/Boundaries-CensusDivisions2011.zip"
target <- file.path("data-raw",  basename(district))
if (!file.exists(target)) {
  curl_download(district, target)
}
unzip(target, exdir = "data-raw/tmp")
# this shape file is super high resolution, simplify it!
d <- st_read("data-raw/tmp/CD_2011.shp")
# str(d$geometry[[2]])
#> List of 1
#> $ :List of 1
#> ..$ : num [1:4627, 1:2] 1392988 1393966 1394930 1395134 1395243 ...
#> - attr(*, "class")= chr [1:3] "XY" "MULTIPOLYGON" "sfg"
d2 <- st_simplify(d, dTolerance = 4000)
# str(d2$geometry[[2]])
#> List of 1
#> $ : num [1:73, 1:2] 1392988 1413363 1429810 1436930 1468324 ...
#> - attr(*, "class")= chr [1:3] "XY" "POLYGON" "sfg"

# Use 'label' as a "primary key" for display-info/linking
# also, leaflet currently requires this transform?
# https://github.com/rstudio/leaflet/blob/24a7dfa68528e32265aa325610eca7f8fa7a8050/R/normalize-sf.R#L86-L91
geoDistricts <- mutate(st_transform(d2, 4326), label = CDNAME)
devtools::use_data(geoDistricts, overwrite = TRUE)

# ---------------------------------------------------------------------------
# obtain/simplify shape files for municipals
# ---------------------------------------------------------------------------

municipal <- "http://www.bcstats.gov.bc.ca/Files/28417d81-0bf6-43d8-b182-7257ed72768f/Boundaries-CensusSubdivisions2011.zip"
target <- file.path("data-raw",  basename(municipal))
if (!file.exists(target)) {
  curl_download(district, target)
}
unzip(target, exdir = "data-raw/tmp")

# this shape file is super high resolution, simplify it!
d <- st_read("data-raw/tmp/CSD_2011.shp")
d2 <- st_simplify(d, dTolerance = 35)
geoMunicipals <- mutate(st_transform(d2, 4326), label = CSDNAME)
devtools::use_data(geoMunicipals, overwrite = TRUE)


# clean-up uncompressed files
unlink("data-raw/tmp", recursive = TRUE)
