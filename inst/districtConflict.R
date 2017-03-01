# ---------------------------------------------------------------------------
# NOTE: the problem described below can be resolved with this data...
# ---------------------------------------------------------------------------

concordance <- readr::read_csv(
  "https://raw.githubusercontent.com/bcgov/housing-data-visualization-project/master/data/geography-concordance.csv"
)

# really??
with(concordance, identical(SGC, SGC_5digit))
#> [1] TRUE


# ---------------------------------------------------------------------------
# The actual problem... a mismatch in distrcit region codes
# ---------------------------------------------------------------------------

library(curl)
library(sf)

tmp <- tempfile(fileext = ".zip")
curl_download(
  "http://www.bcstats.gov.bc.ca/Files/18885d4f-e4cf-443b-bb3b-d169651be62d/Boundaries-CensusDivisions2011.zip",
  tmp
)
unzip(tmp, exdir = dirname(tmp))
boundaries <- st_read(file.path(dirname(tmp), "CD_2011.shp"))

ptt <- readr::read_csv(
  "https://catalogue.data.gov.bc.ca/dataset/9c9b8d35-d59b-436a-a350-f581ea71a798/resource/bfd8129f-dfde-45f8-a0c1-ecc7222e0515/download/regional-district-weekly.csv" 
)

setdiff(
  toupper(boundaries$CDNAME), ptt$RegionalDistrict
)
#> [1] "POWELL RIVER"           "NORTHERN ROCKIES"       "PEACE RIVER"            "MOUNT WADDINGTON"      
#> [5] "SKEENA-QUEEN CHARLOTTE" "KITIMAT-STIKINE"        "COLUMBIA-SHUSWAP"       "CENTRAL COAST"         
#> [9] "STIKINE"                "GREATER VANCOUVER"     

setdiff(
  ptt$RegionalDistrict, toupper(boundaries$CDNAME)
)
#> [1] "METRO VANCOUVER"               "KITIMAT-STIKINE-NORTH COAST"   "NORTHERN ROCKIES-PEACE RIVER" 
#> [4] "COLUMBIA SHUSWAP"              "RURAL-UNKNOWN"                 "POWELL RIVER-MOUNT WADDINGTON"

