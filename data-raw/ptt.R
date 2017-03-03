# Apparently we might get better/more granular data...

library(rvest)
library(curl)

# NOTE: to update, go to https://catalogue.data.gov.bc.ca/dataset/property-transfer-tax-data
# click permalink, replace this link below with that link
src <- html("https://catalogue.data.gov.bc.ca/dataset/9c9b8d35-d59b-436a-a350-f581ea71a798")
# grab links to csvs
sources <- src %>%
  html_nodes(".resource-url-analytics") %>%
  html_attrs() %>%
  sapply("[[", "href") %>%
  grep("\\.csv$", ., value = TRUE)

targets <- file.path("data-raw", basename(sources))

for (i in seq_along(targets)) {
  target <- targets[[i]]
  if (!file.exists(target)) curl_download(sources[[i]], target)
}

# data at the municipal level only includes 3/8 development regions
ptt <- readr::read_csv("data-raw/municipal-monthly.csv")
# reshape/format after grabbing the ptt data dictionary
#devtools::use_data(ptt, overwrite = TRUE)

# ---------------------------------------------------------------------------
# Ummmm, why are there 2 records????
library(dplyr)
ptt %>% filter(Municipality == "Vancouver", trans_period == "2016-06-01")

#> # A tibble: 2 × 31
#> trans_period  DevelopmentRegion RegionalDistrict Municipality no_mkt_trans no_resid_trans no_resid_acreage_trans
#> <date>              <chr>            <chr>        <chr>        <int>          <int>                  <int>
#> 1   2016-06-01 Mainland/Southwest  METRO VANCOUVER    Vancouver         2190           2119                     NA
#> 2   2016-06-01 Mainland/Southwest  METRO VANCOUVER    Vancouver         1516           1477                     NA
#> # ... with 24 more variables: resid_comm_count <int>, no_resid_farm <int>, no_resid_fam <int>, no_res_1fam <int>,
#> #   no_resid_strata <int>, no_resid_non_strata <int>, no_resid_other <int>, no_comm_tot <int>, no_comm_comm <int>,
#> #   no_comm_strata_nores <int>, no_comm_other <int>, no_recr_tot <int>, no_farm_tot <int>, no_unkn_tot <int>,
#> #   sum_FMV <dbl>, mn_FMV <dbl>, md_FMV <dbl>, sum_PPT_paid <dbl>, md_PPT <dbl>, no_foreign <int>,
#> #   sum_FMV_foreign <int>, mn_FMV_foreign <dbl>, md_FMV_foreign <int>, add_tax_paid <dbl>

# ---------------------------------------------------------------------------


# Use the data dictonary to provide better variable names & more contextual info
# NOTE: I couldn't get ropensci/tabulizer installed on MAC OS 
# (since it requires a specific version of Java :(), but this
# runs on my Linux box.
library(tabulizer)
dic <- "https://catalogue.data.gov.bc.ca/dataset/9c9b8d35-d59b-436a-a350-f581ea71a798/resource/741c62e3-acf3-4a37-8647-083654448351/download/data-dictionary.pdf"
#locate_areas(dic, widget = "native")
tabs <- extract_tables(dic)

#> List of 4
#> $ : chr [1:12, 1:6] "Terms - Excel File" "" "DEVLOPMENT REGION" "REGIONAL DISTRICTS" ...
#> $ : chr [1:15, 1:3] "Terms - Excel File" "RESIDENTIAL - MULTI-\rFAMILY (count)" "RESIDENTIAL - SINGLE \rFAMILY RESIDENTIAL (count)" "RESIDENTIAL - STRATA \rRESIDENTIAL (count)" ...
#> $ : chr [1:11, 1:3] "Terms - Excel File" "FMV Median ($ median)" "PTT Paid ($ sum)" "PTT Paid Median ($ median)" ...
#> $ : chr [1:6, 1:3] "Terms - Excel File" "mean)" "Under $1 million (count, \rforeign involvement \rtransactions)" "$1 million - $3 million \r(count, foreign involvement \rtransactions)" ...

tabs[[1]] <- tabs[[1]][, 1:3]
tabs <- lapply(tabs, function(x) tibble::as_tibble(x[-1, ]))
pttDic <- setNames(
  dplyr::bind_rows(tabs), 
  c("description", "varname", "definition")
)
# remove "return" characters from each column
pttDic[] <- lapply(pttDic, function(x) gsub("\\\r", "", x))
# yes, we know they're counts
pttDic$description <- sub("(count)", "", pttDic$description, fixed = TRUE)
# trim leading/trailing white space
pttDic$description <- gsub("^\\s+|\\s+$", "", pttDic$description)
pttDic <- subset(pttDic, description != "")
devtools::use_data(pttDic, overwrite = TRUE)


library(tidyr)
library(dplyr)

pttMelt <- gather(
  ptt, variable, value, -trans_period, 
  -DevelopmentRegion, -RegionalDistrict, -Municipality
)

# use descriptions for variable names
recode_variable <- function(d) {
  if (!exists("pttDic")) data("pttDic")
  varMap <- setNames(pttDic$varname, pttDic$description)
  for (i in seq_along(varMap)) {
    idx <- d$variable %in% varMap[[i]]
    d$variable[idx] <- names(varMap)[[i]]
  }
  d
}

# NOTE: 'corrects' the multiple records problem
pttMunicipals <- pttMelt %>%
  group_by(Municipality, trans_period, variable) %>%
  summarise(value = sum(value, na.rm = TRUE)) %>%
  rename(label = Municipality) %>%
  recode_variable()

pttMunicipals <- recode_variable(pttMunicipals)
# TODO: fix concordance!
concordance <- readr::read_csv(
  "https://raw.githubusercontent.com/bcgov/housing-data-visualization-project/master/data/geography-concordance.csv"
)
devtools::use_data(pttMunicipals, overwrite = TRUE)


# reshape and correct for the multiple records problem
pttDevelopments <- pttMelt %>%
  group_by(DevelopmentRegion, trans_period, variable) %>%
  summarise(value = sum(value, na.rm = TRUE)) %>%
  rename(label = DevelopmentRegion) %>%
  recode_variable()

devtools::use_data(pttDevelopments, overwrite = TRUE)

pttDistricts <- pttMelt %>%
  group_by(RegionalDistrict, trans_period, variable) %>%
  summarise(value = sum(value, na.rm = TRUE)) %>%
  rename(label = RegionalDistrict) %>%
  recode_variable()

devtools::use_data(pttDistricts, overwrite = TRUE)




library(crosstalk)
library(plotly)

# IDEA: let shiny app users pick 1-5 variables of interest
# and link time series to the map as well
pttMunicipals <- pttMunicipals[pttMunicipals$variable %in% names(varMap)[6:10], ]

sd <- SharedData$new(pttMunicipals, ~Municipality)

p <- ggplot(sd, aes(trans_period, value, group = Municipality)) +
  geom_line() +
  facet_wrap(~variable, scales = "free_y", ncol = 1) + 
  theme(legend.position = "none") + labs(x = NULL, y = NULL)

ggplotly(p, height = 600, tooltip = c("x", "group"), dynamicTicks = T) %>% 
  highlight("plotly_click", "plotly_doubleclick") %>%
  layout(
    dragmode = "zoom", 
    margin = list(l = 50),
    annotations = list(
      text = "Click to select a region\n(double-click to reset)",
      x = 0.6, y = 0.9, xref = "paper", yref = "paper",
      ax = 150, ay = -50
    )
  )
  
