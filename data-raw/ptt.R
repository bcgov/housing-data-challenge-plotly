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
devtools::use_data(ptt, overwrite = TRUE)

# why are there 2 records????
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

library(tidyr)
library(dplyr)
library(crosstalk)
library(plotly)


# total counts
d <- gather(ptt, variable, value, matches("^no_.*_tot$"))
d$variable <- sub("_tot$", "", sub("^no_", "", d$variable))

# correction for the multiple records problem
d <- d %>%
  group_by(Municipality, trans_period, variable) %>%
  summarise(value = sum(value, na.rm = TRUE))

sd <- SharedData$new(d, ~Municipality)

p <- ggplot(sd, aes(trans_period, value, color = Municipality)) +
  geom_line() +
  facet_wrap(~variable, scales = "free_y", ncol = 1)

ggplotly(p, height = 600, tooltip = c("x", "colour")) %>% 
  layout(dragmode = "zoom") %>%
  highlight("plotly_click", "plotly_doubleclick")



ptt %>% filter(Municipality == "Vancouver", trans_period == "2016-07-01") 


# summary statistics of foreign fair market value
d <- gather(ptt, variable, value, contains("foreign"))
d$variable <- sub("_tot$", "", sub("^no_", "", d$variable))

# correction for the multiple records problem
d <- d %>%
  group_by(Municipality, trans_period, variable) %>%
  summarise(value = if (grepl("^no", variable)) sum(value, na.rm = TRUE) else mean(value, na.rm = TRUE))

sd <- SharedData$new(d, ~Municipality)

p <- ggplot(sd, aes(trans_period, value, color = Municipality)) +
  geom_line() +
  facet_wrap(~variable, scales = "free_y", ncol = 1)

ggplotly(p, height = 600, tooltip = c("x", "colour")) %>% 
  layout(dragmode = "zoom") %>%
  highlight("plotly_click", "plotly_doubleclick")


# IDEA look at difference in foreign/non-foreign?
