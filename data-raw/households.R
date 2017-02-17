# Manually downloaded from
# http://www.bcstats.gov.bc.ca/StatisticsBySubject/Demography/Households.aspx
# Select "regional district" in dropdown, shift-click to select all regions/years

# read it in to clean it up a bit
houses <- readr::read_csv("data-raw/households-districts.csv")[, -1]
houseDistricts <- setNames(houses, c("district", "year", "houses"))

# TODO: are some of these locations outside BC? And some inside?
houseDistricts %>%
  mutate(isBC = district == "British Columbia") %>%
  group_by(isBC, year) %>%
  summarise(n = sum(houses)) %>%
  arrange(year)

devtools::use_data(houseDistricts, overwrite = TRUE)