# unfortunately, I think in order to get this data by gender/age/district
# we have to manually download from http://www.bcstats.gov.bc.ca/StatisticsBySubject/Demography/PopulationEstimates.aspx

# TODO: can we get the same data at the municipal level?

library(readr)
library(tidyr)
library(dplyr)
library(broom)

d <- read_csv("data-raw/population-districts.csv")[, -1]

pd <- d %>%
  rename(district = `Regional District`) %>%
  select(-Total) %>%
  gather(Age, Population, -district, -Year, -Gender)

# recode the age groups using their midpoint
grps <- unique(pd$Age)
bins <- strsplit(grps, "-")
bins[[1]] <- c(0, 1)
bins[[length(bins)]] <- c(90, 100)
bins <- setNames(
  sapply(bins, function(x) mean(as.numeric(x))), grps
)

for (i in seq_along(bins)) {
  pd$Age[pd$Age == names(bins)[[i]]] <- bins[[i]]
}
pd$Age <- as.numeric(pd$Age)


# now, use these midpoints as input into a smoothed version of these predictions
AgeDF <- data.frame(Age = seq(0.5, 100, length.out = 30))

popDistricts <- pd %>%
  group_by(district, Gender, Year) %>% 
  do(yhat = augment(
    loess(Population ~ Age, data = ., span = 0.3), newdata = AgeDF
  )[c("Age", ".fitted")]) %>%
  unnest() %>%
  rename(Population = .fitted) %>%
  mutate(label = toupper(district))

devtools::use_data(popDistricts, overwrite = TRUE)

# visualize evolution for entire BC area
#bc <- filter(popDistricts, district %in% c("British Columbia", "Peace River"))
#
#p <- ggplot(bc, aes(Age, Population, color = Gender)) +
#  geom_line(aes(group = Year), alpha = 0.1) +
#  geom_line(aes(frame = Year)) + 
#  facet_wrap(~district, ncol = 1, scales = "free_y")
#ggplotly(p) %>%
#  animation_opts(100)


d <- read_csv("data-raw/population-developments.csv")[, -1]

pd <- d %>%
  rename(development = `Development Region`) %>%
  select(-Total) %>%
  gather(Age, Population, -development, -Year, -Gender)

# recode the age groups using their midpoint
grps <- unique(pd$Age)
bins <- strsplit(grps, "-")
bins[[1]] <- c(0, 1)
bins[[length(bins)]] <- c(90, 100)
bins <- setNames(
  sapply(bins, function(x) mean(as.numeric(x))), grps
)

for (i in seq_along(bins)) {
  pd$Age[pd$Age == names(bins)[[i]]] <- bins[[i]]
}
pd$Age <- as.numeric(pd$Age)


# now, use these midpoints as input into a smoothed version of these predictions
AgeDF <- data.frame(Age = seq(0.5, 100, length.out = 30))

popDevelopments <- pd %>%
  group_by(development, Gender, Year) %>% 
  do(yhat = augment(
    loess(Population ~ Age, data = ., span = 0.3), newdata = AgeDF
  )[c("Age", ".fitted")]) %>%
  unnest() %>%
  rename(Population = .fitted) %>%
  mutate(label = toupper(development))


devtools::use_data(popDevelopments, overwrite = TRUE)


# visualize evolution for entire BC area
bc <- filter(popDevelopments, label %in% toupper(c("British Columbia", "Kootenay")))

p <- ggplot(bc, aes(Age, Population, color = Gender)) +
  geom_line(aes(group = Year), alpha = 0.1) +
  geom_line(aes(frame = Year)) + 
  facet_wrap(~label, ncol = 1, scales = "free_y")
ggplotly(p) %>%
  animation_opts(100)


# ------------------------------------------------------------------------
# The data below isn't very granular, SAD!
# ------------------------------------------------------------------------
#
# library(rvest)
# library(curl)
# library(tibble)
# library(tidyr)
# library(dplyr)
#
# 
# # download overall population estimates, if necessary
# path <- "data-raw/pop-overall.csv"
# 
# if (!file.exists(path)) {
#   
#   domain <- "http://www.bcstats.gov.bc.ca"
#   
#   src <- html(
#     file.path(domain, "/StatisticsBySubject/Demography/PopulationEstimates.aspx")
#   )
#   
#   # exract hyperlink from page
#   link <- src %>%
#     html_nodes("a") %>%
#     html_attr("href") %>%
#     grep("BCquarterlypopulationestimates\\.csv$", ., value = TRUE)
#   
#   # download the data
#   curl_download(
#     paste0(domain, link), path
#   )
#   
# }
# 
# 
# # read in raw data to clean it up a bit
# qdat <- readr::read_csv(path, col_names = FALSE, skip = 3)
# 
# qdat <- qdat %>%
#   setNames(c("year", "Q1", "Q2", "Q3", "Q4")) %>%
#   gather(variable, value, -year)
#   
# # wtf???
# qdat[76, ]
# 
# # create a grid of all possible quarters
# rng <- range(qdat$year, na.rm = TRUE)
# g <- expand.grid(
#   seq.int(rng[1], rng[2]),
#   paste0("Q", 1:4)
# )
# g <- setNames(as_tibble(g), c("year", "variable"))
# 
# # join and clean-up
# newdat <- left_join(g, qdat)
# newdat$value[newdat$value == "-"] <- NA
# newdat$value <- as.numeric(newdat$value)
# newdat$time <- with(
#   newdat, year + recode(variable, Q1 = 0, Q2 = 1/4, Q3 = 1/2, Q4 = 3/4)
# )
# 
# # quick viz
# plot_ly(newdat, x = ~time, y = ~value) %>% add_lines()
# 
# 
# 
# # download regional population estimates, if necessary
# path <- "data-raw/pop-region.csv"
# 
# if (!file.exists(path)) {
#   
#   domain <- "http://www.bcstats.gov.bc.ca"
#   
#   src <- html(
#     file.path(domain, "/StatisticsBySubject/Demography/PopulationEstimates.aspx")
#   )
#   
#   # exract hyperlinks from page
#   links <- src %>%
#     html_nodes("a") %>%
#     html_attr("href") %>%
#     grep("DistrictandMuncipalPopulationEstimates", ., value = TRUE)
#   
#   # download the data
#   curl_download(
#     paste0(domain, link), path
#   )
#   
# }
# 
# # read in raw data to clean it up a bit
# d <- readr::read_csv(path, col_names = FALSE, skip = 3)
