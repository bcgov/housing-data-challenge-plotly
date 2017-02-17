# adpated from CANSIM2R:::downloadCANSIM

CANSIM <- function(n = "4010004") {
  if (!is.character(n)) stop("n should be a character string")
  temp <- tempfile(fileext = ".zip")
  url <- "http://www20.statcan.gc.ca/tables-tableaux/cansim/csv/"
  filename <- paste0("0", n, "-eng")
  url <- paste0(url, filename, ".zip")
  curl::curl_download(url, temp, quiet = TRUE)
  unzip(temp, exdir = "data-raw")
  readr::read_csv(file.path("data-raw", paste0(filename, ".csv")))
}

#d <- CANSIM()
d2 <- CANSIM("0510011")