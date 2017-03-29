library(curl)
library(stringi)

curl_download(
  "http://www20.statcan.gc.ca/tables-tableaux/cansim/csv/00260018-eng.zip",
  "data-raw/00260018-eng.zip"
)

unzip("data-raw/00260018-eng.zip", exdir = "data-raw/")
ass <- readr::read_csv("data-raw/00260018-eng.csv")
ass$GEO <- toupper(stringi::stri_enc_toascii(ass$GEO))
ass <- ass[grepl("BRITISH COLUMBIA", ass$GEO),]
ass$GEO <- sapply(strsplit(ass$GEO, ","), "[[", 1)

ggplot(ass, aes(Ref_Date, Value, color = GEO)) + 
  geom_line()
