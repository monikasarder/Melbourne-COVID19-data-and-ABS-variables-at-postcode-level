library(tidyverse)
library(readxl)
library(readr)
library(googlesheets4)
library(googledrive)
library(rvest)


#read 6 August COVID data from Age article by Schneiders and Butte
#https://www.theage.com.au/national/victoria/victoria-coronavirus-data-find-the-number-of-active-covid-19-cases-in-your-postcode-20200731-p55hg2.html 

butt_url <-"https://docs.google.com/spreadsheets/d/1oxJt0BBPzk-w2Gn1ImO4zASBCdqeeLJRwHEA4DASBFQ/edit#gid=0"

ssid <- as_sheets_id(butt_url)

ssid

cases <- read_sheet(butt_url, sheet = "Data (August 6)")
cases <- cases %>% mutate(Postcode = as.numeric(Postcode))

#limit to Melbourne postcodes
#read postcode list from Matthew Proctors site
proctor_url <-"https://www.matthewproctor.com/Content/postcodes/australian_postcodes.csv"
download.file(proctor_url,
              "postcodes.csv")

postcodes <-read.csv("postcodes.csv", stringsAsFactors = FALSE)

postcodes <- postcodes %>%
  filter(str_detect(sa4name, "Melbourne")) %>%
  select(postcode) %>%
  unique()

cases <- cases %>% filter(cases$Postcode %in% postcodes$postcode)

#list of names was copied manually into "The_Age_suburb_names.csv"
suburb_names <- read_csv("suburb_names.csv")

cases <- left_join(cases, suburb_names, by = "Postcode")

saveRDS(cases, "Melbourne_case_data.RDS")
