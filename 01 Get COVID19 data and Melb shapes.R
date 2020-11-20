library(tidyverse)
library(sf)
library(absmapsdata)
library(plotly)
library(crosstalk)
library(readxl)
library(googlesheets4)

# STEP 01 Read 6 August COVID data from Age article by Butt and Stehle
# at https://www.theage.com.au/national/victoria/victoria-coronavirus-data-find-the-number-of-active-covid-19-cases-in-your-postcode-20200731-p55hg2.html
#This is from a Google sheet you will need a google authentication token

#cases <- "https://docs.google.com/spreadsheets/d/1oxJt0BBPzk-w2Gn1ImO4zASBCdqeeLJRwHEA4DASBFQ/edit#gid=0" %>%
#  read_sheet(sheet = "Data (August 6)") %>%
#  mutate(Postcode = as.character(Postcode)) %>%
#  rename(
#    Confirmed = "Confirmed cases (ever)",
#    Active = "Active cases (current)"
#  )

#write.csv(cases, "Inputs/COVID19.Confirmed.cases.20.09.06.csv", row.names = FALSE)

cases <- read.csv("Inputs/COVID19.Confirmed.cases.20.09.06.csv")
# STEP 02 get list of Melb postcodes and suburb names
# Copypasta from the Butt and Stehle article
# Make suburb names more readable

melb_names <- "Inputs/melbourne.postcode.list.csv" %>%
  read_csv() %>%
  rename(Suburb = `City/ Town`) %>%
  mutate(
    Postcode = as.character(Postcode),
    Suburb = iconv(Suburb, "utf-8", "ascii", sub = " "),
    Suburb = str_replace_all(Suburb, "melbourne", "Melbourne")
  ) %>%
  filter(!Postcode %in% c("Unknown", "Others")) %>%
  select(-State, -District)

# STEP 03 extract shapes from absmaps
# absmaps data - look at postcode level - melb postcodes only
# add melbourne features

mapdata <- postcode2016 %>%
  filter(postcode_2016 %in% melb_names$Postcode) %>%
  rename(Postcode = postcode_2016)

# Step 04 add in case data to Melbourne shape data

dat <- mapdata %>%
  left_join(melb_names, by = "Postcode") %>%
  left_join(cases, by = "Postcode") %>%
  replace_na(list(Confirmed = 0))

# Step 05
# determine layout and plot
# visualise to see that it makes sense

g <- list(
  showlegend = FALSE,
  showframe = FALSE,
  showcoastlines = FALSE,
  projection = list(type = "Mercator")
)

dat %>%
  plot_geo(
    split = ~Postcode, showlegend = FALSE, hoverinfo = "text",
    text = ~ paste("Area:", Suburb, "<br>", "Cases:", Confirmed)
  ) %>%
  add_sf(
    color = ~Confirmed,
    hoveron = "points+fills"
  ) %>%
  layout(geo = g)

saveRDS(dat, "Outputs/01.Melbourne.case.data.RDS")



