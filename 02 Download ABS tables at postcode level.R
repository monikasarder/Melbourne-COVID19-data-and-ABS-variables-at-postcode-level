library(tidyverse)
library(readxl)
library(janitor)

# Download ABS Census postcode level data from the ABS website.
# The following options were selected from https://datapacks.censusdata.abs.gov.au/datapacks/:
#  2016 Census Datapacks > General Community > Profile > Postal Areas > Vic

temp <- tempfile()
"https://www.censusdata.abs.gov.au/CensusOutput/copsubdatapacks.nsf/All%20docs%20by%20catNo/2016_GCP_POA_for_Vic/$File/2016_GCP_POA_for_Vic_short-header.zip?OpenElement&key=ee324574-4a37-9d0e-b6ff-66e2d667a73f" %>%
  download.file(temp)
unzip(temp, exdir = "./2016_GCP_POA_for_Vic_short-header")
unlink(temp)



### Load all tables for dataset into nested list and rename Postcode column to allow linkage to spatial object

#Postcode name change function
change_names <- function(x) {
  names(x) <- sub("POA_CODE_2016", "Postcode", names(x))
  x
}

path <- "2016_GCP_POA_for_Vic_short-header/2016 Census GCP Postal Areas for VIC"

ABS_Melbourne_postcodes <- path %>%
  list.files(pattern = "*.csv", full.names=TRUE) %>%
  map(read_csv) %>%
  # we would prefer to work with numeric not doubles
  map(function(x) {map_if(x, is.double, ~ as.numeric(.x))}) %>%
  # Postcode columns and names need to match melbourne COVID object
  map(function(x) {map_if(x, is.character, ~ sub("POA", "", .x))}) %>%
  map(~ change_names(.x))


## Allocate ABS table names to nested lists
names(ABS_Melbourne_postcodes) <- path %>%
  list.files(pattern = "*.csv", full.names = FALSE) %>%
  str_extract("_[A-Za-z0-9]*") %>%
  str_remove("_")

# The metadata for each TABLE name (table number, name and population) is set out in
metadata_summary <- read_excel(
  "2016_GCP_POA_for_Vic_short-header/Metadata/Metadata_2016_GCP_DataPack.xlsx",
  skip = 9
) %>%
  janitor::clean_names(case = "big_camel")

# The metadata for each VARIABLE for each table is set out in
metadata_cell_descriptors <- read_excel(
  "2016_GCP_POA_for_Vic_short-header/Metadata/Metadata_2016_GCP_DataPack.xlsx",
  sheet = 2,
  skip = 10
) %>%
  janitor::clean_names(case = "big_camel")

#save ABS data at vic level
saveRDS(ABS_Melbourne_postcodes, "Outputs/02.ABS.Victoria.postcodes.RDS")
