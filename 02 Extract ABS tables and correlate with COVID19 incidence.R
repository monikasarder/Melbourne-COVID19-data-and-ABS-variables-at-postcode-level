library(tidyverse)
library(plotly)
library(viridis)
library(crosstalk)
library(corrr)
library(Hmisc)
library(readxl)
library(readr)
library(repurrrsive)
library(sf)
library(rstatix)

change_names <- function(x) {
  names(x) <- sub("POA_CODE_2016", "Postcode", names(x))
  x
}

### Load all tables for dataset into nested list
path <- "2016_GCP_POA_for_Vic_short-header/2016 Census GCP Postal Areas for VIC"
pc <- path %>%
  list.files(pattern = "*.csv", full.names=TRUE) %>%
  map(read_csv) %>%
  # we would prefer to work with numeric not doubles
  map(function(x) {map_if(x, is.double, ~ as.numeric(.x))}) %>%
  # Postcode columns and names need to match melbourne COVID object
  map(function(x) {map_if(x, is.character, ~ sub("POA", "", .x))}) %>%
  map(~ change_names(.x))

## Allocate ABS table names to nested lists
names(pc) <- path %>%
  list.files(pattern = "*.csv", full.names = FALSE) %>%
  str_extract("_[A-Za-z0-9]*") %>%
  str_remove("_")

# The metadata for each table name is set out in
metadata <- read_excel(
   "2016_GCP_POA_for_Vic_short-header/Metadata/Metadata_2016_GCP_DataPack.xlsx",
    skip = 9
  ) %>%
  janitor::clean_names(case = "big_camel")

# Read in COVID data from 01 Melbourne shapes and COVID19 data
dat <- readRDS("Melbourne_case_data.RDS")

#################
################ TABLE BY TABLE ANALYSIS
# Table G01 Selected Person Characteristics by Sex

G01 <- pc[["G01"]] %>%
  as_tibble() %>%
  # extract variables of interest
  select(Postcode, Tot_P_P, Lang_spoken_home_Oth_Lang_P)

# link ABS and COVID objects
datm <- dat %>%
  left_join(G01, by = "Postcode") %>%
  mutate(Suburb = trimws(Suburb)) %>%
  # normalise confirmed cases for population size (cases per 100K)
  mutate(Cases.per.100K = round(Confirmed / Tot_P_P * 100000, 2)) %>%
  replace_na(list(Cases.per.100K = 0))

# Outliers and anomalies. There were a few anomalies for exclusion
# Some postcodes had very small populations therefore the numbers were not meaningful (ie Population <1600)
# Some had been the subject of redevelopment (ie "Plenty","Somerton","Ardeer, Deer Park East")
# Melbourne University 3010 seasonal population affected by COVID19
datm <- datm %>%
  mutate(
    Include = if_else(
      Tot_P_P < 1500 | Suburb %in% c("Plenty", "Ardeer, Deer Park East") | Postcode == "3010",
      "No", "Yes"
    ),
    Reason.Excluded = case_when(
      Suburb %in% c("Plenty", "Somerton", "Ardeer, Deer Park East") ~ "Post 2016 development",
      Postcode == "3010" ~ "Melbourne Uni",
      TRUE ~  "Population under 1500")
  )

# Does it look right after normalisation and setting exclusions to zero

g <- list(
  showlegend = FALSE,
  showframe = FALSE,
  showcoastlines = FALSE,
  projection = list(type = "Mercator")
)

vis <- datm %>%
  mutate(Cases.per.100K = if_else(Include == "No", 0, Cases.per.100K))

vis %>%
  plot_geo(
    split = ~Postcode, showlegend = FALSE, hoverinfo = "text",
    text = ~ paste("Area:", Suburb, "<br>", "Cases:", Cases.per.100K)
  ) %>%
  add_sf(
    color = ~Cases.per.100K,
    hoveron = "points+fills"
  ) %>%
  layout(geo = g)

# Does the bar chart make sense
vis %>%
  ggplot(aes(x = reorder(Postcode, -Cases.per.100K), y = Cases.per.100K)) +
  geom_bar(stat = "identity")

# drop sf features, we no longer need them
datm <- datm %>%
  st_set_geometry(NULL) %>%
  as_tibble()

#################
# Read in ABS table G17 Total Personal Income (Weekly) by Age by Sex
# Income brackets are spread across two sheets
### select low income brackets at Person level

G17B <- pc[["G17B"]] %>%
  as_tibble() %>%
  select(
    Postcode, P_Neg_Nil_income_Tot, P_1_149_Tot, P_150_299_Tot, P_300_399_Tot,
    P_400_499_Tot, P_500_649_Tot, P_650_799_Tot, P_800_999_Tot
  )
G17C <- pc[["G17C"]] %>%
  as_tibble() %>%
  select(
    Postcode, P_1000_1249_Tot, P_1250_1499_Tot, P_1500_1749_Tot,
    P_1750_1999_Tot, P_2000_2999_Tot, P_3000_more_Tot, P_PI_NS_ns_Tot, P_Tot_Tot
  )
G17 <- left_join(G17B, G17C, by = "Postcode") %>%
  # Below poverty line taken as below $500 per week in 2016
  # Not stated income and negative income excluded (potentially wealthy retirees etc - unclear if relectance is due to above or below)
  mutate(
    Low.income = P_1_149_Tot + P_150_299_Tot + P_300_399_Tot + P_400_499_Tot,
    Total_included_income = P_Tot_Tot - P_PI_NS_ns_Tot - P_Neg_Nil_income_Tot,
    Percent.poverty = round(Low.income / Total_included_income * 100, digits = 2)
  ) %>%
  select(Postcode, Percent.poverty, Low.income, Total_included_income)

datm <- left_join(datm, G17, by = "Postcode")

datm %>%
  filter(Include == "No") %>%
  cor_test(Cases.per.100K, Percent.poverty)

#################
# Read in Table G33 Tenure Type by Landlord and Dwelling Structure
tenure <- c(
  "R_Tot_Total", "O_OR_Total", "O_MTG_Total", "Oth_ten_type_Total",
  "Ten_type_NS_Total"
)
tenure.perc <- str_c(tenure, "Percent", sep = "_")

G33 <- pc[["G33"]] %>%
  as_tibble() %>%
  select(Postcode, all_of(tenure), Total_Total) %>%
  mutate_at(
    .vars = tenure,
    .funs = list(Percent = ~ (. / Total_Total) * 100)
  ) %>%
  mutate_if(is.numeric, round, digits = 3) %>%
  select(Postcode, all_of(tenure.perc), all_of(tenure), Total_Total)

datm <- left_join(datm, G33, by = "Postcode")

# create correlation matrix
tenure.table <- datm %>%
  filter(Include == "Yes") %>%
  select(Cases.per.100K, all_of(tenure.perc)) %>%
  na.omit() %>%
  as.matrix() %>%
  rcorr()
tenure.table
data.frame(
    corr = round(tenure.table$r["Cases.per.100K", ], digits = 3),
    p.value = round(tenure.table$P["Cases.per.100K", ], digits = 3)
  ) %>%
  filter(!is.na(p.value)) %>%
  arrange(desc(corr))

#################
# Read in ABS table G32 Dwelling Structure

# not looking at unoccupied
dwelling.types <- c("House", "Semi.detached", "Flat", "Other", "Not.stated")
dwelling.perc <- str_c(dwelling.types, "Percent", sep = "_")

G32 <- pc[["G32"]] %>%
  as_tibble() %>%
  select(
    Postcode, OPDs_Separate_house_Persons, OPDs_SD_r_t_h_th_Tot_Psns, OPDs_Flt_apart_Tot_Psns,
    OPDs_Other_dwelling_Tot_Psns, Unoccupied_PDs_Psns, OPDs_Dwlling_structur_NS_Psns, Total_PDs_Persons
  ) %>%
  rename(
    House = OPDs_Separate_house_Persons,
    Semi.detached = OPDs_SD_r_t_h_th_Tot_Psns,
    Flat = OPDs_Flt_apart_Tot_Psns,
    Other = OPDs_Other_dwelling_Tot_Psns,
    Not.stated = OPDs_Dwlling_structur_NS_Psns,
    Unoccupied = Unoccupied_PDs_Psns,
    Total.dwelling = Total_PDs_Persons
  ) %>%
  mutate_at(
    .vars = dwelling.types,
    .funs = list(Percent = ~ (. / Total.dwelling) * 100)
  ) %>%
  mutate_if(is.numeric, round, digits = 3) %>%
  select(Postcode, all_of(dwelling.perc), all_of(dwelling.types), Total.dwelling)

datm <- left_join(datm, G32, by = "Postcode")

# create correlation matrix
dwelling.table <- datm %>%
  filter(Include == "Yes") %>%
  select(Cases.per.100K, all_of(dwelling.perc)) %>%
  na.omit() %>%
  as.matrix() %>%
  rcorr()

data.frame(
    corr = round(dwelling.table$r["Cases.per.100K", ], digits = 3),
    p.value = round(dwelling.table$P["Cases.per.100K", ], digits = 3)
  ) %>%
  filter(!is.na(p.value))

# Other ABS variables
# Table G01
# Correlation between language and COVID
datm <- datm %>%
  mutate(Lang.NE = Lang_spoken_home_Oth_Lang_P / Tot_P_P) %>%
  rename(Population = Tot_P_P)

datm %>%
  filter(Include == "Yes") %>%
  cor_test(Lang.NE, Cases.per.100K)

#######################
# Table G37 Dwelling Internet Connection by Dwelling Structure

G37 <- pc[["G37"]] %>%
  as_tibble() %>%
  mutate(
    No.home.access = round(I_NA_Total / Total_Total * 100, digits = 3)
  ) %>%
  select(Postcode, No.home.access, I_NA_Total)

datm <- left_join(datm, G37, by = "Postcode")

datm %>%
  filter(Include == "Yes") %>%
  cor_test(Cases.per.100K, No.home.access)


#################
# Table G02 Selected Medians and Averages
G02 <- pc[["G02"]] %>%
  as.data.frame() %>%
  select(Postcode, Median_age_persons, Median_tot_hhd_inc_weekly, Average_household_size)

datm <- left_join(datm, G02, by = "Postcode")


# Test correlations COVID with age, income, household
datm %>%
  filter(Include == "Yes") %>%
  cor_test(Cases.per.100K, Median_age_persons)

datm %>%
  filter(Include == "Yes") %>%
  cor_test(Cases.per.100K, Median_tot_hhd_inc_weekly)

datm %>%
  filter(Include == "Yes") %>%
  cor_test(Cases.per.100K, Average_household_size)

#################
# Read in ABS table G351 Industry of Employment by Age and Sex

occupations <- c(
  "Tot_OcMngr", "Tot_OcProf", "Tot_OcTechTrdW", "Tot_OcComPerS",
  "Tot_OcClericAdm", "Tot_OcSalesWk", "Tot_OcMacOp_Driv", "Tot_OcLab", "Tot_OcID_NS"
)
occupations.perc <- str_c(occupations, "Percent", sep = "_")

G53B <- pc[["G53B"]] %>%
  as_tibble() %>%
  select(Postcode, all_of(occupations), Tot_Tot) %>%
  mutate_at(
    .vars = occupations,
    .funs = list(Percent = ~ (. / Tot_Tot) * 100)
  ) %>%
  mutate_if(is.numeric, round, digits = 3) %>%
  select(Postcode, all_of(occupations.perc))

datm <- left_join(datm, G53B, by = "Postcode")

# create correlation matrix
occupations.table <- datm %>%
  filter(Include == "Yes") %>%
  select(Cases.per.100K, all_of(occupations.perc)) %>%
  na.omit() %>%
  as.matrix() %>%
  rcorr()

data.frame(
    corr = round(occupations.table$r["Cases.per.100K", ], digits = 3),
    p.value = round(occupations.table$P["Cases.per.100K", ], digits = 3)
  ) %>%
  filter(!is.na(p.value)) %>%
  arrange(desc(corr))

#############################
# Read in ABS table G35 Industry of Employment by Age and Sex
# Total person level columns
G51C <- pc[["G51C"]] %>%
  as_tibble()
G51D <- pc[["G51D"]] %>%
  as_tibble()
G51 <- left_join(G51C, G51D, by = "Postcode")

ind <- names(G51) %>%
  str_subset("Tot$") %>%
  str_subset("^P")
ind.perc <- str_c(ind, "Percent", sep = "_")

G51 <- G51 %>%
  select(Postcode, all_of(ind))  %>%
  mutate_at(
    .vars = ind,
    .funs = list(Percent = ~ (. / P_Tot_Tot) * 100)
  ) %>%
  mutate_if(is.numeric, round, digits = 3) %>%
  select(Postcode, all_of(ind), all_of(ind.perc), P_Tot_Tot)

datm <- left_join(datm, G51, by = "Postcode")

# create correlation matrix
industry.table <- datm %>%
  filter(Include == "Yes") %>%
  select(Cases.per.100K, all_of(ind.perc)) %>%
  na.omit() %>%
  as.matrix() %>%
  rcorr()
industry.table

data.frame(
    corr = round(industry.table$r["Cases.per.100K", ], digits = 3),
    p.value = round(industry.table$P["Cases.per.100K", ], digits = 3)
  ) %>%
  filter(!is.na(p.value)) %>%
  arrange(desc(corr))

#### JOIN normalised variables with sf object

dat %>%
  select(-postcode_num_2016, -cent_lat, -cent_long, -Suburb, -Confirmed, -Active) %>%
  left_join(datm, by = "Postcode") %>%
  saveRDS("Melbourne.spatial.COVID19.RDS")
