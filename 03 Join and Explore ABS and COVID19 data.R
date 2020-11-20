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

# Read in COVID data from 01 Melbourne shapes and COVID19 data
dat <- readRDS("Outputs/01.Melbourne.case.data.RDS")

# Read in Census data at postcode level
pc <- readRDS("Outputs/02.ABS.Victoria.postcodes.RDS")
#################
# Link COVID data to ABS population data to NORMALISE by postcode population
# Population data (and other variables we are interested in) are in
#Table G01 Selected Person Characteristics by Sex

G01 <- pc[["G01"]] %>%
  as_tibble() %>%
  # extract variables of interest
  select(Postcode, Population = Tot_P_P, Language.NE = Lang_spoken_home_Oth_Lang_P)

# COVID19 data and population data
datm <- dat %>%
  left_join(G01, by = "Postcode") %>%
  mutate(Suburb = trimws(Suburb)) %>%
  # normalise confirmed cases for population size (cases per 100K)
  mutate(Cases.per.100K = round(Confirmed / Population * 100000, 2)) %>%
  replace_na(list(Cases.per.100K = 0))

# Outliers and anomalies. There were a few anomalies for exclusion
# Some postcodes had very small populations therefore the numbers were not meaningful (ie Population <1600)
# Some had been the subject of redevelopment (ie "Plenty","Somerton","Ardeer, Deer Park East")
# Melbourne University 3010 seasonal population affected by COVID19
datm <- datm %>%
  mutate(
    Include = if_else(
      Population < 1500 | Suburb %in% c("Plenty", "Ardeer, Deer Park East") | Postcode == "3010",
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

# drop sf features, we do not need them to test correlation 
datm <- datm %>%
  st_set_geometry(NULL) %>%
  as_tibble()

## Table G01
#Normalise and check correlation with Language spoken at home (other language)

datm <- datm %>%
  mutate(Lang.NE.Percent = round(Language.NE/Population *100, digits = 2))

datm %>%
  filter(Include == "Yes") %>%
  cor_test(Lang.NE.Percent, Cases.per.100K)


#############################
# Read in ABS table G51 Industry of Employment by Age and Sex
# Spread across two sheets, combine for full table
# Explore for correlated industry
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


# create correlation matrix
industry.table <- datm %>%
  left_join(G51, by = "Postcode") %>%
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

G51 <- G51 %>%
  select(Postcode, 
         Employed.transport = P_Trans_post_wrehsg_Tot, 
         Employed.Persons = P_Tot_Tot,  
         Employed.transport.Percent = P_Trans_post_wrehsg_Tot_Percent)

datm <- left_join(datm, G51, by = "Postcode")

#################
# Read in ABS table G17 Total Personal Income (Weekly) by Age by Sex
# Spread across two sheets, combine for full table
# Make new variable Weekly Income Under $500 excluding Nil and Negative (likely to be retirees or have other assets or supports)

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
    Low.income.Persons = P_1_149_Tot + P_150_299_Tot + P_300_399_Tot + P_400_499_Tot,
    Total.over.15 = P_Tot_Tot - P_PI_NS_ns_Tot - P_Neg_Nil_income_Tot,
    Low.income.Percent = round(Low.income.Persons / Total.over.15 * 100, digits = 2)
  ) 

#check correlation with low income

datm %>%
  left_join(G17, by = "Postcode") %>%
  filter(Include == "No") %>%
  cor_test(Cases.per.100K, Low.income.Percent)

G17 <- G17 %>% 
  select(Postcode,  Total.over.15,  Low.income.Persons,Low.income.Percent)

datm <- left_join(datm, G17, by = "Postcode")



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


# create correlation matrix
tenure.table <- datm %>%
  left_join(G33, by = "Postcode") %>%
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


G33 <- G33 %>%
  select(Postcode, 
         Rented.Dwellings = R_Tot_Total, 
         Occupied.dwellings = Total_Total,
         Rented.Percent = R_Tot_Total_Percent )

datm <- left_join(datm, G33, by = "Postcode")
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

# create correlation matrix
dwelling.table <- datm %>%
  left_join(G32, by = "Postcode") %>%
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

G32 <- G32 %>% 
  select(Postcode, 
  House.Persons = House,
  Semi.detached.Persons = Semi.detached,
  Flat.Persons = Flat,
  Dwelling.persons = Total.dwelling,
  House.Percent = House_Percent,
  Semi.detached.Percent = Semi.detached_Percent,
  Flat.Percent = Flat_Percent)

datm <- left_join(datm, G32, by = "Postcode")

#######################
# Table G37 Dwelling Internet Connection by Dwelling Structure
#By Occupied private dwellings

G37 <- pc[["G37"]] %>%
  as_tibble() %>%
  mutate(
    No.internet.Percent = round(I_NA_Total / Total_Total * 100, digits = 3)
  ) 



datm %>%
  left_join(G37, by = "Postcode") %>%
  filter(Include == "Yes") %>%
  cor_test(Cases.per.100K, No.internet.Percent)



G37 <- G37 %>%
  select(Postcode, 
         No.home.access = I_NA_Total,
         No.internet.Percent)

datm <- left_join(datm, G37, by = "Postcode")

#################
# Table G02 Selected Medians and Averages

G02 <- pc[["G02"]] %>%
  as.data.frame() 


# Test correlations COVID with age, income, household
general.stats <- datm %>%
  left_join(G02, by = "Postcode") %>%
  filter(Include == "Yes") %>%
  select(Cases.per.100K, Median_age_persons, Median_tot_hhd_inc_weekly, Average_household_size) %>%
  na.omit() %>%
  as.matrix() %>%
  rcorr()


data.frame(
  corr = round(general.stats$r["Cases.per.100K", ], digits = 3),
  p.value = round(general.stats$P["Cases.per.100K", ], digits = 3)
) %>%
  filter(!is.na(p.value))

G02 <- G02 %>% 
  select(Postcode,
         Median.age = Median_age_persons,
         Median.household.income = Median_tot_hhd_inc_weekly)

datm <- datm %>%
  left_join(G02, by = "Postcode")
#################
# Read in ABS table G53 Labour Force Status by Age and Sex and Occupation

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
    .funs = list(Percent = ~ (. / Tot_Tot) * 100)) %>%
  mutate_if(is.numeric, round, digits = 3)



# create correlation matrix
occupations.table <- datm %>%
  left_join(G53B, by = "Postcode") %>%
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

G53B <- G53B %>% 
  select(Postcode,
         Total.drivers = Tot_OcMacOp_Driv,
         Total.service.workers = Tot_OcComPerS,     
         Total.labourers = Tot_OcLab,
         Total.drivers.Percent = Tot_OcMacOp_Driv_Percent,
         Total.service.workers.Percent = Tot_OcComPerS_Percent,  
         Total.labourers.Percent = Tot_OcLab_Percent)

datm <- left_join(datm, G53B, by = "Postcode")

#### JOIN normalised variables with sf object

dat %>%
  select(-postcode_num_2016, -cent_lat, -cent_long, -Suburb, -Confirmed, -Active) %>%
  left_join(datm, by = "Postcode") %>%
  saveRDS("Outputs/03.Melbourne.spatial.COVID19.RDS")



