library(tidyverse)
library(sf)
library(absmapsdata)
library(plotly)
library(viridis)
library(crosstalk)
library(DT)
library(corrr)
library(Hmisc)
library(readxl)
library(readr)
library(repurrrsive)

###Load all tables for dataset
path ="2016_GCP_POA_for_Vic_short-header/2016 Census GCP Postal Areas for VIC"
filenames <- list.files(path,
                        pattern = "*.csv", full.names = TRUE)  

pc = map(filenames, read_csv)

##Create list of data frame names without the ".csv" part 
table_names <-str_sub(filenames,start = 83, end = -13)

pc <- set_names(pc, table_names)

# The metadata for each table name is set out in 
metadata <- read_excel(
  "2016_GCP_POA_for_Vic_short-header/Metadata/Metadata_2016_GCP_DataPack.xlsx")

#we would prefer to work with numeric not doubles
pc <-
  map(pc, function(x) 
    map_if(x, is.double, ~as.numeric(.x)))


pc <-
  map(pc, function(x) map_if(x, is.character,
                             ~sub('POA', '', .x)))


change_names <- function(x) {
  names(x) <- sub("POA_CODE_2016", "Postcode", names(x))
  x
}

pc <- map(pc, ~change_names(.x))
  
#LINK COVID DATA TO ABS DATA elements
dat <-readRDS("Melbourne_case_data.RDS")


#################
#Table G01 Selected Person Characteristics by Sex

G01 <-pc[['G01']] %>% 
  as.data.frame()

#extract variables of interest - 
G01 <- G01 %>%
  select(Postcode, Tot_P_P, Lang_spoken_home_Oth_Lang_P)

#link to dataset
datm <- left_join(dat, G01, by = "Postcode")
names(datm)

#normalise confirmed cases for population size (cases per 100K)
datm <- datm %>% 
  mutate(Cases.per.100K = `Confirmed cases (ever)`/Tot_P_P*100000) %>%
  mutate(Cases.per.100K = round(Cases.per.100K, 2))

datm$Cases.per.100K[is.na(datm$Cases.per.100K)]<-0

#Postcode sizes very variable 




#Remove postcodes with small population  
datm$Cases.per.100K<-ifelse(datm$Tot_P_P <1500, 0, datm$Cases.per.100K)

#Remove postcodes with recent estates
datm$Cases.per.100K <-ifelse(datm$Suburb %in% c("Plenty","Somerton","Ardeer, Deer Park East","University Of Melbourne"), 0, datm$Cases.per.100K)

#Clean up names
datm <- datm %>% mutate(Suburb = str_replace(Suburb, "melbourne","Melbourne"))

#Does it look right

g <- list(showlegend = FALSE,
          showframe = FALSE,
          showcoastlines = FALSE,
          projection = list(type = 'Mercator')
)

datm %>%
  plot_geo(split = ~Postcode, showlegend = FALSE, hoverinfo = "text",
           text = ~paste("Area:", Suburb, "<br>","Cases:", Cases.per.100K)) %>%
  add_sf(color = ~Cases.per.100K,
         hoveron = "points+fills") %>%
  layout(geo = g)

#drop geometry
datm <- st_set_geometry(datm, NULL)

#Remove postcodes with recent estates
#datm <-ifelse(datm$Suburb %in% c("Plenty","Somerton","Ardeer, Deer Park East","University Of Melbourne"), 0, datm$Cases.per.100K)


datm <- datm %>% 
  filter(Tot_P_P!=0)
ggplot(datm, aes(x = reorder(Postcode, -Cases.per.100K), y = Cases.per.100K)) +
  geom_bar(stat = "identity")

#remove extreme values
datm <-datm %>%
  filter(!Suburb %in% c("Plenty","Somerton","Ardeer, Deer Park East","University Of Melbourne"))


#################
# Read in ABS table G17 Total Personal Income (Weekly) by Age by Sex
#Income brackets are spread across two sheets

G17B <-pc[['G17B']] %>% 
  as.data.frame()

G17C <-pc[['G17C']] %>% 
  as.data.frame()

names(G17C)

### select low income brackets at Person level
G17B <- G17B %>% select(Postcode, P_Neg_Nil_income_Tot, P_1_149_Tot ,P_150_299_Tot ,P_300_399_Tot, 
                        P_400_499_Tot,P_500_649_Tot,P_650_799_Tot , P_800_999_Tot)

G17C <- G17C %>% select(Postcode,P_1000_1249_Tot, P_1250_1499_Tot,P_1500_1749_Tot, 
                        P_1750_1999_Tot, P_2000_2999_Tot,  P_3000_more_Tot, P_PI_NS_ns_Tot, P_Tot_Tot)


G17 <- left_join(G17B, G17C, by  = "Postcode")

#Below poverty line taken as below $500 per week. Not stated income and negative income (potentially wealthy retirees etc) excluded
G17 <- G17 %>%
  mutate(Low.income = P_1_149_Tot +P_150_299_Tot + P_300_399_Tot + P_400_499_Tot) %>%
  mutate(Total_included = P_Tot_Tot-P_PI_NS_ns_Tot-P_Neg_Nil_income_Tot) %>%
  mutate(Percent.poverty = round(Low.income/Total_included*100, digits = 2))

G17 <- G17 %>% select(Postcode, Percent.poverty, Low.income)

datm <- left_join(datm, G17, by = "Postcode")


cor.test(datm$Cases.per.100K, datm$Percent.poverty)





#################
#Read in Table G33 Tenure Type by Landlord and Dwelling Structure

G33<-pc[['G33']] %>% 
  as.data.frame()


tenure <- c("R_Tot_Total", "O_OR_Total", "O_MTG_Total","Oth_ten_type_Total",
            "Ten_type_NS_Total")

head(G33)
G33 <- G33 %>% select(Postcode, all_of(tenure), Total_Total)

tenure.perc <-str_c(tenure, "Percent", sep = "_")


G33 <-G33 %>%
  mutate_at(.vars = tenure,
            .funs = list(Percent = ~ (./Total_Total)*100))

G33 <-G33 %>% mutate_if(is.numeric, round, digits=3)

G33 <- G33 %>% select(Postcode, all_of(tenure.perc))

datm <- left_join(datm, G33, by = "Postcode")

#create correlation matrix
tenure.table <- datm %>% 
  select(Cases.per.100K, all_of(tenure.perc)) %>%
  na.omit() %>%
  as.matrix()%>%
  rcorr()
tenure.table
tenure.corr <- data.frame(corr = round(tenure.table$r['Cases.per.100K',], digits = 3),
                          p.value = round(tenure.table$P['Cases.per.100K',], digits =3)) 

tenure.corr<- tenure.corr[-1,] %>%
  arrange(desc(corr))

tenure.corr

#################
# Read in ABS table G32 Dwelling Structure

G32 <-pc[['G32']] %>% 
  as.data.frame()

names(G32)

G32 <- G32 %>% select(Postcode, OPDs_Separate_house_Persons,  OPDs_SD_r_t_h_th_Tot_Psns,OPDs_Flt_apart_Tot_Psns,
                      OPDs_Other_dwelling_Tot_Psns, Unoccupied_PDs_Psns, OPDs_Dwlling_structur_NS_Psns,Total_PDs_Persons)


G32 <-   G32 %>%  rename (House = OPDs_Separate_house_Persons, 
                          Semi.detached = OPDs_SD_r_t_h_th_Tot_Psns, 
                          Flat = OPDs_Flt_apart_Tot_Psns,
                          Other = OPDs_Other_dwelling_Tot_Psns, 
                          Not.stated = OPDs_Dwlling_structur_NS_Psns, 
                          Unoccupied = Unoccupied_PDs_Psns,
                          Total.dwelling =Total_PDs_Persons)

#not looking at unoccupied
dwelling.types <- c("House","Semi.detached","Flat","Other", "Not.stated")

dwelling.perc <-str_c(dwelling.types, "Percent", sep = "_")


G32 <-G32 %>%
  mutate_at(.vars = dwelling.types,
            .funs = list(Percent = ~ (./Total.dwelling)*100))

G32 <-G32 %>% mutate_if(is.numeric, round, digits=3)

G32 <- G32 %>% select(Postcode, all_of(dwelling.perc))

datm <- left_join(datm, G32, by = "Postcode")

#create correlation matrix
dwelling.table <- datm %>% 
  select(Cases.per.100K, all_of(dwelling.perc)) %>%
  na.omit() %>%
  as.matrix() %>%
  rcorr()

dwelling.corr <- data.frame(corr = round(dwelling.table$r['Cases.per.100K',], digits = 3),
                            p.value = round(dwelling.table$P['Cases.per.100K',], digits =3))


dwelling.corr<- dwelling.corr[-1,]

dwelling.corr

#Other ABS variables
#Table G01
#Correlation between language and COVID
datm <- datm %>% mutate(Lang.NE = Lang_spoken_home_Oth_Lang_P/Tot_P_P)

cor.test(datm$Lang.NE, datm$Cases.per.100K)


#######################
#Table G37 Dwelling Internet Connection by Dwelling Structure

G37 <-pc[['G37']] %>% 
  as.data.frame() 

names(G37)

G37 <- G37 %>% 
  mutate(No.home.access = round(I_NA_Total/Total_Total*100), digits = 3) %>%
  select(Postcode, No.home.access)

datm <-left_join(datm, G37, by = "Postcode")

cor.test(datm$Cases.per.100K, datm$No.home.access)
#Test correlations COVID with age, income, household
cor.test(datm$Cases.per.100K, datm$Median_age_persons)


#################
#Table G02 Selected Medians and Averages
G02 <-pc[['G02']] %>% 
  as.data.frame() 

G02 <- G02 %>% select(Postcode, Median_age_persons, Median_tot_hhd_inc_weekly, Average_household_size)

datm <-left_join(datm, G02, by = "Postcode")


#Test correlations COVID with age, income, household
cor.test(datm$Cases.per.100K, datm$Median_age_persons)

cor.test(datm$Cases.per.100K, datm$Median_tot_hhd_inc_weekly)

cor.test(datm$Cases.per.100K, datm$Average_household_size)



#################
# Read in ABS table G351 Industry of Employment by Age and Sex

G53B<-pc[['G53B']] %>% 
  as.data.frame()
names(G53B)

occupations <- c("Tot_OcMngr"     ,  "Tot_OcProf"   ,    "Tot_OcTechTrdW"  , "Tot_OcComPerS"  , 
 "Tot_OcClericAdm" , "Tot_OcSalesWk"   , "Tot_OcMacOp_Driv", "Tot_OcLab"    ,    "Tot_OcID_NS" )

G53B <- G53B %>% select(Postcode, all_of(occupations), Tot_Tot)

occupations.perc <-str_c(occupations, "Percent", sep = "_")


G53B <-G53B %>%
  mutate_at(.vars = occupations,
            .funs = list(Percent = ~ (./Tot_Tot)*100))

G53B <-G53B %>% mutate_if(is.numeric, round, digits=3)

G53B <- G53B %>% select(Postcode, all_of(occupations.perc))

datm <- left_join(datm, G53B, by = "Postcode")

#create correlation matrix
occupations.table <- datm %>% 
  select(Cases.per.100K, all_of(occupations.perc)) %>%
  na.omit() %>%
  as.matrix()%>%
  rcorr()

occupations.corr <- data.frame(corr = round(occupations.table$r['Cases.per.100K',], digits = 3),
                            p.value = round(occupations.table$P['Cases.per.100K',], digits =3)) 

occupations.corr<- occupations.corr[-1,] %>%
  arrange(desc(corr))

occupations.corr

#############################3
# Read in ABS table G35 Industry of Employment by Age and Sex

G51C <-pc[['G51C']] %>% 
  as.data.frame()
G51D <-pc[['G51D']] %>% 
  as.data.frame()

G51 <- left_join(G51C, G51D, by = "Postcode")

#Total person level columns
ind <- str_subset(names(G51), "Tot$")
ind <- str_subset(ind, "^P")


G51 <- G51 %>% select(Postcode, all_of(ind))

ind.perc <-str_c(ind, "Percent", sep = "_")


G51 <-G51 %>%
  mutate_at(.vars = ind,
            .funs = list(Percent = ~ (./P_Tot_Tot)*100))

G51 <-G51 %>% mutate_if(is.numeric, round, digits=3)

G51 <- G51 %>% select(Postcode, all_of(ind.perc))

datm <- left_join(datm, G51, by = "Postcode")

#create correlation matrix
industry.table <- datm %>% 
  select(Cases.per.100K, all_of(ind.perc)) %>%
  na.omit() %>%
  as.matrix()%>%
  rcorr()
industry.table
industry.corr <- data.frame(corr = round(industry.table$r['Cases.per.100K',], digits = 3),
                            p.value = round(industry.table$P['Cases.per.100K',], digits =3)) 
  
industry.corr<- industry.corr[-1,] %>%
  arrange(desc(corr))


industry.corr
