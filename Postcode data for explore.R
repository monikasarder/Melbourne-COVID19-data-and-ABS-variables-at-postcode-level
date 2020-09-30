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


melb_names <- read_excel("melbourne.postcode.list.xlsx")

melb_names <- melb_names %>% 
  mutate(Postcode = as.character(Postcode))



dat <-read_excel("August.6.POA.xlsx")
names <- read_excel("POA.Codes.names.xlsx")

names <- names %>%
  rename( Postcode= POSTCODE) %>%
  mutate(Postcode = as.character(Postcode))

dat <- left_join(dat, names, by = "Postcode") 

dat <- dat %>%
  filter(!Postcode %in% c("Unknown","Others")) 


mapdata <-postcode2016

mapdata <- mapdata %>%
  filter(postcode_2016 %in% melb_names$Postcode)



datm %>%
  plot_geo(split = ~Postcode, showlegend = FALSE, hoverinfo = "text",
           text = ~paste("Area:", Suburb)) %>%
  add_markers(x= ~cent_long, y = ~cent_lat)%>% 
  add_sf() 


mapdata <- mapdata %>%
  rename(Postcode = postcode_2016)

datm <-left_join(mapdata, dat, by = "Postcode")

datm <-left_join(datm, melb_names, by = "Postcode")

names(datm)
datm <- datm %>%
  rename( Suburb = `City/ Town`, Cumulative.cases =`Confirmed cases (ever)`)

datm$Cumulative.cases[is.na(datm$Cumulative.cases)] <- 0


datm$POA_CODE_2016 <-datm$Postcode
#datm %>%
#  plot_geo(split = ~Postcode, showlegend = FALSE, hoverinfo = "text",
#           text = ~paste("Area:", Suburb)) %>%
#  add_markers(x= ~cent_long, y = ~cent_lat)%>% 
#  add_sf() 


###Load all files

filenames <- list.files(path ="/Users/monika/Desktop/Polygons/2016_GCP_POA_for_Vic/2016 Census GCP Postal Areas for VIC",
                        pattern = "*.csv", full.names = TRUE)  

##Create list of data frame names without the ".csv" part 
names <-str_sub(filenames,start = 101, end = -13)

for(i in names){
  filepath <- file.path("/Users/monika/Desktop/Polygons/2016_GCP_POA_for_Vic/2016 Census GCP Postal Areas for VIC",
                        paste("2016Census_",i,"_VIC_POA.csv",sep=""))
  assign(i, read.csv(filepath))
}
G01<- G01 %>% mutate(POA_CODE_2016= str_replace(POA_CODE_2016,"POA",""))

G01[2:ncol(G01)] <- lapply(G01[2:ncol(G01)], as.numeric)

#Population profile
G01 <- select(G01, Tot_P_P, Lang_spoken_home_Oth_Lang_P, POA_CODE_2016)

#G02 Selected medians

G02<- G02 %>% mutate(POA_CODE_2016= str_replace(POA_CODE_2016,"POA",""))

G02[2:ncol(G02)] <- lapply(G02[2:ncol(G02)], as.numeric)


#Job
G57B<- G57B %>% mutate(POA_CODE_2016= str_replace(POA_CODE_2016,"POA",""))

G57B[2:ncol(G57B)] <- lapply(G57B[2:ncol(G57B)], as.numeric)

#Rent
G33<- G33 %>% mutate(POA_CODE_2016= str_replace(POA_CODE_2016,"POA",""))

G33[2:ncol(G33)] <- lapply(G33[2:ncol(G33)], as.numeric)

#Rent
G33<- G33 %>% mutate(POA_CODE_2016= str_replace(POA_CODE_2016,"POA",""))

G33[2:ncol(G33)] <- lapply(G33[2:ncol(G33)], as.numeric)

#Industry
G53A<- G53A %>% mutate(POA_CODE_2016= str_replace(POA_CODE_2016,"POA",""))

G53A[2:ncol(G53A)] <- lapply(G53A[2:ncol(G53A)], as.numeric)
names(G53A)
#Commute
G59<- G59 %>% mutate(POA_CODE_2016= str_replace(POA_CODE_2016,"POA",""))

G59[2:ncol(G59)] <- lapply(G59[2:ncol(G59)], as.numeric)

#Age intervals
G03<- G03 %>% mutate(POA_CODE_2016= str_replace(POA_CODE_2016,"POA",""))

G03[2:ncol(G03)] <- lapply(G03[2:ncol(G03)], as.numeric)

#Labour force
G43B<- G43B %>% mutate(POA_CODE_2016= str_replace(POA_CODE_2016,"POA",""))

G43B[2:ncol(G43B)] <- lapply(G43B[2:ncol(G43B)], as.numeric)

names(G43B)

#Labour force
G37<- G37 %>% mutate(POA_CODE_2016= str_replace(POA_CODE_2016,"POA",""))

G37[2:ncol(G37)] <- lapply(G37[2:ncol(G37)], as.numeric)

internet <- names(G37)
#Persons <- c("Persons","Psns","Ps")
#Persons_match <-str_c(Persons, collapse = "|")
Internet_cols <- str_subset(internet, "Total")
Internet_cols<-c("IA_Total"          ,       "I_NA_Total"         ,      "IC_not_stated_Total","Total_Total")
G37 <- G37 %>% select(POA_CODE_2016, all_of(Internet_cols))
G37 <- G37 %>% rename(Total.Internet = Total_Total)

#women in workforce
G43B <- G43B %>% select(F_Tot_Tot, F_Tot_LF_Tot, POA_CODE_2016)

#dwelling
G32<- G32 %>% mutate(POA_CODE_2016= str_replace(POA_CODE_2016,"POA",""))

G32[2:ncol(G32)] <- lapply(G32[2:ncol(G32)], as.numeric)

G32.names <-names(G32)
Persons <- c("Persons","Psns","Ps")
Persons_match <-str_c(Persons, collapse = "|")
Persons_cols <- str_subset(G32.names, Persons_match)
G32 <- G32 %>% select(POA_CODE_2016, OPDs_Separate_house_Persons, OPDs_SD_r_t_h_th_Tot_Psns,OPDs_Flt_apart_Tot_Psns,
                      OPDs_Other_dwelling_Tot_Psns, Unoccupied_PDs_Psns, OPDs_Dwlling_structur_NS_Psns,Total_PDs_Persons)


#male income
G17A<- G17A %>% mutate(POA_CODE_2016= str_replace(POA_CODE_2016,"POA",""))

G17A[2:ncol(G17A)] <- lapply(G17A[2:ncol(G17A)], as.numeric)
names(G17A)
M.Brackets = str_subset(names(G17A), "Tot")
M.Brackets = str_subset(M.Brackets, "^F", negate = TRUE)
M.Brackets = str_subset(M.Brackets, "yrs", negate = TRUE)
G17_M <- G17A %>% select(POA_CODE_2016, all_of(M.Brackets))

G17_M <- G17_M %>% rename(M_Tot_Bracket = M_Tot_Tot) 

F2.Brackets = str_subset(names(G17A), "Tot")
F2.Brackets = str_subset(F2.Brackets, "^F")
F2.Brackets = str_subset(F2.Brackets, "yrs", negate = TRUE)
G17_F2 <- G17A %>% select(POA_CODE_2016, all_of(F2.Brackets))
#female income


#income
G17B<- G17B %>% mutate(POA_CODE_2016= str_replace(POA_CODE_2016,"POA",""))

G17B[2:ncol(G17B)] <- lapply(G17B[2:ncol(G17B)], as.numeric)
names(G17B)
L.Brackets = str_subset(names(G17B), "Tot")
L.Brackets = str_subset(L.Brackets, "^F", negate = TRUE)
L.Brackets

G17_P <- G17B %>% select(POA_CODE_2016, all_of(L.Brackets))

F.Brackets = str_subset(names(G17B), "Tot")
F.Brackets = str_subset(F.Brackets, "^F_")

G17_F <-G17B %>% select(POA_CODE_2016, all_of(F.Brackets)) 

#income
G17C<- G17C %>% mutate(POA_CODE_2016= str_replace(POA_CODE_2016,"POA",""))

G17C[2:ncol(G17C)] <- lapply(G17C[2:ncol(G17C)], as.numeric)
names(G17C)
Bracket = str_subset(names(G17C), "Tot")
Bracket = str_subset(Bracket, "yrs", negate = TRUE)
Bracket = str_subset(Bracket, "ov", negate = TRUE)
G17_P2 <- G17C %>% select(POA_CODE_2016, all_of(Bracket))
G17_P2 <- G17_P2 %>% rename(P_Tot_Bracket = P_Tot_Tot) 


#income 25 to 64
#income
G17B<- G17B %>% mutate(POA_CODE_2016= str_replace(POA_CODE_2016,"POA",""))

G17B[2:ncol(G17B)] <- lapply(G17B[2:ncol(G17B)], as.numeric)
names(G17B)
L.Brackets = str_subset(names(G17B), "Tot")
L.Brackets = str_subset(L.Brackets, "^F", negate = TRUE)
L.Brackets

#Persons
G17_P <- G17B %>% select(POA_CODE_2016, all_of(L.Brackets))

F.Brackets = str_subset(names(G17B), "Tot")
F.Brackets = str_subset(F.Brackets, "^F_")

#Female
G17_F <-G17B %>% select(POA_CODE_2016, all_of(F.Brackets)) 

#Age.bracket low
years <- c("25_34_yrs","35_44_yrs","45_54_yrs")
years <-str_c(years, collapse = "|")
Age.Bracket2 = str_subset(names(G17B), years)
Age.Bracket2 = str_subset(Age.Bracket2, "^P_")

Age.Bracket2

G17_Age2 <- G17B %>% select(POA_CODE_2016, all_of(Age.Bracket2))


#######################################G17C
#income 24 to 54
G17C<- G17C %>% mutate(POA_CODE_2016= str_replace(POA_CODE_2016,"POA",""))

G17C[2:ncol(G17C)] <- lapply(G17C[2:ncol(G17C)], as.numeric)
names(G17C)


Age.Bracket = str_subset(names(G17C), years)

Age.Bracket

G17_Age <- G17C %>% select(POA_CODE_2016, all_of(Age.Bracket))

#age int
vars <- str_subset(names(G03), "Total_")
vars
G03 <- G03 %>% select(POA_CODE_2016, all_of(vars))
G03 <- G03 %>% rename(Age.int.Total = Total_Total)
G03 <- G03 %>% mutate(Total_0_24= Total_0_14_yr+Total_15_24_yr,
                      Total_25_44 = Total_25_34_yr +Total_35_44_yr,
                      Total_45_64 = Total_45_54_yr + Total_55_64_yr,
                      Total_65_over = Total_65_74_yr+ Total_75_84_yr +Total_85ov)
#pt
names.G59<-names(G59)
pt<- names.G59[str_detect(names.G59, "_P")] 
transport <-c("TrainTrn","Tram","Trm","Bus","Bs")
transport_match <-str_c(transport, collapse = "|")
pt <- str_subset(pt, transport_match)
pt

#pt 
G59 <- G59 %>% select(POA_CODE_2016, all_of(pt), Tot_P)
G59 <- G59 %>% rename(Total_Transport = Tot_P)


##ind
names.G53A<-names(G53A)
ind <- str_subset(names.G53A, "Tot")
G53A <- G53A %>% select(POA_CODE_2016, all_of(ind))


#rent
G33 <- G33 %>% select(POA_CODE_2016, R_Tot_Total, O_OR_Total, O_MTG_Total,Oth_ten_type_Total,
                      Ten_type_NS_Total , Total_Total)

G33 <- G33 %>% mutate(Oth_Total = Oth_ten_type_Total + Ten_type_NS_Total)
#add in 
datm <- left_join(datm, G01, by = c("POA_CODE_2016"))

datm <- left_join(datm, G02, by = c("POA_CODE_2016"))

datm <- left_join(datm, G57B, by = c("POA_CODE_2016"))

datm <- left_join(datm, G33, by = c("POA_CODE_2016"))

datm <- left_join(datm, G53A, by = c("POA_CODE_2016"))

datm <- left_join(datm, G59, by = c("POA_CODE_2016"))

datm <- left_join(datm, G03, by = c("POA_CODE_2016"))

datm <- left_join(datm, G43B, by = c("POA_CODE_2016"))

datm <- left_join(datm, G32, by = c("POA_CODE_2016"))

datm <- left_join(datm, G37, by = c("POA_CODE_2016"))

datm <- left_join(datm, G17_P2, by = c("POA_CODE_2016"))

datm <- left_join(datm, G17_P, by = c("POA_CODE_2016"))

datm <- left_join(datm, G17_M, by = c("POA_CODE_2016"))

datm <- left_join(datm, G17_F, by = c("POA_CODE_2016"))

datm <- left_join(datm, G17_F2, by = c("POA_CODE_2016"))

datm <- left_join(datm, G17_Age, by = c("POA_CODE_2016"))

datm <- left_join(datm, G17_Age2, by = c("POA_CODE_2016"))

# does it make sense

names(datm)
datm <- datm %>% 
  mutate(Cases.per.100K = Cumulative.cases/Tot_P_P*100000)

datm$Cases.per.100K[is.na(datm$Cases.per.100K)]<-0

datm <- datm %>%
  mutate(Cases.per.100K = round(Cases.per.100K, 2))
datm <- datm %>% mutate(Suburb = str_replace(Suburb, "melbourne","Melbourne"))

blog <-datm
blog$Cases.per.100K<-ifelse(blog$Tot_P_P <1500, 0, blog$Cases.per.100K)
blog$Cases.per.100K <-ifelse(blog$Suburb %in% c("Plenty","Somerton","Ardeer, Deer Park East","University Of Melbourne"), 0, blog$Cases.per.100K)

saveRDS(blog, "Blog4.Case.Numbers.RDS")

excluded <- datm %>% 
  filter(Tot_P_P<1500) %>%
  select(Postcode, Suburb) %>%
  print()

#exclusion
datm$Include <-"Yes"
datm$Include[datm$Tot_P_P <=1500]<-"No"
datm$Include[datm$Suburb %in% c("Plenty",
                           "Somerton","Ardeer, Deer Park East","University Of Melbourne")]<-"No"

table(datm$Include)
table(datm$Include)
pop.poa <- datm %>% select(Postcode, Suburb, Tot_P_P, Cases.per.100K, Cumulative.cases)

g <- list(showlegend = FALSE,
          showframe = FALSE,
          showcoastlines = FALSE,
          projection = list(type = 'Mercator')
)

#add_sf() %>%
#  layout(geo = g)
datm %>%
  plot_geo(split = ~Postcode, showlegend = FALSE, hoverinfo = "text", color = ~Cases.per.100K, 
           colors = viridis_pal(option = "B")(30),
           text = ~paste("Area:", Postcode, 
                         "<br>","Suburb:", Suburb,
                         "<br>","Cumulative cases 31/07:", Cumulative.cases,
                         "<br>","Cases per 100K:", Cases.per.100K)) %>%
  add_sf() %>%
  layout(geo = g)

#Greys,YlGnBu,Greens,YlOrRd,Bluered,RdBu,Reds,Blues,Picnic,Rainbow,Portland,Jet,Hot,Blackbody,Earth,Electric,Viridis,Cividis
datm %>%
  plot_geo(split = ~Postcode, showlegend = FALSE, hoverinfo = "text",
           
           text = ~paste("Area:", Postcode, "<br>","Cases:", Cases.per.100K,
                         "<br>","Median.age:", Median_age_persons)) %>%
  
  add_markers(x= ~cent_long, y = ~cent_lat,size = ~Cases.per.100K, color = I("red"))%>% 
  add_sf() %>%
  layout(geo = g)

datm %>%
  mutate(Suburb = fct_reorder(Suburb, Cases.per.100K, .desc = TRUE)) %>%
  plot_ly(x = ~Suburb, y = ~Cases.per.100K) %>%
  add_bars()
#age analysis
age.table <-datm 

age2 <- c( "Total_0_14_yr" , "Total_15_24_yr", "Total_25_34_yr", "Total_35_44_yr", "Total_45_54_yr",
         "Total_55_64_yr", "Total_65_74_yr", "Total_75_84_yr" ,"Total_85ov" ) 
age2 = c("Total_0_24","Total_25_44","Total_45_64","Total_65_over")



age2.table <- st_set_geometry(age.table, NULL)

age2.table <- age2.table %>% mutate(Total_0_24= Total_0_14_yr+Total_15_24_yr,
                                   Total_25_44 = Total_25_34_yr +Total_35_44_yr,
                                   Total_45_64 = Total_45_54_yr + Total_55_64_yr,
                                   Total_65_over = Total_65_74_yr+ Total_75_84_yr +Total_85ov)

age2.table <- select(age2.table, Postcode, Cases.per.100K, all_of(age2), Age.int.Total)

age2.perc <- str_c(age2, "Perc", sep = "_")

age2.corr <- age2.table

names(age2.corr)



age2.corr <-age2.corr %>%
  mutate_at(.vars = age2,
            .funs = list(Perc = ~ (./Age.int.Total)))



write.csv(age2.corr, "Age.interval.article.csv")
age2.corr <-age2.corr %>% select(-Postcode)
age2.corr <- age2.corr %>% 
  select(Cases.per.100K, all_of(age2.perc))

agecorrtab <-age2.corr %>% correlate() %>% focus(Cases.per.100K) %>%
  arrange(desc(Cases.per.100K))

write.csv(agecorrtab, "Age.interval.correlation.csv")
res2 <- rcorr(as.matrix(age2.corr))
res2

#average age
age <- datm

age<- st_set_geometry(age, NULL)
age <- age %>% select(Postcode, Suburb, Cases.per.100K, Median_age_persons, Include)

names(age)
write.csv(age, "Blog.age.csv", row.names = FALSE)

#dwelling structure
dwelling <- datm %>% 
  select(Postcode, Suburb, Cases.per.100K, OPDs_Separate_house_Persons, OPDs_SD_r_t_h_th_Tot_Psns,OPDs_Flt_apart_Tot_Psns,
         OPDs_Other_dwelling_Tot_Psns, OPDs_Dwlling_structur_NS_Psns, Unoccupied_PDs_Psns, Total_PDs_Persons, Include)
names(G32)
head(dwelling)
dwelling <-   dwelling %>%  rename (House = OPDs_Separate_house_Persons, Semi.detached = OPDs_SD_r_t_h_th_Tot_Psns,
                                    Flat = OPDs_Flt_apart_Tot_Psns,
                                    Other = OPDs_Other_dwelling_Tot_Psns, Not.stated = OPDs_Dwlling_structur_NS_Psns, 
                                    Unoccupied = Unoccupied_PDs_Psns,
                                    Total.dwelling =Total_PDs_Persons)

dwelling.types <- c("House","Semi.detached","Flat","Other", "Not.stated","Unoccupied")

dwelling.perc <-str_c(dwelling.types, "(%)", sep = " ")
dwelling.table <- st_set_geometry(dwelling, NULL)

str(dwelling.table)
dwelling.table <-dwelling.table %>%
  mutate_at(.vars = dwelling.types,
            .funs = list(Percent = ~ (./Total.dwelling)*100))

dwelling.table <-dwelling.table %>% mutate_if(is.numeric, round, digits=3)

dwelling.table <- dwelling.table %>% rename(Occupied.Dwellings =   Total.dwelling)
write.csv(dwelling.table, file = "Blog.dwelling.csv", row.names = FALSE)
head(dwelling.table)


dwelling.table <- dwelling.table %>% select(Postcode, Suburb, Cases.per.100K, all_of(dwelling.perc), Total.dwelling) 

dwelling.corr <- dwelling.table %>% 
  select(Cases.per.100K, all_of(dwelling.perc)) 

dwelling.correlation <-dwelling.corr %>% 
  correlate() %>% 
  focus(Cases.per.100K) %>%
  arrange(Cases.per.100K)
dwelling.correlation

dwelling.corr<- na.omit(dwelling.corr)

res2 <- rcorr(as.matrix(dwelling.corr))
res2

str(res2)
cor<-as.data.frame(res2[[1]])
p<-as.data.frame(res2[3])
correlation.dwelling <-cbind(cor, p)


correlation.dwelling <- correlation.dwelling %>% select(Cases.per.100K, P.Cases.per.100K) %>%
  filter(Cases.per.100K!=1)

x<- data.frame(`Dwelling type` = rownames(correlation.dwelling))

correlation.dwelling <- cbind(x, correlation.dwelling)

correlation.dwelling <- correlation.dwelling %>%
  mutate(Cases.per.100K = round(Cases.per.100K, 3), P.Cases.per.100K = round(P.Cases.per.100K, 3)) %>% 
  rename(`Dwelling type`= Dwelling.type, Correlation = Cases.per.100K, `p value` =P.Cases.per.100K)



write.csv(correlation.dwelling, file = "Correlation.dwelling.POA.csv")
#Female labor force participation
datm <- datm %>% 
  rename(F_Tot_Tot = F_Tot_Tot.x)
datm <- datm %>% mutate(Female.participation = F_Tot_LF_Tot/F_Tot_Tot)

#Language variable
datm <- datm %>% mutate(Lang.NE = Lang_spoken_home_Oth_Lang_P/Tot_P_P)

max <- datm 

max <- st_set_geometry(max, NULL)

max <- max %>% mutate( Lang.NE = round(Lang.NE*100, digits =2))
max.language <- max %>%
  select(Postcode, Suburb, Cases.per.100K, Lang.NE, Tot_P_P, Lang_spoken_home_Oth_Lang_P, Include)


names(max.language)<-c("Postcode", "Suburb" , "Cases.per.100K",
                       "(%) Not English", "Total persons","Not English","Include")

write.csv(max.language, "Blog.language.csv", row.names = FALSE)

cor.test(max.language$Cases.per.100K, max.language$Lang.NE)


#Household
max.household <- max %>%
  select(Postcode, Suburb, Cases.per.100K, Average_household_size)

write.csv(max.household, "household.for.article.csv")

cor.test(max.household$Cases.per.100K, max.household$Average_household_size)



#
#public transport - set at max date
pt.table <-datm
pt.table <- pt.table %>% select(Postcode, Suburb, Cases.per.100K, Total_Transport, all_of(pt)) 

pt.table <- st_set_geometry(pt.table, NULL)

pt.table$Tot_commuters <- apply(pt.table[,c(4:ncol(pt.table))], 1, sum)
pt.table <- pt.table %>% mutate(Public.trans.prop = Tot_commuters/Total_Transport)

cor.test(pt.table$Public.trans.prop,pt.table$Cases.per.100K)

pt.table


#industry analysis
ind.table <-datm


ind.table <- ind.table %>% select(Postcode, Suburb, Cases.per.100K, all_of(ind)) 
str(ind.table)
ind.table <- st_set_geometry(ind.table, NULL)

ind.table$Tot_ind <- apply(ind.table[,c(3:22)], 1, sum)

add.total <- ind.table %>% select(Postcode, Tot_ind)
#add total Employed persons aged 15 and over
datm <- left_join(datm, add.total, by = "Postcode")


ind.perc <- str_c(ind, "Perc", sep = "_")



ind.corr <-ind.table %>%
  mutate_at(.vars = ind,
            .funs = list(Perc = ~ (./Tot_ind)))

names(ind.corr)
ind.corr <-ind.corr%>% select(-Postcode)

ind.corr <- ind.corr %>% 
  select(Cases.per.100K, all_of(ind.perc)) 

industry.correlation <-ind.corr %>% 
  correlate() %>% 
  focus(Cases.per.100K) %>%
  arrange(Cases.per.100K)

industry.correlation
write.csv(industry.correlation, file = "industry.correlation.csv")
res2 <- rcorr(as.matrix(ind.corr))
res2

#Internet
internet.table <- datm %>%
  #
  select(Postcode, Suburb, Cases.per.100K, IA_Total,I_NA_Total,IC_not_stated_Total,Total.Internet, Include)

internet <- c("IA_Total","I_NA_Total","IC_not_stated_Total")

internet.perc <- str_c(internet, "Perc", sep = "_")

internet.table <- st_set_geometry(internet.table, NULL)

internet.perc <-internet.table %>%
  mutate_at(.vars = internet,
            .funs = list(Perc = ~ (./Total.Internet*100)))

names(internet.perc)<-c("Postcode", "Suburb" ,"Cases.per.100K","Internet.available",
                        "Internet.not.available","Not.stated", "Occupied.dwellings","Include",
                        "(%) Internet.available",
                        "(%) Internet.not.available","(%) Not.stated")
                        

internet.perc<- internet.perc %>% mutate_if(is.numeric, round, digits=3)     
head(internet.perc)
write.csv(internet.perc, "Blog.internet.csv", row.names = FALSE)

internet.corr <- internet.perc %>% 
  select(Cases.per.100K, IA_Total_Perc, I_NA_Total_Perc, IC_not_stated_Total_Perc)

internet.corr %>% correlate() %>% focus(Cases.per.100K)

res2 <- rcorr(as.matrix(internet.corr))
res2

#rent analysis
home.table <-datm 


home <- c("O_OR_Total", "O_MTG_Total" , "Oth_Total","R_Tot_Total") 

home.table <- select(home.table, Postcode, Suburb, Cases.per.100K, all_of(home), Total_Total, Include)

home.perc <- str_c(home, "Perc", sep = "_")

home.corr <- st_set_geometry(home.table, NULL)

head(home.corr)



home.corr <-home.corr %>%
  mutate_at(.vars = home,
            .funs = list(Perc = ~ (./Total_Total)*100))

tenure.table <-home.corr %>% mutate_if(is.numeric, round, digits=3)

names(tenure.table)<-c("Postcode", "Suburb" ,"Cases.per.100K","Owner.occupier",
                        "Owner.mortgage","Other", "Rental","Occupied.dwellings","Include",
                        "(%) Owner.occupier",
                        "(%) Owner.mortgage","(%) Other", "(%) Rental")
write.csv(tenure.table, file = "Blog.tenure.csv", row.names = FALSE)



home.corr <- home.corr %>% 
  select(Cases.per.100K, all_of(home.perc)) 
home.correlate <- home.corr %>% 
  correlate() %>% 
  focus(Cases.per.100K) 

home.correlate

write.csv(home.correrlate, file = "home.correlation.csv")
#write.csv(home.corr, "correlation.tenure.table.article.csv")
res2 <- rcorr(as.matrix(home.corr))
res2

cor<-as.data.frame(res2[[1]])
p<-as.data.frame(res2[3])
correlation.tenure <-cbind(cor, p)


correlation.tenure <- correlation.tenure %>% select(Cases.per.100K, P.Cases.per.100K) %>%
  filter(Cases.per.100K!=1)

x<- data.frame(`Tenure type` = rownames(correlation.tenure))

correlation.tenure <- cbind(x, correlation.tenure)

correlation.tenure <- correlation.tenure %>%
  mutate(Cases.per.100K = round(Cases.per.100K, 3), P.Cases.per.100K = round(P.Cases.per.100K, 3)) %>% 
  rename(`tenure type`= Tenure.type, Correlation = Cases.per.100K, `p value` =P.Cases.per.100K)



write.csv(correlation.tenure, file = "Correlation.tenure.POA.csv")
#doubles

names(ind.table)
#Industry analysis
datm <- datm %>% mutate(Transport.prop =Trans_po_wh_Tot/Tot_ind, 
                        Healthcare.prop =Hlth_care_soc_Tot/Tot_ind,
                        Public.service.prop = Pub_adm_sfty_Tot/Tot_ind,
)

Transport <- st_set_geometry(datm, NULL)
Transport <- Transport %>% 
  select(Postcode, Suburb, Cases.per.100K, Transport.prop,Trans_po_wh_Tot,Tot_ind, Include)

Transport <- Transport %>% mutate(Transport.prop = round(Transport.prop*100, digits = 2))

names(Transport)<-c("Postcode"  ,      "Suburb"      ,    "Cases.per.100K" ,
                    "(%) Employed Transport"  ,"Employed Tranport","Employed 15 over","Include")

head(Transport)
write.csv(Transport, file = "Blog.Transport.csv", row.names = FALSE)

#Jobs analysis
datm <- datm %>% mutate(Drivers.prop =P_Tot_Mach_oper_drivers/P_Tot_Tot, 
                        Managers.prop =P_Tot_Managers/P_Tot_Tot,
                        Community.prop= P_Tot_CommunPersnlSvc_W/P_Tot_Tot)

#Home analysis
datm <- datm %>% mutate(Own.prop =O_OR_Total/Total_Total, 
                        Rent.prop = R_Tot_Total/Total_Total)
#correlation manager and driver
table(datm$Rent.Mortgage)

jobs.table <-datm  

jobs <- c("P_Tot_Managers"          
          ,"P_Tot_Professionals"  ,    "P_Tot_TechnicTrades_W"   , "P_Tot_CommunPersnlSvc_W"  ,"P_Tot_ClericalAdminis_W" ,
          "P_Tot_Sales_W"   ,         "P_Tot_Mach_oper_drivers" , "P_Tot_Labourers"     ,     "P_Tot_Occu_ID_NS")
jobs.table <- select(jobs.table, Postcode, Suburb, Cases.per.100K, all_of(jobs), P_Tot_Tot)

names(jobs.table)
jobs.perc <- str_c(jobs, "Perc", sep = "_")

jobs.table <- st_set_geometry(jobs.table, NULL)

jobs.table <-jobs.table %>%
  mutate_at(.vars = vars(jobs),
            .funs = list(Perc = ~ (./P_Tot_Tot)))

head(jobs.table)

jobs.corr <- jobs.table %>% select(-Postcode, -Suburb, -all_of(jobs), -P_Tot_Tot)



jobs.corr.table <-jobs.corr %>% 
  correlate() %>% 
  focus(Cases.per.100K) %>%
  arrange(desc(Cases.per.100K))


write.csv(jobs.corr.table, "jobs.corr.table.csv")

res2 <- rcorr(as.matrix(jobs.corr))
res2

head(jobs.table)
jobs.table <- as.data.frame(jobs.table)
jobs.table <- gather(jobs.table,"Job",'n', 4:12)
jobs.table <- jobs.table %>% mutate(Perc.work = n/P_Tot_Tot)
names(jobs.table)


ggplot(jobs.table, aes(Perc.work, Cases.per.100K))+geom_point()+facet_wrap(~Job, scales = "free")+geom_smooth()

#income

#Internet
income <- datm %>%
  #
  select(Postcode, Suburb, Cases.per.100K, Median_tot_prsnl_inc_weekly, Median_tot_hhd_inc_weekly, Median_tot_fam_inc_weekly)


income <- st_set_geometry(income, NULL)


head(income)
income.corr <- income %>% 
  select(-Postcode, -Suburb) 

res2 <- rcorr(as.matrix(income.corr))
res2

###income brackets
names(datm)
bracket.table <-datm %>% 
  st_set_geometry( NULL)

#create names vectors
brackets <-c("P_1000_1249_Tot", "P_1250_1499_Tot" ,"P_1500_1749_Tot" ,"P_1750_1999_Tot" ,"P_2000_2999_Tot",
             "P_3000_more_Tot", "P_PI_NS_ns_Tot")

#Bring in figures from G17B and G17C
brackets <-c(L.Brackets, brackets)

bracket.table <- bracket.table %>% 
  select(Postcode, Suburb, Cases.per.100K, all_of(brackets), P_Tot_Bracket, Include)

#bracket.table <- bracket.table %>%
#  mutate(L = P_1_149_Tot +P_150_299_Tot + P_300_399_Tot ,
#         ML = P_400_499_TotP_500_649_Tot+P_650_799_Tot + P_800_999_Tot,
#        MH = P_1000_1249_Tot + P_1250_1499_Tot+P_1500_1749_Tot, 
#        H = P_1750_1999_Tot + P_2000_2999_Tot + P_3000_more_Tot)

bracket.table <- bracket.table %>%
  mutate(L = P_1_149_Tot +P_150_299_Tot + P_300_399_Tot + P_400_499_Tot) %>%
  mutate(Percent.poverty = L/P_Tot_Bracket)

Income <- bracket.table %>% select(Postcode, Suburb, Cases.per.100K, L,  P_Tot_Bracket, Percent.poverty,Include)


Income <- Income %>% mutate(Percent.poverty = round(Percent.poverty*100, 2))

names(Income)<-c("Postcode","Suburb","Cases.per.100K","Personal income below 500", "Over 15 persons",
                 "(%) Personal income below 500","Include")
                 
head(Income)
write.csv(Income, "Blog.poverty.csv", row.names = FALSE)
cor.test(bracket.table$Cases.per.100K, bracket.table$Percent.poverty)
# new.intervals <- c("L","ML","MH","H")

names(bracket.table)
bracket.table <-bracket.table %>%
  mutate_at(.vars = vars(all_of(brackets)),
            .funs = list(Perc = ~ (./P_Tot_Bracket)))


#new.int.perc <- str_c(new.intervals, "Perc", sep = "_")
brackets.perc <- str_c(brackets, "Perc", sep = "_")
bracket.corr <- bracket.table %>% select(Cases.per.100K, brackets.perc)

head(bracket.corr)

bracket.corr.table <-bracket.corr %>% 
  correlate() %>% 
  focus(Cases.per.100K) 

bracket.corr.table

res2 <- rcorr(as.matrix(bracket.corr))
res2
Age.Bracket2
Age.Bracket
age.bracket <-c(Age.Bracket, Age.Bracket2)
#income age bracket
ageb <- datm %>%
  #
  select(Postcode, Suburb, Cases.per.100K, all_of(age.bracket))

ageb <- st_set_geometry(ageb, NULL)


ageb <- ageb %>% 
  mutate(Nil = P_Neg_Nil_income_25_34_yrs + P_Neg_Nil_income_35_44_yrs+ P_Neg_Nil_income_45_54_yrs,
         Not.stated = P_PI_NS_ns_25_34_yrs + P_PI_NS_ns_35_44_yrs + P_PI_NS_ns_45_54_yrs,
         B.1.149 =   P_1_149_25_34_yrs + P_1_149_35_44_yrs + P_1_149_45_54_yrs,
         B.150.299 = P_150_299_25_34_yrs + P_150_299_35_44_yrs + P_150_299_45_54_yrs,
         B300.399 = P_300_399_25_34_yrs + P_300_399_35_44_yrs +P_300_399_45_54_yrs,
         B400.499 = P_400_499_25_34_yrs + P_400_499_35_44_yrs + P_400_499_45_54_yrs,
         B500.649 = P_500_649_25_34_yrs + P_500_649_35_44_yrs + P_500_649_45_54_yrs,
         B.650.799 = P_650_799_25_34_yrs+ P_650_799_35_44_yrs + P_650_799_45_54_yrs,
         B800.999 = P_800_999_25_34_yrs +P_800_999_35_44_yrs + P_800_999_45_54_yrs,
         B.1000.1248 = P_1000_1249_25_34_yrs + P_1000_1249_35_44_yrs + P_1000_1249_45_54_yrs,
         B.1250.1499 = P_1250_1499_25_34_yrs+P_1250_1499_35_44_yrs+ P_1250_1499_45_54_yrs,
         B.1500.1749 = P_1500_1749_25_34_yrs + P_1500_1749_35_44_yrs + P_1500_1749_45_54_yrs,
         B.1750.1999 = P_1750_1999_25_34_yrs+ P_1750_1999_35_44_yrs + P_1750_1999_45_54_yrs,
         B.2000.2999 = P_2000_2999_25_34_yrs + P_2000_2999_35_44_yrs + P_2000_2999_45_54_yrs, 
         B.3000.ov = P_3000_more_25_34_yrs +P_3000_more_35_44_yrs +P_3000_more_45_54_yrs,
         Total.age.group = P_Tot_25_34_yrs+ P_Tot_35_44_yrs+P_Tot_45_54_yrs)

agec <- ageb %>% 
  mutate(Nil = P_Neg_Nil_income_35_44_yrs,
         Not.stated =  P_PI_NS_ns_35_44_yrs ,
         B.1.149 =  P_1_149_35_44_yrs ,
         B.150.299 =  P_150_299_35_44_yrs ,
         B300.399 =  P_300_399_35_44_yrs ,
         B400.499 =  P_400_499_35_44_yrs ,
         B500.649 =  P_500_649_35_44_yrs ,
         B.650.799 = P_650_799_35_44_yrs ,
         B800.999 = P_800_999_35_44_yrs ,
         B.1000.1248 =  P_1000_1249_35_44_yrs ,
         B.1250.1499 = P_1250_1499_35_44_yrs,
         B.1500.1749 = P_1500_1749_35_44_yrs ,
         B.1750.1999 =  P_1750_1999_35_44_yrs ,
         B.2000.2999 =  P_2000_2999_35_44_yrs , 
         B.3000.ov = P_3000_more_35_44_yrs ,
         Total.age.group =  P_Tot_35_44_yrs)

agey <- ageb %>% 
  mutate(Nil = P_Neg_Nil_income_25_34_yrs,
         Not.stated = P_PI_NS_ns_25_34_yrs,
         B.1.149 =   P_1_149_25_34_yrs ,
         B.150.299 = P_150_299_25_34_yrs  ,
         B300.399 = P_300_399_25_34_yrs,
         B400.499 = P_400_499_25_34_yrs  ,
         B500.649 = P_500_649_25_34_yrs  ,
         B.650.799 = P_650_799_25_34_yrs ,
         B800.999 = P_800_999_25_34_yrs ,
         B.1000.1248 = P_1000_1249_25_34_yrs ,
         B.1250.1499 = P_1250_1499_25_34_yrs,
         B.1500.1749 = P_1500_1749_25_34_yrs  ,
         B.1750.1999 = P_1750_1999_25_34_yrs ,
         B.2000.2999 = P_2000_2999_25_34_yrs  , 
         B.3000.ov = P_3000_more_25_34_yrs  ,
         Total.age.group = P_Tot_25_34_yrs)
#participation rate of 60% 


names(agey)

#$426.30 a week.
ranges <- c("Nil" ,   "B.1.149" ,     "B.150.299"  , "B300.399", "B400.499", "B500.649", "B.650.799", "B800.999", 
            "B.1000.1248", "B.1250.1499",
            "B.1500.1749", "B.1750.1999", "B.2000.2999", "B.3000.ov")

two.levels = agey %>% 
  mutate(Poverty.line = Nil+ B.1.149 +B.150.299 +B300.399 +B400.499,
         Not.poverty = B500.649+B.650.799+B800.999+ B.1000.1248+ B.1250.1499+B.1500.1749+B.1750.1999+B.2000.2999 + B.3000.ov) %>%
  mutate(Poverty.perc = Poverty.line/Total.age.group, Not.poverty.perc = Not.poverty/Total.age.group)

two.levels <- two.levels %>% select(Postcode, Suburb, Cases.per.100K,Poverty.perc, Not.poverty.perc) 
  
  
two.corr <-two.levels %>%
  select(Cases.per.100K, Poverty.perc, Not.poverty.perc, Not.poverty.perc)


two.corr <- na.omit(two.corr)
res2 <- rcorr(as.matrix(two.corr))
res2

cor.test(three.levels$Cases.per.100K, three.levels$Poverty.perc)

bracket.table2 <- ageb %>% 
  select(Postcode, Suburb, Cases.per.100K, all_of(ranges), Total.age.group)

bracket.table2 <-bracket.table2 %>%
  mutate_at(.vars = vars(all_of(ranges)),
            .funs = list(Perc = ~ (./Total.age.group)))


#new.int.perc <- str_c(new.intervals, "Perc", sep = "_")


#######
saveRDS(datm, "Blog.Melbourne.Postcodes.wrangled.RDS")

g <- list(showlegend = FALSE,
          showframe = FALSE,
          showcoastlines = FALSE,
          projection = list(type = 'Mercator')
)


datm %>%
  plot_geo(split = ~Postcode, showlegend = FALSE, hoverinfo = "text",
           text = ~paste("Area:", Suburb)) %>%
  add_markers(x= ~cent_long, y = ~cent_lat)%>% 
  add_sf() %>%
  layout(geo = g)
str(datm)
#Public servants plot
datm %>%
  plot_geo(split = ~Postcode, showlegend = FALSE, hoverinfo = "text",
           text = ~paste("Area:", Postcode, "<br>","Cases:", Cases.per.100K)) %>%
  add_markers(x= ~cent_long, y = ~cent_lat,size = ~Cases.per.100K, color = I("red"))%>% 
  add_sf(color = ~Rent.prop) %>%
  layout(geo = g)


table(datm$Median_tot_fam_inc_weekly)

datm %>%
  #
  plot_ly(y = ~Public.service.prop, x = ~Cases.per.100K, showlegend = FALSE, hoverinfo = "text",
          text = ~paste("Area:", lga_name_2018, "<br>","Cases:", Cumulative.cases)) %>%
  add_markers()

# https://plotly.com/r/filled-area-plots/

p<-datm %>%
  filter(Date >="2020-06-01") %>%
  ggplot(aes(x = Date, y = Cases.per.100K, type = LGA)) + 
  geom_line(aes(color = Public.service.prop))+
  scale_color_viridis(option = "D")+ 
  theme_bw() + 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        legend.position = "none")

pub.plot.ts <-ggplotly(p)

pub.plot.ts

#health plot
health.plot <- datm %>%
  #
  plot_geo(split = ~lga_name_2018, showlegend = FALSE, hoverinfo = "text",
           text = ~paste("Area:", lga_name_2018, "<br>","Cases:", Cases.per.100K)) %>%
  add_markers(x= ~cent_long, y = ~cent_lat,size = ~Cases.per.100K, color = I("red"))%>% 
  add_sf(color = ~Healthcare.prop,
         hoveron = "points+fills") %>%
  layout(geo = g)

health.plot

datm %>%
  
  #
  plot_ly(y = ~Healthcare.prop, x = ~Cumulative.cases, showlegend = FALSE, hoverinfo = "text",
          text = ~paste("Area:", lga_name_2018, "<br>","Cases:", Cumulative.cases)) %>%
  add_markers()

# https://plotly.com/r/filled-area-plots/

p<-datm %>%
  filter(Date >="2020-06-01") %>%
  ggplot(aes(x = Date, y = Cases.per.100K, type = LGA)) + 
  geom_line(aes(color = Healthcare.prop))+
  scale_color_viridis(option = "D")+ 
  theme_bw() + 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        legend.position = "none")

health.plot.ts <-ggplotly(p)

health.plot.ts

#healthcare
#rent plot
trans.plot <- datm %>%
  #
  plot_geo(split = ~lga_name_2018, showlegend = FALSE, hoverinfo = "text",
           text = ~paste("Area:", lga_name_2018, "<br>","Cases:", Cases.per.100K)) %>%
  add_markers(x= ~cent_long, y = ~cent_lat,size = ~Cases.per.100K, color = I("red"))%>% 
  add_sf(color = ~Transport.prop,
         hoveron = "points+fills") %>%
  layout(geo = g)

trans.plot

datm %>%
  
  #
  plot_ly(y = ~Transport.prop, x = ~Cumulative.cases, showlegend = FALSE, hoverinfo = "text",
          text = ~paste("Area:", lga_name_2018, "<br>","Cases:", Cumulative.cases)) %>%
  add_markers()

# https://plotly.com/r/filled-area-plots/

p<-datm %>%
  filter(Date >="2020-06-01") %>%
  ggplot(aes(x = Date, y = Cases.per.100K, type = LGA)) + 
  geom_line(aes(color = Transport.prop))+
  scale_color_viridis(option = "D")+ 
  theme_bw() + 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        legend.position = "none")

trans.plot.ts <-ggplotly(p)

trans.plot.ts

#rent plot
home.plot <-datm %>%
  #
  plot_geo(split = ~lga_name_2018, showlegend = FALSE, hoverinfo = "text",
           text = ~paste("Area:", lga_name_2018, "<br>","Cases:", Cases.per.100K)) %>%
  add_markers(x= ~cent_long, y = ~cent_lat,size = ~Cases.per.100K, color = I("red"))%>% 
  add_sf(color = ~Rent.Mortgage,
         hoveron = "points+fills") %>%
  layout(geo = g)

home.plot

datm %>%
  
  #
  plot_ly(y = ~Rent.Mortgage, x = ~Cumulative.cases, showlegend = FALSE, hoverinfo = "text",
          text = ~paste("Area:", lga_name_2018, "<br>","Cases:", Cumulative.cases)) %>%
  add_markers()

# https://plotly.com/r/filled-area-plots/

p<-datm %>%
  filter(Date >="2020-06-01") %>%
  ggplot(aes(x = Date, y = Cases.per.100K, type = LGA)) + 
  geom_line(aes(color = Rent.Mortgage))+
  scale_color_viridis(option = "D")+ 
  theme_bw() + 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        legend.position = "none")

table(datm$rent)
home.plot.ts <-ggplotly(p)

home.plot.ts

#drivers.plot
Managers.plot <-datm %>%
  #
  plot_geo(split = ~lga_name_2018, showlegend = FALSE, hoverinfo = "text",
           text = ~paste("Area:", lga_name_2018, "<br>","Cases:", Cases.per.100K, "<br>",
                         "Managers.prop:",Managers.prop)) %>%
  add_markers(x= ~cent_long, y = ~cent_lat,size = ~Cases.per.100K, color = I("red"))%>% 
  add_sf(color = ~Managers.prop,
         hoveron = "points+fills") %>%
  layout(geo = g)

Managers.plot

datm %>%
  
  #
  plot_ly(y = ~Managers.prop, x = ~Cumulative.cases, showlegend = FALSE, hoverinfo = "text",
          text = ~paste("Area:", lga_name_2018, "<br>","Cases:", Cumulative.cases)) %>%
  add_markers()

# https://plotly.com/r/filled-area-plots/

p <-datm %>%
  filter(Date >="2020-06-01") %>%
  ggplot(aes(x = Date, y = Cases.per.100K, type = LGA)) + 
  geom_line(aes(color = Drivers.prop))+
  scale_color_viridis(option = "D")+ 
  theme_bw() + 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black"), legend.position = "none")

managers.plot.ts <-ggplotly(p)

managers.plot.ts

#Community 
#drivers.plot
Community.plot <-datm %>%
  #
  plot_geo(split = ~lga_name_2018, showlegend = FALSE, hoverinfo = "text",
           text = ~paste("Area:", lga_name_2018, "<br>","Cases:", Cases.per.100K, "<br>",
                         "Community.prop:",Community.prop)) %>%
  add_markers(x= ~cent_long, y = ~cent_lat,size = ~Cases.per.100K, color = I("red"))%>% 
  add_sf(color = ~Community.prop,
         hoveron = "points+fills") %>%
  layout(geo = g)

Community.plot

datm %>%
  
  #
  plot_ly(y = ~Community.prop, x = ~Cumulative.cases, showlegend = FALSE, hoverinfo = "text",
          text = ~paste("Area:", lga_name_2018, "<br>","Cases:", Cumulative.cases)) %>%
  add_markers()

# https://plotly.com/r/filled-area-plots/

p <-datm %>%
  filter(Date >="2020-06-01") %>%
  ggplot(aes(x = Date, y = Cases.per.100K, type = LGA)) + 
  geom_line(aes(color = Community.prop))+
  scale_color_viridis(option = "D")+ 
  theme_bw() + 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black"), legend.position = "none")

Community.plot.ts <-ggplotly(p)

Community.plot.ts

library(RColorBrewer)


#Age size
datm %>%
  #
  plot_geo(split = ~Postcode, showlegend = FALSE, hoverinfo = "text", color = ~Rent.prop, 
           colors = viridis_pal(option = "D", direction = -1)(20),
           
           text = ~paste("Area:", Postcode, "<br>","Cases:", Cases.per.100K,
                         "<br>","Median.age:", Median_age_persons)) %>%
  
  add_markers(x= ~cent_long, y = ~cent_lat,size = ~Cases.per.100K, color = I("red"))%>% 
  add_sf( hoveron = "points+fills") %>%
  layout(geo = g)

datm %>%
  #
  plot_geo(split = ~Postcode, showlegend = FALSE, hoverinfo = "text", color = ~Rent.prop, 
           colors = viridis_pal(option = "D", direction = -1)(20),
           
           text = ~paste("Area:", Postcode, "<br>","Cases:", Cases.per.100K,
                         "<br>","Median.age:", Median_age_persons)) %>%
  
  add_markers(x= ~cent_long, y = ~cent_lat,size = ~Cases.per.100K, color = I("red"))%>% 
  add_sf( hoveron = "points+fills") %>%
  layout(geo = g)

datm$Median_tot_hhd_inc_weekly
#Greys,YlGnBu,Greens,YlOrRd,Bluered,RdBu,Reds,Blues,Picnic,Rainbow,Portland,Jet,Hot,Blackbody,Earth,Electric,Viridis,Cividis
# https://plotly.com/r/filled-area-plots/
# https://plotly.com/r/filled-area-plots/
age.plot

age.plot.sc <- datm %>%
  
  #
  plot_ly(y = ~Median_age_persons, x = ~Cases.per.1000, showlegend = FALSE, hoverinfo = "text",
          text = ~paste("Area:", lga_name_2018, "<br>","Cases:", Cases.per.1000)) %>%
  add_markers(color = ~Median_age_persons)

age.plot.sc

age.plot.ts <-datm %>%
  filter(Date >="2020-06-01") %>%
  ggplot(aes(x = Date, y = Cases.per.100K, type = LGA)) + 
  geom_line(aes(color = Median_age_persons))+
  scale_color_viridis(option = "D", direction = -1)+ 
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), legend.position = "none")
age.plot.ts <-ggplotly(age.plot.ts)
age.plot.ts

subplot(age.plot, age.plot.ts, nrows =2)

#Household size
datm %>%
  #
  plot_geo(split = ~lga_name_2018, showlegend = FALSE, hoverinfo = "text",
           text = ~paste("Area:", lga_name_2018, "<br>",
                         "Median.household:", Average_household_size, "<br>", "Cases:", Cases.per.100K)) %>%
  add_markers(x= ~cent_long, y = ~cent_lat,size = ~Cases.per.100K, color = I("red"))%>% 
  add_sf(color = ~Average_household_size,
         hoveron = "points+fills") %>%
  layout(geo = g)


household.plot

household.plot.sc <- datm %>%
  
  #
  plot_ly(y = ~Average_household_size, x = ~Cases.per.100K, showlegend = FALSE, hoverinfo = "text",
          text = ~paste("Area:", lga_name_2018, "<br>","Cases:", Cases.per.100K)) %>%
  add_markers(color = ~Average_household_size)

household.plot.sc

household.plot.ts <-datm %>%
  filter(Date >="2020-06-01") %>%
  ggplot(aes(x = Date, y = Cases.per.100K, type = LGA)) + 
  geom_line(aes(color = Average_household_size))+
  scale_color_viridis(option = "D")+ 
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), legend.position = "none")
household.plot.ts <-ggplotly(household.plot.ts)
household.plot.ts

subplot(income.plot, income.plot.ts, nrows =2)

#Language plots
LOTE.plot <- datm %>%
  #
  plot_geo(split = ~lga_name_2018, showlegend = FALSE, hoverinfo = "text",
           text = ~paste("Area:", lga_name_2018, "<br>","Cases:", Cases.per.100K)) %>%
  add_markers(x= ~cent_long, y = ~cent_lat,size = ~Cases.per.100K, color = I("red"))%>% 
  add_sf(color = ~Lang.NE,
         hoveron = "points+fills") %>%
  layout(geo = g)

# https://plotly.com/r/filled-area-plots/
LOTE.plot

LOTE.plot.sc <- datm %>%
  
  #
  plot_ly(y = ~Lang.NE, x = ~Cases.per.1000, showlegend = FALSE, hoverinfo = "text",
          text = ~paste("Area:", lga_name_2018, "<br>","Cases:", Cases.per.1000)) %>%
  add_markers(color = ~Lang.NE)

LOTE.plot.sc

LOTE.plot.ts <-datm %>%
  filter(Date >="2020-06-01") %>%
  ggplot(aes(x = Date, y = Cases.per.100K, type = LGA)) + 
  geom_line(aes(color = Lang.NE))+
  scale_color_viridis(option = "D")+ 
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), legend.position = "none")
LOTE.plot.ts <-ggplotly(LOTE.plot.ts)
LOTE.plot.ts

subplot(LOTE.plot, LOTE.plot.ts, nrows =2)





fig <- datm %>%
  plot_ly(x = ~Date, y = ~Cumulative.cases, color = ~LGA) %>%
  add_lines()  %>% 
  layout(showlegend = FALSE)
fig

#add difference variable

#rolling seven day difference variable
datm %>%
  plot_ly(x = ~Date, y = ~Change, color = ~LGA) %>%
  add_lines()  %>% 
  layout(showlegend = FALSE)
library(zoo)


datm %>%
  plot_ly(x = ~Date, y = ~Average.change, color = ~LGA) %>%
  add_lines()  %>% 
  layout(showlegend = FALSE)


fig5.plot <- datm %>%
  
  #
  
  plot_geo(split = ~lga_name_2018, showlegend = FALSE, color = I("gray90"), stroke = I("black"), span = I(1)) %>%
  add_markers(x= ~cent_long, y = ~cent_lat,size = ~Cases.per.1000, color = I("red"))%>% 
  add_sf(hoveron = "points") %>%
  layout(geo = g)

fig5.plot
