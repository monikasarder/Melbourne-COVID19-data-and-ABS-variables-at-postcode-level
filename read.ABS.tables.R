library(tidyverse)
library(readxl)
library(readr)


###Specify path to ABS tables
path ="2016_GCP_POA_for_Vic_short-header/2016 Census GCP Postal Areas for VIC"

#Get table names
names <- list.files(path, pattern = "*.csv", full.names = TRUE) %>%
  str_sub(start = 83, end = -13)


#Read ABS tables into names
for(i in names){
  locationfiles <- file.path(path,
                             paste("2016Census_",i,"_VIC_POA.csv",sep=""))
  assign(i, read.csv(locationfiles, stringsAsFactors=FALSE))
}