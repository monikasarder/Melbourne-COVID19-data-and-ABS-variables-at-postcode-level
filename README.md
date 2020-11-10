# Melbourne COVID19 data and ABS variables at postcode level

This repository is to enable users to reproduce the analysis underpinning the data journalism post [Postcode characteristics of Melbourne's COVID19 hotspots](https://www.deploythedata.com/2020/09/11/postcode-characteristics-of-melbournes-covid19-hotspots/)

This repository outlines steps undertaken to accessing and visualise both COVID19 and ABS Census characteristics at postcode level. These methods are generalisable to other projects.

## Data sources  

Four data sources were used:  

*  Will Mackeys package `absmaps`, which makes it straightforward to download, compress and convert ABS shapefile data to `sf` objects in R.  (Download and installation details are available from Will's GitHub at [`absmaps`](https://github.com/wfmackey/absmapsdata).  
  
*  August 6 August COVID19 data for Melbourne at Postcode level from The Age article [*Victoria coronavirus data: Find the number of active COVID-19 cases in your postcode*](https://www.theage.com.au/national/victoria/victoria-coronavirus-data-find-the-number-of-active-covid-19-cases-in-your-postcode-20200731-p55hg2.html), by Craig Butt and Mark Stehle. The data is available in a Google spreadsheet made available by the authors [here](https://docs.google.com/spreadsheets/d/1oxJt0BBPzk-w2Gn1ImO4zASBCdqeeLJRwHEA4DASBFQ/edit#gid=0)  

*  A user friendly Suburbs list from the Butt and Stehle article stored in *melbourne.postcode.list.csv*  

*  ABS Census postcode level data downloaded from the ABS website. The following options were selected from the [ABS datapacks page](https://datapacks.censusdata.abs.gov.au/datapacks/): *2016 Census Datapacks>General Community> Profile>Postal Areas>Vic*   

## 01 Melbourne shapes and COVID19 data.R  

This script will enable you to read in COVID19 Date at postcode level, match postcodes to surburb names and visualise COVID19 on 6 August. 

The plot is not meaningful story as the cumulative case numbers have not yet been rationalised for population, however it should appear as follows

![](Melbourne_plot.jpg)

The completed COVID19 dataset for Melbourne on 6 August 2020 is stored in **Melbourne_case_data.RDS**.

## 02 Extract ABS data and correlate with COVID19 incidence.R

Prior to running this script you will need to download the ABS datapack as described above, unzip the file and have the following in your working directory: /2016_GCP_POA_for_Vic_short-header/2016 Census GCP Postal Areas for VIC  

Once the file is unzipped I highly recommend reviewing the Metadata files at:
*/2016_GCP_POA_for_Vic_short-header/2016 Census GCP Postal Areas for VIC/Metadata/Metadata_2016_GCP_DataPack.xlsx* .   

The metadata table lists all data tables, and the unit at which they are collected. You will need this information to get a sense of what is important, and what steps you need to take to normalise postcode level data (eg is the unit of analysis persons, persons over 15, employed persons over 15, households etc).  

The script performs the following steps:  

* Maps the data tables from the ABS datapack into a nested list named `pc` and names them based on the ABS table name from the metadata. 
* Joins the ABS figures to the COVID case data file **Melbourne_case_data.RDS**, created in step 1. 
* Joins the COVID19 data with ABS table *G01 Selected Person Characteristics by Sex* which gives population tally for each postcode on census night. The postcode population is used to derive Cumulative Cases per 100K of residents.
* Identifies outlier postcodes -  postcodes with less than 1500 residents, and for recently developed suburbs, counts were not meaningful. 
* Creates a choropleth to verify that things look as they should.  
* Links normalised COVID19 data and runs Pearson correlation against:   
  + G01: *Selected Person Characteristics by Sex* normalised by Persons;  
  + G17: *Total Personal Income (Weekly) by Age by Sex* normalised by Persons aged 15 and over; 
  + G33: *Tenure Type and Landlord Type by Dwelling Structure* normalised by occupied private dwellings;  
  + G32: *Dwelling Structure* normalised by persons in occupied private dwelling;  
  + G37: *Dwelling Internet Connection by Dwelling Structure* normalised by occupied private dwellings;  
  + G02: *Selected Medians and Averages* normalised by Persons;  
  + G53: *Industry of Employment by Occupation* normalised by employed persons aged 15 years and over; and  
  + G51: *Industry of Employment by Age by Sex* normalised by employed persons aged 15 years and over. 

COVID19 data and normalised variables along with spatial information are saved in **Melbourne.spatial.COVID19.RDS**.

## 03 Demographic.community.spread.Rmd  

This Rmarkdown script reproduces the data blog post at Deploy the data entitled [Postcode characteristics of Melbourne's COVID19 hotspots](https://www.deploythedata.com/2020/09/11/postcode-characteristics-of-melbournes-covid19-hotspots/).  All of the data needed for telling our story is now in **Melbourne.spatial.COVID19.RDS**. 

Please let me know if you have any questions or improvements at monikasarder@gmail.com . 
