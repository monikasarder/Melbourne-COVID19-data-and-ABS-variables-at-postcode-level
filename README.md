# postcode-level-variables

This repository is to enable users to reproduce the analysis underpinning the data journalism post [Postcode characteristics of Melbourne's COVID19 hotspots](https://www.deploythedata.com/2020/09/11/postcode-characteristics-of-melbournes-covid19-hotspots/)

The steps undertaken to accessing and visualising ABS Census characteristics at postcode level are generalisable to other projects.

## Data sources  

Four data sources were used:  

*  Will Mackeys package `absmaps`, which makes it straightforward to download, compress and convert ABS shapefile data to `sf` objects in R.  (Download and installation details are available from Will's GitHub at [`absmaps`](https://github.com/wfmackey/absmapsdata).  
  
*  August 6 August COVID19 data for Melbourne at Postcode level from The Age article[Victoria coronavirus data: Find the number of active COVID-19 cases in your postcode](https://www.theage.com.au/national/victoria/victoria-coronavirus-data-find-the-number-of-active-covid-19-cases-in-your-postcode-20200731-p55hg2.html), by Craig Butt and Mark Stehle. The data is available in a Google spreadsheet made available by the authors [here](https://docs.google.com/spreadsheets/d/1oxJt0BBPzk-w2Gn1ImO4zASBCdqeeLJRwHEA4DASBFQ/edit#gid=0)  

*  A user friendly Suburbs list from the Butt and Stehle article stored in *melbourne.postcode.list.xlsx*  

*  ABS Census postcode level data downloaded from the [ABS website](https://datapacks.censusdata.abs.gov.au/datapacks/). The following options were selected from the page: *2016 Census Datapacks>General Community> Profile>Postal Areas>Vic*   

## Read in COVID19 Data, match to suburb names and render choropleth  


The shape and COVID19 data for Melbourne was obtained first using the script **01 Melbourne shapes and COVID19 data.R**.

This will do several things noted in the script:    

*  Read in the COVID19 data for 6 August  

*  Read in the list of postcodes and suburb names from the Butt and Stehle article  
*  Read in the shapes from `absmaps` and filter only postcodes in Melbourne
*  Link the suburb names to the postcodes - as these are more reader friendly  
*  Link the case numbers to the postcode shapefiles  
*  Plot the raw cumulative case numbers

The plot is not meaningful story as the cumulative case numbers have not yet been rationalised for population, however it should appear as follows

![](Melbourne Plot.png)

The completed COVID19 dataset for Melbourne on 6 August 2020 is stored in **Melbourne_case_data.RDS**.

## Read in ABS data, join to COVID19 cases, identify variables of interest and normalise them

Census data was joined to the case data and analysed for correlation using the script **Extract ABS data**.

Prior to running these steps you will need to download the ABS datapack as described above, unzip the file and have the following in your working directory: /2016_GCP_POA_for_Vic_short-header/2016 Census GCP Postal Areas for VIC  

Once the file is unzipped I highly recommend reviewing the Metadata files at:
*/2016_GCP_POA_for_Vic_short-header/2016 Census GCP Postal Areas for VIC/Metadata/Metadata_2016_GCP_DataPack.xlsx* 

This lists all data that is collected and the unit at which they are collected. You will need this information to get a sense of what is important, and what steps you need to take to normalise postcode level data (eg is it collected at person level, dwelling level etc.)  

The script performs the following:  

* Map the data tables from the ABS datapack into a nested list named `pc` and named by the ABS table name from the metadat. This can now be explored.    
* Join the COVID case data file **Melbourne_case_data.RDS** with first ABS table in the dataset **G01** which gives the count for total persons in postcode on census night.  
* Normalise the data for Cases per 100K of population. Explore for outliers. It was found that for postcodes with less than 1500 residents, and for recently developed suburbs, counts were not meaningful. 
*  


The script will then:

*  

The geospatial characteristics of Melbourne's postcode and the number of cumulative cases at the peak of the second wave on 6 August 2020. The following characteristics showed positive and statistically significant relationships between COVID19 infection levels and the proportion of residents:


*  living below the poverty line  
*  living in rental properties 
*  living in a home without an internet connection 
*  speaking a language other than English in the home 
*  living in a flat or semi-detached home (cf a detached house) 
*  working in the transport industry; and
*  who are below the median age

### STEP 01 Obtain and visualise Melbourne COVID19 data at the peak 

The script for step one is **01 Melbourne shapes and COVID19 data.R**. It also requires that **melbourne.postcode.list.xlsx** is available in your working directory. Both are available in this repository.

Prior to running you will need to install Will Mackeys `absmaps`, which makes it straightforward to download, compress and convert ABS shapefile data to `sf` objects to be used in R.

Download and installation details are available from Will's GitHub found here: https://github.com/wfmackey/absmapsdata 

Running the script produces a dataset which contains case numbers on 6 August 2020 (daily and cumulative) for all Melbourne postcodes and suburb names.

The dataset is stored in **Melbourne_case_data.RDS** 

### STEP 02 Download and Prepare ABS data at the Postcode 


This project utilised census data at the postcode level downloaded from the [ABS website](https://datapacks.censusdata.abs.gov.au/datapacks/). Simply go to the DataPacks page at https://datapacks.censusdata.abs.gov.au/datapacks/  and select the following options:  

*  Census year: 2016 Census Datapacks  
*  DataPacks type: General Community Profile  
*  Select Geography: Postal Areas  
*  Geographies: Vic  

Download and unzip the folder **2016_GCP_POA_for_Vic_short-header** and place in your working director. This is also available from this repo.

### STEP 03 Prepare variables at postcode level and join to Melbourne cases dataset 
Each table from the data folder was then read into R using the script: **Read.ABS.tables.R**


