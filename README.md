# postcode-level-variables

The aim of this repository is to allow others to reproduce the analysis underten on my blot 'Deploy the Data' in the post [Postcode characteristics of Melbourne's COVID19 hotspots](https://www.deploythedata.com/2020/09/11/postcode-characteristics-of-melbournes-covid19-hotspots/)

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


