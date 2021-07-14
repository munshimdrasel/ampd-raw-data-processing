rm(list = ls())

#in this script I'm coparing facility attributes data provided by Prof. Lucas Henneman and datasets from ampd API websits (downloaded August, 2020)
#old dataset has 87695 observations

#https://ampd.epa.gov//ampd/?bookmark=12266


#upto 2021 query

# https://ampd.epa.gov/ampd/#?bookmark=28623



#recent data checked (july, 2021). Its has 87692 observations


library(ncdf4)
library(data.table)
library(tidyverse)
library(parallel)
library(sf)
library(viridis)
library(ggplot2)
library(scales)
library(ggsn)
library(gridExtra)
library(ggmap)
library(ggrepel)
library(fst)
library(measurements)
library(XML)
library(plyr)
library(RCurl)
library(lubridate)
library(dplyr)
library(stringr)



#on local machine
setwd ("/Users/munshirasel/Google_Drive/R/ampd-3")


facility_attributes_LH <- fread ("data/facility_2016-2018_3.csv")


variables <- make.names(c(
  "State",
  "Facility Name",
  "Facility ID (ORISPL)",
  "Unit ID",
  "Associated Stacks",
  "Year",
  "Program(s)",
  "EPA Region",
  "NERC Region",
  "County",
  "County Code",
  "FIPS Code",
  "Source Category",
  "Facility Latitude",
  "Facility Longitude",
  "Owner",
  "Operator",
  "Representative (Primary)",
  "Representative (Secondary)",
  "SO2 Phase",
  "NOx Phase",
  "Unit Type",
  "Fuel Type (Primary)",
  "Fuel Type (Secondary)",
  "SO2 Control(s)",
  "NOx Control(s)",
  "PM Control(s)",
  "Hg Control(s)",
  "Commercial Operation Date" ,
  "Operating Status",
  "Max Hourly HI Rate (MMBtu/hr)"
))



setnames(facility_attributes_LH, variables)

colnames(facility_attributes_LH)[3] <- "ORISPL_CODE"

colnames(facility_attributes_LH)[4] <- "UNITID"

colnames(facility_attributes_LH)[1] <- "STATE"


#facility attributes data downloaded on August 16, 2020

facility_attributes_old <- fread ("data/facility_attributes.csv")


variables <- make.names(c(
  "State",
  "Facility Name",
  "Facility ID (ORISPL)",
  "Unit ID",
  "Year",
  "Owner",
  "Operator",
  "Representative (Primary)",
  "Representative (Secondary)",
  "Associated Stacks",
  "Program(s)",
  "EPA Region",
  "NERC Region",
  "County",
  "County Code",
  "FIPS Code",
  "Source Category",
  "Facility Latitude",
  "Facility Longitude",
  "SO2 Phase",
  "NOx Phase",
  "Unit Type",
  "Fuel Type (Primary)",
  "Fuel Type (Secondary)",
  "SO2 Control(s)",
  "NOx Control(s)",
  "PM Control(s)",
  "Hg Control(s)",
  "Commercial Operation Date" ,
  "Operating Status",
  "Max Hourly HI Rate (MMBtu/hr)",
  "NA"
))

setnames(facility_attributes_old, variables)

colnames(facility_attributes_old)[3] <- "ORISPL_CODE"

colnames(facility_attributes_old)[4] <- "UNITID"

colnames(facility_attributes_old)[1] <- "STATE"


#facility attributes data downloaded on July 13, 2021

facility_attributes_new <- fread ("data/facility_attributes_new_july_2021.csv")


variables <- make.names(c(
  "State",
  "Facility Name",
  "Facility ID (ORISPL)",
  "Unit ID",
  "Year",
  "Owner",
  "Operator",
  "Representative (Primary)",
  "Representative (Secondary)",
  "Associated Stacks",
  "Program(s)",
  "EPA Region",
  "NERC Region",
  "County",
  "County Code",
  "FIPS Code",
  "Source Category",
  "Facility Latitude",
  "Facility Longitude",
  "SO2 Phase",
  "NOx Phase",
  "Unit Type",
  "Fuel Type (Primary)",
  "Fuel Type (Secondary)",
  "SO2 Control(s)",
  "NOx Control(s)",
  "PM Control(s)",
  "Hg Control(s)",
  "Commercial Operation Date" ,
  "Operating Status",
  "Max Hourly HI Rate (MMBtu/hr)",
  "NA"
))

setnames(facility_attributes_new, variables)

colnames(facility_attributes_new)[3] <- "ORISPL_CODE"

colnames(facility_attributes_new)[4] <- "UNITID"

colnames(facility_attributes_new)[1] <- "STATE"




##old facility attributes datasets  doesn't have activity year from year 2016, 2017, 2018, 2019
sort(unique(facility_attributes_LH$Year))

sort(unique(facility_attributes_old$Year))

sort(unique(facility_attributes_new$Year))


#59 ORISPL code is not available in my facility attributes dataset
length(unique(facility_attributes_LH$ORISPL_CODE))
length(unique(facility_attributes_old$ORISPL_CODE))
length(unique(facility_attributes_new$ORISPL_CODE))


#6 units shortage
length(unique(facility_attributes_LH$UNITID))
length(unique(facility_attributes_old$UNITID))
length(unique(facility_attributes_new$UNITID))


#units ID testing
unique(facility_attributes_LH$UNITID [facility_attributes_LH$ORISPL_CODE==7790])

unique(facility_attributes_old$UNITID [facility_attributes_old$ORISPL_CODE==7790])

unique(facility_attributes_new$UNITID [facility_attributes_new$ORISPL_CODE==7790])


unique(facility_attributes_LH$UNITID [facility_attributes_LH$ORISPL_CODE==2832])

unique(facility_attributes_old$UNITID [facility_attributes_old$ORISPL_CODE==2832])

unique(facility_attributes_new$UNITID [facility_attributes_new$ORISPL_CODE==2832])


unique(facility_attributes_LH$UNITID [facility_attributes_LH$ORISPL_CODE==1004])

unique(facility_attributes_old$UNITID [facility_attributes_old$ORISPL_CODE==1004])

unique(facility_attributes_new$UNITID [facility_attributes_new$ORISPL_CODE==1004])
