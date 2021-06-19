
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
library(ggpubr)
library(mapproj)

setwd ("/Users/munshirasel/Google Drive/R/ampd-3")



metrics <- read.fst("/Users/munshirasel/Google Drive/R/ampd-3/data/metrics.fst")
  

ampd_daily<- as.data.table(read.fst("/Users/munshirasel/Google Drive/R/ampd-3/data/ampd_daily_all_ec_units.fst"))

ampd_daily <- ampd_daily [, ID := paste(ORISPL_CODE, UNITID, sep = "-")]


facility.54 <- ampd_daily %>% filter (ORISPL_CODE==54)

unique(facility.54$Fuel.Type..Primary.)

unique(facility.54$ID)

#"54-SCT1" ,54-SCT2",  No Primary fuels

unique(facility.54 %>% filter (ID== "54-SCT1") %>% dplyr::select(Fuel.Type..Primary.))


facility_attributes <- fread ("data/facility_attributes.csv")


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

setnames(facility_attributes, variables)

colnames(facility_attributes)[3] <- "ORISPL_CODE"

colnames(facility_attributes)[4] <- "UNITID"

facility_attributes <- facility_attributes[, ID := paste(ORISPL_CODE, UNITID, sep = "-")]


facility_attributes<- unique( facility_attributes, by = c ("ID", "State"))


names(facility_attributes)

unique(facility_attributes$Fuel.Type..Primary.)

no.primary.fuel <- facility_attributes %>% filter (Fuel.Type..Primary.== "")

# write.csv(no.primary.fuel, "no.primary.fuel.csv")

fac.54.sct1 <- no.primary.fuel%>% filter (ORISPL_CODE==54 & UNITID == "SCT1")

# fac.54.sct1 <- facility_attributes%>% filter (ORISPL_CODE==117 & UNITID == "4")

unique(fac.54.sct1$Fuel.Type..Primary..x)


#something happened in my code which created ampd_daily_emission.fst file

# for some reason for some facilities primary fuel types were not added into the dataset.

#need to rerun those codes


#looking into Harvard datasets

harvard_ampd <- read.csv ("/Users/munshirasel/Google Drive/R/ampd-3/data/AMPD_Unit_with_Sulfur_Content_and_Regulations_with_Facility_Attributes.csv")


facility.54.harvard <- harvard_ampd %>% filter (Facility.ID..ORISPL.==54)

unique(facility.54.harvard$Unit.ID)

unique(facility.54.harvard$Fuel.Type..Primary..x)


#reply from ANN AMPD support on missing data
# 
# The Clean Air Markets Division (CAMD) of EPA is in charge of collecting data
# for programs associated with 40CFR Part 75. This includes regulatory programs
# such as the Acid Rain Program (ARP). The ARP is the earliest of the programs,
# starting in 1995. While there is some data in the database from years prior to
# that, the data is not complete and should not be expected to be.
# 
# In your spreadsheet that are 170 records from 1995 or later years. In looking
# at a few of those:
#   
#   * ORIS 117 unit 4 was affected by phase 2 of the ARP which started in 2000.
# The unit retired on 1/1/2000 so they never submitted data to CAMD and
# therefore do not show a primary fuel record.
# 
# * The 2 units at ORIS 54 were also affected by phase 2 of the ARP. They
# started submitting emissions data to CAMD in 2000 which is also when they
# submitted their primary fuel information. They were not required to submit
# any data prior that year.
# 
# * There are some units in the spreadsheet only affected by the SIPNOX or MATS
# programs. Those units have never reported emissions data to CAMD (they were
#                                                                   not required to) and so have never reported primary fuel information.
# 
# * The units at ORIS 60264 started reporting emissions data to CAMD in 2016,
# so they do not have fuel records on file for years prior to that.
# 
# Basically, if a unit is required to report emissions data to CAMD for a given
# year (not all the units in the database are) then they should have a primary
# fuel record. Otherwise, they may or may not have a primary fuel record on file.
# 
# Regards,
# Ann
# 



