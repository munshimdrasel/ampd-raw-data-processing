
#Script 3: Merging facility attributes on monthly data


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

setwd ("/Users/munshirasel/Google Drive/R/ampd-3")

ampd_monthly<- read.fst ("data/ampd_monthly.fst")


facility_attributes <- fread ("data/facility_attributes.csv")

names(facility_attributes)
names (ampd_monthly)


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

facility_attributes <- facility_attributes %>% select (-c ("Fuel.Type..Primary.", "Fuel.Type..Secondary."))


#adding two dataframe

dfy <- merge(ampd_monthly,facility_attributes,by=c("ORISPL_CODE","UNITID"))


#here merging Fuel types information from ampd harva dataset 
#In facility attributes some of the facility has no information on Primary fuel types.
#for example facility 54 Orispl CODE and UNIT ID SCT1 & SCT2 has both "" and "Pipeline nat. gas" in facility attributes file
#Ampd harvard dataset covers most of this issue (not sure how did they do this, may be assumed "" values with the known values)

ampd_harvard <- fread ("data/AMPD_Unit_with_Sulfur_Content_and_Regulations_with_Facility_Attributes.csv")
ampd_harvard<- as.data.table (ampd_harvard)
ampd_harvard <- ampd_harvard [ , V1 := NULL] 
ampd_harvard<- unique( ampd_harvard, by = c ("Facility.ID..ORISPL.", "Unit.ID", "Year", "Month", "State.x"))
ampd_harvard2 <- ampd_harvard %>% select (c("Facility.ID..ORISPL.", "Unit.ID", "Fuel.Type..Primary..y",
                                            "Fuel.Type..Secondary..y", "State.x"))

colnames(ampd_harvard2)[1] <- "ORISPL_CODE"
colnames(ampd_harvard2)[2] <- "UNITID"
colnames(ampd_harvard2)[3] <- "Fuel.Type..Primary."
colnames(ampd_harvard2)[4] <- "Fuel.Type..Secondary."


dfy<- as.data.table(dfy)

dfy<- unique( dfy, by = c ("ORISPL_CODE", "UNITID", "year", "month", "STATE"))

ampd_harvard2<- unique( ampd_harvard2, by = c ("ORISPL_CODE", "UNITID", "State.x"))

dfy <- merge(dfy,ampd_harvard2,by=c("ORISPL_CODE","UNITID"))

dfy2 <- dfy %>% select (-c ("State", "NA.", "State.x"))



write.fst(dfy2, "data/ampd_monthly_all.fst")







