#Script 4: Merging facility attributes on daily ampd data
rm(list = ls())

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


#on local machine
setwd ("/Users/munshirasel/Google_Drive/R/ampd-3")


#on hopper cluster
 # setwd ("/projects/HAQ_LAB/mrasel/R/ampd-raw-data-processing")




df <- as.data.table(read.fst ("data/emissions-raw.fst"))


df$Gross.Load..MW.h. = coalesce(df$GLOAD,df$GLOAD..MWh.)
df$SO2..tons. = coalesce(df$ SO2_MASS, df$ SO2_MASS..tons.)
df$Steam.Load..1000lb. = coalesce(df$SLOAD,df$SLOAD..1000.lbs.)

df$NOx..tons. = coalesce(df$NOX_MASS,df$NOX_MASS..tons.)
df$CO2..tons. = coalesce(df$CO2_MASS,df$CO2_MASS..tons.)

df$SO2.RATE = coalesce(df$SO2_RATE,df$SO2_RATE..lbs.mmBtu.)

df$NOx.RATE = coalesce(df$NOX_RATE,df$NOX_RATE..lbs.mmBtu.)

df$CO2.RATE = coalesce(df$CO2_RATE,df$CO2_RATE..tons.mmBtu.)

df$HEAT.INPUT = coalesce(df$HEAT_INPUT,df$HEAT_INPUT..mmBtu.)



head (df, 10)

names(df)

#separating dates in different columns
df<- df %>% mutate(date = mdy(OP_DATE)) %>% mutate_at(vars(date), funs(year, month, day))

names(df)

df_subset <- as.data.table (subset (df, select = c ("STATE",
                                                    "FACILITY_NAME",
                                                    "ORISPL_CODE",
                                                    "UNITID",
                                                    "date",
                                                    "year",
                                                    "month",
                                                    "day",
                                                    "SUM_OP_TIME",
                                                    "COUNT_OP_TIME",
                                                    "FAC_ID",
                                                    "UNIT_ID",
                                                    "Gross.Load..MW.h.",
                                                    "Steam.Load..1000lb.",
                                                    "SO2..tons.",
                                                    "NOx..tons.",
                                                    "CO2..tons.",
                                                    "SO2.RATE",
                                                    "NOx.RATE",
                                                    "CO2.RATE",
                                                    "HEAT.INPUT")))

str(df_subset)

names(df_subset)

ampd_daily<- unique( df_subset, by = c ("ORISPL_CODE", "UNITID", "date", "STATE"))


write.fst(df_subset, "data/ampd_daily.fst")

# https://ampd.epa.gov/ampd/#?bookmark=28623  (facility attributes)

facility_attributes <- fread ("data/facility_attributes_updated.csv")

rm(df)
rm (df_subset)


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
  "Max Hourly HI Rate (MMBtu/hr)",
  "NA"
))

setnames(facility_attributes, variables)

colnames(facility_attributes)[3] <- "ORISPL_CODE"

colnames(facility_attributes)[4] <- "UNITID"

colnames(facility_attributes)[1] <- "STATE"


sort(unique(facility_attributes$Year))

length(unique(ampd_daily$ORISPL_CODE))
length(unique(facility_attributes$ORISPL_CODE))

#my emission data is short of 58 facilities comparing to facility attributes files. missing facilities (example: ORISPL CODE: 10784, 913)

ampd_daily <- as_tibble(ampd_daily %>% distinct())
facility_attributes <- as_tibble((facility_attributes %>% distinct()))

#removing 0's from in front of UNITID in monthly emission datasets and facility attributes
ampd_daily$UNITID <-  str_replace(ampd_daily$UNITID, "^0+" ,"")
facility_attributes$UNITID <- str_replace(facility_attributes$UNITID, "^0+" ,"")

# 
# ampd_daily$UNITID [ampd_daily$ORISPL_CODE==7790] <- "1-Jan" #for 7790 facility, unit id in disperseR unit data is different from my dataset. my data set has "1-1
# 
# #ampd_harvard dataset 6193 facility has units that has 0 infront of them
# ampd_daily$UNITID [ampd_daily$ORISPL_CODE==6193 & ampd_daily$UNITID== "61B"] <- "061B"
# ampd_daily$UNITID [ampd_daily$ORISPL_CODE==6193 & ampd_daily$UNITID== "62B"] <- "062B"
# ampd_daily$UNITID [ampd_daily$ORISPL_CODE==6193 & ampd_daily$UNITID== "63B"] <- "063B"
# facility_attributes$UNITID [facility_attributes$ORISPL_CODE==6193 & facility_attributes$UNITID== "61B"] <- "061B"
# facility_attributes$UNITID [facility_attributes$ORISPL_CODE==6193 & facility_attributes$UNITID== "62B"] <- "062B"
# facility_attributes$UNITID [facility_attributes$ORISPL_CODE==6193 & facility_attributes$UNITID== "63B"] <- "063B"
# 
# #facility 1004, 2832 check (unit id has different name)
# 
# unique(facility_attributes$UNITID [facility_attributes$ORISPL_CODE==2832])
# unique(ampd_daily$UNITID [ampd_daily$ORISPL_CODE==2832])
# 
# ampd_daily$UNITID [ampd_daily$ORISPL_CODE==2832 & ampd_daily$UNITID== "5-1"] <- "1-May"
# ampd_daily$UNITID [ampd_daily$ORISPL_CODE==2832 & ampd_daily$UNITID== "5-2"] <- "2-May"
# 
# unique(ampd_daily$UNITID [ampd_daily$ORISPL_CODE==2832])
# 
# unique(facility_attributes$UNITID [facility_attributes$ORISPL_CODE==1004])
# unique(ampd_daily$UNITID [ampd_daily$ORISPL_CODE==1004])
# 
# ampd_daily$UNITID [ampd_daily$ORISPL_CODE==1004 & ampd_daily$UNITID== "6-1"] <- "1-Jun"
# ampd_daily$UNITID [ampd_daily$ORISPL_CODE==1004 & ampd_daily$UNITID== "7-1"] <- "1-Jul"
# ampd_daily$UNITID [ampd_daily$ORISPL_CODE==1004 & ampd_daily$UNITID== "7-2"] <- "2-Jul"
# ampd_daily$UNITID [ampd_daily$ORISPL_CODE==1004 & ampd_daily$UNITID== "8-1"] <- "1-Aug"
# 
# 
# 
# 
# 
# unique(ampd_daily$UNITID [ampd_daily$ORISPL_CODE==1004])
# # 6193-061B #need to change unit id of facility attributes and emission data to 061 to match with units data in disperseR
# 
# #assuming all facility attributes of 2019 are same for the year of 2020 & 2021 (couldn't find updated facility attributes from epa ampd website)
# facility_attributes_2019 <- facility_attributes %>% filter(Year==2019)
# 
# facility_attributes_2019$Year [facility_attributes_2019$Year==2019] <- 2020
# 
# facility_attributes_2020 <- facility_attributes_2019
# 
# facility_attributes_2019$Year [facility_attributes_2019$Year==2020] <- 2021
# 
# facility_attributes_2021 <- facility_attributes_2019
# 
# facility_attributes <- do.call("rbind", list(facility_attributes, facility_attributes_2020, facility_attributes_2021))
# 
# unique(facility_attributes$Year)

#Year is activity year. In that year may be facility changed fuel types/ or implemented new regulations etc.
ampd_daily$Year <- ampd_daily$year


#filtering data from 2010 to 2020
ampd_daily <- ampd_daily %>% filter (Year >=2010 & Year <=2020)

#merging
dfy <- merge(ampd_daily,facility_attributes, by=c("STATE", "ORISPL_CODE","UNITID", "Year" ), 
             all.x = T, allow.cartesian = T) 

#dfy observation must match ampd_monthly observations 


dfy<- as.data.table(dfy)

dfy[, ID := paste(ORISPL_CODE, UNITID, sep = "-")]


# 
# # facility_attributes <- facility_attributes %>% dplyr::select (-c ("Fuel.Type..Primary.", "Fuel.Type..Secondary."))
# facility_attributes <- facility_attributes[, ID := paste(ORISPL_CODE, UNITID, sep = "-")]
# ampd_daily <- ampd_daily[, ID := paste(ORISPL_CODE, UNITID, sep = "-")]
# 
# facility_attributes <- facility_attributes %>% 
#   dplyr::select (-c ("STATE", "ORISPL_CODE", "UNITID", "NA.",
#                      "Representative..Primary.","Representative..Secondary.",
#                      "Operator", "Owner" ))
# 
# 
# #vector memory gets exhausted (filtering data from 2000)
# 
# ampd_daily <- ampd_daily %>% filter(year>=2000)
# 
# dfy <- merge(ampd_daily,facility_attributes, by=c("ID"), all = TRUE,allow.cartesian=TRUE) 
# 
# dfy<- as.data.table( dfy)
# 
# # Checking if we have repeatated values or not
# 
# dfy<- unique( dfy, by = c ("ID", "date", "STATE"))
# 
# 
# 
# rm(ampd_daily)
# dfz <- dfy
# rm (dfy)
# 
# colnames(dfy)[24]
# 
# colnames(dfy)[24] <- "activity.year"
# 
# colnames(dfy)[4] <- "ORISPL_CODE"
# 
# colnames(dfy)[5] <- "UNITID"
# 
# 
# dfy <- dfy %>% dplyr :: select ( ORISPL_CODE, UNITID, ID, STATE, date, year, month, day, SUM_OP_TIME, COUNT_OP_TIME,
#                     Gross.Load..MW.h., Steam.Load..1000lb., SO2..tons.,  NOx..tons.,
#                     CO2..tons., SO2.RATE, NOx.RATE, CO2.RATE, HEAT.INPUT, activity.year,
#                     Associated.Stacks, Program.s., EPA.Region,  NERC.Region, County,
#                     County.Code, FIPS.Code, Source.Category, Facility.Latitude, Facility.Longitude,
#                     SO2.Phase, NOx.Phase, Unit.Type, 
#                     SO2.Control.s., NOx.Control.s., PM.Control.s., Hg.Control.s., Commercial.Operation.Date,
#                     Operating.Status )


#here merging Fuel types information from ampd harva dataset 
#In facility attributes some of the facility has no information on Primary fuel types.
#for example facility 54 Orispl CODE and UNIT ID SCT1 & SCT2 has both "" and "Pipeline nat. gas" in facility attributes file
#Ampd harvard dataset covers most of this issue (not sure how did they do this, may be assumed "" values with the known values)
# 
# ampd_harvard <- fread ("data/AMPD_Unit_with_Sulfur_Content_and_Regulations_with_Facility_Attributes.csv")
# ampd_harvard<- as.data.table (ampd_harvard)
# ampd_harvard <- ampd_harvard [ , V1 := NULL] 
# ampd_harvard<- unique( ampd_harvard, by = c ("Facility.ID..ORISPL.", "Unit.ID", "Year", "Month", "State.x"))
# ampd_harvard2 <- ampd_harvard %>% dplyr::select (c("Facility.ID..ORISPL.", "Unit.ID", "Fuel.Type..Primary..y",
#                                             "Fuel.Type..Secondary..y", "State.x"))
# 
# colnames(ampd_harvard2)[1] <- "ORISPL_CODE"
# colnames(ampd_harvard2)[2] <- "UNITID"
# colnames(ampd_harvard2)[3] <- "Fuel.Type..Primary."
# colnames(ampd_harvard2)[4] <- "Fuel.Type..Secondary."
# 
# 
# dfz<- as.data.table(dfz)
# 
# dfz<- unique( dfz, by = c ("ORISPL_CODE", "UNITID", "date", "STATE"))
# 
# ampd_harvard2<- unique( ampd_harvard2, by = c ("ORISPL_CODE", "UNITID", "State.x"))
# 
# dfz <- merge(dfz,ampd_harvard2,by=c("ORISPL_CODE","UNITID"))
# 
# dfz2 <- dfz %>% dplyr::select (-c ("State.x"))



write.fst(dfy, "data/ampd_daily_emission.fst")


#===========================================

ampd_daily_emissions <- read.fst ("data/ampd_daily_emission.fst")
ampd_daily_emissions <- ampd_daily_emissions %>% filter (year>=2010 & year <=2020 & month >=1 & month <=4)

unique(ampd_daily_emissions$Source.Category)


facility.561 <-ampd_daily_emissions %>% filter (ORISPL_CODE==561)
unique(facility.561$month)

#checked ampd_harvard datase, facility 561 has emission data from month 4, 5, 6, 7, 8, 9. 
