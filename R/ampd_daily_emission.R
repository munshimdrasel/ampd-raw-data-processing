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
setwd ("/Users/munshirasel/Google_Drive/R/ampd-raw-data-processing")


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



write.fst(dfy, "data/ampd_daily_emission.fst")


#===========================================

ampd_daily_emissions <- read.fst ("data/ampd_daily_emission.fst")
ampd_daily_emissions <- ampd_daily_emissions %>% filter (year>=2010 & year <=2020 & month >=1 & month <=4)

unique(ampd_daily_emissions$Source.Category)


facility.561 <-ampd_daily_emissions %>% filter (ORISPL_CODE==561)
unique(facility.561$month)

#checked ampd_harvard datase, facility 561 has emission data from month 4, 5, 6, 7, 8, 9. 
