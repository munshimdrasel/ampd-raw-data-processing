#Script 4: Merging facility attributes on daily ampd data

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

#causal impact
library(gsynth)
library(CausalImpact)

setwd ("/Users/munshirasel/Google Drive/R/ampd-3")



df <- read.fst ("data/emissions-raw.fst")


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


# write.fst(df_subset, "data/ampd_daily.fst")

facility_attributes <- fread ("data/facility_attributes.csv")

rm(df)
rm (df_subset)

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
facility_attributes <- facility_attributes %>% dplyr::select (-c ("Fuel.Type..Primary.", "Fuel.Type..Secondary."))
facility_attributes <- facility_attributes[, ID := paste(ORISPL_CODE, UNITID, sep = "-")]

facility_attributes<- unique( facility_attributes, by = c ("ID", "State"))


ampd_daily <- as.data.table(ampd_daily)

ampd_daily <- ampd_daily[, ID := paste(ORISPL_CODE, UNITID, sep = "-")]


dfy <- merge(ampd_daily,facility_attributes,by=c("ID"))



dfy<- as.data.table( dfy)

# Checking if we have repeatated values or not

dfz<- unique( dfy, by = c ("ID", "date", "STATE"))



rm(ampd_daily)

rm (dfy)

colnames(dfz)[27]

colnames(dfz)[27] <- "activity.year"

colnames(dfz)[4] <- "ORISPL_CODE"

colnames(dfz)[5] <- "UNITID"


dfz <- dfz %>% dplyr :: select ( ORISPL_CODE, UNITID, ID, STATE, date, year, month, day, SUM_OP_TIME, COUNT_OP_TIME,
                    Gross.Load..MW.h., Steam.Load..1000lb., SO2..tons.,  NOx..tons.,
                    CO2..tons., SO2.RATE, NOx.RATE, CO2.RATE, HEAT.INPUT, activity.year,
                    Associated.Stacks, Program.s., EPA.Region,  NERC.Region, County,
                    County.Code, FIPS.Code, Source.Category, Facility.Latitude, Facility.Longitude,
                    SO2.Phase, NOx.Phase, Unit.Type, 
                    SO2.Control.s., NOx.Control.s., PM.Control.s., Hg.Control.s., Commercial.Operation.Date,
                    Operating.Status )


#here merging Fuel types information from ampd harva dataset 
#In facility attributes some of the facility has no information on Primary fuel types.
#for example facility 54 Orispl CODE and UNIT ID SCT1 & SCT2 has both "" and "Pipeline nat. gas" in facility attributes file
#Ampd harvard dataset covers most of this issue (not sure how did they do this, may be assumed "" values with the known values)

ampd_harvard <- fread ("data/AMPD_Unit_with_Sulfur_Content_and_Regulations_with_Facility_Attributes.csv")
ampd_harvard<- as.data.table (ampd_harvard)
ampd_harvard <- ampd_harvard [ , V1 := NULL] 
ampd_harvard<- unique( ampd_harvard, by = c ("Facility.ID..ORISPL.", "Unit.ID", "Year", "Month", "State.x"))
ampd_harvard2 <- ampd_harvard %>% dplyr::select (c("Facility.ID..ORISPL.", "Unit.ID", "Fuel.Type..Primary..y",
                                            "Fuel.Type..Secondary..y", "State.x"))

colnames(ampd_harvard2)[1] <- "ORISPL_CODE"
colnames(ampd_harvard2)[2] <- "UNITID"
colnames(ampd_harvard2)[3] <- "Fuel.Type..Primary."
colnames(ampd_harvard2)[4] <- "Fuel.Type..Secondary."


dfz<- as.data.table(dfz)

dfz<- unique( dfz, by = c ("ORISPL_CODE", "UNITID", "year", "month", "STATE"))

ampd_harvard2<- unique( ampd_harvard2, by = c ("ORISPL_CODE", "UNITID", "State.x"))

dfz <- merge(dfz,ampd_harvard2,by=c("ORISPL_CODE","UNITID"))

dfz2 <- dfz %>% dplyr::select (-c ("State.x"))



write.fst(dfz2, "data/ampd_daily_emission.fst")







#===========================================

