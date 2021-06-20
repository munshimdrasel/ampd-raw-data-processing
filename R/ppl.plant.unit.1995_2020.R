#Script 5: #This script generates ampd monthly emission data to be used in disperseR package


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


ampd_raw <- read.fst ("data/ampd_monthly_all.fst")

names(ampd_raw)

ampd_raw<-as.data.table(ampd_raw)

str(ampd_raw)



DTUS <- ampd_raw [ , .(
  Facility.ID..ORISPL.=ORISPL_CODE,
  Unit.ID=UNITID,
  Year=year,
  Month= month,
  State= STATE,
  County=County,
  FIPS=FIPS.Code,
  Facility.Latitude,
  Facility.Longitude,
  SO2..tons., 
  NOx..tons., 
  Avg..NOx.Rate..lb.MMBtu.=Avg.NOx.RATE, 
  CO2..short.tons.=CO2..tons.,
  Heat.Input..MMBtu.=HEAT.INPUT, 
  Gross.Load..MW.h., 
  Steam.Load..1000lb.,
  Operating.Time=SUM_OP_TIME,
  Source.Category,
  Fuel.Type..Primary.,
  Fuel.Type..Secondary.
  
)]

write.csv(DTUS, "data/AMPD_Unit.csv")

PP.units.monthly1997_2021 <- fread("data/AMPD_Unit.csv")

PP.units.monthly1997_2021 [, V1 := NULL]

PP.units.monthly1997_2021 <-PP.units.monthly1997_2021 %>% filter(Year >=1997)

save( PP.units.monthly1997_2021,
      file = 'data/PP.units.monthly1997_2021.rda')

