#Script 5: #This script generates ampd monthly emission data to be used in disperseR package

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
library(disperseR)

setwd ("/Users/munshirasel/Google_Drive/R/ampd-3")


ampd_raw <- read.fst ("data/ampd_monthly_all.fst")

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

# write.csv(DTUS, "data/AMPD_Unit.csv")

PP.units.monthly1997_2021 <- DTUS

# PP.units.monthly1997_2021 [, V1 := NULL]

PP.units.monthly1997_2021 <-as.data.table(PP.units.monthly1997_2021 %>% filter(Year >=1997))
PP.units.monthly1997_2021 <- PP.units.monthly1997_2021[, uID := paste(Facility.ID..ORISPL., Unit.ID, sep = ".")]

save( PP.units.monthly1997_2021,
      file = 'data/PP.units.monthly1997_2021.rda')


# ==================================================Data Validation-===============================

#disperseR power plants monthly dataset
PP.units.monthly1995_2017 <- disperseR::PP.units.monthly1995_2017 %>% filter(year>=1997 & year <=2017 )


#updated dataset (rasel)
load("data/PP.units.monthly1997_2021.rda")



PP.units.monthly1997_2021 <- PP.units.monthly1997_2021 %>% filter(Year>= 1997 & Year<=2017)


length(unique(PP.units.monthly1995_2017$uID)) #disperseR dataset has 5866 unit id

length(unique(PP.units.monthly1997_2021$uID)) #updated dataset has 5959 unit id.

#units data check yearly 

units.data <- disperseR::units %>% filter (year >=1997 & year <=2018)

length(unique(units.data$ID))

coal.fuels <- c ("Coal", "Coal, Pipeline Natural Gas", "Coal, Natural Gas", "Coal, Other Gas",
                 "Coal, Coal Refuse", "Coal Refuse", "Coal, Wood" )

ampd_coal <- PP.units.monthly1997_2021 %>% filter ( Fuel.Type..Primary. %in% coal.fuels & Year >=1997 & Year <= 2018)
ampd_coal[, ID := paste(Facility.ID..ORISPL., Unit.ID, sep = "-")]


length(unique(ampd_coal$ID)) 

x <- unique(units.data$ID)
y <- unique(ampd_coal$ID)

tail(x %in% y, 381)

x[1262]

#55245-1 is other gas in my dataset, in harvard dataset it is as coal.
ampd_harvard <- fread ("data/AMPD_Unit_with_Sulfur_Content_and_Regulations_with_Facility_Attributes.csv")
ampd_harvard<- as.data.table (ampd_harvard)
ampd_harvard <- ampd_harvard [ , V1 := NULL]
ampd_harvard<- unique( ampd_harvard, by = c ("Facility.ID..ORISPL.", "Unit.ID", "Year", "Month", "State.x"))
ampd_harvard <- ampd_harvard[, ID := paste(Facility.ID..ORISPL., Unit.ID, sep = "-")]
ampd_harvard_55245_1 <- ampd_harvard %>%  filter (Facility.ID..ORISPL.==55245 & Unit.ID=="1")
unique(ampd_harvard_55245_1$Fuel.Type..Primary..x)

PP.units.monthly1997_2021_55245 <- PP.units.monthly1997_2021 %>% filter (Facility.ID..ORISPL.==55245)

unique(PP.units.monthly1997_2021$Fuel.Type..Primary.[PP.units.monthly1997_2021$Facility.ID..ORISPL.==55245
                                                     &PP.units.monthly1997_2021$Unit.ID=="1"])




sum(PP.units.monthly1995_2017$NOx.tons, na.rm=T)

sum(PP.units.monthly1997_2021$NOx..tons., na.rm=T)


#monthly plot of emissions data from disperseR vs. updated data (for all fuel types)
monthly.disperseR_ym <- aggregate(list (NOx..tons.=PP.units.monthly1995_2017$NOx.tons, 
                                                  SO2..tons.=PP.units.monthly1995_2017$SO2.tons
                                           ), by=list( year=PP.units.monthly1995_2017$year,
            month=PP.units.monthly1995_2017$month), FUN=sum, na.rm=T)

monthly.disperseR_ym$group <- "emis_monthly_disperseR"

monthly.updated_ym <- aggregate(list (NOx..tons.=PP.units.monthly1997_2021$NOx..tons., 
                                      SO2..tons.=PP.units.monthly1997_2021$SO2..tons.
), by=list( year=PP.units.monthly1997_2021$Year,
            month=PP.units.monthly1997_2021$Month), FUN=sum, na.rm=T)

monthly.updated_ym$group <- "emis_monthly_updated"


emission_monthly_combined <- do.call("rbind", list(monthly.disperseR_ym, monthly.updated_ym))


years <- c (2017)
emission_monthly_combined%>% filter (year %in% years) %>% 
  ggplot(aes(month, NOx..tons., group= group, color= group)) + geom_line() + geom_point() +
  scale_x_continuous(breaks = seq(1, 12, by = 1)) + labs(x= "month",  y = "NOx (tons)", title = "2017") 

emission_monthly_combined%>% filter (year %in% years) %>% 
  ggplot(aes(month, SO2..tons., group= group, color= group)) + geom_line() + geom_point() +
  scale_x_continuous(breaks = seq(1, 12, by = 1)) + labs(x= "month",  y = "NOx (tons)", title = "2017") 


