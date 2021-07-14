#Script 3: Merging facility attributes with ampd monthly emission data 
#here I've used Facility attributes data provided by Prof. Lucas Henneman;


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
library(dplyr)
library(stringr)



#on local machine
setwd ("/Users/munshirasel/Google_Drive/R/ampd-3")


#on hopper cluster use this working directory:
#setwd ("/projects/HAQ_LAB/mrasel/R/ampd-raw-data-processing")


#see ampd-3.R file for more details on the dataset
ampd_monthly<- as.data.table(read.fst ("data/ampd_monthly.fst"))

# https://ampd.epa.gov/ampd/#?bookmark=28623  (facility attributes)

facility_attributes <- fread ("data/facility_attributes_updated.csv")


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


length(unique(ampd_monthly$ORISPL_CODE))
length(unique(facility_attributes$ORISPL_CODE))

#my emission data is short of 58 facilities comparing to facility attributes files. missing facilities (example: ORISPL CODE: 10784, 913)

sort(unique(facility_attributes$Year))


#checking if data has duplicated rows
ampd_monthly <- as_tibble(ampd_monthly %>% distinct())
facility_attributes <- as_tibble((facility_attributes %>% distinct()))

#removing 0's from in front of UNITID in monthly emission datasets and facility attributes to match same UNITID in both datasets
ampd_monthly$UNITID <-  str_replace(ampd_monthly$UNITID, "^0+" ,"")
facility_attributes$UNITID <- str_replace(facility_attributes$UNITID, "^0+" ,"")

# ampd_monthly$UNITID [ampd_monthly$ORISPL_CODE==7790] <- "1-Jan" #for 7790 facility, unit id in disperseR unit data is different from my dataset. my data set has "1-1

#ampd_harvard dataset 6193 facility has units that has 0 infront of them
# ampd_monthly$UNITID [ampd_monthly$ORISPL_CODE==6193 & ampd_monthly$UNITID== "61B"] <- "061B"
# ampd_monthly$UNITID [ampd_monthly$ORISPL_CODE==6193 & ampd_monthly$UNITID== "62B"] <- "062B"
# ampd_monthly$UNITID [ampd_monthly$ORISPL_CODE==6193 & ampd_monthly$UNITID== "63B"] <- "063B"
# facility_attributes$UNITID [facility_attributes$ORISPL_CODE==6193 & facility_attributes$UNITID== "61B"] <- "061B"
# facility_attributes$UNITID [facility_attributes$ORISPL_CODE==6193 & facility_attributes$UNITID== "62B"] <- "062B"
# facility_attributes$UNITID [facility_attributes$ORISPL_CODE==6193 & facility_attributes$UNITID== "63B"] <- "063B"

#facility 1004, 2832 check (unit id has different name)


# unique(facility_attributes$UNITID [facility_attributes$ORISPL_CODE==1004])
# unique(ampd_monthly$UNITID [ampd_monthly$ORISPL_CODE==2832])

# ampd_monthly$UNITID [ampd_monthly$ORISPL_CODE==2832 & ampd_monthly$UNITID== "5-1"] <- "1-May"
# ampd_monthly$UNITID [ampd_monthly$ORISPL_CODE==2832 & ampd_monthly$UNITID== "5-2"] <- "2-May"
# 
# unique(ampd_monthly$UNITID [ampd_monthly$ORISPL_CODE==2832])
# 
# unique(facility_attributes$UNITID [facility_attributes$ORISPL_CODE==1004])
# unique(ampd_monthly$UNITID [ampd_monthly$ORISPL_CODE==1004])
# 
# ampd_monthly$UNITID [ampd_monthly$ORISPL_CODE==1004 & ampd_monthly$UNITID== "6-1"] <- "1-Jun"
# ampd_monthly$UNITID [ampd_monthly$ORISPL_CODE==1004 & ampd_monthly$UNITID== "7-1"] <- "1-Jul"
# ampd_monthly$UNITID [ampd_monthly$ORISPL_CODE==1004 & ampd_monthly$UNITID== "7-2"] <- "2-Jul"
# ampd_monthly$UNITID [ampd_monthly$ORISPL_CODE==1004 & ampd_monthly$UNITID== "8-1"] <- "1-Aug"





# unique(ampd_monthly$UNITID [ampd_monthly$ORISPL_CODE==1004])
# 6193-061B #need to change unit id of facility attributes and emission data to 061 to match with units data in disperseR

#assuming all facility attributes of 2019 are same for the year of 2020 & 2021 (couldn't find updated facility attributes from epa ampd website)
# facility_attributes_2019 <- facility_attributes %>% filter(Year==2019)
# 
# facility_attributes_2019$Year [facility_attributes_2019$Year==2019] <- 2020
# 
# facility_attributes_2020 <- facility_attributes_2019
# 
# facility_attributes_2019$Year [facility_attributes_2019$Year==2020] <- 2021
# 
# facility_attributes_2021 <- facility_attributes_2019

# facility_attributes <- do.call("rbind", list(facility_attributes, facility_attributes_2020, facility_attributes_2021))
# 
# unique(facility_attributes$Year)

#Year is activity year. In that year may be facility changed fuel types/ or implemented new regulations etc.
ampd_monthly$Year <- ampd_monthly$year

#merging
dfy <- merge(ampd_monthly,facility_attributes, by=c("STATE", "ORISPL_CODE","UNITID", "Year" ), 
             all.x = T, allow.cartesian = T) 

#dfy observation must match ampd_monthly observations 

dfy<- unique( as.data.table(dfy), by = c ("ORISPL_CODE", "UNITID", "year", "month", "STATE" )) 

dfy <- dfy %>% dplyr::select(-"NA.")

dfy<- as.data.table(dfy)

dfy[, ID := paste(ORISPL_CODE, UNITID, sep = "-")]

dfy[, Fuel1.IsCoal := as.numeric(grepl("Coal", Fuel.Type..Primary.))]
dfy[Fuel.Type..Primary. == "", Fuel1.IsCoal := NA]

dfy[, Fuel2.IsCoal := as.numeric(grepl("Coal", Fuel.Type..Secondary.))]
dfy[Fuel.Type..Primary. == "", Fuel2.IsCoal := NA]

###trying Has.SO2.scrubber variable 




write.fst(dfy, "data/ampd_monthly_all.fst")

# =============================================================================================

# Data validation with AMPD Harvard datasets after merging Facility attributes information

# =============================================================================================


#Data-validation: PART1

ampd_raw <- read.fst ("data/ampd_monthly_all.fst")

ampd_harvard <- fread ("data/AMPD_Unit_with_Sulfur_Content_and_Regulations_with_Facility_Attributes.csv")

names(ampd_harvard)
ampd_harvard<- as.data.table (ampd_harvard)
ampd_harvard <- ampd_harvard [ , V1 := NULL] 

# geting unique values
ampd_harvard<- unique( ampd_harvard, by = c ("Facility.ID..ORISPL.", "Unit.ID", "Year", "Month", "State.x"))

#sorting AMPD data by state, year and month

ampd_sym<-            aggregate(list (NOx..tons.=ampd_raw$NOx..tons., 
                                      SO2..tons.=ampd_raw$SO2..tons.,
                                      CO2..tons.= ampd_raw$CO2..tons.,
                                      Gross.Load..MW.h.=ampd_raw$Gross.Load..MW.h.,
                                      Steam.Load..1000lb.= ampd_raw$Steam.Load..1000lb.,
                                      HEAT.INPUT= ampd_raw$HEAT.INPUT
), by=list(STATE=ampd_raw $ STATE, year=ampd_raw$year,
           month=ampd_raw $month), FUN=sum)


#sum of all states

ampd_ym<- aggregate(list (NOx..tons.=ampd_raw$NOx..tons., 
                          SO2..tons.=ampd_raw$SO2..tons.,
                          CO2..tons.= ampd_raw$CO2..tons.,
                          Gross.Load..MW.h.=ampd_raw$Gross.Load..MW.h.,
                          Steam.Load..1000lb.= ampd_raw$Steam.Load..1000lb.,
                          HEAT.INPUT= ampd_raw$HEAT.INPUT
), by=list( year=ampd_raw$year,
            month=ampd_raw $month), FUN=sum)




ampd_harvard_sym<- aggregate(list (NOx..tons.=ampd_harvard$NOx..tons., 
                                   SO2..tons.=ampd_harvard$SO2..tons.,
                                   CO2..short.tons.= ampd_harvard$CO2..short.tons.,
                                   Gross.Load..MW.h.=ampd_harvard$Gross.Load..MW.h.,
                                   Steam.Load..1000lb.= ampd_harvard$Steam.Load..1000lb.,
                                   Heat.Input..MMBtu.= ampd_harvard$Heat.Input..MMBtu.
), by=list( State= ampd_harvard$ State.x, Year=ampd_harvard$Year,
            Month=ampd_harvard $Month), FUN=sum, na.rm=TRUE)


colnames(ampd_harvard_sym)[1] <- "STATE"

colnames(ampd_harvard_sym)[2] <- "year"

colnames(ampd_harvard_sym)[3] <- "month"

colnames(ampd_harvard_sym)[4] <- "NOx..tons.harvard"

colnames(ampd_harvard_sym)[5] <- "SO2..tons.harvard"

colnames(ampd_harvard_sym)[6] <- "CO2..tons.harvard"

colnames(ampd_harvard_sym)[7] <- "Gross.Load..MW.h.harvard"

colnames(ampd_harvard_sym)[8] <- "Steam.Load..1000lb.harvard"

colnames(ampd_harvard_sym)[9] <- "HEAT.INPUT"



ampd_harvard_ym<- aggregate(list (NOx..tons.=ampd_harvard$NOx..tons.,
                                  SO2..tons.=ampd_harvard$SO2..tons.,
                                  CO2..short.tons.= ampd_harvard$CO2..short.tons.,
                                  Gross.Load..MW.h.=ampd_harvard$Gross.Load..MW.h.,
                                  Steam.Load..1000lb.= ampd_harvard$Steam.Load..1000lb.,
                                  Heat.Input..MMBtu.= ampd_harvard$Heat.Input..MMBtu.
), by=list(  Year=ampd_harvard$Year,
             Month=ampd_harvard $Month), FUN=sum, na.rm=TRUE)


names(ampd_harvard_ym)



colnames(ampd_harvard_ym)[1] <- "year"

colnames(ampd_harvard_ym)[2] <- "month"

colnames(ampd_harvard_ym)[3] <- "NOx..tons.harvard"

colnames(ampd_harvard_ym)[4] <- "SO2..tons.harvard"

colnames(ampd_harvard_ym)[5] <- "CO2..tons.harvard"

colnames(ampd_harvard_ym)[6] <- "Gross.Load..MW.h.harvard"

colnames(ampd_harvard_ym)[7] <- "Steam.Load..1000lb.harvard"

colnames(ampd_harvard_ym)[8] <- "HEAT.INPUT.harvard"

combined<-left_join(ampd_sym, ampd_harvard_sym, by = c("STATE", "year", "month")) 
combined_ym <-left_join(ampd_ym, ampd_harvard_ym, by = c("year", "month")) 

combined_sym <- combined %>% mutate(year.harvard=year, month.harvard=month) 


#combining two plot for data validation

#yearly NOx emission in the US

unique(ampd_raw$STATE)


years <- c (2010 : 2014)

x <- combined_sym %>% filter ( year %in% years, STATE =="ND")


#NOx graph comparison state wise (my data vs harvard data)

x %>% ggplot() + geom_line(aes( x= month, y = NOx..tons.), color = "red") +
  geom_line(aes( x= month, y = NOx..tons.harvard), color = "steelblue") + facet_grid(year~ .) +
  scale_x_continuous(breaks = seq(1, 12, by = 1)) + labs(x= "month", 
                                                         y = "NOx Emission (tons)",
                                                         title = "virginia , red-my data, blue-harvard data")
# ggsave("plot/va2010-2014.png")


#SO2 graph comparison state wise (my data vs harvard data)
x %>% ggplot() + geom_line(aes( x= month, y = SO2..tons.), color = "red") +
  geom_line(aes( x= month, y = SO2..tons.harvard), color = "steelblue") + facet_grid(year~ .) +
  scale_x_continuous(breaks = seq(1, 12, by = 1)) + labs(x= "month", 
                                                         y = "NOx Emission (tons)",
                                                         title = "red-my data, blue-harvard data")

#yearly NOx emission in the US

x <- ampd_ym %>% filter ( year <= 2015)
y <- ampd_harvard_ym  %>% filter (year <= 2015)

plot1 <- ggplot() + geom_boxplot(data= x, aes( year, NOx..tons.,color= year, group= year)) + 
  geom_boxplot(data= y, aes( year, NOx..tons.harvard,color= year, group= year)) +
  scale_x_continuous(breaks = seq(1995, 2021, by = 2))  

#yearly SO2 emission in the US


plot2 <- ggplot() + geom_boxplot(data= x, aes( year, SO2..tons.,color= year, group= year)) + 
  geom_boxplot(data= y, aes( year, SO2..tons.harvard,color= year, group= year)) +
  scale_x_continuous(breaks = seq(1995, 2021, by = 2))  

#yearly CO2 emission in the US

plot3 <- ggplot() + geom_boxplot(data= x, aes( year, CO2..tons.,color= year, group= year)) + 
  geom_boxplot(data= y, aes( year, CO2..tons.harvard,color= year, group= year)) +
  scale_x_continuous(breaks = seq(1995, 2021, by = 2)) 

#combining Nox SO2 and CO2 in one graph
library(ggpubr)
ggarrange(plot1, plot2, plot3, labels= c("A", "B", "C"), ncol=1, nrow=3)


# Few notes:

# 1. 1995, 1977 We shouldn't consider because the AMPD website doesn't have the data from all States
#2. Harvard data of 2015 is not right since it doesn't has data from June to December of 2015(see the plot below)

years<- c ( 2015)


plot1<- ampd_ym %>% filter ( year %in% years) %>% ggplot(aes(month, NOx..tons., color= year, group= year)) +
  geom_path() + geom_line() + scale_x_continuous(breaks = seq(1, 12, by = 1)) + 
  labs(x= "month",  y = "NOx Emission (tons)", title = "NOx Emission_updated data") +
  ylim (0, 300000)

plot2 <- ampd_harvard_ym  %>% filter ( year %in% years)%>% ggplot(aes(month, NOx..tons.harvard, color= year, group= year)) +
  geom_path() + geom_line() + scale_x_continuous(breaks = seq(1, 12, by = 1)) + 
  labs(x= "month",  y = "NOx Emission (tons)", title = "NOx Emission_Harvard data") +
  ylim (0, 300000)

ggarrange (plot1, plot2, labels= c("A", "B"), ncol=1, nrow=2)


# ==================================
# data validation: PART 2 : validating with respect to facility attributes
# =================================
# 
# ampd_raw <- as.data.table(read.fst ("data/ampd_monthly_all.fst"))
# ampd_raw[, ID := paste(ORISPL_CODE, UNITID, sep = "-")]
# 
# coal.fuels <- c ("Coal", "Coal, Pipeline Natural Gas", "Coal, Natural Gas", "Coal, Other Gas",
#                  "Coal, Coal Refuse", "Coal Refuse", "Coal, Wood" )
# 
# #Harvard AMPD dataset
# 
# # number of ORISPL Code matches with ampd_harvard dataset
# ampd_harvard <- fread ("data/AMPD_Unit_with_Sulfur_Content_and_Regulations_with_Facility_Attributes.csv")
# ampd_harvard<- as.data.table (ampd_harvard)
# ampd_harvard <- ampd_harvard [ , V1 := NULL]
# ampd_harvard<- unique( ampd_harvard, by = c ("Facility.ID..ORISPL.", "Unit.ID", "Year", "Month", "State.x"))
# ampd_harvard <- ampd_harvard[, ID := paste(Facility.ID..ORISPL., Unit.ID, sep = "-")]
# 
# #ampd_coal ORISPL code matches with ampd_harvard_coal ORISPL CODE numbers (check)
# ampd_harvard_coal <- ampd_harvard %>% filter (Year>= 1997 & Year <=2014 & Fuel1.IsCoal==1)
# length(unique(ampd_harvard_coal$Facility.ID..ORISPL.) )
# ampd_coal <- ampd_raw %>%  filter (year >=1997 & year <=2014 & Fuel.Type..Primary. %in% coal.fuels )
# length(unique(ampd_coal$ORISPL_CODE))
# 
# length(unique(ampd_harvard_coal$Unit.ID) )
# length(unique(ampd_coal$UNITID))
# 
# 
# sum(ampd_harvard_coal$NOx..tons., na.rm = T)
# sum(ampd_coal$NOx..tons., na.rm=T)
# 
# # almost same NOx emissions 59463888 vs 59456784
# 
# sum(ampd_harvard_coal$SO2..tons., na.rm = T)
# sum(ampd_coal$SO2..tons., na.rm=T)
# 
# # almost same SO2 emissions
# 
# 
# 
# 
# 
# #total facilities
# 
# length(unique(ampd_monthly$UNITID))
# length(unique(dfy$UNITID))
# length(unique(facility_attributes$UNITID))
# 
# 
# ####
# length(unique(ampd_harvard$Facility.ID..ORISPL.))
# length(unique(dfy$ORISPL_CODE))
# length(unique(facility_attributes$ORISPL_CODE ))
# 
# 
# ####
# 
# disperseR_units <- disperseR::units
# 
# length(unique(disperseR_units$ID))
# 
# ampd_coal <- ampd_raw %>% filter ( Fuel.Type..Primary. %in% coal.fuels)
# 
# length(unique(ampd_coal$ID))
# 
# 
# ##plots
# 
# ampd_coal <- ampd_coal 
# 
# ampd_coal_ym<- aggregate(list (NOx..tons.=ampd_coal$NOx..tons., 
#                           SO2..tons.=ampd_coal$SO2..tons.), by=list( year=ampd_coal$year,
#             month=ampd_coal $month), FUN=sum)
# ampd_coal_ym $group <- ""
# 

# PP.units.monthly1995_2017
