#script 3 (use this script)
#Script 3: Merging facility attributes on monthly data
#here I've used Facility attributes data provided by Prof. Lucas Henneman


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


#on hopper cluster
#setwd ("/projects/HAQ_LAB/mrasel/R/ampd-raw-data-processing")


ampd_monthly<- as.data.table(read.fst ("data/ampd_monthly.fst"))

# ampd_monthly_va <- ampd_monthly %>% filter (STATE=="VA")

facility_attributes <- fread ("data/facility_2016-2018_3.csv")


names(facility_attributes)
names (ampd_monthly)


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



setnames(facility_attributes, variables)

colnames(facility_attributes)[3] <- "ORISPL_CODE"

colnames(facility_attributes)[4] <- "UNITID"

colnames(facility_attributes)[1] <- "STATE"

# facility_attributes <- facility_attributes %>% dplyr::select (-c ("Fuel.Type..Primary.", "Fuel.Type..Secondary."))


# facility_attributes_va <- facility_attributes %>% filter (STATE=="VA")

length(unique(ampd_monthly$ORISPL_CODE))
length(unique(facility_attributes$ORISPL_CODE))

#my data is short of 23 facilities

#merging two dataframe


# ampd_monthly_va <- ampd_monthly_va[, ID := paste(ORISPL_CODE, UNITID, sep = "-")]
# 
# 
# facility_attributes_va <- facility_attributes_va[, ID := paste(ORISPL_CODE, UNITID, sep = "-")]

# facility_attributes <- facility_attributes %>% 
#   dplyr::select (-c ("STATE", "ORISPL_CODE", "UNITID" ))
# facility_attributes_va <- facility_attributes_va %>% 
#   dplyr::select (-c ("ORISPL_CODE", "UNITID", "STATE" ))

# length(unique(ampd_monthly_va$ID))
# length(unique(facility_attributes_va$ID))
ampd_monthly <- as_tibble(ampd_monthly %>% distinct())
facility_attributes <- as_tibble((facility_attributes %>% distinct()))

ampd_monthly$UNITID <-  str_replace(ampd_monthly$UNITID, "^0+" ,"")
facility_attributes$UNITID <- str_replace(facility_attributes$UNITID, "^0+" ,"")
ampd_monthly$UNITID [ampd_monthly$ORISPL_CODE==7790] <- "1-Jan" #for 7790 facility, unit id is different from my dataset. my data set has "1-1


#Year is activity year. In that year may be facility changed fuel types.

ampd_monthly$Year <- ampd_monthly$year
dfy <- merge(ampd_monthly,facility_attributes, by=c("STATE", "ORISPL_CODE",
                                                            "UNITID", "Year" ), all.x = T, allow.cartesian = T) 

names(dfy)

dfy<- unique( as.data.table(dfy), by = c ("ORISPL_CODE", "UNITID", "year", "month", "STATE" )) 


dfy<- as.data.table(dfy)

write.fst(dfy, "data/ampd_monthly_all.fst")

# =============================================================================================

# Data validation with AMPD Harvard datasets after merging Facility attributes information

# Issue: after merging I'm missing some information. ampd_monthly dataset has 1140789 observation
# However, when I merge facility attributes and create ampd_monthly_all file, it has 1072662 observations  (1997-2020 Q1)
# =============================================================================================




ampd_raw <- read.fst ("data/ampd_monthly_all.fst")

ampd_harvard <- fread ("data/AMPD_Unit_with_Sulfur_Content_and_Regulations_with_Facility_Attributes.csv")

names(ampd_harvard)
ampd_harvard<- as.data.table (ampd_harvard)
ampd_harvard <- ampd_harvard [ , V1 := NULL] 


#Looking into texas 2010, month 7 data for example. 




ampd_harvard%>% filter (State.x== "TX", Year== 2010, Month == 7) %>% top_n(2, NOx..tons.) %>% 
  dplyr::select (Facility.ID..ORISPL.,  Unit.ID , NOx..tons. ,SO2..tons.  ) 

ampd_raw %>% filter (STATE== "TX", year== 2010, month == 7) %>%top_n(2, NOx..tons.) %>%
  dplyr::select (ORISPL_CODE, UNITID, NOx..tons., SO2..tons.)



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



#Changing column names of ampd_harvard data to match  column names of  ampd_raw data

# 
# ampd_harvard <- ampd_harvard %>% drop_na(NOx..tons.) 




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
# data validation
# =================================

ampd_monthly <- read.fst ("data/ampd_monthly_all.fst")
ampd_monthly_coal <- ampd_monthly %>% filter (year>= 1997 & year <=2014 & Fuel.Type..Primary.== "Coal")

#368 units has coal fuel in my dataset

#Harvard AMPD dataset


ampd_harvard <- fread ("data/AMPD_Unit_with_Sulfur_Content_and_Regulations_with_Facility_Attributes.csv")
ampd_harvard<- as.data.table (ampd_harvard)
ampd_harvard <- ampd_harvard [ , V1 := NULL]
ampd_harvard<- unique( ampd_harvard, by = c ("Facility.ID..ORISPL.", "Unit.ID", "Year", "Month", "State.x"))
ampd_harvard <- ampd_harvard[, ID := paste(Facility.ID..ORISPL., Unit.ID, sep = "-")]
ampd_harvard_coal <- ampd_harvard %>% filter (Year>= 1997 & Year <=2014 & Fuel.Type..Primary..x=="Coal")

length(unique(ampd_harvard_coal$Facility.ID..ORISPL.) )

#harvard dataset has 533 facility with coal fuel types

dfy1997_2014 <- dfy %>%  filter (year >=1997 & year <=2014  )
length(unique(dfy1997_2014$ORISPL_CODE))

dfy_coal <- dfy %>%  filter (year >=1997 & year <=2014 & Fuel.Type..Primary.=="Coal" )
length(unique(dfy_coal$ORISPL_CODE))

ampd_harvard_facility <- unique(ampd_harvard_coal$Facility.ID..ORISPL.)
dfy_coal_facility <- unique(dfy_coal$ORISPL_CODE)

ampd_harvard_facility %in% dfy_coal_facility


ampd_harvard_facility[410]
dfy_coal_7790 <- dfy%>% filter (ORISPL_CODE==7790)

ampd_harvard_coal_7790 <- ampd_harvard %>%  filter (Facility.ID..ORISPL.==7790)

facility_attributes_coal <- facility_attributes %>%  filter (Fuel.Type..Primary.== "Coal")

facility_attributes_coal_7790 <- facility_attributes %>% filter(ORISPL_CODE==7790)
unique(facility_attributes_coal_6193$Fuel.Type..Primary.)

length(unique(facility_attributes_coal$ORISPL_CODE))

dfy_coal_units <- unique(dfy_coal$UNITID)
fac_att_units <- unique(facility_attributes_coal$UNITID)

xx <- fac_att_units %in% dfy_coal_units


length(unique(ampd_monthly$UNITID))
length(unique(dfy$UNITID))
length(unique(facility_attributes$UNITID))


####
ampd_harvard_va <- ampd_harvard %>% filter (Year>= 1997 & Year <=2014  & 
                                              State.x== "VA")

length(unique(ampd_harvard_va$Facility.ID..ORISPL.))

length(unique(dfy_va$ORISPL_CODE))



length(unique(dfy_va_coal$ORISPL_CODE))
length(unique(ampd_harvard_coal_va$Facility.ID..ORISPL.) )


ampd_harvard_coal_va_10017 <- ampd_harvard_coal_va %>% filter (Facility.ID..ORISPL.==10017)
dfy_va_10017 <- dfy_va %>% filter (ORISPL_CODE==10017)

facility_attributes_va_10017 <- facility_attributes_va %>%  filter (ORISPL_CODE==10017)



unique(dfy_va$UNITID)

unique(facility_attributes_va$UNITID)



#

length(unique(ampd_harvard$Facility.ID..ORISPL.))
length(unique(dfy$ORISPL_CODE))
length(unique(facility_attributes$ORISPL_CODE ))

