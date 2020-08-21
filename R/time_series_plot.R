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


ampd_raw <- read.fst ("data/ampd_raw.fst")

ampd_harvard <- fread ("data/AMPD_Unit_with_Sulfur_Content_and_Regulations_with_Facility_Attributes.csv")

names(ampd_harvard)
ampd_harvard<- as.data.table (ampd_harvard)
ampd_harvard <- ampd_harvard [ , V1 := NULL] 

# ampd <- as.data.table( ampd_raw %>% filter (year == 2015 :2020, month == 1:6))

# str(ampd)

# half.year.emission<- setDT(ampd)[, .(SO2..tons. = sum(SO2..tons., na.rm=TRUE),
#                            NOx..tons. = sum(NOx..tons., na.rm=TRUE),
#                            CO2..tons. = sum(CO2..tons., na.rm=TRUE),
#                            SUM_OP_TIME = sum(SUM_OP_TIME, na.rm=TRUE),
#                            Gross.Load..MW.h. = sum(Gross.Load..MW.h., na.rm=TRUE),
#                            Steam.Load..1000lb. = sum(Steam.Load..1000lb., na.rm=TRUE),
#                            HEAT.INPUT = sum(HEAT.INPUT, na.rm=TRUE)), 
#                        by = .(STATE, year, month)]
# 
# half.year.emission %>% filter ( STATE=="VA")
# 
# states <- c("CA","VA", "FL")
# 
# half.year.emission %>% as_tibble()



# dfz<- ampd %>% group_by(month, year, STATE) %>% summarise_all(sum)

# names(ampd_raw)

#sorting AMPD data by state, year and month

ampd_sym<- aggregate(list (NOx..tons.=ampd_raw$NOx..tons., 
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

names(ampd_harvard)

ampd_harvard_sym<- aggregate(list (NOx..tons.=ampd_harvard$NOx..tons., 
                                   SO2..tons.=ampd_harvard$SO2..tons.,
                                   CO2..short.tons.= ampd_harvard$CO2..short.tons.,
                                   Gross.Load..MW.h.=ampd_harvard$Gross.Load..MW.h.,
                                   Steam.Load..1000lb.= ampd_harvard$Steam.Load..1000lb.,
                                   Heat.Input..MMBtu.= ampd_harvard$Heat.Input..MMBtu.
), by=list( State= ampd_harvard$ State.x, Year=ampd_harvard$Year,
            Month=ampd_harvard $Month), FUN=sum, na.rm=TRUE)

ampd_harvard_ym<- aggregate(list (NOx..tons.=ampd_harvard$NOx..tons., 
                                   SO2..tons.=ampd_harvard$SO2..tons.,
                                   CO2..short.tons.= ampd_harvard$CO2..short.tons.,
                                   Gross.Load..MW.h.=ampd_harvard$Gross.Load..MW.h.,
                                   Steam.Load..1000lb.= ampd_harvard$Steam.Load..1000lb.,
                                   Heat.Input..MMBtu.= ampd_harvard$Heat.Input..MMBtu.
), by=list(  Year=ampd_harvard$Year,
            Month=ampd_harvard $Month), FUN=sum, na.rm=TRUE)


# just taking first 6 months data (for comparing 6 months of data)

# ampd_6 <- dfz %>% filter( year >= 2016, month <=6 ) 
# 
# ampd_6 %>% filter (STATE== "OR")
# 
# ampd_all %>% filter (year== 2015)
# 
# as.tibble(ampd_all)


#ampd_ym plot

names(ampd_ym)

years <- c (2010, 2011, 2012)
ampd_ym %>% filter( year %in% years) %>%
  ggplot(aes(month, NOx..tons., color= year, group= year )) +
  geom_point() + geom_line(aes())+ labs(x= "Month", 
                                   y = "NOx Emission (tons)",
                                   title = "NOx Emission  ") +
  scale_x_continuous(breaks = seq(0, 12, by = 1)) 


Years<- c (2010, 2011, 2012)
ampd_harvard_ym %>% filter( Year %in% Years) %>%
  ggplot(aes(Month, NOx..tons., color= Year, group= Year )) +
  geom_point() + geom_line(aes())+ labs(x= "Month", 
                                        y = "NOx Emission (tons)",
                                        title = "NOx Emission Emission ") +
  scale_x_continuous(breaks = seq(0, 12, by = 1)) 




ampd_ym %>% filter( year >= 2018) %>%
  ggplot(aes(month, SO2..tons., color= year, group= year, )) +
  geom_point() + geom_line(aes())+ labs(x= "Month", 
                                        y = "SO2 Emission (tons)",
                                        title = "Emission O2 Emission from 2018 to 2020") +
  scale_x_continuous(breaks = seq(0, 12, by = 1)) 






#NOX and SO2 emission year wise

ampd_ym %>% filter (year >= 1995) %>%
  ggplot(aes(year, NOx..tons., group= year)) + geom_boxplot()+ labs(x= "Year", 
                                        y = "NOx Emission (tons)",
                                        title = "NOx Emission Emission 1995 to 2020") +
  scale_x_continuous(breaks = seq(1995, 2021, by = 2))


ampd_harvard_ym %>% filter( Year >= 1995) %>%
  ggplot(aes(Year, NOx..tons., group= Year )) +geom_boxplot() + labs(x= "Month", 
                                        y = "NOx Emission (tons)",
                                        title = "NOx Emission Emission ") +
  scale_x_continuous(breaks = seq(1995, 2021, by = 2)) 


#SO2


ampd_ym %>% filter (year >= 1995) %>%
  ggplot(aes(year, SO2..tons., group= year)) + geom_boxplot()+ labs(x= "Year", 
                                                                    y = "SO2 Emission (tons)",
                                                                    title = "SO2 Emission 1995 to 2020") +
  scale_x_continuous(breaks = seq(1995, 2021, by = 2))

ampd_ym %>% filter (year <= 2014) %>%
  ggplot(aes(year, SO2..tons., group= year)) + geom_boxplot()+ labs(x= "Year", 
                                                                    y = "SO2 Emission (tons)",
                                                                    title = "SO2 Emission 1995 to 2020") +
  scale_x_continuous(breaks = seq(1995, 2021, by = 2))


#check
ampd_harvard_ym %>% filter( Year >= 1995) %>%
  ggplot(aes(Year, SO2..tons., group= Year )) +geom_boxplot() + labs(x= "Month", 
                                                                     y = "SO2 Emission (tons)",
                                                                     title = "SO2 Emission Emission ") +
  scale_x_continuous(breaks = seq(1995, 2021, by = 2)) 


#looking into emissions (state variable, year fixed)


# NOx emission 

states <- c ("GA", "TX", "VA",  "PA")

ampd_sym %>% filter(STATE %in% states, year == 2014) %>%
  ggplot(aes(month, NOx..tons., color= STATE, group= STATE)) +
  geom_point() + geom_line()+ labs(x= "Month", 
                                   y = "NOx Emission (tons)",
                                   title = "Emission  from 2016 to 2020") +
  scale_x_continuous(breaks = seq(0, 12, by = 1))  + ylim (0, 20000)


ampd_harvard_sym %>% filter(State %in% states, Year == 2014) %>%
  ggplot(aes(Month, NOx..tons., color= State, group= State)) +
  geom_point() + geom_line()+ labs(x= "Month", 
                                   y = "NOx Emission (tons)",
                                   title = "") +
  scale_x_continuous(breaks = seq(0, 12, by = 1))  + ylim (0, 20000)

#texas not matching with harvard data



#looking into emissions (year variable, state fixed)

years <- c (2017, 2018, 2019, 2020)

ampd_sym %>% filter(year %in% years, STATE == "TX") %>%
  ggplot(aes(month, NOx..tons., color= year, group= year)) +
  geom_point() + geom_line()+ labs(x= "Month", 
                                   y = "NOx Emission (tons)",
                                   title = "") +
  scale_x_continuous(breaks = seq(0, 12, by = 1))  + ylim (0, 20000)


ampd_sym %>% filter(year %in% years, STATE == "TX") %>%
  ggplot(aes(month, SO2..tons., color= year, group= year)) +
  geom_point() + geom_line()+ labs(x= "Month", 
                                   y = "SO2 Emission (tons)",
                                   title = "") +
  scale_x_continuous(breaks = seq(0, 12, by = 1))  + ylim (0, 30000)



years <- c (2011, 2012, 2013)

ampd_harvard_sym %>% filter(Year %in% years, State == "WV") %>%
  ggplot(aes(Month, SO2..tons., color= Year, group= Year)) +
  geom_point() + geom_line()+ labs(x= "Month", 
                                   y = "SO2 Emission (tons)",
                                   title = "") +
  scale_x_continuous(breaks = seq(0, 12, by = 1))  + ylim (0, 25000)

ampd_sym %>% filter(year %in% years, STATE == "WV") %>%
  ggplot(aes(month, SO2..tons., color= year, group= year)) +
  geom_point() + geom_line()+ labs(x= "Month", 
                                   y = "SO2 Emission (tons)",
                                   title = "") +
  scale_x_continuous(breaks = seq(0, 12, by = 1))  + ylim (0, 25000)









# ampd_harvard_sym %>% filter(Year %in% years, State == "TX") %>%
#   ggplot(aes(Month, NOx..tons., color= Year, group= Year)) +
#   geom_point() + geom_line()+ labs(x= "Month", 
#                                    y = "NOx Emission (tons)",
#                                    title = "") +
#   scale_x_continuous(breaks = seq(0, 12, by = 1))  + ylim (0, 20000)


states <- c ("TX", "VA", "FL")

ampd_6 %>% filter(STATE %in% states, year == 2020 ) %>%
  ggplot(aes(month, NOx..tons., group = STATE, color= STATE)) + 
  geom_line()+ geom_point() + labs(x= "Month", 
                                   y = "NOx Emission (tons)",
                                   title = "Emission") +
  scale_x_continuous(breaks = seq(0, 12, by = 1))



#heat input

states <- c ("TX")

ampd_6 %>% filter(STATE %in% states, year == 2020 ) %>%
  ggplot(aes(month, HEAT.INPUT, group = STATE, color= STATE)) + 
  geom_line()+ geom_point() + labs(x= "Month", 
                                   y = "HEAT.INPUT",
                                   title = "Emission") +
  scale_x_continuous(breaks = seq(0, 12, by = 1))













dfz %>% filter (STATE== "VA", month <= 6, year>= 2018)


ampd %>% group_by(month, year, STATE) 



TX_county<- ampd_raw %>% filter (STATE=="TX") %>% select (County)
names (TX_county)

TX_county %>% unique(County)
as.character(unique(unlist(TX_county)))

# https://eaglefordshale.com/counties

ampd_raw %>% filter (STATE=="TX", County == "Milam County")


       