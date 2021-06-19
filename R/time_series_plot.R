#time series plot of AMPD data 
#AMPD data were collected from EPA FTP site
#

# 
# install.packages("mapproj")
# 
# install.packages("ggpubr")
# install.packages("fiftystater")
#install.packages('gsynth', type = 'source')

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


ampd_raw <- as.data.table(read.fst ("data/ampd_monthly_all.fst"))

ampd_raw <- ampd_raw [, ID := paste(ORISPL_CODE, UNITID, sep = "-")]


#filtering electric utility as source category

ampd_raw <- ampd_raw %>% filter (Source.Category== "Electric Utility")

#Glossary of ampd data
# glossary <- read.csv("data/datadefinition_09-01-2020_170204640.csv")

# Facility ID (ORISPL)	The unique six-digit facility identification number, also called an ORISPL, assigned by the Energy Information Administration.
# Unit ID	Unique identifier for each unit at a facility.
# Unit	A fossil fuel-fired combustion device.
# SO2 (tons)	Sulfur dioxide emissions in short tons.
# SO2 Phase	Title IV of the Clean Air Act SO2 Phase. Phase I started in 1995; Phase II started in 2000.
# Steam Load (1000lb)	Total steam pressure produced by a unit or source in any calendar year (or other specified time period) produced by combusting a given heat input of fuel.
# SO2 Control(s)	Method or equipment used by the combustion unit to minimize production or emission of sulfur dioxide (SO2).
# PM Control(s)	Method or equipment used by the combustion unit to minimize production or emission of particulate matter (PM).
# Operating Time	Any part of an hour in which a unit combusts any fuel.
# Operating Status	An indication of the present condition of a unit (planned, operating, shutdown, etc.).
# NOx Phase	Group 1 boilers are divided into two mutually exclusive groups: Phase I and Phase II. The Phase a Group 1 boiler is associated with determines (in part) the Acid Rain NOx limit a boiler is subject to. There is no Phase I-Phase II bifurcation of Group 2 boilers.
# NOx (tons)	Nitrogen oxide emissions in short tons.
# NOx Control(s)	Method or equipment used by the combustion unit to minimize production or emission of nitrogen oxides (NOx).
# Max Hourly HI Rate (MMBtu/hr)	The design heat input capacity (in MMBtu/hr) for the unit or the highest hourly heat input rate observed in the past five years, whichever is greater.
# Hg Control(s)	Method or equipment used by the combustion unit to minimize production or emission of mercury (Hg).
# Heat Input (MMBtu)	The measure of utilization that is calculated by multiplying the quantity of fuel by the fuels heat content.
# Gross Load (MW-h)	Total electrical generation of a unit or source in any calendar year (or other specified time period) produced by combusting a given heat input of fuel.
# Fuel Type (Primary)	The primary type of fuel combusted by the unit.
# Fuel Type (Secondary)	The secondary type of fuel combusted by the unit.

#ampd_pplants_va <- ampd_raw %>% filter (STATE== 'VA') 

#write.csv(ampd_pplants_va, "/Users/munshirasel/Google Drive/R/ampd-3/data/ampd_pplants_va.csv")



#sorting AMPD data by state, year and month

ampd_sym<-            aggregate(list (NOx..tons.=ampd_raw$NOx..tons., 
                      SO2..tons.=ampd_raw$SO2..tons.,
                      CO2..tons.= ampd_raw$CO2..tons.,
                      Gross.Load..MW.h.=ampd_raw$Gross.Load..MW.h.,
                      Steam.Load..1000lb.= ampd_raw$Steam.Load..1000lb.,
                      HEAT.INPUT= ampd_raw$HEAT.INPUT,
                      SUM_OP_TIME= ampd_raw$SUM_OP_TIME
                      ), by=list(STATE=ampd_raw $ STATE, year=ampd_raw$year,
                                 month=ampd_raw $month), FUN=sum)


#sum of all states

ampd_ym<- aggregate(list (NOx..tons.=ampd_raw$NOx..tons., 
                      SO2..tons.=ampd_raw$SO2..tons.,
                      CO2..tons.= ampd_raw$CO2..tons.,
                      Gross.Load..MW.h.=ampd_raw$Gross.Load..MW.h.,
                      Steam.Load..1000lb.= ampd_raw$Steam.Load..1000lb.,
                      HEAT.INPUT= ampd_raw$HEAT.INPUT,
                      SUM_OP_TIME= ampd_raw$SUM_OP_TIME
), by=list( year=ampd_raw$year,
           month=ampd_raw $month), FUN=sum)


ampd_y<- aggregate(list (NOx..tons.=ampd_raw$NOx..tons., 
                          SO2..tons.=ampd_raw$SO2..tons.,
                          CO2..tons.= ampd_raw$CO2..tons.,
                          Gross.Load..MW.h.=ampd_raw$Gross.Load..MW.h.,
                          Steam.Load..1000lb.= ampd_raw$Steam.Load..1000lb.,
                          HEAT.INPUT= ampd_raw$HEAT.INPUT,
                          SUM_OP_TIME= ampd_raw$SUM_OP_TIME
), by=list( year=ampd_raw$year), FUN=sum)

ampd_y_97 <- ampd_y %>% filter (year >= 1997)

summary(ampd_y_97)

x <- ampd_y_97 %>%  filter (year >= 2015 & year < 2020) 

mean(x $SO2..tons.)


# names(ampd_harvard_sym)
names(ampd_sym)



#Yearly NOx emission:
names(ampd_ym)
plot1<- ampd_ym%>% filter (year >= 1995) %>%
  ggplot(aes( year, NOx..tons., group= year)) + geom_boxplot(aes(fill=year)) +
  scale_x_continuous(breaks = seq(1995, 2021, by = 2)) + labs(x= "Year", 
                                                              y = "NOx Emission (tons)",
                                                              title = "") +
   theme( panel.grid.major = element_blank(),  panel.grid.minor = element_blank(),
          panel.background = element_rect(colour = "black", size=1,   fill=NA)) +
   theme(legend.position = "") 

plot2 <- ampd_ym%>% filter (year >= 1997) %>%
  ggplot(aes( year, SO2..tons., group= year)) + geom_boxplot(aes(fill=year)) +
  scale_x_continuous(breaks = seq(1995, 2021, by = 2)) + labs(x= "Year", 
                                                              y = "SO2 Emission (tons)",
                                                              title = "") +
   theme( panel.grid.major = element_blank(),  panel.grid.minor = element_blank(),
          panel.background = element_rect(colour = "black", size=1,   fill=NA)) +
   theme(legend.position = "") 




plot3 <- ampd_ym%>% filter (year >= 1997) %>%
  ggplot(aes( year, CO2..tons., group= year)) + geom_boxplot(aes(fill=year)) +
  scale_x_continuous(breaks = seq(1995, 2021, by = 2)) + labs(x= "Year", 
                                                              y = "CO2 Emission (tons)",
                                                              title = "CO2 Emission")


ggarrange (plot1, plot2, ncol=1, nrow=2, common.legend = TRUE, legend= "bottom")

ggarrange (plot1, plot2, plot3, labels= c("A", "B", "C"), ncol=1, nrow=3)

plot1
plot2
plot3


plot4 <- ampd_ym%>% filter (year >= 1997) %>%
  ggplot(aes( year, Gross.Load..MW.h., group= year)) + geom_boxplot(aes(fill=year)) +
  scale_x_continuous(breaks = seq(1995, 2021, by = 2)) + labs(x= "Year", 
                                                              y = "Gross Load",
                                                              title = "") +
   theme( panel.grid.major = element_blank(),  panel.grid.minor = element_blank(),
          panel.background = element_rect(colour = "black", size=1,   fill=NA)) +
   theme(legend.position = "") 

plot5 <- ampd_ym%>% filter (year >=1997) %>%
  ggplot(aes( year, HEAT.INPUT, group= year)) + geom_boxplot(aes(fill=year)) +
  scale_x_continuous(breaks = seq(1995, 2021, by = 2)) + labs(x= "Year", 
                                                              y = "Heat Input",
                                                              title = "") +
   theme( panel.grid.major = element_blank(),  panel.grid.minor = element_blank(),
          panel.background = element_rect(colour = "black", size=1,   fill=NA)) +
   theme(legend.position = "") 

plot6 <- ampd_ym%>% filter (year >= 1997) %>%
  ggplot(aes( year, Steam.Load..1000lb., group= year)) + geom_boxplot(aes(fill=year)) +
  scale_x_continuous(breaks = seq(1995, 2021, by = 2)) + labs(x= "Year", 
                                                              y = "Steam Load",
                                                              title = "") +
   theme( panel.grid.major = element_blank(),  panel.grid.minor = element_blank(),
          panel.background = element_rect(colour = "black", size=1,   fill=NA)) +
   theme(legend.position = "") 


plot4
plot5
plot6





#Monthly comparision of emission data in the US


names(ampd_ym)


years <- c ( 2018, 2019, 2020, 2021)

plot1 <- ampd_ym %>% filter( year %in% years) %>%
  ggplot(aes(month, NOx..tons., color= year, group= year )) +
  geom_point() + geom_line()+ labs(x= "Month", 
                                   y = "NOx Emission (tons)",
                                   title = " ") +
  scale_x_continuous(breaks = seq(0, 12, by = 1)) +
   theme( panel.grid.major = element_blank(),  panel.grid.minor = element_blank(),
          panel.background = element_rect(colour = "black", size=1,   fill=NA)) 

plot2 <- ampd_ym %>% filter( year %in% years) %>%
  ggplot(aes(month, Gross.Load..MW.h., color= year, group= year )) +
  geom_point() + geom_line(aes())+ labs(x= "Month", 
                                        y = "Gross Load Emission (tons)",
                                        title = "Gross Load Emission  ") +
  scale_x_continuous(breaks = seq(0, 12, by = 1)) 

plot3 <- ampd_ym %>% filter( year %in% years) %>%
  ggplot(aes(month, Steam.Load..1000lb., color= year, group= year )) +
  geom_point() + geom_line(aes())+ labs(x= "Month", 
                                        y = "Steam Load",
                                        title = "Steam Load  ") +
  scale_x_continuous(breaks = seq(0, 12, by = 1)) 

plot4 <- ampd_ym %>% filter( year %in% years) %>%
  ggplot(aes(month, HEAT.INPUT, color= year, group= year )) +
  geom_point() + geom_line(aes())+ labs(x= "Month", 
                                        y = "Heat Input",
                                        title = "Heat Input") +
  scale_x_continuous(breaks = seq(0, 12, by = 1)) 

ggarrange(plot1, plot2, plot3, plot4, labels = c ("A", "B", "C", "D"), nrow=4, ncol= 1)


# =============================================================================
#NOx & SO2 emission over months US



#monthly SO2 emission year wise
years <- c (  2018, 2019, 2020, 2021)


ampd_ym %>% filter( year %in% years)  %>%
  ggplot(aes(month, SO2..tons., color= year, group= year, )) +
  geom_point() + geom_line(aes())+ labs(x= "Month", 
                                        y = "SO2 Emission (tons)",
                                        title = "") +
  scale_x_continuous(breaks = seq(0, 12, by = 1))  +
   theme( panel.grid.major = element_blank(),  panel.grid.minor = element_blank(),
          panel.background = element_rect(colour = "black", size=1,   fill=NA))


#monthly NOx emission year wise

ampd_ym %>% filter( year >= 2018) %>%
  ggplot(aes(month, NOx..tons., color= year, group= year, )) +
  geom_point() + geom_line(aes())+ labs(x= "Month", 
                                        y = "NOx Emission (tons)",
                                        title = "NOx Emission from 2018 to 2021") +
  scale_x_continuous(breaks = seq(0, 12, by = 1)) 


#Comparing NOx emission over months state wise
#########################

# NOx emission 



#looking into emissions (year variable, state fixed)

years <- c (  2017, 2018, 2019, 2020, 2021)

 ampd_sym %>% filter(year %in% years, STATE == "AL") %>%
  ggplot(aes(month, NOx..tons., color= year, group= year)) +
  geom_point() + geom_line()+ labs(x= "Month", 
                                   y = "NOx Emission (tons)",
                                   title = "") +
  scale_x_continuous(breaks = seq(0, 12, by = 1))  

 ampd_sym %>% filter(year %in% years, STATE == "IN") %>%
  ggplot(aes(month, NOx..tons., color= year, group= year)) +
  geom_point() + geom_line()+ labs(x= "Month", 
                                   y = "NOx Emission (tons)",
                                   title = "") +
  scale_x_continuous(breaks = seq(0, 12, by = 1)) 

 ampd_sym %>% filter(year %in% years, STATE == "VA") %>%
  ggplot(aes(month, NOx..tons., color= year, group= year)) +
  geom_point() + geom_line()+ labs(x= "Month", 
                                   y = "NOx Emission (tons)",
                                   title = "") +
  scale_x_continuous(breaks = seq(0, 12, by = 1)) 


 ampd_sym %>% filter(year %in% years, STATE == "NY") %>%
   ggplot(aes(month, NOx..tons., color= year, group= year)) +
   geom_point() + geom_line()+ labs(x= "Month", 
                                    y = "NOx Emission (tons)",
                                    title = "NY") +
   scale_x_continuous(breaks = seq(0, 12, by = 1)) +
    theme( panel.grid.major = element_blank(),  panel.grid.minor = element_blank(),
           panel.background = element_rect(colour = "black", size=1,   fill=NA)) 
 
 ampd_sym %>% filter(year %in% years, STATE == "MA") %>%
   ggplot(aes(month, NOx..tons., color= year, group= year)) +
   geom_point() + geom_line()+ labs(x= "Month", 
                                    y = "NOx Emission (tons)",
                                    title = "") +
   scale_x_continuous(breaks = seq(0, 12, by = 1)) 
 
 ampd_sym %>% filter(year %in% years, STATE == "DE") %>%
   ggplot(aes(month, NOx..tons., color= year, group= year)) +
   geom_point() + geom_line()+ labs(x= "Month", 
                                    y = "NOx Emission (tons)",
                                    title = "") +
   scale_x_continuous(breaks = seq(0, 12, by = 1)) 


 ampd_sym %>% filter(year %in% years, STATE == "CA") %>%
   ggplot(aes(month, NOx..tons., color= year, group= year)) +
   geom_point() + geom_line()+ labs(x= "Month", 
                                    y = "NOx Emission (tons)",
                                    title = "CA") +
   scale_x_continuous(breaks = seq(0, 12, by = 1)) + 
    theme( panel.grid.major = element_blank(),  panel.grid.minor = element_blank(),
           panel.background = element_rect(colour = "black", size=1,   fill=NA)) 
 
 ampd_sym %>% filter(year %in% years, STATE == "NJ") %>%
   ggplot(aes(month, NOx..tons., color= year, group= year)) +
   geom_point() + geom_line()+ labs(x= "Month", 
                                    y = "NOx Emission (tons)",
                                    title = "") +
   scale_x_continuous(breaks = seq(0, 12, by = 1)) 
 
 ampd_sym %>% filter(year %in% years, STATE == "MN") %>%
   ggplot(aes(month, NOx..tons., color= year, group= year)) +
   geom_point() + geom_line()+ labs(x= "Month", 
                                    y = "NOx Emission (tons)",
                                    title = "") +
   scale_x_continuous(breaks = seq(0, 12, by = 1)) 
 
 ampd_sym %>% filter(year %in% years, STATE == "MI") %>%
   ggplot(aes(month, NOx..tons., color= year, group= year)) +
   geom_point() + geom_line()+ labs(x= "Month", 
                                    y = "NOx Emission (tons)",
                                    title = "") +
   scale_x_continuous(breaks = seq(0, 12, by = 1)) 
 
 ampd_sym %>% filter(year %in% years, STATE == "PA") %>%
   ggplot(aes(month, NOx..tons., color= year, group= year)) +
   geom_point() + geom_line()+ labs(x= "Month", 
                                    y = "NOx Emission (tons)",
                                    title = "") +
   scale_x_continuous(breaks = seq(0, 12, by = 1)) 
 
 ampd_sym %>% filter(year %in% years, STATE == "OH") %>%
   ggplot(aes(month, NOx..tons., color= year, group= year)) +
   geom_point() + geom_line()+ labs(x= "Month", 
                                    y = "NOx Emission (tons)",
                                    title = "") +
   scale_x_continuous(breaks = seq(0, 12, by = 1)) 
 
 ampd_sym %>% filter(year %in% years, STATE == "WV") %>%
   ggplot(aes(month, NOx..tons., color= year, group= year)) +
   geom_point() + geom_line()+ labs(x= "Month", 
                                    y = "NOx Emission (tons)",
                                    title = "") +
   scale_x_continuous(breaks = seq(0, 12, by = 1)) 
 
 ampd_sym %>% filter(year %in% years, STATE == "NC") %>%
   ggplot(aes(month, NOx..tons., color= year, group= year)) +
   geom_point() + geom_line()+ labs(x= "Month", 
                                    y = "NOx Emission (tons)",
                                    title = "") +
   scale_x_continuous(breaks = seq(0, 12, by = 1)) 
 
 ampd_sym %>% filter(year %in% years, STATE == "TN") %>%
   ggplot(aes(month, NOx..tons., color= year, group= year)) +
   geom_point() + geom_line()+ labs(x= "Month", 
                                    y = "NOx Emission (tons)",
                                    title = "") +
   scale_x_continuous(breaks = seq(0, 12, by = 1)) 
 
 ampd_sym %>% filter(year %in% years, STATE == "TX") %>%
   ggplot(aes(month, NOx..tons., color= year, group= year)) +
   geom_point() + geom_line()+ labs(x= "Month", 
                                    y = "NOx Emission (tons)",
                                    title = "") +
   scale_x_continuous(breaks = seq(0, 12, by = 1)) 
 
 ampd_sym %>% filter(year %in% years, STATE == "FL") %>%
   ggplot(aes(month, NOx..tons., color= year, group= year)) +
   geom_point() + geom_line()+ labs(x= "Month", 
                                    y = "NOx Emission (tons)",
                                    title = "") +
   scale_x_continuous(breaks = seq(0, 12, by = 1)) 
 
 ampd_sym %>% filter(year %in% years, STATE == "MO") %>%
   ggplot(aes(month, NOx..tons., color= year, group= year)) +
   geom_point() + geom_line()+ labs(x= "Month", 
                                    y = "NOx Emission (tons)",
                                    title = "MO") +
   scale_x_continuous(breaks = seq(0, 12, by = 1)) + 
    theme( panel.grid.major = element_blank(),  panel.grid.minor = element_blank(),
           panel.background = element_rect(colour = "black", size=1,   fill=NA)) 
 
 ampd_sym %>% filter(year %in% years, STATE == "IA") %>%
   ggplot(aes(month, NOx..tons., color= year, group= year)) +
   geom_point() + geom_line()+ labs(x= "Month", 
                                    y = "NOx Emission (tons)",
                                    title = "") +
   scale_x_continuous(breaks = seq(0, 12, by = 1)) 
 
 ampd_sym %>% filter(year %in% years, STATE == "MD") %>%
   ggplot(aes(month, NOx..tons., color= year, group= year)) +
   geom_point() + geom_line()+ labs(x= "Month", 
                                    y = "NOx Emission (tons)",
                                    title = "") +
   scale_x_continuous(breaks = seq(0, 12, by = 1)) 
 
 ampd_sym %>% filter(year %in% years, STATE == "CT") %>%
   ggplot(aes(month, NOx..tons., color= year, group= year)) +
   geom_point() + geom_line()+ labs(x= "Month", 
                                    y = "NOx Emission (tons)",
                                    title = "") +
   scale_x_continuous(breaks = seq(0, 12, by = 1)) 
 
 ampd_sym %>% filter(year %in% years, STATE == "OK") %>%
   ggplot(aes(month, NOx..tons., color= year, group= year)) +
   geom_point() + geom_line()+ labs(x= "Month", 
                                    y = "NOx Emission (tons)",
                                    title = "") +
   scale_x_continuous(breaks = seq(0, 12, by = 1)) 
 
 ampd_sym %>% filter(year %in% years, STATE == "CO") %>%
   ggplot(aes(month, NOx..tons., color= year, group= year)) +
   geom_point() + geom_line()+ labs(x= "Month", 
                                    y = "NOx Emission (tons)",
                                    title = "") +
   scale_x_continuous(breaks = seq(0, 12, by = 1)) 
 
 ampd_sym %>% filter(year %in% years, STATE == "NV") %>%
   ggplot(aes(month, NOx..tons., color= year, group= year)) +
   geom_point() + geom_line()+ labs(x= "Month", 
                                    y = "NOx Emission (tons)",
                                    title = "") +
   scale_x_continuous(breaks = seq(0, 12, by = 1)) 
 
 ampd_sym %>% filter(year %in% years, STATE == "KS") %>%
   ggplot(aes(month, NOx..tons., color= year, group= year)) +
   geom_point() + geom_line()+ labs(x= "Month", 
                                    y = "NOx Emission (tons)",
                                    title = "") +
   scale_x_continuous(breaks = seq(0, 12, by = 1)) 
 
 ampd_sym %>% filter(year %in% years, STATE == "IL") %>%
   ggplot(aes(month, NOx..tons., color= year, group= year)) +
   geom_point() + geom_line()+ labs(x= "Month", 
                                    y = "NOx Emission (tons)",
                                    title = "") +
   scale_x_continuous(breaks = seq(0, 12, by = 1)) 
 
 ampd_sym %>% filter(year %in% years, STATE == "AZ") %>%
   ggplot(aes(month, NOx..tons., color= year, group= year)) +
   geom_point() + geom_line()+ labs(x= "Month", 
                                    y = "NOx Emission (tons)",
                                    title = "") +
   scale_x_continuous(breaks = seq(0, 12, by = 1)) 
 
 
 ampd_sym %>% filter(year %in% years, STATE == "SC") %>%
   ggplot(aes(month, NOx..tons., color= year, group= year)) +
   geom_point() + geom_line()+ labs(x= "Month", 
                                    y = "NOx Emission (tons)",
                                    title = "") +
   scale_x_continuous(breaks = seq(0, 12, by = 1)) 
 
 ampd_sym %>% filter(year %in% years, STATE == "MS") %>%
   ggplot(aes(month, NOx..tons., color= year, group= year)) +
   geom_point() + geom_line()+ labs(x= "Month", 
                                    y = "NOx Emission (tons)",
                                    title = "MS") +
   scale_x_continuous(breaks = seq(0, 12, by = 1)) + 
    theme( panel.grid.major = element_blank(),  panel.grid.minor = element_blank(),
           panel.background = element_rect(colour = "black", size=1,   fill=NA)) 
 
 
 ampd_sym %>% filter(year %in% years, STATE == "KY") %>%
   ggplot(aes(month, NOx..tons., color= year, group= year)) +
   geom_point() + geom_line()+ labs(x= "Month", 
                                    y = "NOx Emission (tons)",
                                    title = "") +
   scale_x_continuous(breaks = seq(0, 12, by = 1)) 
 
 ampd_sym %>% filter(year %in% years, STATE == "LA") %>%
   ggplot(aes(month, NOx..tons., color= year, group= year)) +
   geom_point() + geom_line()+ labs(x= "Month", 
                                    y = "NOx Emission (tons)",
                                    title = "") +
   scale_x_continuous(breaks = seq(0, 12, by = 1)) 
 
 
 ampd_sym %>% filter(year %in% years, STATE == "ME") %>%
   ggplot(aes(month, NOx..tons., color= year, group= year)) +
   geom_point() + geom_line()+ labs(x= "Month", 
                                    y = "NOx Emission (tons)",
                                    title = "") +
   scale_x_continuous(breaks = seq(0, 12, by = 1)) 
 
 ampd_sym %>% filter(year %in% years, STATE == "AR") %>%
   ggplot(aes(month, NOx..tons., color= year, group= year)) +
   geom_point() + geom_line()+ labs(x= "Month", 
                                    y = "NOx Emission (tons)",
                                    title = "") +
   scale_x_continuous(breaks = seq(0, 12, by = 1)) 
 
 
 ampd_sym %>% filter(year %in% years, STATE == "MT") %>%
   ggplot(aes(month, NOx..tons., color= year, group= year)) +
   geom_point() + geom_line()+ labs(x= "Month", 
                                    y = "NOx Emission (tons)",
                                    title = "") +
   scale_x_continuous(breaks = seq(0, 12, by = 1)) 
 
 
 ampd_sym %>% filter(year %in% years, STATE == "NE") %>%
   ggplot(aes(month, NOx..tons., color= year, group= year)) +
   geom_point() + geom_line()+ labs(x= "Month", 
                                    y = "NOx Emission (tons)",
                                    title = "") +
   scale_x_continuous(breaks = seq(0, 12, by = 1)) 
 
 ampd_sym %>% filter(year %in% years, STATE == "NH") %>%
   ggplot(aes(month, NOx..tons., color= year, group= year)) +
   geom_point() + geom_line()+ labs(x= "Month", 
                                    y = "NOx Emission (tons)",
                                    title = "") +
   scale_x_continuous(breaks = seq(0, 12, by = 1)) 
 
 
 ampd_sym %>% filter(year %in% years, STATE == "NM") %>%
   ggplot(aes(month, NOx..tons., color= year, group= year)) +
   geom_point() + geom_line()+ labs(x= "Month", 
                                    y = "NOx Emission (tons)",
                                    title = "") +
   scale_x_continuous(breaks = seq(0, 12, by = 1)) 
 
 
 ampd_sym %>% filter(year %in% years, STATE == "ND") %>%
   ggplot(aes(month, NOx..tons., color= year, group= year)) +
   geom_point() + geom_line()+ labs(x= "Month", 
                                    y = "NOx Emission (tons)",
                                    title = "") +
   scale_x_continuous(breaks = seq(0, 12, by = 1)) 
 
 
 ampd_sym %>% filter(year %in% years, STATE == "RI") %>%
   ggplot(aes(month, NOx..tons., color= year, group= year)) +
   geom_point() + geom_line()+ labs(x= "Month", 
                                    y = "NOx Emission (tons)",
                                    title = "") +
   scale_x_continuous(breaks = seq(0, 12, by = 1)) 
 
 ampd_sym %>% filter(year %in% years, STATE == "SD") %>%
   ggplot(aes(month, NOx..tons., color= year, group= year)) +
   geom_point() + geom_line()+ labs(x= "Month", 
                                    y = "NOx Emission (tons)",
                                    title = "") +
   scale_x_continuous(breaks = seq(0, 12, by = 1)) 
 
 ampd_sym %>% filter(year %in% years, STATE == "UT") %>%
   ggplot(aes(month, NOx..tons., color= year, group= year)) +
   geom_point() + geom_line()+ labs(x= "Month", 
                                    y = "NOx Emission (tons)",
                                    title = "") +
   scale_x_continuous(breaks = seq(0, 12, by = 1)) 
 
 ampd_sym %>% filter(year %in% years, STATE == "VT") %>%
   ggplot(aes(month, NOx..tons., color= year, group= year)) +
   geom_point() + geom_line()+ labs(x= "Month", 
                                    y = "NOx Emission (tons)",
                                    title = "") +
   scale_x_continuous(breaks = seq(0, 12, by = 1)) 
 
 ampd_sym %>% filter(year %in% years, STATE == "WA") %>%
   ggplot(aes(month, NOx..tons., color= year, group= year)) +
   geom_point() + geom_line()+ labs(x= "Month", 
                                    y = "NOx Emission (tons)",
                                    title = "") +
   scale_x_continuous(breaks = seq(0, 12, by = 1)) 
 
 ampd_sym %>% filter(year %in% years, STATE == "WI") %>%
   ggplot(aes(month, NOx..tons., color= year, group= year)) +
   geom_point() + geom_line()+ labs(x= "Month", 
                                    y = "NOx Emission (tons)",
                                    title = "") +
   scale_x_continuous(breaks = seq(0, 12, by = 1)) 
 
 ampd_sym %>% filter(year %in% years, STATE == "WY") %>%
   ggplot(aes(month, NOx..tons., color= year, group= year)) +
   geom_point() + geom_line()+ labs(x= "Month", 
                                    y = "NOx Emission (tons)",
                                    title = "") +
   scale_x_continuous(breaks = seq(0, 12, by = 1)) 
 
 
 ampd_sym %>% filter(year %in% years, STATE == "OR") %>%
   ggplot(aes(month, NOx..tons., color= year, group= year)) +
   geom_point() + geom_line()+ labs(x= "Month", 
                                    y = "NOx Emission (tons)",
                                    title = "") +
   scale_x_continuous(breaks = seq(0, 12, by = 1)) 
 
 
 ampd_sym %>% filter(year %in% years, STATE == "GA") %>%
   ggplot(aes(month, NOx..tons., color= year, group= year)) +
   geom_point() + geom_line()+ labs(x= "Month", 
                                    y = "NOx Emission (tons)",
                                    title = "") +
   scale_x_continuous(breaks = seq(0, 12, by = 1)) 
 
 
 ampd_sym %>% filter(year %in% years, STATE == "ID") %>%
   ggplot(aes(month, NOx..tons., color= year, group= year)) +
   geom_point() + geom_line()+ labs(x= "Month", 
                                    y = "NOx Emission (tons)",
                                    title = "") +
   scale_x_continuous(breaks = seq(0, 12, by = 1)) 
 
 ampd_sym %>% filter(year %in% years, STATE == "DC") %>%
   ggplot(aes(month, NOx..tons., color= year, group= year)) +
   geom_point() + geom_line()+ labs(x= "Month", 
                                    y = "NOx Emission (tons)",
                                    title = "") +
   scale_x_continuous(breaks = seq(0, 12, by = 1)) 
 
 ampd_sym %>% filter(year %in% years, STATE == "AK") %>%
   ggplot(aes(month, NOx..tons., color= year, group= year)) +
   geom_point() + geom_line()+ labs(x= "Month", 
                                    y = "NOx Emission (tons)",
                                    title = "") +
   scale_x_continuous(breaks = seq(0, 12, by = 1)) 
 
 ampd_sym %>% filter(year %in% years, STATE == "PR") %>%
   ggplot(aes(month, NOx..tons., color= year, group= year)) +
   geom_point() + geom_line()+ labs(x= "Month", 
                                    y = "NOx Emission (tons)",
                                    title = "") +
   scale_x_continuous(breaks = seq(0, 12, by = 1)) 
 
 
 



#Sum op time
ampd_ym %>% filter ( year <= 2020) %>%
  ggplot(aes(year, SUM_OP_TIME, group=year)) + geom_boxplot() +
  scale_x_continuous(breaks = seq(1995, 2021, by = 2)) + labs( x= "Year", y= " Operation time (hours)",
                                                               title= "Operating time")  

#Gross Load

ampd_ym %>% filter ( year <= 2020) %>%
  ggplot(aes(year,Gross.Load..MW.h., group=year)) + geom_boxplot() +
  scale_x_continuous(breaks = seq(1995, 2021, by = 2)) + labs( x= "Year", y= " Gross load (MW-hr)",
                                                               title= "Gross Load MW-hr")  

#Steam Load

ampd_ym %>% filter ( year <= 2020) %>%
  ggplot(aes(year,Steam.Load..1000lb., group=year)) + geom_boxplot() +
  scale_x_continuous(breaks = seq(1995, 2021, by = 2)) + labs( x= "Year", y= " Steam.Load..1000lb.",
                                                               title= "Steam.Load..1000lb.")  

#Heat Input

ampd_ym %>% filter ( year <= 2020) %>%
  ggplot(aes(year,HEAT.INPUT, group=year)) + geom_boxplot() +
  scale_x_continuous(breaks = seq(1995, 2021, by = 2)) + labs( x= "Year", y= " HEAT.INPUT",
                                                               title= "HEAT.INPUT")  
       




fuel <-  c ("Coal", "Pipeline Natural Gas")
coal.natgas <-  ampd_raw %>% filter (Fuel.Type..Primary. %in% fuel)

coal.natgas_ym <-  aggregate(list (NOx..tons.=coal.natgas $NOx..tons., 
                SO2..tons.=coal.natgas $SO2..tons.
     
), by=list(  Fuel.Type..Primary.= coal.natgas$ Fuel.Type..Primary., year=coal.natgas $year), FUN=sum)



coal.natgas_ym %>%
  ggplot() + geom_line(aes(year, NOx..tons., color= Fuel.Type..Primary.)) +
  scale_x_continuous(breaks = seq(1995, 2021, by = 2))  

coal.natgas_ym %>%
  ggplot() + geom_line(aes(year, SO2..tons., color= Fuel.Type..Primary.)) +
  scale_x_continuous(breaks = seq(1995, 2021, by = 2))  



# ================================================
#CO2
ampd_ym%>% filter (year >= 1997) %>%
  ggplot(aes( year, CO2..tons., group= year)) + geom_boxplot(aes(fill=year)) +
  scale_x_continuous(breaks = seq(1995, 2021, by = 2)) + labs(x= "Year", 
                                                              y = "CO2 Emission (tons)",
                                                              title = "CO2 Emission")  + 
   theme( panel.grid.major = element_blank(),  panel.grid.minor = element_blank(),
          panel.background = element_rect(colour = "black", size=1,   fill=NA)) 


#Heat Input
ampd_ym %>% filter ( year <= 2020) %>%
  ggplot(aes(year,HEAT.INPUT, group=year)) + geom_boxplot(aes(fill=year)) +
  scale_x_continuous(breaks = seq(1995, 2021, by = 2)) + labs( x= "Year", y= " HEAT.INPUT (MMBtu)",
                                                               title= "HEAT.INPUT")  



#Normalizing data with respect to heat and CO2



ampd_sym <-  drop_na(  mutate (ampd_sym, NOx.to.Heat = NOx..tons./ HEAT.INPUT, 
                     NOx.to.CO2 = NOx..tons./ CO2..tons.,
                     SO2.to.Heat = SO2..tons./ HEAT.INPUT ,
                     SO2.to.CO2 = SO2..tons./ CO2..tons. )) #per million

ampd_ym <-  drop_na(mutate (ampd_ym, NOx.to.Heat = NOx..tons./ HEAT.INPUT, 
                     NOx.to.CO2 = NOx..tons./ CO2..tons. ,
                     SO2.to.Heat = SO2..tons./ HEAT.INPUT ,
                     SO2.to.CO2 = SO2..tons./ CO2..tons. ) )





#boxplot year wise of normalized data

#NOx

plot1<- ampd_ym%>% filter (year <= 2020) %>%
  ggplot(aes( year, NOx.to.Heat, group= year)) + geom_boxplot(aes(fill=year)) +
  scale_x_continuous(breaks = seq(1995, 2021, by = 2)) + labs(x= "Year", 
                                                              y = "NOx Emission/Heat Input (tons/MMBtu)",
                                                              title = "NOx Emission/Heat Input (tons/MMBtu) (~2020June)")

plot2 <- ampd_ym%>% filter (year <= 2020) %>%
  ggplot(aes( year, NOx.to.CO2, group= year)) + geom_boxplot(aes(fill=year)) +
  scale_x_continuous(breaks = seq(1995, 2021, by = 2)) + labs(x= "Year", 
                                                              y = "NOx.to.CO2 Emission (tons/tons)",
                                                              title = "NOx/CO2 Emission (tons/tons) (~2020June)") 


#SO2, year wise emission of Normalized data (Heat/CO2)


plot3<- ampd_ym%>% filter (year <= 2020) %>%
  ggplot(aes( year, SO2.to.Heat, group= year)) + geom_boxplot(aes(fill=year)) +
  scale_x_continuous(breaks = seq(1995, 2021, by = 2)) + labs(x= "Year", 
                                                              y = "SO2 Emission/Heat Input (tons/MMBtu)",
                                                              title = "SO2 Emission/Heat Input (tons/MMBtu) (~2020June)")

plot4 <- ampd_ym%>% filter (year <= 2020) %>%
  ggplot(aes( year, SO2.to.CO2, group= year)) + geom_boxplot(aes(fill=year)) +
  scale_x_continuous(breaks = seq(1995, 2021, by = 2)) + labs(x= "Year", 
                                                              y = "SO2/CO2 Emission (tons/tons)",
                                                              title = "SO2/CO2 Emission (tons/tons) (~2020June)")



ggarrange (plot1, plot3, labels= c("A", "B"), ncol=1, nrow=2)
ggarrange (plot2, plot4, labels= c("A", "B"), ncol=1, nrow=2)




# load the map data
states = map_data("state")
str(states)

## MAKE THE PLOT ####

library(fiftystater)
data("fifty_states")
library(dslabs)
data(murders)
state.abb <- murders %>% select (state, abb) 
colnames(state.abb)[2] <- "STATE"

ampd_sym <-  merge(ampd_sym, state.abb, by="STATE", all.x=T)

ampd_sym$state.lower <-  tolower(ampd_sym$state)



# NOx over Heat
years <-  c (2020)
ampd_sym %>% filter (year %in% years) %>% ggplot( aes(map_id = state.lower)) + 
  # map points to the fifty_states shape data
  geom_map(aes(fill = NOx.to.Heat), map = fifty_states) + 
  expand_limits(x = fifty_states$long, y = fifty_states$lat) +
  coord_map() +
  scale_x_continuous(breaks = NULL) + 
  scale_y_continuous(breaks = NULL) +
  labs(x = "NOx Emission/Heat Input (tons/MMBtu)", y = "") +
  theme(legend.position = "bottom", 
        panel.background = element_blank()) +   facet_grid(.~ year) + 
   theme( panel.grid.major = element_blank(),  panel.grid.minor = element_blank(),
          panel.background = element_rect(colour = "black", size=1,   fill=NA)) 


#SO2 over Heat input

years <-  c (2019)
ampd_sym %>% filter (year %in% years) %>% ggplot( aes(map_id = state.lower)) + 
  # map points to the fifty_states shape data
  geom_map(aes(fill = SO2.to.Heat), map = fifty_states) + 
  expand_limits(x = fifty_states$long, y = fifty_states$lat) +
  coord_map() +
  scale_x_continuous(breaks = NULL) + 
  scale_y_continuous(breaks = NULL) +
  labs(x = "SO2/Heat Input (tons/MMBtu)", y = "") +
  theme(legend.position = "bottom", 
        panel.background = element_blank()) +   facet_grid(.~ year) 



#Comparing NOx emission vs. month state wise
#########################

# NOx emission 

states <- c ("TX", "UT", "NM",  "PA")

plot1<- ampd_sym %>% filter( STATE %in% states, year == 2020) %>% 
  ggplot(aes(month, NOx.to.Heat, color= STATE, group= STATE)) +
  geom_point() + geom_line()+ labs(x= "Month", 
                                   y = "NOx Emission/Heat Input (tons/MMBtu)",
                                   title = "2020") +
  scale_x_continuous(breaks = seq(0, 12, by = 1)) 

plot1

states <- c ("TX", "UT", "NM",  "PA")

plot2<- ampd_sym %>% filter( STATE %in% states, year == 2020) %>% 
  ggplot(aes(month, SO2.to.Heat, color= STATE, group= STATE)) +
  geom_point() + geom_line()+ labs(x= "Month", 
                                   y = "SO2 Emission/Heat Input (tons/MMBtu)",
                                   title = "2020") +
  scale_x_continuous(breaks = seq(0, 12, by = 1)) 

plot2



#looking into emissions (year variable, state fixed)

years <- c (2008, 2011, 2014, 2017, 2019)

ampd_sym %>% filter(year %in% years, STATE == "NM") %>%
  ggplot(aes(month, NOx.to.Heat, color= year, group= year)) +
  geom_point() + geom_line()+ labs(x= "Month", 
                                   y = "NOx Emission/Heat Input(tons/MMBtu)",
                                   title = "New Mexico") +
  scale_x_continuous(breaks = seq(0, 12, by = 1))  

ampd_sym %>% filter(year %in% years, STATE == "NM") %>%
  ggplot(aes(month, NOx.to.CO2, color= year, group= year)) +
  geom_point() + geom_line()+ labs(x= "Month", 
                                   y = "NOx/CO2  (tons/tons)",
                                   title = "New Mexico") +
  scale_x_continuous(breaks = seq(0, 12, by = 1))  










## MAKE THE PLOT ####

library(fiftystater)
data("fifty_states")
library(dslabs)
data(murders)
state.abb <- murders %>% select (state, abb) 
colnames(state.abb)[2] <- "STATE"

ampd_sym_com <-  merge(ampd_sym_com, state.abb, by="STATE", all.x=T)

ampd_sym_com$state.lower <-  tolower(ampd_sym_com$state)





####Corrplot



library(corrplot)
ampd_raw <- as_tibble(ampd_raw)
ampd_raw_corrplot <- ampd_raw %>% dplyr::select( SO2..tons., NOx..tons., CO2..tons., Avg.SO2.RATE, Avg.NOx.RATE, Avg.CO2.RATE,
                                               SUM_OP_TIME, Gross.Load..MW.h.,  HEAT.INPUT, Max.Hourly.HI.Rate..MMBtu.hr.)

ampd_raw_corrplot <- na.omit (ampd_raw_corrplot)

corr_ampd_raw <- cor (ampd_raw_corrplot)

corrplot(corr_ampd_raw, method="circle", type="upper", order="hclust")





#Virginia emissions

ampd_sym %>% filter(year >= 2015, STATE == "VA") %>%
   ggplot(aes(month, NOx..tons., color= year, group= year)) +
   geom_point() + geom_line()+ labs(x= "Month", 
                                    y = "NOx Emission (tons)",
                                    title = "Virginia Power Plant Emissions") +
   scale_x_continuous(breaks = seq(0, 12, by = 1)) + 
   scale_y_continuous(limits = c(0, 4000), expand = c(0,0)) + 
   theme( panel.grid.major = element_blank(),  panel.grid.minor = element_blank(),
          panel.background = element_rect(colour = "black", size=1,   fill=NA),
          legend.title = element_blank(), legend.position = c( 0.85, 0.9), legend.direction = "horizontal",
          legend.text = element_text(size = 10), text = element_text(size=15),
          legend.key.size = unit(2, 'lines'),
          plot.title = element_text(hjust = 0.5)) + expand_limits(y = 0)  + geom_hline(yintercept = 0) 

ggsave("va_plot.png", path = "/Users/munshirasel/Google Drive/R/ampd-3")


ampd_sym%>% filter (year >= 2011, STATE=="VA") %>%
   ggplot(aes( year, NOx..tons., group= year)) + geom_boxplot(aes(fill=year)) +
   scale_x_continuous(breaks = seq(2011, 2020, by = 1)) + 
   labs(x= "Year",   y = "NOx Emission (tons)", title = "Yearly Virginia Power Plant Emissions") +
   theme( panel.grid.major = element_blank(),  panel.grid.minor = element_blank(),
          panel.background = element_rect(colour = "black", size=1,   fill=NA),
                                          legend.position = "bottom", 
          plot.title = element_text(hjust = 0.5)) + 
   guides(fill = guide_colorbar(title = "year",
                                label.position = "bottom",
                                title.position = "left", title.vjust = 1, title.hjust = 10,
                                # draw border around the legend
                                frame.colour = "black",
                                barwidth = 10,
                                barheight = 1)) 




#Slope calculating NOx emission 2015-2019 vs 2020

ampd_y<- aggregate(list (NOx..tons.=ampd_raw$NOx..tons., 
                         SO2..tons.=ampd_raw$SO2..tons.,
                         CO2..tons.= ampd_raw$CO2..tons.,
                         Gross.Load..MW.h.=ampd_raw$Gross.Load..MW.h.,
                         Steam.Load..1000lb.= ampd_raw$Steam.Load..1000lb.,
                         HEAT.INPUT= ampd_raw$HEAT.INPUT,
                         SUM_OP_TIME= ampd_raw$SUM_OP_TIME
), by=list( year=ampd_raw$year), FUN=sum)


p <- ampd_y %>% filter (year >=2015 & year <2021) %>% select(year, NOx..tons.)



prep5 <- p %>% filter (year<2020) %>%
   mutate(group5 = c(1,1,1,1,1)) %>%
   group_by(group5) %>%
   mutate(
      slope = round(lm(NOx..tons. ~ year)$coefficients[2], 2),
      significance = summary(lm(NOx..tons. ~ year))$coefficients[2, 4],
      x = mean(year),   # x coordinate for slope label
      y = mean(NOx..tons.)     # y coordinate for slope label
   ) %>%
   filter(significance < .2) 


ampd_y %>% filter (year>= 2015 & year <=2020) %>% ggplot( aes(x = year, y = NOx..tons.)) +
   geom_line(colour = "blue", lwd = 1) +
   geom_point(colour = "blue", size = 2) +
   theme(
      panel.background = element_rect(fill = "white"),
      plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"),
      panel.border = element_rect(colour = "black", fill = NA, size = 1),
      axis.line.x = element_line(colour = "black"),
      axis.line.y = element_line(colour = "black"),
      axis.text = element_text(size = 15, colour = "black", family = "serif"),
      axis.title = element_text(size = 15, colour = "black", family = "serif"),
      legend.position = "top" ) + 
   scale_x_discrete(limits = c(seq(2015, 2020, 1))) + geom_smooth(
   data = prep5, aes(x = year, y = NOx..tons., group = group5),  # grouping variable does the plots for us!
   method = "lm", se = FALSE, color = "black",
   formula = y ~ x, linetype = "dashed"
) +  geom_text(
      data = prep5, aes(x = x, y = y, label = slope),
      nudge_y = 12, nudge_x = -1
   )




#VA State deviation 2020 vs 2015-2019

ampd_sym<-            aggregate(list (NOx..tons.=ampd_raw$NOx..tons., 
                                      SO2..tons.=ampd_raw$SO2..tons.,
                                      CO2..tons.= ampd_raw$CO2..tons.,
                                      Gross.Load..MW.h.=ampd_raw$Gross.Load..MW.h.,
                                      Steam.Load..1000lb.= ampd_raw$Steam.Load..1000lb.,
                                      HEAT.INPUT= ampd_raw$HEAT.INPUT,
                                      SUM_OP_TIME= ampd_raw$SUM_OP_TIME
), by=list(STATE=ampd_raw $ STATE, year=ampd_raw$year,
           month=ampd_raw $month), FUN=sum)

ampd_sym

ampd_ym <- ampd_sym %>% filter (STATE == "VA" & year >=1997)

ampd_y<- aggregate(list (NOx..tons.=ampd_ym$NOx..tons., 
                         SO2..tons.=ampd_ym$SO2..tons.,
                         CO2..tons.= ampd_ym$CO2..tons.,
                         Gross.Load..MW.h.=ampd_ym$Gross.Load..MW.h.,
                         Steam.Load..1000lb.= ampd_ym$Steam.Load..1000lb.,
                         HEAT.INPUT= ampd_ym$HEAT.INPUT,
                         SUM_OP_TIME= ampd_ym$SUM_OP_TIME
), by=list( year=ampd_ym$year), FUN=sum)


p <- ampd_y %>% filter (year >=2015 & year <2020) %>% select(year, NOx..tons.)



prep5 <- p %>% filter (year<2020) %>%
   mutate(group5 = c(1,1,1,1,1)) %>%
   group_by(group5) %>%
   mutate(
      slope = round(lm(NOx..tons. ~ year)$coefficients[2], 2),
      significance = summary(lm(NOx..tons. ~ year))$coefficients[2, 4],
      x = mean(year),   # x coordinate for slope label
      y = mean(NOx..tons.)     # y coordinate for slope label
   ) %>%
   filter(significance < .2) 


ampd_y %>% filter (year>= 2015 & year <=2020) %>% ggplot( aes(x = year, y = NOx..tons.)) +
   geom_line(colour = "blue", lwd = 1) +
   geom_point(colour = "blue", size = 2) +
   theme(
      panel.background = element_rect(fill = "white"),
      plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"),
      panel.border = element_rect(colour = "black", fill = NA, size = 1),
      axis.line.x = element_line(colour = "black"),
      axis.line.y = element_line(colour = "black"),
      axis.text = element_text(size = 15, colour = "black", family = "serif"),
      axis.title = element_text(size = 15, colour = "black", family = "serif"),
      legend.position = "top" ) + 
   scale_x_discrete(limits = c(seq(2015, 2020, 1))) + geom_smooth(
      data = prep5, aes(x = year, y = NOx..tons., group = group5),  # grouping variable does the plots for us!
      method = "lm", se = FALSE, color = "black",
      formula = y ~ x, linetype = "dashed"
   ) +  geom_text(
      data = prep5, aes(x = x, y = y, label = slope),
      nudge_y = 12, nudge_x = -1
   )


